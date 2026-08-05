# Does cache_guard actually guard? Testing the three behaviours it claims.
suppressMessages({ library(arrow); library(data.table) })
source("C:/dev/torpverse/torp/data-raw/04-analysis/cache_guard.R")

TMP <- file.path(tempdir(), "cgtest"); dir.create(TMP, showWarnings = FALSE)
mk <- function(v) data.frame(x = v)
ok <- function(lbl, cond) cat(sprintf("%-58s %s\n", lbl, if (isTRUE(cond)) "PASS" else "**FAIL**"))

# 1. First build writes both the frame and a stamp.
d1 <- cached_frame("t1", function() mk(1), dir = TMP)
ok("first call builds and stamps",
   file.exists(file.path(TMP, "t1.parquet")) && file.exists(file.path(TMP, "t1.fingerprint")))

# 2. Same fingerprint -> reuse, builder never runs.
ran <- FALSE
d2 <- cached_frame("t1", function() { ran <<- TRUE; mk(99) }, dir = TMP)
ok("matching fingerprint reuses without rebuilding", !ran && d2$x[1] == 1)

# 3. Stale stamp -> rebuild (default), and the new value wins.
writeLines("deadbeefcafe", file.path(TMP, "t1.fingerprint"))
d3 <- cached_frame("t1", function() mk(2), dir = TMP)
ok("stale stamp rebuilds by default", d3$x[1] == 2)

# 4. Stale stamp + abort -> error, not silent reuse.
writeLines("deadbeefcafe", file.path(TMP, "t1.fingerprint"))
e <- tryCatch({ cached_frame("t1", function() mk(3), dir = TMP, on_stale = "abort"); NULL },
              error = function(e) conditionMessage(e))
ok("stale stamp aborts when asked", !is.null(e) && grepl("Refusing", e))

# 5. No stamp at all (a frame predating the guard) -> still treated as stale.
writeLines("x", file.path(TMP, "t2.parquet"))  # placeholder so the file exists
write_parquet(mk(7), file.path(TMP, "t2.parquet"))
unlink(file.path(TMP, "t2.fingerprint"))
d5 <- cached_frame("t2", function() mk(8), dir = TMP)
ok("unstamped frame is treated as stale", d5$x[1] == 8)

# 6. THE PIN. Simulate code changing mid-run by moving the run's pinned value,
#    then confirm a later call aborts rather than quietly building a second
#    engine's worth of data.
.run_fingerprint("aaaaaaaaaaaa")
e2 <- tryCatch({ cached_frame("t3", function() mk(4), dir = TMP); NULL },
               error = function(e) conditionMessage(e))
ok("code changing mid-run aborts the later arm",
   !is.null(e2) && grepl("Code changed mid-run", e2))

# 7. And the abort must fire even with on_stale = "rebuild", because it is not
#    about trusting old data.
e3 <- tryCatch({ cached_frame("t4", function() mk(5), dir = TMP, on_stale = "rebuild"); NULL },
               error = function(e) conditionMessage(e))
ok("mid-run abort ignores on_stale", !is.null(e3) && grepl("Code changed mid-run", e3))

# 8. Fingerprint is content-based: touching a file must not change it.
f <- "C:/dev/torpverse/torp/R/epv_calibration.R"
before <- code_fingerprint()
Sys.setFileTime(f, Sys.time())
ok("touch does not change the fingerprint", identical(before, code_fingerprint()))
cat("\ndone\n")
