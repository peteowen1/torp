# Re-record a vintage's manifest entry under the CURRENT constant set
# =============================================================================
# When .rating_defining_constants() gains a constant, the published manifest
# still records the old, narrower set, so check_vintage_alignment() reports
# drift and ABORTS every pipeline run -- even with strict = FALSE. The data is
# fine; only what is written down about it is stale. This re-records the same
# vintage under current code: same label, same file, same rows, wider
# defining_constants.
#
# Written 2026-09-10 for the v8 re-record after torp 1.8.2 wired ten NP_
# constants (32 leaves added, 0 removed, 0 value changes).
#
# TWO TRAPS THIS SCRIPT EXISTS TO AVOID, both of which a hand-typed
# publish_ratings_manifest() call walks straight into:
#
#   1. `n_rows` is REQUIRED and has no default -- a call without it errors.
#   2. `file` DEFAULTS TO .rating_vintage_file(version), i.e.
#      "torp_ratings_v8.parquet" -- but the CANONICAL vintage lives at
#      "torp_ratings.parquet". Taking the default writes a path into the
#      manifest that does not hold that vintage's data. The parameter's own
#      docstring says it: "Must be passed explicitly: the vintage label and the
#      filename are independent."
#
# Both are avoided here by reading the existing entry and reusing its values.
#
# WHEN TO RUN IT. This is the opposite of the order a VALUE change uses, and
# getting it backwards took the pipeline down on 2026-09-10:
#
#   VALUE change (a constant's value moves, so published numbers change):
#     promote/record the new vintage BEFORE the code reaches `main`. The
#     manifest has to describe the new data before the code that writes it
#     lands. This is the v6 -> v7 -> v8 pattern in NEWS.
#
#   WIDENING change (constants ADDED to what the guard records; values
#     unchanged, published numbers unmoved -- what this script is for):
#     merge the code to `main` FIRST, then run this. A manifest that lists
#     constants the running code does not have IS drift, because
#     .diff_defining_constants() unions both name sets and reports <absent> in
#     EITHER direction. Recording 97 leaves from a dev branch while `main` knew
#     65 made main see 29 phantom differences and abort every run.
#
# The rule of thumb: the manifest may never describe more constants than the
# code on `main` knows about.
#
#   powershell.exe -Command 'Rscript "data-raw/05-validation/rerecord_vintage_manifest.R"'
suppressMessages(devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE))
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

# Defaults to the vintage the loaded code declares, which is the one whose
# constants just moved. Override with RERECORD_VINTAGE to re-record another.
VERSION <- Sys.getenv("RERECORD_VINTAGE", RATING_VINTAGE)
say("re-recording vintage ", VERSION, " (torp ",
    as.character(utils::packageVersion("torp")), ")")

before <- read_ratings_manifest()
v8 <- before$vintages[[VERSION]]
stopifnot(!is.null(v8))
if (!identical(before$canonical, VERSION)) {
  say("NOTE: ", VERSION, " is not canonical (canonical is ", before$canonical,
      ") -- re-recording a non-canonical vintage, which is allowed but rarely intended.")
}

# Read the existing values rather than hardcoding them. `file` in particular
# MUST be passed explicitly: its default is .rating_vintage_file("v8") ->
# "torp_ratings_v8.parquet", but v8 is canonical and lives at
# "torp_ratings.parquet", so the default would write a wrong path.
n_rows_before  <- v8$rows
file_before    <- v8$file
leaves_before  <- length(unlist(v8$defining_constants))
leaves_live    <- length(unlist(torp:::.rating_defining_constants()))
say("re-recording ", VERSION, ": file=", file_before, "  rows=", n_rows_before)
say("defining_constants: ", leaves_before, " recorded -> ", leaves_live, " under live code")
stopifnot(is.character(file_before), length(file_before) == 1L,
          is.numeric(n_rows_before) || is.integer(n_rows_before))

publish_ratings_manifest(n_rows = n_rows_before, version = VERSION,
                         file = file_before, set_canonical = FALSE)

# Read back past GitHub's CDN, which served a stale manifest for ~2 minutes
# after the last overwrite (recorded in NEXT-STEPS, 2026-09-09) and for ~84
# seconds after this one. Poll the UNCACHED url read_ratings_manifest()
# actually fetches rather than assuming the first response is current.
#
# Poll on .diff_defining_constants(), NOT on a leaf count. A count cannot work:
# NA-valued constants (NP_TEAM_MARGIN_NAMED_SHARE and the INT/SUB/EMERG slots of
# LINEUP_POSITION_GROUP_MAP) serialise to JSON null and unlist() drops them, so
# the published set counts 94 leaves where the live one counts 97 even when the
# two agree exactly. The first version of this script polled on the count, never
# matched, and burned all 20 attempts on a run that had already succeeded --
# a green result that reads as a timeout. This is the comparison
# check_vintage_alignment() itself makes.
say("\npolling the uncached manifest for the re-record to land...")
ok <- FALSE
for (i in 1:20) {
  Sys.sleep(6)
  after <- tryCatch(read_ratings_manifest(), error = function(e) NULL)
  if (is.null(after)) next
  a8 <- after$vintages[[VERSION]]
  n_diff <- length(torp:::.diff_defining_constants(a8$defining_constants))
  say("  attempt ", i, ": torp_version=", a8$torp_version,
      "  constant differences=", n_diff, "  canonical=", after$canonical)
  if (n_diff == 0L) { ok <- TRUE; break }
}
if (!ok) {
  say("\nWARNING: the re-record did not show up as aligned within ",
      20 * 6, "s. The upload itself reported success, so this is most likely",
      " the CDN still serving a stale copy -- re-read in a minute before",
      " re-running anything.")
}

say("\n=== verification ===")
after <- read_ratings_manifest()
a8 <- after$vintages[[VERSION]]
say("canonical:     ", after$canonical, "   (must still be the same)")
say("file:          ", a8$file, "   (must be ", file_before, ")")
say("rows:          ", a8$rows, "   (must be ", n_rows_before, ")")
say("torp_version:  ", a8$torp_version)
say("leaves:        ", length(unlist(a8$defining_constants)), " (was ", leaves_before,
    "; live unlist() says ", leaves_live, " -- NA leaves serialise to null, see above)")
say("constant diffs: ", length(torp:::.diff_defining_constants(a8$defining_constants)), "   (must be 0)")
say("vintages kept: ", paste(names(after$vintages), collapse = ", "))
stopifnot(identical(after$canonical, before$canonical),
          identical(a8$file, file_before),
          identical(as.integer(a8$rows), as.integer(n_rows_before)))

say("\n=== does the guard now report aligned? ===")
res <- tryCatch({ check_vintage_alignment(strict = TRUE); "ALIGNED" },
                error = function(e) paste("STILL DRIFTING:", conditionMessage(e)))
say(res)
