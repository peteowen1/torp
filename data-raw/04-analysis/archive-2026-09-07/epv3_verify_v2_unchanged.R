# The claim v3 rests on: "EPV_ENGINE defaults to v2 and production ratings are
# byte-for-byte unchanged." That is an assertion about a code path nobody ran a
# baseline on, so this script makes it a measurement.
#
# Regenerates the v2 arm with the CURRENT code and compares it column-by-column
# against the v2 frame written by the draft run. Any difference at all is a
# failure -- the v3 work was supposed to be additive, and a v2 path that drifted
# would silently move every published rating the moment the pipeline next runs.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
BASE <- file.path(OUT_DIR, "epv3_player_game_v2.parquet")
stopifnot(file.exists(BASE))

old <- as.data.table(arrow::read_parquet(BASE))
new <- as.data.table(create_player_game_data(
  load_pbp(TRUE), load_player_stats(TRUE), load_teams(TRUE), load_chains(TRUE),
  epv_engine = "v2"))

cat("=== v2 regression check ===\n")
cat("baseline rows ", nrow(old), " cols ", ncol(old), "\n")
cat("current  rows ", nrow(new), " cols ", ncol(new), "\n")

if (!identical(sort(names(old)), sort(names(new)))) {
  cat("!! COLUMN SET CHANGED\n")
  cat("  only in baseline: ", paste(setdiff(names(old), names(new)), collapse = ", "), "\n")
  cat("  only in current : ", paste(setdiff(names(new), names(old)), collapse = ", "), "\n")
}

key <- c("player_id", "match_id")
setkeyv(old, key); setkeyv(new, key)
common <- intersect(names(old), names(new))
m <- merge(old[, ..common], new[, ..common], by = key, suffixes = c(".old", ".new"))
cat("matched rows ", nrow(m), "\n\n")

bad <- 0L
for (cc in setdiff(common, key)) {
  a <- m[[paste0(cc, ".old")]]; b <- m[[paste0(cc, ".new")]]
  same <- if (is.numeric(a) && is.numeric(b)) {
    isTRUE(all.equal(a, b, tolerance = 0)) ||
      max(abs(a - b), na.rm = TRUE) < 1e-12
  } else identical(a, b)
  if (!same) {
    bad <- bad + 1L
    d <- if (is.numeric(a)) max(abs(a - b), na.rm = TRUE) else NA
    cat(sprintf("  DIFFERS: %-32s max|diff| %s\n", cc, format(d)))
  }
}

if (bad == 0L) {
  cat("\nPASS: every column identical. The v2 path did not move.\n")
} else {
  cat("\nFAIL: ", bad, " column(s) changed. v3 was meant to be additive.\n")
  quit(status = 1)
}
