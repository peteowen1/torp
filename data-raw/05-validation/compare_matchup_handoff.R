# Does the matchup table come out the same from a handed-over state?
# ==================================================================
# Check for R/match_state_handoff.R. Publishes nothing: every state is built
# with refresh_results = FALSE and no table is uploaded.
#
#   Rscript data-raw/05-validation/compare_matchup_handoff.R [season] [week]
#
# Four tables for one round, compared on p_home / pred_margin / pred_total:
#   mem   state A, handed straight to .freeze_match_state(prebuilt =)
#   disk  state A, saved and re-read through the hand-over file
#   B     a fresh rebuild, which is what the matchup step did before
#   C     a second fresh rebuild
#
#   mem vs disk  must be identical(): the file round-trip loses nothing.
#   B vs C       the noise floor: two refits on the same data (xgboost
#                threading is not deterministic).
#   disk vs B    what the change does to served numbers. Read it against B vs C.
#
# Needs about 8 GB free and takes three state builds (192 s each, 18 Nov 2026).

t0 <- Sys.time()
suppressMessages({
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
  devtools::load_all("C:/dev/torpverse/torpmodels", quiet = TRUE)
})
options(torp.local_data_dir = NA)  # read releases, not a stale local copy
args <- commandArgs(trailingOnly = TRUE)
season <- if (length(args) >= 1) as.integer(args[1]) else 2026L
week   <- if (length(args) >= 2) as.integer(args[2]) else 20L
stamp <- function(msg) cat(sprintf("[%4.0fs] %s\n", as.numeric(difftime(Sys.time(), t0, units = "secs")), msg))

num_cols <- c("p_home", "pred_margin", "pred_total")
key_cols <- c("home", "away", "venue")
table_of <- function(frozen) {
  tbl <- as.data.frame(build_matchup_table(state = frozen))
  stopifnot(nrow(tbl) == 612, !anyNA(tbl[num_cols]))
  tbl[do.call(order, tbl[key_cols]), c(key_cols, num_cols)]
}
max_diff <- function(a, b) {
  stopifnot(identical(a[key_cols], b[key_cols]))
  vapply(num_cols, function(col) max(abs(a[[col]] - b[[col]])), numeric(1))
}

st_a <- build_prediction_state(season = season, week = week, refresh_results = FALSE)
stopifnot(!is.null(st_a), length(st_a$validation_errors) == 0)
stamp(sprintf("state A built: %d rows", nrow(st_a$team_mdl_df)))

path <- tempfile(fileext = ".rds")
stopifnot(isTRUE(torp:::.save_match_state_handoff(st_a, path)))
h <- torp:::.load_match_state_handoff(season, week, path)
cat("Matchup state:", if (h$reuse) "REUSED" else "REBUILT", "--", h$reason, "\n")
stopifnot(isTRUE(h$reuse))

# The in-memory arm is trimmed the same way the saved one is, without the disk.
mem_state <- st_a
mem_state$gam_result <- list(models = st_a$gam_result$models)
tbl_mem  <- table_of(torp:::.freeze_match_state(season, week, prebuilt = mem_state))
tbl_disk <- table_of(torp:::.freeze_match_state(season, week, prebuilt = h$state))
stamp("tables from state A built (mem, disk)")

tbl_b <- table_of(torp:::.freeze_match_state(season, week)); stamp("rebuild B done")
tbl_c <- table_of(torp:::.freeze_match_state(season, week)); stamp("rebuild C done")

cat("\nmem vs disk identical():", identical(tbl_mem, tbl_disk), "\n")
cat("\nLargest absolute difference across the 612 rows (p_home is a probability, the others are points):\n")
print(round(rbind(
  "mem vs disk (must be 0)"    = max_diff(tbl_mem, tbl_disk),
  "B vs C (refit noise floor)" = max_diff(tbl_b, tbl_c),
  "disk vs B (the change)"     = max_diff(tbl_disk, tbl_b)
), 5))
cat("\nMargin calibration: state A used b =", format(st_a$margin_calib$b),
    "; release now has b =", format(load_match_margin_calibration()$b), "\n")
stamp("done")
if (!identical(tbl_mem, tbl_disk)) quit(save = "no", status = 1)
