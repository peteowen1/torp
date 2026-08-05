# Prove the difficulty wiring is inert when its flag is off.
#
# WHY THIS EXISTS. `create_player_game_data()` is production. The difficulty
# split added four things to it that run on EVERY call regardless of the flag:
# a `.is_diff_disp` column, a `.disp_scale := 0` update keyed on it, an extra
# condition in the `recv_dt` filter, and two zero columns joined in and added to
# `epv_recv`/`epv_disp` in both engine branches.
#
# All four are no-ops by construction when EPV_DIFFICULTY_SPLIT is FALSE. "By
# construction" is exactly the kind of claim this repo has been wrong about
# before -- the v3 rebuild shipped `centre_epv_by_position()` falling back to the
# v2 scale for months, and constants defined above the flag they read threw at
# BUILD time. Asserting inertness is not the same as measuring it.
#
# So: rebuild both engines with the current code and compare EVERY column against
# the frames built before the change. Any difference at all is a regression.
#
# ~15 min (two player-game builds). Run detached.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_verify_difficulty_inert.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

say("=== Is the difficulty wiring inert with its flag off? ===")
say("run at ", format(Sys.time()))
say("EPV_DIFFICULTY_SPLIT = ", EPV_DIFFICULTY_SPLIT,
    " | EPV_DIFFICULTY_SURPRISE_BY_TYPE = ", EPV_DIFFICULTY_SURPRISE_BY_TYPE)
if (isTRUE(EPV_DIFFICULTY_SPLIT)) {
  say("!! The flag is ON. This script tests the OFF path and cannot run.")
  close(con); quit(status = 1)
}

pbp <- load_pbp(TRUE); stats_ <- load_player_stats(TRUE)
teams <- load_teams(TRUE); chains <- load_chains(TRUE)

KEY <- c("player_id", "match_id")
compare <- function(old_f, engine, label) {
  say(""); say("--- ", label, " ---")
  if (!file.exists(old_f)) { say("  baseline missing: ", basename(old_f)); return(invisible(NULL)) }
  old <- as.data.table(read_parquet(old_f))
  new <- as.data.table(create_player_game_data(pbp, stats_, teams, chains,
                                               epv_engine = engine))

  say(sprintf("  rows  old %s  new %s", format(nrow(old), big.mark = ","),
              format(nrow(new), big.mark = ",")))
  only_old <- setdiff(names(old), names(new)); only_new <- setdiff(names(new), names(old))
  if (length(only_old)) say("  columns only in OLD: ", paste(only_old, collapse = ", "))
  if (length(only_new)) say("  columns only in NEW: ", paste(only_new, collapse = ", "))

  shared <- intersect(names(old), names(new))
  m <- merge(old[, shared, with = FALSE], new[, shared, with = FALSE],
             by = KEY, suffixes = c("_o", "_n"))
  say("  matched player-games: ", format(nrow(m), big.mark = ","))
  if (nrow(m) != nrow(old) || nrow(m) != nrow(new)) {
    say("  !! ROW SET CHANGED -- that alone is a regression")
  }

  cmp <- setdiff(shared, KEY)
  rows <- rbindlist(lapply(cmp, function(cc) {
    a <- m[[paste0(cc, "_o")]]; b <- m[[paste0(cc, "_n")]]
    if (is.numeric(a) && is.numeric(b)) {
      d <- abs(a - b); d[is.na(a) & is.na(b)] <- 0
      data.table(column = cc, kind = "numeric", max_abs_diff = max(d, na.rm = TRUE),
                 n_diff = sum(d > 1e-9, na.rm = TRUE))
    } else {
      neq <- !(as.character(a) == as.character(b)) & !(is.na(a) & is.na(b))
      data.table(column = cc, kind = "other", max_abs_diff = NA_real_,
                 n_diff = sum(neq, na.rm = TRUE))
    }
  }))
  bad <- rows[n_diff > 0]
  say("  columns compared: ", nrow(rows))
  if (nrow(bad) == 0) {
    say("  VERDICT: IDENTICAL across all ", nrow(rows), " columns.")
  } else {
    say("  !! VERDICT: ", nrow(bad), " column(s) DIFFER -- the wiring is not inert")
    say_dt(bad[order(-n_diff)], 20)
  }
  invisible(bad)
}

# The v3 baseline is the frame the ship constants were fitted against; the v2
# baseline is production itself, which is the one that actually matters.
b3 <- compare(file.path(OUT_DIR, "epv3_fin_pgd_ship.parquet"), "v3", "v3 (ship frame)")
b2 <- compare(file.path(OUT_DIR, "epv3_cal_pgd_v2prod.parquet"), "v2", "v2 (production)")

say("")
if (is.null(b3) && is.null(b2)) {
  say("NOTHING VERIFIED -- both baselines missing. Do not read this as a pass.")
} else if ((is.null(b3) || nrow(b3) == 0) && (is.null(b2) || nrow(b2) == 0)) {
  say("OVERALL: the difficulty wiring changes nothing while its flag is off.")
} else {
  say("OVERALL: REGRESSION. Do not ship until the differing columns are explained.")
}
say(""); say("done ", format(Sys.time())); close(con); cat("\nDone\n")
