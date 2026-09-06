# Does the ruck LEDGER fix improve the stoppage component?
#
# The live hitout formula pays EPV_RUCK_CONTEST_WT for every contest a ruckman
# ATTENDS, wins and losses alike, so it is the only channel that is not
# credit-and-debit. EPV3_STOP_ZERO_SUM turns the attendance term into a win/loss
# ledger: `ruck_contests - hitouts` is what he lost.
#
# WHY IT MATTERS MORE NOW. Under the three-channel spec the contest channel is
# cont_aerial + cont_stop, each calibrated to one-point-per-unit first. That
# construction gave contest t = 4.43, and the aerial half is a null (t 0.91) --
# so the merged channel is carried almost entirely by the STOPPAGE component.
# Improving that component improves the whole contest channel.
#
# WHAT TO LOOK FOR. The coefficient is set by scale and will move for
# uninteresting reasons; the t is what says whether the signal got cleaner.
# A better component should raise the stoppage t, and through it the merged
# contest t above 4.43.
#
# ANCHOR, pre-registered: a ruckman who splits contests 50/50 should score ZERO
# from the attendance term under the ledger, where he currently scores positive.
# Asserted below on the constants before anything is built.
#
# PERFORMANCE: one v3 player-game build (~5 min) plus one rating build (~3 min,
# measured). Both cached. Runs alongside ws21 -- they compete for cores but both
# complete.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_ruck_ledger.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

say("=== Ruck ledger arm: EPV3_STOP_ZERO_SUM ===")

say("")
say("--- ANCHOR: a 50/50 ruckman must score zero from the attendance term ---")
wins <- 10; contests <- 20
attend_now <- contests * EPV_RUCK_CONTEST_WT
attend_led <- wins * EPV_RUCK_CONTEST_WT - max(0, contests - wins) * EPV_RUCK_LOSS_WT
say("  current formula, 10 of 20 won: ", round(attend_now, 4), "  (paid for losing half)")
say("  ledger formula,  10 of 20 won: ", round(attend_led, 4))
stopifnot(abs(attend_led) < 1e-9, attend_now > 0)
say("  PASS")

teams  <- load_teams(TRUE)
res    <- as.data.table(load_results(TRUE))
xg     <- as.data.table(load_xg(TRUE))
psr_df <- tryCatch(.compute_psr_from_stat_ratings(load_player_stat_ratings(TRUE)),
                   error = function(e) NULL)
tgt <- merge(res[, .(match_id = as.character(match_id), margin = home_score - away_score)],
             xg[, .(match_id = as.character(match_id), xmargin = xscore_diff)],
             by = "match_id")[is.finite(margin) & is.finite(xmargin)]

shared_stat_ratings <- get_player_stat_ratings(current = FALSE)
shared_fixtures     <- load_fixtures(TRUE)

SUB <- c("epr_recv", "epr_disp", "epr_spoil", "epr_hitout")

build_ratings <- function(pgd, tag) {
  f <- file.path(OUT_DIR, paste0("epv3_rt_", tag, ".parquet"))
  if (file.exists(f)) {
    cli::cli_alert_info("Reusing cached ratings {tag}")
    return(as.data.table(arrow::read_parquet(f)))
  }
  d <- adjust_epv_for_opponents(as.data.table(copy(pgd)))
  if (isTRUE(EPV_LEVEL_CENTRE)) d <- centre_epv_by_position(d)
  out <- rbindlist(lapply(sort(unique(d$season)), function(s) {
    sr <- if (s >= 2024) 0 else 1
    mr <- if (s == get_afl_season()) get_afl_week(type = "next") else 28
    torp:::.build_epr_season(s, sr:mr, d, shared_stat_ratings, shared_fixtures)
  }), use.names = TRUE, fill = TRUE)
  if (isTRUE(EPR_POSITION_CENTRE)) out <- centre_epr_by_position(out)
  if (!is.null(psr_df) && nrow(psr_df) > 0 && "psr" %in% names(psr_df)) {
    out <- calculate_torp(out, psr_df)
  }
  out <- as.data.table(out)
  arrow::write_parquet(out, f)
  out
}

score <- function(rt, label) {
  tr <- as.data.table(.build_team_ratings_df(teams, as.data.frame(rt), psr_df))
  h <- tr[team_type == "home"]; a <- tr[team_type == "away"]
  m <- merge(h[, c("match_id", SUB), with = FALSE],
             a[, c("match_id", SUB), with = FALSE],
             by = "match_id", suffixes = c("_h", "_a"))
  for (v in SUB) m[, (paste0("d_", v)) := get(paste0(v, "_h")) - get(paste0(v, "_a"))]
  m <- merge(m, tgt, by = "match_id")
  # Step 1: calibrate each sub-component to one-point-per-unit.
  f4 <- lm(as.formula(paste("xmargin ~ 0 +", paste0("d_", SUB, collapse = " + "))), data = m)
  co4 <- summary(f4)$coefficients
  k <- setNames(co4[, 1], sub("^d_", "", rownames(co4)))
  sub_t <- setNames(co4[, 3], sub("^d_", "", rownames(co4)))
  # Step 2: apply, merge the contest pair, refit as three channels.
  mc <- copy(m)
  for (v in SUB) mc[, (paste0("d_", v)) := get(paste0("d_", v)) * k[[v]]]
  mc[, d_cont := d_epr_spoil + d_epr_hitout]
  f3 <- lm(xmargin ~ 0 + d_epr_recv + d_epr_disp + d_cont, data = mc)
  co3 <- summary(f3)$coefficients
  list(label = label, k = k, sub_t = sub_t,
       three = data.table(arm = label, channel = c("recv", "disp", "contest"),
                          coef = round(co3[, 1], 4), t = round(co3[, 3], 2)))
}

# --- arm A: current attendance formula (ratings already cached) --------------
rtA <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "epv3_ratings_v3.parquet")))
A <- score(rtA, "attendance (current)")

# --- arm B: ledger ----------------------------------------------------------
old <- torp:::EPV3_STOP_ZERO_SUM
assignInNamespace("EPV3_STOP_ZERO_SUM", TRUE, ns = "torp")
on.exit(assignInNamespace("EPV3_STOP_ZERO_SUM", old, ns = "torp"), add = TRUE)
say("")
say("EPV3_STOP_ZERO_SUM switched: ", old, " -> ", torp:::EPV3_STOP_ZERO_SUM)

fB <- file.path(OUT_DIR, "epv3_pgd_ruckledger.parquet")
if (file.exists(fB)) {
  pgdB <- as.data.table(arrow::read_parquet(fB))
  say("reusing cached ledger player-game frame")
} else {
  pgdB <- as.data.table(create_player_game_data(
    load_pbp(TRUE), load_player_stats(TRUE), teams, load_chains(TRUE),
    epv_engine = "v3"))
  arrow::write_parquet(pgdB, fB)
}

# Guard: the stoppage channel must actually have changed.
pgdA <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "epv3_player_game_v3.parquet")))
cm <- merge(pgdA[, .(player_id, match_id, a = epv_hitout)],
            pgdB[, .(player_id, match_id, b = epv_hitout)],
            by = c("player_id", "match_id"))
say("stoppage channel mean|diff| ", round(mean(abs(cm$a - cm$b), na.rm = TRUE), 5),
    " | share of NA ", round(100 * mean(!is.finite(cm$b)), 2), "%")
stopifnot(mean(abs(cm$a - cm$b), na.rm = TRUE) > 1e-9, all(is.finite(cm$b)))

rtB <- build_ratings(pgdB, "ruckledger")
B <- score(rtB, "ledger (zero-sum)")

# ---- Compare ---------------------------------------------------------------
say("")
say("=== STOPPAGE SUB-COMPONENT: does the ledger clean it up? ===")
say_dt(data.table(
  arm = c(A$label, B$label),
  coef = c(round(A$k[["epr_hitout"]], 4), round(B$k[["epr_hitout"]], 4)),
  t    = c(round(A$sub_t[["epr_hitout"]], 2), round(B$sub_t[["epr_hitout"]], 2))), 4)
say("")
say("The COEFFICIENT is set by scale and moving is uninformative. The t is the")
say("signal-quality number: higher means the component is cleaner.")

say("")
say("=== all four sub-components, t statistics ===")
say_dt(data.table(
  component = c("recv", "disp", "cont_aerial", "cont_stop"),
  attendance = round(A$sub_t[SUB], 2),
  ledger     = round(B$sub_t[SUB], 2)), 6)

say("")
say("=== THE THREE CHANNELS (target 1.000 each) ===")
say_dt(rbind(A$three, B$three), 8)
say("")
say("contest t: attendance ", A$three[channel == "contest"]$t,
    "  ->  ledger ", B$three[channel == "contest"]$t)

saveRDS(list(attendance = A, ledger = B), file.path(OUT_DIR, "epv3_ruck_ledger.rds"))
close(con)
cat("\nDone\n")
