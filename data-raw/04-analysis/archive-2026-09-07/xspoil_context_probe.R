# xspoil_context_probe.R --------------------------------------------------
# WS2 go/no-go probe for docs/plans/FABLE-DEFENDER-VALUE-PLAN.md §5.
#
# Question: does a spoil's value vary enough with context (field position,
# kick length, contest type) to justify building an xSpoil model? If the
# value-at-stake is flat across contexts, a constant EPV_SPOIL_WT loses
# nothing and WS2 collapses into WS3 (role-aware pricing).
#
# Method: locate every Spoil in chains, find the kick it defused (same
# lag-scan compute_contest_credit() uses), join to PBP for that kick's
# delta_epv -- the expected-points swing genuinely at stake in the contest.
# Then measure how much that quantity moves across context bins, and how
# much of it the flat weight currently throws away.
#
# Spoil rows are filtered out of clean PBP (clean_pbp.R:344), so spoils are
# identified in chains and valued via a join back to PBP on display_order.
#
#   powershell.exe -Command 'Rscript "torp/data-raw/04-analysis/xspoil_context_probe.R"'
# -------------------------------------------------------------------------

suppressMessages({
  library(arrow)
  library(data.table)
})

DATA_DIR <- "C:/dev/torpverse/torpdata/data/"
SEASONS  <- 2023:2025
KICK_DESCS <- c("Kick", "Ground Kick")

load_stub <- function(stub, season) {
  fs <- list.files(DATA_DIR, pattern = sprintf("^%s_%d_\\d+\\.parquet$", stub, season),
                   full.names = TRUE)
  rbindlist(lapply(fs, function(f) as.data.table(read_parquet(f))),
            use.names = TRUE, fill = TRUE)
}

spoils_for_season <- function(season) {
  ch <- load_stub("chains_data", season)
  pb <- load_stub("pbp_data", season)
  if (!nrow(ch) || !nrow(pb)) return(NULL)

  setnames(ch, c("displayOrder", "matchId", "teamId", "playerId"),
           c("display_order", "match_id", "team_id", "player_id"), skip_absent = TRUE)
  setorder(ch, match_id, display_order)

  # Lag scan for the kick that produced the contest, mirroring
  # compute_contest_credit(): first Kick/Ground Kick within 5 rows back.
  for (k in 1:5) {
    ch[, paste0(".d", k) := shift(description, k, type = "lag"), by = match_id]
    ch[, paste0(".o", k) := shift(display_order, k, type = "lag"), by = match_id]
    ch[, paste0(".x", k) := shift(x, k, type = "lag"), by = match_id]
    ch[, paste0(".y", k) := shift(y, k, type = "lag"), by = match_id]
  }
  sp <- ch[description == "Spoil" & !is.na(player_id)]
  if (!nrow(sp)) return(NULL)

  pick <- function(dt, prefix) {
    fcase(dt$.d1 %chin% KICK_DESCS, as.numeric(dt[[paste0(prefix, 1)]]),
          dt$.d2 %chin% KICK_DESCS, as.numeric(dt[[paste0(prefix, 2)]]),
          dt$.d3 %chin% KICK_DESCS, as.numeric(dt[[paste0(prefix, 3)]]),
          dt$.d4 %chin% KICK_DESCS, as.numeric(dt[[paste0(prefix, 4)]]),
          dt$.d5 %chin% KICK_DESCS, as.numeric(dt[[paste0(prefix, 5)]]),
          default = NA_real_)
  }
  sp[, `:=`(kick_do = pick(sp, ".o"), kick_x = pick(sp, ".x"), kick_y = pick(sp, ".y"))]
  sp <- sp[!is.na(kick_do)]

  # PBP carries the modelled state: delta_epv is the swing the kick was worth,
  # exp_pts the possessing team's expected points at that moment.
  keep <- intersect(c("match_id", "display_order", "delta_epv", "exp_pts",
                      "goal_x", "x", "y"), names(pb))
  k <- pb[, ..keep]
  setnames(k, c("x", "y", "goal_x"), c("pb_x", "pb_y", "pb_goal_x"), skip_absent = TRUE)
  out <- merge(sp[, .(match_id, season = season, display_order, player_id,
                      spoil_x = x, spoil_y = y, kick_do, kick_x, kick_y,
                      venue_length = venueLength, final_state = finalState,
                      chain_number)],
               k, by.x = c("match_id", "kick_do"),
               by.y = c("match_id", "display_order"), all.x = TRUE)
  out[!is.na(delta_epv)]
}

cat("Loading chains + PBP for", paste(range(SEASONS), collapse = "-"), "...\n")
sp <- rbindlist(lapply(SEASONS, function(s) {
  r <- spoils_for_season(s)
  cat(sprintf("  %d: %s spoils valued\n", s, format(if (is.null(r)) 0 else nrow(r), big.mark = ",")))
  r
}), use.names = TRUE, fill = TRUE)

stopifnot(nrow(sp) > 0)

# --- context features -----------------------------------------------------
# Orient off PBP's goal_x, which is the attacking goal for the team in
# possession on the kick row. The spoiler defends that goal, so the true
# feature is distance from the contest to goal_x. Using |half - x| instead
# would merge attacking-end and defending-end spoils into one bin and bias
# the context R2 downward.
sp[, half_len := fifelse(is.na(venue_length), 165, venue_length) / 2]
sp[, dist_to_goal := fifelse(is.na(pb_goal_x),
                             pmax(0, half_len - abs(spoil_x)),
                             sqrt((spoil_x - pb_goal_x)^2 + spoil_y^2))]
sp[, lateral := abs(spoil_y)]
sp[, kick_len := sqrt((spoil_x - kick_x)^2 + (spoil_y - kick_y)^2)]

# Value genuinely at stake: the EPV swing of the kick the spoil defused.
sp[, at_stake := abs(delta_epv)]

fmt <- function(x) sprintf("%.3f", x)
cat(sprintf("\nn = %s spoils, %s matches\n", format(nrow(sp), big.mark = ","),
            format(uniqueN(sp$match_id), big.mark = ",")))
cat(sprintf("\nValue at stake per spoil (|delta_epv| of the defused kick):\n"))
cat(sprintf("  mean %s   sd %s   p10 %s   p50 %s   p90 %s   p99 %s\n",
            fmt(mean(sp$at_stake)), fmt(sd(sp$at_stake)),
            fmt(quantile(sp$at_stake, .10)), fmt(quantile(sp$at_stake, .50)),
            fmt(quantile(sp$at_stake, .90)), fmt(quantile(sp$at_stake, .99))))
cat(sprintf("  current flat weight EPV_SPOIL_WT = %.4f\n", 0.0737))
cat(sprintf("  ratio p90/p10 = %.1fx\n", quantile(sp$at_stake, .90) / quantile(sp$at_stake, .10)))

# --- how much of the spread is explained by observable context? ----------
cat("\n--- by distance to defended goal ---\n")
sp[, bin_dist := cut(dist_to_goal, c(-1, 20, 40, 60, 80, 200),
                     labels = c("0-20m (in the goalsquare)", "20-40m", "40-60m",
                                "60-80m", "80m+ (up the ground)"))]
print(sp[, .(n = .N, mean_at_stake = round(mean(at_stake), 3),
             sd = round(sd(at_stake), 3)), by = bin_dist][order(bin_dist)])

cat("\n--- by kick length ---\n")
sp[, bin_kick := cut(kick_len, c(-1, 20, 35, 50, 500),
                     labels = c("<20m", "20-35m", "35-50m", "50m+"))]
print(sp[, .(n = .N, mean_at_stake = round(mean(at_stake), 3)),
         by = bin_kick][order(bin_kick)])

cat("\n--- by lateral position (distance off the corridor) ---\n")
sp[, bin_lat := cut(lateral, c(-1, 15, 30, 200),
                    labels = c("corridor", "flank", "boundary"))]
print(sp[, .(n = .N, mean_at_stake = round(mean(at_stake), 3)),
         by = bin_lat][order(bin_lat)])

# --- the decision statistic ----------------------------------------------
fit <- lm(at_stake ~ dist_to_goal + I(dist_to_goal^2) + lateral + kick_len, data = sp)
r2 <- summary(fit)$r.squared
pred <- predict(fit)
cat("\n=========================================================\n")
cat("DECISION STATISTIC\n")
cat("=========================================================\n")
cat(sprintf("Context model R2 (dist + dist^2 + lateral + kick length) = %.3f\n", r2))
cat(sprintf("SD of context-predicted value at stake  = %.3f\n", sd(pred)))
cat(sprintf("SD of a flat per-spoil weight            = 0.000 (constant)\n"))
cat(sprintf("Predicted value at stake, p10 -> p90     = %.3f -> %.3f (%.1fx)\n",
            quantile(pred, .10), quantile(pred, .90),
            quantile(pred, .90) / quantile(pred, .10)))
cat("\nRead: a flat weight is defensible only if the context-predicted spread\n")
cat("is small relative to the overall spread. If p90/p10 of the PREDICTED\n")
cat("value is large, the flat weight is discarding recoverable signal and\n")
cat("WS2 (xSpoil) has headroom. If it is ~1x, WS2 collapses into WS3.\n")

# --- what share is already context-priced via contest_epv? ---------------
cat("\n--- coverage check: spoils already valued contextually ---\n")
cat("compute_contest_credit() gives aerial-contest defenders -delta_epv/3,\n")
cat("which IS context-sensitive. The flat EPV_SPOIL_WT is applied on top,\n")
cat("from the box score, to ALL spoils. Overlap matters for sizing WS2.\n")
pgd <- rbindlist(lapply(SEASONS, function(s)
  as.data.table(read_parquet(file.path(DATA_DIR, sprintf("player_game_%d.parquet", s))))),
  use.names = TRUE, fill = TRUE)
cov <- pgd[, .(spoils = sum(spoils, na.rm = TRUE),
               aerial_def = sum(aerial_def_wins + aerial_def_losses, na.rm = TRUE))]
cat(sprintf("  box-score spoils            : %s\n", format(cov$spoils, big.mark = ",")))
cat(sprintf("  aerial contests w/ defender : %s\n", format(cov$aerial_def, big.mark = ",")))
cat(sprintf("  ratio                       : %.2f contest-priced per spoil\n",
            cov$aerial_def / cov$spoils))
