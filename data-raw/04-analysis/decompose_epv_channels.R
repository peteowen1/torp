# What is each EPV channel actually MADE OF?
# ==========================================
# Pete's question, 2026-07-30, following ws13: the multivariate points-conversion
# slopes are recv 1.03, disp 0.57, spoil 4.02, hitout 7.22. Which knobs could move
# them, and where is the mass?
#
# Reconstructs each channel from its constituent terms using the SAME weights
# production uses (default_epv_params()) and checks the reconstruction against the
# published column, so the shares below are verified rather than read off code.
#
# The structural thing this is looking for: recv and disp are part chain-EPV --
# real expected-points deltas, already in points by construction -- plus box-score
# add-ons. spoil and hitout have NO chain-EPV term at all; they are pure weighted
# box-score sums. If that is right it explains why recv sits at 1.03 while spoil
# and hitout are 4x and 7x out: nothing anchors them to points in the first place.

suppressMessages({
  library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
options(torp.local_data_dir = NA)

p <- torp:::default_epv_params()
d <- as.data.table(load_player_game_data(TRUE))
cli::cli_alert_info("{nrow(d)} player-games, seasons {min(d$season)}-{max(d$season)}")

# Term definitions, mirroring player_credit.R Step 4/5 exactly.
TERMS <- list(
  epv_spoil = c(spoils = "spoil_wt", tackles = "tackle_wt",
                pressure_acts = "pressure_wt", def_half_pressure_acts = "def_pressure_wt",
                intercepts = "intercepts_wt", one_percenters = "one_percenters_wt",
                rebound50s = "rebound50s_wt", frees_against = "frees_against_wt"),
  epv_hitout = c(hitouts = "hitout_wt", hitouts_to_advantage = "hitout_adv_wt",
                 ruck_contests = "ruck_contest_wt"),
  epv_disp = c(inside50s = "inside50s_wt", clangers = "clangers_wt",
               score_involvements = "score_involvements_wt", kicks = "kicks_wt",
               handballs = "handballs_wt", metres_gained = "metres_gained_wt",
               turnovers = "turnovers_wt", goal_assists = "goal_assists_wt",
               goals = "goals_wt", behinds = "behinds_wt",
               shots_at_goal = "shots_at_goal_wt"),
  epv_recv = c(contested_possessions = "contested_poss_wt",
               contested_marks = "contested_marks_wt",
               ground_ball_gets = "ground_ball_gets_wt",
               marks_inside50 = "marks_inside50_wt", marks = "marks_wt",
               uncontested_possessions = "uncontested_poss_wt",
               frees_for = "frees_for_wt")
)

for (ch in names(TERMS)) {
  cli::cli_h1(ch)
  tt <- TERMS[[ch]]
  contrib <- lapply(names(tt), function(stat) d[[stat]] * p[[tt[[stat]]]])
  names(contrib) <- names(tt)
  box_total <- Reduce(`+`, contrib)

  # The chain-EPV part is whatever the published channel has that the box terms
  # do not. For spoil/hitout this should be ~0 (they have no chain term);
  # recv additionally carries contest_epv, which is chain-derived.
  published <- d[[ch]]
  resid <- published - box_total
  if (ch == "epv_recv") resid <- resid - d$contest_epv

  tab <- rbindlist(lapply(names(contrib), function(nm) {
    data.table(term = nm, weight = p[[tt[[nm]]]],
               mean_abs = mean(abs(contrib[[nm]]), na.rm = TRUE))
  }))
  extra <- data.table(
    term = if (ch == "epv_recv") "CHAIN EPV (delta_epv x recv_scale)" else
           if (ch == "epv_disp") "CHAIN EPV (delta_epv x disp_scale)" else
           "UNEXPLAINED (expect ~0)",
    weight = if (ch == "epv_recv") p$recv_scale else if (ch == "epv_disp") p$disp_scale else NA_real_,
    mean_abs = mean(abs(resid), na.rm = TRUE))
  if (ch == "epv_recv") {
    extra <- rbind(extra, data.table(term = "contest_epv (3-way contest, chain)",
                                     weight = p$contest_share %||% (1/3),
                                     mean_abs = mean(abs(d$contest_epv), na.rm = TRUE)))
  }
  tab <- rbind(tab, extra)
  tab[, share := round(mean_abs / sum(mean_abs), 3)]
  tab[, mean_abs := round(mean_abs, 4)]
  print(tab[order(-share)], row.names = FALSE)

  # Reconstruction check. For spoil/hitout the box terms are the WHOLE channel,
  # so a non-zero residual means the term list here is wrong and every share
  # above is wrong with it.
  if (ch %in% c("epv_spoil", "epv_hitout")) {
    worst <- max(abs(resid), na.rm = TRUE)
    cli::cli_alert_info("reconstruction: max |published - sum(terms)| = {signif(worst, 3)}")
    if (worst > 1e-8) cli::cli_alert_danger("Term list does NOT reproduce {ch} -- shares above are unreliable.")
  }
}

cli::cli_h1("how much of each channel is chain-EPV vs box-score?")
# This is the headline. A channel with no chain-EPV component has nothing holding
# it to points units, which is the cheapest explanation of ws13's 4x and 7x.
summ <- rbindlist(lapply(names(TERMS), function(ch) {
  tt <- TERMS[[ch]]
  box <- Reduce(`+`, lapply(names(tt), function(s) d[[s]] * p[[tt[[s]]]]))
  chain <- d[[ch]] - box
  if (ch == "epv_recv") chain <- chain   # contest_epv is chain-derived, keep it in
  data.table(channel = ch,
             mean_abs_channel = round(mean(abs(d[[ch]]), na.rm = TRUE), 4),
             pct_from_chain_epv = round(100 * mean(abs(chain), na.rm = TRUE) /
                                          (mean(abs(chain), na.rm = TRUE) + mean(abs(box), na.rm = TRUE)), 1),
             pct_from_box_score = round(100 * mean(abs(box), na.rm = TRUE) /
                                          (mean(abs(chain), na.rm = TRUE) + mean(abs(box), na.rm = TRUE)), 1))
}))
print(summ, row.names = FALSE)
cli::cli_alert_success("done")
