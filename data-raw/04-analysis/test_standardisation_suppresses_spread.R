# Does EPV_POSITION_STANDARDISE delete legitimate between-position spread?
# =======================================================================
# Hypothesis, 2026-07-30: the defender under-dispersion (~1.65x) may be caused by
# EPV_POSITION_STANDARDISE rather than by any weight. `.position_adjust()` does
#
#     adj = (p80 - wmean(p80)) / s * pooled_sd * tog        s = within-position SD
#
# so `adj / tog` has within-position SD of EXACTLY pooled_sd, for every position, by
# construction. That is the flag's stated purpose ("between-position spread
# differences are removed"). If key defenders genuinely have more spoil-value spread
# than midfielders, this deletes that signal by design.
#
# TESTABLE WITHOUT A REBUILD: player_game publishes both the raw channel (epv_spoil)
# and the adjusted one (epv_spoil_adj), so the before/after of the very operation in
# question is already on disk.
#
# Two predictions:
#   1. within-position SD of (adj / tog) is the SAME for every position (~pooled_sd)
#   2. within-position SD of (raw / tog) is NOT -- and the question that decides the
#      hypothesis is whether KEY DEFENDERS are among the positions losing spread.
#
# If defenders had HIGH natural spread and it was equalised down, the flag is a cause
# of their under-dispersion. If their natural spread was already average or low, it is
# not, and the weights are the whole story.

suppressMessages({
  library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
options(torp.local_data_dir = NA)

SEASONS <- 2024:2026
d <- as.data.table(load_player_game_data(SEASONS))
cli::cli_alert_info("{nrow(d)} player-games")

# The standardisation is keyed on lineup_position inside create_player_game_data().
# Check what position keys survived into the published frame.
poskeys <- intersect(c("lineup_position", "position_group", "pos_group"), names(d))
cli::cli_alert_info("position columns present: {paste(poskeys, collapse=', ')}")
KEY <- if ("lineup_position" %in% poskeys) "lineup_position" else "position_group"
if (KEY != "lineup_position") {
  cli::cli_alert_warning("lineup_position is NOT in player_game -- using {.field position_group} instead.")
  cli::cli_alert_warning("That is a COARSER key than the one standardisation actually used, so the")
  cli::cli_alert_warning("equalisation will look imperfect even if it is exact on the real key. Noted, not fatal.")
}

CH <- c("epv_recv", "epv_disp", "epv_spoil", "epv_hitout")
d[, tog_safe := pmax(dplyr::coalesce(time_on_ground_percentage / 100, 0.1), 0.1)]
D <- d[!is.na(get(KEY)) & is.finite(tog_safe) & tog_safe > 0]

cli::cli_h1("1. did standardisation equalise the spread? (prediction: post-SDs identical)")
res <- rbindlist(lapply(CH, function(ch) {
  adj <- paste0(ch, "_adj")
  if (!all(c(ch, adj) %in% names(D))) return(NULL)
  x <- D[is.finite(get(ch)) & is.finite(get(adj))]
  x[, `:=`(pre = get(ch) / tog_safe, post = get(adj) / tog_safe)]
  out <- x[, .(n = .N, sd_pre = sd(pre), sd_post = sd(post)), by = c(KEY)]
  out[, channel := ch]
  out
}))
for (ch in unique(res$channel)) {
  r <- res[channel == ch & n >= 300][order(-sd_pre)]
  if (!nrow(r)) next
  cli::cli_h2(ch)
  print(r[, .(pos = get(KEY), n, sd_pre = round(sd_pre, 3), sd_post = round(sd_post, 3),
              ratio = round(sd_post / sd_pre, 3))], row.names = FALSE)
  cv_pre  <- sd(r$sd_pre)  / mean(r$sd_pre)
  cv_post <- sd(r$sd_post) / mean(r$sd_post)
  cli::cli_alert_info("spread-of-spreads (CV across positions): pre {round(cv_pre, 3)} -> post {round(cv_post, 3)}")
  if (cv_post < 0.5 * cv_pre) {
    cli::cli_alert_success("Between-position spread differences COMPRESSED by this channel's adjustment.")
  } else {
    cli::cli_alert_info("Between-position spread differences largely PRESERVED here.")
  }
}

cli::cli_h1("2. the question that decides it: do key defenders LOSE spread?")
# hitout is excluded from EPV_STANDARDISE_CHANNELS, so it is the control: if the
# compression pattern appears in recv/disp/spoil but not hitout, the flag is the cause.
cli::cli_alert_info("EPV_STANDARDISE_CHANNELS = {paste(torp:::EPV_STANDARDISE_CHANNELS, collapse=', ')} (hitout EXCLUDED -- the control)")
def_like <- grep("KEY_DEF|key_def|FB|CHB|BP", unique(as.character(D[[KEY]])), value = TRUE)
cli::cli_alert_info("defender-like {KEY} values: {paste(def_like, collapse=', ')}")
for (ch in unique(res$channel)) {
  r <- res[channel == ch & n >= 300]
  if (!nrow(r)) next
  r[, is_def := get(KEY) %in% def_like]
  if (!any(r$is_def)) next
  cli::cli_alert_info("{ch}: defenders sd_pre {round(mean(r[is_def == TRUE]$sd_pre), 3)} vs others {round(mean(r[is_def == FALSE]$sd_pre), 3)}; ")
  cli::cli_alert_info("      post: defenders {round(mean(r[is_def == TRUE]$sd_post), 3)} vs others {round(mean(r[is_def == FALSE]$sd_post), 3)}")
  rel_pre  <- mean(r[r$is_def == TRUE]$sd_pre)  / mean(r[r$is_def == FALSE]$sd_pre)
  rel_post <- mean(r[r$is_def == TRUE]$sd_post) / mean(r[r$is_def == FALSE]$sd_post)
  cli::cli_alert_info("      defender/other spread ratio: {round(rel_pre, 3)} -> {round(rel_post, 3)}")
  if (rel_pre > 1.1 && rel_post < rel_pre * 0.9) {
    cli::cli_alert_danger("{ch}: defenders HAD more spread and it was equalised away -- flag is implicated.")
  } else if (rel_pre <= 1.1) {
    cli::cli_alert_info("{ch}: defenders did not have more natural spread, so nothing was taken from them here.")
  }
}

cli::cli_h1("3. how often does the standardisation fallback fire?")
# .position_adjust() falls back to centre-only when the within-position SD is
# degenerate. If that fires often, the flag does less than the algebra suggests.
cli::cli_alert_info("Cannot be observed from published data -- it is internal to")
cli::cli_alert_info("create_player_game_data(). Flagged as the one part of this test that needs a rebuild.")

saveRDS(res, "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-torpverse/92e2b422-0dee-4727-90de-364d23375767/scratchpad/standardisation_test.rds")
cli::cli_alert_success("done")
