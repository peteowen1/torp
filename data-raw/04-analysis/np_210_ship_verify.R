# Does the SHIPPED code produce the arm that was measured? (issue #210)
# =============================================================================
# Every gate in this repo checks "is it better". None checks "is it the SAME
# THING" -- and that gap has cost half a headline gain before. The clean arm was
# measured by monkey-patching fit_disposal_models() in np_leak_effect_on_ratings.R;
# the shipped fix edits the real function. Those are two different pieces of code
# and they have to be shown to agree.
#
# The measured CLEAN arm, production regime (fit 2025, score 2026):
#
#   rows at p_hat >= 0.99 : 18
#   max p_hat             : 1.00000
#
# and against the LEAKY arm it was 768 rows and a far heavier upper tail. If the
# shipped code lands on those numbers, the arm is the arm. If it lands on the
# leaky ones, the edit did not take.
#
# ALSO CHECKED, because a formula edit can go wrong in a way that still "works":
# that the two dropped terms are genuinely absent from all three fitted models,
# read off the fits themselves rather than off the source.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_210_ship_verify.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON))

say("=== 1. are the leaky terms gone from the FITTED models? ===")
say("(read off the fit objects, not the source -- the source is what I edited)")
de <- as.data.table(build_disposal_events(ch, pbp))
mods <- fit_disposal_models(de)
for (nm in names(mods)) {
  tms <- attr(stats::terms(stats::formula(mods[[nm]])), "term.labels")
  leaky <- grep("kick_len|fwd_gain", tms, value = TRUE)
  say("  ", nm, ": ", paste(tms, collapse = " + "))
  say("      leaky terms present: ", if (length(leaky)) paste(leaky, collapse = ", ") else "NONE")
}
stopifnot(!any(grepl("kick_len|fwd_gain",
                     unlist(lapply(mods, function(m)
                       attr(stats::terms(stats::formula(m)), "term.labels"))))))

say("\n=== 2. are the columns still BUILT (they define the row population)? ===")
say("  kick_len present in disposal table: ", "kick_len" %chin% names(de))
say("  fwd_gain present in disposal table: ", "fwd_gain" %chin% names(de))
say("  disposals scored: ", format(nrow(de), big.mark = ","))
stopifnot("kick_len" %chin% names(de), "fwd_gain" %chin% names(de))

say("\n=== 3. production regime: fit on ", SEASON - 1, ", score ", SEASON, " ===")
tm <- as.data.table(np_difficulty_terms_for_season(SEASON, pbp_data = pbp, chains = ch))
say("  rows                : ", format(nrow(tm), big.mark = ","))
say("  p_hat median        : ", round(median(tm$p_hat), 4))
say("  p_hat p95           : ", round(quantile(tm$p_hat, .95), 4))
say("  p_hat max           : ", round(max(tm$p_hat), 5))
say("  rows at p >= 0.99   : ", tm[p_hat >= 0.99, .N],
    "   (measured for CLEAN: 18;  LEAKY was 768)")
say("  rows at p >= 0.999  : ", tm[p_hat >= 0.999, .N])

say("\n=== VERDICT ===")
n99 <- tm[p_hat >= 0.99, .N]
if (n99 <= 40) {
  say("  MATCHES the measured clean arm (", n99, " rows at p >= 0.99).")
  say("  The saturation behind #209 is ~", round(100 * (1 - n99 / 768)), "% reduced, NOT eliminated --")
  say("  the defensive clamp stays.")
} else {
  say("  *** DOES NOT MATCH. ", n99, " rows at p >= 0.99 against 18 measured. ***")
  say("  The shipped edit is not the arm that was scored. Stop and find out why.")
}
