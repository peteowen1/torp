# Why does the win model disagree with the margin model for a match?
# =============================================================================
# build_prediction_state() refuses to publish when a match's predicted margin
# and win probability point opposite ways (match_model.R, the direction check).
# 2026 R29: Fremantle v Brisbane (CD_M20260142901), margin -10.4, win 0.533.
# The margin is only rescaled after pred_win is derived (sign-preserving), so
# the disagreement is inside the win GAM, which carries team, team-season,
# travel, familiarity and rest terms on top of the margin. This splits the win
# model's linear predictor into its terms for the failing match, run in the
# debug path that returns the fitted models instead of aborting.
#
#   powershell.exe -Command 'Rscript "data-raw/05-validation/diagnose_margin_win_disagreement.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
MATCH <- Sys.getenv("MATCH_ID", "CD_M20260142901")
say <- function(...) cat(..., "\n", sep = "")

# The build fits every match model (about 10 minutes), so keep what the
# analysis needs and reuse it on a rerun.
CACHE <- "data-raw/outputs/margin_win_diag.rds"
if (file.exists(CACHE)) {
  k <- readRDS(CACHE)
} else {
  st <- testthat::with_mocked_bindings(
    build_prediction_state(),
    interactive = function() TRUE, .package = "base")
  say("validation errors: ", length(st$validation_errors))
  k <- list(win = st$gam_result$models$win, df = as.data.table(st$team_mdl_df),
            week_gms = st$week_gms, errors = st$validation_errors)
  saveRDS(k, CACHE)
}
d <- k$df[match_id == MATCH]
stopifnot(nrow(d) == 2)
m <- k$win
tt <- predict(m, newdata = d, type = "terms")
lp <- predict(m, newdata = d, type = "link")
out <- data.table(team = d$team_name.x, side = d$team_type_fac.x,
                  pred_score_diff = round(d$pred_score_diff, 2),
                  pred_win = round(plogis(lp), 3), intercept = round(as.numeric(lp - rowSums(tt)), 3))
out <- cbind(out, round(as.data.table(tt), 3))
say("\nwin model, link scale, per team row (positive = favours the row's team):")
print(t(out))
say("\ninputs behind the non-margin terms:")
print(d[, .(team_name.x, team_name.y, log_dist_diff, familiarity_diff, days_rest_diff_fac,
            team_name_season.x, team_name_season.y)])
