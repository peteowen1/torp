# torp#247: finals `home_win_prob` does not follow `pred_margin`

Issue: https://github.com/peteowen1/torp/issues/247

Line numbers are for branch `claude/torp-247-finals-win-prob-wcor2e` (based on
`origin/dev` at 3b800bf). Everything up to `R/match_model.R:1011` is unchanged
from `dev`; later lines in that file are shifted by the fix.

## Short answer

Finals rows and home-and-away rows go through **the same code path** in torp.
There is no finals-specific branch and the published `predictions.parquet` does
not read the matchup table. The difference is between the two *columns*, not
between two row types:

- `pred_margin` comes from the margin chain (GAM model 4 + XGBoost, blended,
  then recalibrated).
- `pred_win` (published as `home_win_prob`) comes from a **separate binomial
  GAM** (model 5) that takes the margin as one input among several. Nothing
  forces its output to follow the margin.

On H&A rows the two agree closely enough that nobody noticed. On finals rows
they do not, and the existing safety net (`.reconcile_win_with_margin()`)
only acts when the two point in opposite directions by more than 2 points of
probability, which the reported −6.6 / 0.496 row does not.

The blog's diagnosis ("the finals probability comes from the matchup-table
pricing path") does not match the code for `predictions.parquet`. The matchup
table is a separate artifact (`matchup_table_<season>.parquet`) used by the
site's season simulator for hypothetical finals ties; see "Matchup table"
below. It shares the same win-GAM design and so has the same weakness, but it
is not where `home_win_prob` comes from.

## Where each column is produced

### 1. Model frame and training (`R/match_train.R`)

- Model 4, margin: `R/match_train.R:419-455`. Gaussian GAM on `score_diff`,
  with team random effects, `s(team_type_fac)` (home advantage), rating diffs,
  travel, familiarity, rest.
- Model 5, win: `R/match_train.R:462-474`.

  ```r
  win ~ s(team_name.x, re) + s(team_name.y, re)
      + s(team_name_season.x, re) + s(team_name_season.y, re)
      + ti(pred_tot_xscore, pred_score_diff, k = 4)
      + s(pred_score_diff, ts, k = 5)
      + s(log_dist_diff) + s(familiarity_diff) + s(days_rest_diff_fac, re)
  ```

  Margin enters through `s(pred_score_diff)` and the `ti(total, margin)`
  interaction. Team, team-season, travel, familiarity and rest terms enter a
  **second time**, independently of the margin model that already used them.
  Nothing constrains the fitted function to be monotone in `pred_score_diff`.

### 2. Prediction (`R/match_model.R`, `build_prediction_state()`)

- `R/match_model.R:977-985`: blended margin
  `pred_score_diff = .blend_gam_xgb(gam, xgb)`.
- `R/match_model.R:986-988`: `pred_win` = model 5 re-fed the blended margin and
  total.
- `R/match_model.R:1003-1004`: margin recalibration
  `pred_score_diff <- b * pred_score_diff` (`R/match_calibration.R:126`),
  applied **after** `pred_win` was derived. A positive scale preserves sign,
  so this changes the spacing between margins but cannot by itself put a
  negative margin at ≥0.5.
- `R/match_model.R:1005`: `.reconcile_win_with_margin()`
  (`R/match_model.R:1710`). It replaces `pred_win` with
  `pnorm(margin / sigma)` only where
  `abs(margin) > 1 & abs(pred_win - 0.5) > 0.02 & signs differ`
  (`R/match_model.R:1712`). A home side at −6.6 with 0.496 is 0.004 from 0.5,
  so it is left alone.
- `R/match_model.R:1011`: `.format_match_preds()` (`R/match_model.R:278`)
  flips the away row (`pred_win = 1 - pred_win`, line 303) and averages it with
  the home row (`pred_margin = mean(pred_margin), pred_win = mean(pred_win)`,
  line 331). Same for every row type.
- `R/match_model.R:1015`: `week_gms` is a filter of `all_preds`; the
  direction check at `R/match_model.R:1028-1042` uses the same 0.02
  tolerance (`meaningful`, line 1032), so the −6.6 / 0.496 row passes
  validation and is published.
- `R/match_model.R:1223`: retrodictions are also taken from `all_preds`.

### 3. Rename to `home_win_prob` (torpdata, not this repo)

`peteowen1/torpdata` `scripts/build_blog_data.R` (checked at d5443af):
`home_win_prob = round(pmin(pmax(pred_win, 0.001), 0.999), 3)` at lines 110
(legacy format), 128 (current format) and 169 (retrodiction backfill). The
same expression for every row, finals or not. `game_type` (finals vs regular)
is attached later (line ~843) and does not change `home_win_prob`.

## Why finals rows in particular

The code above is the same for all rows; finals are where the win GAM's
non-margin terms are most likely to outweigh the margin. The mechanisms,
with what can and cannot be shown without the data releases:

1. **The `ti(pred_tot_xscore, pred_score_diff)` interaction flattens the
   margin slope at unusual totals.** Already measured once:
   `NEWS.md` (torp 1.9.2) and the doc comment at `R/match_model.R:1688-1702`
   record 2026 R29 (Grand Final, Fremantle v Brisbane) at margin −10.4 with
   win 0.540, at an expected total of 189, above the 99th percentile of
   training, where "the interaction flattens the margin's effect". The issue's
   +12.1 and +18.4 both pricing at ~0.658 is what a flattened slope looks
   like. *Not re-measured for the issue's rows: needs the fitted models.*
   `data-raw/05-validation/diagnose_margin_win_disagreement.R` splits the win
   GAM into its terms for any `MATCH_ID` and is the tool to confirm it.
2. **Team-season random effects counted twice.** `s(team_name_season.*)`
   appears in both the margin model and the win model. A sizeable team-season
   effect in the win model can pull a −6.6 underdog to ~0.5 even when the
   margin model has already accounted for that team. Finals feature the
   strongest team-seasons, where these effects are largest. *Not verified
   for the specific rows.*
3. **Rest and venue terms outside their usual range.** `days_rest_diff_fac`
   is capped to −4..4 (`R/match_data_prep.R:889`) and finals produce the
   extreme levels (a team off a week's break vs one that played six days
   earlier). Neutral-venue finals (MCG for non-Victorian teams) change
   `log_dist_diff`/`familiarity_diff`. Both enter the win GAM independently
   of the margin. *Not verified for the specific rows.*

Whichever term dominates for a given row, the structural cause is the same:
the published probability is produced by a model that is not a function of
the published margin, and the only consistency check tolerates small
contradictions.

## Matchup table (related, not the source of `home_win_prob`)

`build_matchup_table()` (`R/matchup_table.R:813`) prices every hypothetical
(host, visitor, venue) tie with `.predict_match_model()`
(`R/matchup_table.R:640`). It uses the same win GAM
(`R/matchup_table.R:749`), applies the margin calibration afterwards (line
750), averages host and flipped visitor rows into `p_home` and `pred_margin`
(lines 841-847), and **does not call `.reconcile_win_with_margin()` at all**.
So `p_home` can contradict `pred_margin` there too, with no tolerance check.
Its output goes to `matchup_table_<season>.parquet`, which the site's
`season-sim.js` reads for finals odds; it is not merged into
`predictions.parquet`. The fix below does not touch it.

## Fix applied on this branch

`R/match_model.R`:

- `.margin_residual_sd()` (`R/match_model.R:1747`): the residual SD of the
  margin model on completed rows, factored out of
  `.reconcile_win_with_margin()` (whose behaviour and warning are unchanged).
- `.finals_win_from_margin()` (`R/match_model.R:1779`): on match-level rows
  whose `round` is above the season's last H&A round
  (`AFL_REGULAR_SEASON_ROUNDS`, falling back to `AFL_MAX_REGULAR_ROUNDS`),
  sets `pred_win <- pnorm(pred_margin / sigma)`. Wired in at
  `R/match_model.R:1012`, right after `.format_match_preds()`, so it covers
  the locked predictions and the retrodictions.

Because it runs after the home/away averaging, finals `pred_win` is a strictly
increasing function of the published `pred_margin`, equal to 0.5 at 0, below
0.5 for every negative margin. H&A rows are unchanged.

The rule is the one `.reconcile_win_with_margin()` already uses for
contradicting rows (torp 1.9.2), with the same sigma. For scale: the 1.9.2
run measured sigma = 33.1. With that value, −6.6 → 0.421, +12.1 → 0.643,
+18.4 → 0.711; the H&A model gives +21.3 → 0.731, and pnorm(21.3 / 33.1) =
0.740, so the two rules sit close together on that H&A example. The blog
stopgap uses sigma = 26 (`SIM_NOISE_SD`), which gives sharper numbers
(0.400 / 0.679 / 0.760). Which spread is better calibrated for finals is an
empirical question this branch does not answer.

`tests/testthat/test-finals-win-from-margin.R` checks, on synthetic rows, that
finals `pred_win` is increasing in `pred_margin`, below 0.5 when it is
negative, that H&A rows are untouched, the fallback for a season missing from
`AFL_REGULAR_SEASON_ROUNDS`, the no-sigma path, and `.margin_residual_sd()`.

## What this does not prove

- That pnorm with the run's residual SD is better calibrated on finals than
  the win GAM. Needs the release data: compare log-loss/Brier of both on
  historical finals (2021-2026), e.g. from `retrodictions_*`.
- Which of the three mechanisms produced the issue's rows. Run
  `diagnose_margin_win_disagreement.R` with each `MATCH_ID`.
- The site: once a run with this change publishes, `afl/team-maps.js`
  `aflTeamMaps.homeWinProb` can go back to the stored value (blog #658).

## Proper fix (not done here)

A win model that is monotone in the margin by construction, for all rows,
e.g. `win ~ s(pred_score_diff)` alone (or a logistic on the margin with a
spread term), with team/venue/rest effects left to the margin model where they
already live. Queued in torpverse NEXT-STEPS per the 1.9.2 note. That would
also make the matchup table consistent, which this branch does not.
