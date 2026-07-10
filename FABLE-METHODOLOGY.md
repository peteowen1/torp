# FABLE-METHODOLOGY.md — EP/WP Modelling Methodology Review

**Scope**: AFL Expected Points (EP) and Win Probability (WP) models in torpverse — training scripts
(`torpmodels/data-raw/01-ep-model`, `02-wp-model`, `04-match-model`, `05-live-wp-model`), feature
engineering in `torp/R/`, the debug/calibration scratch scripts, and the live JSON serving path in
`inthegame-blog/worker/src/`. Pipeline/caching/release code is covered separately in
`torp/FABLE-REVIEW.md` and is not re-reviewed here.

**Reviewer stance**: skeptical peer review — models assumed subtly wrong until proven otherwise.
Everything below is from static reading of the code + training logs + file/release timestamps; no
model was retrained. Section 3 gives the experiments that would confirm or kill each suspicion.

---

## 1. Summary verdict

The foundations are better than most hobby sports models — every XGBoost training script uses
**match-grouped CV folds** (no naive row-level splits), the match model has a genuine **rolling
week-by-week temporal evaluation** with a Squiggle benchmark and calibration bins, and the team
knows about the EP→WP leakage (it's documented in three places). But the execution has four
systemic problems:

1. **The EP→WP in-sample leakage is not a "1–2% metrics" footnote — it contaminates every WP
   evaluation ever run**, including the "honest OOS" comparisons in `data-raw/debug/` that drove
   feature and hyperparameter selection. The WP model's most important features
   (`xpoints_diff`, `pos_lead_prob`, `diff_time_ratio`) are all functions of EP predictions from a
   model that was trained on the very rows being evaluated. The fix
   (`train_wp_model_cv_ep.R`) exists but the shipped `wp_model.rds` almost certainly did not come
   from it (timestamp evidence below), and its promised validation (`validate_cv_ep_wp.R`) has no
   recorded output. The "~1–2% optimistic" figure appears to be an assertion, not a measurement.

2. **Calibration was only ever checked in-sample.** The two calibration scratch scripts
   (`calibration_check.R`, `train_best_and_calibrate.R`) predict with models trained on the same
   rows they tabulate. An overfit model looks perfectly calibrated in-sample, so these tables were
   structurally incapable of detecting the one defect the team has actually observed in production
   — "the WP model's gradient is too steep in close/late situations". Excluding WPA from the TORP
   blend is a workaround for a measurable, plausibly fixable calibration defect, and the steepness
   is partly *engineered in* (exponential time features + monotone constraints + leaked-sharp EP
   inputs), not an inherent property of WP models.

3. **No temporal holdout exists anywhere in the EP/WP stack.** All selection — features,
   hyperparameters, nrounds, "game time vs raw clock", v1 vs v2 live features — was done on the
   same seed-1234 random match partition of 2021–2026, reused across at least six experiment
   scripts. That partition is exhausted as an unbiased estimator. (The match model is the honorable
   exception: rolling 2025–2026 evaluation, nrounds tuned on pre-2025 only.)

4. **The live serving path has real train/serve skew**: the chain-aware live WP (v4) was trained
   on `exp_pts` from the full 19-feature core EP model but is served `exp_pts` from the 13-feature
   live EP model (plus a silent `0` fallback), on a feature that carries **91.5% of the model's
   gain**. Its own training-log sanity check prints a tied-game Q4 possession WP of **39.5%** and a
   40-point WP jump for 3 points of expected margin — the steep-gradient defect, visible at export
   time, shipped anyway.

Verdict: **EP is probably fine; WP (core and live v4) should not be trusted for close/late-game
inference until it is retrained on cross-validated EP features and re-validated on a temporal
holdout with match-level reliability curves.** The WPA exclusion from TORP is prudent given
current calibration, but the underlying defect looks fixable.

---

## 2. Findings

### Critical

#### C1. WP trained on in-sample EP predictions — and every WP evaluation shares the leak

- **Training**: `torpmodels/data-raw/02-wp-model/train_wp_model.R:37-43` — WP features come from
  `torp::add_epv_vars()`, which loads the production EP model
  (`torp/R/add_variables.R:26,192-237` — `get_epv_preds()` → `load_model_with_fallback("ep")`)
  trained on *all* seasons. Every WP training row's EP features were produced by a model that saw
  that row's `label_ep` (the realized next score) during training.
- **Mechanism, and why it maps onto the observed "too steep" symptom**: EP's label is the next
  score; WP's label is the match winner. In-sample EP predictions are sharper — they lean toward
  the *realized* next score (`validate_cv_ep_wp.R:98-105` measures exactly this as
  "mean max-class prob" but was never run to a recorded conclusion). The realized next score is
  strongly informative of the final winner precisely in close/late states. So WP learns to trust
  `xpoints_diff` / `diff_time_ratio` / `pos_lead_prob` more than honest inputs would justify, and
  at serve time (where EP is genuinely OOS and noisier) WP is overconfident — *steepest exactly in
  close/late situations*, which is the documented reason WPA is excluded from TORP
  (`torpverse/CLAUDE.md` "TORP Composition"; `torp/CLAUDE.md` Key Constants).
- **The leak contaminates the WP CV metric itself**, not just the headline number: in
  `train_wp_model.R:67-92` the WP CV test folds contain rows whose EP-derived features encode
  those rows' outcomes. This is stronger than "optimistic by reuse" — it is feature-borne label
  leakage into the test fold.
- **All downstream evaluations inherit it**: `data-raw/debug/compare_wp_subsets.R:21-23` builds
  `base` via `add_epv_vars()` (in-sample EP) before running its otherwise-honest 5-fold OOS WP
  comparison (:92-122). `train_best_and_calibrate.R` uses a cached `wp_compare_base.rds` built the
  same way. So the "game time vs raw clock" decision, the `score_urgency`/`goal_x` feature adds,
  and the eta/depth choices were all selected on leaked features.
- **Magnitude unverified**: the "~1–2% optimistic" claim (`train_wp_model.R:38`,
  `torpmodels/ARCHITECTURE.md:169`) has no recorded measurement. `validate_cv_ep_wp.R` was written
  (2026-03-26) to measure it; no output/log exists in `data-raw/02-wp-model/`. Note also that even
  1–2% *logloss* optimism can coexist with much larger *calibration-slope* distortion in the
  close/late subset, which logloss averaged over all rows barely sees. Treat the 1–2% figure as
  unsubstantiated until Experiment E1 is run.

#### C2. The CV-EP fix exists but the shipped model almost certainly didn't use it

- `train_wp_model_cv_ep.R` (correct nested design: OOS EP preds via the same match folds,
  :89-137) and `train_wp_model.R` both write **the same output path**
  (`inst/models/core/wp_model.rds`, :207 vs :110) and upload to the same release asset — nothing
  records which script produced the published artifact.
- **Timestamp evidence points to the in-sample path**: local mtimes show
  `ep_model.rds` 2026-06-18 04:02:30, `wp_model.rds` 04:03:26, `shot_*.rds` 04:04:02 — a
  sequential batch run with only **56 seconds** between the EP save and the WP save. The CV-EP
  script must run the EP CV *plus retrain five EP fold models* before WP training even starts
  (:73-121); that cannot fit in 56s on ~1.8M rows. The release assets
  (`gh release view core-models`) show `ep_model.rds` and `wp_model.rds` uploaded together at
  2026-06-18T11:33Z. Docs agree: `torpmodels/ARCHITECTURE.md:134` lists `train_wp_model.R` as
  *the* WP training script and frames `train_wp_model_cv_ep.R` as "for true OOS eval" only
  (:156, :169); `torpmodels/CLAUDE.md:27-29` likewise.
- Additionally, even the CV-EP variant selects nrounds and evaluates on a **random** match
  partition of 2021–2026 (`train_wp_model_cv_ep.R:63-70,167-173`) — there is still no temporal
  split (see H1).

#### C3. Live chain WP (v4): train/serve skew on the feature carrying 91.5% of model gain

Training (`torpmodels/data-raw/05-live-wp-model/train_live_wp_chain_v4.R`):
- :46-52 — `exp_pts` comes from `torp::add_epv_vars()`, i.e. the **full 19-feature core EP model**,
  with its special-casing: `exp_pts = 0` forced on Centre Bounce rows and the negated-lead
  override on "Out On Full After Kick" (`torp/R/add_variables.R:37-41`).
- :84 — `xmargin = margin_poss + exp_pts`; training log (`training_v4.log`) shows
  `xmargin: Gain=91.5%`.
- Also in-sample: the EP model providing `exp_pts` was trained on all 1,116 matches in the v4
  training set (same C1 leak, now in the live model).

Serving (`inthegame-blog/worker/src/`):
- `index.js:2092-2098` — the Worker feeds `r.ep` as the `exp_pts` feature, where `r.ep` is produced
  by the **13-feature live EP v2 model** (`ep-model.js:32-47,104-122`), a different model with a
  different feature set and different (CV-selected) sharpness.
- `wp-model.js:363` — `const expPts = typeof r.ep === "number" ? r.ep : 0` — silent 0 fallback for
  any row the EP scorer skipped (missing coords, non-relevant description), whereas training rows
  always had a full-model prediction.
- No Centre-Bounce zeroing or Out-On-Full negation at serve — the Worker scores Centre Bounce with
  the live EP model directly (`ep-model.js:142-149` includes it in `EPV_RELEVANT`).
- Net effect: the serve-time `xmargin` distribution is shifted/noisier relative to training on the
  single feature the model almost entirely depends on. Unlike football (which has
  `validate-football-wpa.mjs` as a deploy regression gate — see `inthegame-blog/CLAUDE.md`), **AFL
  has no train/serve parity harness at all.**

Calibration defect visible in the training log itself (`training_v4.log`, sample-state check):
- "Q4 30min, home poss, **tied** (xm=0)" → **39.5%** — the team *with the ball* in a tied game
  shown as a 3:2 underdog. (Q2-end tied home-poss → 53.3%, away-poss → 48.3%; the Q4 number is an
  outlier, not a home/away artifact.)
- "Q4 30min, home poss, tied, about to score (xm=+3)" → **80.2%** — a 40-point WP swing for 3
  points of expected margin. This is the steep-gradient defect, printed at export time, shipped
  anyway (`wp-model-chain.json` overwrote v3; `torpmodels/CLAUDE.md:37` marks v4 as current).
- Also note the fold spread in the same log: CV test logloss 0.4592 **±0.031** across folds —
  the model-selection deltas chased elsewhere (0.001–0.005) are inside this noise (see H3).

### High

#### H1. No temporal holdout; one random partition (seed 1234) exhausted by repeated selection

- Every EP/WP training and comparison script draws the *same* 5-fold match partition:
  `set.seed(1234); sample(rep(1:5, ...))` over `torp_match_id` — `train_ep_model.R:62-67`,
  `train_wp_model.R:67-74`, `train_wp_model_cv_ep.R:63-70`, `train_ep_model_live_v2.R:75-81`,
  `debug/compare_wp_subsets.R:85-91`, `debug/train_best_and_calibrate.R:20-26` (v4 uses seed 42,
  `train_live_wp_chain_v4.R:114-119`). Feature sets, monotone constraints, eta, depth, time
  encodings, and nrounds were all chosen by comparing test-fold logloss on this one partition
  across many experiments. After that many comparisons the partition's test error is no longer an
  unbiased estimate — a fresh temporal holdout is required to know what any of these models
  actually do OOS.
- Random match folds mix 2021 and 2026 matches freely. AFL rule/style drift (e.g. rolling
  interpretation changes since 2021) is averaged away rather than measured; no script reports
  metrics train-on-past/test-on-future for EP or WP. The utilities to do this **already exist and
  are tested but unused**: `torp/R/model_validation.R:106-124` `create_temporal_splits()` is
  referenced only by `tests/testthat/test-model-validation.R` — never by a training script.
  Validation infrastructure as theater.
- Contrast: the match model does this right — `train_match_models.R:49-60` tunes nrounds only on
  pre-2025 seasons (commit `2a1b013` "avoid leakage"), :164-219 rolls week-by-week through
  2025–2026 training strictly on the past, and :313-343 prints calibration bins on those OOS
  predictions. This is the template the EP/WP stack should copy.

#### H2. All calibration diagnostics are in-sample; production has no calibration check at all

- `debug/calibration_check.R:20-21` loads the shipped `inst/models/core/wp_model.rds` and :57-62
  predicts on the same `wp_compare_base.rds` rows the model was trained on; the per-time-bucket ×
  score-bin "V2 err" tables that follow are therefore in-sample reliability — blind to
  overfitting-induced steepness by construction.
- `debug/train_best_and_calibrate.R:75-76` trains `mdl_best` on **all** rows, then :98-106 builds
  its Q4 "calibration" tables from `predict(mdl_best, X_best_q4)` on those same rows. The one
  genuinely useful design choice in these scripts — deduplicating to one observation per match per
  time bucket (:123-131 in `calibration_check.R`) — is wasted on in-sample predictions.
- `evaluate_model_comprehensive()` (`torp/R/model_validation.R:141-246`) computes a calibration
  slope, but (a) it is only called from `baseline_models.R` (also not part of any training
  pipeline), and (b) the slope is estimated by unweighted OLS on decile means (:170-190), which is
  statistically weak — use a logistic recalibration slope on row/match-level data instead
  (Experiment E3).
- Nothing validates WP calibration before `piggyback::pb_upload()` ships a model.
  `torp/R/data_validation.R:440-442` checks only that `label_wp ∈ [0,1]`.

#### H3. Effective sample size illusion: ~1.8M rows are ~1,116 matches; decisions made inside the noise

- `training_v4.log`: 1,789,156 rows / 1,116 matches (2021–2026) — ~1,600 rows per match sharing a
  single `label_wp`. Row-level logloss therefore has an effective n closer to the match count, and
  the observed CV fold SD (±0.031 for v4; similar spreads implied elsewhere) confirms it.
- The debug comparisons select between models on subset-logloss differences of 0.001–0.005
  (`compare_wp_subsets.R:167-181` marks `<<<` at 0.001) with no uncertainty quantification —
  a match-level block bootstrap (Experiment E5) would likely show most of those "wins" are not
  distinguishable from zero.
- Same structural issue for EP: all rows in a (match, period, score-segment) share `label_ep`
  (`torp/R/clean_pbp.R:640-649`), so EP's CV mlogloss is also computed on heavily duplicated
  labels. Match-grouped folds prevent *leakage* (correctly implemented everywhere — verified in
  every training script), but do not fix *metric overconfidence*; nrounds selection via
  `which.min` on the mean CV curve with no 1-SE rule (`train_ep_model.R:83`) is noise-chasing at
  the margin.

#### H4. The engineered time features force the late-game steepness the team then distrusts

- `torp/R/clean_features.R:213-221`: `time_left_scaler = exp(pmin(est_match_elapsed/1200, 4))`
  (range 1 → e⁴ ≈ 54.6), `diff_time_ratio = xpoints_diff * time_left_scaler`,
  `score_urgency = points_diff / pmax(est_match_remaining/60, 1)`. Three redundant
  margin×time-amplifier encodings of the same signal, and the monotone constraint vector
  (`train_wp_model.R:53`, positions for `points_diff`, `xpoints_diff`, `pos_lead_prob`,
  `diff_time_ratio`, `score_urgency` all +1 — order matches `select_wp_model_vars()`
  `clean_features.R:252-261`) *forces* WP to be non-decreasing in each. With depth-6 trees,
  `min_child_weight = 1` (`train_wp_model.R:55` — vs 25 for EP), and leaked-sharp EP inputs (C1),
  the model is maximally equipped to be steep late and nothing in the pipeline measures whether it
  is *too* steep OOS.
- Additional time pathology: `est_match_elapsed = (period-1)*1200 + est_qtr_elapsed`
  (`clean_pbp.R:629`) is **non-monotone across quarter boundaries** whenever a quarter's play-time
  estimate exceeds the 1200s budget (elapsed jumps backward at the next quarter's start), and
  `est_match_remaining = pmax(0, 4800 - ...)` (:630) saturates at 0 while the game is still live —
  at which point `score_urgency` degenerates to raw `points_diff` and `time_left_scaler` is pinned
  at e⁴. All end-of-game states collapse into one feature cell exactly where calibration matters
  most. Not leakage — but a plausible co-driver of the late-game steepness/miscalibration.

### Medium

#### M1. Live EP v2 `chain_action_num` train/serve skew

- Training: computed **after** EPV filtering — `train_ep_model_live_v2.R:39-42` explicitly says
  "Must be computed AFTER clean_model_data_epv filtering", so it counts only EPV-relevant rows
  within a chain.
- Serving: `ep-model.js:207-213` increments `chainActionNum` for **every** row in the live stream
  (including non-relevant events like Goal/Behind/contest rows) before the `_epvRelevant` check at
  :226. Serve-time values are systematically larger than training values for the same state.
  Low-importance feature, but a free correctness fix: only increment when `_epvRelevant`.

#### M2. `score_ep()` matrix reshape in the live-v2 comparison uses `byrow = FALSE`

- `train_ep_model_live_v2.R:144-148`: `matrix(p, ncol = 5, byrow = FALSE)`. Every other script
  handles the XGBoost <3.x flat-vector return with `byrow = TRUE` (row-major), e.g.
  `add_variables.R:217-221`, `train_wp_model_cv_ep.R:114-118`. Under xgboost 3.x (matrix return)
  this is a no-op; under an older xgboost it silently scrambles class probabilities in the
  v1/v2/full comparison tables that justified shipping v2. The shipped model itself is unaffected,
  but the evidence used to select it may be.

#### M3. `shot_row` semantic mismatch between R and Worker

- `torp/R/clean_pbp.R:90`: `shot_row = fifelse(is.na(shot_at_goal), 0L, 1L)` — an explicit
  `shot_at_goal == FALSE` becomes **1**. Worker (`ep-model.js:244`, `wp-model.js:382`):
  `r.shot_at_goal ? 1 : 0` — `FALSE` becomes **0**. Harmless if the API only ever emits
  TRUE/NA; a silent skew if FALSE appears. Worth one assertion in the data validation layer.

#### M4. Match model: in-sample stacking inside the GAM chain; stale `HOLDOUT_SEASON` docs

- `torp/R/match_train.R:235-239` — model 1's `gam_pred_tot_xscore` is predicted on its own
  training rows and fed into models 2–5 (:245-341), same class of in-sample stacking as C1 (models
  2–5 learn to over-trust an in-sample-sharp input). The rolling evaluation in
  `train_match_models.R` measures the *whole chain* OOS, which bounds the damage — this is why
  it's Medium here and Critical for WP. Production `match_gams.rds` is then trained on all data
  (:456-466), which is defensible given the rolling eval, but per-step CV-stacked inputs would be
  more honest.
- Doc drift: `torpmodels/CLAUDE.md:32` still instructs "Set `HOLDOUT_SEASON` to a finite year" —
  no such variable exists in `train_match_models.R` anymore (it's `TEST_SEASONS <- 2025:2026`,
  :13). The only remaining `HOLDOUT_SEASON` reference is a stale commit message.

#### M5. Legacy `fit_win_probability()` trains on synthetic, near-noiseless quarter margins

- `torp/R/win_probability.R:47-90` fabricates quarter-break margins as
  `final_score × cumulative_pct + N(0, 5%·score)` — i.e., the "Q1 margin" feature is a shrunken,
  almost deterministic copy of the final margin. A logistic fit on this data learns an absurdly
  steep early-game margin coefficient (real Q1 margins have enormous conditional variance;
  synthetic ones have almost none). This is textbook target leakage via simulated features.
  Mitigating factor: the blog's browser WP (`inthegame-blog/afl/win-prob.js`) actually uses the
  `train_live_wp_xgb.R` model (real pbp, symmetrized, match-grouped CV — sound), so this exported
  function appears to be **unused legacy**. It should be deprecated or gutted before someone wires
  it to a page; it is currently an exported, documented footgun.

#### M6. Training-data quality warning ignored at train time

- `training_v4.log`: `fix_chain_coordinates_dt: reached max iterations (50) with 604 rows still
  flagged. Possible oscillating coordinates.` — the v4 model (and any core retrain on the same
  pull) trained through a known unresolved coordinate defect without any row-drop or triage. Small
  count, but the warning → proceed pattern means nobody would notice if it were 60,000.

#### M7. Versionless model artifacts; no provenance stamped into RDS models

- Both WP training scripts write the same `wp_model.rds`; the live EP v2 JSON is uploaded to R2
  under the versionless key `afl/ep-model-live.json` (`ep-model.js:55`). The v4 WP JSON envelope
  at least records `trained_on`/`exported_at` (`train_live_wp_chain_v4.R:228-239`); the core RDS
  models record nothing — no training-script hash, data span, or CV metric. This is why C2
  ("which script produced the shipped model?") requires forensic timestamp analysis at all. The
  Worker's `EXPECTED_FEATURES` load-time validation (`ep-model.js:85-100`,
  `wp-model.js:42-100`) is a good guard and worth mirroring on the R side.

---

## 3. Experiments to confirm each suspicion

Run these in order; E1–E3 are the load-bearing ones. All assume `devtools::load_all("torp")` from
`torpverse/torpmodels/` and use the WSL/PowerShell Rscript pattern from `torpverse/CLAUDE.md`.

### E1. Quantify the EP→WP leak (confirms/refutes C1's magnitude; the "1–2%" claim)

`validate_cv_ep_wp.R` already does 80% of this — run it, but add calibration-slope and
close/late-subset reporting, which is where the damage should concentrate:

```r
# After validate_cv_ep_wp.R has produced wp_data_insample / wp_data_oos and
# OOS WP predictions for both (extend run_wp_cv to return xgb.cv predictions:
#   xgb.cv(..., prediction = TRUE)$pred ),
cal_slope <- function(y, p) {
  p <- pmin(pmax(p, 1e-6), 1 - 1e-6)
  coef(glm(y ~ qlogis(p), family = binomial()))[2]  # 1 = calibrated, <1 = too steep
}
report <- function(df, pred, label = "") {
  idx_late_close <- df$period == 4 & abs(df$points_diff) <= 12
  cat(sprintf("%s  logloss=%.5f  slope_all=%.3f  slope_Q4close=%.3f\n", label,
      MLmetrics::LogLoss(pred, df$label_wp),
      cal_slope(df$label_wp, pred),
      cal_slope(df$label_wp[idx_late_close], pred[idx_late_close])))
}
report(wp_data_insample, pred_insample, "in-sample EP")
report(wp_data_oos,      pred_oos,      "CV EP      ")
```

**Predicted outcome if C1 is right**: logloss gap modest (maybe the claimed 1–2%), but
`slope_Q4close` for the in-sample-EP model noticeably **< 1** (too steep) and closer to 1 for the
CV-EP model. If both slopes are ≈1, the WPA-exclusion rationale needs a different explanation.

### E2. Temporal holdout for EP and WP (confirms/refutes H1)

```r
chains <- torp::load_chains(TRUE, TRUE)
pbp    <- torp::clean_pbp(chains)
mde    <- torp::clean_model_data_epv(pbp)
mde$season_yr <- as.integer(substr(mde$match_id, 5, 8))   # CD_M<yyyy>...
train <- mde[mde$season_yr <= 2024, ]; test <- mde[mde$season_yr >= 2025, ]
# EP: train with the existing params/nrounds procedure on `train` only
# (reuse train_ep_model.R body), then:
X_te <- stats::model.matrix(~ . + 0, data = torp::select_epv_model_vars(test))
p_te <- predict(ep_model_2024, X_te)                       # matrix under xgb 3.x
mlogloss_temporal <- -mean(log(pmax(p_te[cbind(seq_len(nrow(p_te)), test$label_ep + 1)], 1e-15)))
# Compare with the random-CV mlogloss the current script reports.
# Then repeat the full WP pipeline (CV-EP inside 2021-2024 only) and evaluate
# WP on 2025-2026 with the E1 report() function.
```

**Predicted outcome**: temporal EP mlogloss a few % worse than the random-CV figure (drift), and
temporal WP calibration slope worse than the random-CV one. If the temporal numbers match the CV
numbers, H1's severity drops to housekeeping.

### E3. OOS reliability by phase × margin (confirms/refutes H2/H4 — the "too steep" claim)

Reuse the good bones of `calibration_check.R` (game-level dedup, time buckets) but feed it **OOS
CV predictions** (from `xgb.cv(..., prediction = TRUE)` with the match folds) instead of the
shipped model's in-sample predictions:

```r
cv <- xgboost::xgb.cv(params = wp_params, data = full_train, nrounds = best_n,
                      folds = wp_folds, prediction = TRUE, verbose = 0)
q4 <- as.data.table(model_data_wp)[, pred := cv$pred][period == 4]
game_level <- q4[, .(label_wp = last(label_wp), pred = last(pred),
                     points_diff = last(points_diff)),
                 by = .(match_id, bucket = pmin(4, est_match_elapsed %/% 300 - 11))]
game_level[, .(n = .N, actual = mean(label_wp), predicted = mean(pred),
               slope = tryCatch(coef(glm(label_wp ~ qlogis(pmin(pmax(pred,1e-6),1-1e-6)),
                                          family = binomial()))[2], error = function(e) NA)),
           by = .(bucket, close = abs(points_diff) <= 12)][order(bucket, close)]
```

Also run the same table through the **v4 live model** (score `wp-model-chain.rds` over the same
states) — its own log already shows the 39.5%-tied / 80.2%-at-+3 anomaly (C3); this quantifies it
across all matches rather than 10 synthetic states. Wilson CIs on `actual` recommended
(`binom::binom.wilson(n*actual, n)`).

### E4. Train/serve skew audit for the live stack (confirms/refutes C3/M1)

Pick 2–3 recent matches. In R, compute the *training-convention* features
(`add_epv_vars()` exp_pts from the full core EP model; training-style `chain_action_num`); in
Node, run the same chains through the Worker path (`scoreChainRows` + `makeAflFeatureExtractor` —
they're plain functions, importable in a script) and diff:

```
# per row: exp_pts_R  vs  r.ep (worker)   → distribution of (exp_pts_R - ep_live)
#          xmargin_R  vs  xmargin_worker  → then Δ WP = wp(xmargin_R) - wp(xmargin_worker)
#          chain_action_num_R vs worker   → % rows differing, mean offset
```

**Ship gate**: add an AFL equivalent of `validate-football-wpa.mjs` asserting per-match WP RMSE
between the R reference and the Worker below a threshold, wired into `deploy-worker.yml`.

### E5. Are the model-selection wins real? Match-level block bootstrap (confirms/refutes H3)

```r
boot_diff <- function(results, B = 2000) {  # results: match_id, ll_new, ll_old per match
  ids <- unique(results$match_id)
  replicate(B, {
    s <- sample(ids, replace = TRUE)
    d <- results[match(s, match_id)]          # per-match mean logloss precomputed
    mean(d$ll_new - d$ll_old)
  })
}
# 95% CI from quantile(boots, c(.025, .975)); if it straddles 0, the "win" that
# selected the current feature set / time encoding was noise.
```

Apply to the stored subset comparisons in `compare_wp_subsets.R` (re-run; it's self-contained).

### E6. Overfitting sweep under temporal validation (confirms/refutes the depth-6 concern)

With the E2 temporal split fixed, grid `max_depth ∈ {3,4,6}`, `min_child_weight ∈ {1,25,100}`
for WP (currently 6/1 — `train_wp_model.R:54-55`). **Prediction**: with honest (CV-EP) features
and temporal validation, shallower/heavier-leaf settings match or beat 6/1, and the depth-6
advantage seen historically was partly leak-fitting. If 6/1 still wins temporally, drop this
concern.

### E7. Post-hoc recalibration of the shipped WP (cheap production fix)

If E3 shows slope < 1 late/close: fit per-time-bucket isotonic (or a single logistic with
`qlogis(pred) × time_bucket` interactions) on OOS CV predictions and apply at serve time
(R: wrap `get_wp_preds()`; Worker: a 1D lookup applied after `predictWp`). This fixes WPA's
usability without retraining, and would let WPA be re-evaluated for the TORP blend.

---

## 4. Recommended validation protocol going forward

1. **One untouched temporal holdout, always.** Hold out the most recent completed season (plus
   the in-progress one) from *all* model selection. Random match-grouped CV stays for tuning
   *inside* the training window; the holdout is scored once per release candidate, and its
   metrics (logloss, Brier, calibration slope overall and for Q4/close) go in the release notes.
   `create_temporal_splits()` already exists — use it.

2. **Never train a downstream model on in-sample upstream predictions.** WP (core and live) must
   consume cross-validated EP predictions during training (`train_wp_model_cv_ep.R` pattern);
   retire `train_wp_model.R` or make it delegate. Same principle for the GAM chain in
   `match_train.R` (per-step CV-stacked inputs) when convenient.

3. **Calibration is a release gate, not a debug scratch script.** Promote the game-level-dedup
   reliability table (from `calibration_check.R`) into `torp/R/model_validation.R`, run it on OOS
   predictions only, and fail the release if |slope − 1| exceeds a tolerance in the Q4/close cell.
   Report Brier + slope alongside logloss everywhere; logloss alone hid this whole problem.

4. **Match-level metrics with uncertainty.** Every model-vs-model comparison reports a
   match-level block-bootstrap CI. No feature/hyperparameter change ships on a delta whose CI
   includes zero. Rotate the CV seed between experiment campaigns (the seed-1234 partition is
   spent).

5. **Train/serve parity harness for AFL live models**, mirroring football's
   `validate-football-wpa.mjs`: fixed test chains → R reference EP/WP/WPA vs Worker output, RMSE
   thresholds, wired into `deploy-worker.yml`. The EP-feature drift assertions already in
   `ep-model.js`/`wp-model.js` catch renames but not distribution skew — this catches both.

6. **Stamp provenance into every artifact.** RDS + JSON envelopes carry: training script name +
   git SHA, data span (`trained_on`), fold scheme + seed, CV metric, holdout metric, and the
   upstream EP artifact hash the WP model was trained against. v4's JSON envelope is the
   starting template; extend to the core RDS models so "which script trained the published
   `wp_model.rds`?" is answerable by inspection, not forensics.

7. **Fix the cheap skews while you're in there**: worker `chain_action_num` increment (M1),
   `shot_row` FALSE semantics (M3), `byrow=FALSE` reshape (M2), Centre-Bounce/Out-On-Full
   `exp_pts` conventions in the Worker (C3), and either train the live WP on live-EP-generated
   `exp_pts` (best: eliminates the model mismatch by construction) or export the full EP model for
   serving. Deprecate `fit_win_probability()` (M5) before it gets wired to anything.

8. **Revisit the WPA-in-TORP decision after E1/E3/E7.** The exclusion was made on the basis of a
   gradient defect that this review traces to measurable causes (leaked-sharp EP features,
   engineered exponential time terms, in-sample-only calibration checks, live train/serve skew).
   If a CV-EP-trained, temporally validated, recalibrated WP passes the slope gate in close/late
   buckets, WPA becomes a defensible blend component again — or at minimum, a trustworthy display
   metric.

---

*Review completed 2026-07-10 by Fable 5 (methodology pass). Companion doc:
`torp/FABLE-REVIEW.md` (pipeline/caching/release). No code was modified.*
