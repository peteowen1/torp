# FABLE-RECAL-PLAN.md — WP recalibration layer + temporal slope release gate

**Date:** 2026-07-12
**Author:** Claude Fable 5 (design pass; no code modified)
**Inputs:** `torp/FABLE-WP-EXPERIMENTS.md` (§1 conventions, §5 temporal results, §7 verdict points 3–4), the 2026-07-11 TRAINING-CONSOLIDATION migration (train_lib.R / publish.R / model_meta.R as they exist today on dev), evidence sweep of the WP serving + WPA credit chain.
**Executor profile:** Sonnet-class agent. Every design decision is made here; do not re-litigate. Line references are to dev as of 2026-07-12 — locate by content if drifted.

**What this closes:** FABLE-WP-EXPERIMENTS §7 points 3 and 4 — the E7 recalibration layer and the temporal Q4/close slope release gate. Together they are the two prerequisites for the WPA-in-TORP reinstatement decision recorded in `torp/CLAUDE.md` (2026-07-11).

---

## 0. Problem restated

The canonical WP model (arm B config, now shipped) is ~perfectly calibrated under random CV (slope 0.998–1.004) but runs **flat on the most recent seasons**: temporal holdout slope **1.140 all-rows / 1.263 Q4-close** (FABLE-WP-EXPERIMENTS §5). Slope convention: coefficient of `qlogis(pred)` in `glm(label ~ qlogis(pred), binomial)`; >1 = too flat/underconfident. Consequences:

1. WP values in Q4/close mid-range states are ~8–12 points under-confident → per-action WPA magnitudes are systematically understated exactly where leverage is highest.
2. Nothing gates a release on this: the publish path (`torpmodels/R/publish.R`) checks file completeness only; `torp/data-raw/05-validation/validate_wp_model.R` is a manual cat()-only diagnostic; logloss cannot see the defect (§7 point 4).
3. WPA-in-TORP reinstatement is blocked on both fixes existing.

**Constraint:** single maintainer; training is manual (`torpmodels/data-raw/train_models.R`); no new services or CI. The calibration must flow to every consumer of core WP (pbp `wp`/`wpa`, `wp_credit`, `player_game_ratings`, released parquets) through the existing single choke point.

---

## 1. Design decisions (settled — do not reopen)

### D1. Calibration form: two-parameter logistic on the logit ("Platt on logit")

`p' = plogis(a + b * qlogis(p))`, coefficients from `glm(label ~ qlogis(pred), family = binomial)` on held-out predictions.

- **Not isotonic.** Isotonic is a step function; WPA consumes *differences* of consecutive predictions, so steps would zero out most per-action WPA and spike the rest. Smooth + strictly monotone is a hard requirement.
- Two parameters only. If (and only if) the calibrated Q4/close gate cell still fails, escalate to ONE interaction: `glm(label ~ qlogis(pred) * is_q4close, binomial)` (4 params). Do not go further (no splines, no per-quarter models) — that is retraining by another name.
- Draw rows (`label_wp == 0.5`, ~18k rows) are **excluded** from the calibration fit and from all slope computations, matching the experiments' convention (§1: "kept for logloss, excluded from calibration slopes").

### D2. Fit protocol: temporal, not random-CV

Random-CV predictions look calibrated (0.998–1.004) — fitting a calibration there would learn the identity and fix nothing. The flatness only appears on temporally-held-out recent seasons, so that is where the calibration is fitted:

```
seasons = default_training_seasons()           # e.g. 2021:2025
S_gate  = max(seasons)                          # 2025 — last completed season
1. Train a TEMPORAL VARIANT exactly like production but on seasons < S_gate:
   EP on 2021:2024 (CV nrounds inside the window) → 5-fold OOS EP preds →
   WP features → WP on 2021:2024.  (This mirrors FABLE-WP-EXPERIMENTS §5's
   protocol exactly — "test-season WP features are built from an EP model
   that never saw" the holdout.)
2. Predict the temporal variant on all S_gate rows (features from the
   temporal EP model). These are the honest recent-season OOS predictions.
3. Fit (a, b) on them (all S_gate non-draw rows, row-level — 2 params don't
   overfit; the dedup convention is for MEASUREMENT, not fitting).
4. The published model remains trained on the FULL window (2021:2025) as
   today; it ships with the (a, b) fitted in step 3.
```

Accepted approximation: (a,b) fitted on the temporal variant is applied to the full-window model. This is standard practice; the drift being corrected is a property of the training scheme (past→future), which the temporal variant reproduces and the full-window model will exhibit on 2026+.

In-progress-season rows (2026 to current round) are a **report-only** second check: print the calibrated slope on them; never gate on them (partial season, round-composition artifacts — see the §3.3 1.689 lesson in the experiments).

### D3. Artifact: `wp_calibration.rds`, atomic with the model

A small list: `list(a =, b =, formula = "plogis(a + b*qlogis(p))", fitted_on = "temporal-oos", gate_season =, n_fit =, slope_before =, slope_after =, slope_q4close_before =, slope_q4close_after =)`, stamped with `torp_meta` via `build_model_meta()` like every other artifact (its `extra` carries the slope numbers).

- `torpmodels/R/publish.R` `.MODEL_GROUPS`: `wp = c("wp_model.rds", "wp_calibration.rds")` — the existing F3 both-or-neither machinery makes the pair atomic for free.
- `torpmodels/R/load_model.R`: add `wp_calibration` to `.CORE_MODELS` (`:9-17`) and `normalize_model_name()`'s map (`:436-448`) so `load_torp_model("wp_calibration")` works.
- Old deployments and meta-less loads keep working: absence of the artifact = identity calibration (D4).

### D4. Serving: applied inside `get_wp_preds()`, identity fallback

`torp/R/add_variables.R:249-268` (`get_wp_preds`): immediately after `stats::predict(wp_model, model_matrix)` (and the xgboost-3.x flatten), apply:

```r
calib <- load_model_with_fallback("wp_calibration")   # same pattern as :250
if (!is.null(calib) && is.finite(calib$a) && is.finite(calib$b)) {
  preds_raw <- stats::plogis(calib$a + calib$b * stats::qlogis(preds_raw))
} else {
  # one-time per session: cli_warn("wp_calibration unavailable — serving uncalibrated WP")
}
```

- `load_model_with_fallback` must treat a 404/absence as the warn-once identity path, never an error (the artifact won't exist until Step 5 publishes it; every historical environment lacks it).
- Placed here (not in `add_wp_vars`) so *anything* calling `get_wp_preds()` is calibrated; `add_wp_vars`'s existing clamp to [0.001, 0.999] at `:117` stays and runs after.
- Downstream is automatically covered — verified chain: `add_wp_vars` (`wp`, `wpa`) → `create_wp_credit` (`wp_credit.R:32-139`) → `player_credit.R:167-179` → `player_game_ratings.R:104-188` → released parquets. No changes needed in any of those files.

### D5. The gate: |slope − 1| ≤ 0.10, calibrated, Q4/close cell, game-level dedup

Computed on the **calibrated** temporal-variant predictions over S_gate, in the experiments' exact Q4/close convention:

- Cell: `period == 4 & abs(points_diff) <= 12`, non-draw labels.
- Dedup before the GLM: `bucket = pmin(4, pmax(0, est_match_elapsed %/% 300 - 11))`; keep the **last** row per `(match_id, bucket)` (the experiments' anti-pseudoreplication convention, quoted at §3 of the doc).
- Statistic: slope from `glm(label ~ qlogis(pred), binomial)` on the deduped cell. Convention note: this is NOT `model_validation.R`'s decile-OLS slope — do not reuse `evaluate_model_comprehensive()`; leave that file alone.
- Threshold **0.10**: the defect guarded against measured 0.263 in this cell; fold-to-fold noise in the experiments ran ~±0.05; 0.10 splits them with margin. Also gate the all-rows slope at the same 0.10 (it measured 0.140 uncalibrated — post-calibration both should sit ~0).
- Fires as `cli::cli_abort()` in the same idiom as `validate_wp_spec()` (train_lib.R:187-191) and `publish_model_group()` (publish.R:41-47): loud bulleted message with the measured slopes, BEFORE `saveRDS`/upload. Nothing is written or published on failure.
- Override: `train_models.R --skip-slope-gate` sets `slope_gate = FALSE` with a shouted warning; exists for emergencies only.
- No logloss gate, deliberately (§7 point 4: logloss saw none of the defects).

### D6. WPA-in-TORP reinstatement stays a separate, measured decision

This plan ships the prerequisites and the measurement, not the blend change. Protocol in §5 below; the decision itself is a one-line constant flip in a later commit, gated on the measurement.

---

## 2. Implementation steps (ordered; each = one commit, tests green before the next)

### Step 1 — torpmodels train_lib: temporal variant + calibration fit + gate *(the core)*

`torpmodels/data-raw/lib/train_lib.R`:

1. `cv_wp_oos_preds(X, y, folds, params, nrounds)` — sibling of `cv_ep_oos_preds()` (`:133-159`), same per-fold-refit pattern, returns a full-length OOS probability vector. (Needed for the report-only random-CV slope and future diagnostics; the temporal path below is the load-bearing one.)
2. `fit_wp_temporal_variant(model_data_epv, gate_season, params_ep = ep_params(), params_wp = wp_params())` — trains EP on `seasons < gate_season` (CV nrounds inside the window), builds 5-fold OOS EP preds within the window, WP features via `build_wp_data()`, trains WP on the window, then predicts all `gate_season` rows using gate-season WP features built from the **temporal** EP model's predictions on those rows. Returns `list(preds, labels, meta_cols)` where `meta_cols` carries `period`, `points_diff`, `est_match_elapsed`, `match_id` for the gate cell. Season derivation: `season` column if present, else `as.numeric(substr(match_id, 5, 8))` (the `create_temporal_splits()` convention, `torp/R/model_validation.R:106-124` — reuse that function via `torp:::` for the split itself).
3. `fit_wp_calibration(preds, labels)` — drops draws, fits `glm(labels ~ qlogis(preds), binomial)`, returns `list(a, b)` with `stopifnot(is.finite(a), is.finite(b), b > 0)`.
4. `wp_gate_slope(preds, labels, meta_cols, cell = c("q4close", "all"))` — implements D5's dedup + GLM; returns the slope scalar. Unit-testable pure function.
5. `validate_wp_temporal_slope(calibrated_preds, labels, meta_cols, threshold = 0.10)` — computes both cells, `cli::cli_abort()` per D5 on breach, else `cli::cli_alert_success()` with the numbers.
6. Wire into `train_core_models()` WP branch (`:353-379`), new args `slope_gate = TRUE, calibrate = TRUE`:
   - after `wp_fit <- fit_wp(...)`: run the temporal variant → fit calibration → apply → gate → on pass, build `wp_calibration` object (D3 fields incl. before/after slopes), `stamp_model_meta()`, `saveRDS` alongside `wp_model.rds` → `publish_model_group("wp", ...)` publishes the pair.
   - `calibrate = FALSE` (or `wp_ep_source = "insample"`) skips variant+calibration AND forces `upload = FALSE` — an uncalibrated model can be built locally but never published through the front door.
   - Print the report-only slopes (in-progress season; random-CV via `cv_wp_oos_preds`) after the gate.

Cost note for the executor: the temporal variant adds roughly one EP CV + 5 EP fold refits + one WP CV to the retrain (~2× today's WP phase). Acceptable — manual, occasional.

### Step 2 — torpmodels package: group + loader for the sidecar

`publish.R` `.MODEL_GROUPS$wp` → pair (D3). `load_model.R` `.CORE_MODELS` + `normalize_model_name()` additions. Tests: publishing `wp` without the sidecar aborts (extends the existing F3 test); `load_torp_model("wp_calibration")` round-trips the object and prints meta. `devtools::document()` + `devtools::check()`.

### Step 3 — torp serving: apply calibration in `get_wp_preds()`

Per D4. `load_model_with_fallback` (`add_variables.R:306-330`): confirm it degrades to `NULL` + warn (not abort) on download failure; if it aborts today, add a `tryCatch` for the calibration load only. Tests (`torp/tests/testthat/test-add-model-variables.R` or new `test-wp-calibration.R`): (a) identity behaviour when calibration absent — predictions byte-identical to pre-change snapshot; (b) with a fixture `list(a = 0, b = 1.3)`, output equals `plogis(1.3 * qlogis(raw))`; (c) monotonicity preserved; (d) WPA sign flips unaffected (wpa recomputed from calibrated wp is still lead-difference — no code change, regression test only).

### Step 4 — trainer CLI + docs

`train_models.R`: `--skip-slope-gate`, `--no-calibrate` flags mapped to the Step 1 args; help text. Docs in the same commit: `torpmodels/CLAUDE.md` training section (the pair, the gate, the flags), `torp/CLAUDE.md` TORP-composition note gains "recalibration layer shipped (FABLE-RECAL-PLAN); WPA reinstatement pending §5 measurement", `torpmodels/ARCHITECTURE.md` provenance section mentions the sidecar.

### Step 5 — retrain + publish through the gated path *(execution)*

`Rscript data-raw/train_models.R ep wp shot` from torpmodels root. Acceptance (§4). This replaces the 2026-07-11 uncalibrated artifact — same filename, manifest history records the supersession.

### Step 6 — WPA bias re-measurement + decision record *(analysis, not product code)*

New `torp/data-raw/04-analysis/wpa_close_game_bias.R` (manual script, not wired to any pipeline):

1. Load 2–3 recent completed seasons of pbp; compute per-player-season WPA twice: uncalibrated (raw `get_wp_preds` output, temporarily bypassing the calibration via a script-local flag) and calibrated.
2. Per player-season: `close_share` = fraction of their games with final `|margin| <= 12`; `wpa_rate` = season WPA per game.
3. Report `cor(wpa_rate, close_share)` and the regression coefficient of `wpa_rate ~ close_share + torp_value` (does close-game exposure predict WPA *beyond* quality?) for both variants, plus the top-20 WPA leaderboards side by side.
4. Decision rule to record in `torp/CLAUDE.md` afterwards: reinstatement (as a blend component or continued parallel display — Pete's call) becomes *defensible* if the exposure coefficient shrinks materially (>50%) under calibration and the calibrated leaderboard is not dominated by close-game exposure. The plan deliberately does NOT pre-commit the blend weight; it produces the evidence memo (`FABLE-WPA-DECISION.md`, short) for Pete.

---

## 3. Exact files to change

| Repo | File | Change |
|---|---|---|
| torpmodels | `data-raw/lib/train_lib.R` | +5 functions (§2 Step 1), `train_core_models()` WP branch wiring, new args |
| torpmodels | `R/publish.R:10-15` | `.MODEL_GROUPS$wp` → `c("wp_model.rds", "wp_calibration.rds")` |
| torpmodels | `R/load_model.R:9-17, 436-448` | `wp_calibration` in `.CORE_MODELS` + name map |
| torpmodels | `data-raw/train_models.R` | `--skip-slope-gate`, `--no-calibrate` |
| torpmodels | `tests/testthat/test-train_lib.R`, `test-publish.R`, `test-load_model.R` | §2 tests |
| torpmodels | `CLAUDE.md`, `ARCHITECTURE.md` | doc updates (Step 4) |
| torp | `R/add_variables.R:249-268` (`get_wp_preds`), `:306-330` (fallback loader) | D4 |
| torp | `tests/testthat/test-wp-calibration.R` (new) | Step 3 tests |
| torp | `data-raw/04-analysis/wpa_close_game_bias.R` (new) | Step 6 |
| torp | `CLAUDE.md` | Step 4 note |

**Do not touch:** `torp/R/model_validation.R` (different slope convention, deliberately left as-is), `torp/R/wp_credit.R` / `player_credit.R` / `player_game_ratings.R` (calibration flows through), all live-model export scripts (§6), `.github/workflows/**`.

---

## 4. Acceptance (Step 5 checklist)

1. Trainer log shows: temporal variant trained on `2021:2024`, calibration `(a, b)` printed with `b` in (1.05, 1.45) (the measured flatness inverted — sanity range, not a gate), gate slopes both within 0.10 of 1, report-only slopes printed.
2. `core-models` release holds `wp_model.rds` + `wp_calibration.rds`, same `generation` entry in `models_manifest.json`; `check_manifest_sync()` clean.
3. Fresh `load_torp_model("wp_calibration", force_download = TRUE)` prints meta with `slope_q4close_before ≈ 1.26`-ish and `slope_q4close_after` within gate.
4. `add_wp_vars()` on a cached recent match produces `wp` values that differ from the pre-calibration run in the expected direction (mid-range Q4 values pushed away from 0.5) and WPA per match still sums to ≈ (final result − opening WP).
5. Negative test: rerun trainer with a deliberately mis-fit calibration fixture (b = 1) → gate aborts, nothing uploaded (release asset `updated_at` unchanged).

---

## 5. Non-goals

1. **Live/worker WP models.** `live_wp_lookup.json`, `wp-model-chain.json`, and the Worker's chain-aware model are separate artifacts/algorithms. Known, larger issue: worker vs torp WP divergence is material in production (AFL_CHAIN_PARQUET_PLAN.md: ≈ +32pp vs +13pp on the same Q4 sequence) — that is its own future plan and MUST NOT be conflated with this calibration.
2. **EP calibration.** No evidence of EP miscalibration; out of scope.
3. **Isotonic / spline / per-quarter calibration** (D1 rationale).
4. **The blend change itself** — §2 Step 6 produces the evidence; the constant flip is a separate decision commit.
5. **`model_validation.R` consolidation** — its decile-OLS slope differs from the experiments' GLM convention; unifying them is cosmetic debt, not this plan.
6. **Automated retraining/CI** — the gate lives in the manual trainer, consistent with TRAINING-CONSOLIDATION-PLAN non-goal 2.
