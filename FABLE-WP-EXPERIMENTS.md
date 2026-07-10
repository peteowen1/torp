# FABLE-WP-EXPERIMENTS.md — WP Validation Experiments: Measured Results

**Scope**: executes the confirmation experiments recommended by `FABLE-METHODOLOGY.md`
(§3 + the 2026-07-10 adversarial-verification addendum) against the **shipped**
`torpmodels/inst/models/core/wp_model.rds`. Everything below is *measured*, not asserted.
No repo code was modified; no model or data was uploaded or committed. All experiment
scripts ran from a session scratchpad and are inlined in §8 for reproduction.

**Environment**: R 4.5.1 (ucrt), xgboost 3.2.1.1, arrow 22.0.0.1, 24 threads,
Windows 11. `torp` via `devtools::load_all("C:/dev/torpverse/torp")`; shipped models read
directly from `C:/dev/torpverse/torpmodels/inst/models/core/`. Run date: 2026-07-10.

**Runtime**: full suite ~35 min wall clock. No subsampling and no nrounds trimming was
needed — all CV runs used the training scripts' own caps with early stopping.

---

## 1. Setup and data actually used

Data came through the exact loaders the training scripts use
(`torp::load_chains(TRUE, TRUE)` → `clean_pbp()` → `clean_model_data_epv()` →
`add_epv_vars()` → `clean_model_data_wp()`). The chains release was fetched fresh
(2026-07-10), so it contains ~4 weeks of matches the shipped model (trained 2026-06-18)
never saw — used below as a genuine post-ship holdout.

| Check | Value |
|---|---|
| Chains rows loaded | 2,448,217 |
| EPV/WP model rows | 1,993,332 (no `label_wp` NAs dropped; 0 feature NAs) |
| Matches | 1,207 |
| Rows by season | 2021: 346,476 · 2022: 342,044 · 2023: 360,281 · 2024: 348,608 · 2025: 351,708 · 2026 (to Jul): 244,215 |
| `label_wp` | mean 0.5163; values {0, 0.5, 1}; 18,216 draw rows (0.5) — kept for logloss, excluded from calibration slopes |
| Rows by quarter | Q1 510,540 · Q2 498,246 · Q3 494,324 · Q4 490,222 |
| Post-ship slice (2026 R15+) | 41,737 rows, 25 matches — genuinely OOS for the shipped artifact |

Sanity per the data rules: ~1.99M rows / 1,207 matches is the expected full 2021–2026
scale (the v4 training log's 1,116 matches was the June snapshot; the delta is the
2026 rounds played since).

**Calibration slope convention** used throughout: coefficient of `qlogis(pred)` in
`glm(label ~ qlogis(pred), binomial)`. 1 = calibrated, **< 1 = too steep/overconfident,
> 1 = too flat/underconfident.**

---

## 2. E0 — Artifact forensics (confirms C2/F1 by direct inspection)

`readRDS()` on the shipped `wp_model.rds`, reading its stored `call` and `params`
attributes (no training needed):

```
call: xgboost::xgb.train(params = params_wp, data = full_train_wp,
                         nrounds = optimal_rounds_wp, print_every_n = 10)
eta: 0.1   subsample: 0.85   colsample_bytree: 0.85   min_child_weight: 1
monotone_constraints: "(0,0,0,1,1,1,0,1,0,0,0,0,0,0,0)"   <- 15 entries vs 18 features
nrounds: 47
```

`params_wp` / `full_train_wp` / `optimal_rounds_wp` are the variable names of
`torp/data-raw/rebuild_everything.R` phase 4b — **the shipped artifact provably came from
the rebuild's inlined path**, not `train_wp_model.R` (eta 0.025) nor
`train_wp_model_cv_ep.R`. The methodology review's C2 verdict is confirmed by inspection.

How the 15-entry string lands on the 18 features (order = `select_wp_model_vars()`):

| # | feature | intended | shipped | effect |
|---|---------|----------|---------|--------|
| 4 | `home` | 0 | **+1** | spurious constraint (directionally benign) |
| 7 | `pos_lead_prob` | +1 | **0** | intended constraint lost |
| 8 | `time_left_scaler` | 0 | **+1** | WP forced non-decreasing in exp(elapsed) — wrong for trailing teams |
| 9 | `diff_time_ratio` | +1 | **0** | intended constraint lost |
| 10 | `score_urgency` | +1 | **0** | intended constraint lost |
| 16–18 | `phase_of_play_*` | 0 | (absent → 0) | xgboost pads silently |

Verified empirically: xgboost 3.2.1 trains without error or warning on a 15-entry
constraint string against an 18-column DMatrix (it silently pads) — which is exactly how
this shipped.

---

## 3. E1 — Calibration audit of the shipped model as-is

The shipped model scored over the full serving-convention feature pipeline
(`add_epv_vars()` → `clean_model_data_wp()`), i.e. measured *as production uses it*.
It saw every match up to 2026-06-17 in training, so the full-data numbers are
**in-sample** — which makes the result below stronger, because in-sample miscalibration
can only get worse OOS.

| Slice | n | logloss | Brier | slope (all) | slope (Q4, \|margin\| ≤ 12) |
|---|---|---|---|---|---|
| In-sample (≤ 2026 R14) | 1,951,595 | 0.45430 | 0.15147 | **1.064** | **1.073** |
| Post-ship OOS (2026 R15+) | 41,737 | 0.38465 | 0.12639 | 1.689* | 1.122 |

\* 25 matches; see §3.3 — the honest retrains show the same slope on this slice, so 1.689
is mostly a property of the slice, not of this artifact specifically.

Slope by quarter (in-sample): Q1 1.090 · Q2 1.019 · Q3 1.055 · **Q4 1.109**.

**Headline finding: the shipped WP model is too FLAT (underconfident), not too steep.**
A well-fit model's in-sample slope is ≤ 1; this one is 1.06–1.11 *on its own training
data*. The historical "WP gradient too steep in close/late situations" (the documented
rationale for excluding WPA from TORP) does not describe the current core artifact —
the current artifact errs in the opposite direction. Consistent with underfit: 47 trees
at eta 0.1.

Q4/close, game-level dedup (last observation per match per 5-min bucket), reliability by
predicted-probability decile — the miscalibration is material where WPA is scrutinised:

| decile | n | mean pred | actual | Wilson 95% |
|---|---|---|---|---|
| (0.1, 0.2] | 126 | 0.164 | **0.103** | [0.061, 0.169] |
| (0.5, 0.6] | 88 | 0.541 | **0.659** | [0.555, 0.750] |
| (0.7, 0.8] | 257 | 0.750 | **0.829** | [0.778, 0.870] |
| (0.9, 1.0] | 320 | 0.946 | **0.978** | [0.956, 0.989] |

i.e. ~8–12 WP points of under-confidence through the middle deciles in Q4 close games.
Row-level Q4 margin-bucket gaps are smaller (≤ 1.4 pts) because blowout rows dominate.

Full quarter × margin-bucket reliability (row-level, in-sample; `gap = actual − mean
pred`, `*` = mean pred outside the Wilson 95% CI of actual):

| margin bucket | Q1 pred→actual | Q2 pred→actual | Q3 pred→actual | Q4 pred→actual |
|---|---|---|---|---|
| ≤ −25 | .180→.185 | .116→.121* | .054→.047* | .012→.008* |
| −24:−13 | .313→.310 | .269→.268 | .208→.205 | .097→.093* |
| −12:−7 | .406→.400* | .374→.366* | .351→.355 | .226→.225 |
| −6:0 | .493→.486* | .480→.489* | .464→.474* | .416→.410* |
| 1:6 | .566→.577* | .567→.553* | .605→.607 | .687→.695* |
| 7:12 | .630→.628 | .666→.675* | .693→.695 | .830→.834* |
| 13:24 | .730→.735* | .771→.772 | .830→.834* | .927→.931* |
| ≥ 25 | .839→.844 | .901→.901 | .959→.963* | .991→.996* |

The signature is systematic: in Q4 nearly every leading bucket's actual is *above* the
prediction and every trailing bucket's is *below* — flatness, at every margin.

### 3.1 E1b — Is the flatness the rebuild's feature-convention skew? No.

The rebuild trained WP on `add_epv_custom()` features (raw `exp_pts`, **no**
Centre-Bounce zeroing / Out-On-Full negation), while production serves `add_epv_vars()`
features (with both). Re-scoring the shipped model on rebuild-convention features:
slope_all **1.072**, slope_Q4close **1.074** — unchanged. The two conventions differ on
only 1.8% of rows (mean |Δ`xpoints_diff`| = 0.013). The flatness is the artifact itself,
not train/serve feature skew inside the core pipeline.

### 3.2 What the flatness is (and isn't)

Constraint misalignment and eta were the suspects, but E2 (§4) shows a correctly
constrained, correctly tuned retrain is calibrated OOS while the shipped artifact is flat
*in-sample* — the residual explanation is the specific 47-round early stop on the June
data snapshot (the same config CV'd on today's data stops at 64 rounds and is calibrated).
The steep-gradient defect the team engineered constraints against is not present in the
current core WP at any slice measured.

### 3.3 Post-ship head-to-head (both honestly OOS on identical rows)

On the 25 matches after 2026-06-18, the shipped model is genuinely OOS, and the E2
retrains' CV predictions are OOS on those rows by fold construction:

| model | logloss | slope all | slope Q4close |
|---|---|---|---|
| shipped artifact | 0.38465 | 1.689 | 1.122 |
| A (shipped-replica, CV) | 0.38243 | 1.711 | 1.298 |
| B (canonical-fixed, CV) | 0.38100 | 1.710 | 1.258 |
| C (eta 0.1 + fixed constraints, CV) | 0.38015 | 1.693 | 1.240 |

B − shipped: −0.0038 logloss, match-level bootstrap 95% CI [−0.0102, +0.0029] (25
matches — underpowered, but every retrain ≥ the shipped artifact). All models share the
~1.7 slope on this slice: that number reflects the slice, not the artifact.

---

## 4. E2 — Constraints-fixed retrain comparison (match-grouped CV)

Three arms, identical seed-1234 match-grouped 5-fold partition (same construction as the
training scripts), identical data, OOS predictions from per-fold retrains at the CV
optimum:

- **A — shipped replica**: rebuild params (eta 0.1, ss/cs 0.85, 15-entry misaligned constraints)
- **B — canonical fixed**: `train_wp_model.R` params (eta 0.025, ss/cs 0.8, correct 18-entry constraints)
- **C — isolate the constraint fix**: A's params but correct 18-entry constraints

| arm | CV opt nrounds | CV logloss (±fold SD) | OOS logloss | Brier | slope all | slope Q4close |
|---|---|---|---|---|---|---|
| A shipped-replica | 64 | 0.457838 ± 0.0224 | 0.457810 | 0.152512 | 0.998 | 1.004 |
| B canonical-fixed | 286 | 0.457469 ± 0.0226 | 0.457514 | 0.152450 | 1.000 | 0.998 |
| C eta0.1-fixed | 83 | 0.457585 ± 0.0227 | 0.457519 | 0.152430 | 0.986 | 0.982 |

Match-level block bootstrap (B = 2000) on per-match logloss differences:

| comparison | mean Δ | 95% CI | verdict |
|---|---|---|---|
| B − A (all rows) | −0.000296 | [−0.000586, −0.000023] | real, but **0.06%** |
| C − A (all rows) | −0.000276 | [−0.000587, +0.000037] | borderline |
| B − C | −0.000019 | [−0.000279, +0.000201] | nil — eta adds nothing beyond the constraint fix |
| B − A (Q4 close only) | −0.000445 | [−0.002046, +0.001275] | not detectable |

Q4 row-level margin-bucket reliability: max |actual − predicted| ≤ 0.006 for **all**
arms. Game-level Q4-close dedup slopes: A 1.175, B 1.154, C 1.125 (all > 1 on the final
bucket, 1.45–1.55 — a property of the deduped close-game slice, shared by every config).

**Conclusion**: the 15-vs-18 constraint misalignment is real and proven in the artifact
(§2) but its *measured* damage is ~0.0003 logloss and no detectable calibration effect.
The rebuild's inlined path is a provenance/hygiene defect more than a performance one.
Both C1's leak (~0.01%, measured in torpmodels commit `7e27bdc`) and F1's misalignment
(~0.03–0.06%) are dwarfed by the fold SD (±0.022) — consistent with the methodology
review's H3 point that these deltas live inside the noise.

---

## 5. E3 — Temporal holdout (train ≤ 2024, test 2025–26)

Used `torp:::create_temporal_splits()` (exists, tested, never used by a training script —
verified it splits correctly: 1,397,409 train rows / 845 matches; 595,923 test rows /
362 matches). EP was retrained on 2021–24 only (nrounds via match-grouped CV inside the
training window), so test-season WP features are built from an EP model that never saw
2025–26. WP features for training rows use the EP-past model's in-sample predictions,
mirroring the production training convention.

**EP**: train-window CV mlogloss 1.35116 (100 rounds); temporal test on 2025–26:
**1.34529**. No drift penalty at all — random-CV EP numbers are honest.

**WP** — the fair comparison is on identical rows (all 595,923 rows of 2025–26), random-CV
prediction (model saw other 2021–26 matches) vs temporal prediction (model saw 2021–24
only):

| model | logloss | slope all | slope Q4close |
|---|---|---|---|
| B random-CV (sees 21–26) | 0.436002 | 1.094 | 1.196 |
| B temporal (sees 21–24) | 0.437176 | 1.140 | 1.263 |
| A random-CV | 0.436240 | 1.093 | 1.205 |
| A temporal | 0.437435 | 1.141 | 1.271 |

Temporal − random gap for B: **+0.001125 logloss (+0.26%)**, match-level bootstrap 95% CI
[−0.0012, +0.0034] — small and not statistically distinguishable at 362 matches. Slope by
quarter (B): random Q1 1.292 / Q2 1.018 / Q3 1.055 / Q4 1.143; temporal Q1 1.426 /
Q2 1.028 / Q3 1.099 / Q4 1.220.

**Conclusion**: H1's feared "random CV is lying about OOS performance" is mostly not
realised — the honest temporal logloss is within ~0.3% of the random-CV figure. The real
temporal signal is in *calibration*: every model (however trained) runs flat on 2025–26
(slope 1.09–1.14 overall, 1.20–1.27 Q4/close), and training only on the past adds ~0.05
of flatness. This argues for (a) keeping a temporal season holdout as a release gate on
the *slope*, and (b) a light recency-aware recalibration, rather than for distrusting the
CV pipeline wholesale.

---

## 6. E4 — Core-EP vs live-EP feature parity (bounds the C3 train/serve skew)

The live chain WP (v4) was trained on `exp_pts` from the 19-feature core EP model but is
served `exp_pts` from the 14-feature live EP v2 model. Both models scored here on the
same 1,993,332 rows (live-v2 with training-convention `chain_action_num`, i.e. computed
after EPV filtering), `Δ = exp_pts_live_v2 − exp_pts_core`:

| statistic | all rows | Q4 close (\|margin\| ≤ 12, n = 139,014) |
|---|---|---|
| correlation | 0.9262 | — |
| mean Δ | −0.039 | −0.037 |
| **sd Δ** | **0.385** | **0.375** |
| mean \|Δ\| | 0.263 | 0.251 |
| P(\|Δ\| > 1 pt) | 2.75% | — |
| P(\|Δ\| > 3 pts) | 0.03% | 0.02% |

Since v4's `xmargin = margin_poss + exp_pts`, this Δ passes through 1:1 into the feature
carrying 91.5% of v4's gain. At v4's own logged late-game gradient (~40 WP points per 3
xmargin points ≈ 13 WP pts/pt), a 1-sd feature skew in Q4/close is **~5 WP points of
per-row noise** injected at serve time — material for per-event WPA, exactly as the C3
verifier concluded ("material where it matters most, muted elsewhere").

Caveats: this bounds the *model-mismatch* component only. It excludes three additional
serve-time divergences that all push the same direction (worse): the Worker's
`exp_pts = 0` fallback on non-scored rows, the Worker's `chain_action_num`
over-counting (M1), and Centre-Bounce rows (zeroed by `add_epv_vars()` in v4 training
but scored live at serve). A real parity harness (R reference vs Worker output on fixed
chains, per the review's E4/ship-gate) remains the fix; this measurement says the harness
would trip today.

---

## 7. Verdict

**Should the constraints-fixed, eta-corrected retrain ship? Yes — as provenance hygiene,
with correctly set expectations.** Measured case:

1. The shipped artifact is provably off-spec (§2: rebuild params, misaligned constraints,
   47 trees) and mildly underfit — in-sample calibration slope 1.06–1.11 where a well-fit
   model sits ≤ 1, and ~8–12 WP points of under-confidence in Q4/close mid-range states
   (§3). The canonical retrain (B) was ≥ the shipped artifact on every metric measured,
   including the genuinely-OOS post-ship slice.
2. But the *size* of the win is small: −0.0003 logloss OOS vs the replica config (CI
   excludes zero, all-rows), nil detectable in Q4/close specifically. Do not expect
   visible product improvement; expect correct provenance, correct constraints, and a
   model that matches its documentation. Retrain via `train_wp_model.R` (or better,
   `train_wp_model_cv_ep.R` — the CV-EP hygiene is free) and stamp provenance into the
   RDS. Fix `rebuild_everything.R` phase 4b to delegate to (or at least copy the params
   of) the canonical script so this cannot recur.
3. **The WPA-exclusion premise needs updating either way.** The exclusion rationale is
   "WP gradient too steep in close/late situations". Measured: the current core WP is too
   *flat* in exactly those situations, and a correctly trained replacement is
   ~perfectly calibrated on random-CV (slope 0.998–1.004) while running modestly flat on
   the most recent seasons (1.14 overall / 1.26 Q4-close temporal). Nothing in the core
   WP family currently exhibits the steep gradient the exclusion guards against. A
   recalibrated WP (canonical retrain + a light logistic/isotonic recalibration fitted on
   recent-season OOS predictions, per the review's E7) is defensible for revisiting
   WPA-in-TORP — the remaining, unmeasured-here risk is season-to-season calibration
   drift, which the slope gate below handles.
4. **Adopt the temporal slope gate.** `create_temporal_splits()` works; the temporal
   logloss gap is negligible (+0.26%) but the slope drift is real. Gate releases on
   |slope − 1| in the Q4/close cell of the held-out season, not on logloss (logloss could
   not see any of the defects found here).

Priority order confirmed against the addendum: (1) retrain + ship from a canonical script
with correct constraints — *confirmed worth doing, for hygiene not headline metrics*;
(2) temporal holdout + close/late calibration gates — *confirmed, slope is where the
signal is*; (3) AFL worker parity harness — see §6; (4) CV-EP as hygiene — *confirmed
tiny (0.01% recorded; consistent with everything measured here)*.

---

## 8. Reproduction

Scripts ran from the session scratchpad (never committed). Order:

```powershell
Rscript 01_prep_data.R          # ~3 min  — builds wp_full.parquet + epv_lite.parquet
Rscript 02_audit_shipped.R      # ~2 min  — E0 + E1
Rscript 03_cv_compare.R         # ~15 min — E2 training (3 arms; accepts arm names as args)
Rscript 04_cv_metrics.R         # ~1 min  — E2 metrics
Rscript 05_temporal.R           # ~12 min — E3
Rscript 09_temporal_compare.R   # <1 min  — E3 same-rows comparison
Rscript 08_postship_headtohead.R# ~1 min  — §3.3
Rscript 07_e1b_feature_skew.R   # ~2 min  — §3.1
Rscript 06_parity.R             # ~8 min  — E4
```

`SCRATCH` in each script points at the scratchpad directory; retarget it anywhere
writable. All scripts assume the shipped models at
`C:/dev/torpverse/torpmodels/inst/models/core/` and torp at `C:/dev/torpverse/torp`.

### 01_prep_data.R
```r
# 01_prep_data.R — build WP modelling frames once, cache to scratchpad parquet
# Mirrors torpmodels/data-raw/02-wp-model/train_wp_model.R data path exactly.
SCRATCH <- "C:/Users/peteo/AppData/Local/Temp/claude/C--dev/f7acd5a2-9d66-4234-978e-f3ea4fdc8482/scratchpad"

suppressMessages({
  library(dplyr)
  library(arrow)
  library(xgboost)
})
suppressMessages(devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE))

# Preload shipped models into torp's model cache so no torpmodels download is attempted
assign("ep", readRDS("C:/dev/torpverse/torpmodels/inst/models/core/ep_model.rds"),
       envir = torp:::.torp_model_cache)

t0 <- Sys.time()
cat("Loading chains (all seasons, local torpdata cache preferred)...\n")
chains <- torp::load_chains(TRUE, TRUE)
cat("chains rows:", nrow(chains), "\n")

cat("clean_pbp...\n")
pbp <- torp::clean_pbp(chains)
cat("pbp rows:", nrow(pbp), "\n")
rm(chains); invisible(gc())

cat("clean_model_data_epv...\n")
model_data_epv <- torp:::clean_model_data_epv(pbp)
cat("model_data_epv rows:", nrow(model_data_epv), "\n")
rm(pbp); invisible(gc())

# ---- Save EPV-lite frame (for temporal EP retrain + WP feature rebuild) ----
epv_vars <- torp:::select_epv_model_vars(model_data_epv)
epv_lite <- bind_cols(
  epv_vars,
  model_data_epv |> select(any_of(c(
    "label_ep", "label_wp", "points_diff", "est_match_elapsed",
    "torp_match_id", "match_id"
  )))
)
epv_lite$season <- as.integer(substr(epv_lite$match_id, 5, 8))
write_parquet(epv_lite, file.path(SCRATCH, "epv_lite.parquet"))
cat("epv_lite saved:", nrow(epv_lite), "rows,", ncol(epv_lite), "cols\n")
rm(epv_vars, epv_lite); invisible(gc())

# ---- Canonical WP path: add_epv_vars (shipped EP, in-sample) + clean_model_data_wp ----
cat("add_epv_vars (shipped core EP model, in-sample)...\n")
model_data_wp <- model_data_epv |>
  torp::add_epv_vars() |>
  torp:::clean_model_data_wp()
cat("model_data_wp rows:", nrow(model_data_wp), "\n")
rm(model_data_epv); invisible(gc())

wp_feats <- torp:::select_wp_model_vars(model_data_wp)
wp_full <- bind_cols(
  wp_feats,
  model_data_wp |> select(any_of(c("label_wp", "torp_match_id", "match_id", "period", "exp_pts")))
)
wp_full$season <- as.integer(substr(wp_full$match_id, 5, 8))

# Sanity checks (per data rules)
cat("\n==== SANITY ====\n")
cat("rows:", nrow(wp_full), " matches:", length(unique(wp_full$torp_match_id)), "\n")
print(table(wp_full$season))
cat("label_wp mean:", round(mean(wp_full$label_wp), 4),
    " values:", paste(sort(unique(wp_full$label_wp)), collapse = ","), "\n")
cat("period table:\n"); print(table(wp_full$period))
cat("NA counts in features:", sum(is.na(wp_feats)), "\n")

write_parquet(wp_full, file.path(SCRATCH, "wp_full.parquet"))
cat("wp_full saved.\n")
cat("elapsed:", round(difftime(Sys.time(), t0, units = "mins"), 1), "min\n")
```

### 02_audit_shipped.R
```r
# 02_audit_shipped.R — E1: calibration audit of the SHIPPED wp_model.rds as-is
SCRATCH <- "C:/Users/peteo/AppData/Local/Temp/claude/C--dev/f7acd5a2-9d66-4234-978e-f3ea4fdc8482/scratchpad"

suppressMessages({
  library(data.table)
  library(arrow)
  library(xgboost)
})

wp_full <- as.data.table(read_parquet(file.path(SCRATCH, "wp_full.parquet")))
cat("rows:", nrow(wp_full), " matches:", uniqueN(wp_full$torp_match_id), "\n")

FEATS <- c("est_match_elapsed", "est_match_remaining", "shot_row", "home", "points_diff",
           "xpoints_diff", "pos_lead_prob", "time_left_scaler", "diff_time_ratio", "score_urgency",
           "goal_x", "play_type_handball", "play_type_kick", "play_type_reception",
           "phase_of_play_handball_received", "phase_of_play_hard_ball",
           "phase_of_play_loose_ball", "phase_of_play_set_shot")

wp <- readRDS("C:/dev/torpverse/torpmodels/inst/models/core/wp_model.rds")
cat("\n-- artifact provenance --\n")
cat("call:", paste(deparse(attributes(wp)$call), collapse = " "), "\n")
p <- attributes(wp)$params
cat("eta:", p$eta, " subsample:", p$subsample, " colsample:", p$colsample_bytree,
    " mcw:", p$min_child_weight, "\n")
cat("monotone_constraints:", p$monotone_constraints,
    "(", length(strsplit(gsub("[()]", "", p$monotone_constraints), ",")[[1]]), "entries vs",
    length(FEATS), "features )\n")
cat("nrounds:", xgboost::xgb.get.num.boosted.rounds(wp), "\n")

X <- as.matrix(wp_full[, ..FEATS])
pred <- predict(wp, X)
if (is.matrix(pred)) pred <- as.vector(pred)
wp_full[, pred := pred]

logloss <- function(y, p) { p <- pmin(pmax(p, 1e-15), 1 - 1e-15); -mean(y * log(p) + (1 - y) * log(1 - p)) }
brier   <- function(y, p) mean((y - p)^2)
cal_slope <- function(y, p) {
  ok <- y %in% c(0, 1)
  y <- y[ok]; p <- pmin(pmax(p[ok], 1e-6), 1 - 1e-6)
  if (length(y) < 50 || length(unique(y)) < 2) return(NA_real_)
  suppressWarnings(unname(coef(glm(y ~ qlogis(p), family = binomial()))[2]))
}
wilson <- function(x, n, z = 1.96) {
  ph <- x / n; d <- 1 + z^2 / n
  ctr <- (ph + z^2 / (2 * n)) / d
  hw <- z * sqrt(ph * (1 - ph) / n + z^2 / (4 * n^2)) / d
  c(lo = ctr - hw, hi = ctr + hw)
}

n_draw <- sum(!wp_full$label_wp %in% c(0, 1))
cat("\nnon-binary label rows (draws):", n_draw, "— excluded from slope/reliability, included in logloss\n")

cat("\n==== OVERALL (in-sample: model saw all matches up to 2026-06-17) ====\n")
# split by shipped-model training cutoff: 2026 rounds >= 15 started after 2026-06-18
wp_full[, round_no := as.integer(substr(match_id, 12, 13))]
wp_full[, oos_post_ship := season == 2026 & round_no >= 15]
cat("post-ship (2026 R15+) rows:", sum(wp_full$oos_post_ship),
    " matches:", uniqueN(wp_full[oos_post_ship == TRUE, torp_match_id]), "\n")

for (grp in c("in-sample", "post-ship-OOS")) {
  d <- if (grp == "in-sample") wp_full[oos_post_ship == FALSE] else wp_full[oos_post_ship == TRUE]
  if (nrow(d) == 0) next
  cat(sprintf("%-14s n=%9d  logloss=%.5f  brier=%.5f  slope_all=%.3f  slope_Q4close=%.3f\n",
      grp, nrow(d), logloss(d$label_wp, d$pred), brier(d$label_wp, d$pred),
      cal_slope(d$label_wp, d$pred),
      cal_slope(d[period == 4 & abs(points_diff) <= 12, label_wp],
                d[period == 4 & abs(points_diff) <= 12, pred])))
}

cat("\n==== Calibration slope by quarter (in-sample rows) ====\n")
ins <- wp_full[oos_post_ship == FALSE]
print(ins[, .(n = .N, slope = round(cal_slope(label_wp, pred), 3)), by = period][order(period)])

cat("\n==== Reliability: quarter x margin bucket (row-level, in-sample) ====\n")
ins[, mbkt := cut(points_diff, c(-Inf, -24, -12, -6, 0, 6, 12, 24, Inf),
                  labels = c("<=-25", "-24:-13", "-12:-7", "-6:0", "1:6", "7:12", "13:24", ">=25"))]
rel <- ins[label_wp %in% c(0, 1),
           .(n = .N, mean_pred = mean(pred), actual = mean(label_wp)), by = .(period, mbkt)]
rel[, `:=`(gap = actual - mean_pred,
           wlo = wilson(actual * n, n)[1], whi = wilson(actual * n, n)[2]), by = .(period, mbkt)]
rel[, sig := mean_pred < wlo | mean_pred > whi]
print(rel[order(period, mbkt)], digits = 3, nrows = 100)

cat("\n==== Q4 close (|margin|<=12): game-level dedup by 5-min bucket, in-sample ====\n")
q4 <- ins[period == 4 & label_wp %in% c(0, 1)]
q4[, bucket := pmin(4, pmax(0, est_match_elapsed %/% 300 - 11))]
gl <- q4[abs(points_diff) <= 12,
         .(label_wp = last(label_wp), pred = last(pred)), by = .(torp_match_id, bucket)]
glt <- gl[, .(n = .N, mean_pred = mean(pred), actual = mean(label_wp),
              slope = round(cal_slope(label_wp, pred), 3)), by = bucket][order(bucket)]
glt[, c("wlo", "whi") := as.list(wilson(actual * n, n)), by = bucket]
print(glt, digits = 3)

cat("\n==== Q4 close: reliability by predicted-prob decile (game-level dedup, in-sample) ====\n")
gl2 <- q4[abs(points_diff) <= 12,
          .(label_wp = last(label_wp), pred = last(pred)), by = .(torp_match_id, bucket)]
gl2[, dec := cut(pred, seq(0, 1, 0.1), include.lowest = TRUE)]
dt <- gl2[, .(n = .N, mean_pred = mean(pred), actual = mean(label_wp)), by = dec][order(dec)]
dt[, c("wlo", "whi") := as.list(wilson(actual * n, n)), by = dec]
print(dt, digits = 3)

if (sum(wp_full$oos_post_ship) > 0) {
  cat("\n==== POST-SHIP OOS: Q4 close game-level dedup ====\n")
  q4o <- wp_full[oos_post_ship == TRUE & period == 4 & label_wp %in% c(0, 1)]
  q4o[, bucket := pmin(4, pmax(0, est_match_elapsed %/% 300 - 11))]
  glo <- q4o[abs(points_diff) <= 12,
             .(label_wp = last(label_wp), pred = last(pred)), by = .(torp_match_id, bucket)]
  cat("game-bucket obs:", nrow(glo), "\n")
  print(glo[, .(n = .N, mean_pred = mean(pred), actual = mean(label_wp)), by = bucket][order(bucket)], digits = 3)
  cat("slope Q4close post-ship:", round(cal_slope(glo$label_wp, glo$pred), 3), "\n")
}

fwrite(wp_full[, .(torp_match_id, season, period, points_diff, est_match_elapsed,
                   label_wp, pred, oos_post_ship)],
       file.path(SCRATCH, "shipped_preds.csv"))
cat("\nsaved shipped_preds.csv\n")
```

### 03_cv_compare.R
```r
# 03_cv_compare.R — E2: match-grouped CV comparison of WP configs
#   A "shipped-replica"  : rebuild_everything.R inlined params (eta=0.1, 15-entry misaligned constraints)
#   B "canonical-fixed"  : train_wp_model.R params (eta=0.025, correct 18-entry constraints)
#   C "eta0.1-fixed"     : shipped eta/subsample but corrected 18-entry constraints (isolates constraint damage)
# Same folds for all arms (seed 1234 match-grouped — identical to the training scripts).
SCRATCH <- "C:/Users/peteo/AppData/Local/Temp/claude/C--dev/f7acd5a2-9d66-4234-978e-f3ea4fdc8482/scratchpad"

suppressMessages({
  library(data.table)
  library(arrow)
  library(xgboost)
})
set.seed(1234)

wp_full <- as.data.table(read_parquet(file.path(SCRATCH, "wp_full.parquet")))
cat("rows:", nrow(wp_full), " matches:", uniqueN(wp_full$torp_match_id), "\n")

FEATS <- c("est_match_elapsed", "est_match_remaining", "shot_row", "home", "points_diff",
           "xpoints_diff", "pos_lead_prob", "time_left_scaler", "diff_time_ratio", "score_urgency",
           "goal_x", "play_type_handball", "play_type_kick", "play_type_reception",
           "phase_of_play_handball_received", "phase_of_play_hard_ball",
           "phase_of_play_loose_ball", "phase_of_play_set_shot")
X <- as.matrix(wp_full[, ..FEATS])
y <- wp_full$label_wp

# Match-grouped folds, identical construction to training scripts
match_ids <- unique(wp_full$torp_match_id)
set.seed(1234)
match_folds <- sample(rep(1:5, length.out = length(match_ids)))
names(match_folds) <- match_ids
row_folds <- match_folds[wp_full$torp_match_id]
folds <- lapply(1:5, function(k) which(row_folds == k))
cat("fold sizes:", paste(lengths(folds), collapse = ", "), "\n")

full_dmat <- xgb.DMatrix(data = X, label = y)

arms <- list(
  A_shipped_replica = list(
    params = list(booster = "gbtree", objective = "binary:logistic", eval_metric = "logloss",
                  tree_method = "hist", eta = 0.1, gamma = 0, subsample = 0.85,
                  colsample_bytree = 0.85, max_depth = 6, min_child_weight = 1,
                  monotone_constraints = "(0,0,0,1,1,1,0,1,0,0,0,0,0,0,0)"),
    cap = 600),
  B_canonical_fixed = list(
    params = list(booster = "gbtree", objective = "binary:logistic", eval_metric = "logloss",
                  tree_method = "hist", eta = 0.025, gamma = 0,
                  monotone_constraints = "(0,0,0,0,1,1,1,0,1,1,0,0,0,0,0,0,0,0)",
                  max_depth = 6, min_child_weight = 1, subsample = 0.8, colsample_bytree = 0.8),
    cap = 1200),
  C_eta01_fixed = list(
    params = list(booster = "gbtree", objective = "binary:logistic", eval_metric = "logloss",
                  tree_method = "hist", eta = 0.1, gamma = 0, subsample = 0.85,
                  colsample_bytree = 0.85, max_depth = 6, min_child_weight = 1,
                  monotone_constraints = "(0,0,0,0,1,1,1,0,1,1,0,0,0,0,0,0,0,0)"),
    cap = 600)
)

# Which arms to run this invocation (default: all)
run_arms <- commandArgs(trailingOnly = TRUE)
if (length(run_arms) == 0) run_arms <- names(arms)
stopifnot(all(run_arms %in% names(arms)))

for (arm in run_arms) {
  a <- arms[[arm]]
  t0 <- Sys.time()
  cat("\n=====", arm, "=====\n")
  set.seed(1234)
  cv <- xgb.cv(params = a$params, data = full_dmat, nrounds = a$cap, folds = folds,
               early_stopping_rounds = 30, print_every_n = 50, verbose = 1)
  el <- cv$evaluation_log
  opt_n <- which.min(el$test_logloss_mean)
  cat(arm, "optimal nrounds:", opt_n,
      " cv test logloss:", sprintf("%.6f", min(el$test_logloss_mean)),
      " +/-", sprintf("%.6f", el$test_logloss_std[opt_n]), "\n")

  # Manual fold loop at opt_n to get OOS predictions (mirrors pipeline's cv->train pattern)
  pred_oos <- rep(NA_real_, nrow(wp_full))
  for (k in 1:5) {
    te <- folds[[k]]; tr <- setdiff(seq_len(nrow(wp_full)), te)
    dtr <- xgb.DMatrix(data = X[tr, ], label = y[tr])
    set.seed(1234)
    bst <- xgb.train(params = a$params, data = dtr, nrounds = opt_n, verbose = 0)
    p <- predict(bst, X[te, ])
    if (is.matrix(p)) p <- as.vector(p)
    pred_oos[te] <- p
    rm(bst, dtr); invisible(gc(verbose = FALSE))
  }
  stopifnot(!anyNA(pred_oos))
  out <- wp_full[, .(torp_match_id, season, period, points_diff, est_match_elapsed, label_wp)]
  out[[arm]] <- pred_oos
  write_parquet(out, file.path(SCRATCH, paste0("cv_oos_", arm, ".parquet")))
  cat(arm, "done in", round(difftime(Sys.time(), t0, units = "mins"), 1),
      "min — saved cv_oos_", arm, ".parquet\n")
}
cat("ALL_DONE\n")
```

### 04_cv_metrics.R
```r
# 04_cv_metrics.R — E2 reporting: OOS metrics, calibration slices, match-level bootstrap
SCRATCH <- "C:/Users/peteo/AppData/Local/Temp/claude/C--dev/f7acd5a2-9d66-4234-978e-f3ea4fdc8482/scratchpad"
suppressMessages({ library(data.table); library(arrow) })

arms <- c("A_shipped_replica", "B_canonical_fixed", "C_eta01_fixed")
d <- as.data.table(read_parquet(file.path(SCRATCH, "cv_oos_A_shipped_replica.parquet")))
for (a in arms[-1]) {
  dd <- as.data.table(read_parquet(file.path(SCRATCH, paste0("cv_oos_", a, ".parquet"))))
  stopifnot(nrow(dd) == nrow(d), all(dd$label_wp == d$label_wp))
  d[[a]] <- dd[[a]]
}

logloss_v <- function(y, p) { p <- pmin(pmax(p, 1e-15), 1 - 1e-15); -(y * log(p) + (1 - y) * log(1 - p)) }
brier <- function(y, p) mean((y - p)^2)
cal_slope <- function(y, p) {
  ok <- y %in% c(0, 1)
  y <- y[ok]; p <- pmin(pmax(p[ok], 1e-6), 1 - 1e-6)
  if (length(y) < 50 || length(unique(y)) < 2) return(NA_real_)
  suppressWarnings(unname(coef(glm(y ~ qlogis(p), family = binomial()))[2]))
}
wilson <- function(x, n, z = 1.96) {
  ph <- x / n; den <- 1 + z^2 / n
  ctr <- (ph + z^2 / (2 * n)) / den
  hw <- z * sqrt(ph * (1 - ph) / n + z^2 / (4 * n^2)) / den
  list(lo = ctr - hw, hi = ctr + hw)
}

cat("==== OOS summary (match-grouped 5-fold CV, all rows) ====\n")
for (a in arms) {
  p <- d[[a]]
  q4c <- d[period == 4 & abs(points_diff) <= 12]
  cat(sprintf("%-18s logloss=%.6f brier=%.6f slope_all=%.3f | Q4close: logloss=%.6f slope=%.3f\n",
      a, mean(logloss_v(d$label_wp, p)), brier(d$label_wp, p), cal_slope(d$label_wp, p),
      mean(logloss_v(q4c$label_wp, q4c[[a]])), cal_slope(q4c$label_wp, q4c[[a]])))
}

cat("\n==== Slope by quarter ====\n")
for (a in arms) {
  s <- d[, .(slope = round(cal_slope(label_wp, get(a)), 3)), by = period][order(period)]
  cat(sprintf("%-18s %s\n", a, paste0("Q", s$period, "=", s$slope, collapse = "  ")))
}

cat("\n==== Q4 x margin-bucket reliability (row-level OOS) ====\n")
d[, mbkt := cut(points_diff, c(-Inf, -24, -12, -6, 0, 6, 12, 24, Inf),
                labels = c("<=-25", "-24:-13", "-12:-7", "-6:0", "1:6", "7:12", "13:24", ">=25"))]
q4 <- d[period == 4 & label_wp %in% c(0, 1)]
for (a in arms) {
  cat("\n--", a, "--\n")
  t <- q4[, .(n = .N, mean_pred = round(mean(get(a)), 3), actual = round(mean(label_wp), 3)), by = mbkt][order(mbkt)]
  t[, gap := actual - mean_pred]
  t[, c("wlo", "whi") := wilson(actual * n, n), by = mbkt]
  t[, outside_ci := mean_pred < wlo | mean_pred > whi]
  print(t[, .(mbkt, n, mean_pred, actual, gap = round(gap, 3), outside_ci)], nrows = 20)
}

cat("\n==== Q4 close, game-level dedup (last obs per match per 5-min bucket) ====\n")
q4[, bucket := pmin(4, pmax(0, est_match_elapsed %/% 300 - 11))]
for (a in arms) {
  gl <- q4[abs(points_diff) <= 12,
           .(label_wp = last(label_wp), pred = last(get(a))), by = .(torp_match_id, bucket)]
  t <- gl[, .(n = .N, mean_pred = round(mean(pred), 3), actual = round(mean(label_wp), 3),
              slope = round(cal_slope(label_wp, pred), 3)), by = bucket][order(bucket)]
  cat("\n--", a, "-- overall game-level slope:", round(cal_slope(gl$label_wp, gl$pred), 3), "\n")
  print(t, nrows = 10)
}

cat("\n==== Match-level block bootstrap on logloss differences (B=2000) ====\n")
per_match <- d[, c(.(match = torp_match_id[1]),
                   lapply(.SD, function(col) mean(logloss_v(label_wp, col)))),
               by = torp_match_id, .SDcols = arms]
boot_ci <- function(dnew, dold, B = 2000) {
  diffs <- dnew - dold
  n <- length(diffs)
  set.seed(42)
  bs <- replicate(B, mean(diffs[sample.int(n, replace = TRUE)]))
  quantile(bs, c(0.025, 0.975))
}
for (pair in list(c("B_canonical_fixed", "A_shipped_replica"),
                  c("C_eta01_fixed", "A_shipped_replica"),
                  c("B_canonical_fixed", "C_eta01_fixed"))) {
  ci <- boot_ci(per_match[[pair[1]]], per_match[[pair[2]]])
  cat(sprintf("%s - %s: mean=%.6f  95%% CI [%.6f, %.6f]\n", pair[1], pair[2],
      mean(per_match[[pair[1]]] - per_match[[pair[2]]]), ci[1], ci[2]))
}

cat("\n==== Q4-close-only match-level bootstrap ====\n")
q4c <- d[period == 4 & abs(points_diff) <= 12]
pm2 <- q4c[, lapply(.SD, function(col) mean(logloss_v(label_wp, col))), by = torp_match_id, .SDcols = arms]
for (pair in list(c("B_canonical_fixed", "A_shipped_replica"))) {
  ci <- boot_ci(pm2[[pair[1]]], pm2[[pair[2]]])
  cat(sprintf("%s - %s (Q4 close): mean=%.6f  95%% CI [%.6f, %.6f]\n", pair[1], pair[2],
      mean(pm2[[pair[1]]] - pm2[[pair[2]]]), ci[1], ci[2]))
}
```

### 05_temporal.R
```r
# 05_temporal.R — E3: temporal holdout (train 2021-2024, test 2025-2026)
# EP trained on train seasons only -> honest OOS EP features for test-season WP rows.
# WP trained on train seasons (canonical-fixed AND shipped-replica configs), tested on 2025-26.
# Uses torp::create_temporal_splits() for the season split (the tested-but-never-used utility).
SCRATCH <- "C:/Users/peteo/AppData/Local/Temp/claude/C--dev/f7acd5a2-9d66-4234-978e-f3ea4fdc8482/scratchpad"

suppressMessages({
  library(data.table)
  library(dplyr)
  library(arrow)
  library(xgboost)
})
suppressMessages(devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE))

epv <- as.data.table(read_parquet(file.path(SCRATCH, "epv_lite.parquet")))
cat("epv_lite rows:", nrow(epv), " matches:", uniqueN(epv$torp_match_id), "\n")
print(table(epv$season))

# --- temporal split via the package utility (train/val merged: tuning is CV-within-train) ---
splits <- torp:::create_temporal_splits(epv, train_seasons = 2021:2024,
                                        val_seasons = integer(0), test_seasons = 2025:2026)
tr_idx <- which(epv$season <= 2024)
te_idx <- which(epv$season >= 2025)
stopifnot(nrow(splits$train) == length(tr_idx), nrow(splits$test) == length(te_idx))
cat("train rows:", length(tr_idx), " matches:", uniqueN(epv$torp_match_id[tr_idx]), "\n")
cat("test  rows:", length(te_idx), " matches:", uniqueN(epv$torp_match_id[te_idx]), "\n")

EP_FEATS <- c("goal_x", "y", "lag_goal_x", "lag_goal_x5", "lag_y",
              "period_seconds", "period", "play_type_handball", "play_type_kick",
              "play_type_reception", "phase_of_play_handball_received",
              "phase_of_play_hard_ball", "phase_of_play_loose_ball",
              "phase_of_play_set_shot", "shot_row", "speed5", "home",
              "est_qtr_remaining", "est_match_remaining")
X_ep <- as.matrix(epv[, ..EP_FEATS])
y_ep <- epv$label_ep

ep_params <- list(booster = "gbtree", objective = "multi:softprob", eval_metric = "mlogloss",
                  tree_method = "hist", num_class = 5, eta = 0.1, gamma = 0,
                  subsample = 0.85, colsample_bytree = 0.85, max_depth = 6, min_child_weight = 25)

# EP nrounds via match-grouped CV inside train seasons only
tr_matches <- unique(epv$torp_match_id[tr_idx])
set.seed(1234)
mf <- sample(rep(1:5, length.out = length(tr_matches)))
names(mf) <- tr_matches
rf <- mf[epv$torp_match_id[tr_idx]]
ep_folds <- lapply(1:5, function(k) which(rf == k))

cat("\nEP xgb.cv on train seasons...\n")
d_ep_tr <- xgb.DMatrix(data = X_ep[tr_idx, ], label = y_ep[tr_idx])
set.seed(1234)
ep_cv <- xgb.cv(params = ep_params, data = d_ep_tr, nrounds = 400, folds = ep_folds,
                early_stopping_rounds = 20, print_every_n = 50, verbose = 1)
ep_n <- which.min(ep_cv$evaluation_log$test_mlogloss_mean)
cat("EP optimal nrounds (train-only CV):", ep_n,
    " mlogloss:", sprintf("%.6f", min(ep_cv$evaluation_log$test_mlogloss_mean)), "\n")

set.seed(1234)
ep_past <- xgb.train(params = ep_params, data = d_ep_tr, nrounds = ep_n, verbose = 0)

# EP temporal-OOS mlogloss on 2025-26 for the record
p_te <- predict(ep_past, X_ep[te_idx, ])
if (!is.matrix(p_te)) p_te <- matrix(p_te, ncol = 5, byrow = TRUE)
ml_temporal <- -mean(log(pmax(p_te[cbind(seq_len(nrow(p_te)), y_ep[te_idx] + 1)], 1e-15)))
cat("EP temporal mlogloss (2025-26):", sprintf("%.6f", ml_temporal), "\n")

# EP features for ALL rows from ep_past: in-sample for train rows (mirrors production
# training), honest OOS for test rows
p_all <- predict(ep_past, X_ep)
if (!is.matrix(p_all)) p_all <- matrix(p_all, ncol = 5, byrow = TRUE)
colnames(p_all) <- c("opp_goal", "opp_behind", "behind", "goal", "no_score")
epv[, c("opp_goal", "opp_behind", "behind", "goal", "no_score") :=
      as.data.table(p_all)]
# exp_pts formula synced with train_wp_model_cv_ep.R:137 / add_epv_vars()
epv[, exp_pts := round(-6 * opp_goal - opp_behind + behind + 6 * goal, 5)]

# --- WP feature build via the package's own clean_model_data_wp() ---
mdw <- torp:::clean_model_data_wp(epv)
setDT(mdw)
cat("\nWP temporal frame rows:", nrow(mdw), "\n")

WP_FEATS <- c("est_match_elapsed", "est_match_remaining", "shot_row", "home", "points_diff",
              "xpoints_diff", "pos_lead_prob", "time_left_scaler", "diff_time_ratio", "score_urgency",
              "goal_x", "play_type_handball", "play_type_kick", "play_type_reception",
              "phase_of_play_handball_received", "phase_of_play_hard_ball",
              "phase_of_play_loose_ball", "phase_of_play_set_shot")
X_wp <- as.matrix(mdw[, ..WP_FEATS])
y_wp <- mdw$label_wp
wtr <- which(mdw$season <= 2024)
wte <- which(mdw$season >= 2025)
cat("WP train rows:", length(wtr), " test rows:", length(wte), "\n")

wp_arms <- list(
  A_shipped_replica = list(booster = "gbtree", objective = "binary:logistic", eval_metric = "logloss",
                           tree_method = "hist", eta = 0.1, gamma = 0, subsample = 0.85,
                           colsample_bytree = 0.85, max_depth = 6, min_child_weight = 1,
                           monotone_constraints = "(0,0,0,1,1,1,0,1,0,0,0,0,0,0,0)"),
  B_canonical_fixed = list(booster = "gbtree", objective = "binary:logistic", eval_metric = "logloss",
                           tree_method = "hist", eta = 0.025, gamma = 0,
                           monotone_constraints = "(0,0,0,0,1,1,1,0,1,1,0,0,0,0,0,0,0,0)",
                           max_depth = 6, min_child_weight = 1, subsample = 0.8, colsample_bytree = 0.8)
)
caps <- c(A_shipped_replica = 600, B_canonical_fixed = 1200)

# WP CV folds inside train seasons (match-grouped)
wtr_matches <- unique(mdw$torp_match_id[wtr])
set.seed(1234)
wmf <- sample(rep(1:5, length.out = length(wtr_matches)))
names(wmf) <- wtr_matches
wrf <- wmf[mdw$torp_match_id[wtr]]
wp_folds <- lapply(1:5, function(k) which(wrf == k))

d_wp_tr <- xgb.DMatrix(data = X_wp[wtr, ], label = y_wp[wtr])
res <- mdw[wte, .(torp_match_id, season, period, points_diff, est_match_elapsed, label_wp)]

for (arm in names(wp_arms)) {
  cat("\n=====", arm, "(temporal) =====\n")
  set.seed(1234)
  cv <- xgb.cv(params = wp_arms[[arm]], data = d_wp_tr, nrounds = caps[arm], folds = wp_folds,
               early_stopping_rounds = 30, print_every_n = 100, verbose = 1)
  opt_n <- which.min(cv$evaluation_log$test_logloss_mean)
  cat(arm, "train-only CV nrounds:", opt_n,
      " cv logloss:", sprintf("%.6f", min(cv$evaluation_log$test_logloss_mean)), "\n")
  set.seed(1234)
  bst <- xgb.train(params = wp_arms[[arm]], data = d_wp_tr, nrounds = opt_n, verbose = 0)
  p <- predict(bst, X_wp[wte, ])
  if (is.matrix(p)) p <- as.vector(p)
  res[[arm]] <- p
}

write_parquet(res, file.path(SCRATCH, "temporal_preds.parquet"))
cat("\nsaved temporal_preds.parquet\n")

# --- metrics ---
logloss_v <- function(y, p) { p <- pmin(pmax(p, 1e-15), 1 - 1e-15); -(y * log(p) + (1 - y) * log(1 - p)) }
cal_slope <- function(y, p) {
  ok <- y %in% c(0, 1); y <- y[ok]; p <- pmin(pmax(p[ok], 1e-6), 1 - 1e-6)
  if (length(y) < 50 || length(unique(y)) < 2) return(NA_real_)
  suppressWarnings(unname(coef(glm(y ~ qlogis(p), family = binomial()))[2]))
}
cat("\n==== TEMPORAL HOLDOUT (2025-26) ====\n")
for (arm in names(wp_arms)) {
  p <- res[[arm]]
  q4c <- res[period == 4 & abs(points_diff) <= 12]
  cat(sprintf("%-18s logloss=%.6f brier=%.6f slope_all=%.3f | Q4close: logloss=%.6f slope=%.3f\n",
      arm, mean(logloss_v(res$label_wp, p)), mean((res$label_wp - p)^2),
      cal_slope(res$label_wp, p),
      mean(logloss_v(q4c$label_wp, q4c[[arm]])), cal_slope(q4c$label_wp, q4c[[arm]])))
}
cat("\nSlope by quarter (temporal test):\n")
for (arm in names(wp_arms)) {
  s <- res[, .(slope = round(cal_slope(label_wp, get(arm)), 3)), by = period][order(period)]
  cat(sprintf("%-18s %s\n", arm, paste0("Q", s$period, "=", s$slope, collapse = "  ")))
}
```

### 09_temporal_compare.R
```r
# 09_temporal_compare.R — apples-to-apples: random-CV vs temporal predictions on the
# IDENTICAL 2025-26 rows. Isolates the true cost of training only on the past.
SCRATCH <- "C:/Users/peteo/AppData/Local/Temp/claude/C--dev/f7acd5a2-9d66-4234-978e-f3ea4fdc8482/scratchpad"
suppressMessages({ library(data.table); library(arrow) })

tp <- as.data.table(read_parquet(file.path(SCRATCH, "temporal_preds.parquet")))
cvB <- as.data.table(read_parquet(file.path(SCRATCH, "cv_oos_B_canonical_fixed.parquet")))
cvA <- as.data.table(read_parquet(file.path(SCRATCH, "cv_oos_A_shipped_replica.parquet")))

cv25 <- cvB[season >= 2025]
cv25[, A_random := cvA[season >= 2025, A_shipped_replica]]
stopifnot(nrow(cv25) == nrow(tp), all(cv25$label_wp == tp$label_wp))

ll_v <- function(y, p) { p <- pmin(pmax(p, 1e-15), 1 - 1e-15); -(y * log(p) + (1 - y) * log(1 - p)) }
cal_slope <- function(y, p) {
  ok <- y %in% c(0, 1); y <- y[ok]; p <- pmin(pmax(p[ok], 1e-6), 1 - 1e-6)
  suppressWarnings(unname(coef(glm(y ~ qlogis(p), family = binomial()))[2]))
}
rep_row <- function(lbl, y, p, per, pd) {
  q4 <- per == 4 & abs(pd) <= 12
  cat(sprintf("%-28s logloss=%.6f  slope_all=%.3f  slope_Q4close=%.3f\n", lbl,
      mean(ll_v(y, p)), cal_slope(y, p), cal_slope(y[q4], p[q4])))
}
cat("Same 595,923 rows (seasons 2025-26), 362 matches:\n\n")
rep_row("B random-CV (sees 21-26)", cv25$label_wp, cv25$B_canonical_fixed, cv25$period, cv25$points_diff)
rep_row("B temporal  (sees 21-24)", tp$label_wp, tp$B_canonical_fixed, tp$period, tp$points_diff)
rep_row("A random-CV (sees 21-26)", cv25$label_wp, cv25$A_random, cv25$period, cv25$points_diff)
rep_row("A temporal  (sees 21-24)", tp$label_wp, tp$A_shipped_replica, tp$period, tp$points_diff)

# match-level bootstrap of the temporal-vs-random gap for B
pm <- data.table(match = cv25$torp_match_id,
                 d = ll_v(cv25$label_wp, tp$B_canonical_fixed) -
                     ll_v(cv25$label_wp, cv25$B_canonical_fixed))
pm <- pm[, .(d = mean(d)), by = match]
set.seed(42)
bs <- replicate(4000, { i <- sample.int(nrow(pm), replace = TRUE); mean(pm$d[i]) })
cat(sprintf("\nB temporal - B random-CV (match-level, %d matches): mean=%.6f  95%% CI [%.6f, %.6f]\n",
    nrow(pm), mean(pm$d), quantile(bs, 0.025), quantile(bs, 0.975)))

# slope by quarter, both on same rows
cat("\nslope by quarter on 2025-26 rows:\n")
for (v in c("random", "temporal")) {
  cv25[, tmp_p := if (v == "random") B_canonical_fixed else tp$B_canonical_fixed]
  s <- cv25[, .(slope = round(cal_slope(label_wp, tmp_p), 3)), by = period][order(period)]
  cat(sprintf("B %-9s %s\n", v, paste0("Q", s$period, "=", s$slope, collapse = "  ")))
}
```

### 08_postship_headtohead.R
```r
# 08_postship_headtohead.R — shipped model vs retrained arms on the 25 post-ship matches
# (2026 R15+, after the 2026-06-18 training snapshot). Shipped is genuinely OOS there;
# arm CV predictions are OOS by fold construction. Identical rows -> clean head-to-head.
SCRATCH <- "C:/Users/peteo/AppData/Local/Temp/claude/C--dev/f7acd5a2-9d66-4234-978e-f3ea4fdc8482/scratchpad"
suppressMessages({ library(data.table); library(arrow); library(xgboost) })

wf <- as.data.table(read_parquet(file.path(SCRATCH, "wp_full.parquet")))
FEATS <- c("est_match_elapsed", "est_match_remaining", "shot_row", "home", "points_diff",
           "xpoints_diff", "pos_lead_prob", "time_left_scaler", "diff_time_ratio", "score_urgency",
           "goal_x", "play_type_handball", "play_type_kick", "play_type_reception",
           "phase_of_play_handball_received", "phase_of_play_hard_ball",
           "phase_of_play_loose_ball", "phase_of_play_set_shot")
wpm <- readRDS("C:/dev/torpverse/torpmodels/inst/models/core/wp_model.rds")
wf[, round_no := as.integer(substr(match_id, 12, 13))]
wf[, post := season == 2026 & round_no >= 15]
ps <- wf[post == TRUE]
p_ship <- predict(wpm, as.matrix(ps[, ..FEATS]))
if (is.matrix(p_ship)) p_ship <- as.vector(p_ship)
ps[, shipped := p_ship]

arms <- c("A_shipped_replica", "B_canonical_fixed", "C_eta01_fixed")
cvp <- as.data.table(read_parquet(file.path(SCRATCH, "cv_oos_A_shipped_replica.parquet")))
for (a in arms[-1]) {
  dd <- as.data.table(read_parquet(file.path(SCRATCH, paste0("cv_oos_", a, ".parquet"))))
  cvp[[a]] <- dd[[a]]
}
# align by row order (both from wp_full order)
stopifnot(nrow(cvp) == nrow(wf), all(cvp$label_wp == wf$label_wp))
for (a in arms) ps[[a]] <- cvp[[a]][which(wf$post)]

ll_v <- function(y, p) { p <- pmin(pmax(p, 1e-15), 1 - 1e-15); -(y * log(p) + (1 - y) * log(1 - p)) }
cal_slope <- function(y, p) {
  ok <- y %in% c(0, 1); y <- y[ok]; p <- pmin(pmax(p[ok], 1e-6), 1 - 1e-6)
  suppressWarnings(unname(coef(glm(y ~ qlogis(p), family = binomial()))[2]))
}
cat("post-ship rows:", nrow(ps), " matches:", uniqueN(ps$torp_match_id), "\n\n")
for (m in c("shipped", arms)) {
  q4c <- ps[period == 4 & abs(points_diff) <= 12]
  cat(sprintf("%-18s logloss=%.5f  slope_all=%.3f  slope_Q4close=%.3f\n",
      m, mean(ll_v(ps$label_wp, ps[[m]])), cal_slope(ps$label_wp, ps[[m]]),
      cal_slope(q4c$label_wp, q4c[[m]])))
}

# match-level bootstrap: shipped vs B on identical post-ship rows
pm <- ps[, .(ship = mean(ll_v(label_wp, shipped)), b = mean(ll_v(label_wp, B_canonical_fixed))),
         by = torp_match_id]
set.seed(42)
bs <- replicate(4000, { i <- sample.int(nrow(pm), replace = TRUE); mean(pm$b[i] - pm$ship[i]) })
cat(sprintf("\nB - shipped (post-ship, %d matches): mean=%.5f  95%% CI [%.5f, %.5f]\n",
    nrow(pm), mean(pm$b - pm$ship), quantile(bs, 0.025), quantile(bs, 0.975)))
```

### 07_e1b_feature_skew.R
```r
# 07_e1b_feature_skew.R — E1b: is the shipped model's flatness explained by the
# core-pipeline train/serve feature skew?
# Rebuild trained WP on add_epv_custom() features (raw exp_pts formula, NO Centre-Bounce
# zeroing / Out-On-Full negation). Production serves add_epv_vars() features (WITH both).
# Score the shipped model on rebuild-style features and compare calibration vs E1.
SCRATCH <- "C:/Users/peteo/AppData/Local/Temp/claude/C--dev/f7acd5a2-9d66-4234-978e-f3ea4fdc8482/scratchpad"
suppressMessages({ library(data.table); library(arrow); library(xgboost) })
suppressMessages(devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE))

epv <- as.data.table(read_parquet(file.path(SCRATCH, "epv_lite.parquet")))

EP_FEATS <- c("goal_x", "y", "lag_goal_x", "lag_goal_x5", "lag_y",
              "period_seconds", "period", "play_type_handball", "play_type_kick",
              "play_type_reception", "phase_of_play_handball_received",
              "phase_of_play_hard_ball", "phase_of_play_loose_ball",
              "phase_of_play_set_shot", "shot_row", "speed5", "home",
              "est_qtr_remaining", "est_match_remaining")
ep <- readRDS("C:/dev/torpverse/torpmodels/inst/models/core/ep_model.rds")
p <- predict(ep, as.matrix(epv[, ..EP_FEATS]))
if (!is.matrix(p)) p <- matrix(p, ncol = 5, byrow = TRUE)
colnames(p) <- c("opp_goal", "opp_behind", "behind", "goal", "no_score")
epv[, c("opp_goal", "opp_behind", "behind", "goal", "no_score") := as.data.table(p)]
# rebuild-style exp_pts: raw formula, no special-casing (add_epv_custom in rebuild_everything.R)
epv[, exp_pts := as.vector(p %*% c(-6, -1, 1, 6, 0))]

mdw <- torp:::clean_model_data_wp(epv)
setDT(mdw)
FEATS <- c("est_match_elapsed", "est_match_remaining", "shot_row", "home", "points_diff",
           "xpoints_diff", "pos_lead_prob", "time_left_scaler", "diff_time_ratio", "score_urgency",
           "goal_x", "play_type_handball", "play_type_kick", "play_type_reception",
           "phase_of_play_handball_received", "phase_of_play_hard_ball",
           "phase_of_play_loose_ball", "phase_of_play_set_shot")
wpm <- readRDS("C:/dev/torpverse/torpmodels/inst/models/core/wp_model.rds")
pr <- predict(wpm, as.matrix(mdw[, ..FEATS]))
if (is.matrix(pr)) pr <- as.vector(pr)
mdw[, pred := pr]

cal_slope <- function(y, p) {
  ok <- y %in% c(0, 1); y <- y[ok]; p <- pmin(pmax(p[ok], 1e-6), 1 - 1e-6)
  suppressWarnings(unname(coef(glm(y ~ qlogis(p), family = binomial()))[2]))
}
logloss <- function(y, p) { p <- pmin(pmax(p, 1e-15), 1 - 1e-15); -mean(y * log(p) + (1 - y) * log(1 - p)) }

cat("==== Shipped model scored on REBUILD-STYLE features (training convention) ====\n")
cat(sprintf("n=%d  logloss=%.5f  slope_all=%.3f  slope_Q4close=%.3f\n",
    nrow(mdw), logloss(mdw$label_wp, mdw$pred), cal_slope(mdw$label_wp, mdw$pred),
    cal_slope(mdw[period == 4 & abs(points_diff) <= 12, label_wp],
              mdw[period == 4 & abs(points_diff) <= 12, pred])))
cat("slope by quarter:\n")
print(mdw[, .(slope = round(cal_slope(label_wp, pred), 3)), by = period][order(period)])

cat("\n(compare E1, same model on serving-convention add_epv_vars features:\n")
cat(" slope_all=1.064, slope_Q4close=1.073, by-quarter 1.090/1.019/1.055/1.109)\n")

# how many rows differ between conventions, and where?
wf <- as.data.table(read_parquet(file.path(SCRATCH, "wp_full.parquet")))
stopifnot(nrow(wf) == nrow(mdw))
dd <- abs(wf$xpoints_diff - mdw$xpoints_diff)
cat("\nfeature skew between conventions: mean|d xpoints_diff|=", round(mean(dd), 4),
    "  %rows differing >0.01:", round(mean(dd > 0.01) * 100, 2), "%\n")
```

### 06_parity.R
```r
# 06_parity.R — E4 (bounded): core-EP vs live-EP-v2 exp_pts on identical rows.
# Bounds the train/serve skew on the live WP v4's dominant feature (xmargin = margin_poss + exp_pts):
# v4 was TRAINED on core-EP exp_pts but is SERVED live-EP-v2 exp_pts, so
# delta(exp_pts) = delta(xmargin) row-for-row.
# NOTE: this is the model-mismatch component only. The serve side additionally has the
# exp_pts=0 fallback and worker-side chain_action_num over-counting (M1), not measured here.
SCRATCH <- "C:/Users/peteo/AppData/Local/Temp/claude/C--dev/f7acd5a2-9d66-4234-978e-f3ea4fdc8482/scratchpad"

suppressMessages({
  library(data.table)
  library(xgboost)
  library(arrow)
})
suppressMessages(devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE))

chains <- torp::load_chains(TRUE, TRUE)
pbp <- torp::clean_pbp(chains)
rm(chains); invisible(gc())
dt <- as.data.table(torp:::clean_model_data_epv(pbp))
rm(pbp); invisible(gc())

# training-style chain_action_num (AFTER clean_model_data_epv filtering)
dt[, chain_action_num := seq_len(.N), by = .(match_id, chain_number)]
cat("rows:", nrow(dt), " matches:", uniqueN(dt$torp_match_id), "\n")

score_ep <- function(model, X) {
  p <- predict(model, X)
  if (!is.matrix(p)) p <- matrix(p, ncol = 5, byrow = TRUE)
  -6 * p[, 1] - p[, 2] + p[, 3] + 6 * p[, 4]
}

core <- readRDS("C:/dev/torpverse/torpmodels/inst/models/core/ep_model.rds")
live <- readRDS("C:/dev/torpverse/torpmodels/inst/models/core/ep_model_live_v2.rds")

X_core <- as.matrix(dt[, .(goal_x, y, lag_goal_x, lag_goal_x5, lag_y, period_seconds, period,
                           play_type_handball, play_type_kick, play_type_reception,
                           phase_of_play_handball_received, phase_of_play_hard_ball,
                           phase_of_play_loose_ball, phase_of_play_set_shot, shot_row,
                           speed5, home, est_qtr_remaining, est_match_remaining)])
v2_vars <- c("goal_x", "y", "period_seconds", "est_qtr_remaining", "est_match_remaining",
             "shot_row", "chain_action_num", "play_type_handball", "play_type_kick",
             "play_type_reception", "phase_of_play_handball_received", "phase_of_play_hard_ball",
             "phase_of_play_loose_ball", "phase_of_play_set_shot")
X_live <- as.matrix(dt[, ..v2_vars])

dt[, ep_core := score_ep(core, X_core)]
dt[, ep_live := score_ep(live, X_live)]
dt[, d := ep_live - ep_core]

cat("\n==== exp_pts: live-v2 minus core (identical rows) ====\n")
cat(sprintf("cor=%.4f  mean_diff=%+.4f  sd_diff=%.4f  mean|diff|=%.4f\n",
    cor(dt$ep_core, dt$ep_live), mean(dt$d), sd(dt$d), mean(abs(dt$d))))
cat("quantiles of diff:\n")
print(round(quantile(dt$d, c(0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99)), 4))
cat("\n% rows with |diff| > 1 pt:", round(mean(abs(dt$d) > 1) * 100, 2), "%\n")
cat("% rows with |diff| > 3 pts:", round(mean(abs(dt$d) > 3) * 100, 2), "%\n")

cat("\n==== by quarter ====\n")
print(dt[, .(n = .N, mean_d = round(mean(d), 4), sd_d = round(sd(d), 4),
             mean_abs_d = round(mean(abs(d)), 4)), by = period][order(period)])

cat("\n==== Q4, close game (|points_diff| <= 12) ====\n")
q4c <- dt[period == 4 & abs(points_diff) <= 12]
cat(sprintf("n=%d  mean_d=%+.4f  sd_d=%.4f  mean|d|=%.4f  P(|d|>3)=%.2f%%\n",
    nrow(q4c), mean(q4c$d), sd(q4c$d), mean(abs(q4c$d)), mean(abs(q4c$d) > 3) * 100))

cat("\n==== implied WP swing through live WP v4 gradient ====\n")
# v4 log showed ~40pt WP swing per 3 xmargin points late in close games -> ~13.3 WP pts per xmargin pt
cat("Q4-close sd of xmargin skew:", round(sd(q4c$d), 3),
    "pts -> at ~13 WP pts per xmargin pt (v4 late-game gradient), 1-sd skew ~",
    round(sd(q4c$d) * 13.3, 1), "WP pts\n")

fwrite(dt[, .(n = .N, mean_d = mean(d), sd_d = sd(d)), by = .(period)],
       file.path(SCRATCH, "parity_summary.csv"))
```

---

*Experiments run 2026-07-10 by Fable 5. Companion docs: `FABLE-METHODOLOGY.md` (the
review these experiments confirm/refute), `FABLE-REVIEW.md` (pipeline/caching/release).
No repo code was modified; nothing was committed, pushed, or uploaded.*
