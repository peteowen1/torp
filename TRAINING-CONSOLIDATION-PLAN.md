# TRAINING-CONSOLIDATION-PLAN.md — One canonical training path for torpverse models

**Date:** 2026-07-10
**Author:** Claude Fable 5 (design pass; no code modified)
**Inputs:** `torp/FABLE-REVIEW.md` §9 (F1–F5, F6, F8, F16–F18), `torp/FABLE-METHODOLOGY.md` (C2, M7, adversarial-verification addendum), full read of every training entry point.
**Executor profile:** written for a Sonnet-class agent. Every decision is made here; do not re-litigate design. Where a step says "same commit", that is a correctness requirement, not a style preference.

---

## 0. Problem restated (what this plan closes)

| ID | Defect | Root cause |
|----|--------|-----------|
| F1 | Production `wp_model.rds` trained with a **15-entry monotone-constraint string against 18 features** (xgboost silently accepts), eta 0.1 vs canonical 0.025 | Hyperparameters + constraint vector hand-inlined in `rebuild_everything.R:465-471`, drifted from the canonical scripts and from `select_wp_model_vars()` |
| F2 | `rebuild_everything.R` non-interactive default `start_from = 6` silently skips scrape/retrain; interactive prompt displays `[1]` but Enter gives 6 | `rebuild_everything.R:79`, `:88` |
| F3 | `train_shot_model.R` uploads the GAM but not its `shot_player_df.rds` sidecar — released pair drifts out of sync | Upload block at `train_shot_model.R:94-102` covers one file |
| F4 | Three EP/WP entry points, three training windows (`TRUE,TRUE` / hardcoded `2021:2025` / `2021:(current-1)`), all writing the same unversioned release asset | Window defined per-script; no metadata on artifacts |
| F5 | Rebuild Phase 4d injects `UPLOAD_TO_GITHUB <- TRUE` into an env, then `train_match_models.R:14` unconditionally reassigns `FALSE`; `HOLDOUT_SEASON` no longer exists — match GAMs never uploaded by rebuild, silently | Script-level config clobbering; duplicate publish path |
| C2 | Shipped `wp_model.rds` **proven** (stored params + training call) to come from rebuild's inlined phase; CV-EP model shipped March, silently overwritten June; docs name `train_wp_model.R` as THE script | No provenance stamped in artifacts; multiple writers to one filename |
| Docs | `torpmodels/CLAUDE.md` + `ARCHITECTURE.md` describe the wrong training scripts and a dead `HOLDOUT_SEASON` knob; `torp/CLAUDE.md` says `data-raw/02-models/` holds "EP/WP/xG/shot training" (it doesn't) | Drift |

**Constraint:** single-maintainer ecosystem. No new services, no training CI. R scripts + piggyback to GitHub Releases only.

---

## 1. Target architecture

### 1.1 Principles

1. **One fitting function per model.** Everything else (CLI script, rebuild, wrappers) *delegates* to it with arguments — never re-states hyperparameters, feature lists, constraints, or windows.
2. **Feature order and monotone constraints have a single source of truth**: the selector constants in `torp/R/clean_features.R`. Constraints are *derived* from them at train time, so F1's class (positional misalignment) becomes structurally impossible.
3. **The training window is defined once** (`default_training_seasons()`), overridable per invocation, always recorded in the artifact.
4. **Every artifact carries a `torp_meta` attribute** (script, git SHAs, window, params, CV metric, timestamp) and every upload appends to a release-level `models_manifest.json` — a silent overwrite like June's becomes detectable by inspection, not forensics.
5. **Uploads are atomic groups**: an artifact and its sidecar(s) either both publish or the publish aborts (F3).
6. **Match GAMs have exactly one publisher**: torp's `run_predictions_pipeline()` (the daily CI path, already live). The torpmodels rolling-eval script becomes evaluation-only.
7. **CV-EP is the canonical WP training method** (per the adversarial verification: cheap hygiene, and it was the intended shipped model in March). The in-sample variant survives only as a `--insample-ep` comparison flag that refuses to upload.

### 1.2 File layout (after migration)

```
torp/
  R/clean_features.R              # + EPV_MODEL_FEATURES, WP_MODEL_FEATURES,
                                  #   WP_MONOTONE_INCREASING, wp_monotone_constraints()
  R/match_model.R                 # run_predictions_pipeline(): match-GAM publish gains
                                  #   metadata stamp + manifest update (sole match publisher)
  tests/testthat/test-model-constraints.R   # NEW (F1 regression)
  data-raw/rebuild_everything.R   # Phase 4 = thin delegation to torpmodels lib;
                                  #   Phase 4d deleted; start_from default fixed

torpmodels/
  R/model_meta.R                  # NEW: build_model_meta(), stamp_model_meta(),
                                  #   model_meta(), describe_model_meta()
  R/publish.R                     # NEW: publish_model_group(), update_models_manifest(),
                                  #   check_manifest_sync(), .MODEL_GROUPS
  R/load_model.R                  # load_torp_model(): print provenance, warn if meta absent
  tests/testthat/test-model_meta.R    # NEW
  tests/testthat/test-publish.R       # NEW
  data-raw/
    train_models.R                # NEW — THE canonical CLI entry point (EP/WP/shot)
    lib/train_lib.R               # NEW — fitting functions; no side effects at source time
    01-ep-model/train_ep_model_live_v2.R   # kept (canonical live EP export; F6 reorder)
    04-match-model/train_match_models.R    # kept, eval-only (upload block removed)
    04-match-model/eval_squiggle_rank.R    # kept (consumer of the eval harness)
    05-live-wp-model/train_live_wp_chain_v4.R  # kept (canonical live chain-WP export)
    05-live-wp-model/train_live_wp_model.R     # kept (canonical browser GAM lookup; F18 fix)
    02-wp-model/validate_cv_ep_wp.R  # kept, refactored to source lib/train_lib.R

  DELETED:
    01-ep-model/train_ep_model.R
    01-ep-model/train_ep_model_run.R
    01-ep-model/train_ep_model_live.R          # superseded by v2
    02-wp-model/train_wp_model.R
    02-wp-model/train_wp_model_run.R
    02-wp-model/train_wp_model_cv_ep.R         # logic absorbed into lib
    03-shot-model/train_shot_model.R
    03-shot-model/train_shot_model_run.R
    05-live-wp-model/train_live_wp_chain.R     # superseded by v4
    05-live-wp-model/train_live_wp_chain_v2.R
    05-live-wp-model/train_live_wp_chain_v3.R
    05-live-wp-model/train_live_wp_xgb.R       # experimental, superseded
```

(Git history retains all deleted scripts; do not archive copies.)

### 1.3 Who calls whom

```
Manual retrain:        Rscript torpmodels/data-raw/train_models.R ep wp shot
                            └─ sources lib/train_lib.R
                                 ├─ torp:::select_epv_model_vars / select_wp_model_vars   (feature order)
                                 ├─ torp:::wp_monotone_constraints()                      (derived, F1)
                                 ├─ default_training_seasons()                            (window, F4)
                                 ├─ torpmodels::build_model_meta / stamp_model_meta       (provenance, C2)
                                 └─ torpmodels::publish_model_group                       (atomic pairs, F3; manifest)

Nuclear rebuild:       torp/data-raw/rebuild_everything.R  Phase 4
                            └─ sources the SAME lib/train_lib.R, calls train_core_models(
                                   c("ep","wp","shot"), seasons = training_seasons,
                                   upload = !skip_model_upload)                           (F1/F4/F5 for rebuild)
                       Phase 9 → run_predictions_pipeline()  (match GAMs, as today)

Daily CI:              torp .github daily-ratings-predictions.yml
                            └─ run_predictions_pipeline()  → .train_match_gams()
                                 └─ stamp_model_meta + publish via manifest               (sole match publisher)

Live exports (manual): train_ep_model_live_v2.R, train_live_wp_chain_v4.R,
                       train_live_wp_model.R, torp/scripts/live-model-export.R
                            └─ each is the single script for its JSON artifact;
                               envelopes extended with git SHA + script name
```

### 1.4 Single source of truth for features & constraints (torp)

In `torp/R/clean_features.R`, replace the inline vectors inside the selectors with package-level constants. **The literal contents below are the current behavior — copy exactly; any diff is a bug.**

```r
# Order is load-bearing: xgboost monotone constraints and the serving path
# (get_epv_preds/get_wp_preds -> model.matrix) both consume this order.
EPV_MODEL_FEATURES <- c(
  "goal_x", "y", "lag_goal_x", "lag_goal_x5", "lag_y",
  "period_seconds", "period", "play_type_handball", "play_type_kick",
  "play_type_reception", "phase_of_play_handball_received",
  "phase_of_play_hard_ball", "phase_of_play_loose_ball",
  "phase_of_play_set_shot", "shot_row", "speed5", "home",
  "est_qtr_remaining", "est_match_remaining"
)

WP_MODEL_FEATURES <- c(
  "est_match_elapsed", "est_match_remaining", "shot_row", "home", "points_diff",
  "xpoints_diff", "pos_lead_prob", "time_left_scaler", "diff_time_ratio", "score_urgency",
  "goal_x",
  "play_type_handball", "play_type_kick", "play_type_reception",
  "phase_of_play_handball_received", "phase_of_play_hard_ball",
  "phase_of_play_loose_ball", "phase_of_play_set_shot"
)

# Monotone-increasing WP features, BY NAME (never by position).
WP_MONOTONE_INCREASING <- c(
  "points_diff", "xpoints_diff", "pos_lead_prob", "diff_time_ratio", "score_urgency"
)

select_epv_model_vars <- function(df, label = FALSE) {
  vars <- if (label) c(EPV_MODEL_FEATURES, "label_ep") else EPV_MODEL_FEATURES
  df |> dplyr::select(dplyr::all_of(vars))
}

select_wp_model_vars <- function(df) {
  df |> dplyr::select(dplyr::all_of(WP_MODEL_FEATURES))
}

#' Derive the xgboost monotone-constraint string for the WP model.
#' Length and positions always track WP_MODEL_FEATURES — F1 cannot recur.
#' @keywords internal
wp_monotone_constraints <- function() {
  missing <- setdiff(WP_MONOTONE_INCREASING, WP_MODEL_FEATURES)
  if (length(missing) > 0) {
    cli::cli_abort("WP_MONOTONE_INCREASING names not in WP_MODEL_FEATURES: {missing}")
  }
  paste0("(", paste(as.integer(WP_MODEL_FEATURES %in% WP_MONOTONE_INCREASING),
                    collapse = ","), ")")
}
```

Sanity anchor for the executor: `wp_monotone_constraints()` must reproduce **exactly** the canonical string `"(0,0,0,0,1,1,1,0,1,1,0,0,0,0,0,0,0,0)"` from `train_wp_model.R:53` / `train_wp_model_cv_ep.R:154` (+1 at positions 5, 6, 7, 9, 10). This is asserted by the new test (§4.1). Keep all of these `@keywords internal` (no `@export`) — avoids the NAMESPACE/pkgdown churn; the training lib sees them via `devtools::load_all(torp)` (the existing wrapper convention).

### 1.5 Canonical hyperparameters (torpmodels lib — defined once)

In `lib/train_lib.R`:

```r
ep_params <- function() list(
  booster = "gbtree", objective = "multi:softprob", eval_metric = "mlogloss",
  tree_method = "hist", num_class = 5, eta = 0.1, gamma = 0,
  subsample = 0.85, colsample_bytree = 0.85, max_depth = 6, min_child_weight = 25
)
# Source: train_ep_model.R:39-51 == rebuild:379-385 == cv_ep:49-61 (all three agree).

wp_params <- function() list(
  booster = "gbtree", objective = "binary:logistic", eval_metric = "logloss",
  tree_method = "hist", eta = 0.025, gamma = 0,
  monotone_constraints = torp:::wp_monotone_constraints(),   # DERIVED (F1)
  max_depth = 6, min_child_weight = 1, subsample = 0.8, colsample_bytree = 0.8
)
# Source: train_wp_model.R:46-58 == cv_ep:147-159. The rebuild variant
# (eta 0.1, subsample/colsample 0.85, 15-entry constraints,
# rebuild_everything.R:465-471) is RETIRED — it is the F1 bug.
```

WP data prep decision: canonical prep is the `train_wp_model_cv_ep.R` path — `clean_model_data_epv()` → inject EP preds → `clean_model_data_wp()`, **without** the `clean_shots_data() |> add_shot_vars()` insertion that rebuild 4b adds at `rebuild_everything.R:458-462` (no shot-derived column is in `WP_MODEL_FEATURES`; the insertion is dead weight and a drift source).

Model-matrix decision: always `stats::model.matrix(~ . + 0, data = vars, na.action = na.pass)` followed by `stopifnot(nrow(X) == nrow(vars))` and `stopifnot(identical(colnames(X), <the feature constant>))` — makes silent row-drops and column reordering loud.

### 1.6 The training library — `torpmodels/data-raw/lib/train_lib.R`

Plain functions, **no `library()` calls and no top-level execution** (so tests can `source()` it). Signatures:

```r
default_training_seasons <- function() 2021:(torp::get_afl_season() - 1L)
# THE window (F4). Rationale: completed seasons only — matches rebuild's intent
# and keeps the in-progress season available as an OOS check. Every trainer
# takes `seasons` and defaults to this.

load_training_pbp <- function(seasons)
# load_chains(seasons, rounds = TRUE) -> clean_pbp() -> clean_model_data_epv().
# Loaded ONCE and shared by EP and WP within a run.

make_match_folds <- function(match_ids, k = 5L, seed = 1234L)
# The identical fold construction currently copy-pasted in 6 scripts.

fit_ep <- function(model_data_epv, params = ep_params(), nrounds_max = 500L)
# -> list(model, optimal_nrounds, cv_logloss, X, y, folds)
# CV via xgb.cv with match folds, early_stopping_rounds = 20,
# best_iteration with which.min fallback (rebuild:408-411 pattern).

cv_ep_oos_preds <- function(X, y, folds, row_folds, params, nrounds)
# The 5-fold OOS EP prediction loop from train_wp_model_cv_ep.R:89-125,
# including the is.matrix()/byrow=TRUE guard and stopifnot(!anyNA(...)).

build_wp_data <- function(model_data_epv, oos_ep_preds)
# Inject the 5 class-prob columns + exp_pts (cv_ep:131-137 formula),
# then clean_model_data_wp().

fit_wp <- function(model_data_wp, params = wp_params(), nrounds_max = 500L)
# Same CV/train shape as fit_ep. BEFORE training, run validate_wp_spec(X, params).

validate_wp_spec <- function(X, params)
# Hard train-time F1 guard:
#   n_constraints <- length(strsplit(gsub("[()]", "", params$monotone_constraints), ",")[[1]])
#   stopifnot(n_constraints == ncol(X))
#   stopifnot(identical(colnames(X), torp:::WP_MODEL_FEATURES))

fit_shot <- function(seasons)
# Body of train_shot_model.R:16-76 (load_pbp(seasons, TRUE), filters, fct_lump_min,
# player mapping, mgcv::bam ocat). -> list(model, shot_player_df).
# NOTE window unification: current script uses seasons = TRUE; rebuild uses
# training_seasons. Canonical: `seasons` argument, default default_training_seasons().

train_core_models <- function(models = c("ep", "wp", "shot"),
                              seasons = default_training_seasons(),
                              upload = TRUE,
                              wp_ep_source = c("cv", "insample"),
                              output_dir = file.path("inst", "models", "core"))
# Orchestrator used by BOTH train_models.R and rebuild_everything.R Phase 4.
# - trains in dependency order (ep before wp when both requested)
# - wp_ep_source = "insample" forces upload = FALSE with a loud message
#   (comparison/debug only — the old train_wp_model.R semantics)
# - stamps meta on every model (build_model_meta + stamp_model_meta)
# - saveRDS to output_dir with the EXISTING filenames
#   (ep_model.rds / wp_model.rds / shot_ocat_mdl.rds / shot_player_df.rds)
# - if (upload) publish_model_group() per model group (atomic pairs)
# - returns a named list of meta objects (for rebuild's summary)
```

### 1.7 The CLI entry point — `torpmodels/data-raw/train_models.R`

```
Usage (from torpmodels root):
  Rscript data-raw/train_models.R ep wp shot                 # canonical full retrain + upload
  Rscript data-raw/train_models.R wp --no-upload             # local-only
  Rscript data-raw/train_models.R ep wp --seasons 2021 2024  # explicit window
  Rscript data-raw/train_models.R wp --insample-ep           # legacy comparison; never uploads
```

Responsibilities: `library()` calls; locate + `devtools::load_all()` dev torp (reuse the relative-path search from `train_wp_model_cv_ep.R:19-30`, plus optional `--torp <path>`); parse args (`--seasons` takes **exactly two** values and validates them — F16's lesson); source `lib/train_lib.R`; call `train_core_models()`; print each model's `describe_model_meta()` at the end. This script replaces the `*_run.R` wrappers: their only real value was "load dev torp first", which is now the default.

### 1.8 Provenance metadata (torpmodels R/ — `model_meta.R`)

```r
build_model_meta <- function(model_name, seasons, params, feature_names,
                             cv_metric = NA_real_, n_rows = NA_integer_,
                             n_matches = NA_integer_, extra = list())
# Returns a list with REQUIRED fields:
#   model, schema_version = 1L, trained_at (ISO-8601 UTC), script,
#   seasons (range string "2021-2025"), n_rows, n_matches,
#   params (the full list, incl. monotone_constraints string),
#   feature_names, cv_metric,
#   torp_sha, torpmodels_sha       (via git -C <root> rev-parse --short HEAD,
#                                   NA_character_ if git unavailable),
#   torp_version (packageVersion), r_version, xgboost_version
# `extra` merges in model-specific fields, e.g. for WP:
#   ep_source = "cv" | "insample", ep_artifact_sha256 (sha256 of the ep_model.rds
#   shipped alongside, or of the currently released one when WP trains alone).

stamp_model_meta <- function(object, meta)   # attr(object, "torp_meta") <- meta; object
model_meta       <- function(object)         # attr(object, "torp_meta", exact = TRUE)
describe_model_meta <- function(meta)        # one-line cli summary
```

RDS round-trips arbitrary attributes on xgb.Booster / bam / tibble objects, so this works for every core model including `shot_player_df`. **Loader change** (`load_torp_model()`, `torpmodels/R/load_model.R:122-155`): after `safe_read_rds()`, if `verbose` and meta exists → `describe_model_meta()`; if meta absent → `cli_warn("{model_name} has no torp_meta — trained before provenance stamping or via a non-canonical path")`. Never hard-fail (old artifacts must keep loading).

### 1.9 Atomic publish + manifest (torpmodels R/ — `publish.R`)

```r
.MODEL_GROUPS <- list(
  ep    = "ep_model.rds",
  wp    = "wp_model.rds",
  shot  = c("shot_ocat_mdl.rds", "shot_player_df.rds"),   # the F3 pair
  match = c("match_gams.rds", "match_xgb_pipeline.rds")
)

publish_model_group <- function(group, dir, repo = get_torpmodels_repo(),
                                tag = "core-models", update_manifest = TRUE)
# 1. files <- .MODEL_GROUPS[[group]]; abort unless ALL exist in `dir` (F3 guard:
#    a partial group never starts uploading).
# 2. Sys.setenv(piggyback_cache_duration = 1)  (P6 lesson).
# 3. pb_upload each file; on any failure abort with a list of what did/didn't
#    upload (operator re-runs; piggyback re-upload is idempotent per file).
# 4. if (update_manifest) update_models_manifest(files, dir, repo, tag).

update_models_manifest <- function(files, dir, repo, tag)
# Read-modify-write with the C2/P1 lesson applied:
#   - pb_download("models_manifest.json"); a 404 => start a fresh manifest;
#     ANY OTHER error => cli_abort (never clobber the ledger on a transient).
#   - For each file: entry <- list(sha256 = digest::digest(file = path, algo = "sha256"),
#     size, uploaded_at, plus a meta subset (model, script, seasons, torp_sha,
#     params_hash = digest of params list, cv_metric)).
#   - Move the previous current entry onto that artifact's `history` (cap 20).
#   - Write JSON, pb_upload manifest LAST (so artifacts land before the ledger
#     claims them).

check_manifest_sync <- function(repo = get_torpmodels_repo(), tag = "core-models")
# Detection for June-style silent overwrites: `gh api` the release assets,
# compare each asset's updatedAt/size against the manifest entry; report any
# artifact newer than its manifest record ("uploaded outside the canonical
# path") and any manifest entry without an asset. Exported; runnable ad hoc
# and referenced from the verse-survey skill docs.
```

Manifest schema (asset `models_manifest.json` on the `core-models` release):

```json
{
  "schema_version": 1,
  "updated_at": "2026-07-11T02:15:00Z",
  "artifacts": {
    "wp_model.rds": {
      "sha256": "…", "size": 1234567, "uploaded_at": "…",
      "model": "wp", "script": "train_models.R", "seasons": "2021-2025",
      "torp_sha": "abc1234", "torpmodels_sha": "def5678",
      "params_hash": "…", "cv_metric": 0.45631,
      "history": [ { "...previous entries, newest first, max 20..." } ]
    }
  }
}
```

Versioned-filename decision: **manifest history is the provenance ledger; no timestamped RDS copies by default** (release bloat for a single maintainer). `train_models.R --archive` may additionally upload `wp_model_<yyyymmdd>_<sha7>.rds` when a keep-forever snapshot is wanted; implement as a trivial extra `pb_upload`, not required for v1.

New torpmodels dependency: `digest` (Imports) — tiny, zero-dep. `piggyback` moves from the current usage into `publish.R` (already a dependency).

### 1.10 Match GAMs — one publisher

- **Canonical fitter:** `torp/R/match_train.R::.train_match_gams()` (already shared).
- **Canonical publisher:** `run_predictions_pipeline()` upload block (`torp/R/match_model.R:792-828`), which the daily workflow and rebuild Phase 9 both hit. Change: before `saveRDS`, `stamp_model_meta()` the stripped GAM list and the xgb pipeline (guard with `requireNamespace("torpmodels", quietly = TRUE)` — torpmodels is already in torp's `Suggests`, DESCRIPTION:48; if unavailable, warn and upload unstamped rather than fail the daily run). After both uploads, call `torpmodels::update_models_manifest(c("match_gams.rds","match_xgb_pipeline.rds"), ...)` under the same guard.
- **`train_match_models.R` becomes evaluation-only:** delete the production save/upload block (`:456-474`). Guard its remaining config for external injection: `if (!exists("TEST_SEASONS", inherits = FALSE)) TEST_SEASONS <- 2025:2026` (F5's exists-guard, still useful for `eval_squiggle_rank.R`). `UPLOAD_TO_GITHUB` disappears entirely.
- **Rebuild Phase 4d is deleted** (`rebuild_everything.R:582-604`): it was dead-on-arrival (F5) and redundant — Phase 9's `run_predictions_pipeline()` already trains and uploads match GAMs.

### 1.11 Live exports — one script per artifact, stamped envelopes

| Artifact | Canonical script (kept) | Change |
|----------|------------------------|--------|
| `ep_model_live_v2.json` (+`.rds`) | `01-ep-model/train_ep_model_live_v2.R` | Move save/export (`:210-233`) **before** the brittle diagnostics (`:139-208`); wrap diagnostics in `tryCatch` (F6). Extend the JSON export list (`:222-229`) with `trained_on`, `exported_at`, `torp_sha`, `script` (v4 envelope is the template). |
| `wp-model-chain.json` | `05-live-wp-model/train_live_wp_chain_v4.R` | Envelope (`:228-239`) already has `trained_on`/`exported_at`; add `torp_sha`, `script`. |
| `live-wp-lookup.json` | `05-live-wp-model/train_live_wp_model.R` | Fix output dir at `:149` to the sibling convention `file.path(getwd(), "inst", "models", "core")` (F18); add envelope meta. |
| `xg_lookup.json` etc. | `torp/scripts/live-model-export.R` | No change (already single-source). |

Superseded live scripts (v1 EP live, chain v1–v3, live_wp_xgb) are **deleted** (§1.2 list).

---

## 2. Migration steps (ordered; repo never has two live sources of truth)

Execute in order. Each step is one commit per named repo, on `dev`, tests passing before moving on. "Closes" = the finding is fully dead after that step.

**Step 1 — torp: feature/constraint single source** *(groundwork for F1; no behavior change)*
Add `EPV_MODEL_FEATURES`, `WP_MODEL_FEATURES`, `WP_MONOTONE_INCREASING`, `wp_monotone_constraints()` to `torp/R/clean_features.R` exactly as §1.4; rewrite the two selectors to use the constants; add `tests/testthat/test-model-constraints.R` (§4.1). Run `devtools::document()` (internal-only — verify NAMESPACE diff is empty) and `devtools::test()`. Selector outputs must be byte-identical (the snapshot test pins this).

**Step 2 — torpmodels package: provenance + publish plumbing** *(groundwork for C2, F3, F4)*
Add `R/model_meta.R` + `R/publish.R` per §1.8–1.9; add `digest` to Imports; extend `load_torp_model()` with provenance print/warn; add tests §4.2–4.3; `devtools::document()` + reconcile `_pkgdown.yml` for the new exports (`publish_model_group`, `check_manifest_sync`, `model_meta` — keep the rest `@keywords internal`); `devtools::check()`.

**Step 3 — torpmodels data-raw: the canonical trainer, old scripts deleted** *(closes F1 [script side], F3, F4, C2 [go-forward]; single commit)*
Create `data-raw/lib/train_lib.R` + `data-raw/train_models.R` per §1.6–1.7. **In the same commit** delete: `train_ep_model.R`, `train_ep_model_run.R`, `train_wp_model.R`, `train_wp_model_run.R`, `train_wp_model_cv_ep.R`, `train_shot_model.R`, `train_shot_model_run.R`. Refactor `validate_cv_ep_wp.R` to source the lib for its data/fold/prediction plumbing. Update `torpmodels/CLAUDE.md` "Model Training Scripts" section and `ARCHITECTURE.md:14-15, 132-134, 154-160, 169` in this commit too, so at no point do the docs point at deleted files.
Smoke-verify without a full run: `Rscript -e 'source("data-raw/lib/train_lib.R"); print(default_training_seasons()); print(wp_params()$monotone_constraints)'` (via the PowerShell wrapper per verse CLAUDE.md) — the constraint string must equal the 18-entry canonical string.

**Step 4 — torpmodels: match harness goes eval-only** *(closes F5 [torpmodels side] + HOLDOUT_SEASON doc fix)*
In `train_match_models.R`: delete `:456-474` (prod save/upload); change `:13-14` to `exists(..., inherits = FALSE)` guards for `TEST_SEASONS`; delete `UPLOAD_TO_GITHUB`. Fix `torpmodels/CLAUDE.md:32` (drop "Set `HOLDOUT_SEASON`…", describe `TEST_SEASONS`). Verify `eval_squiggle_rank.R` still finds every object it consumes.

**Step 5 — torp: rebuild delegates; honest start_from** *(closes F1 [rebuild side], F2, F5 [rebuild side]; also retires F17's 4a→4b global-env coupling)*
In `data-raw/rebuild_everything.R`:
- `:79` `start_from <- 1L`; prompt at `:88` stays `"[1]"` (now honest). Header comment (`:9-11, 21-31`) updated: plain invocation = full nuclear rebuild; the printed phase banner (`:189-193`) is retained as the confirmation.
- Replace Phase 4a–4c bodies (`:366-580`) with: locate `torpmodels_root` (already at `:51-57`; now **abort** if NULL when `run_models` — training without the lib is no longer possible), `source(file.path(torpmodels_root, "data-raw", "lib", "train_lib.R"))`, then `safe_run("core_model_training", train_core_models(c("ep","wp","shot"), seasons = training_seasons, upload = !skip_model_upload, output_dir = model_output_dir))`. Keep `clear_model_cache()` (`:607`).
- Delete Phase 4d (`:582-604`) with a comment: match GAMs train+publish in Phase 9 via `run_predictions_pipeline()`.
- Note the accepted cost: rebuild's WP phase now runs CV-EP (5 extra fold-EP trainings) instead of reusing the in-memory EP — slower, correct, and identical to the manual path.
Steps 3 and 5 land back-to-back (torpmodels first). Between them, do not run `rebuild_everything.R --models-only`.

**Step 6 — torp: match-GAM publish gains provenance** *(closes C2/M7-P16 for match artifacts)*
In `torp/R/match_model.R:792-828`: stamp meta on `.strip_gam_models(gam_result$models)` and `xgb_result$models` before `saveRDS`; call `torpmodels::update_models_manifest()` after both uploads; all under `requireNamespace("torpmodels", quietly = TRUE)` with a warning fallback (daily CI must not break if torpmodels is somehow absent).

**Step 7 — torpmodels: live-export consolidation** *(closes F6, F18; completes "one script per artifact")*
Delete the 5 superseded live scripts (§1.2); apply the three kept-script changes (§1.11); update `torpmodels/CLAUDE.md` live-export bullets (drop "superseded by…" lines — the files are gone).

**Step 8 — docs sweep** *(closes remaining doc fixes)*
- `torp/CLAUDE.md` data-raw table: `02-models/` line → "match-prediction builders (`build_match_predictions*.R`, `push_predictions_to_r2.R`) — EP/WP/shot training lives in `torpmodels/data-raw/train_models.R`". Update the "Live Model Exports" section's script names.
- `torpverse/ARCHITECTURE.md` + `torpverse/CLAUDE.md`: point "Model Training Order" text at `train_models.R`.
- `torpmodels/ARCHITECTURE.md`: add a short "Provenance & manifest" section (meta attribute, `models_manifest.json`, `check_manifest_sync()`).
- `torp/FABLE-REVIEW.md` §9 / FABLE-METHODOLOGY: do **not** edit (review documents are historical records).

**Step 9 — retrain and republish (execution, not code)** *(actually replaces the mis-constrained production wp_model.rds — the sharpest live defect per the adversarial verification)*
`powershell.exe -Command 'Rscript "data-raw/train_models.R" ep wp shot'` from torpmodels root. Then verify: `load_torp_model("wp", force_download = TRUE)` prints meta with 18-entry constraints, eta 0.025, `ep_source = "cv"`; `check_manifest_sync()` is clean. This step is deliberately last: everything before it is inert until a model is actually shipped through the new path.

---

## 3. Exact files to change / delete (current-state line refs)

### torp (repo: `C:\dev\torpverse\torp`)

| File | Lines (today) | Action |
|------|---------------|--------|
| `R/clean_features.R` | `:230-245` (`select_epv_model_vars`), `:252-261` (`select_wp_model_vars`) | Rewrite over constants; add `EPV_MODEL_FEATURES` / `WP_MODEL_FEATURES` / `WP_MONOTONE_INCREASING` / `wp_monotone_constraints()` (§1.4) |
| `tests/testthat/test-model-constraints.R` | — | NEW (§4.1) |
| `data-raw/rebuild_everything.R` | `:79` (default 6), `:88` (prompt), `:9-31` (header), `:359-364` (model_output_dir — keep), `:366-429` (4a), `:431-515` (4b — the F1 site: `:470` constraint, `:468` eta), `:517-580` (4c), `:582-604` (4d), `:111` (`skip_model_upload` — keep, feeds `upload=`) | Phase 4a–4c → `train_core_models()` delegation; 4d deleted; `start_from <- 1L` (§2 Step 5) |
| `R/match_model.R` | `:792-828` (match_gams/xgb upload block) | Add meta stamp + manifest update (§2 Step 6) |
| `CLAUDE.md` | data-raw table ("02-models/ # EP/WP/xG/shot training"), Live Model Exports section | Doc fix (§2 Step 8) |

### torpmodels (repo: `C:\dev\torpverse\torpmodels`)

| File | Lines (today) | Action |
|------|---------------|--------|
| `R/model_meta.R`, `R/publish.R` | — | NEW (§1.8, §1.9) |
| `R/load_model.R` | `:122-155` (`load_torp_model`) | Print provenance / warn when absent |
| `DESCRIPTION`, `NAMESPACE`, `_pkgdown.yml` | — | `digest` to Imports; document + reconcile pkgdown for new exports |
| `data-raw/train_models.R`, `data-raw/lib/train_lib.R` | — | NEW (§1.6, §1.7) |
| `data-raw/01-ep-model/train_ep_model.R` | whole file (window `:30` = `TRUE,TRUE`; upload `:108-116`) | **DELETE** |
| `data-raw/01-ep-model/train_ep_model_run.R` | whole file (hardcoded `2021:2025` at `:19`; `load_all` at `:12`) | **DELETE** |
| `data-raw/02-wp-model/train_wp_model.R` | whole file (params `:46-58` are the canonical source — port to `wp_params()` first) | **DELETE** |
| `data-raw/02-wp-model/train_wp_model_run.R` | whole file (`2021:2025` at `:16`) | **DELETE** |
| `data-raw/02-wp-model/train_wp_model_cv_ep.R` | whole file (CV loop `:89-125`, injection `:131-137` — port to lib first) | **DELETE** |
| `data-raw/02-wp-model/validate_cv_ep_wp.R` | data/fold plumbing | Refactor to source `lib/train_lib.R` |
| `data-raw/03-shot-model/train_shot_model.R` | whole file (F3 gap `:94-102`; body `:16-76` ports to `fit_shot()`) | **DELETE** |
| `data-raw/03-shot-model/train_shot_model_run.R` | whole file | **DELETE** |
| `data-raw/04-match-model/train_match_models.R` | `:13-14` (config), `:456-474` (prod save/upload) | exists-guards; delete upload block |
| `data-raw/01-ep-model/train_ep_model_live.R` | whole file | **DELETE** (superseded by v2) |
| `data-raw/01-ep-model/train_ep_model_live_v2.R` | `:139-208` (diagnostics), `:210-233` (save/export) | Reorder save-first (F6); envelope meta |
| `data-raw/05-live-wp-model/train_live_wp_chain.R`, `_v2.R`, `_v3.R`, `train_live_wp_xgb.R` | whole files | **DELETE** (superseded by v4) |
| `data-raw/05-live-wp-model/train_live_wp_chain_v4.R` | `:228-239` (envelope) | Add `torp_sha`, `script` |
| `data-raw/05-live-wp-model/train_live_wp_model.R` | `:149` (output dir) | F18 path fix; envelope meta |
| `CLAUDE.md` | training-scripts + wrapper-convention + live-export sections; `:32` (`HOLDOUT_SEASON`) | Rewrite around `train_models.R` |
| `ARCHITECTURE.md` | `:14-15, 132-134, 154-160, 169` | Rewrite training-script inventory; add provenance section |

**Do not touch:** `data-raw/debug/**` (documented scratch), `convert_rda_to_rds.R` (utility), `torp/data-raw/06-stat-ratings/**` and `torp/data-raw/02-models/**` (out of scope, see §5), `.github/workflows/**` in all three repos (no training workflow exists or is added).

---

## 4. Regression tests

### 4.1 Constraints-alignment test — `torp/tests/testthat/test-model-constraints.R` *(the F1 regression; must exist before any trainer uses the derivation)*

```r
test_that("WP monotone constraint string aligns with WP_MODEL_FEATURES", {
  vals <- as.integer(strsplit(gsub("[()]", "", wp_monotone_constraints()), ",")[[1]])
  expect_length(vals, length(WP_MODEL_FEATURES))            # 15-vs-18 can never recur
  expect_true(all(vals %in% 0:1))
  expect_identical(WP_MODEL_FEATURES[vals == 1L], 
                   WP_MODEL_FEATURES[WP_MODEL_FEATURES %in% WP_MONOTONE_INCREASING])
  expect_setequal(WP_MODEL_FEATURES[vals == 1L], WP_MONOTONE_INCREASING)
})

test_that("constraint string matches the canonical literal (conscious-change pin)", {
  expect_identical(wp_monotone_constraints(),
                   "(0,0,0,0,1,1,1,0,1,1,0,0,0,0,0,0,0,0)")
  # Changing WP features/monotonicity is allowed — but you must update this
  # literal in the same commit, i.e. consciously.
})

test_that("selectors return the pinned feature sets in pinned order", {
  # snapshot pins: adding/reordering a feature forces this file to change too
  expect_identical(WP_MODEL_FEATURES, c("est_match_elapsed", ..., "phase_of_play_set_shot"))
  expect_identical(EPV_MODEL_FEATURES, c("goal_x", ..., "est_match_remaining"))
  df <- as.data.frame(setNames(as.list(rep(1, length(WP_MODEL_FEATURES) + 2)),
                               c(rev(WP_MODEL_FEATURES), "junk1", "junk2")))  # scrambled input
  expect_identical(names(select_wp_model_vars(df)), WP_MODEL_FEATURES)
})

test_that("model.matrix preserves selector order (the serving contract)", {
  df <- as.data.frame(setNames(as.list(rep(1, length(WP_MODEL_FEATURES))), WP_MODEL_FEATURES))
  X <- stats::model.matrix(~ . + 0, data = select_wp_model_vars(df), na.action = na.pass)
  expect_identical(colnames(X), WP_MODEL_FEATURES)
})
```

### 4.2 Artifact-metadata test — `torpmodels/tests/testthat/test-model_meta.R`

```r
test_that("meta stamp survives an RDS round-trip on an xgboost booster", {
  skip_if_not_installed("xgboost")
  X <- matrix(rnorm(200), ncol = 2, dimnames = list(NULL, c("a", "b")))
  m <- xgboost::xgb.train(params = list(objective = "binary:logistic"),
                          data = xgboost::xgb.DMatrix(X, label = rbinom(100, 1, .5)),
                          nrounds = 2, verbose = 0)
  meta <- build_model_meta("wp", 2021:2025, list(eta = 0.025), c("a", "b"), cv_metric = 0.45)
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(stamp_model_meta(m, meta), path)
  got <- model_meta(readRDS(path))
  expect_identical(got$model, "wp"); expect_identical(got$seasons, "2021-2025")
  for (f in c("trained_at", "script", "params", "feature_names",
              "torp_sha", "r_version", "schema_version")) expect_true(f %in% names(got))
})

test_that("build_model_meta rejects missing required inputs", {
  expect_error(build_model_meta(seasons = 2021:2025), "model")   # etc.
})

test_that("load_torp_model warns on meta-less artifact and prints provenance on stamped one", {
  # seed a local cache dir via options(torpmodels.cache_dir = tempdir-fixture)
  # with one stamped and one unstamped RDS; expect cli warning / provenance message
})
```

### 4.3 Publish-group tests — `torpmodels/tests/testthat/test-publish.R`

```r
test_that("publish_model_group aborts when any group member is missing", {  # F3 regression
  dir <- withr::local_tempdir(); saveRDS(1, file.path(dir, "shot_ocat_mdl.rds"))
  expect_error(publish_model_group("shot", dir), "shot_player_df")
  # no upload attempted: mock piggyback::pb_upload and assert 0 calls
})

test_that("update_models_manifest treats 404 as fresh and aborts on transient errors", {
  # mock pb_download: (a) throw "404 Not Found"  -> fresh manifest built
  #                   (b) throw "timeout"        -> expect_error (ledger never clobbered)
})

test_that("manifest write moves prior entry to history and records sha256", { ... })
```

### 4.4 Trainer-spec test — lib-level (in `torpmodels/tests/testthat/test-train_lib.R`)

`train_lib.R` is side-effect-free at source time, so tests can `source()` it:

```r
lib <- file.path(testthat::test_path("..", ".."), "data-raw", "lib", "train_lib.R")
source(lib, local = env <- new.env())

test_that("wp_params derives constraints from torp and validate_wp_spec enforces width", {
  skip_if_not_installed("torp")   # skip_if(!requireNamespace) on CI without torp
  p <- env$wp_params()
  expect_identical(p$eta, 0.025)
  X_bad <- matrix(0, 1, 15); X_ok <- matrix(0, 1, 18,
      dimnames = list(NULL, torp:::WP_MODEL_FEATURES))
  expect_error(env$validate_wp_spec(X_bad, p))               # the exact F1 shape
  expect_silent(env$validate_wp_spec(X_ok, p))
})

test_that("default_training_seasons excludes the in-progress season", {   # F4
  skip_if_not_installed("torp")
  expect_identical(max(env$default_training_seasons()), torp::get_afl_season() - 1L)
  expect_identical(min(env$default_training_seasons()), 2021L)
})

test_that("insample EP source forces upload = FALSE", {
  # call train_core_models with mocked fit functions; assert publish never invoked
})
```

### 4.5 Post-retrain acceptance check (Step 9, manual, scriptable)

After the first canonical publish: load `wp_model.rds` fresh; parse `xgboost::xgb.config(model)` and assert `monotone_constraints` has 18 entries and `eta == 0.025`; assert `model_meta(model)$ep_source == "cv"`; run `check_manifest_sync()` → clean. (This is the check that would have caught June's overwrite the day it happened.)

---

## 5. Non-goals

1. **No methodology changes.** Temporal holdouts, calibration release-gates, recalibration, WPA-in-TORP revisit, hyperparameter re-search (FABLE-METHODOLOGY E1–E7, H1–H4) are separate work. This plan only fixes *plumbing* — same models, trained through one door. (Exception: WP switches to CV-EP features, which the adversarial verification measured at ~0.01% logloss — a provenance decision, not a modelling one.)
2. **No training CI/workflow.** Training stays manual (`train_models.R`) / rebuild-driven; the only automated trainer remains the existing daily match-GAM path. No new GHA jobs, services, or schedulers.
3. **Stat models untouched.** The 58 per-stat GAMs (`torp/data-raw/06-stat-ratings/`, `stat-models` tag) and `xgb_win_model.rds` (legacy, never retrained) keep their current pipelines. Extending meta/manifest to them is a follow-up.
4. **No Worker/serving changes.** `inthegame-blog` worker parity harness, `exp_pts` train/serve skew (METHODOLOGY C3, M1–M3), and R2 JSON upload mechanics are out of scope.
5. **No loader-hard-failures on old artifacts.** Meta-less models load with a warning forever; the manifest is additive.
6. **No renaming of release assets.** `ep_model.rds` / `wp_model.rds` / `shot_ocat_mdl.rds` / `shot_player_df.rds` / `match_gams.rds` keep their names — every downstream consumer (`normalize_model_name`, `torpmodels/R/load_model.R:336-354`) is untouched.
7. **Other FABLE-REVIEW findings** (P1–P20, H1–H8, F7–F20 except F6/F16/F18 where they intersect files being rewritten) are not addressed here.
