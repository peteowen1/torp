# CLAUDE.md — torp R package

Core AFL analytics package: EP/WP/xG models, TORP/EPR/PSR player ratings, match prediction, simulations, and the AFL data scraper. Loaded by inthegame-blog and the AFLW-adjacent daisychain.

See [`ARCHITECTURE.md`](ARCHITECTURE.md) for full pipeline and module details. For the verse-level overview and cross-repo workflows, see `../CLAUDE.md`.

**WSL/Bash workaround**: `arrow` segfaults under Git Bash R. Run via PowerShell wrapper:
```bash
powershell.exe -Command 'Rscript "path/to/script.R"'
```

## Code Organization

141 exports across ~65 R files. Grouped by domain:

| Domain | Key files | Purpose |
|--------|-----------|---------|
| **Data loading** | `load_data.R`, `load_utils.R`, `load_engines.R`, `local_data.R` | `load_*()` family (load_pbp, load_results, load_torp_ratings, ...) — fetch from GitHub Releases or `get_local_data_dir()` |
| **Scraping** | `afl_api.R`, `scraper.R`, `injuries_scrape.R` | In-house AFL API (replaced fitzRoy), injury scraping — see [`AFL-API-REFERENCE.md`](AFL-API-REFERENCE.md) for the full endpoint/field dictionary |
| **EP / WP / xG** | `add_variables.R`, `win_probability.R`, `wp_credit.R`, `wp_utils.R`, `xg.R` | `add_epv_vars()`, `add_wp_vars()`, `add_shot_vars()` — feature engineering and credit assignment |
| **TORP / EPR / PSR** | `player_ratings.R`, `player_skills.R`, `psr.R`, `player_credit.R`, `player_attribution.R` | Core rating composition. `TORP_EPR_WEIGHT = 0.5` blends EPV+PSV; WPA tracked separately |
| **Per-game ratings** | `player_game_ratings.R`, `player_skills_data.R`, `player_skills_profile.R` | `get_player_game_ratings()` returns EPV+WPA+PSV per game |
| **Match model** | `match_model.R`, `match_train.R`, `match_data_prep.R` | 5-GAM sequential match prediction; XGBoost holdout comparison via `HOLDOUT_SEASON` |
| **Simulation** | `simulate.R`, `simulate_match.R`, `season_sim.R`, `finals_sim.R`, `ladder.R` | Monte Carlo ladder and finals |
| **Opponent adj** | `opponent_adjustment.R`, `epv_opponent_adjustment.R` | EPV opponent strength adjustment in daily pipeline |
| **Validation** | `data_validation.R`, `model_validation.R`, `injuries_validation.R` | Pre-release data integrity checks |
| **Caching** | `cache.R`, `disk_cache.R` | In-memory + disk caches for loaded data and models |
| **Constants** | `constants_afl.R`, `constants_data.R`, `constants_match.R`, `constants_ratings.R`, `constants_sim.R` | All tunables live here — no magic numbers in functions |
| **Plotting** | `plot_*.R` | ggplot2 visualizations (game, player, team, shots, simulation) |
| **Profiles** | `player_profile.R`, `team_profile.R`, `player_skills_profile.R` | S3 print methods for `torp_*` objects |
| **Format** | `format_blog.R` | Blog parquet shapes consumed by torpdata `build_blog_data.R` |
| **Logging** | `logging.R` | Internal cli wrappers |

## Key Constants (R/constants_ratings.R)

```r
TORP_EPR_WEIGHT       # 0.5 — blend weight in torp_value = 0.5*EPV + 0.5*PSV
EPR_DECAY_RECV        # 273 days — receiving decay
EPR_DECAY_DISP        # 630 days — disposal decay
EPR_DECAY_SPOIL       # 523 days
EPR_DECAY_HITOUT      # 545 days
EPR_PRIOR_GAMES_RECV  # 3.0 — Bayesian prior
EPV_WEIGHT_DECAY_DAYS # 365 — PBP-level recency decay
TOTAL_PRED_TOG        # 324L (18 teams * 18 players) — league centering fallback
```

WPA is intentionally **not** folded into `torp_value` — surfaced as a parallel metric. (The original "WP gradient too steep in close/late" rationale was measured **false** in 2026-07: the WP family is actually *flat* there — see [`../docs/reviews/FABLE-WP-EXPERIMENTS.md`](../docs/reviews/FABLE-WP-EXPERIMENTS.md) §7. Decision 2026-07-11: exclusion stood until (a) a light recalibration layer fitted on recent-season OOS predictions ships, and (b) a temporal Q4/close slope release gate exists — the canonical model still ran slope ~1.14/1.26 on temporal holdout. Update 2026-07-12: recalibration layer shipped ([`../docs/plans/FABLE-RECAL-PLAN.md`](../docs/plans/FABLE-RECAL-PLAN.md) — `get_wp_preds()` applies the `wp_calibration` sidecar, `torpmodels::train_core_models()` gates every WP release on the calibrated temporal slope); WPA reinstatement is now pending the `../docs/plans/FABLE-RECAL-PLAN.md` §2 Step 6 bias re-measurement (the plan's own D6 cross-reference to "§5" for this protocol is stale -- Step 6 is where it actually lives, §5 is Non-goals), not a separate design decision.)

## Data Loaders

All `load_*()` functions default to fetching from GitHub Releases on `peteowen1/torpdata`. They auto-detect a local `torpdata/data/` sibling via `get_local_data_dir()` and prefer it when available (zero-network dev).

Common loaders (see `?load_data` for the full list):

| Function | Release tag | Returns |
|----------|-------------|---------|
| `load_pbp()` | `pbp-data` | Play-by-play |
| `load_chains()` | `chains-data` | Possession chains |
| `load_results()` | API (not a release)* | Match results |
| `load_fixtures()` | API (not a release)* | Upcoming fixtures |
| `load_torp_ratings()` | `ratings-data` | Player ratings |
| `load_team_ratings()` | `team_ratings-data` | Team ratings |
| `load_predictions()` | `predictions` | Match predictions |
| `load_player_game_ratings()` | `player_game_ratings-data` | Per-game EPV/WPA/PSV |
| `load_player_skills()` | `player_skills-data` | Per-stat skill ratings |
| `load_player_stats()` | API (not a release)* | Box-score stats |
| `load_weather()` | `weather-data` | Historical weather features |

\* `load_results()`, `load_fixtures()`, `load_player_stats()`, and `load_teams()` fetch live from the AFL API (via `.load_with_cache()`), not from a torpdata release — despite their roxygen historically implying otherwise.

`save_to_release()` (internal) handles uploads from `data-raw/01-data/` scripts; uses `piggyback` with a 404 retry for concurrent-upload races.

## data-raw/ Pipeline

```
01-data/        # Scraping + daily release (daily_release.R is the GHA entry point — calls run_daily_release())
02-models/      # match-prediction builders (build_match_predictions*.R, push_predictions_to_r2.R) —
                #   EP/WP/shot training lives in torpmodels/data-raw/train_models.R, not here
03-ratings/     # TORP/EPR/PSR computation
04-analysis/    # Ad-hoc analysis
05-validation/  # Cross-release sanity checks
06-stat-ratings/ # Per-stat Bayesian rating training
stat-models/    # Cached artifacts (2 .rds) + README only — the 58 per-stat GAMs are released via torpmodels' stat-models tag, not committed here
```

`rebuild_everything.R` re-runs the full data-raw pipeline end-to-end.

## Validators

Pre-release data integrity checks in `data_validation.R` and `model_validation.R`:
- Column schema enforcement (`column_schema.R`) — every public dataset declares its expected columns
- Row count / date range / unique-key guards before `save_to_release()`
- Model-output validators check prediction ranges (e.g., EP within plausible bounds, WP in [0,1])

When changing a column or release, update the schema declaration alongside the data so downstream consumers fail loud.

## Caching

Two layers:
- **In-memory** (`cache.R`): Loaded data + models kept in a package-level env. Inspect with `get_cache_info()` / `get_model_cache_info()`.
- **Disk** (`disk_cache.R`): Persistent cache at `~/.torp/cache`. Use `get_disk_cache_info()` / `get_disk_cache_size()`. Survives session restarts.

Pass `refresh = TRUE` to bypass caches and re-fetch — only `load_player_stats()`, `load_teams()`, and `load_player_details()` currently expose it; most other loaders have no cache-bypass argument.

## Live Model Exports (for inthegame-blog Worker)

Live EP/WP/xG models are trained in **torpmodels** — one script per artifact: `data-raw/01-ep-model/train_ep_model_live_v2.R` (EP), `data-raw/05-live-wp-model/train_live_wp_chain_v4.R` and `train_live_wp_model.R` (WP) — and exported as JSON. Worker tree-walk lives in `inthegame-blog/worker/src/ep-model.js`. torp itself does not export these — `torp/scripts/live-model-export.R` only handles xG lookup grid generation.

## Gotchas

- **EP must be trained before WP** — WP uses EP predictions as features. Same for live variants.
- **Arrow + Git Bash R = segfault** — wrap in PowerShell (see top of file).
- **Weather imputation** — `add_weather_to_preds()` uses median imputation for missing weather; the `total_xpoints` GAM expects this neutral fallback rather than NA.
- **Shot distance bug** — `shots.parquet` computes distance with signed x (`halfLen - x`) instead of `halfLen - |x|`, so negative-x shots show distance to the *far* goal. inthegame-blog overrides client-side; fix in torp would let the override become a no-op.
- **Team name canonicalisation** — `save_to_release()` calls `.normalise_team_values()` before write; outside that path you may see raw API names (Footscray, GWS) vs full names (Western Bulldogs, GWS Giants). Use `AFL_TEAM_ALIASES` to translate.
- **Off-season `run_daily_release()` returns `'none'`** — by design, so the GHA workflow can skip release/dispatch steps (`release_done <- result != 'none'`). Don't treat `'none'` as an error.
- **`load_*()` loaders default to the *current* season** — `load_player_stat_ratings()`, `load_player_stats()`, etc. default `seasons = get_afl_season()`, and `seasons = TRUE` means *all* seasons (`AFL_MIN_SEASON:current`). But `torp_ratings.parquet` (`ratings-data`) is **full-history** — it's upserted into the existing release each run. So any pipeline stage that blends per-round data into the full table must pass `TRUE`, or historical rows silently fall back to a current-season snapshot. This was the #88 PSR/OSR/DSR "flat across history" bug: `run_ratings_pipeline.R` fed `calculate_torp()` a current-season-only PSR frame.

## Tests

~50 test files in `tests/testthat/`. Key ones:
- `test-load_torp_data.R` — loader contracts
- `test-add-model-variables.R` — EP/WP/xG feature engineering
- `test-player-ratings.R` — TORP composition math
- `test-sim-helpers.R` — Monte Carlo simulation
- `match_model.R` (published predictions) has no dedicated test file — zero test coverage today

Run a single file with `testthat::test_file("tests/testthat/test-NAME.R")`.

## GitHub Actions (this repo)

| Workflow | Trigger | Purpose |
|----------|---------|---------|
| `daily-ratings-predictions.yml` | Repository dispatch (from torpdata) or manual | Compute ratings + match predictions, upload to `predictions` / `ratings-data` |
| `test-package.yml` | Push/PR | R CMD check + coverage |
| `pkgdown.yml` | Push to main | Deploy docs to GitHub Pages |

`pre-game-data-update.yml.template` is an inactive template (`.template` suffix = not run by GitHub Actions); rename to drop the suffix to enable.
