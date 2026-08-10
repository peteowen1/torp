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
| **Match model** | `match_model.R`, `match_train.R`, `match_data_prep.R`, `team_elo.R`, `match_calibration.R` | 5-GAM + XGBoost sequential match prediction, 50/50 Input Blend (`run_predictions_pipeline()`). Features: player-rating diffs (EPR/PSR/TORP) + `elo_diff` (sequential team-Elo, `team_elo.R` — added 2026-07, FABLE-MATCH-MAE-PLAN.md). Final margin passes through a post-hoc recalibration sidecar (`match_calibration.R`, mirrors `wp_calibration`) before serving; identity fallback when the sidecar is absent. Rolling week-by-week OOS eval (not a fixed `HOLDOUT_SEASON`) lives in `torpmodels/data-raw/04-match-model/train_match_models.R` / `experiments/rolling_lib.R`. |
| **Simulation** | `simulate.R`, `simulate_match.R`, `season_sim.R`, `finals_sim.R`, `ladder.R` | Monte Carlo ladder and finals |
| **Opponent adj** | `opponent_adjustment.R`, `epv_opponent_adjustment.R` | EPV opponent strength adjustment in daily pipeline |
| **Validation** | `data_validation.R`, `model_validation.R`, `injuries_validation.R` | Pre-release data integrity checks |
| **Caching** | `cache.R`, `disk_cache.R` | In-memory + disk caches for loaded data and models |
| **Constants** | `constants_afl.R`, `constants_data.R`, `constants_match.R`, `constants_ratings.R`, `constants_sim.R` | All tunables live here — no magic numbers in functions |
| **Plotting** | `plot_*.R` | ggplot2 visualizations (game, player, team, shots, simulation) |
| **Profiles** | `player_profile.R`, `team_profile.R`, `player_skills_profile.R` | S3 print methods for `torp_*` objects |
| **Format** | `format_blog.R` | Blog parquet shapes consumed by torpdata `build_blog_data.R` |
| **Logging** | `logging.R` | Internal cli wrappers |

## Key Constants

All rating-blend weights and decay parameters (`TORP_EPR_WEIGHT`, `EPR_DECAY_RECV`, `EPR_DECAY_DISP`, `EPR_DECAY_SPOIL`, `EPR_DECAY_HITOUT`, `EPR_PRIOR_GAMES_RECV`, `EPV_WEIGHT_DECAY_DAYS`, `TOTAL_PRED_TOG`) live in `R/constants_ratings.R` — see the source for current values.

### EPV v3 (built 2026-08-03, NOT shipped)

`EPV_ENGINE` selects the engine and defaults to `"v2"`, so published ratings are
unchanged — proven, not asserted: `data-raw/04-analysis/epv3_verify_v2_unchanged.R`
compares all 73 columns across 56,576 player-games.

**Before touching anything EPV- or contest-related, read
[`../docs/reference/EPV-V3-CHANNELS.md`](../docs/reference/EPV-V3-CHANNELS.md)**
(formula by formula, what is in each channel) **and
[`../docs/reference/EPV-VALUE-ANATOMY.md`](../docs/reference/EPV-VALUE-ANATOMY.md)**
(where the value actually comes from, measured over 2.05M PBP rows).

**Read the anatomy doc before proposing any credit weight change.** Two live
constants were refuted by it in one session, and both had converging evidence
behind them beforehand:

- `EPV_RUCK_SWING_SCALE`'s 3.14× justification is **~93% centre-bounce reset
  artifact** — `exp_pts` is exactly 0.0000 on every `Centre Bounce` row.
- `EPV_RECV_NEG_MULT = 0` deletes the **intercept** branch, which is the
  highest-value receiving act in the game (+0.625/event against +0.079).

The general shape: **when a change improves a summary statistic, ask what events
it removed before banking it.** Both of those looked like noise reduction and
were signal deletion. And note game-to-game reliability and year-over-year
repeatability can move in *opposite* directions — quote which one you mean; the
second is the one that separates ability from noise.

- **The v3 channel names are ALIASES and they lie.** `epv_spoil` /
  `epr_spoil` hold *aerial contest* value and contain no spoil weight;
  `epv_hitout` / `epr_hitout` hold stoppage value. Same for
  `EPR_DECAY_SPOIL`, `EPR_PRIOR_GAMES_SPOIL`, `EPR_PRIOR_RATE_SPOIL` and their
  hitout twins. Renaming touches 18 R files and two released artifacts, so it
  waits until v3 is chosen.
- **Two different contest coefficients exist and both are valid.** At the EPV
  layer (value accrued *in* a match → that match's margin) v2 reads −0.41 and v3
  +0.27; at the EPR layer (historical rating → margin) v2 reads +3.2 to +4.05 and
  v3 −0.02. They answer different questions. Quote which layer you mean.
- **`player_game_data` is lineup-correct by construction** (22.79 players per
  team-match) and `epv_*_adj` is already TOG-scaled. Summing it per (match, team)
  needs no lineup filter and no `POSITION_AVG_TOG` weighting — adding either
  double-counts. Summing EPR *ratings* per team-round is the opposite: those exist
  for every listed player, so that path must go through
  `.build_team_ratings_df()`.

Design, gates and the dead ends not to re-open:
[`../docs/plans/EPV-V3-CHAIN-NATIVE.md`](../docs/plans/EPV-V3-CHAIN-NATIVE.md).

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

## Positions — read the reference first

**Anything that groups, centres or adjusts by position: read
[`../docs/reference/POSITIONS.md`](../docs/reference/POSITIONS.md) BEFORE writing code.**

The short version, because getting this wrong has cost real MAE more than once:

- There are two real things — `lineup_position` (where he lined up this match, 21 slots) and
  the club's listing (what he's listed as, season-stable).
- **`position_group` means two different things depending on the frame.** In `torp_ratings`
  it is the season listing (7 levels, has `MIDFIELDER_FORWARD`). In `player_game_data` it is
  the per-match listing (6 levels, never has it). Same name, different source. **Count the
  levels to tell which one you're holding.**
- **`pos_group` (stat ratings / psr) is NOT "playstyle"**, despite the comment in
  `player_skills_data.R` saying so. It is the same AFL-registered position, aggregated to a
  career modal, and it is 20% NA.
- Never trust a position variable name or a comment about positions. Trace to source.

## Gotchas

- **Every new exported function MUST be added to `_pkgdown.yml`'s `reference:` index** — `pkgdown::check_pkgdown()` runs in both CI workflows and fails the build otherwise (bit twice on 2026-07-21: #111, PR #114).
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
| `daily-ratings-predictions.yml` | Repository dispatch (from torpdata), **pre-game schedule**, or manual | Compute ratings + match predictions, upload to `predictions` / `ratings-data` |
| `test-package.yml` | Push/PR | R CMD check + coverage |
| `pkgdown.yml` | Push to main | Deploy docs to GitHub Pages |

`pre-game-data-update.yml.template` is an inactive template (`.template` suffix = not run by GitHub Actions); rename to drop the suffix to enable.

**The pre-game schedule on `daily-ratings-predictions.yml` is load-bearing — do not remove it.**
Until 2026-07-28 the only automatic trigger was the torpdata `repository_dispatch`, which fires
after a **data release**, which happens only when there are **new games**. The AFL publishes team
lists *between* rounds, so the pipeline could never run in the window between team-naming and
first bounce, and every round was locked using the previous round's lineup state — none. Rounds
19, 20 and 21 of 2026 all published with `players = NA` (every player on the position prior),
costing ~0.50 MAE season-wide and 4.75 on the affected rounds. Nothing failed; the predictions
were just worse. Note GitHub only fires `schedule` from the **default branch**, so the crons are
inert on any other ref.

Two guards back it up: `.warn_missing_lineups()` reports at write time, and
`data-raw/05-validation/check_prediction_lineups.R` answers on demand whether the upcoming round
is safe, how long until first bounce, and whether a re-run would help (exit 1 = action needed).
Both check lineup **completeness**, not mere presence — `players` is a *count*, so a partially
published team sheet yields a small non-`NA` number that a presence-only check misses
(`MIN_PLAUSIBLE_LINEUP`).
