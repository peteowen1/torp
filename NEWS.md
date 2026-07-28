# torp (development version)

## Bug Fixes

* **EPR position centring and the match model's position features now use one
  taxonomy.** They shipped on different ones: the features collapse
  `position_group` to 6 buckets via `MATCH_LISTED_POS_MAP` (combining
  `MEDIUM_FORWARD` and `MIDFIELDER_FORWARD`), while centring keyed on the raw
  7-value column. So `med_fwd_diff` pooled two groups the ratings had already
  been centred apart, and the pooling carried whichever level difference
  centring had just removed. Both now go through
  `.collapse_listed_position()`, the single place a `position_group` becomes a
  bucket name. Ratings rebuilt after this change differ for forwards only, by
  roughly +-0.05 to +0.14 EPR per player.

* **`check_predictions_csv.R`**: `predictions_<season>.csv` -- the file
  squiggle.com.au actually reads -- is now verifiable against the parquet every
  other loader reads. A failed CSV upload only warns (deliberately: the parquet
  has already landed by then), so nothing inside torp could previously detect
  that Squiggle was serving the previous round's tips. `save_to_release()`'s
  warning now names that consequence.

* **Match predictions are no longer locked before AFL team lists exist.** Rounds 19,
  20 and 21 of 2026 were all published with `players = NA` -- no team sheet available,
  so every player fell back to the position prior and the predictions were
  squad-average rather than team-specific. Rounds 13-18 carried 23. Nothing reported
  it; a paired comparison against Squiggle's record of our submitted tips put the cost
  at roughly 3.2 MAE on rounds 19-20 (mean per-game disagreement 8.73 points against a
  correctly-fed model, versus 4.15 on rounds with lineups).

  The cause was structural rather than a transient failure. The predictions workflow's
  only automatic trigger was the `repository_dispatch` torpdata fires after a data
  release, and data releases only happen when there are new games. The AFL publishes
  team lists *between* rounds, so the pipeline could never run in the window between
  team-naming and first bounce, and every round was locked using the previous round's
  lineup state: none.

  Three changes: the predictions workflow gains its own pre-game schedule (Thu/Fri
  06:00 UTC, Sat/Sun 00:00 UTC); `.warn_missing_lineups()` reports at write time when
  a prediction is being locked without a full team sheet; and
  `data-raw/05-validation/check_prediction_lineups.R` answers on demand whether the
  upcoming round is safe, how long until first bounce, and whether a re-run would
  help. The guards check *completeness*, not mere presence -- `players` is a count, so
  a partially published sheet yields a small non-`NA` number that a presence-only
  check would miss (`MIN_PLAUSIBLE_LINEUP`).

## New Features

* **EPR is position-centred, and the match model gets listed-position splits.**
  A published EPR now reads "points above the average player in your position":
  each channel is centred on its position's TOG-weighted mean within every
  `(season, round)` cross-section, keyed on `position_group`
  (`EPR_POSITION_CENTRE`, `centre_epr_by_position()`). The match model gains the
  six listed-position differentials as features (`MATCH_LISTED_POS_DIFF_COLS`).

  **Why.** `EPV_POSITION_STANDARDISE` equalises between-position *spread* at the
  player-game level and works exactly there -- the TOG-weighted mean of
  `epv_recv_adj` is 0.000 in all 20 lineup positions -- but the correction does
  not survive to the published rating, because the TOG weighting, opponent
  adjustment, decay and global prior that follow are all position-blind. Read on
  the listed taxonomy, key defenders sat at median EPR **-2.18** against medium
  forwards' **+0.66**, which is pipeline residue rather than a value judgement.

  **This is a normalisation, not a measurement, and that distinction is
  load-bearing.** Position *levels* are unidentifiable from match margins: the
  on-field structure is rigid (every team fields exactly one full-back), and
  although listed-position counts do vary (teams field 2-9 midfielders), holding
  total EPR constant the positional mix explains nothing -- F(5, 1113) = 0.47,
  p = 0.80, every CI spanning roughly +/-3 points. Setting each position's mean
  to zero therefore *asserts* that an average key defender and an average
  midfielder contribute equally. That cannot be checked against results. It is
  preferred anyway because the status quo also embeds an assumption -- that the
  uncentred levels are right -- and those levels are an accident of the
  pipeline. A deliberate, symmetric assumption beats an accidental one.

  **Cost: measured, and neutral.** Centring alone is dMAE +0.121, 95% CI
  [-0.250, +0.485]; the position splits it enables bring that to **-0.026**, CI
  [-0.413, +0.358] (2025-26 pooled, 387 games). The splits are included because
  they make the centring free, not because they stand alone -- their own CI
  spans zero too, and they cost about 0.003 bits.

  Position *slopes* are separately identifiable (medium defenders convert EPR to
  points at 0.46 against midfielders' 1.12, the only group differing from 1
  after Bonferroni, p = 0.0005) -- that is what the splits let the model exploit.

  Ratings keep the **v2** vintage: this changes the published numbers but the
  site has no live audience today, so a vintage bump would cost more in
  cross-reference churn than it buys.


* **Locked predictions record when they were computed** (`generated_utc`). Previously
  "is this row genuinely pre-game?" could only be answered by reconstructing against
  Squiggle's submitted tips, which is how three rounds of stored-versus-submitted
  divergence became an open forensic question rather than a lookup. The check is now
  `generated_utc < utc_start_time`, and `.warn_post_hoc_predictions()` surfaces
  violations at write time. Rows published before stamping existed carry `NA` and are
  skipped rather than flagged.

* **The post-upload verify no longer aborts the daily release on a stale-but-larger
  asset listing** (torpdata#74, third iteration). The first two iterations assumed a
  lagging listing and widened the retry budget (~7s, then ~20s); neither worked --
  Daily Data Release failed 33 times between 2026-07-14 and 2026-07-27. The actual
  failures had the sign backwards from the earlier diagnosis: the listed size was
  *larger* than local (the previous, bigger asset) and byte-identical on all five
  attempts, so no amount of waiting could converge it. Each aborted release also
  skipped the downstream dispatch to torp, collapsing its game-day prediction
  refresh from ~6 runs to 1-2 and staling its submitted tips for two weeks.

  Truncation -- the failure worth aborting for -- makes the listing *smaller*, so
  that direction stays fatal. A *larger* listing is decided on the listing's own
  `updated_at`: stamped before our upload means a lagging read (retry, then warn
  and proceed); stamped at or after it means a different write replaced ours, or
  the replace failed and the old asset is still live, and stays fatal.

  **Correction:** the first version of this fix decided the larger-than case on
  size direction alone, justified by a claim that truncation was "independently
  guarded by the row-count floor against `bus_manifest.json`". That claim was
  **false** -- `prev_rows_floor` defaults to `NULL` and none of the ~80
  `save_to_release()` call sites pass it, so that check is inert in production.
  Size direction alone cannot separate a lagging listing from a failed replace
  (piggyback's delete-then-upload is not atomic) or a concurrent writer, which is
  why the decision now rests on a real staleness signal instead.

## Rating changes — NOT yet reflected in published ratings

These change how EPR and PSR are computed. Published `ratings-data` is untouched
until a full-history regeneration runs, which per decision D-DEF3 will ship as a
**new rating vintage alongside the existing one**, not an in-place overwrite.
Evidence for every item is in `../docs/plans/FABLE-DEFENDER-VALUE-PLAN.md` §7.

* **The EPV position adjustment now rescales as well as recentres**
  (`EPV_POSITION_STANDARDISE`). It previously subtracted a within-position mean
  and stopped, which corrects positional *level* but leaves positional *spread*
  alone — and the measured defect in key-defender ratings is under-dispersion,
  not under-levelling. Key-defender rating SD moves 1.40 → 1.60 and the best
  key-defender season 3.42 → 4.04, narrowing the best-forward-to-best-key-defender
  gap from 1.96× to 1.55×. Paired bootstrap on positional calibration:
  Δ mean|β−1| −0.095, 95% CI [−0.160, −0.016], P(improves) 0.987 — the first
  result in this program whose interval excludes zero.

* **`hitout` is deliberately excluded from that rescaling**
  (`EPV_STANDARDISE_CHANNELS`). Rescaling divides by a within-position SD, which
  is only meaningful for a channel every position participates in. Hitouts are
  ruck-exclusive, so outfield positions carry a near-zero hitout SD and rescaling
  amplified their deviations 4–9× (and 1.24 million-fold for `EMERG`, where the
  SD is exactly zero). Left unguarded this put a ruck named at nine different
  lineup positions into the overall top 10 at 4.06 against his true 1.12.
  Excluding the channel scores strictly better than capping the amplifier.

* **The 20-way lineup-position map is corrected** (`LINEUP_POSITION_GROUP_MAP`,
  previously inline in `player_skills_data.R`). An audit of all 18 on-field codes
  against player height, the clubs' listed positions, PBP-derived position groups
  and each code's on-field statistical profile found three assignments
  contradicted by every source: `CHF` was MEDIUM_FORWARD (a centre half forward
  averages 190.8cm and is listed KEY_FORWARD; PBP disagreed 67% of the time), and
  `FPL`/`FPR` were KEY_FORWARD (the pockets average 187cm, are listed
  MEDIUM_FORWARD, and PBP disagreed 72% and 69% — the highest rates in the
  table). `CHB` is also now grouped with `FB`; that one is a football judgement
  on genuinely ambiguous evidence rather than a correction, and is flagged as
  the taxonomy's softest call.

* **`calculate_psr()` now prefers a weekly position group** (`lineup_pos_group`)
  over `pos_group`. What drives positional calibration is temporal resolution,
  not granularity: `pos_group` is effectively season-constant (it varies in 0.6%
  of player-seasons) while the team sheet varies for 77.8%, and moving to a
  weekly 6-way role improved mean|β−1| by 0.138 (P 0.956) where going finer than
  6-way added nothing (P 0.417). **This is inert until the `06-stat-ratings`
  pipeline joins `lineup_position` into the stat-ratings frame** — that frame
  carries no lineup column today, which is exactly why production has silently
  centred on the season-constant label for years.

# torp 1.3.9 (2026-07-28)

## Match model

* **The team-strength feature is now an xScore power rating (`xelo_diff`),
  replacing the win-based team Elo (`elo_diff`)** — new `R/xscore_rating.R`.
  The old feature updated on a binary win/loss with a margin multiplier; the new
  one lives in points space and updates on the error of an *expected*-score
  margin. AFL conversion variance is large enough that a side can dominate
  territory and shots and still lose, so updating on expected score strips that
  noise out — and it is signal no competitor can construct, since xScore is
  torp's own. Standalone on an identical 695-match set (2023–2026): MAE
  27.15 → 26.38, cor 0.524 → 0.559, and the new rating renders the Elo redundant
  (β(elo) 0.09, p 9e-10) rather than the reverse. In-model, rolling week-by-week
  OOS on 2025–2026 (n=387), swapping only this feature improved **all six**
  headline metrics: MAE 25.622 → 25.510, RMSE 32.646 → 32.525, Brier 0.17891 →
  0.17696, bits 0.23135 → 0.23701, slope 0.959 → 0.982, cor 0.610 → 0.613.
  Adopted under the EXPLORE tier of the new signal gate (decision D-M1) rather
  than as a bootstrap-confirmed win: the MAE 95% CI is [−0.442, +0.219] and spans
  zero, because the effect is smaller than the measured XGBoost retraining noise
  floor (~0.157) on the largest window available. Evidence:
  `../docs/plans/FABLE-MATCH-FEATURES-PLAN.md` §6.1/§6.4/§6.6.

  `elo_diff` is still computed and published for comparison — it is simply no
  longer consumed by the GAM or XGBoost feature sets. **`match_gams.rds` and
  `match_xgb_pipeline.rds` must be retrained and republished together with this
  change**: models trained on `elo_diff` cannot score a frame carrying
  `xelo_diff`.

  `build_matchup_table()` was switched over in the same commit — it hand-builds a
  feature frame that is fed straight to the trained models, so leaving it on the
  old feature would have silently produced an unscoreable frame.

# torp 1.3.8 (2026-07-25)

## Bug Fixes

* **`assess_model_calibration()`'s slope/intercept now use the GLM logit convention**, matching `evaluate_model_comprehensive()` (`model_validation.R`, unified 2026-07-22) instead of the old decile-binned OLS fit. The two had drifted apart — this function was left on the old convention when the other was unified — so a caller comparing `calibration_slope` output from both functions was comparing two related-but-distinct quantities without knowing it. `calibration_data` (the per-bin breakdown) is retained unchanged for the Hosmer-Lemeshow test and reliability/resolution/uncertainty decomposition, which are legitimately bin-based statistics uninvolved in this convention.

## Chores

* **`versebus.R` sync check now actually runs in CI.** `test-versebus-sync.R` (added 2026-07-22) compares the vendored `R/versebus.R` against torpmodels' copy, but is local-dev-only by design — it skips silently when no sibling `../torpmodels` checkout is present, which was true on every CI run since it shipped. New `versebus-sync` job in `test-package.yml` checks out both repos as siblings so the guard actually executes (confirmed clean on the real dependency-drift check: 25/25 pass); the job also installs torp itself (`R CMD INSTALL`), not just its dependencies, since every test file's `setup-test-env.R` requires `library(torp)` to succeed.

# torp 1.3.7 (2026-07-25)

## Bug Fixes

* **`save_to_release()` post-upload verify retry budget widened (torpdata#74 follow-up)** — the 1.3.6 fix retried the post-upload listing check through `.vb_retry()`'s default budget (3 attempts, 2s+5s delays, ~7s total), but the failure kept recurring on live game days (2026-07-23, 2026-07-24): the listed size was consistently *smaller* than the just-uploaded local size, consistent with GitHub's listing lag outlasting 7s during high-frequency upload bursts, not real corruption. Widened to 5 attempts with 2+3+5+10s delays (~20s total) for this specific verify call.

# torp 1.3.6 (2026-07-23)

## Bug Fixes

* **`save_to_release()` post-upload verify aborted on GitHub release-asset listing lag (torpdata#74)** — the daily data release failed two days running (2026-07-21, 2026-07-22) when a fresh listing call right after upload reported a slightly different asset size than what was just written. Both deltas were small and non-data-shaped, consistent with GitHub's release-asset listing lagging the upload rather than real corruption. The verify now retries the listing + compare through `.vb_retry()` (same backoff already used for download-side flakes, #66/#68) before treating a mismatch as a real integrity failure.

# torp 1.3.4 (2026-05-09)

## Bug Fixes

* **PSR forward-leakage in match prediction features** — `.build_team_ratings_df()` previously joined PSR via `slice_tail(n = 1)` per `player_id`, applying each player's *latest available* PSR to **every** historical lineup row. This leaked future skill information into past games used for GAM/XGB training. Replaced with `dplyr::join_by(closest(.lineup_key >= psr_key))`, so each lineup row gets the most recent PSR with `(season, round) <= (lineup season, round_number)`. PSR(s, r) is itself computed using `match_date_rating < first_utc_start_time(round_r)`, so it's snapshot-as-of-start-of-round-r and safe to use when predicting round r. Production prediction behaviour is preserved (predicting round R picks PSR(s, R) when present, falling back to PSR(s, R-1) otherwise — identical to the prior `slice_tail(n=1)` latest-PSR behaviour for unscheduled rounds); only historical training rows shift. Discovered while comparing rolling-OOS evaluation metrics against actual Squiggle leaderboard rank.

  Also adds defensive guards on the join: aborts on NA `season`/`round_number` instead of silently falling back to `PSR_PRIOR_RATE`, dedups duplicate `(player_id, season, round)` PSR rows with a warning (otherwise `closest()` with default `multiple = "all"` would duplicate lineup rows and silently inflate team aggregates), and emits coverage telemetry mirroring the existing EPR diagnostic block (`cli_inform` on missing-PSR rate, `cli_warn` >25%, `cli_abort` >50%).

* **`torp_replace_teams()` scrambled factor inputs** — `AFL_TEAM_ALIASES[factor_var]` indexes by the factor's underlying integer level codes, not by label, so factor inputs got silently mapped to whichever names happened to occupy the early alias slots. Function now coerces `as.character(team)` before lookup. Affected any caller passing a factor — most commonly downstream consumers of `load_predictions()`, which was the only loader returning factor team columns.

* **`.normalise_team_values()` silently skipped factor columns** — the `is.character(vals)` guard prevented factor columns from being normalised at all (they passed through un-mapped). Now also handles `is.factor(vals)` and emits character output, aligning `load_predictions()` schema with every other loader.

* **Predictions parquet stored factor `home_team` / `away_team`** — `team_name.x = as.factor(...)` in `.build_team_mdl_df()` (needed for GAM categorical predictors) propagated through `.format_match_preds()`'s `home_team = team_name.x` assignment, so factor types round-tripped through every parquet write/read cycle. Added explicit `as.character()` coercion in the formatter so future writes store character columns.

## Tests

* New `test-match-data-prep.R` — eight regression tests pinning the PSR rolling-join semantics: round 0 picks PSR(s, 0) when present (same-round non-strict match); round N lineups pick PSR(s, N) and not the global tail; future-round prediction picks the latest available prior PSR; missing PSR for a player falls back to `PSR_PRIOR_RATE`; duplicate `(player, season, round)` rows are deduped with a warning; NA in `season`/`round_number` aborts; works without `osr`/`dsr` columns; works when `psr_df = NULL`.

* `test-team-names.R` — added factor-handling regression tests for `torp_replace_teams()`, `torp_team_abbr()`, `torp_team_full()`, and `.normalise_team_values()` covering the integer-level-code coercion bug.

## Internal

* `R/globals.R` — declared `.lineup_key`, `psr_key`, and the `closest` `join_by()` token to silence the new R CMD check globals NOTE.

# torp 1.3.3 (2026-04-26)

## New Features

* **Injury listing-accuracy validation** — `test_played_rate()`, `tbc_played_rate()`, `injury_return_accuracy()`, and `tbc_return_survival()` quantify how often listed-as-injured players actually play, how accurate the estimated return rounds are, and how long TBC listings persist. Calibration after R10+ once there's enough history.

* **Stale preseason injury filter** — `get_all_injuries()` now drops preseason CSV entries for players who have already played a senior game this season, preventing phantom "TBC" listings from lingering after a player has clearly returned. Includes team-name dedup so weekly + preseason sources merge on a normalised key.

* **Historical injury snapshot log** — preseason and weekly scrapes are now appended to a per-season history file, enabling backwards-looking accuracy validation.

* **Team-quality residual SE widening in season simulation** — `simulate_afl_season()` now multiplies the xscore-diff GAM random-effect SE by `SIM_RESIDUAL_SE_MULT` (default 1.5) before per-sim sampling. The raw GAM SE understates true team uncertainty because random effects are shrunk toward the league mean; the multiplier produces wider, better-calibrated Premier and Top-N bands.

* **`SIM_INJURY_SD_KNOWN` raised from 2 → 3** to match `SIM_INJURY_SD`. Scraped injury lists only capture officially-listed absences — form slumps, minor niggles, and game-day late-outs still contribute meaningful week-to-week jitter, so the "we already excluded the known injured" discount was over-tight.

* **New simulation summary bands** — `summarise_simulations()` adds `top_6_pct` and `top_10_pct` (matching the 2026 finals structure: top-6 home-finals, top-10 finals qualification) plus `w10` / `w90` — 10th/90th percentile of season wins per team — for a cheap summary of the full ladder distribution.

* **Parallel pipeline hardened** — `closeAllConnections()` runs unconditionally before PSOCK workers spawn (the prior selective cleanup missed leaks from arrow/piggyback that surfaced intermittently on Windows as `serialize(...)` errors during `clusterExport`). The full parallel pipeline now sits inside one tryCatch so any worker-setup failure cleanly falls through to the sequential branch instead of leaving an orphaned cluster.

* **Blog data formatter** — `format_predictions_blog()` produces a canonical schema for predictions consumed by inthegame-blog (with new `PREDICTIONS_BLOG_COLS` exported as the column-order source of truth); `xg_to_blog_lookup()` reshapes `get_xg()` / `load_xg()` output for the formatter. Both replace duplicate schema definitions that previously lived in two producer paths and drifted apart.

## Bug Fixes

* **`fit_win_probability()` is now reproducible** — quarter-break training data was synthesised via `rnorm()` with no seed, so each retraining produced different coefficients. Now seeded via `withr::local_seed()` (new `seed` parameter, defaults to `20250101L`), so the JSON exported for browser inference is stable across runs. The caller's RNG stream is unaffected.

* **`EPR_PRIOR_GAMES_HITOUT` rounded from 3.0013 → 3.0** — the trailing precision was an optimizer artifact; the other four EPR priors are exactly 3.0000. Companion test now `expect_identical` rather than `tolerance = 0.01` so future drift won't pass.

* **`.normalise_results_schema()` no longer silently empties results** — added a guard that detects and rejects malformed input that previously slipped through as zero rows.

## Internal

* **`constants.R` (1054 LOC) split into 5 themed files** — `constants_afl.R` (league/team/colours/API), `constants_ratings.R` (EPR/EPV/PSR/TORP composition), `constants_sim.R` (simulation parameters), `constants_match.R` (match prediction model), `constants_data.R` (validation + coord/contest extraction). Pure file-organisation change; no constants renamed or removed. 160 declarations confirmed across the new files.

* **`injuries.R` (1012 LOC) split into 4 themed files** — `injuries_scrape.R`, `injuries_match.R`, `injuries_schedule.R`, `injuries_validation.R`. Same 13 functions, organised by concern.

* **`ladder.R` (1426 LOC) split into 3 themed files** — `ladder.R` keeps `calculate_ladder()` / `calculate_final_ladder()`; `finals_sim.R` houses the top-8 bracket (`simulate_finals()`, `simulate_match()`, finals home advantage); `season_sim.R` covers data prep, residual extraction, the `simulate_afl_season()` entry point, and the print method. Same 12 functions, organised along the section-header boundaries the file already had.

* **`R/globals.R` pruned** — removed ~84 orphan `utils::globalVariables()` declarations (per-position `_diff` / `.x` / `.y` columns refactored away, plus a long tail of one-off renames). New helper script `data-raw/debug/find_orphan_globals.R` re-runnable against any future drift.

* **`plot_defaults()` exported** — new helper in `R/plot_utils.R` returning a named list of recurring visual constants (line weights, point sizes, reference greys) so future plots have a single source of truth. Existing `plot_*.R` functions still hardcode these values inline; migration is opportunistic.

* **`ladder.R simulate_finals()` refactored** — closure `<<-` mutation of the `ratings` vector replaced with explicit environment-held state, matching CLAUDE.md's "use environments instead of `<<-`" rule.

* **`download_model_from_release()` (torpmodels)** — error-message accumulation via `<<-` replaced with `tryCatch` return values. Same semantics, more idiomatic R.

* **GAM smooth-basis `k=` convention documented** in `match_train.R` (k=5 for `bs="ts"` thin-plate splines, k=4 for `ti(...)` tensor interactions, with rationale).

* **Documentation** — `scraper.R` now has a clear header declaring its scope (chains-only) versus the broader endpoints in `afl_api.R`. `centrality.R find_components()` documents the deliberate `<<-` in its union-find path-compression. `zzz.R` clarifies that the `attachNamespace("mgcv")` is a perf optimisation rather than a correctness requirement (the function-level guard in `get_shot_result_preds()` covers correctness).

* **PR#86 review response** — chunk validation, empty-xG guards, format_blog tests, attr docs.

* **CI** — pkgdown.yml now registers all exported functions; cleared dev-branch test failures.

---

# torp 1.3.2 (2026-04-18)

## Bug Fixes

* **Player position groups now derived from PBP playstyle, not lineup role** — `.resolve_stat_rating_positions()` prefers `position_group` (6-way PBP-derived classification) over `lineup_position` (20-way AFL API named role). Previously, players named in the forward pocket — e.g. tall forwards rotating through FPL — were classified as small/medium forwards based on their lineup, when their actual ball-contest behavior was key-position. Adds a teams-table fallback for fringe players who never registered a PBP `position_group`.

## Model Updates

* **Removed `bounces` from PSR feature set** — coefficient was non-causal (bouncers are ball carriers in transition, often correlated with losing teams rather than causing losses). Refit PSR/OSR/DSR coefficients on the reduced feature set. The defensive transition signal now concentrates correctly into `def_half_pressure_acts`.

## Internal

* `.prepare_stat_rating_data()` and `.resolve_stat_rating_positions()` accept a new `teams` parameter for fallback `pos_group` assignment from `lineup_position` modal.

* PSR training script (`06_train_psr_model.R`) now filters leaderboard displays to `wt_80s >= 5` to suppress fringe low-sample players from top-N lists.

---

# torp 1.3.0 (2026-04-02)

## New Features

* **Coordinate sign-flip correction** — `fix_chain_coordinates_dt()` detects and corrects AFL API sign-flipped x,y coordinates at possession changes. 8-step pipeline (throw-in fix, iterative sign-flip, both-neighbor confirmation, neighbor interpolation, paired flip) eliminates 99.7% of >100m pitch-relative jumps. New constants `COORD_JUMP_THRESHOLD` (100m) and `COORD_FLIP_TOLERANCE` (70m).

* **Player xG skill extraction** — `extract_player_xg_skill()` extracts per-player shooting ability from the shot GAM's random effects. Returns player-level xG skill adjustments, standard errors, and shot counts.

* **Generic GAM random effect extractor** — `extract_gam_random_effects()` extracts coefficients and SEs from any mgcv GAM random effect smooth, recovering actual factor level names from the model's training data.

* **Team quality residuals in simulation** — `simulate_afl_season()` now correctly displays team GAM residuals in the summary table (fixed factor level name extraction).

* **Win probability model fitting** — `fit_win_probability()` now exported for custom WP model training.

## Bug Fixes

* Fixed AFL API delivering coordinates in the wrong team's frame for ~12% of PBP rows at possession changes (Spoils, Loose Ball Gets, Contested Marks, etc.).

* Fixed team residual extraction returning numeric indices instead of team names, causing all residuals to be zero in simulation output.

* Fixed parallel connection error in stat rating optimization (`02_optimize_stat_rating_params.R`) caused by stale file handles from prior pipeline phases exhausting R's connection pool.

* Fixed non-ASCII character (`x` instead of `×`) in `win_probability.R` that caused R CMD check WARNING.

* Fixed integer truncation warnings in pitch-relative coordinate conversion by using `as.double()`.

## Model Updates

* Retrained EP model (128 rounds), WP model (50 rounds), shot GAM, and match prediction GAMs on coordinate-corrected data.

* Re-optimized all 56 stat rating hyperparameters with cleaner coordinate data.

## Documentation

* Added Coordinate System section to ARCHITECTURE.md documenting the 8-step sign-flip fix pipeline with step-by-step table.

* Added Release Workflow section to CLAUDE.md (pre-PR checklist, version bumping, NEWS.md conventions).

---

# torp 1.2.0 (2026-03-31)

## New Features

* **Player Stat Ratings system** — Bayesian estimation of 48 rate stats + 6 efficiency stats with positional priors and exponential decay. New exported functions:
  - `estimate_player_stat_ratings()` — batch stat rating estimation
  - `player_stat_rating_profile()` — per-player stat rating profiles with percentile ranks
  - `get_player_stat_ratings()` — lookup stat ratings for a player
  - `team_stat_rating_profile()` — team-aggregated stat rating profiles
  - `get_team_stat_ratings()` — lookup team stat ratings
  - `aggregate_team_stat_ratings()` — aggregate player stat ratings to team level
  - `stat_rating_definitions()`, `default_stat_rating_params()`, `stat_rating_position_map()` — configuration helpers

* **Player Skill Rating (PSR)** — glmnet model mapping stat ratings to predicted margin contribution. New functions: `calculate_psr()`, `calculate_psr_components()`, `calculate_psv()`, `calculate_psv_components()`, `psr_ratings()`.

* **TORP blend** — `torp_ratings()` now combines EPR (50%) + PSR (50%) for a complete player rating. Deprecates `calculate_torp_ratings()`.

* **Win Probability Added (WPA) credit** — `create_wp_credit()` allocates WPA between disposers and receivers.

* **Player attribution** — `calculate_player_attribution()` and `batch_player_attribution()` for zero-ablation player impact measurement.

* **Network centrality** — `calculate_player_centrality()` for opponent quality adjustment.

* **Team profiles** — `team_profile()`, `team_stat_rating_profile()`, `get_team_stat_ratings()` for team-level analysis.

* **Weather data loading** — `load_weather()` for historical weather data from torpdata releases.

* **Injury scheduling** — `build_injury_schedule()` and `load_preseason_injuries()` for simulation-aware injury management.

* **New load functions** — `load_player_stat_ratings()`, `load_psr()`, `load_retrodictions()`.

## Documentation

* Updated README, vignettes, and pkgdown reference to use `torp_ratings()` instead of deprecated `calculate_torp()` / `calculate_torp_ratings()`.

* Expanded CLAUDE.md Key Files section to cover all 52 R files.

* Expanded ARCHITECTURE.md Code References table with 14 previously undocumented components.

* Added position codes and RAPM to ARCHITECTURE.md glossary.

* Added GitHub issue and PR templates.

---

# torp 1.1.0 (2026-03-10)

## Breaking Changes

* **Standardised all column names to canonical `snake_case`** across the entire torp ecosystem. Old column names from multiple API schema versions (CFS camelCase, v2 dot-notation, ad-hoc abbreviations) are now normalised at load/fetch time via central column maps in `R/column_schema.R`. Key renames include:
  - Fixtures: `providerId` → `match_id`, `compSeason.year` → `season`, `round.roundNumber` → `round_number`, `home.score.totalScore` → `home_score`
  - PBP: `home_team_team_name` → `home_team_name`, `home_team_score_total_score` → `home_score`
  - Chains: `matchId` → `match_id`, `playerId` → `player_id`, `displayOrder` → `display_order`
  - Player stats: `extended_stats_spoils` → `spoils`, `clearances_total_clearances` → `clearances`
  - Player game data: `plyr_nm` → `player_name`, `tot_p` → `total_credits`, `recv_pts` → `recv_credits`, `tm` → `team`

* Old parquet files with legacy column names are normalised automatically at load time — no data regeneration required for backward compatibility.

## New Features

* **Central column schema infrastructure** (`R/column_schema.R`): per-data-type column maps (`FIXTURE_COL_MAP`, `PBP_COL_MAP`, `CHAINS_COL_MAP`, `PLAYER_STATS_COL_MAP`, `PLAYER_GAME_COL_MAP`, `TEAMS_COL_MAP`) and a generic `.normalise_columns()` function that remaps old names at load time.

## Bug Fixes

* Fixed dplyr data masking bug in `filter_game_data()` where renaming column `tm` → `team` caused it to shadow the function parameter. Now uses `.env$` pronoun for disambiguation.

* Fixed `get_afl_player_stats()` returning `providerId` instead of `match_id` in output.

* Fixed `detect_chains_columns()` silently returning wrong column name mappings when passed a plain data.frame (non-data.table) with camelCase columns.

---

# torp 1.0.0 (2026-03-05)

## Breaking Changes

* `load_pbp()` and `load_chains()` now default `rounds = TRUE` (all rounds) instead of the current week. Callers that relied on getting only the latest round without specifying `rounds` will now receive the full season. Use `rounds = get_afl_week()` to restore the old behaviour.

* `load_from_url()` now defaults `use_disk_cache = FALSE` (previously `TRUE`). Pass `use_disk_cache = TRUE` explicitly if you want persistent disk caching.

* `load_fixtures(all = TRUE)` now starts from 2021 (previously 2018). Seasons before 2021 are outside `validate_seasons()` range and were producing errors.

* `check_internet_connection()` has been removed. Use `curl::has_internet()` directly.

* Unexported internal-use functions: `get_wp_model_info()`, `check_wp_model_health()`, `harmonic_mean()`, `norm_name()`. These remain accessible via `torp:::`.

## New Features

* **Local-first data loading**: All `load_*()` functions now check `torpdata/data/` first, then disk cache, then download. Downloaded files are auto-saved locally.

* `download_torp_data()` for bulk-downloading parquet files for offline access.

* **Parallel downloads**: Multi-URL loads use `curl::multi_download()` for faster batch fetching.

* **Negative cache (skip markers)**: 404 URLs are marked with `.skip` files to avoid repeated failed downloads. Use `clear_skip_markers()` to retry.

* **Column selection**: All `load_*()` functions accept a `columns` parameter to read only specific columns.

* New load functions: `load_ep_wp_charts()`, `load_player_game_ratings()`, `load_player_season_ratings()`, `load_team_ratings()`.

* `CREDIT_POS_ADJ_QUANTILE` split into 4 per-dimension constants: `CREDIT_POS_ADJ_QUANTILE_RECV`, `_DISP`, `_SPOIL`, `_HITOUT`.

* Added `R/constants.R` with centralized AFL and model constants:
  - `AFL_GOAL_WIDTH`, `AFL_QUARTER_DURATION`, `AFL_TOTAL_GAME_SECONDS`
  - `RATING_DECAY_DEFAULT_DAYS`, `SIM_NOISE_SD`, `SIM_WP_SCALING_FACTOR`

* Placeholder dashboard functions (`create_monitoring_dashboard_data()`, `get_model_health_status()`) now return informative empty structures instead of fake data.

## Bug Fixes

* `parquet_from_urls_parallel()` now warns instead of silently dropping data when column selection finds no matching columns.

* `parquet_from_urls_parallel()` now errors (instead of just warning) when downloads completely fail and no local data is available.

* `mark_download_skippable()` now logs write errors instead of silently swallowing them.

* `read_local_parquet()` now only deletes files on likely corruption errors, not transient failures (memory, locking).

* `load_torp_ratings()` and `load_team_ratings()` now warn when returning empty data.

* `load_from_url()` now warns when round filtering is requested but no round column exists in the data.

* Integration test data loading is now guarded against CRAN environments.

* Fixed double fixture load in `get_afl_week()` - now loads fixtures once and filters twice.

## Optimized Parameters

* Re-optimized rating constants with per-component decay: `RATING_DECAY_RECV` (260), `RATING_DECAY_DISP` (700), `RATING_DECAY_SPOIL` (295), `RATING_DECAY_HITOUT` (700). Prior games: `RATING_PRIOR_GAMES_RECV` (12.56), `RATING_PRIOR_GAMES_DISP` (5.83), `RATING_PRIOR_GAMES_SPOIL` (3.00), `RATING_PRIOR_GAMES_HITOUT` (15.00).

* Re-optimized credit assignment constants for disposal, reception, and position adjustment.

## Code Quality

* Merged `logging_monitoring.R` and `safe_logging.R` into a single `logging.R` file for better organization.

* Fixed global state issues by replacing `<<-` operator with package-level environment for logging state.

* Improved AUC calculation efficiency using Mann-Whitney U-statistic (O(n log n) instead of O(n^2)).

* Fixed inefficient `rbind()` patterns in `compare_baseline_models()` and `evaluate_baseline_models()` by using pre-allocated lists with `dplyr::bind_rows()`.

* Fixed `mutate_all()` performance issue in data validation using `lapply()` for column-wise operations.

* Removed unused `match_id` parameter from `match_xgs()` function.

* Replaced deprecated `dplyr::group_by_all()` with `dplyr::group_by(dplyr::across(dplyr::everything()))`.

## Documentation

* Added two vignettes: Getting Started and torp Reference Guide (consolidating ratings, models, data architecture, and simulation).

* Added pkgdown site configuration with comprehensive reference sections.

* Improved README with lifecycle badge, ecosystem table, and torpmodels install instructions.

## Internal Changes

* Reduced exported functions - internalized helper functions:
  - `rds_from_url()`, `file_reader()` (internal data loading helpers)
  - `predict_wp_naive()`, `predict_wp_time_only()` (baseline model internals)
  - `log_prediction_event()`, `log_data_quality()` (internal logging helpers)
  - `get_wp_model_info()`, `check_wp_model_health()` (model diagnostics)
  - `harmonic_mean()`, `norm_name()` (utility helpers)

* Moved manual test scripts to `tests/manual/` directory.

* Archived legacy data-raw scripts to `inst/extdata/archive/scripts/`.

* Enhanced test helper functions with additional mock data creators.

## Test Coverage

* Added tests for `helper_functions.R` internal functions.
* Added tests for `match_xg_functions.R` function signatures.
* Added tests for `sim-helpers.R` simulation functions.

---

# torp 0.0.0.9001

* Initial development version.
* Core data loading functions for AFL play-by-play, chains, xG, and player stats.
* Expected points (EP) and win probability (WP) models.
* Player rating system (TORP).
* Match prediction framework.
