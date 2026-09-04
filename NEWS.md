# torp 1.4.3

## Matchup table (finals odds)

* **Fixed: the matchup table stopped publishing every finals round, from round 1
  of finals onward (torp#163, first failed 2026-08-23).** `.extract_frozen_teams()`
  read team ratings from the fixture for the target round only; every
  home-and-away round happens to field all 18 clubs, but a finals round fields
  fewer, so the table's `MIN_TEAMS = 18` gate correctly refused to publish an
  incomplete table. Eliminated/bye teams now carry forward their most recent
  available roster/injury-adjusted rating instead of being dropped, so the
  table stays a genuine full 18-team round-robin through every finals week
  (Pete's call, 2026-09-04 -- the alternative was shrinking the gate to match
  the actual finals field, which would leave `resolveFinal()` unable to find a
  row for some hypothetical ties).

* **Fixed a second, independent failure surfaced while validating the above:**
  the predict-path verification gate (Gate 1) compared `.predict_match_model()`'s
  replay against `pred_score_diff` for every row in the target round, including
  rows whose result had already landed (finals games are staggered across
  several days, so this script can now run mid-round). A played row is a
  TRAINING row, and the 2026-09 out-of-fold stacking fix deliberately gives it a
  different `gam_pred_score_diff` than a fresh `predict()` replay -- correctly,
  by design, but Gate 1 read that as drift (up to 16.3 margin points on 2026
  R26) and refused to publish. Gate 1 now compares only rows still awaiting a
  result, which is also the only class of row the matchup table actually needs
  the predict path to get right.

## Match model

* **`MATCH_BLEND_WEIGHT` set to 1.0 (pure GAM; XGBoost still trained but no
  longer blended into `pred_score_diff`/`pred_win`).** Three independent
  measurements agree GAM-only beats every blend weight on MAE, Brier and
  log-loss: the original WS5 sweep, a fresh out-of-fold validation on current
  data, and a from-scratch retest of a better-calibrated XGBoost variant.
  XGBoost's own training path is unchanged, so this is a served-blend change
  only, not an architecture change.
* **Fixed in-sample leakage in the GAM and XGBoost stacked cascades**
  (`.train_match_gams()`, `.train_match_xgb()`). Each stage's prediction fed
  forward into later stages was previously the stage's own in-sample fit;
  training rows now get season-grouped out-of-fold predictions instead.
  Measured on a full 54-round rolling comparison (423 out-of-sample matches):
  every delta on the served blends is inside noise. This is a correctness fix
  for a real leak class, not a performance change.

  **Known limitation:** the GAM cascade's `team_name_season` random effect
  is season-scoped by construction, so season-grouped folds give it zero
  training/held-out overlap on every fold -- mgcv predicts it at the
  population mean rather than erroring. Already reflected in the rolling
  comparison above (measured harmless on the served blend); see the comment
  above `gam_folds` in `R/match_train.R` for detail.

# torp 1.4.1

## Rating changes

* **AFLW PSR/OSR/DSR coefficients retrained on the full 2018-2026 history.** The
  published set was trained on 2021-2024 only -- not by choice, but because
  `validate_seasons()` floored every AFLW load at `AFL_MIN_SEASON` (2021, the
  men's chain-data start). The comp-aware floor shipped in 1.4.0 made 2018-2020
  reachable; this retrain is the first to use it. Training grows from 350 to 463
  matches (+32%) and CV folds from 4 to 7.

  **The size of the gain is easy to overstate, so state it carefully.** A first
  pass reported margin RMSE 30.31 -> 29.38 on the identical 135-match 2025-2026
  test set. That comparison is confounded: each arm was CV-selecting its own
  elastic-net alpha and they landed on different ones (2021-24 chose alpha=1,
  2018-24 chose alpha=0), so it moved the training window and the penalty family
  together. Holding alpha fixed and comparing like with like, the window alone is
  worth roughly -0.4 RMSE averaged over the grid, and at alpha=0 -- the penalty
  this release actually ships -- it is:

  | | 2021-24 | 2018-24 |
  |---|---|---|
  | Margin RMSE | 29.50 | 29.38 |
  | Margin MAE | 23.41 | 23.53 |

  So RMSE improves by 0.12 and **MAE gets slightly worse**. The window is better
  on RMSE at every alpha tested (0, 0.25, 0.5, 0.75, 1), which is real evidence of
  direction, but the effect is small and **not statistically significant anywhere**
  (best paired p = 0.29; at alpha=0, p = 0.64, better on 72 of 135 matches).

  It is adopted on the principle that excluding available data needs the stronger
  justification, not on a demonstrated accuracy win.

  Ratings move very little: Spearman rank correlation 0.992 across 91,083
  player-rounds, mean |delta| 0.12 PSR, and 9 of the top 10 at 2026 R2 are
  unchanged (the one swap is a 0.01 tie at tenth). The top three are identical.

  **Known regression:** the OSR/DSR decomposition path gets slightly worse
  (off-minus-def RMSE 29.39 -> 29.92) while the direct margin fit improves.
  PSR itself is scored from the margin fit, so the headline rating is the one
  that improved, but the component split is marginally worse.

  This one is explained. Pinning alpha=1 removes the regression (off-minus-def
  29.38) and gives the best margin metrics of any arm, but it **fails the anchor
  checks** and was rejected: lasso at that lambda collapses PSR nonzero betas from
  33 to 9 of 48, puts 93% of |beta*sd| on three stats, changes the top drivers
  entirely, drops Spearman against published ratings to 0.829, and drives the OSR
  `goals` coefficient to exactly 0 -- an offensive rating with no weight on goals.
  Alpha therefore stays CV-selected; see the note in
  `data-raw/06-stat-ratings/aflw_run_pipeline.R`.

  The retrained files carry six extra `stat_name` rows (`effective_kicks`,
  `effective_disposals`, `intercept_marks`, `f50_ground_ball_gets`,
  `score_launches`, `marks_on_lead`). These stats exist **only in 2018-2019** and
  are absent from 2020 onward, including all live data, so the per-round
  estimator collapses them to a constant (sd exactly 0) and they contribute
  nothing: dropping them yields bit-identical predictions. They are inert
  placeholders, not live features.

# torp 1.4.0

## New features

* **AFLW extended stats, via a previously-undocumented AFL API endpoint.** CFS's
  `playerStats/match` returns an empty `extendedStats` block for AFLW, so 25
  fields (`spoils`, `pressure_acts`, `effective_disposals`, ...) had never
  existed for the women's competition. An outside contributor (`jhol3990`, on
  commit `abe27f56`) located a working alternative at
  `api.afl.com.au/statspro/playersStats/seasons/{id}`. New
  `get_afl_player_season_stats()` and `load_aflw_season_stats()`;
  `aflw_season_stats-data` published for 2018-2026.

* **Per-round AFLW extended stats, by differencing weekly snapshots.** That
  endpoint returns only a *season-to-date cumulative total* — no as-at-date
  parameter exists, confirmed from AFL.com.au's own client source. So
  `aflw-season-stats-weekly.yml` captures a dated snapshot each Tuesday and
  `diff_aflw_season_snapshots()` differences consecutive captures. **Works only
  going forward from when the cron starts**; already-played rounds remain
  season-total only. Only *cumulative* columns are differenced — subtracting
  season-to-date rates or `_avg` columns yields a plausible-looking meaningless
  number, so those are excluded and listed in a `rate_cols_dropped` attribute.

* **AFLW PSR is stored, not just computed.** Previously every AFLW PSR figure
  was calculated on demand and discarded. New `load_aflw_psr()` and a Stage 7
  in `run_ratings_pipeline.R` that scores from frozen coefficients and
  publishes `aflw_psr-data` (2018-2026, 91,083 player-rounds). Scoring only —
  `aflw_run_pipeline.R` remains the *training* script and deliberately stays
  off the daily cadence, since running it there would retrain the rating
  definition every day.

* **`xrapm_diff` added to the match model**, with a production home for the
  rating: `team_rapm_asof-data`, `load_team_rapm_asof()`, and a weekly
  `publish-xrapm-snapshots.yml`. **This feature does not pass the project's own
  `g7_verdict()` gate** (β=1.079, p=0.078, dMAE −0.143) — deterministically
  reproducible, not noise, but below threshold. Shipped as a deliberate
  judgement call, recorded here so the evidence level travels with the code.

## Bug fixes

* **Stat-rating pipelines were estimating at unplayed future rounds.** Both
  pipelines built their checkpoint dates from `load_fixtures()`, which includes
  scheduled-but-unplayed fixtures — AFLW 2026 produced 10 phantom rounds (map to
  round 12, only 2 played), AFLM 5. Not inert: the phantom rows flow into
  `calculate_psr()`'s position-standardisation step, which pools by position
  with no season/round grouping, shifting the within-position SD and rescaling
  **real** players' ratings (AFLW up to 0.2095, 80%+ of rows; AFLM ~0.0159).
  They also made `max(round)` a trap for anything reading the intermediate
  artifact. New `.played_round_ref_dates()` filters on a *recorded score* rather
  than a date, so postponed-but-past-dated matches are excluded too, and
  `.assert_ref_date_coverage()` catches the reverse case where the results feed
  lags the player-stats feed and a genuinely-played round would be dropped.

* **AFLW's 2018-2020 history was unreachable through five loaders.**
  `validate_seasons()` floors at `AFL_MIN_SEASON` (2021, where men's *chain*
  data starts), so `load_results(2019, comp = "AFLW")` aborted outright while
  `load_fixtures(2019, comp = "AFLW")` returned 38 scored matches. New
  `.validate_seasons_comp()` dispatcher routes to an AFLW floor of 2018;
  the men's path is provably unchanged. Found by consequence — it blocked the
  first `aflw_psr-data` publish, because the artifact guard verifies against
  `load_results()` and could not see those seasons.

* **The as-of xRAPM join leaked each round's own result into its own feature.**
  Checkpoints labelled `round_number = r` are dated the day *before* round r+1,
  so they contain round r's results, and the join used inclusive `>=`. Now a
  strict `>`, so a round only ever sees a prior checkpoint. Three existing tests
  had asserted the leaked behaviour as correct and were corrected.

* **`versebus.R`: four silent-failure defects ported from bouncer's review
  (canonical copy; already fixed in `peteowen1/bouncer@86e2ebc` and ported to
  `pannaverse/panna` as panna#187; this repo was the last vendored copy still
  carrying all four).** All four turn a transient failure into a
  silently-accepted "everything is fine":
  * `vb_read_manifest()`'s retry-once branch classified every error as
    "confirmed absent" instead of reusing `vb_classify_error()` like the
    first attempt does. A network blip on the retry looked identical to the
    manifest genuinely having been deleted, fell through to legacy mode, and
    **disabled sha256 verification for every download on that tag for the
    rest of the session** behind a one-time warning nobody would connect to
    the cause.
  * `vb_download()`'s `verify_by_size()` swallowed a failed asset listing and
    skipped the check entirely rather than distinguishing "listing worked,
    asset not in it" (fine, nothing to check) from "the listing call itself
    errored" (no check happened at all). This is the *only* integrity check
    on an unmanifested tag -- the common case -- so a transient API failure
    meant the file was moved into place and given a `.sha256` sidecar as
    though verification had passed.
  * `vb_publish()`'s cache-invalidation hook failed via bare
    `try(..., silent = TRUE)` -- the only failure path in this file with no
    logging at all. A dead hook meant downstream consumers kept serving
    pre-publish data indefinitely with nothing recording why.
  * `vb_generation()` ran `max()` on `updated_at` with no `na.rm`. One
    unrelated asset missing a timestamp (which `vb_list_assets()`
    deliberately tolerates as `NA` rather than failing the whole listing)
    silently turned the entire generation into `NA`, indistinguishable from
    "no assets at all". Latent today (no caller in this package yet).

  Each has a dedicated regression test in `tests/testthat/test-versebus.R`,
  mutation-tested by reverting the fix and confirming the test fails.
  `torpverse/torpmodels/R/versebus.R` still carries all four unfixed and now
  fails `test-versebus-sync.R`'s drift guard against this copy -- follow-up
  needed there.
* **`versebus.R` → `VERSEBUS_VERSION` 1.1.0** (canonical copy; mirrored to
  `pannaverse/panna` in the same change, which `test-versebus-sync.R` verifies).
  * `vb_publish()` now restores `piggyback_cache_duration` on exit. It was set
    unconditionally and never reset, so the first publish in a session silently
    disabled piggyback's listing cache for every unrelated caller afterwards.
  * `.vb_generation_stamp()` no longer builds its local suffix with `sample()`.
    Doing so advanced the **caller's** RNG stream, so publishing changed the
    draws of any simulation seeded before it — an invisible reproducibility
    break in a package that also fits models and runs sims. Now uses
    `tempfile()`, which is process-unique and does not touch `.Random.seed`.
  * `vb_publish()`'s post-upload verify loop iterates `seq_len(n + 1L)` rather
    than `seq_along(c(verify_delays, NA))` — same count, no throwaway `NA`.

## Bug fixes

* **`versebus.R` → `VERSEBUS_VERSION` 1.1.0** (canonical copy; mirrored to
  `pannaverse/panna` in the same change, which `test-versebus-sync.R` verifies).
  * `vb_publish()` now restores `piggyback_cache_duration` on exit. It was set
    unconditionally and never reset, so the first publish in a session silently
    disabled piggyback's listing cache for every unrelated caller afterwards.
  * `.vb_generation_stamp()` no longer builds its local suffix with `sample()`.
    Doing so advanced the **caller's** RNG stream, so publishing changed the
    draws of any simulation seeded before it — an invisible reproducibility
    break in a package that also fits models and runs sims. Now uses
    `tempfile()`, which is process-unique and does not touch `.Random.seed`.
  * `vb_publish()`'s post-upload verify loop iterates `seq_len(n + 1L)` rather
    than `seq_along(c(verify_delays, NA))` — same count, no throwaway `NA`.

## New features

* **`player_epv_breakdown()` — where a player's value actually comes from.**
  Decomposes each player-game's EPV into the **29 box-score categories the credit
  model is built from** (contested marks, ground ball gets, marks inside 50,
  goals, shots at goal, clangers, turnovers, metres gained, intercepts,
  one-percenters, tackles, hitouts, …) plus a per-channel `chain` residual.
  Returns points, counts and share, long-format, ready for a profile page.

  **The parts add to the whole, and that is enforced.** `verify = TRUE` aborts
  unless the categories reproduce the published `epv` for every player-game — a
  breakdown that merely came close would put numbers on a page that visibly
  disagree with the rating beside them.

  **The residual is a finding, not a leftover.** It averages **39.7% of total
  absolute EPV**, so roughly two-fifths of a rating comes from play-by-play
  context with no counting stat behind it — precisely what a counting-stat
  profile cannot show.

  **Not built from `delta_epv`.** That was the obvious approach and it is wrong:
  summing pbp's per-row `delta_epv` by player correlates only **0.626** with
  published EPV and is out by a mean of **4.82 points per player-game**, because
  `delta_epv` is the swing *caused by* an event while the credit model splits it
  between disposer and receiver. A prototype built that way failed its gate at
  `max|epv_disp − rebuilt| = 16.78`, which is how the real structure was found.

  Plan, measurements and the remaining chain-side phase:
  `docs/plans/PLAYER-EPV-BREAKDOWN-PLAN.md`.

## Display

* **Spoil and hitout are now shown together as one "Contest" channel.** Neither
  was ever what its name said: `epv_spoil` is spoils **plus tackles plus
  pressure**, and `epv_hitout` is hitouts **plus ruck contests**
  (`player_credit.R:857-859`). Both are contest value, so the sum is the honest
  unit to display.

  `plot_team_ratings()` gains `metric = "contest"`; `"spoil"` and `"hitout"`
  still work and are relabelled as its aerial and stoppage halves. The
  `team_profile` print method now shows one `contest` column in place of the two.

  **Display only — no published column changes.** `team_epr_contest` is computed
  on demand rather than added to the release, because the release schema is a
  consumer contract and none of the ratings move.

  Two things to expect. **It looks flat**: contest sd is 0.266 against disposal's
  1.693 and receiving's 1.134, about 2% of EPR's spread — that is its honest
  size. And **midfielders top the aerial half** (Dunkley, Cripps and Curtis above
  Harris Andrews at 2026 R23), which is the tackles-and-pressure content showing
  through, not an error.

  Accuracy, precisely: the merge itself moves the last bit only (~1e-16, from
  reassociating the sum — floating-point addition is not associative). Player
  ratings reconcile exactly (`epr = recv + disp + contest` to 0.000000 across all
  721 rated players at 2026 R23). Team ratings reconcile to ~0.02, a
  **pre-existing** gap from each `team_epr_*` being rounded to 2dp independently;
  the four-way split carries it too and merging does not widen it.

## Bug fixes

* **Match predictions could be attached to the wrong match, silently, as soon as
  the finals fixture is published.** `.train_match_xgb()`'s `predict_all()` built
  its design matrix with `stats::model.matrix()`, whose default `na.action` is
  `na.omit` — so every row carrying an NA in a feature column was **dropped**, and
  the function returned fewer predictions than the frame had rows. The caller
  assigns that straight back (`team_mdl_df$xgb_pred_... <- predict_all(...)`),
  which fails two ways: when the lengths do not divide it errors far from the
  cause (`"replacement has N rows, data has M"`, naming neither the NAs nor the
  matches), and **when they do divide R recycles in silence** and every prediction
  after the first gap lands on the wrong match. Placeholder finals fixtures (teams
  TBD) carry NA rating features every year from the moment the AFL publishes the
  finals schedule — precisely when predictions matter most. Bit twice on
  2026-07-29.

  Fixed by routing `na.action` through `stats::model.frame()`, which is the only
  form that works: **passing `na.action = na.pass` to `model.matrix()` directly
  does not preserve the rows** (measured — 5-row frame in, 3 rows out), and that
  is the obvious one-line fix. XGBoost then routes NA down the default branch it
  learned at training time, so the vector comes back full length and finite. The
  helper is now `.predict_all_rows()` at file scope, and it **aborts** rather than
  return a vector that could recycle. Training is unaffected — it is fed completed
  matches only. Identity behaviour on frames with no NAs is covered by test.
  New tests: `tests/testthat/test-match-predict-alignment.R`.

* **Season simulations were built by the wrong estimator for five months, and
  the numbers move now that they aren't.** `c847a917` (2026-03-16) renamed the
  published team-rating column `team_torp` → `team_epr`, but
  `prepare_sim_data()` kept asking for `team_torp`. `as.numeric(NULL)` builds a
  0-row table rather than erroring, so a perfectly successful load looked
  exactly like "no data" and every simulation silently fell through to a
  player-TORP fallback. The lookup now resolves `team_torp` first, then
  `team_epr`, and `run_ratings_pipeline.R` publishes `team_torp`.
  **This changes the non-injury-aware path only**: on it, simulated margins
  widen ~19% and 4 of 18 teams move more than 2 ladder places. **It does not
  change the simulations published to inthegame.blog.** Those pass injuries, so
  `use_injury_aware` is `TRUE`, the whole team-ratings lookup is skipped
  (`R/season_sim.R`, `if (!use_injury_aware)`), and team ratings are built live
  from player TORP with injured players excluded. Verified on the first blog
  build after this merged: torpdata run 31661873642, `torp_sha 0d2676a`, logging
  `Building injury-aware team ratings from player TORP (top 21 per team)`.
  Whether the published path *should* use `team_torp` is open and unmeasured —
  see torp#149. `team_torp` was chosen on measurement —
  it wins every scale-free comparison against `team_epr` and `team_psr` — see
  `../docs/reviews/2026-08-12-TEAM-RATING-CALIBRATION.md`, which also records what
  the first pass of that analysis got wrong. Releases published before
  2026-08-12 carry no `team_torp`; until the ratings pipeline re-runs,
  `prepare_sim_data()` falls back to `team_epr` and says so.

* `prepare_sim_data(injuries = FALSE)` no longer errors. `nrow()` returns `NULL`
  for a non-data.frame, so the documented "no injury adjustment" value made the
  internal `use_injury_aware` flag `NA` and died on `if (NA)` with a message
  naming neither the argument nor injuries. Only `simulate_afl_season()`
  normalising `FALSE` to `NULL` kept it off the front door.

## Chores

* **Building a match prediction no longer implies publishing one.**
  `run_predictions_pipeline()` bundled an upload it could not opt out of, and
  that single fact caused three separate problems: `build_matchup_table()`
  re-implemented the load → feature → injury-overlay sequence rather than call
  it (and said so in its own header), the orchestration had no test coverage
  because exercising it published, and the margin-calibration sidecar scored
  its own copy of the blend. The state half is now
  `build_prediction_state()`; `run_predictions_pipeline()` is that plus the
  uploads, with identical arguments and identical behaviour.

  **Verified as a move, not a rewrite:** of the 345 lines relocated, 338 are
  byte-identical. The only edits are the results refresh gaining an
  `isTRUE(refresh_results)` guard (one line, default `TRUE`, so production is
  unchanged) and the interactive validation-failure return being rebuilt from
  the shared state list instead of being written out inline. A mechanical diff
  of old-span against new-span is what establishes this, not review by eye.

  `refresh_results` exists because publishing the season's results to the
  `results-data` release is the one side effect that lives inside the state
  half. Production keeps it; a read-only caller passes `FALSE`. Note what
  `FALSE` costs: `results` feeds GAM training, so a read-only build trains on
  whatever the release already held, silently.

  **One bug the move introduced, found in pre-PR review and fixed before
  merge, because it is the exact blind spot of a diff-based proof.** The state
  half has a *third* exit: when there are no TORP ratings for the target week
  yet — pre-season, or fixtures not published — it returns `NULL`. That used
  to end the whole pipeline. After the split it only ended the builder, and
  the wrapper unpacked `state$…` from `NULL` without checking. `NULL$anything`
  is `NULL` and `length(NULL) == 0`, so the validation gate was bypassed and
  execution reached the upload and died on `NULL |> dplyr::ungroup()` — an
  unhandled crash replacing a clean, expected no-op, on the scheduled
  workflow's direct call. The guard is one line; the lesson is that a
  line-diff of relocated code cannot see a *missing* guard at the new seam,
  so the regression test mocks the builder and asserts the wrapper exits
  cleanly and publishes nothing.

* **Four tests on the new seam**, including a contract guard for the failure
  mode this refactor actually risks: the uploader reading a `state$` field the
  builder does not return, which today would surface as "object not found" in
  production. Both new guards were mutation-tested — dropping `team_mdl_df`
  from the state list and planting a `.build_team_mdl_df()` call back in the
  upload half are each detected. The source-scanning helpers moved to
  `helper-source-scan.R` rather than being copied into a second test file.

## Rating changes

* **The ruck must now win contests to gain points.** This CHANGES PUBLISHED
  RATINGS. `EPV_RUCK_CONTEST_WT` goes **+0.0232 → −0.0232**: it used to pay for
  every contest *attended*, won or lost, so a ruck banked roughly +0.70 a game
  for turning up — most of the "the channel over-pays rucks ~11×" finding.
  `EPV_HITOUT_WT` goes **0.0510 → 0.0615**, which sets break-even at the actual
  league average win rate of **37.7%**: an average ruck's contest work is worth
  about nothing, a better one positive, and a ruck who attends 30 and wins 10
  without direction now goes negative. `EPV_HITOUT_ADV_WT` is unchanged at
  0.1748 — direction is the ruck's skill and carries 70.5% of the channel's
  variance.

  Judged on the fast EPR gate (1,194 matches, rating as the only feature):
  **better on all five of MAE, RMSE, Brier, logloss and bits**, with the
  within-team coefficient moving toward 1.0. Tips is fractionally down, about
  two across 1,005 matches. Face validity passes on all four rows — position mix
  +1, Spearman 0.9991, nobody appears from nowhere, biggest climb +10.

  The production match gate read dMAE +0.2194 and is **not** the basis for this;
  it was overturned by the EPR gate and by the channel's own signal, which
  roughly doubled (correlation with margin 0.089 → 0.169). See
  `docs/HOW-WE-WORK.md`.

  One weight is set against the measurement, deliberately and on the record: the
  fit puts an undirected tap at −0.0209 per ruck (t −3.5, stable across halves),
  and it ships positive. Attendance, by contrast, **cannot** be priced from
  margin at all — both teams attend the same contests, so its differential has an
  sd of 0.59 on a level of 92.1.

* **The centring cell is now the job a player did, not the slot he started in.**
  This CHANGES PUBLISHED RATINGS. `lineup_position` records where a player
  started, so every bench-starting specialist was being centred against
  benchwarmers: the `INT` cell averages 0.378 per-80 hitout against `RK`'s 5.158,
  which put Sean Darcy — rucking at an ordinary 5.44 — 4.1 standard deviations
  above his cell and 5th in the competition, while Max Gawn's ruck channel read
  negative. Three constants change together, and they are not separable:

  - `ROLE_REMAP_BENCH` (now `TRUE`) resolves a bench start to the role the player
    actually filled — season role, then career role, then listed position, with
    the count reaching each tier reported.
  - `EPV_HITOUT_CENTRE_ON_RUCK` (now `TRUE`) cells the hitout channel on ruck
    involvement rather than a position label. Listed position does not fix this:
    11 of the 44 players averaging 15+ ruck contests a game are not listed as
    rucks (Rory Lobb is a KEY_DEFENDER at 29.2 a game).
  - `EPV_RUCK_BLEND_WIDTH` (now `10`) blends the reference across the involvement
    threshold instead of switching at it, so a part-time ruck gets a part-time
    cell and there is no cliff.

  Verified two ways, because no single check can see both halves. Match gate:
  dMAE +0.0534, 95% CI [−0.4980, +0.6049] on 396 paired matches — a null, which
  is the pass for a change that reallocates credit within a team. Leaderboard:
  position mix in the top 40 identical before and after, Spearman 0.9636, Gawn's
  hitout channel −0.15 → +0.65, and the biggest fallers are exactly the
  ruck-forwards who had been credited against forwards.

## New Features

* **EPV v3: a chain-native rebuild of the credit system, behind `EPV_ENGINE`
  (default `"v2"`).** Nothing about published ratings changes — that is proven,
  not asserted: `data-raw/04-analysis/epv3_verify_v2_unchanged.R` regenerates the
  v2 arm with current code and compares all 73 columns across 56,576
  player-games, all identical.

  v2 stapled together a chain-derived part that conserves the expected-points
  swing exactly and thirty box-score weights that do not. v3 prices every event
  from `delta_epv`, via one decomposition of a kick's swing:

  ```
  delta_epv = (V_pre    - exp_pts )   disposal, to the kicker
            + (V_branch - V_pre   )   contest surprise, split zero-sum
            + (V_after  - V_branch)   subsequent play, paid by the next row
  ```

  with `V_pre = (1-p) V_att + p V_def`, so the contest term is `+p*Delta` when
  the attack retains and `-(1-p)*Delta` when the defence wins. The winner banks
  it and the loser sheds it — no share parameter, and the payout scales with the
  **surprise**, so beating a contest you were expected to lose is worth far more
  than winning a gimme.

  Contest value goes from **1.6% to 15.7%** of EPV variance and **1.3% to 19.8%**
  of EPR variance. `Delta` averages **2.06 points** per aerial contest against
  v2's flat `EPV_SPOIL_WT = 0.0737`, and 57.6% of player-games now carry a
  *negative* contest value, which a flat weight cannot express. The key-defender
  positional level closes from **−2.176 to −0.405** with no centring fix,
  because it was largely an artefact of the box weights.

  **Costs, measured rather than assumed.** Tackles leave EPV entirely — chains
  logs 0.49 `Tackle` rows per match against ~60 real ones — moving tackle
  quintile 5 down 1.06 monotonically; PSV carries them. And the match gate says
  v3 costs **0.184 MAE** (95% CI [−0.378, +0.746], not significant) because the
  contest channel adds nothing *incremental* at team level (multivariate
  t = −0.06, p = 0.954). It is redundant to recv/disp/cont_stop, not noisy.

  Channel contents formula by formula, and the naming warning that the v3 column
  names are aliases describing the v2 quantity:
  `../docs/reference/EPV-V3-CHANNELS.md`. Design and every gate:
  `../docs/plans/EPV-V3-CHAIN-NATIVE.md`.

* **`.build_epr_season()` accepts `epr_params`**, passed straight through to
  `calculate_epr_stats_batch()`. Lets an optimiser vary the aggregation
  constants without a second implementation of the logic to drift.

## Bug Fixes

* **`VERSEBUS_STRICT="0"` no longer goes strict at exactly one call site.**
  `check_vintage_alignment()` tested `nzchar(Sys.getenv("VERSEBUS_STRICT"))`
  while the other four sites tested `== "1"`, so any non-empty value — `"0"`,
  `"false"` — aborted there and stayed lenient everywhere else, despite that
  function's own roxygen claiming it matched "every other pipeline entry
  point's convention". Nothing in this repo or torpdata ever set the variable
  to anything but `"1"`, so no production run was affected; the divergence was
  live but unexercised. The parse rule now lives once in `.strict_mode()`
  (`R/load_utils.R`) and the four torp-local sites call it. `R/versebus.R`
  deliberately keeps its inline copy: that file is vendored into torpmodels and
  guarded function-by-function by `test-versebus-sync.R`, so it cannot call a
  torp-local helper until the sibling copy has one.

* **`vb_publish()` retries its post-upload verify instead of failing on a listing
  race.** Ported from panna (`39e413c`/`387ea96`/`6ddff96`), where a 6-byte mismatch on
  `predictions.parquet` resolved within 2s — a pure listing race, not corruption — and a
  longer stale window was seen the same day on two assets still mismatched after 3
  attempts. Budget is now 6 attempts / ~95s, and the final failure reports the actual byte
  deltas so a persistent mismatch (real corruption) is distinguishable from API lag. Same
  failure family as the `save_to_release()` work above, in the other publish path. Landed
  alongside torpmodels#28 so the two vendored copies of `versebus.R` stay in sync — torp's
  CI guards `vb_publish` function-by-function against torpmodels' copy.

* **A lagging listing is now resolved rather than shrugged at** (torpdata#74,
  fifth iteration). The fourth iteration made the stale-listing path warn and
  proceed, which knowingly left one hole: a genuinely short upload whose listing
  *also* lags looked identical to a lagging read of a good one. `save_to_release()`
  now asks storage directly via `.vb_asset_true_size()` — a one-byte ranged GET on
  the release **download** path, which resolves the asset by name and so cannot
  return the previous asset the way a stale listing row can. Matches what we
  wrote → confirmed, and the warning disappears entirely; genuinely short → fatal;
  unavailable → the previous warn-and-proceed, unchanged. Note `prev_rows_floor` is
  **not** a truncation backstop and never was: it compares `nrow(df)` in memory and
  aborts before the write, so it guards bad input, not a bad transfer.

* **The post-upload verify no longer reads a growing file as a truncated one**
  (torpdata#74, fourth iteration). `save_to_release()` treated any listing
  smaller than the local file as a possible truncation and aborted, which failed
  5 of 8 daily releases on 2026-08-08 — every one of them adding a new round to
  `pbp_data_2026_all.parquet`, where the season file grows and a lagging listing
  therefore serves the previous, *smaller* asset. Size direction turns out to
  carry no information in either direction, so the decision now rests entirely
  on the listing's own `updated_at`: a row stamped before our upload is a
  previous asset (retry, then warn and proceed), and a row stamped at or after
  it is our write, so short means truncation and long means a failed replace.

* **The positional level correction moved to EPV, where the gap is actually
  created.** `.position_adjust()` already centred every EPV channel to
  machine-precision zero — but by `lineup_position`, the weekly on-field role.
  That removes the role effect and leaves the player-type one: key defenders
  are a subset of the players filling full-back and centre-half-back and sit
  below those roles' own means. Measured on 2026 per-game data, `epv_adj` spanned
  2.94 points across listed buckets (key_def −2.17, key_fwd +0.77) while all 20
  lineup positions read exactly 0.

  `centre_epv_by_position()` (`EPV_LEVEL_CENTRE`) now centres the channel set EPR
  consumes on its listed bucket, TOG-weighted, per `(season, round)`. TOG
  weighting is what makes EPR's numerator vanish — EPR forms
  `sum(x * tog_safe * decay)` and decay is ~constant within a round, so zeroing
  the *unweighted* mean would look centred while EPR stayed skewed. Per-round
  grouping keeps it leak-safe.

  Measured effect on the round-20 EPR cross-section: positional spread falls
  from **1.725 to 0.420** (−76%), within-position spread and player ordering
  intact (`cor` = 0.965 before/after).

  **`EPR_POSITION_CENTRE` stays on as a backstop, not replaced.**
  `.bayesian_shrink()` pulls toward a non-zero `prior_rate` (−0.7 / −0.3) by an
  amount set by each player's `wt_gms`, so a zeroed EPV sum does not produce a
  zeroed EPR level. With both layers the residual spread is 0.0000.

  Because it runs after `adjust_epv_for_opponents()` and before both consumers,
  it also reaches `get_player_game_ratings()`, whose per-game EPV display was
  uncentred while the season rating was not.

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

## Chores

* **The GAM/XGBoost Input Blend is defined once.** The `0.5 * gam + 0.5 * xgb`
  arithmetic was written out at three call sites — `run_predictions_pipeline()`,
  `fit_match_margin_calibration()` and `build_matchup_table()` — so the
  calibration sidecar that gates what gets served, and the matchup table that
  prices finals for the blog, were each scoring a *copy* of production rather
  than production. Now `.blend_gam_xgb()` (`R/match_model.R`) with the weight in
  `MATCH_BLEND_WEIGHT` (`R/constants_match.R`). Output-neutral: `1 - 0.5` is
  exact, and a test asserts bit-identity against the literal it replaced.

* **`.build_week_ratings()` is defined once.** It was pasted verbatim into
  `match_model.R` and `matchup_table.R` — the two copies differed only in a line
  wrap — so a new EPR channel or a changed injury discount had to be edited
  twice or the blog's matchup table would silently disagree with the published
  predictions. Now one internal function in `R/match_data_prep.R`, alongside its
  lineup-based sibling `.build_team_ratings_df()`, taking `target_weeks`
  explicitly instead of capturing it from the enclosing frame.

* **Three drift guards added** (`test-shared-match-helpers.R`) so the copies
  stay gone: no `R/` file may re-parse `VERSEBUS_STRICT`, write the blend
  arithmetic inline, or define `.build_week_ratings()` a second time. Local-dev
  only, same as `test-versebus-sync.R` — `R CMD check` runs against an installed
  package with no `R/` tree beside it, so they skip there.

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
