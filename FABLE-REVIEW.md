# Torpverse Deep Review — FABLE-REVIEW.md

**Date:** 2026-07-08
**Reviewer:** Claude Fable 5 (one-shot frontier review)
**Scope:** torp, torpdata, torpmodels — architecture vs docs, code quality, caching correctness, pipeline robustness, test gaps.

## Coverage disclosure (read this first)

**Covered:**
- All verse + sub-repo CLAUDE.md / ARCHITECTURE.md files, ECOSYSTEM.md (drift audit vs code).
- Full read of torp caching/loading layer: `R/cache.R`, `R/disk_cache.R`, `R/load_utils.R`, `R/load_engines.R`, `R/local_data.R`, `R/load_data.R`, `R/utils.R`, `R/zzz.R`.
- torpmodels `R/load_model.R` (full read).
- Code-quality sweep of `torp/R/` (silent failures, O(n²), correctness, loader API design) — deep pass over ~25 files.
- Test-coverage gap analysis of `torp/tests/testthat/` (50 files) and `torpmodels/tests/` (1 file), plus CI test workflow.
- Verified findings by direct read: simulation sign inversion (simulate.R vs finals_sim.R), decay-constant doc drift, disk-cache location drift, release tags on GitHub (`gh release list`).

- Pipeline robustness pass (completed late in the session, findings in Section 7): `torp/data-raw/01-data/daily_release.R`, `release_data.R`, `03-ratings/run_ratings_pipeline.R`, `02-models/build_match_predictions.R`, `torpdata/scripts/build_blog_data.R`, all GHA workflow YAMLs in torp + torpdata, torpmodels `*_run.R` training wrappers.

**Covered in the 2026-07-10 follow-up review (Section 9):**
- torpmodels `data-raw/` training script *internals*, code-correctness only (methodology is FABLE-METHODOLOGY.md's remit): `01-ep-model/` (train, run wrapper, live v2), `02-wp-model/` (train, run wrapper, cv_ep, validate_cv_ep_wp), `03-shot-model/` (train + wrapper), `04-match-model/` (train_match_models, eval_squiggle_rank), `05-live-wp-model/` (chain v4, lookup GAM), `convert_rda_to_rds.R`.
- `afl_api.R` endpoint/auth correctness — full read, plus the `get_token()`/`access_api()` layer in `scraper.R:160-300`.
- Empirical verification of C1 — **run 2026-07-10 on the fixed code, passed** (see §9.1).
- `rebuild_everything.R` / `rebuild_all_release_data.R` internals.

**Still NOT covered:**
- torpmodels `data-raw/debug/` scratch scripts (per torpmodels CLAUDE.md: not part of the pipeline).
- Superseded/experimental live scripts (`train_ep_model_live.R` v1, `train_live_wp_chain.R`/`_v2`/`_v3`, `train_live_wp_xgb.R`) — red-flag grep skim only.
- Worker/browser-side inference consuming the live JSON exports (`inthegame-blog/worker/src/ep-model.js`, win-prob.js) — base_score/margin-accumulation correctness of the consumers is unverified.
- `torp/data-raw/06-stat-ratings/` script internals (only their orchestration from `rebuild_everything.R` Phase 7 reviewed).

Severity counts: **5 Critical, 14 High, 21 Medium, 12 Low** (52 findings; Sections 1–5 = package/tests, Section 7 = pipeline/workflows). Quick wins are tagged `[QUICK WIN]` — each is <30 min for a Sonnet-class session.

---

## 1. Critical

### C1. Regular-season simulation "hot rating" update has an inverted sign
**Files:** `C:\dev\torpverse\torp\R\simulate.R:131-133` and `:221` + `:233-236`. Compare `C:\dev\torpverse\torp\R\finals_sim.R:126-128` and `C:\dev\torpverse\torp\R\constants_sim.R:63-68`.

```r
# simulate.R:131-133 (and same logic in process_games_dt at :221/:233-236)
shifts <- SIM_RATING_SHIFT * (result_vec - estimate)
torp_vec[home_idx] <- torp_vec[home_idx] - shifts   # over-performing home team is DOWNGRADED
torp_vec[away_idx] <- torp_vec[away_idx] + shifts
```
```r
# finals_sim.R:126-128 — the opposite (Elo-correct) direction
shift <- SIM_RATING_SHIFT * (res$result - res$estimate)
state$ratings[home] <- state$ratings[home] + shift
state$ratings[away] <- state$ratings[away] - shift
```
`result - estimate > 0` = home over-performed, yet `simulate.R` *reduces* its rating. `constants_sim.R` documents `SIM_RATING_SHIFT` as an Elo-style in-season update. Effect: in every `simulate_afl_season()` run, teams that string simulated wins together get progressively downgraded during home-and-away rounds, then correctly upgraded in finals — compressing win distributions and distorting published top-4/premiership probabilities. Both regular-season sites share the inversion (consistent with each other, contradicting finals + docs), so verify empirically before flipping — but the burden of proof is on the current sign.
**Fix:** flip the signs in both `simulate_season()`'s loop and `process_games_dt()` to match `finals_sim.R`; add a regression test asserting an over-performing team's rating rises. `[QUICK WIN]` (the code change; add the test in the same PR).

### C2. `run_predictions_pipeline()` can clobber the full-season locked-predictions release with one week of data
**File:** `C:\dev\torpverse\torp\R\match_model.R:663-669` and `:728-730`.

```r
existing <- tryCatch(file_reader(pred_file_name, "predictions"),
  error = function(e) { cli::cli_warn("Could not load existing predictions ... Uploading current weeks only."); NULL })
...
} else { combined <- week_gms }
save_to_release(combined, pred_file_name, "predictions", also_csv = TRUE)
```
`file_reader()` (`R/load_data.R:142-169`) aborts on *any* download failure — a transient GitHub 5xx/timeout, not just a missing asset. The tryCatch converts that into `existing <- NULL`, and the pipeline uploads `week_gms` alone, **replacing** `predictions_2026.parquet`. All locked pre-game predictions for earlier rounds — the data the "frozen at kickoff" design exists to protect — are destroyed, unattended, in the daily GHA run. This is the read-modify-write anti-pattern on the data bus.
**Fix:** distinguish 404 from transient errors; on non-404, abort the upload step. Cheap guard: before uploading with `existing == NULL`, check `get_release_assets("predictions")` — if the asset exists, refuse to overwrite.

### C3. `psr_ratings()` ignores its `season_val` argument — returns matching rounds from *every* season
**File:** `C:\dev\torpverse\torp\R\player_ratings.R:732-749`.

```r
psr_ratings <- function(season_val = get_afl_season(type = "current"),
                        round_val = get_afl_week(type = "next"), ...) {
  skills <- load_player_stat_ratings(TRUE)   # all seasons
  ...
  if (!is.null(round_val)) result <- result[result$round == round_val, ]
```
`season_val` is accepted, documented, and never used. `psr_ratings(2024, 5)` returns round-5 rows for 2021–2026 mixed, with duplicate `player_id`s across seasons; any downstream join on `player_id` alone silently picks an arbitrary season's PSR. Same bug family as #88.
**Fix:** `result <- result[result$season == season_val, ]` before the round filter; validate `season_val`. `[QUICK WIN]`

---

## 2. High

### H1. Read-modify-write wipe hazard #2: `save_injury_data()` can erase season injury history
**File:** `C:\dev\torpverse\torp\R\injuries_schedule.R:146-158, 178`.
`existing <- tryCatch(load_injury_data(season), error = function(e) NULL)` — but `load_injury_data()` doesn't even error on network failure: `parquet_from_url()` (`R/load_engines.R:365-384`) warns and returns an **empty** data.table, so `existing` is 0-row and `combined <- injuries_df` (today's snapshot only) overwrites the accumulated season history. Worse: one 404 writes a `.skip` marker (`R/local_data.R:247-255`), making subsequent loads return empty *silently* for up to a day — guaranteeing the wipe on the next run.
**Fix:** treat 0-row `existing` as "unknown"; confirm the asset is genuinely absent via the release asset list before writing fresh; never mark a URL skippable if it's one the same pipeline upserts.

### H2. Loaders return partial data on failure, warnings only — and cache the hole
**Files:** `C:\dev\torpverse\torp\R\load_data.R:260-263, 273-287` (`.load_with_cache`), `C:\dev\torpverse\torp\R\load_engines.R:226-238, 365-384`.
Per-season fetch failures are reduced to `cli_alert_danger` (a message — not catchable as a warning, invisible in CI logs); failed seasons are dropped and the **incomplete frame is stored in the in-memory cache for 1 hour**, so every later loader call in the same pipeline run reuses the hole. `load_player_stats(2021:2026)` with one flaky season silently computes ratings with a season missing — the exact #88 "flat across history" failure class. Multi-URL parquet loads behave the same (`parquet_from_urls_parallel` warns per failed download, returns the rest).
**Fix:** use `cli_warn()`; track failed seasons/URLs; abort (or add `strict = TRUE` used by pipelines) when any requested asset fails with a non-404; never `store_in_cache()` an incomplete result.

### H3. Negative-cache `.skip` markers can suppress newly published round files for up to a day
**Files:** `C:\dev\torpverse\torp\R\local_data.R:224-237` (`is_download_skippable`), consumed at `R/load_engines.R:100-102, 273-280`.
Live-season sequence: a loader requests the current round's file *before* the daily release uploads it → 404 → `mark_download_skippable()` → the file is then skipped even after it exists, until the marker ages out (`local_max_age_for_url()` = 1 day for current-season). Signal is a `cli_inform`, not a warning. Any workspace sharing `torpdata/data/` (dev machine, or a GHA job that checks out torpdata) can silently drop the newest round from ratings/predictions input.
**Fix:** expire current-season skip markers in hours (1–3h), and/or `clear_skip_markers()` at the top of every pipeline entry point; don't mark current-round URLs skippable at all.

### H4. Disk cache serves current-season data up to 7 days stale — no season awareness
**File:** `C:\dev\torpverse\torp\R\disk_cache.R:69-88` (`is_disk_cached`, `DISK_CACHE_DEFAULT_AGE_DAYS = 7`).
Layer-1 local storage has the 1-day current-season rule (`local_data.R:94-106`); layer-2 disk cache applies a flat 7-day TTL to everything. Any loader called with `use_disk_cache = TRUE` on a machine without a `torpdata/data/` dir returns week-old current-season PBP/chains silently, mid-season. Docs (verse `ARCHITECTURE.md` §7) present the layers as equivalent-but-slower, which they are not.
**Fix:** apply `local_max_age_for_url()` (or a 1-day cap for current-season files) inside `is_disk_cached()`. `[QUICK WIN]`

### H5. `get_release_assets()` session cache never invalidates — pipeline can filter out files it just uploaded
**File:** `C:\dev\torpverse\torp\R\load_utils.R:45-70`, consumer `generate_urls()` at `:185-195`.
Asset lists are cached per tag for the whole session and treated as ground truth: requested URLs not in the cached list are **dropped** (inform only). A session that uploads a new round file (or `predictions_2026.parquet`) and later reloads it gets the pre-upload list → stale/empty data. Also breaks long-lived Shiny/blog sessions across a release refresh.
**Fix:** short TTL (minutes) on `.torp_release_cache`; invalidate the tag entry inside `save_to_release()` after successful upload. `[QUICK WIN]`

### H6. `load_predictions()` / `load_retrodictions()` default `rounds` to the *current* season's next round even for historical seasons
**File:** `C:\dev\torpverse\torp\R\load_data.R:721-724, 762-765`.
`load_predictions(2023)` gets `rounds = get_afl_week(type = "next")` computed from *today's* fixtures — in July 2026 that silently returns only round-18 rows of 2023. Off-season, `get_afl_week("next")` returns the last played round. Also inconsistent API: "all rounds" is spelled `TRUE` (`load_chains`), `NULL` (`load_xg`), and `get_afl_week("next")` (here) across the family.
**Fix:** default `rounds = TRUE` whenever any requested season ≠ current season (or always); standardise the `rounds` default across all loaders.

### H7. `.fetch_cfs_batch()` uploads partially-fetched seasons to the data bus
**File:** `C:\dev\torpverse\torp\R\afl_api.R:50-71`, consumer `update_player_stats()` at `R/load_data.R:97-126`.
Mid-batch token expiry / rate limiting drops matches with only an `cli_inform` byline ("Fetched X of Y"). `update_player_stats()` then `save_to_release()`s the partial frame, poisoning `player_stats_2026.parquet` until noticed — and the daily job happily re-uploads partial data.
**Fix:** `cli_warn()` (or abort above ~5% failure); compare fetched match count vs fixture count before `save_to_release()`.

### H8. GHA entry points live outside the tested package surface
**Files:** `C:\dev\torpverse\torp\data-raw\01-data\daily_release.R` (defines `run_daily_release()`, `has_new_games()`, `has_new_team_data()`); referenced by `torpdata/.github/workflows/*.yml` and documented as `torp::run_daily_release()` in `torpdata/ARCHITECTURE.md:67`.
These functions are sourced scripts — not exported, not installed, **not reachable by testthat**. The single most operationally important code path (daily new-game detection → release → dispatch) has zero test coverage and can't get any without refactoring. The `torp::` prefix in torpdata docs is wrong today.
**Fix:** move `has_new_games()`/`has_new_team_data()` (pure logic, injectable fixtures argument) into `R/`, export or `@keywords internal`, keep `daily_release.R` as a thin orchestrator; add the tests in T1 below.

---

## 3. Medium

### M1. Historical local files never expire — stale forever after a backfill/rebuild
**File:** `C:\dev\torpverse\torp\R\local_data.R:104` (`if (file_year < current_year) return(NULL)`).
After a historical re-release (e.g. the #92 coordinate-fix re-release, `rebuild_all_release_data.R` runs), every workspace with `torpdata/data/` keeps serving pre-fix historical parquets indefinitely; disk cache self-heals in 7 days, local storage never. There is no version/etag check anywhere.
**Fix:** document + script a `download_torp_data(overwrite = TRUE)` / clear step into rebuild scripts; longer term, compare release asset `updatedAt` (already a known pattern in `C:\dev\CLAUDE.md`) against local mtime.

### M2. `rounds = TRUE` is rewritten to `0:28`, silently dropping NA-round and >28 rows
**Files:** `C:\dev\torpverse\torp\R\load_utils.R:80-93` (`validate_rounds`), `R\load_engines.R:46-58`, also `R\load_data.R:851-853`.
After `validate_rounds(TRUE) → 0:28`, the "no-op" filter still runs: rows with `round` NA vanish (`NA %in% x` = FALSE), and any future round numbering >28 is dropped in two hardcoded places.
**Fix:** keep `TRUE` as a sentinel through to `load_from_url()`; only filter on explicit user-supplied rounds; warn on NA rounds.

### M3. `validate_model_data_quality()` crashes on all-NA numeric columns
**File:** `C:\dev\torpverse\torp\R\data_validation.R:451-458`.
`var(all-NA) == 0` → NA → `if (any(constant_cols))` throws "missing value where TRUE/FALSE needed" — the validator crashes on exactly the degenerate data it should flag. Its twin `validate_generic_data_quality()` (`:478-479`) has the `| all(is.na(x))` guard.
**Fix:** copy the guard; use `vapply(..., logical(1))` instead of `sapply`. `[QUICK WIN]`

### M4. `get_game_chains()` silently discards a match on schema drift
**File:** `C:\dev\torpverse\torp\R\scraper.R:388-390`.
`if (... || length(chain_list) <= 5) return(data.frame())` — a magic column-count heuristic. A trimmed API response drops whole matches from the chains release with zero message; `get_many_game_chains()` rbinds over the hole.
**Fix:** `cli_warn()` with match_id when triggered; test for the actions/stats column explicitly. `[QUICK WIN]`

### M5. Season/timezone inconsistency at year boundary; `get_afl_season("next")` wrong in Jan–Feb
**Files:** `C:\dev\torpverse\torp\R\utils.R:24-40` (local-tz `Sys.Date()`), `:59` (AEST), `R\load_utils.R:102`, `R\local_data.R:95`.
On UTC runners between 00:00 UTC and 10:00 AEST on Jan 1, season helpers disagree with round helpers; `"next"` returns `year + 1` even in January when next season is the current calendar year.
**Fix:** one shared AEST-based season helper used by all four call sites.

### M6. torpmodels: cached models never expire, no version check
**File:** `C:\dev\torpverse\torpmodels\R\load_model.R:135, 189`.
Once cached, a retrained/re-uploaded model is never picked up without `force_download = TRUE`. GHA runners are fine (ephemeral); local users silently run stale models — compounded by the documented XGBoost RDS version incompatibility.
**Fix:** optional `max_age_days`, or compare release asset `updatedAt` (one `gh api` call, already cached per session) against cache mtime.

### M7. torpmodels: `match_xgb_pipeline` is documented as loadable but isn't
**Files:** `C:\dev\torpverse\torpmodels\R\load_model.R:335-352` (`normalize_model_name` map — no entry), vs `torpmodels/CLAUDE.md:27` and `torpmodels/ARCHITECTURE.md:61` (both list it as a `load_torp_model()` core model). `.CORE_MODELS` (`:9-16`) also omits it while docs say "7 files".
`load_torp_model("match_xgb_pipeline")` aborts "Unknown model".
**Fix:** add the map entry + `.CORE_MODELS` row, or fix both docs. `[QUICK WIN]`

### M8. O(n²) rolling-profile loop in opponent adjustment
**File:** `C:\dev\torpverse\torp\R\opponent_adjustment.R:281-347`; same pattern in `R\ladder.R:230-236` (`cum_boost` subset-inside-vapply).
`.compute_rolling_stat_profiles()` re-scans all prior matches per match × ~18 teams × ~30 stats. Minutes of pure subsetting on full-history runs; the ladder case is a textbook data.table non-equi join.
**Fix:** pre-sort by date + maintain running decay-weighted sums per team (`exp(-λt)` factorises), or non-equi joins.

### M9. Round parsed by fixed character positions from `match_id` in two loaders
**File:** `C:\dev\torpverse\torp\R\load_data.R:451` (`load_xg`), `:627` (`load_teams`): `as.integer(substr(match_id, 12L, 13L))`.
Assumes exact `CD_M{yyyy}{comp}{round}` layout; a non-AFLM comp id or format drift yields garbage integers, not NA. `scraper.R:121-125` already has an anchored-regex extractor that returns NA on mismatch.
**Fix:** reuse the regex extractor in both loaders. `[QUICK WIN]`

### M10. `.load_player_details_with_fallback()` silently substitutes last season's rosters
**File:** `C:\dev\torpverse\torp\R\player_ratings.R:6-12`.
Empty current-season details (pre-season, API hiccup, or H2's empty-return path) → loads `season_val - 1` with no message; `.prepare_final_dataframe()` stamps `season = season_val` on previous-season team affiliations — traded players attributed to old clubs in published EPR.
**Fix:** `cli_warn()` on fallback; carry a `details_season` attribute.

### M11. Documentation drift — decay constants, cache locations, loader sources (multiple docs)
- `torp/ARCHITECTURE.md` EPR decay table (Recv 282 / Hitout 456 / Disp 573 / Spoil 577) **contradicts** `R/constants_ratings.R:22-34` (273 / 545 / 630 / 523). `torp/CLAUDE.md` matches code; ARCHITECTURE is stale.
- `torp/CLAUDE.md:109`: disk cache "at `tools::R_user_dir("torp", "cache")`" — code uses `~/.torp/cache` (`R/disk_cache.R:18`). Verse ARCHITECTURE has it right.
- `torp/CLAUDE.md:60-78` loader table lists release tags for `load_player_stats()`/`load_fixtures()`/`load_results()`/`load_teams()`, and roxygen for these says "from the torpdata repository" (`R/load_data.R:461, 534, 592, 637`) — but all four are **API-path** (they call `get_afl_*()` via `.load_with_cache`, never touching releases). Verse ARCHITECTURE §7 knows this; the per-function docs mislead.
- `torp/CLAUDE.md:111`: "Pass `force = TRUE` to most loaders" — the actual argument is `refresh`, and only 3 of ~15 loaders have it.
- `torpdata/ARCHITECTURE.md:133`: `run_daily_release()` "returns 'full', 'team_only', or 'none'" vs `torpdata/CLAUDE.md:45` "returns TRUE/FALSE" — one is wrong; also `torp::` namespace is wrong (H8).
- Release-tag inventories disagree with GitHub reality (`gh release list`, 2026-07-08: 22 tags): `torpdata/ARCHITECTURE.md` lists `ps-data` and `stat_ratings-data` (neither exists; actual `player_stat_ratings-data`, `player_skills-data`); `torpdata/CLAUDE.md` omits `injury-data`/`retrodictions`/`reference-data` and lists `stat-models` (lives on torpmodels); verse `ARCHITECTURE.md` §3 lists `ps-data` too.
- Counts: verse ARCHITECTURE "55 R files, 130+ exports" — actual 63 files, 141 exports; ECOSYSTEM.md "49 R files, 121 exports, 40 test files" — actual 63/141/50+; torpmodels ARCHITECTURE "166 cases" — 17 `test_that` blocks.
- `R/load_data.R:1037` comment claims a release-tag fallback ("fall back to old for backward compat") that is not implemented.
- `torp/CLAUDE.md:129-136` names test files (`test-add_variables.R`, `test-player_ratings.R`, `test-match_model.R`, `test-simulate.R`) that don't exist under those names; the latter two have no equivalents.
**Fix:** one doc-sync pass; each bullet is mechanical. `[QUICK WIN]` (as a batch)

### M12. Intra-day staleness of predictions/ratings via the 1-day local rule; no `refresh` on most loaders
**File:** `C:\dev\torpverse\torp\R\local_data.R:94-106` + `R\load_engines.R:108-114`.
`predictions_{yyyy}.parquet` (updated multiple times on lineup days) and `torp_ratings.parquet` (no year token → also 1-day rule) can be served up to 24h stale; `load_predictions()`/`load_torp_ratings()` expose no `refresh` bypass.
**Fix:** hours-level max-age for `predictions`/`ratings-data` tags; add `refresh` uniformly to the `load_*()` family.

### M13. AFL tie-break incomplete in `calculate_ladder()`
**File:** `C:\dev\torpverse\torp\R\ladder.R:65-72`.
Sort is `-ladder_points, -percentage` only — the third AFL tie-break (points for) documented in verse ARCHITECTURE §"Ladder Calculation" is not implemented, so exact ties rank by input row order (non-deterministic). Also `fifelse(points_against > 0, pf/pa*100, 0)`: a team that conceded 0 gets percentage 0 and sorts *last* instead of first.
**Fix:** add `-points_for` to the ordering; use `Inf`/large sentinel for zero-against percentage. `[QUICK WIN]`

### M14. Test-suite network coupling produces silently-green CI
**Files:** `C:\dev\torpverse\torp\tests\testthat\helper-test-data.R:417-437`, `test-load_torp_data.R:43`, `test-integration.R:199`, `test-cache.R`.
Suite startup downloads 8 datasets whenever online; on failure everything becomes NULL and ~25 tests silently skip → green builds with invisible coverage holes. `test-load_torp_data.R:43` and `test-integration.R:199` hit the network with no skip guard. `test-cache.R` is 100% live-API and uses the deprecated `use_cache=` arg. CI coverage job is `continue-on-error: true` with no threshold, and there is no scheduled run to catch off-season breakage of date-dependent paths.
**Fix:** commit small fixture parquets; guard all network tests; drop `continue-on-error` or add a floor; add a weekly cron run of the test workflow.

---

## 4. Low

### L1. `is_cache_valid()` throws on entries without a timestamp
`C:\dev\torpverse\torp\R\cache.R:92-99`. Add `if (is.null(cache_entry$timestamp)) return(FALSE)`. `[QUICK WIN]`

### L2. Growing vector in prediction printer
`C:\dev\torpverse\torp\R\match_model.R:936-953` — `abs_errors <- c(abs_errors, ...)` in a row loop (~9 rows; harmless, trivially vectorisable). `[QUICK WIN]`

### L3. `torp_team_abbr()`/`torp_team_full()` return NA silently for unknown teams
`C:\dev\torpverse\torp\R\afl_api.R:955-975`. `cli_warn()` listing unmatched inputs. `[QUICK WIN]`

### L4. `load_predictions()` default triggers a network fixtures fetch just to evaluate a default argument
`R/load_data.R:721` — `rounds = get_afl_week(type = "next")` in the signature; off-season this warns and returns 0. Related to H6; fold into that fix.

### L5. `.onLoad()` attaches mgcv to the user search path
`C:\dev\torpverse\torp\R\zzz.R:39-47`. Deliberate and well-commented (shot-model `Xbd` resolution); acceptable, but note the documented `Depends` alternative if CRAN submission ever happens.

### L6. `torpmodels::check_model_cache()` rbind-in-loop
`C:\dev\torpverse\torpmodels\R\load_model.R:259, 274`. Tiny n; cosmetic.

### L7. `get_afl_season()` is calendar year — pre-season loads default to an empty season
`C:\dev\torpverse\torp\R\utils.R:24-40`, acknowledged in the docstring. Jan–Feb: every defaulting loader returns empty tibbles + warnings. Fold into M5's shared helper.

### L8. `MIN_PARQUET_BYTES = 100` / `> 1000` byte validity checks are heuristics
`R/local_data.R:9`, `torpmodels/R/load_model.R:413, 440`. A GitHub HTML error page can exceed both. Low because parquet/RDS read fails loudly afterwards and corruption paths delete the file.

---

## 5. Test coverage gaps that matter (prioritized)

Context: torp has 50 test files / ~700 `test_that` blocks — good breadth on team names, clean_pbp, column maps, validators, injuries, ladder math, #88 regression on `calculate_torp`. torpmodels has 1 file / 17 tests (substantive). `_snaps/` is empty (CI `upload-snapshots` is a no-op).

| # | Sev | Gap | Suggested test |
|---|-----|-----|----------------|
| T1 | Critical | `run_daily_release()`/`has_new_games()` — zero tests, untestable as written (H8). Untested: off-season FALSE path; `"%Y-%m-%dT%H:%M"` parse (API format change → all-NA `start_utc` → FALSE forever = silent pipeline death); "started = completed" heuristic. | After H8 refactor: `test-daily-release.R` mocking `get_afl_fixtures` — (i) 0 rows → FALSE, (ii) one past-start game → TRUE, (iii) all-NA parsed times → warns, not silent FALSE. |
| T2 | Critical | `save_to_release()` (`R/load_data.R:20`) — zero tests for the function all 14 release writers flow through: 404-retry branch, CSV warn-continue, team-name normalisation pre-write. | `local_mocked_bindings(pb_upload=...)`: errors "404" once then succeeds → exactly 2 calls + warning; twice → error; written parquet has "Footscray"→"Western Bulldogs". |
| T3 | High | Cache layer has no offline tests; `test-cache.R` is all live-API + deprecated arg. Cache-key sort, TTL boundary, `refresh=TRUE` clearing both layers untested. | Offline `.load_with_cache("t", c(2022,2021), fetch_fn=mock)`: key `t_2021_2022`; fetch called 1× then 0× on hit; again after `refresh=TRUE`; again with TTL=0. |
| T4 | High | EP/WP feature engineering has no known-answer tests — every real assertion skips when the model download fails. | Mock `get_epv_preds`/`get_wp_preds` with fixed probs; assert `exp_pts = 6*(goal-opp_goal)+(behind-opp_behind)` hand-computed on a 5-row fixture; `delta_epv` lag/lead; `wpa` diffs. |
| T5 | High | Loader-layer #88 semantics untested: `validate_seasons(TRUE) == AFL_MIN_SEASON:current`, current-season defaults vs full-history `torp_ratings.parquet`. | `test-load-utils.R`: expect_equal on `validate_seasons(TRUE)`; `expect_error(validate_seasons(2020))`; pin `formals(load_psr)$seasons`. |
| T6 | High | `match_model.R` (published predictions) — zero tests, even structural (probs in [0,1], home+away sum to 1, weather median-imputation fallback). Also cover C2's non-404 abort once fixed. | Mocked-model structural test on a 2-fixture frame. |
| T7 | Med-High | Opponent adjustment (3 exports) — zero tests; a sign flip would invert strength-of-schedule silently. | Synthetic 2-team frame: strong-opponent schedule moves adjusted EPV in documented direction; neutral schedule is a no-op. |
| T8 | Med | Ladder tie-breaks: `calculate_final_ladder()` zero tests; the M13 points-for test will fail today (real bug). | Two teams tied on points+percentage, different points_for → deterministic documented order. |
| T9 | Med | Simulation determinism: `seed=` (incl. `clusterSetRNGStream`, `season_sim.R:452,628`) never asserted. Add a C1 regression test here too. | `simulate_afl_season(seed=1, n_sims=5)` twice → `expect_identical`; `n_cores=2` ≡ serial; over-performing team's rating rises. |
| T10 | Med | torpmodels fallback chain + `force_download` untested (`load_model.R:391-461`). | Mock `pb_download` to throw + `download.file` writes real RDS → loads via fallback; 10-byte file → error + deleted; `force_download=TRUE` re-downloads. |
| T11 | Med | `.read_season_disk_cache` current-season guard + corrupt-delete branch untested (`load_data.R:310-331`). | Seed `cfs_teams_<current>.parquet` → NULL; garbage past-season file → NULL + removed. |
| T12 | Low-Med | `substr(match_id,12,13)` round parsing untested (M9). | 3-row crafted-ID test each for `load_xg`/`load_teams`. |

---

## 6. Architecture summary (docs vs reality)

The layered design (loaders → cleaning → credit → ratings → match → sim; releases as data bus; 3-layer cache) **matches the code overall** — the ARCHITECTURE.md set is unusually good. The real drift is concentrated in:
1. **Numbers and inventories** (decay constants, file/export/tag counts, release-tag lists) — see M11.
2. **The boundary between "package" and "pipeline"** — the daily entry points are scripts documented as package functions (H8).
3. **Caching guarantees** — docs imply the disk cache honors the same freshness rules as local storage; it doesn't (H4), and nothing anywhere handles historical re-releases (M1).
4. **Coupling risks**: torp→torpdata is entirely convention-based (URL/filename patterns, `substr` match-id parsing, column maps); the schema declarations in `column_schema.R` are the right instrument — extend them to round/match-id parsing (M9) and release-tag inventories. torpmodels is cleanly decoupled (one-directional), with the stale-cache caveat (M6) and one doc-phantom model (M7).

## 7. Pipeline & workflow robustness (data-raw scripts + GHA YAMLs)

### Critical

**P1. "Load existing → NULL on error → overwrite release with new-only" — transient read failures become permanent data loss (4 sites)**
- `C:\dev\torpverse\torp\data-raw\01-data\daily_release.R:204-209` (chains `_all`) and `:281-286` (pbp `_all`): `existing <- tryCatch(file_reader(...), error = function(e) { cli::cli_inform("No existing _all file ... creating fresh"); NULL })` then `combined <- rbindlist(list(existing, new))` → `save_to_release()`. A transient `pb_download` failure is indistinguishable from "file doesn't exist".
- `C:\dev\torpverse\torp\R\match_model.R:663-669, 728-730`: locked predictions (same as C2 above).
- `C:\dev\torpverse\torp\data-raw\03-ratings\run_ratings_pipeline.R:337-343, 363-364, 387`: incremental path — `load_torp_ratings()` error → NULL → `torp_df_total <- torp_new` (current season only) → overwrite of full-history `ratings-data`. The row-count-decrease guard at `:359-361` only runs when `existing` loaded successfully — the one case that needs it is unguarded.

Scenario: round 15, Saturday 10:30 PM run hits a 30-second GitHub blip reading `pbp_data_2026_all.parquet` → uploads a "fresh" file containing only round 15 → next ratings run rebuilds `player_game_2026` from one round → season ratings collapse; subsequent runs only re-merge the current round so the hole never heals without a manual `rebuild_all_release_data.R`.
**Fix:** `file_reader()` must distinguish 404 from other errors and abort on non-404; add a floor guard before every accumulating upload: `if (!is.null(existing) && nrow(combined) < 0.9 * nrow(existing)) cli_abort(...)` — apply to chains/pbp `_all`, `predictions_*`, `torp_ratings`.

**P2. piggyback delete-then-upload + `cancel-in-progress: true` + 45-min timeout can leave an asset deleted but never re-uploaded**
`C:\dev\torpverse\torpdata\.github\workflows\daily-data-release.yml:47-49` (`concurrency: group: daily-release / cancel-in-progress: true`, `timeout-minutes: 45` at `:58`) and `C:\dev\torpverse\torp\.github\workflows\daily-ratings-predictions.yml:25-27` (same, during the multi-MB `match_gams.rds` upload at `match_model.R:804`). `pb_upload` deletes the existing asset then uploads; a cancellation/timeout in that window removes e.g. `pbp_data_2026_all.parquet` from the release entirely → next run 404s → P1's "creating fresh" path fires.
**Fix:** `cancel-in-progress: false` (queue, don't kill) on both release-writing workflows; optionally upload-to-temp-name-then-rename or post-run asset existence verification.

**P3. Chains/PBP processing failure is fully silent — workflow goes green, dispatch still fires, hole becomes permanent**
`daily_release.R:875-876` calls `update_season_chains()`/`update_season_pbp()` bare — not tracked in `seasonal_failures`; internal errors collapse to NULL (`:189-194`, `:268-271`) and the run still returns `"full"`. The verify step (`daily-data-release.yml:172-178`) passes if **any one** of pbp/chains/player_stats is fresh. Scenario: AFL API changes a chains field mid-round → `clean_pbp()` errors every run → chains still upload → verified → ratings dispatched nightly against PBP missing the whole round → once `get_afl_week()` flips, `update_season_pbp(season, current_round)` never re-fetches the missed round. No issue is ever opened because no step fails.
**Fix:** verify step must require *all present* critical files fresh; propagate chains/pbp failures into `seasonal_failures` + step exit code; add round-coverage check (expected vs distinct rounds in `_all`) before dispatch.

### High

**P4. `has_new_games()` detects "started", not "completed since last release"** — `daily_release.R:61-64` (`start_utc < utc_now`). (a) Returns TRUE all week in-season → every cron does a full re-release+dispatch; (b) an in-progress game triggers a full release — chains are safe (`scraper.R:236-238` filters `status == "CONCLUDED"`) but `update_results`/`update_player_stats`/`update_xg_data` scrape mid-game partials into releases; (c) makes `release_mode = "team_only"` near-dead code (`:855-857`), so lineup-predictions runs full releases too. **Fix:** diff `CONCLUDED` match_ids against match_ids already in the released results parquet.

**P5. `lineup-predictions.yml` has no `concurrency:` block** (`C:\dev\torpverse\torpdata\.github\workflows\lineup-predictions.yml:5-11`) — overlaps itself and the daily workflow's Thursday cron; with P4, two full `run_daily_release()` runs race delete/upload on the same ~10 assets, and both dispatch `ratings-trigger` (which torp's `cancel-in-progress: true` turns into a mid-upload kill → P2). **Fix:** shared `concurrency: { group: daily-release, cancel-in-progress: false }`. `[QUICK WIN]`

**P6. The "404 retry" in `save_to_release()` is only half-correct** (`R/load_data.R:41-59`): piggyback memoises asset listings (~600 s default `piggyback_cache_duration`) so the immediate retry can reuse the same stale asset id; concurrent creates surface as 422 (missed by the `grepl("404")` match); any message containing "404" also matches. **Fix:** `Sys.setenv(piggyback_cache_duration = 1)` in pipeline scripts, exponential backoff, match HTTP status classes (404 + 422). `[QUICK WIN]`

**P7. Predictions pipeline runs twice per workflow run** — `build_match_predictions.R:15-18` executes `run_predictions_pipeline()` at top level and `daily-ratings-predictions.yml:156-162` sources the script *then* calls `run_predictions_pipeline(week = week)`. Double GAM training, double uploads (doubled race windows), and pre-season the script's line 18 errors on the NULL early-return (`match_model.R:444-449`) before the intended call. **Fix:** guard the script body with `if (sys.nframe() == 0)` (pattern already used at `daily_release.R:983`). `[QUICK WIN]`

**P8. Manual `seasons` input on the ratings workflow silently wipes full-history `torp_ratings`** — `daily-ratings-predictions.yml:86` sets `REBUILD_ALL_RATINGS <- !is.null(SEASONS)`; with `seasons: "2026"`, `run_ratings_pipeline.R:366-367, 387` overwrites `ratings-data` with 2026 only — 2021–2025 vanish from the blog. Same family as #88. **Fix:** partial `SEASONS` must force the upsert path; refuse overwrite when `nrow(new) < nrow(existing)`.

**P9. Round-boundary lag: Thursday-night games are never released Thursday night** — `R/utils.R:83-86,104` uses fixture *date* `< current_day`, so a Thu 7:20 PM game isn't "past" until Friday; the Thu 10:30/11:30 PM crons (`daily-data-release.yml:9-10`) compute last week's round and dispatch ratings without the game that just finished; data lands at the 2 AM safety net. **Fix:** derive current round from max `CONCLUDED` round, or compare timestamps not dates.

### Medium

**P10. AFL API outage is indistinguishable from off-season** — `daily_release.R:73-77`: `has_new_games()` error → FALSE → green "No New Data" summary (`daily-data-release.yml:251-261`); a week of auth breakage in-season freezes everything with zero notifications. **Fix:** third return state ("check errored") that fails/annotates the step.

**P11. `seasonal_failures`/`derived_failures` never fail the workflow** — `daily_release.R:909-946` logs them; step still writes `release_done=true`, exits 0. **Fix:** emit `failures=` to `GITHUB_OUTPUT`; fail or auto-issue when non-empty. `[QUICK WIN]`

**P12. Stage 2 → Stage 3 release-propagation race in the ratings pipeline** — `run_ratings_pipeline.R:151-156` uploads `player_game_*` then immediately `clear_all_cache(); load_player_game_data(TRUE)`; torpdata's own workflow documents 10–30 s list-endpoint lag and sleeps 30 s before dispatch, but this in-process read has no wait — Stage 3 can rate off the *previous* upload; the row-count warning at `:389-392` doesn't fail anything. **Fix:** sleep + verify asset `updatedAt` before Stage 3; escalate mismatch to error in CI.

**P13. R2 upload is not atomic** — `build-blog-data.yml:276-283` per-file loop with mid-loop `exit 1`: blog can serve mixed old/new datasets (ratings new, predictions stale) with no rollback/manifest. Also `blog-trigger` only fires on `build-predictions` success (`daily-ratings-predictions.yml:182-188`), so a predictions failure leaves R2 stale even when ratings updated. **Fix:** staging prefix + manifest-last upload; dispatch blog build from the ratings job too.

**P14. Stage 5 breaks when `REBUILD_PLAYER_GAME = FALSE`** — `run_ratings_pipeline.R:460` references `all_pstats`, defined only inside the Stage 2 block (`:105`); documented config yields per-season "object not found" failures caught cryptically at `:521-523`. CI always sets TRUE so it hides until a manual run. **Fix:** hoist the batch loads or lazy-load in Stage 5. `[QUICK WIN]`

**P15. `build_blog_data.R` game-stats uses a strict `select()` of ~35 bare columns** (`torpdata/scripts/build_blog_data.R:399-421`): one missing column (schema drift) nulls `game_stats` entirely via the tryCatch at `:423` — R2 `game-stats.parquet` silently stays stale (it's "optional" at `build-blog-data.yml:234`). Retrodictions branch (`:118`) similarly returns NULL on a future format change. **Fix:** `any_of()` + explicit missing-column report; promote staleness to a checked output when inputs existed.

**P16. Cross-repo version skew: CI runs torp `main`; models trained from a local dev checkout** — torpdata workflows check out torp with no `ref` (`daily-data-release.yml:68-73`, `build-blog-data.yml:18-22`) while torpmodels wrappers hardcode `devtools::load_all("C:/dev/torpverse/torp")` (`torpmodels/data-raw/01-ep-model/train_ep_model_run.R:12`, `02-wp-model/train_wp_model_run.R:9`, `03-shot-model/train_shot_model_run.R:7`) and `pb_upload` to `core-models` with no version metadata. A reordered/renamed feature in dev torp → model trained on dev features, scored by main features → silently wrong EP/WP everywhere. **Fix:** embed `packageVersion("torp")` + git SHA as an attr in the RDS and assert at load in `load_torp_model()`; record the torp SHA in workflow summaries.

### Low

**P17. NA-round rows silently dropped in chains dedupe** — `daily_release.R:222`: `existing[existing[[round_col]] != round, ]` drops NA-round rows in data.table. Fix: `is.na(round_number) | round_number != round`. `[QUICK WIN]`

**P18. Duplicate `update_player_stats()`/`update_teams()` implementations** — package (`torp/R/load_data.R:97-126`, `torp_clean_names()`) vs script (`daily_release.R:394-423`, `janitor::clean_names()`); `run_ratings_pipeline.R:31-36` deliberately uses the script copies. Name-cleaning drift between the two feeds P15. Fix: delete script copies, reuse package versions.

**P19. Stale inactive template** — `torp/.github/workflows/pre-game-data-update.yml.template` pins checkout@v4 / R 4.3.2 and predates `release_mode` plumbing; enabling as-is reintroduces solved problems. Fix or delete. `[QUICK WIN]`

**P20. `build-blog-data.yml:145-158` fetches torpmodels assets with `github.token`** — works only while torpmodels is public; if privated, shot-model download degrades to a `::warning::` and `player-finishing.parquet` silently stops updating. Use `WORKFLOW_PAT`. `[QUICK WIN]`

### Overall idempotency posture

The core write pattern — re-fetch the whole current round, drop that round from the `_all` file, merge, re-upload — is idempotent *within a round*, and because P4 makes `has_new_games()` return TRUE all week in-season, most half-finished runs are healed by the next cron. `GITHUB_OUTPUT` plumbing is correct, the verify step checks asset-level `updatedAt`, and the 30 s propagation sleep before dispatch shows operational learning. But the self-healing is accidental, not designed: nothing records what was last successfully released, so the system cannot distinguish "nothing new" / "API down" / "release half-done", and every accumulating file is one transient `pb_download` failure or one mid-upload cancellation away from being overwritten by a fresh-start subset — no minimum-row guard on writes, no backfill once the round pointer advances. Highest-leverage trio: (1) P1's 404-vs-transient distinction + row-count floor; (2) P2/P5's `cancel-in-progress: false` + shared concurrency group; (3) P3's all-files-fresh verify gate + surfacing swallowed chains/pbp failures.

---

## 8. Quick-win checklist (each <30 min, safe for a cheaper model)

1. C3 — season filter in `psr_ratings()` (`player_ratings.R:732-749`).
2. C1 — sign flip in `simulate.R:131-133` + `:233-236` (verify vs finals first; add T9 regression test).
3. H4 — season-aware max-age in `is_disk_cached()` (`disk_cache.R:69`).
4. H5 — invalidate `.torp_release_cache[tag]` in `save_to_release()`; add TTL.
5. M3 — all-NA guard in `validate_model_data_quality()` (`data_validation.R:451`).
6. M4 — warn-with-match-id in `get_game_chains()` (`scraper.R:388`).
7. M7 — add `match_xgb_pipeline` to `normalize_model_name()` + `.CORE_MODELS`.
8. M9 — reuse regex round extractor in `load_xg`/`load_teams`.
9. M13 — add points-for tie-break + zero-against percentage sentinel in `ladder.R`.
10. L1, L2, L3 — one-liners (cache timestamp guard, vectorise abs_errors, warn on unknown team).
11. M11 — batch doc-sync pass (decay table, cache path, loader-source claims, release-tag lists, `run_daily_release` return type, test-file names).

Additional pipeline quick wins: P5 (concurrency block on lineup-predictions), P6 (piggyback cache duration + backoff), P7 (`sys.nframe()` guard on build_match_predictions.R), P11 (fail on non-empty failures list), P14 (hoist `all_pstats`), P17 (NA-round dedupe), P19 (fix/delete stale template), P20 (WORKFLOW_PAT for torpmodels fetch). Plus P2's `cancel-in-progress: false` — a 2-line YAML change with Critical-level payoff.

Deeper refactors (do NOT hand to a cheap model without design review): C2/P1/H1 (read-modify-write protocol + row-count floors for all release upserts), P3 (verify-gate redesign + failure propagation), P4/P9 (release-state-based new-game detection), H2 (strict-mode loader failure semantics), H3 (skip-marker policy), H6 (loader API standardisation), H8 (move daily-release logic into `R/`), P13 (atomic R2 publish via manifest), P16 (model version stamping), M8 (O(n²) → running sums/non-equi joins), M14/T1-T6 (test infrastructure with mocked fixtures).

---

## 9. Follow-up review (2026-07-10)

**Reviewer:** Claude Fable 5. **Scope:** exactly the four items deferred by the 2026-07-08 review (see updated coverage disclosure). Training scripts were reviewed for *code correctness only* — data-handling bugs, silent failures, wrong joins, stale-cache traps — not modelling design (that's FABLE-METHODOLOGY.md). Findings numbered F1+ to avoid colliding with Sections 1-7.

Severity counts for this section: **0 Critical, 4 High, 6 Medium, 10 Low** (20 findings), plus one empirical verification closing C1.

### 9.1 C1 empirical verification — PASSED (on the fixed code)

C1's sign inversion was fixed after the first review in commits `b2fa0d8` (simulate_season) and `6376abe` (process_games_dt), and regression tests now exist at `C:\dev\torpverse\torp\tests\testthat\test-sim-helpers.R:115-176`. I independently ran a synthetic, network-free experiment (R 4.5.1, `devtools::load_all` of dev torp, 2026-07-10):

1. **Deterministic `process_games_dt`**: teams A/B at torp 0, `injury_sd = 0`, pre-set `result = 60` (home blowout vs estimate = `SIM_HOME_ADVANTAGE` = 6) → A `+5.346`, B `-5.346`. Over-performer **rises**. ✔
2. **`simulate_season` single game** (seed 42): simulated result 44, estimate 6.00 → Δ(A) = `+3.762`; `sign(Δ) == sign(result − estimate)`. ✔
3. **50 rounds × 4 teams** (seed 7): correlation between each team's cumulative over-performance `Σ(result − estimate)` and final rating = **0.994**. ✔

Conclusion: regular-season and finals simulation now share the Elo-correct update direction. C1 can be considered closed. (Experiment script: 3 synthetic data.tables + the three assertions above; the committed regression tests in `test-sim-helpers.R` supersede it.)

Also verified fixed in passing (quick-win commit `9bce523` and neighbours): H5 (`save_to_release()` now calls `invalidate_release_cache()`, `R/load_data.R:64`), skip markers now 404-only (`R/load_engines.R:227-234, 304-308`), P6 retry now matches 404|422 (`R/load_data.R:46`), L3 (`torp_team_abbr`/`torp_team_full` now warn, `R/afl_api.R:958-961, 980-983`).

### 9.2 High

**F1. `rebuild_everything.R` trains the WP model with a stale 15-length monotone-constraint vector against 18 features — silently mis-constrained (empirically confirmed)**
**Files:** `C:\dev\torpverse\torp\data-raw\rebuild_everything.R:470` (`"(0,0,0,1,1,1,0,1,0,0,0,0,0,0,0)"`, 15 entries) vs `C:\dev\torpverse\torp\R\clean_features.R:252-261` (`select_wp_model_vars()` returns **18** columns) and the canonical scripts' 18-length vector (`torpmodels/data-raw/02-wp-model/train_wp_model.R:53`, `train_wp_model_cv_ep.R:154`).
I tested this locally: **xgboost 3.2.1 trains without error or warning** when `monotone_constraints` is shorter than the feature count. In `select_wp_model_vars()` order, the rebuild's `+1`s land on `home` (pos 4), `points_diff` (5), `xpoints_diff` (6) and `time_left_scaler` (8) — while the intended monotone features `pos_lead_prob` (7), `diff_time_ratio` (9), `score_urgency` (10) are unconstrained. The rebuild also uses `eta = 0.1` vs the canonical `0.025`, `na.action = na.pass` in `model.matrix` (`:388, :474`) where the canonical scripts omit it, and a WP prep pipeline that inserts `clean_shots_data() |> add_shot_vars()` (`:458-462`) where `train_wp_model.R:41-43` does not. A `--models-only` rebuild therefore uploads a *differently-specified* `wp_model.rds` than the torpmodels scripts produce, with wrong monotonicity, silently.
**Fix:** delete the inlined Phase 4a/4b training and `source()` the canonical torpmodels scripts (as Phase 4d already does), or generate the constraint string programmatically from `names(select_wp_model_vars(...))`. Never hand-maintain a positional constraint vector in two places.

**F2. `rebuild_everything.R` "nuclear option" silently starts at Phase 6 — documented usage never rescrapes or retrains**
**File:** `C:\dev\torpverse\torp\data-raw\rebuild_everything.R:79` (`start_from <- 6L`), `:86-90`, header usage `:9-11`.
Non-interactively, `Rscript rebuild_everything.R 2021 2025` (the documented full-rebuild invocation) runs Phases 6-10 only: `start_from = 6` force-sets `skip_api/skip_chains/skip_pbp/skip_models` (`:140-143`). Interactively the prompt displays `"Start from phase [1]: "` but pressing Enter keeps 6 — the shown default is a lie. Either way the operator believes they re-scraped and retrained; in fact derived data was rebuilt from the *existing* releases. Deceptive-success failure mode.
**Fix:** default `start_from <- 1L`, or print a loud banner of skipped phases and make the prompt default honest. `[QUICK WIN]`

**F3. `train_shot_model.R` uploads the shot model but not its companion `shot_player_df` — released pair goes out of sync**
**File:** `C:\dev\torpverse\torpmodels\data-raw\03-shot-model\train_shot_model.R:88-102` (saves `shot_player_df.rds` locally at `:89-90`; the `pb_upload` block at `:94-102` uploads only `shot_ocat_mdl.rds`). The `_run.R` wrapper sources this script, so it inherits the gap. `rebuild_everything.R:568-574` uploads both — so the pair is only coherent when retrained via the rebuild path.
The GAM's `player_id_shot` random-effect levels and the released name-mapping (`load_torp_model("shot_player_df")`) drift apart after any retrain via the torpmodels script: xG/shot attributions join against the wrong or missing player set with no error.
**Fix:** add a second `pb_upload(player_path, ...)` in the same `if` block. `[QUICK WIN]`

**F4. Three EP/WP training entry points, three different training windows, one artifact name — prod model provenance is unknowable**
**Files:** `torpmodels/data-raw/01-ep-model/train_ep_model.R:30` (`load_chains(TRUE, TRUE)` — all seasons *including the in-progress one*), `train_ep_model_run.R:19` (`load_chains(2021:2025)` — hardcoded, already excludes 2026 and will silently rot), `torp/data-raw/rebuild_everything.R:180-184` (`2021:(current-1)`). Same for WP (`train_wp_model.R:29` vs `train_wp_model_run.R:16`). All save to the same `ep_model.rds`/`wp_model.rds` on `core-models` with zero metadata.
Extends P16: not just torp-version skew, but *training-window* skew — whichever script ran last defines production EP/WP, and nothing records which one it was. The wrapper's hardcoded `2021:2025` is the worst offender: torpmodels CLAUDE.md recommends the wrappers, so following the docs trains on a frozen window forever.
**Fix:** single parameterised training function (window as argument, default `2021:(current-1)`); stamp `attr(model, "torp_meta") <- list(seasons, torp_sha, trained_at)` before `saveRDS` and print it at load (same mechanism as P16's fix).

### 9.3 Medium

**F5. `rebuild_everything.R` Phase 4d's injected config is dead on arrival — `train_match_models.R` clobbers `UPLOAD_TO_GITHUB` and never reads `HOLDOUT_SEASON`**
**Files:** `C:\dev\torpverse\torp\data-raw\rebuild_everything.R:593-598` injects `match_env$HOLDOUT_SEASON <- Inf; match_env$UPLOAD_TO_GITHUB <- TRUE` then sources the script in `match_env` — but `torpmodels/data-raw/04-match-model/train_match_models.R:14` unconditionally reassigns `UPLOAD_TO_GITHUB <- FALSE` (evaluated in `match_env`, overwriting the injection), and `HOLDOUT_SEASON` appears nowhere in the script (it was replaced by `TEST_SEASONS <- 2025:2026` at `:13`).
Net effect: the rebuild's match-GAM phase trains, evaluates ~48 rolling rounds (slow), saves locally, and **never uploads** — while reporting success. Mitigated because the daily predictions pipeline retrains and uploads `match_gams.rds` anyway, but the rebuild's stated intent silently fails, and torpmodels `CLAUDE.md:32` documents a `HOLDOUT_SEASON` knob that no longer exists.
**Fix:** in the script, guard the default: `if (!exists("UPLOAD_TO_GITHUB", inherits = FALSE)) UPLOAD_TO_GITHUB <- FALSE` (same for `TEST_SEASONS`); update both docs. `[QUICK WIN]`

**F6. `train_ep_model_live_v2.R` runs brittle hardcoded diagnostics *before* saving — a failed comparison discards 30+ min of training**
**File:** `C:\dev\torpverse\torpmodels\data-raw\01-ep-model\train_ep_model_live_v2.R:139` (`dt[season == 2026 & round_number == 6 & grepl("Carl", home_team_name)][1, match_id]` — one specific Carlton match), `:157-159` (`readRDS` of a hardcoded absolute path to a local `ep_model.rds`), vs the save/JSON-export at `:210-233`.
Section 7 (Daicos comparison) executes before Section 8 (persist + export). If that match is absent from chains, the local full model file is missing, or `cor()` hits an empty subset, the script errors *after* both CV runs and *before* anything is written — the trained live model is lost. The exported artifact is what the Cloudflare Worker consumes, so a blocked export path means a silent failure to refresh live EP.
**Fix:** move Section 8 before Section 7, or wrap the diagnostics in `tryCatch` + existence guards.

**F7. `eval_squiggle_rank.R` mixes 2025 rolling predictions into a 2026 Squiggle leaderboard — rankings not comparable**
**File:** `C:\dev\torpverse\torpmodels\data-raw\04-match-model\eval_squiggle_rank.R:80-96`. `train_match_models.R` produces `gam_preds` etc. for `TEST_SEASONS <- 2025:2026`, but `.rolling_to_sq()` keeps only `(source, round, hteam_norm, ...)` — no season column, no filter to `TEST_YEAR`. `build_board()` filters by round only (`:121`), so torp variants' `n` and summed `bits_total` include 2025 rounds 1-10 while every Squiggle source has 2026 rows only — roughly doubling the torp sample and making the bits ranking (a sum, not a mean) meaningless. Also `:151-155` hardcodes buckets to R1-10, silently discarding later rounds as the season progresses.
**Fix:** `filter(season == TEST_YEAR)` in `.rolling_to_sq()`; derive the round cap from `max(.done_games$round)`.

**F8. Rebuild chains phase uploads `_all` files with silently missing rounds — full-rebuild variant of P1**
**Files:** `C:\dev\torpverse\torp\data-raw\01-data\rebuild_all_release_data.R:181-200` and `rebuild_everything.R:290-307`. A per-round `get_match_chains()` failure is downgraded to `cli_warn` + `NULL`, then the season's `chains_data_{season}_all` is uploaded anyway — **replacing** the previous complete release asset with one missing that round. Unlike the daily pipeline (which only merges the current round), the rebuild overwrites whole-season history, so one 30-second API blip during a backfill permanently drops a round from chains (and then PBP, player_game, ratings) until someone notices and re-runs.
**Fix:** count distinct rounds fetched vs requested and abort the season's upload on a shortfall (reuse the row-count-floor guard proposed in P1); at minimum compare `nrow(chains)` against the existing release asset before overwrite.

**F9. `.afl_all_comp_seasons()` falls back to "first competition" without warning — one schema drift poisons every season-ID lookup for the session**
**File:** `C:\dev\torpverse\torp\R\afl_api.R:324-328`. If the competitions response loses its `code` column (or the codes change), `comps[1, ]` is silently selected — which may be AFLW or any other competition — and the result is cached for the session (`:356`). Every `get_afl_fixtures()`/`get_afl_player_details()` call then resolves season IDs against the wrong competition, producing plausible-but-wrong data rather than an error.
**Fix:** `cli_warn` naming the fallback competition; verify `grepl("AFL", aflm$name[1])` before accepting; don't cache a fallback result. `[QUICK WIN]`

**F10. `score_ep()` in the live-EP script reshapes predictions with `byrow = FALSE` — scrambles class probabilities under pre-3.x xgboost**
**File:** `C:\dev\torpverse\torpmodels\data-raw\01-ep-model\train_ep_model_live_v2.R:144-148`: `matrix(p, ncol = 5, byrow = FALSE)`. Correct only when `predict()` already returns a matrix (xgboost ≥ 3.x; refilling column-major reproduces it). Under older xgboost the return is a flat **row-major** vector and `byrow = FALSE` interleaves classes across rows. Every other site handles this correctly (`is.matrix()` guard + `byrow = TRUE`: `rebuild_everything.R:442-446`, `train_wp_model_cv_ep.R:114-118`, `validate_cv_ep_wp.R:69-73`). Affects only the printed v1/v2/full comparison, not the exported model — but that comparison is the evidence used to promote v2.
**Fix:** copy the `is.matrix()` guard. `[QUICK WIN]`

### 9.4 Low

**F11.** `.post_process_squad_result()` duplicate-name repair is order-dependent — `C:\dev\torpverse\torp\R\afl_api.R:132-138`. When a bare column (e.g. `foo`) precedes its `player.foo` sibling, both end up named `foo` (the `new_names != names(result)` test is FALSE for the unchanged later duplicate) and `tibble::as_tibble()` at `:171` aborts on `check_unique`. Repair with `make.unique()` or `vctrs::vec_as_names(..., repair = "unique")` instead.

**F12.** `get_afl_lineups()` parses season by fixed position — `afl_api.R:776`: `as.numeric(substr(result$providerId, 5, 8))`. M9 family, new site missed by the first review; reuse the anchored-regex extractor. `[QUICK WIN]`

**F13.** `.parse_match_stats()` drops flatten-induced duplicate columns keeping the *first* occurrence, silently — `afl_api.R:471-475`. If the wanted variant sorts second (e.g. `teamId` at player vs team level), the wrong one wins with no message.

**F14.** `.parse_match_roster()` distinguishes "position groups" from "player rows" via `nrow(positions) <= 10` — `afl_api.R:386`. A future 11-group response silently flips the parse mode. Test for the nested `players` list-column alone, warn when ambiguous.

**F15.** `rebuild_all_release_data.R` PSR coefficient fallback path omits `torp_root` — `:312-316`: `file.path("data-raw", "cache-stat-ratings", "psr_coefficients.csv")` resolves against cwd, so running from `torpverse/` (the documented invocation) hits the "not found - skipping PSR" branch even when the file exists. Prefix with `torp_root`. `[QUICK WIN]`

**F16.** `rebuild_everything.R` `--training-seasons` parsing grabs *all* subsequent non-flag args — `:173-184`. Positional season args placed after the flag are swallowed into the training range; a bare flag with no values yields `NA:NA` → crash at `:184`. Take exactly two values and validate.

**F17.** `safe_run()`'s lazily-evaluated `expr` writes phase objects into the global env — `rebuild_everything.R:208-220`. Phase 4b (`:435-462`) consumes `model_data_epv`/`ep_model` created by Phase 4a's block; in an interactive session with leftovers from a previous run, a failed Phase 4a lets 4b silently train WP against a *stale* EP model instead of erroring. Guard 4b with a freshness check (e.g. `exists(..., inherits = FALSE)` on a sentinel set by 4a's success path).

**F18.** `train_live_wp_model.R` output dir assumes cwd = script dir — `:149`: `file.path(dirname(dirname(getwd())), "inst", "models", "core")`. Run from the torpmodels root (the convention every sibling script assumes via `file.path(getwd(), "inst", ...)`), it silently creates and writes to `C:\dev\inst\models\core`. The blog copy at `:200-202` still succeeds, masking the misplacement. Standardise output-path resolution across the training scripts. `[QUICK WIN]`

**F19.** `get_afl_results()` score-based fallback counts in-progress games as completed — `afl_api.R:679-688`: when `status` is absent, `home_score > 0` admits live games (and excludes a genuinely scoreless concluded side). Fallback-only path; add a `utcStartTime`-based guard or warn louder.

**F20.** `train_live_wp_model.R` infers draws from the last PBP row — `:52-67`: `last(points_diff) == 0` per match relabels *all* rows of that match as `label_wp = 0.5`. `points_diff` is possession-POV and the final recorded chain action isn't guaranteed to reflect the final score, so a match whose last action happened at level scores is mislabeled wholesale. Derive draws from `home_score == away_score` (already required columns in the v4 script) instead.

### 9.5 afl_api.R endpoint/auth verification (no finding — recorded for coverage)

`get_token()` (`scraper.R:174-189`): unauthenticated POST to `{CFS}/WMCTok`, 5-min session cache, aborts loudly on HTTP error or empty token — correct. `access_api()` and both curl-pool paths send the same `x-media-mis-token` header; the public `aflapi.afl.com.au/afl/v2/` endpoints (`squads`, `competitions`, `compseasons`, `matches`) correctly send *no* auth header. Endpoint templates verified mutually consistent: `fixturesAndResults/season/CD_S{yyyy}014/round/CD_R{yyyy}014{rr}`, `matchRoster/full/{match_id}`, `playerStats/match/{match_id}`, `squads?teamId={id}&compSeasonId={id}`. The hardcoded `014` comp code (AFLM) is the same convention risk as M9. Batch fetches acquire one token per batch (fine at ~200 requests/batch; token expiry mid-batch degrades into H7's silent-partial problem, already filed). No secrets in code.

### 9.6 Follow-up quick-win checklist

1. F2 — honest `start_from` default/prompt in `rebuild_everything.R:79-90`.
2. F3 — upload `shot_player_df.rds` in `train_shot_model.R:94-102`.
3. F5 — `exists()` guards for `UPLOAD_TO_GITHUB`/`TEST_SEASONS` in `train_match_models.R:13-14`; fix `HOLDOUT_SEASON` references in rebuild + both CLAUDE.mds.
4. F9 — warn + verify on the `comps[1, ]` fallback (`afl_api.R:324-328`).
5. F10 — `is.matrix()` guard in `score_ep()` (`train_ep_model_live_v2.R:144-148`).
6. F12 — regex season extractor in `get_afl_lineups()` (`afl_api.R:776`).
7. F15 — `torp_root` prefix on the PSR fallback path (`rebuild_all_release_data.R:312-316`).
8. F18 — standardise output dir in `train_live_wp_model.R:149`.

Deeper work (design review first): F1/F4 (single-source training definitions + model metadata stamping — fold into P16's fix), F6 (persist-before-diagnose restructure), F7 (season-aware Squiggle eval), F8 (round-coverage floor for rebuild uploads — fold into P1's guard), F17 (phase-dependency contract in `rebuild_everything.R`).
