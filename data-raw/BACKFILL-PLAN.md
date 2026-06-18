# Backfill Plan — corrected shot coordinates everywhere (#92)

**Status:** ✅ **COMPLETE — 2026-06-18.** All stages executed and verified end to end:
Stage 0 (clean_pbp orientation) → Stage 1 (re-scrape chains + rebuild PBP, 0–1
mis-oriented shots/season) → Stage 2 (retrain EP/WP/shot, verified better on 2026
holdout, published) → Stage 3 (rebuild PBP with new models + recompute ratings/
predictions, aggregate-stable) → Stage 4 (build-blog-data → R2 refreshed). Along the
way: removed the redundant sign-flip cascade (steps D/E/F) and the EPV `mirror`
transform, made the chains archive field-complete, and documented the AFL API
(`AFL-API-REFERENCE.md`). Incumbent models backed up at `/c/tmp_model_backup`.
**Created:** 2026-06-18.

## Why

The #92 chain-coordinate fix (`scraper.R` captures chain-level `teamId` → `clean_pbp`
orients off the possessing team) is **back-compat by design**: historical chains lack
`chain_team_id`, so they take the fallback path and reproduce the old buggy `x`. So every
existing release + the EP/WP/shot models trained on them are still on *uncorrected*
coordinates. The blog's `add_shot_geometry_variables` band-aid (PR #91) covers legacy
data only at display time.

**Doing it right = re-scrape all chains with the fixed scraper, rebuild PBP, retrain
EP→WP→shot on corrected data, and backfill ratings/predictions.** This touches the
foundational data the entire stack sits on — the biggest blast radius in the system.

The same re-scrape also captures the API fields recovered in the field audit
(`homeTeamId`, `awayTeamId`, `chain_period_seconds`, fixtures `round_byes`, player
`team_*`, lineup `team_status` — see `../AFL-API-REFERENCE.md`).

## Orchestrator

`data-raw/rebuild_everything.R` — "nuclear option" with phase gating:

| Phase | What | Skip flag |
|-------|------|-----------|
| 1 | API scrape (fixtures/squads/lineups/stats — recovers non-chain captured fields) | `--skip-api` |
| 2 | **Scrape chains** (enhanced scraper → `chain_team_id`, corrected frame) | `--skip-chains` |
| 3 | **Build PBP** from chains (`clean_pbp` applies corrected orientation) | `--skip-pbp` |
| 4 | **Train models** — 4a EP → 4b WP → 4c shot/xG | `--skip-models` |
| 7 | Stat ratings | `--skip-skills` |
| 8 | Ratings (TORP/EPR/PSR) | (gated by start) |
| 9 | Match predictions | `--predictions-only` |
| 10 | Simulation | `--skip-sim` |

Run via PowerShell (arrow segfaults under Git Bash R):
`powershell.exe -Command 'Rscript "torp/data-raw/rebuild_everything.R" 2021 2026'`

---

## Stage 0 — `clean_pbp` orientation rewiring (code, NOT a publish)

Make orientation self-contained: prefer the API-sourced `home_team_id` (from the chains
response that defines `(x,y)`) over the `load_results()` join, with fallback when absent.

- **Files:** `R/clean_pbp.R` (`fix_chain_coordinates_dt` / the `home_team_id` source),
  possibly `R/column_schema.R` (carry `homeTeamId`/`awayTeamId` through normalisation).
- **Verify:** unit test on a match where the results-join home/away and the API home/away
  agree (sanity) + a synthetic case where they'd differ (the fallback/precedence).
  `devtools::test()` green.
- **Risk:** low — pure code, no data published. Lands on a branch/PR for review.
- **Status:** ✅ **done** (2026-06-18). `clean_pbp.R` computes `coord_home_team_id`
  (prefer API `homeTeamId`, fallback to joined `home_team_id`) and uses it in both
  orientation steps (A pitch, G action). Verified: strict no-op on current data (no
  `homeTeamId` → fallback); precedence test proves it follows the API id when the
  results join is swapped. 53 test blocks, 0 fail. Activates automatically once Stage 1
  re-scrapes chains carrying `homeTeamId`.

## Stage 1 — re-scrape chains + rebuild PBP (Phases 1–3)

`rebuild_everything.R --skip-models` (or start-from Phase 1) for `2021:2026`.

- **Produces:** new `chains-data` + `pbp-data` releases with `chain_team_id` populated and
  corrected `x/x_pitch` for the ~7% previously mis-flipped shot rows, plus the newly
  captured fields.
- **Verify (before any model retrain):** on several historical matches, shot rows with
  `x<0`/`goal_x>venue/2` drop to ~0 (mirror the #92 test-match check, 7→0); row counts
  per season unchanged; `chain_team_id` non-NA for all new rows; spot-check a known goal's
  coordinates point at the attacking goal.
- **Rollback:** prior `chains-data`/`pbp-data` release assets (GitHub keeps history; note
  the asset `updatedAt` before overwrite). Models/ratings still point at old data until
  Stage 2, so a bad re-scrape is contained here.
- **Blast radius:** foundational PBP. ~hours of scraping. **Publishing event.**
- **Status:** ⬜ gated.

## Stage 2 — retrain EP → WP → shot (Phase 4)

`rebuild_everything.R --models-only --training-seasons 2021 <current-1>`.
**EP must train before WP** (WP uses EP as a feature); shot/xG after.

- **Produces:** retrained EP/WP/shot models in `torpmodels` (+ the live JSON exports in
  torpmodels for the Worker — confirm those are regenerated, see live-model export note).
- **Verify:** model-output validators (EP in plausible bounds, WP∈[0,1]); compare holdout
  metrics vs the pre-retrain models (expect small movement, not a regression);
  shot/xG calibration sane.
- **Rollback:** previous model release assets.
- **Blast radius:** every EP/WP/xG number downstream. **Publishing event.**
- **Status:** ⬜ gated.

## Stage 3 — backfill ratings + predictions (Phases 8–9)

`run_ratings_pipeline.R` with `SEASONS=TRUE` (all history) → republish `ratings-data`,
`player_game_ratings-data`, `team_ratings-data`; then match predictions.

- **Verify:** capture a baseline first (per the publishing-events discipline), then diff
  before→after on representative players/games; low-TOG `torp_value`, PSR historical, and
  now coordinate-dependent EPV should all move coherently.
- **Note:** this restacks on top of the #80/#88 corrections already live; expect TORP/EPV
  to shift again from the coordinate fix. **Needs release notes** (foundational data +
  model retrain, not just a refresh).
- **Blast radius:** all published ratings + predictions + blog. **Publishing event.**
- **Status:** ⬜ gated.

## Stage 4 — blog refresh

Dispatch torpdata `build-blog-data` to pull the corrected parquets to R2; the blog's
client-side shot-distance band-aid (PR #91 / `add_shot_geometry_variables`) becomes a
no-op once the upstream `x` is correct — confirm and optionally remove it.

- **Status:** ⬜ gated.

---

## Sequencing & gating

Stage 0 (code) → review/merge → **Stage 1** → verify coords → **Stage 2** → verify models
→ **Stage 3** → verify ratings → **Stage 4**. Each stage gates the next on verification;
each of 1–4 is an owner-triggered publishing event. Do NOT chain them blindly — verify
between stages so a regression is attributable to one stage (same discipline as the
#80→#88 value republishes and the rename cutover).

## Open questions to resolve before Stage 2

- Are the **live JSON model exports** (torpmodels, consumed by the inthegame-blog Worker
  `ep-model.js`) regenerated by Phase 4, or a separate step? Confirm so live inference
  isn't left on stale models.
- Training season range: default `2021:(current-1)`; confirm whether to include the
  partial current season.
