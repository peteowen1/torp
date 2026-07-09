# Design plans — deeper refactors from FABLE-REVIEW.md

Date: 2026-07-09. These are plans only — no code changes. Each needs your go/no-go before implementation.

**Deferred to Sunday's Fable cross-verse design session, not covered here:** C2/P1/H1 (read-modify-write protocol + row-count floors), P3 (verify-gate redesign), P4/P9 (release-state new-game detection), H6 (loader API standardisation), H8 (move daily-release logic into `R/`), P13 (atomic R2 publish via manifest). All of these are facets of the same "atomic publish / data bus" problem that bouncer and panna also have — better designed once, consistently, than three times separately. See `C:\dev\FABLE-BURN.md` Prompt F.

---

## 1. H2 — Strict-mode loader failure semantics

**Problem:** `.load_with_cache()` (`R/load_data.R:260-263,273-287`) and the multi-URL loaders in `load_engines.R:226-238,365-384` reduce per-season/per-URL fetch failures to `cli_alert_danger` — a message, not a catchable condition, invisible in CI logs. Failed pieces are silently dropped and the **incomplete result is cached for 1 hour**, so every subsequent loader call in the same pipeline run reuses the hole. This is the same failure class as the documented #88 "flat across history" bug (torp/CLAUDE.md gotchas).

**Proposed approach:**
1. `cli_alert_danger` → `cli_warn()` so failures are catchable via `withCallingHandlers`/`tryCatch` and visible in CI logs as real warnings, not just console noise.
2. Attach a `failed_seasons`/`failed_urls` attribute to the returned object — mirrors the `failures` attribute pattern just added to `run_daily_release()` this session (P11 fix), so the idiom is consistent across the package.
3. Add `strict = FALSE` to the affected loaders (`load_player_stats()`, `.load_with_cache()`, `parquet_from_urls_parallel()`, etc.). `strict = TRUE` aborts via `cli_abort()` if any requested season/URL fails with a non-404 error. Pipeline scripts (`data-raw/`) call with `strict = TRUE`; interactive use keeps the permissive default.
4. Never `store_in_cache()` an incomplete result — gate the cache write on "everything requested succeeded."

**Risk:** distinguishing "season doesn't exist yet" (fine, not a failure) from "season should exist but fetch failed" (real failure) needs a check against `get_afl_season()`/known season ranges — conceptually the same 404-vs-transient distinction the atomic-publish family needs, worth keeping consistent with that work once it lands.

**Verification:** unit tests with a mocked flaky season (one URL fails, others succeed): assert `strict=TRUE` aborts naming the failed season; `strict=FALSE` returns partial data + `failed_seasons` attribute + warning; and — critically — a retry immediately after a transient failure re-fetches from source rather than reusing the cached hole.

---

## 2. H3 — Skip-marker policy (negative cache)

**Problem:** `is_download_skippable()`/`mark_download_skippable()` (`R/local_data.R:224-237`, consumed at `R/load_engines.R:100-102,273-280`) is a negative cache: a 404 marks a URL skippable for up to a day. Mid-week, a loader can request the current round's file moments before the daily release uploads it, get a 404, and then suppress that same file for up to 24h even after it's published — silently starving ratings/predictions of the newest round.

**Proposed approach:**
1. Shorten the skip-marker TTL for current-season/current-round URLs to 1-3 hours (the negative-cache case should be cheaper to re-check than the existing 1-day current-season read TTL).
2. Add `clear_skip_markers()` at the top of every pipeline entry point (`run_daily_release()`, `run_ratings_pipeline()`, etc.) so each run starts clean rather than inheriting yesterday's markers.
3. Consider not marking current-round URLs skippable at all — detect "this URL is for the round we're actively expecting new data on" and skip the negative-cache write entirely, relying on normal retry.

**Risk:** option 3 trades away the negative-cache's perf benefit (repeated 404s during the publish gap) for correctness — likely the right trade given file size/call frequency, but should be checked against any batch script that calls loaders dozens of times in quick succession during that window.

**Verification:** simulate the exact race — request before upload (404, marker set) → "publish" the file → request again inside the old TTL. Assert the new code fetches successfully instead of skipping.

---

## 3. M8 — O(n²) rolling-profile loop

**Problem:** `.compute_rolling_stat_profiles()` (`R/opponent_adjustment.R:281-347`) rescans all prior matches per match × ~18 teams × ~30 stats — quadratic in match count, costing real minutes on a full-history run. `R/ladder.R:230-236`'s `cum_boost` has the same subset-inside-`vapply` anti-pattern.

**Proposed approach:** pre-sort by date once, then replace the per-match rescan with a running decay-weighted sum per team — the exponential decay factorises (`S_t = S_{t-1} * exp(-λΔt) + x_t`), so no need to touch history older than one step back. For `ladder.R`, replace the vapply-subset with a `data.table` non-equi join or a running `cumsum`.

**Risk — this is the one to be most careful with.** It's a rewrite of a recurrence relation; an off-by-one (whether today's observation folds into the running sum before or after computing today's adjustment) would silently shift every opponent-adjustment number without erroring. This needs a design review specifically on the recurrence's exact semantics before any code is written, not just a code review after.

**Verification:** before touching production code, run both old and new implementations side-by-side on the full real historical dataset and assert numerical identity (or ~1e-9 tolerance) across every team/stat/date — the same "old-vs-narrowed output byte-identical" discipline panna's CLAUDE.md documents for this exact class of refactor. Only swap the production function once that comparison passes on real data.

---

## 4. M14 / T1-T6 — Test infrastructure with mocked fixtures

**Problem:** the test suite has real network coupling — startup downloads 8 datasets whenever online; on failure everything becomes NULL and ~25 tests silently skip, producing green CI with invisible coverage holes. `test-load_torp_data.R:43` and `test-integration.R:199` hit the network with no skip guard. `test-cache.R` is 100% live-API. Coverage job has `continue-on-error: true` with no floor. No scheduled run exists to catch off-season date-path breakage.

**Proposed approach:**
1. Commit small deterministic fixture parquets (`tests/testthat/fixtures/`) covering the shapes each network-dependent test needs; swap live downloads for `source = "local"` reads against fixtures.
2. Add `skip_if_offline()`/`skip_on_ci()` guards to any tests that genuinely can't be fixture-ized (e.g. testing the live scraper against the real AFL API).
3. Remove `continue-on-error: true` from the coverage job, or add a floor.
4. Add a weekly scheduled cron run of the test workflow (separate from push/PR) to catch date-dependent breakage during the off-season.

**Risk/scope:** building correct fixtures requires knowing exactly what shape each test needs (full season vs. 2 rounds, which edge cases) — this is genuinely multi-session work, not a single sitting. Recommend scoping an MVP first: fixture-ize just the files the review named (`helper-test-data.R`, `test-load_torp_data.R:43`, `test-integration.R:199`, `test-cache.R`) rather than attempting the whole suite at once.

**Verification:** after fixture-izing, run the suite fully offline (block network access) and confirm the SAME pass count as before — proving we've closed the "silently green via skip" gap, not just moved it.

---

## 5. P16 — Cross-repo model version skew (torp / torpmodels / torpdata)

**Problem:** torpdata workflows check out torp with no `ref` (`daily-data-release.yml:68-73`, `build-blog-data.yml:18-22`); torpmodels training wrappers hardcode `devtools::load_all("C:/dev/torpverse/torp")` (a local dev-machine path, not portable to CI); `pb_upload` to `core-models` carries no version metadata. A reordered/renamed feature in dev torp → a model trained on dev's feature set, scored later by main's feature set → silently wrong EP/WP everywhere, with nothing to catch it.

**Proposed approach:**
1. Embed `packageVersion("torp")` + git SHA as an attribute on the saved model RDS at training time.
2. `load_torp_model()` asserts (or at minimum warns loudly) if the loaded model's recorded torp version/SHA doesn't match the currently-loaded torp package version.
3. Record the torp SHA used in each training run in the GHA workflow summary/logs, so a version-skew incident is traceable after the fact.
4. Fix the hardcoded local path in the torpmodels training wrappers to work in CI (they currently only work on this dev machine).

**Risk:** low — this is additive metadata + an assertion, not a behavior change to the model-serving path itself. Main risk is scope creep into "should CI pin torp to a specific tag/release instead of floating `main`?" — that's a bigger policy question worth a separate conversation, not bundled into this fix.

**Verification:** train a throwaway model with a deliberately mismatched torp version, confirm `load_torp_model()` catches it; confirm the existing (matched-version) path still loads cleanly with no new warnings.
