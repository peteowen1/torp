# FABLE-WPA-DECISION.md -- WPA-in-TORP reinstatement evidence memo

**Generated:** 2026-07-22 by `data-raw/04-analysis/wpa_close_game_bias.R`
**Seasons analysed:** 2023, 2024, 2025

**Applied wp_calibration sidecar:** form = leverage_interaction_v1, a = -0.0946, b = 1.0700, c = 1.2018 (slope_q4close_before = 1.5432, slope_q4close_after = 1.0746)
**Sanity check:** max abs diff between uncalibrated and calibrated raw wp = 0.17117 (must be >= 1e-12 or the script aborts before reaching this point)

## Decision rule (torpverse/docs/plans/FABLE-RECAL-PLAN.md Step 6.4)

Reinstatement (as a blend component or continued parallel display -- Pete's call)
becomes *defensible* if the close-game exposure coefficient shrinks materially
(>50%) under calibration AND the calibrated leaderboard is not dominated by
close-game exposure. This memo does NOT pre-commit the blend weight -- that is a
separate, later decision commit, gated on this measurement.

## Measured

- Uncalibrated: cor(wpa_rate, close_share) = 0.1642, lm coefficient on close_share = 0.0284 (n = 1677 player-seasons)
- Calibrated:   cor(wpa_rate, close_share) = 0.1163, lm coefficient on close_share = 0.0418 (n = 1677 player-seasons)
- Exposure-coefficient shrinkage under calibration: -47.2%
- Calibrated top-20 leaderboard dominated by close-game exposure (>50% of top-20 with close_share > 0.5): no

## Verdict: reinstatement is **NOT YET DEFENSIBLE** by this measurement

Full leaderboards and regression summaries are console output only (not persisted
here) -- re-run the script to reproduce, or capture its stdout alongside this file.

