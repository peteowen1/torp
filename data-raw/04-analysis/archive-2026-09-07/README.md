# Archived one-off analysis scripts (2026-09-07)

173 scripts, ~24,900 lines, moved here on the day EPV v4 was published. They are the
measurement record behind decisions already made -- the `epv3_*` gate and experiment
cluster, the defender-value and spoil-pricing audits, the standardisation and calibration
sweeps -- and nothing in `R/` or any pipeline sources them. Kept rather than deleted
because roxygen comments throughout `R/` cite them by name as the provenance for a
constant, and a cited measurement that cannot be read is worse than a large directory.

**These are not maintained and are not expected to run.** Several source a sibling by
absolute path (`C:/dev/torpverse/torp/data-raw/04-analysis/<name>.R`); those paths are
stale for any file that moved here. Repoint them if you need to re-run one.

What stayed in `data-raw/04-analysis/`, and why:

| Script | Why it is live |
|---|---|
| `np_*.R` | the v4 Net Points ledger: chain equivalence, year-over-year shares, conversion persistence |
| `run_epr_gate_v3v4.R`, `run_epr_gate_v4layer.R` | the fast EPR gate that chose v4 and its layer variants |
| `build_net_points_explainer.R` + template | builds the published Net Points explainer pages |
| `epv_calibrate_game_level.R` | game-level calibration of the engine |
| `benchmark_epr_gate.R`, `benchmark_suite.R`, `cache_guard.R` | shared helpers the live scripts source |
| `aflw_decay_*.R` | the AFLW decay work, still open |

Git history holds every version; `git log --follow` works across this move.
