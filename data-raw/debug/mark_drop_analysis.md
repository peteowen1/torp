# Mark Dropped / Mark Fumbled — Credit & Blame Analysis

## Discovery

AFL chains data contains "Mark Dropped" and "Mark Fumbled" events. These were previously invisible to the EPV credit system — ~9,300 events per season with no attribution.

## Data Patterns

- **Mark Fumbled** is the primary event (9,337 total across 2021-2026)
- **Mark Dropped** always follows Mark Fumbled from the same player — it's a duplicate
- Detection: start from Mark Fumbled, look backward 1-5 rows for a Kick (99.98% match rate)
- **~60% same-team** (5,573): teammate fumbled the kicker's delivery
- **~40% opponent** (3,748): opponent tried to mark/intercept but fumbled

Zero "contested" drops found (no defender at sign-flipped coords 2 rows ahead).

### Key Chain Sequences

**Same-team fumble → teammate crumbs (437 cases):**
```
Kick (A, team X) → Mark Fumbled (B, team X) → Mark Dropped (B) → Loose Ball Get (C, team X)
```
Avg kick delta_epv: **+0.18** (slightly positive — teammate recovered well)

**Same-team fumble → opponent crumbs (1,095 cases):**
```
Kick (A, team X) → Mark Fumbled (B, team X) → Mark Dropped (B) → Hard Ball Get (D, team Y)
```
Avg kick delta_epv: **-1.29** (very negative — turnover)

**Opponent fumble:**
```
Kick (A, team X) → Mark Fumbled (E, team Y) → Mark Dropped (E) → [crumb]
```
Avg kick delta_epv: **-0.74** (negative — kicked to wrong team)

## How PBP Credit Works (Background)

Each Kick row in PBP has:
- `delta_epv`: EP change from this play (incorporates the fumble + crumb outcome)
- `lead_player_id`: whoever appears in the NEXT EPV-relevant row (Mark Fumbled/Dropped are filtered out, so this is the crumber, not the fumbler)

Credit is split:
- Kicker gets `disp_epv = delta_epv * 0.5`
- `lead_player_id` gets `recv_epv = delta_epv * 0.5`
- The crumber also has their OWN row in PBP (Loose Ball Get, etc.) with its own `delta_epv`

## Before vs After — Current Implementation

### Same-Team Fumbles (5,573 kicks)

| Role | BEFORE | AFTER |
|---|---|---|
| Kicker | disp_epv = -0.281 avg (wrongly penalized) | 0 (held harmless) |
| Fumbler | invisible (0) | contest_epv = -0.562 avg (full blame) |
| Crumber (lead_player_id) | recv_epv = -0.281 avg (wrongly blamed) | excluded from this row |

### Opponent Fumbles (3,748 kicks)

| Role | BEFORE | AFTER |
|---|---|---|
| Kicker | disp_epv = -0.370 avg (deserved — kicked to wrong team) | -0.370 (unchanged) |
| Opponent fumbler | invisible (0) | contest_epv = -0.247 avg (1/3 penalty) |
| Crumber (lead_player_id) | normal recv_epv | unchanged |

### Per Season Summary

| Season | Same-team drops | Opp drops | Fumbler blame (same-team) | Opp fumbler penalty | Kicker saved |
|--------|----------------|-----------|--------------------------|--------------------:|-------------:|
| 2021 | 1,157 | 645 | -722.7 | -144.1 | +361.3 |
| 2022 | 1,125 | 769 | -606.1 | -185.3 | +303.0 |
| 2023 | 1,016 | 771 | -602.2 | -202.9 | +301.1 |
| 2024 | 1,051 | 713 | -528.7 | -175.7 | +264.3 |
| 2025 | 1,029 | 746 | -585.9 | -179.3 | +293.0 |
| 2026 | 195 | 104 | -87.8 | -37.0 | +43.9 |

## Concrete Examples

### Example A: Teammate crumbs (North Melbourne)

```
CHAINS:                              PBP:
1328. Kick (Ben McKay)          →  1328. Kick         delta_epv = +0.158
1329. Mark Fumbled (player X)        [filtered]
1330. Mark Dropped (player X)        [filtered]
1331. Loose Ball Get (Walker)   →  1331. LBG Crumb    delta_epv = +0.161
1332. Kick (Walker)             →  1332. Kick         delta_epv = +1.971
```

- McKay (kicker): BEFORE +0.079 disp → AFTER 0
- Fumbler: BEFORE 0 → AFTER +0.158 contest_epv
- Walker (crumber): loses +0.079 recv from Kick row, still gets +0.080 from own LBG row
- Note: delta_epv is positive here because the crumb recovered well

### Example B: Opponent crumbs (Hawthorn vs Sydney)

```
CHAINS:                              PBP:
297. Kick (Morrison, Hawks)     →  297. Kick          delta_epv = -1.762
298. Mark Fumbled (Hawks player)     [filtered]
299. Mark Dropped (Hawks player)     [filtered]
300. Hard Ball Get (McCartin, Swans) → 301. HBG      delta_epv = -0.235
302. Handball (McCartin)        →  302. Handball      delta_epv = +0.494
```

- Morrison (kicker): BEFORE -0.881 disp → AFTER 0
- Fumbler: BEFORE 0 → AFTER -1.762 contest_epv
- McCartin (opp crumber): BEFORE **-0.881 wrongful blame** → AFTER 0 (excluded)
- McCartin gets -0.117 from own HBG row instead

## Open Questions / Things to Think About

1. **Should the crumber really be fully excluded from the Kick row's recv_epv?**
   - PRO: The Kick row's delta_epv is mostly driven by the fumble, not the crumb. Sharing it with the crumber wrongly blames them (avg -0.28 for same-team).
   - CON: The crumber IS involved in the play — they positioned themselves to recover. Should they get some credit for the recovery? Their own row captures the crumb-forward value but not the "being in position" value.

2. **Should the kicker really get 0 for same-team fumbles?**
   - PRO: The ball reached the teammate's hands — delivery was good.
   - CON: Maybe some kicks are harder to mark than others (low, bouncing, etc.). A poor delivery that technically reaches the teammate might contribute to the fumble.
   - Future: counterfactual EP (EP_if_marked vs EP_actual) would give the kicker positive credit for good deliveries and less/none for poor ones.

3. **Fumbler gets full delta_epv — is that too harsh?**
   - The full delta_epv includes both the fumble cost AND whatever happened after (crumb recovery or turnover). If a teammate crumbs well, delta_epv might be near 0 or positive, so the fumbler isn't penalized much. If it turns over, delta_epv is very negative (-1.29 avg for opponent crumbs).
   - Is the fumbler responsible for the crumb outcome? They caused the loose ball, but the crumb quality depends on other players.
   - Alternative: give fumbler a fixed penalty or use the counterfactual (EP_if_marked - EP_at_fumble_spot).

4. **Opponent fumbles — is 1/3 penalty right?**
   - Currently: opponent fumbler gets `delta_epv * contest_share` = -0.247 avg.
   - For context: a successful intercept mark defender gets +0.247 avg (the mirror).
   - The kicker still keeps normal disp_epv blame. The opponent penalty is "additional" blame from the opponent's team perspective.

5. **Delta_epv distribution is wide (SD ~1.1)**
   - ~35% of same-team fumble kicks have POSITIVE delta_epv (fumble didn't cost much)
   - ~24% are worse than -1.0 (expensive fumbles)
   - Should credit be capped or smoothed?

## Implementation Status

Code is implemented across:
- `torp/R/constants.R` — `CHAINS_MARK_DROP_DESCS`
- `torp/R/clean_pbp.R` — same-team fumble detection in `add_contest_vars_dt()`
- `torp/R/player_credit.R` — disposal scale override + `compute_contest_credit()` extension
- `torp/R/contests.R` — `extract_contests()` mark_dropped outcome
- `torp/R/globals.R` — global variable declarations
- `torp/tests/testthat/test-contests.R` — tests (all passing)

PBP data needs regeneration to bake in `mark_drop` contest_outcome tags.
