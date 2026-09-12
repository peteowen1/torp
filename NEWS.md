# torp 1.9.0

## Rating vintage v13: a contest has three outcomes, not two

`EPV3_CONTEST_OUTCOMES` goes `"two"` -> `"three"` and
`EPV3_CONTEST_EXCLUDE_SHOTS` `FALSE` -> `TRUE`, shipped together because they
are one design.

A kick into a contest ends with a mark to the attack, a mark to the defence, or
nobody marking it. With two bins all 11,302 spoils are booked "defence won", and
two things follow:

- `V_def` is fitted **88.4% on spoils**, so an intercept mark genuinely worth
  **−0.722** expected points prices at **+0.270** — about a full point
  understated, on each of 1,602 of them. Under `"three"` it prices at −0.720.
- The fitted probability is blind to the distinction: mean `p_hat` 0.787 on
  spoils, 0.786 on attacking marks, 0.791 on defensive marks.

On inside-50 kicks the real split is nobody marks it 44.1%, attack marks it
37.5%, defence marks it 18.4% — so when the ball sticks the attack takes
**58.8%** of the marks. Shots at goal and ruck-tap kicks leave the population:
a defender touching a shot on the goal line is not a marking duel.

### Unlike v12, this NARROWS the gap

| position | v12 | v13 |
|---|---|---|
| KEY_FORWARD | 8.404 | 7.891 |
| MEDIUM_FORWARD | 6.385 | 6.164 |
| MIDFIELDER | 5.868 | 5.730 |
| RUCK | 4.971 | 4.809 |
| MEDIUM_DEFENDER | 4.565 | 4.424 |
| KEY_DEFENDER | 4.609 | 4.243 |
| **forward-defender gap** | **3.795** | **3.648** |

Measured against v11 the same model read **+0.235**, the opposite sign. That
was a comparison error, not a model change: v11 still carried the asymmetric
filter, so the filter fix sat inside the three-way arm and was absent from the
baseline. The three-way arm lands on 3.648 in both measurements — this path
skips the second filter entirely — and only the baseline moved, 3.413 to 3.795.

Key forwards give up 0.513 a game, 0.355 of it from `np_direct`, which is where
a same-team contest winner is booked. Every player-game moves (mean 0.352, max
5.882). Branch models are weak either way: AUC 0.603 for "nobody marks it" and
0.591 for "which side marks it", where 0.5 is a coin flip.

v12 is preserved as `torp_ratings_v12.parquet`. Verify with
`data-raw/05-validation/check_v13_published.R`, whose prediction was recorded
before the rebuild ran.

# torp 1.8.9

## Rating vintage v12: judge a contest by what happened, not by who won it

`NP_CONTEST_FILTER_SYMMETRIC` goes `FALSE` -> `TRUE`, fixing torp#220.

The contest path filtered its scored rows a second time with
`def_win == TRUE | out_desc %chin% EPV3_DUEL_OUT`, keeping every defensive win
unconditionally but an attacking win only when the outcome was in the narrower
duel list. A `Mark Fumbled` won by the defence was priced; the identical
outcome won by the attack was discarded. The intent stated one line above it is
symmetric, so this was a defect against its own spec.

**Measured:** 1,483 of 17,078 contests dropped (8.7%), **100% of them attacking
wins** — 1,149 `Mark Fumbled` and 339 `Free For`. The defensive win rate read
86% against a true 78.7%, and the defence took 60.1% of all contest credit
against a corrected 48.3%.

In plain terms: a key forward who won a contested mark after a fumble or a free
was being paid nothing for it, 1,490 times a season.

| position | v11 | v12 |
|---|---|---|
| KEY_FORWARD | 8.005 | 8.404 |
| MEDIUM_FORWARD | 6.337 | 6.385 |
| MIDFIELDER | 5.903 | 5.868 |
| RUCK | 4.882 | 4.971 |
| MEDIUM_DEFENDER | 4.632 | 4.565 |
| KEY_DEFENDER | 4.592 | 4.609 |
| **forward-defender gap** | **3.413** | **3.795** |

**This WIDENS the gap, and that is the correct direction.** The defence's share
of contest credit was inflated by the discarded rows. It does partly reverse
what v9-v11 achieved for defenders, which is worth stating plainly: two of
those three vintages were also fixing specification errors, and this one
happens to run the other way. Combined with the finding that 83% of the gap is
earned, the honest read is that the forward-defender gap is largely real.

73% of player-games move (mean 0.223, max 5.838). v11 is preserved as
`torp_ratings_v11.parquet`. Verify with
`data-raw/05-validation/check_v12_published.R`, whose prediction was recorded
before the rebuild ran.

# torp 1.8.8

## A contest has three outcomes, and the population filter was asymmetric

Nothing here changes a published rating. Every new constant defaults to the
shipped v11 behaviour, and the inertness check reproduces it exactly: 17,571
contests built, 16,081 scored, 78.7% defensive.

### `NP_CONTEST_FILTER_SYMMETRIC` — torp#220, a live defect

The two-way contest path filtered its scored rows a second time with
`def_win == TRUE | out_desc %chin% EPV3_DUEL_OUT`, which keeps every defensive
win unconditionally but an attacking win only when the outcome is in the
narrower duel list. A `Mark Fumbled` won by the defence was priced; the
identical outcome won by the attack was thrown away. The intent stated one line
above it is symmetric, so this is a defect against its own spec.

Measured on 2026: 1,483 of 17,078 contests dropped (8.7%), **100% of them
attacking wins**. The defensive win rate reads 86% instead of 78.7%. Setting
the new constant `TRUE` is worth **+0.399 a game to key forwards** — one who
wins a contested mark after a fumble or a free was being paid nothing for it,
1,490 times a season.

### `EPV3_CONTEST_OUTCOMES` — three branches instead of two

A kick into a contest ends with a mark to the attack, a mark to the defence, or
nobody marking it. With two bins all 11,302 spoils are booked as "defence won",
so `V_def` is fitted 88.4% on spoils and an intercept mark genuinely worth
−0.722 expected points is priced at +0.270. The fitted probability is blind
too: 0.787 on spoils, 0.786 on attacking marks, 0.791 on defensive marks.

Under `"three"` an intercept mark prices at −0.720. Built as two nested
binomials rather than a multinomial, which keeps the three weights
non-negative and summing to 1 by construction and preserves
`bam(discrete = TRUE)`.

`EPV3_CONTEST_EXCLUDE_SHOTS` drops shots at goal and ruck-tap kicks from the
population: a defender touching a shot on the goal line is not a marking duel.

### Both flips widen the forward-defender gap

Symmetric filter 3.413 → 3.795; three-way model 3.413 → 3.648. That is the
correct direction — the defence's current 60.1% share of contest credit is
inflated by the discarded rows — but it reverses part of what v9–v11 achieved,
so enabling either is a decision rather than a default.

All four constants are registered in `.rating_defining_constants()` despite
being inert, because a flag that becomes rating-defining only once enabled
makes the run that enables it look like a no-change run to the drift guard.

# torp 1.8.7

## The "Where Net Points Come From" artifact can be rebuilt

`data-raw/04-analysis/build_np_categories_artifact.R` is new. The published page
had no committed generator, so nobody could regenerate it and it sat on rating
vintage v8 while production ran v11. The script computes every figure live from
`build_net_points(return_payments = TRUE)` — no cached CSV, no hand-entered
numbers — which is the same defect that put a pre-`#210` model on the ledger
walkthrough until Pete's own arithmetic caught it.

It also reports how far the per-play-type categories sum from `net_points`
rather than forcing the gap to zero: the payment ledger is read one step before
`.np_team_margin()`'s correction, which books its adjustment into `np_team`
alone.

# torp 1.8.6

## Rating vintage v11: stop charging defenders most for defending

`NP_TEAM_MARGIN_POOL_BY` goes `"dacts"` -> `"tog"`.

**This came out of decomposing the forward-defender gap for the first time
rather than guessing at it**, and the result reframes the defender program.
`build_net_points()` returns eight columns that sum to `net_points` exactly, so
the 4.273-a-game gap is arithmetic:

| column | KEY_FWD | KEY_DEF | gap |
|---|---:|---:|---:|
| **own acts** (`np_direct`) | **+4.079** | **-1.972** | **+6.051** |
| turnovers won | 0.446 | **2.259** | -1.813 |
| **offence pool** (`np_team`) | -5.619 | **-6.433** | **+0.814** |
| contests won | 0.033 | **0.702** | -0.669 |

**Defenders are not under-credited for defending.** They already out-earn
forwards on turnovers won and contests. The entire gap is that marking near goal
gains expected points while rebounding out of defensive 50 loses them -- which is
football. **3.543 of the 4.274 is earned and beyond any re-split.**

The one allocated lever was this constant, and it was hard to defend on its own
terms: it spread a side's **offence** pool in proportion to **defensive acts**, so
a defender doing his job more diligently took a bigger share of a pool that
exists to account for attacking value. Key defenders carried **-6.433** against
forwards' **-5.619** -- or -643 against -562 per 100 TOG.

| position | `dacts` | **`tog`** | change |
|---|---:|---:|---:|
| KEY_FORWARD | 2.031 | 1.650 | -0.381 |
| MEDIUM_FORWARD | 0.763 | 0.560 | -0.203 |
| MIDFIELDER | 0.329 | 0.345 | +0.016 |
| RUCK | 0.630 | 0.696 | +0.066 |
| MEDIUM_DEFENDER | -1.294 | -1.159 | +0.135 |
| **KEY_DEFENDER** | **-2.242** | **-1.765** | **+0.477** |

Gap **4.273 -> 3.415, narrower by 0.858** -- more than the entire allocated part
(0.731), because it flips the imbalance rather than shrinking it: under `"tog"`
defenders are charged slightly *less* of the pool than forwards.

**Cost: repeatability 0.606 -> 0.598**, the 0.008 edge that chose `"dacts"`.
Accepted knowingly -- position balance was never the criterion in that
comparison. All 9,838 player-games move (mean 0.339, max 3.08), so this ships
with a full-history rebuild.

# torp 1.8.5

## Rating vintage v10: the contest population and error blame go live

Two constants flip together. Both are Pete's decisions, and the mechanisms were
reviewed and merged separately (#214, #215) before either default moved.

| constant | from | to |
|---|---|---|
| `EPV3_CONTEST_POPULATION` | `"all"` | **`"evidence"`** |
| `NP_ERROR_BLAME_SHARE` | `0` | **`1`** |
| `RATING_VINTAGE` | `v9` | **`v10`** |

**The contest population was never a contest population.** It was inferred from
the *outcome* description, so **69.8%** of it was plain receptions -- a kick
finding a teammate, or an opponent marking it unopposed. Pete spotted this from
the data, not from a metric, and decided the rule by reading real chain
sequences: a contest is a kick where chains logged a target, or the outcome is
self-evidently a duel. Population 50,050 -> 17,493, and the worst calibration
error in the 0.8-0.99 band falls from **13.6 points to 2.8** with no
recalibration layer.

**A dropped mark is no longer charged to the kicker.** `Mark Fumbled` and
`Mark Dropped` are not in PBP, so the ledger saw only the kick followed by an
opponent winning the ball -- and booked the *kicker* with the turnover, a
different player than the one who erred **100%** of the time. The whole debit now
moves to the player who dropped it.

### The bundling was verified, not assumed

The two ship as one vintage on the claim that they are independent. Measured
across four arms (neither / each / both), the largest interaction term across all
six positions is **0.001 points a game**, so their separate figures genuinely add
and one rebuild covers both. Had they interacted, every attribution below would
have been wrong.

| position | v9 | **v10** | change |
|---|---:|---:|---:|
| KEY_FORWARD | 2.042 | 2.032 | −0.010 |
| MEDIUM_FORWARD | 0.900 | 0.762 | −0.138 |
| MIDFIELDER | 0.352 | 0.331 | −0.021 |
| RUCK | 0.439 | 0.618 | **+0.179** |
| MEDIUM_DEFENDER | −1.319 | −1.294 | +0.025 |
| **KEY_DEFENDER** | **−2.432** | **−2.235** | **+0.197** |

Forward−defender gap **4.474 → 4.267**. This is the first change in the
defender program to narrow it by *crediting defenders* rather than deflating
forwards: dropping 30k uncontested receptions stops a defender's contest wins
being diluted by rows where nobody contested anything.

All 9,794 player-games move (mean 0.493, max 5.205, 1,140 over a point), so this
ships with a full-history rebuild.

Reference: [PLAY-TYPES.md](https://github.com/peteowen1/torpverse/blob/dev/docs/reference/PLAY-TYPES.md).

### Also in this release

`DESCRIPTION` reaches `main` at 1.8.5. It had drifted: the 1.8.4 bump was staged
by the PR hook *after* #214's head commit was pushed, so it travelled to `dev`
with #215 instead and `main` briefly carried a NEWS heading for a version the
package did not claim.

# torp 1.8.4

## Blame the player who actually made the error (`NP_ERROR_BLAME_SHARE`)

The ledger runs on PBP, and **51 of chains' 78 play types never reach it**
(80,694 rows in 2026). When one of those names a player at fault and sits
between a disposal and the opposition's next possession, PBP sees only

```
Kick (SYD Bice)  ->  Loose Ball Get (NMFC Duursma)
```

the team changes, the kick is booked as the turnover, and Bice wears the whole
swing for a mark **Nick Blakey** dropped. Measured on 2026, `Mark Fumbled` and
`Mark Dropped` are blamed on a different player **100%** of the time.

`.np_error_actor()` names the culprit from the sequence `.np_sequence()` already
assembles, and `NP_ERROR_BLAME_SHARE` moves that share of the debit to him under
a new `error_blame` payment role. It moves the **recipient, never the amount**,
so conservation is untouched by construction -- verified at the aggressive
setting: team-margin max gap 7.1e-14, ledger-rebuild assertion clean.

**`Out On Full` and `Out On Full After Kick` are deliberately excluded.** They
name the *kicker*, and a kick out on the full **is** the kicker's error, so
today's attribution is already right on 1,819 of 1,902 rows. Including them
would have moved blame away from the player who earned it. That split is why
`NP_ERROR_DESCS` is a list rather than "every chains-only act".

Default is `0` -- inert, no published number moves. At `1.0`: 784 turnovers,
309.1 points, 1,436 player-games, max 1.609 a game.

## A second contest-target marker, and an honest note on what it does

`constants_data.R:132` has always defined
`CHAINS_CONTEST_TARGET_DESCS <- c("Contest Target", "Kick Inside 50 Result")`
while the contest engine scanned only the first. Pete spotted the beaten
contestant named on the i50 row at the outcome's own coordinates. A player is
named on 64.2% of those rows and is on the **kicking side 100.0%** of the time
(12,555 of 12,556) -- the property that makes it safe, since `target_pid` is
consumed only as the loser on a defensive win, and a row naming the *winner*
would debit the player who won.

That safety property is now **enforced, not documented**: `build_aerial_contests()`
drops any target equal to the winner and warns with a count. A 100% rate on one
season is a fact about that season, not about the code.

**It does not move published ratings, and an earlier draft of this entry said it
did.** `target_pid` reaches the ledger only through `compute_aerial_credit()`,
which `player_credit.R:658` calls inside `if (v3)`. Production runs v4, whose
path drops `target_pid` before the ledger sees it -- measured, the difficulty
terms are byte-identical with the flag on and off. Live under v3, inert under
v4, and wired into the drift guard because `EPV_ENGINE` is selectable.

**Under v4 the beaten target is never charged at all** -- the kicker wears it.
Simulated, moving that debit to the player actually beaten narrows the
forward/defender gap **4.474 → 4.096**, recovering 74% of what the #210 leak fix
cost, by *deflating key forwards* (−0.365) rather than crediting defenders
(+0.014). Not implemented; recorded as a decision for Pete.

# torp 1.8.3

## The disposal-difficulty model no longer reads its own outcome (#210)

`fit_disposal_models()` drops `s(kick_len)` and `s(fwd_gain)`. Both were
computed in `build_disposal_events()` from `out_x`/`out_y` -- the resolving row
-- which is the same row whose `out_tid` **is** the target
(`turnover = out_tid != team_id`). The old docstring excluded the outcome
*description* and then admitted the outcome *coordinates*.

`p_hat` splits the credit on ~96% of disposals, so this sat underneath most of
every player's Net Points. Measured over 150,071 disposals in 2026:

| | turnover rate |
|---|---|
| `kick_len` <= 0.5m (9,440 rows -- Goals and Behinds) | **0.0%** |
| `kick_len` 50m+ | 59.6% |

and the rows the model was surest about had a median `kick_len` of **115m** --
not a kick, but the distance to wherever the ball was next touched. `kick_len`
alone scored 0.45151 log loss against 0.54031 for intercept-only, i.e. 77% of
the full model's gain from one contaminated variable.

**Three cheaper repairs were measured and all three fail.** The in-flight chains
rows are not a landing coordinate (38.7% coverage, median implied length 0.0m --
they are logged at the kick's own position). Capping at a plausible kick
distance keeps 98.6% of the fit while over-60m rows are only 2.7% of the data,
so the leak is spread through the ordinary range rather than concentrated in the
absurd tail. Shortening the 1-6 row lookahead cannot help because 85.0% of scans
already stop at one row, and it would corrupt the *label*: an in-flight
annotation carries the KICKING team's id, so a 1-row window relabels 35.6% of
turnovers as retained.

Between them those three name the mechanism: **after a turnover the next
recorded event is intrinsically distant** -- a kick-in, the opponent downfield --
so any feature read off the resolving row carries the outcome, at every distance
and in every window. That is why there is no cheap repair and the features go.

**Cost:** 0.050 log loss, 44% of the old model's gain over the base rate. It
also all but removes the `p_hat` saturation behind #209 -- from 768 rows at
`p_hat >= 0.99` down to 18, with the disposal path now reaching **zero** (its
99.9th percentile is 0.7402; all 18 survivors are contests).

An earlier draft of this entry said that saturation matters because "the defence
is paid `(1 - p_hat) * surprise`". **That was wrong and is corrected here**, since
it is the kind of claim someone would later build on. `(1 - p_hat) * sur_hm` is
`recv_hm` on the *retained* branch (`epv_net_points.R:1061`) -- the receiving
teammate, not the defence -- and the defence is paid `cede_hm = (1 - beta) *
sur_hm` on the turnover branch, where `p_hat` does not appear at all. That
retained line is moreover **dead**: every retained row is overwritten downstream,
contested ones at `:1093`/`:1102` and uncontested ones at `:1125` (live, because
`NP_UNCONTESTED_RECEIVER_SHARE` is 0.5), and measurement confirms no row escapes
-- 0 of 19,660 contested rows have non-finite contest terms.

So `p_hat` is not a payment share on any live branch; it acts only through
`V_pre`, which shapes `decision` and `surprise` themselves. Clamping `p_hat` to
0.99 on all 18 rows and re-running the ledger moves **zero** of 9,794
player-games.

The real mechanism, measured: for a contest, `V_pre = (1 - p) * V_att + p * V_def`
and the ledger pays the winner out of `c_cont = V_branch - V_pre`. When the model
is certain **and the certain side wins**, `V_branch` already equals `V_pre`, so
the contest surprise collapses to zero and the winner's share of nothing is
nothing. Mean `|cont_surprise|` is **0.0005** on the saturated rows against
**0.937** elsewhere. Certainty does not divert the payment; it destroys the
quantity being divided.

It is symmetric, which confirms the mechanism rather than a defender-specific
story: where the *attack* is certain (`p <= 0.01`) and the attack wins,
`|cont_surprise|` is 0.0129; where the attack is certain and the **defence** wins
anyway, it is **1.9554**. And it is a gradient, not 18 rows -- mean
`|cont_surprise|` runs 1.29 / 0.83 / 0.71 / 0.67 / 0.28 / 0.002 / 0.000 across
rising `p_hat` bands. The 18 are simply where it reaches exactly zero.

That reframes #209 from a bug into a design question, which is why it is recorded
here rather than fixed: surprise-based credit pays nothing for an unsurprising
outcome **by construction**. Whether a defender who wins a contest he was
near-certain to win should be paid nothing is Pete's call, not a code fix.

**This moves published ratings** -- mean |change| 0.396 a game, max 4.364, 718 of
9,794 player-games moving more than a point -- so it needs a `RATING_VINTAGE`
bump and a full-history rebuild, not a quiet merge.

**It is NOT the defender fix, and that was tested rather than assumed.** The
hypothesis was that an inflated upper tail of `p_hat` suppresses defensive
credit. Measured through the real machinery on production's fit-on-2025 regime,
removing the leak moves key defenders -2.107 -> -2.432 a game and *widens* the
forward/defender gap 3.963 -> 4.474.

The aerial-contest model in `epv_v3.R` was checked for the same defect and is
left alone. Its `out_desc` is restricted to marks and spoils, so the resolving
row is where the ball arrived, and `kick_len` there is physically plausible
(median 33.8m, p95 55.6m, 726 of 50,050 rows over 60m) with a smooth monotone
defence-win gradient from 9.1% to 67.4%. Conditioning on where the ball landed is
a legitimate question about a kick the kicker aimed.

**But it is not spotless, and an earlier draft of this entry overclaimed it as
having "none over 100m".** There are **16 of 50,050 (0.032%)** between 102.0m and
143.3m -- impossible as a single kick. All 16 are `def_win`, and all 16 resolve at
a reception (15 Uncontested Mark, 1 Mark On Lead): the same mechanism as the
disposal leak, at 1/3000th the scale. `CHAINS_INFLIGHT_DESCS` includes phases that
are not one kick's flight (`Rebound 50`, `Kick In Ineffective`, the kick-in
entries), so the `.olag` scan can occasionally walk through a whole unrelated
possession. Left as-is on materiality, now stated as a count rather than a false
absolute.

The error is worth recording because of how it happened: the check script printed
`pct_over_100m` rounded to one decimal, 0.032% rendered as "0.0", and that was
read as zero -- while the `max` column in the same table read 143.3m and said
otherwise. A percentage cannot express "none"; only a count can. The script now
prints counts at 60/80/100/120/140m.

`kick_len` and `fwd_gain` are still built in `build_disposal_events()`: the
`is.finite()` filter that ends that function uses them to define which disposals
are scored, so removing them would silently change the row population.

# torp 1.8.2

## Ten rating-defining constants wired into the drift guard

`.rating_defining_constants()` now captures the stoppage pricing unit
(`NP_STOPPAGE_DESCS`, `NP_STOPPAGE_HITOUT_DESCS`, `NP_STOPPAGE_RUCK_OWN_DESCS`,
`NP_STOPPAGE_SPLIT`, `NP_STOPPAGE_BAND_M`, `NP_STOPPAGE_Y_BREAKS`,
`NP_STOPPAGE_SHRINK_N`) plus `NP_OFFENCE_POOL_SHARE`,
`NP_CONTEST_WINNER_SHARE` and `NP_CONTEST_WINNER_SHARE_DEFAULT`. Until now
changing any of them altered every published rating with no manifest trace and
no vintage bump required -- they were honestly registered as a known gap in
`test-vintage-wiring.R`'s `KNOWN_NON_DEFINING`, on "unaudited, could move
ratings" rather than on measurement.

They are measured now, on 2026 (9,794 player-matches, published numbers after
`.np_team_margin()`): `NP_OFFENCE_POOL_SHARE` 0.10 -> 0.40 moves **100% of
players, max 9.15 a game** -- the largest unguarded mover found;
`NP_CONTEST_WINNER_SHARE` with the mark entries 0.80 -> 0.40 moves 100%, max
8.50; `NP_STOPPAGE_SPLIT` flipped back to its pre-2026-09-10 `ground` split
moves 100%, max 2.83. The remaining stoppage constants go in as structural
members of the same mechanism rather than swept individually: `DESCS` selects
which rows are stoppages at all, the hitout/ruck-own descriptions route which
split each takes, and `BAND_M` + `Y_BREAKS` are the two halves of one cell key.
Wiring half a cell key is what the register refused to do and was right to
refuse, so they go in together.

## NP_STOPPAGE_LOSER_SHARE is inert, and why that is safe to rely on

Swept 0.50 -> 0.20 over 2026: the raw `build_net_points()` ledger moves up to
**9.68 points a player-game** (mean 0.72) and the published numbers move
**8.0e-15** -- 0 of 9,794 player-matches change at 1e-6. Not small: zero.

The mechanism, which is what makes it trustworthy rather than a lucky sweep:
`.np_team_margin()` rescales each SIDE of every row up to that row's full value
(`own * target / side_sum`), so a constant whose only job is splitting a row
BETWEEN the two teams is divided straight back out. `NP_STOPPAGE_LOSER_SHARE`
is exactly that and nothing else -- the within-side ruck/player/pool split is
`NP_STOPPAGE_SPLIT`'s job. It stays in `KNOWN_NON_DEFINING`, moved to the
proven-inert section with the measurement recorded.

The same algebra predicted `NP_BLAME_SHARE` inert and was **wrong** (it moves
100% of players, max 6.25), because `NP_BLAME_POOL` gives the conceding side its
own pool, making it a within-side split as well as a cross-team one. Measure,
do not infer.

## Sequencing: a widening change merges FIRST, then re-records

Recorded because getting it backwards took the pipeline down on 2026-09-10.
`check_vintage_alignment()` compares via `.diff_defining_constants()`, which
**unions** the manifest's and the live code's name sets and reports `<absent>`
in **either** direction. So the manifest must never describe more constants than
the code on `main` knows about.

That inverts the familiar order. A **value** change (a constant's value moves,
so published numbers change) promotes the manifest BEFORE the code reaches
`main` — the v6/v7/v8 pattern above. A **widening** change like this one, where
values are unchanged and only the recorded set grows, has to go the other way:
merge to `main` first, then re-record. Re-recording v8's 97 leaves from this
branch while `main` still knew 65 made `main` see 29 phantom differences and
abort every run; reverted the same hour from a worktree at `origin/main`, with
the pipeline never firing in between and no published number touched.

The re-record itself is scripted at
`data-raw/05-validation/rerecord_vintage_manifest.R` rather than left as a
hand-typed call, because `publish_ratings_manifest()` needs `n_rows` (required,
no default) and an explicit `file` — its default names `torp_ratings_v8.parquet`
while the canonical vintage lives at `torp_ratings.parquet`.

## Note on the v8 vintage

`RATING_VINTAGE` reached `"v8"` in 1.8.1 (the `EPR_PRIOR_RATE_HITOUT` zero-guard
fix, torp#206) without its own NEWS section or a version bump. Recorded here so
the changelog's latest vintage note is not still v7 while production serves v8.

# torp 1.8.1

## RATING_VINTAGE moves to v7

Operational follow-up to 1.8.0, shipped separately because the order matters.
1.8.0 wired four rating-defining constants that the published v6 manifest did
not record, which makes `check_vintage_alignment(strict = TRUE)` abort every
scheduled run -- the same guard, and the same failure, as 2026-09-07.

Rolled forward rather than refreshing v6's manifest entry: the numbers genuinely
changed, so refreshing would claim v6 was produced by constants that now produce
different values, and the next run would upsert two different rule sets into one
published file.

v6 is preserved byte-identical as `torp_ratings_v6.parquet` (md5 `f1d0472d`,
checked against a re-download) and the manifest's `canonical` moved to v7 BEFORE
this constant reached `main`, which is the order the guard enforces. Note
GitHub's CDN served the stale manifest for about two minutes after the
overwrite, so a read-back has to be verified against the uncached URL that
`read_ratings_manifest()` actually fetches.

# torp 1.8.0

**Rating vintage v7.** Every published number moves, so this ships as a new
vintage rather than an in-place overwrite: the four credit rules re-split every
lost possession and every uncontested reception, and the stoppage rebuild
reprices 21.5% of stoppages by more than 0.10 points. The outgoing v6 is
preserved byte-identical as `torp_ratings_v6.parquet`; `canonical` moved to v7
before this reached `main`, which is the order `check_vintage_alignment()`
enforces.

## Four Net Points rules from Pete's row-by-row review of the scenarios page

Pete read `net-points-scenarios.html` row by row on 2026-09-09 (comments in
`docs/plans/NET-POINTS-SCENARIOS-REVIEW.md`, the data checks and plan in
`docs/plans/NET-POINTS-SCENARIOS-FIX-PLAN.md`). Four rules came out of it, each
behind a constant registered in `.rating_defining_constants()`:

- **`NP_BLAME_POOL` (on).** Under the team-margin convention every row is
  allocated once to each side, but the side that lost a possession had only
  the disposer to allocate to, so the rescale put 100% of every turnover on
  him: Sheldrick -1.43 of -1.43, Lobb -4.10 of -4.10, Sinclair -4.93 of -4.93.
  The conceding side now gets a pool worth what the row ceded, so the
  disposer keeps the decision and his `NP_BLAME_SHARE` of the surprise and the
  rest is the team's (Sheldrick -0.54 and Sydney -0.90 on the same row). The
  pool rows are marked `doubled` in `np_payments` and live only in the
  doubling layer; the raw ledger and its conservation checks are unchanged.
- **`NP_PRESSURE_BACK_SHARE` (0.5).** When a player loses the ball on a
  non-disposal act after a teammate's disposal to him, that share of his blame
  pool goes to the passer. Sinclair tackled after Howard's handball: Sinclair
  -3.45, Howard -0.74, St Kilda's pool -0.74 (was Sinclair -4.93).
- **`NP_SIREN_TO_POOL` (on).** `delta_epv` is built within a quarter, so the
  last row of each quarter carried the whole evaporating state to whoever last
  touched the ball: 852 rows and 1,082 points in 2026, 5.08 a match, 809 of it
  on named players. It goes to the possessing side's pool now, and the D15
  stoppage repricing no longer crosses a quarter boundary. Needs `period` on
  the play-by-play; the ledger says so when it is missing.
- **`NP_UNCONTESTED_RECEIVER_SHARE` (0.5).** A reception row has two players
  and they split its surprise evenly where no contest was fought; contested
  kicks keep D6/D8. Lewis to Weddle, uncontested deep in space: +1.23 / +1.46
  instead of +0.47 / +2.22.

Every team still sums to its own margin (max gap 6e-14 over 213 matches), and
the four rules are part of the still-unpublished v6 vintage, so no new vintage.
Not changed, measured and recorded in the plan: the difficulty model's
decision terms (median 0.14-0.31 of the row) are not the source of the numbers
that looked wrong on the page; the page was displaying each row's neighbour.

## The stoppage baseline is keyed on where the stoppage was, not on its outcome

`.np_stoppage_baseline()` bands a stoppage by location so it can be priced
neutrally. The location came from play-by-play -- and a PBP stoppage row's `x`
is where the ball was **next gathered**, identical to the following row's on
97.7% of stoppages and to the preceding row's on 7.1%. The neutral baseline was
therefore conditioned on the outcome it exists to be neutral about. The tell:
centre-bounce P(home wins) read 0.599 in one band and 0.435 in the next, which
is impossible for an event that always happens on the centre circle (pooled it
was always 50.8%; the split was artifact). `team` is NA on every stoppage row
too, so the `home` flag orienting them is inherited, and 2.4% of stoppages more
than 5m from centre sat on the wrong half of the ground.

Chains carries the real coordinate: 100.0% of 6,162 centre bounces sit at
exactly (0,0) there, against 12.1% in PBP.

- `.np_sequence()` now carries `chain_x_home` (chains `x` flipped to the home
  frame via `chain_team_id`) and `chain_aby`. The frame was verified on each
  side separately -- +0.967 on home-owned chains, +0.969 on away -- because a
  sign error is invisible on the home team, where the transform is the identity.
- **`abs(y)` joins the cell key.** Holding type and 20m band fixed, the baseline
  spreads 0.345 points across corridor / 15-30m / boundary, and 0.536 in the
  defensive 50, against a 0.66-point mean swing. That is roughly 10x what a
  stoppage-specific win probability could be worth (measured ceiling 6.2%).
- **The baseline is written as the contest it is**, `P(win) * V(win) + (1 - P) *
  V(lose)`, with `P` fitted rather than taken from each cell's own noisy win
  rate. Not a change of definition -- a cell mean already equals that expression
  by the law of total expectation -- a change of estimator.
- `NP_STOPPAGE_SHRINK_N` shrinks each side's conditional value toward its type's,
  because the true location reaches pockets where a season leaves `n = 1` cells
  that would otherwise set a baseline from one stoppage and pay its winner
  nothing.
- `.np_stoppage_cells()` is one shared cell key called by both the estimator and
  the ledger, so a stoppage cannot be priced from one cell and charged against
  another.
- Two guards: a chi-square homogeneity test on the win rate across a type's
  cells, and `NP_STOPPAGE_WIN_WARN` / `NP_STOPPAGE_WIN_ABORT` bounding the fitted
  rate. The first has to be a chi-square rather than a range -- the broken key's
  centre bounce spans 0.164 across 2 cells and the fixed key's out-of-bounds
  spans 0.160 across 12, indistinguishable by range and 32 orders of magnitude
  apart by chi-square.

The effect is a redistribution, not a level change, which is the right shape for
fixing a conditioning variable: the channel total moves -0.4% and the mean
baseline change is exactly 0.000, but 40.7% of stoppages change band and 21.5%
reprice by more than 0.10 points. Against actual scoreboard outcomes across 11
type x zone cells, r = 0.992.

Also fixed: `build_net_points_scenarios.R` passed `chains = NULL`, so every
stoppage on the published explainer page had been priced on the old,
outcome-keyed bands.

# torp 1.7.0

## Each team's players now sum to that team's own margin

The team-sum convention, `NP_TEAM_MARGIN_CONVENTION`, is on. Sydney beat Carlton
by 63; their 23 players now total +63 and Carlton's total -63. Previously only
the difference between the sides was pinned, so the same match read Sydney 208
and Carlton 145. This is the convention ESPN's Net Points uses, which is the
metric this ledger is named after, and Pete identified it correctly twice before
I accepted it.

Every row is now allocated twice, as credit to the side that gained it and as
blame to the side that conceded it, with each side keeping whatever shares the
ledger already computed. The pool is spread by defensive acts.

**Shipped against the measurements, not because of them.** Over five season
pairs and 1,794 player-pairs: within-position repeatability 0.5716 against the
shipped 0.591, and team dependence 23% against 11%. Both worse. What it buys is
a number that answers who won the game rather than who played well, and the
tightest position spread of anything tested, 2.12 against 2.78, with rucks at
+1.28 rather than the -3.49 the alternative split produces through a naming
artefact. Pete's call, made with those figures in front of him.

Rejected on the way: charging the named player half of each row instead of
keeping the ledger's shares. It repeats better, 0.6107, but breaks the ruck
mirror at a stoppage -- the winning side has two names to share the half and the
losing side has one, so the losing ruck wears 1.00 where his opponent earns
0.625 on the same contest. Stable is not the same as correct.

**The invariant, stated honestly.** `.np_team_margin()` aborts unless named plus
pool equals what each side was charged. That check has real teeth only when
`NP_TEAM_MARGIN_NAMED_SHARE` is a number: in the branch actually shipped, where
it is `NA` and each side simply doubles, the pool is DEFINED as the charge minus
the named part, so the identity holds by algebra no matter what the named part
is. A review caught me describing it as protection for the shipped path when it
is not. Chasing that down further: the replacement check I first wrote, that
every rescaled row-side sums to that side's full charge, is tautological for the
same reason, and I nearly shipped the same overclaim twice. It is kept because
it catches a structural break, a team landing on both sides of one row, but it
is now described as what it is. The check with real teeth is the last one in the
function: each team's total against the official result, an external input the
ledger cannot manufacture. An identity is not a test.

Settling the split took four reversals, because every wrong version was
internally consistent; two calculations agreeing proves nothing when both share
a misreading. `data-raw/04-analysis/np_row_audit.R` prints the shares per row for
any player, and the per-player split is now exposed as its own attribute with the
reconciliation as a separate column rather than folded into the total.

**Two NA paths that would each have taken a whole team down.** The pool spread
and the reconciliation both divide by a GROUP SUM of time on ground. A group sum
is a scalar, so one missing value made every player on that team NA rather than
just the one. Both paths are now defaulted and warned about, and both are tested
by asserting the warning fires, not merely that nothing crashed.

**The rating gate: predictive rows worse, face validity FAIL.** Within-position
repeatability 0.5663 to 0.5250 and skill score 0.1608 to 0.0582, both worse, as
the sweep predicted. Face validity fails on four of four change-detectors:
Spearman rank stability 0.49 against a 0.90 limit, key defenders going from 0 to
6 of the top 30, five of the ten biggest risers key defenders, and Tim English
climbing 175 to 13. That gate measures how much the leaderboard MOVED, and a
convention chosen for its position spread is expected to move it, so this is not
read as a defect. It is recorded because the direction matters: the change lifts
key defenders and rucks, which is the direction the defender-undervaluation work
predicted, and nobody should be surprised by the new leaderboard.

`RATING_VINTAGE` bumps to v6 and the three convention constants are wired into
`.rating_defining_constants()`. Promote before merging, or the nightly aborts:
`promote_rating_vintage.R` with PROMOTE_FROM=v5 PROMOTE_TO=v6.

# torp 1.6.0

## The player who wins the ball back is paid for it

The turnover split fired only on kicks and handballs. Any other act that lost
the ball -- a loose ball get, a gather, a handball receive, a ground kick --
paid the whole swing as blame on the actor and credited the opponent who took
it **nothing**. Measured on 2026: 15.9 such rows a match carrying 27.1 points of
swing, none of it credited, against 156 disposal turnovers a match where the
opponent is paid 88% of the time. Per event the uncredited ones are worth 1.72
against 0.54, because they are the tackled-in-possession cases.

Found by tracing one free kick end to end. Carlton lost the ball at a loose ball
get, Sydney won a free worth 3.0 points of field position, and the ledger
debited the Carlton player 2.098 while paying Sydney nothing. On frees won
straight off the opposition, 1,439 of 3,214 paid the winner nothing at an
average swing of 2.21 points.

`NP_TURNOVER_ON_ALL_ACTS` is now TRUE. Cleared the fast rating gate over
2021-2026, both arms built by the same code: MAE 26.793 -> 26.738, RMSE 34.082
-> 33.984, Brier 0.19642 -> 0.19542, log loss 0.58040 -> 0.57800, bits 0.16266
-> 0.16613. Five of five predictive rows better, none worse. Within-team
coefficient 1.018 -> 1.013 with t from 10.46 to 10.62, and face validity passes
at rank stability 0.997. Tipping is fractionally down, 0.6957 -> 0.6929, about
three tips across 1,038 matches; taken because the change repairs a defect
rather than tunes a parameter.

Per position, per game: midfielders +0.44, medium forwards +0.39, medium
defenders +0.31, rucks +0.31, key forwards +0.24, key defenders +0.23. Both
teams rise, because the margin identity constrains the gap between them and not
either total. The ten largest gainers are contested midfielders.

Gates added: `run_epr_gate_tackle_fix.R`, `np_espn_mirror_sweep.R`,
`np_defensive_share_gate.R` and `run_epr_gate_team_margin.R`, all carrying an
arms guard that aborts when two arms come back identical. It earned its place
four times in one session. The last of those gates an idea that has NOT shipped:
the convention where each team's players sum to that team's own margin, scoped in
`../docs/plans/NET-POINTS-TEAM-SUM-CONVENTION.md`.

Three tests now pin the new branch directly, including one asserting that exactly
one row reclassifies when the flag moves. It shipped first with only downstream
aggregate gates behind it, which cannot say which rows changed.

# torp 1.5.3

## Retracted: the 1.5.2 "last score of a match" fix was inert and its numbers were wrong

The code added in 1.5.2 **never fired on real data** and has been removed, along
with its tests. Measured across 2021-2026 by running `clean_pbp()` on the stored
chains for every season: it fired zero times, and the cleaned frame was already
1,262 of 1,274 matches exact against the official result.

What went wrong is worth recording. The 1-or-6 point shortfall is real, but it
lives in the **published** `pbp-data` frame, not in `clean_pbp()`'s output. The
release is filtered by `EPV_RELEVANT_DESCRIPTIONS` in `clean_model_data_epv()`,
which drops `Goal`, `Behind` and `Kick Inside 50 Result` rows. A match ending on a
score therefore has its final scoring row removed, and the last surviving row
carries the running score from just before that score. The terminating row is
present in the feed, its chain is marked, and its points are booked correctly --
none of which the 1.5.2 note said.

The measurement that justified 1.5.2 was taken on the released, filtered frame,
and the code was then written into `add_quarter_vars_dt()`, which runs before that
filter and sees a frame where the defect does not exist. Scoring a rule on one
frame and implementing it against another is the whole of the error.

**Nothing published is affected.** `build_net_points()` pins to `load_results()`,
and the v4 ratings, game logs and predictions released on 2026-09-07 never read
the running score. Consumers wanting a final score should use `load_results()`;
the last row of `pbp-data` is not it, by design of the filter.

The archive of 173 one-off analysis scripts shipped in 1.5.2 stands and is
unaffected.

# torp 1.5.2

> **RETRACTED in 1.5.3.** The "last score of a match" change below never fired on
> real data and its before/after figures were measured on the released, filtered
> frame rather than on the frame the code runs against. It has been removed. The
> archive of 173 analysis scripts, described at the end, stands. Left in place
> rather than deleted so the retraction has something to point at.


## The last score of a match is booked

A match that ends on a scoring shot never receives its terminating Goal or Behind
row from the feed, so `end_of_chain` stayed 0 across that closing chain and its
points were never booked. The running score on the final play-by-play row was
therefore 1 or 6 points short of the official result in **254 of 1,274 matches**
across 2021-2026. `add_quarter_vars_dt()` now books the score on the match's last
row when its chain scored and nothing in that chain is booked already.

Measured over the same six seasons: matches whose running score matches the
official result go from 1,009 to 1,263 of 1,274, with **zero** matches changed
that were already correct. The 11 that remain wrong are separate data gaps, two
of them play-by-play files missing large blocks of a match.

The change is narrow in what it stamps: `end_of_chain` and `scoring_team_id` are
not touched, because both feed EPV features and the defect is an unbooked score,
not a mis-drawn chain. Leaving `scoring_team_id` alone keeps `pos_points_team_id`
NA and with it the EP training label, so no model input moves.

It is not free of downstream effect, and the first draft of this note said
otherwise. `pos_points` is a next-observation-carried-backward fill of
`points_row` within the quarter, so booking the last row also fills every
previously unbooked row of that closing quarter, and `pos_is_goal` with it. Those
rows really were followed by that score, so the new value is the correct one, but
it is a change to released columns rather than a one-row edit.

Published EPV is unaffected either way: `build_net_points()` pins to
`load_results()` and uses the running score only as a live-match fallback, which
is exactly the path this repairs.

**This only affects newly cleaned play-by-play.** The `pbp-data` release holds
already-cleaned frames, so the published history keeps the short scores until
torpdata regenerates it.

Also: 173 superseded one-off analysis scripts (24,880 lines) moved to
`data-raw/04-analysis/archive-2026-09-07/`. Nothing in `R/` sources them; the
roxygen citations that named a moved file by path are repointed.

# torp 1.5.1

## Rating vintage promoted to v4

`RATING_VINTAGE` is now `"v4"`. The 1.5.0 engine flip changed every rating-defining
constant the manifest records without bumping the vintage, so
`check_vintage_alignment()` refused the first scheduled ratings run on `main`
(2026-09-06 13:25 UTC: `EPV_ENGINE: manifest "v3" vs live "v4"` and nine more
drifted constants). That is the guard doing its job; this release is the bump it
asked for. Promotion follows the v3 recipe in
`data-raw/03-ratings/promote_v4_vintage.R`: the published v3 file is re-uploaded
as `torp_ratings_v3.parquet` byte for byte (md5-checked), the manifest's v3 entry
points at it, a v4 entry carries the live constants, and canonical moves to v4
before this constant lands on `main`.

Also: `data-raw/04-analysis/np_conversion_persistence.R` measures whether the
goal-kicker's conversion credit repeats year to year before anyone moves it to the
team pool. It does (r 0.60 over 358 players, 0.50 within key forwards; 2025
conversion predicts 2026 total one for one), so it stays with the kicker (D18).

# torp 1.5.0

## EPV_ENGINE flipped to "v4": Net Points is the published EPV

Pete's decision, 2026-09-06, after the six-season fast gate
(`data-raw/04-analysis/run_epr_gate_v3v4.R`, 1,227 matches, both engines built
leak-safe by the same code): v4 won every out-of-sample row -- MAE 26.77 vs
27.18, RMSE 34.05 vs 34.58, tips 0.694 vs 0.684, Brier 0.196 vs 0.204, log
loss 0.580 vs 0.597, bits 0.163 vs 0.139 -- and the defence question stated
directly (points conceded against the opposition's defensive rating, club and
season fixed effects) reads -1.45 (t -4.0) against v3's -0.57 (t -3.5).

With it, `EPR_UNITS_SCALE_V4` (2.40): the finished v4 EPR channels are
multiplied by the measured within-team coefficient so the rating is in margin
points. It is a units factor, not a prediction change -- a linear rescale is
invisible to every regression in the gate -- and it is NOT `EPR_LOADING_DEFAULT`,
which multiplies only the data term against the prior and lost the gate on all
five rows when tried at 2.4. Chad Warner reads about +5.5 above a medium
forward instead of +2.3. Measured while deciding this: with 25+ weighted games
81% of a player's above-position mean carries into his next game, but the
lineup gate still preferred the heavier shrink (24 games) to the player-level
best-guess prior (10 games) on every row, so the shrink stays. The units
factor is part of the ratings vintage fingerprint, and v4 keeps the
standardise list it was gated with.

Also fixed: the v4 engine was still running the v2 contest split
(`compute_contest_credit()`) and discarding it; the gate is now `chain_native`.

Downstream: the match models in torpmodels were trained on v3's EPR scale and
distribution and need retraining on the v4 ratings history before predictions
are trusted; the production match gate runs once after that as the guardrail.
inthegame-blog issue #649 carries the units-gate re-anchor and channel labels.
## `EPV_ENGINE = "v4"`: the Net Points ledger as an engine (not yet the default)

`create_player_game_data(epv_engine = "v4")` builds the player-game frame from
`build_net_points()` under the full v4 rule set (difficulty credit, contest
split, routing by act, stoppages allocated). Three channels to start, Pete's
choice for EPR: own acts in `epv_disp`, what he won back (turnovers, contests,
stoppages) in `epv_recv`, his share of the pools plus the reconciliation
residual in `epv_spoil`; `epv_hitout` is zero and `epv` equals `net_points`
exactly. The four column names are kept so EPR's plumbing reads them unchanged;
EPR's own structure is open (Pete, 2026-09-06). The margin comes from the
official results, with the play-by-play's running score as the live fallback,
and every match sums to it (max error 1.6e-14 on 2026). A player with net
points but no play-by-play act (two player-matches, 1.56 points in 2026) has
his value re-spread within his team by time on ground, so nothing leaves the
frame. `np_difficulty_terms_for_season()` fits a season's difficulty and
contest models on the season before (2021 on 2022) with narrowed loads, for the
per-season pipeline loop. `data-raw/04-analysis/run_epr_gate_v3v4.R` is the
fast-gate and face-validity comparison against v3; the default stays `"v3"`
until that gate is read.

Measured on 2026: the v4 frame builds in 0.4 minutes; correlation with v3's
`epv` 0.84 (Spearman 0.79); per-player-game sd 3.97 against v3's 4.25.

Data fact found on the way: the play-by-play's booked final score differs from
the official result in 52 of 213 matches of 2026, always by exactly 1 or 6
points, because points are booked on the row after the scoring act and a match
whose last act is a score has no such row.
# torp 1.4.9

## Net Points: stoppages and ruck credit (D15)

`build_net_points(stoppages = "allocate")` (difficulty credit only) stops dropping
centre bounces, ball-ups and throw-ins. Each is valued at a neutral baseline for
its type and 20m band of the ground (`.np_stoppage_baseline()`: the average
first-possession value over both winners; ball-ups run from -2.05 at the home
side's defensive end to +1.41 at its goal, centre bounces sit at 0.1), the row
before it is paid up to that baseline and the stoppage row from the baseline to
the first possession, so the pair still sums to what it did. The swing splits
`NP_STOPPAGE_LOSER_SHARE` (0.5) to the side that lost the ball; each side's half
follows `NP_STOPPAGE_SPLIT` by how the ball came out (a gather from a hitout:
ruck 50 / player 30 / pool 20; a ground ball or free: player 50 / pool 30 /
rucks 20; a ruck's own hard-ball get: his 80 / pool 20), with a team's ruck
share divided by `hitouts_to_advantage` (winner) or `ruck_contests - hitouts`
(loser) that match. New `np_stoppage` column and `stoppage_*` payment roles.

2026: 18,325 stoppage rows carrying 57 points of gross swing a match, all of
which used to fall into the reconciliation residual (median |residual| per
player-game 0.104 -> 0.021). Gawn and Grundy lead the column; rucks' stoppage
credit correlates 0.63 with hitouts to advantage; rucks (lineup code RK; R is
a rover) rise from 4.1 to 5.4 net points a game. Split-half reliability 0.680 -> 0.665, so the shares go to
the year-over-year test with the rest. This replaces the old EPV's flat
per-hitout constant, whose sign was wrong.
# torp 1.4.8

## Net Points explainer rebuilt on the v4 rules, plus a defender's page

`data-raw/04-analysis/build_net_points_explainer.R` now reads the ledger's own
payment table (`build_net_points(return_payments = TRUE)`) instead of
re-deriving the split, scores under `credit = "difficulty"` by default with the
models fitted leak-safe on 2025, and explains each act as decision + surprise
with the kick's chance of being lost. A fourth role, a contest he won, joins
the page. `NP_PLAYER` / `NP_OUT` / `NP_CREDIT` select the player, the output
file and the rule set; the site now carries Papley (a forward) and Harris
Andrews (a defender, 9.35 net points in the same game against a published EPV
of 3.2) under a Net Points menu.

Review findings on the context spread, fixed: a caller-supplied `contest_pairs`
table with a repeated key was truncated to its last row by the update-join (now
summed first), the per-pool renormalising divisor was inert and is gone, and
the context test now asserts the exact weights from `NP_CONTEXT_WEIGHTS`.

## Net Points: routing by act, and a context spread (D11, D12)

Step 4 of the v4 credit rules. Under difficulty credit the ball-winner's share
of a ceded ground ball follows WHAT he did to win it (`NP_BALL_WINNER_SHARE_BY_ACT`:
a mark or a free 80%, a loose ball 30%) instead of the flat
`NP_BALL_WINNER_SHARE`. And `spread = "context"` shares a team pool by Pete's
mix of evidence: observed attacker-versus-defender pairings from chains contest
targets (`.np_contest_pairs()`, 12.7 a match in 2026), box-score defensive acts,
the positional mirror and time on ground, weighted by `NP_CONTEXT_WEIGHTS`
(0.4 / 0.3 / 0.2 / 0.1); a component with no support in a pool drops out. Pools
are now keyed by the disposer as well, so the pairing can be looked up.

Measured on 2026: 1,574 of 9,169 defensive pools carry pairing evidence; the
context spread moves the forward/defender gap from 2.03 (matchup) to 1.61 points
a game, but split-half reliability falls from 0.680 to 0.649, so `"matchup"`
stays the default and the weights are left to the year-over-year test.

Review finding fixed here: the opposition that receives a contest cession is
now the OTHER team on the match roster, never the team of the resolution row,
and the roster includes chains-only actors (a spoiler who never touched the
ball in PBP). A winner not on that roster sends his share to the pool with a
logged count rather than creating a phantom row.

## Net Points: contested kicks split at the contest (D8)

Step 3 of the v4 credit rules. A kick that resolves at a fought contest (a
spoil, a contested or pack mark, or any mark the defence took; 19,574 in
2026, 88.6% won by the defence) now splits its surprise once more, using the
v3 aerial branch models: the contest surprise (branch value minus EV) goes to
whoever won the contest, the ground-ball surprise (what happened after the
fall of the ball) to whoever possessed next. A same-team winner is paid
directly; a defensive winner is paid through the new `np_contest_won` column
at `NP_CONTEST_WINNER_SHARE` for how the contest was won (a mark 80%, a spoil
50%), the rest joining the defensive pool. This is the term the 2026-08 build
paid to nobody, and it is what stops a spoil the attack regathers from
debiting the spoiler. Harris Andrews in the Sydney game: 3.77 (flat) -> 6.57
(step 2) -> 9.01; split-half reliability 0.645 -> 0.671.

## Net Points: PBP rows keep PBP's player and team

Review finding on the chains-aware ledger: `.np_sequence()` took `player_id`
and `team_id` for PBP rows from chains and only checked that the description
agreed, so a chains/PBP disagreement on the actor would have moved credit
silently. PBP rows now keep PBP's own player and team, and any disagreement on
description, player or team at the same key aborts.

## Net Points: difficulty credit (`credit = "difficulty"`)

Step 2 of the v4 credit rules (`docs/plans/EPV-V4-CREDIT-RULES.md`, D5-D7, D9).
Every disposal the difficulty model can score splits into a decision term
(`EV - before`, always the disposer's) and a surprise (`after - EV`). Retained:
the disposer keeps `p` of the surprise and the receiver `1 - p`, with `p` the
modelled chance of losing the ball. Turnover: the disposer keeps
`NP_BLAME_SHARE` (0.30) of the surprise, the defence is credited the rest.
`NP_OFFENCE_POOL_SHARE` (0.10) of every non-turnover disposal goes to the
attacking team's pool, reported in the new `np_team` column. The 4.2% of
disposals the model cannot score fall back to the flat rule, and the count is
logged. The row identity is asserted, not assumed: the terms must rebuild
every row's `delta_epv` to 1e-9 or the build aborts.

The flat rule is untouched: `credit = "flat"` (the default) reproduces the
previous build to 5e-14 on all 9,792 player-matches of 2026. Both modes go
through one per-row split (`.np_credit_terms()`: own / receiver / team pool /
ceded), which is the role tagging D3 asks for.

Measured on 2026, in-sample fit, default shares: 95.8% of disposals scored,
conservation 4e-14, Spearman against flat 0.89 over player-games, split-half
reliability 0.645 -> 0.656. Harris Andrews in the Sydney game goes 3.77 -> 6.57
while Papley is unchanged (23.19 -> 23.17); the season top 12 gains Richards,
Bontempelli, Dempsey and Daicos instead of being twelve forwards. The
`difficulty_terms` argument lets a share sweep fit the models once.
## Net Points reads chains alongside PBP

`build_net_points(chains=)` takes the raw chains for the same matches. The
allocation is **identical** -- proven on all 213 matches of 2026 with `identical()`
in `data-raw/04-analysis/np_chains_ledger_equivalence.R` -- but every disposal in
the ledger now carries `resolve_desc` / `resolve_team` / `resolve_player`: the
chains row that ended it (a spoil, a contested mark, a goal), which PBP drops.
PBP is an exact subset of chains on `(match_id, display_order)`; the 82,718
chains-only rows in 2026 are spoils, contest targets, goals, behinds and fumbles.
Nothing uses the resolution yet. It is step 1 of the v4 credit rules
(`docs/plans/EPV-V4-CREDIT-RULES.md`).

Two facts the resolution column showed on day one: the spoiler is the next
possessor only 7.8% of the time, and 3,860 kicks that resolve to a behind were
classed as turnovers because the next state is the opposition's kick-in.

## A score is followed by a restart, never a turnover

Fixed the second of those. The defence was being paid 30% of every behind
conceded -- 1,981 points across 2026, 9.3 a match -- and the kick-in taker took
60% of that, almost all of it to half-backs. Adjacency now treats a row after
which the running score moved as chain-terminal, the same status a centre
bounce already had; that covers goals, behinds, rushed behinds and dribbled
scores in one rule, with or without chains. PBP books the points on the row
AFTER the scoring act, so `home_points` / `away_points` are now required
ledger columns. Per-position means all fall (the shooter wears the whole
behind, the defence stops collecting it): defenders 2.23 -> 1.67, forwards
4.07 -> 3.49 points a game. Defenders now win 43.3% of turnovers, not 47.3%;
the difference was kick-ins.

# torp 1.4.7

## Net Points -- the level constraint was wrong

`build_net_points(level=)` now defaults to `"sum"`, not `"half_margin"`.

The identity that matters -- `sum(home) - sum(away) == margin` -- holds under
BOTH modes; pinning the match total is the whole of Oliver's identity.
`"half_margin"` added a second, cosmetic constraint (each team lands on
`margin/2`) at a cost that measurement did not support. Over 211 matches of 2026:

| | `sum` | `half_margin` |
|---|---|---|
| median \|np_residual\| | 0.10 | 2.64 |
| residual as % of \|net_points\| | 3% | 102% |
| Spearman(raw, final) | 0.9993 | 0.7425 |

* Under `"half_margin"` the correction was **larger than the thing it
  corrected**, and it **reordered players** -- it spreads by time on ground and
  TOG varies (`cor(np_residual, tog) = -0.481`). The docs claimed it "shifts the
  level without reordering anyone"; that was asserted, never measured, and
  false. Corrected.
* Papley in the 2026 R26 Sydney-Brisbane game moves +20.52 to +23.96, his
  residual falling from -3.56 to -0.11.
* Worth knowing: the raw ledger tracks a team's OWN SCORE (cor 0.906) better
  than the margin (0.786), because `delta_epv` measures scoring production
  against expectation. Both teams read positive in a high-scoring game and the
  median distance from `margin/2` is 61.4 points, systematically. That is the
  deep form of the missing-defence gap, and flattening it was never a fix.


# torp 1.4.6

## Net Points -- EPV as a conservation ledger

New, and deliberately parallel to the published pipeline: nothing in
`get_player_game_ratings()` or EPR changes. `build_net_points()` allocates the
ACTUAL match margin across players instead of fitting a scale to it, which is
Dean Oliver's Net Points model stated for AFL. Team totals differ by exactly the
final margin -- 211 matches of 2026, max error 5.2e-14 points.

Design, measurements and the dead end not to re-open:
`../docs/plans/EPV-NET-POINTS.md`.

* **The margin is the total; the work is dividing it up.** `delta_epv`
  telescopes along a chain, so summed per team it already lands within a goal
  of the margin (cor 0.9857, median error 4.9 points) with nothing fitted. The
  published player `epv` turns that into a 40-point error, which is what the
  abandoned per-channel calibration was compensating for.
* **Defensive credit, which the ledger had none of.** PBP carries 24 act types
  and not one is defensive -- no spoils, tackles, smothers or pressure acts. So
  across 32,938 turnovers the disposer was debited 43,728 points while the
  player who won the ball got -0.029 on average. `NP_DEFENSIVE_SHARE` now moves
  a share to the winning side.
* **Paid to the player observed to win the ball, not to a positional mirror.**
  Routing the whole pool by mirror made the forward/defender gap WIDER as more
  defensive credit was paid (2.19 -> 2.58 points per game), because a
  midfielder's mirror is another midfielder. Defenders win 47.3% of turnovers
  and lose 22.8%, and the winner is directly observable as the actor on the next
  row. `NP_BALL_WINNER_SHARE` pays him; the rest still spreads by matchup for
  the pressure that forced it.
* **Adjacency is taken on the unfiltered sequence.** Filtering rows out and
  then taking the next row steps over the gap, so the "next actor" became
  whoever followed it. Found in review: 15,556 disposals (10.2%) had the wrong
  next team, 19.2% of detected turnovers were actually restarts, and **2,586
  goals -- half of all goal kicks -- were classified as turnovers**, firing the
  defensive pool and paying the opposition for conceding. A restart is now
  chain-terminal: neither a retained disposal nor a turnover. Every conservation
  test stayed green throughout, because it is a pure attribution error.
* **The centre-bounce artifact is excluded by rule, not by luck.** `exp_pts` is
  exactly 0 on every Centre Bounce row, making its delta phantom value (+4,461
  points in 2026). It was previously dropped only as a side effect of an
  `is.na(team)` filter.
* **No centring, standardisation, TOG scaling or opponent adjustment.** Each
  subtracts an expectation, and that is what breaks the identity. They stay at
  the EPR layer, whose job is prediction.

`check_net_points_conservation()` asserts the identity. Note what it cannot see:
conservation holds no matter WHICH players are paid, so the shares are not
identifiable from it and must never be fitted against it.


# torp 1.4.5

## EPV v3 -- mirror allocator

Two defects in `allocate_by_mirror()` (`R/epv_v3_mirror.R`). Both are **latent,
not live**: `EPV_CONT_LOSS_ALLOC` is `"team"`, so the mirror branch does not run
in production. The code is kept deliberately (the constant's own docs say the
reasoning was sound and only the data settled it), so it is worth having it work.

* **A documented fallback that silently did the opposite.** The `weights` @param
  said "NULL falls back to the flat share"; the code returned an EMPTY table, so
  the entire contest-debit allocation vanished with nothing logged.
  `build_mirror_weights()` returns NULL on three separate paths (no positions,
  fewer than 500 named duels, no rows after the join), so this was reachable. It
  now allocates a genuine flat share and warns that positional matchup is off.

* **A conservation check that could not see the loss it was checking for.** The
  check compared `owed` against `got`, but `owed` was computed AFTER the
  `!is.na(winner_pos)` filter -- filtered-to-filtered always balances, so a
  debit belonging to a winner with no known position was dropped silently and
  the check still passed. The dropped share is now measured before the filter
  and warned on.

## Tests

* Replaced a test that restated the credit formula in its own body -- its final
  assertion was `x + (-x) == 0`, true for any x, exercising no production code.
  It now calls `score_contests()` with stub models. Mutation-tested: swapping
  `p` and `(1 - p)` fails 4 assertions. Also recorded what it does NOT catch
  (a defence-win sign inversion passes, because the credits go through `abs()`).

# torp 1.4.4

## Contests

* **`extract_contests()` gains a `population` argument, so a contest W-L can be
  built on the project's own duel definition.** `EPV3_DUEL_OUT`
  (`epv_v3_duels.R`) excludes `Uncontested Mark` and `Mark On Lead` as
  "receptions rather than duels" -- `Mark On Lead` records a defence win 0.0%
  of the time across 19,247 events -- but `extract_contests()` reached them
  anyway through a second, looser constant (`CHAINS_MARK_WIN_DESCS`). Measured
  on 2026 chains, that is **2,921 of 3,914 rows (74%) of the same-team `mark`
  branch counted as won contests that were never contested**.

  `population` defaults to `EPV3_CONTEST_POPULATION` -- the same global switch
  the v3 contest channel uses, currently `"all"` -- so **the default output is
  unchanged and no existing caller moves**. Pass `population = "duel"` for
  anything presented as a contest won or lost.

  Anchor check: on 2026 the duel population gives a league forward win rate of
  **0.186**, reproducing the 0.182 recorded independently from raw chains in the
  2026-08-15 decision. The unrestricted population gives 0.427, more than double.

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
