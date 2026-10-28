"""Full-season Squiggle leaderboard with the two frozen_psr_cost.R arms added.

Squiggle tips and games for 2026 are read from torpdata/data/raw/squiggle_2026/
(saved raw from api.squiggle.com.au). Each model is scored on every completed
game it tipped: tips correct, bits (Squiggle's score for win probabilities,
higher is better) and MAE (average margin error in points, lower is better).
The two TORP backfill arms are scored the same way on the same games.
Usage: python data-raw/04-analysis/frozen_psr_squiggle.py
"""
import collections
import glob
import json
import math

import pyarrow.parquet as pq

D = "C:/dev/torpverse/torpdata/data/raw/squiggle_2026/"
tips = json.load(open(D + "tips_2026.json", encoding="utf-8"))["tips"]
games = [g for g in json.load(open(D + "games_2026.json", encoding="utf-8"))["games"] if g.get("complete") == 100]
sq_names = sorted({g["hteam"] for g in games})


def to_sq(n):
    if n == "GWS Giants":
        return "Greater Western Sydney"
    m = [s for s in sq_names if n.startswith(s)]
    assert m, n
    return max(m, key=len)


def bits(p, hwin):
    p = min(max(p, 1e-9), 1 - 1e-9)
    if hwin == 0.5:
        return 1 + 0.5 * math.log2(p * (1 - p))
    return 1 + math.log2(p if hwin == 1 else 1 - p)


by_key = {(g["hteam"], g["ateam"], str(g["date"])[:10]): g for g in games}
rows = {}
agg = collections.defaultdict(lambda: [0, 0.0, 0.0, 0])
done = {g["id"] for g in games}
for t in tips:
    if t["gameid"] not in done or t["err"] in (None, ""):
        continue
    a = agg[t["source"]]
    a[0] += int(t["correct"] or 0); a[1] += float(t["bits"]); a[2] += float(t["err"]); a[3] += 1
for s, (c, b, e, n) in agg.items():
    rows[s] = (c, b, e / n, n)

# Anchor from squiggle.com.au/leaderboard (2026, end of season), read 2026-10-28.
dbtd = rows["Don't Blame the Data"]
print(f"anchor Don't Blame the Data: tips {dbtd[0]} (site 156), bits {dbtd[1]:.2f} (site 55.20), MAE {dbtd[2]:.2f} (site 25.41)")
assert dbtd[0] == 156 and abs(dbtd[1] - 55.20) < 0.01 and abs(dbtd[2] - 25.41) < 0.01, "does not reproduce the site"

for arm in ("frozen", "fixed"):
    o = {}
    for f in glob.glob(f"data-raw/outputs/frozen-psr-cost/{arm}/_profile/2026/round_*.parquet"):
        d = pq.read_table(f).to_pydict()
        for i in range(len(d["match_id"])):
            o[(to_sq(d["home_team"][i]), to_sq(d["away_team"][i]), str(d["start_time"][i])[:10])] = (d["pred_margin"][i], d["pred_win"][i])
    n = c = 0; b = e = 0.0; miss = []
    for k, g in by_key.items():
        if k not in o:
            miss.append(k); continue
        pm, pw = o[k]; act = g["hscore"] - g["ascore"]
        hwin = 1 if act > 0 else 0 if act < 0 else 0.5
        n += 1; c += int(act != 0 and (pw > 0.5) == (act > 0)); b += bits(pw, hwin); e += abs(pm - act)
    rows[f"TORP rebuild, {arm} PSR"] = (c, b, e / n, n)
    print(f"{arm}: scored {n} of {len(by_key)} games; unmatched {len(miss)} {miss[:3]}")

full = sorted(((s, v) for s, v in rows.items() if v[3] >= len(games) - 3), key=lambda x: -x[1][1])
print(f"\nfull season, {len(games)} completed games; ranked by bits (higher is better), MAE lower is better")
for rank, (s, v) in enumerate(full, 1):
    print(f"{rank:2d} {s:30s} tips {v[0]:3d}  bits {v[1]:6.2f}  MAE {v[2]:5.2f}  n {v[3]}")
print("left out (tipped fewer games):", [(s, v[3]) for s, v in rows.items() if v[3] < len(games) - 3])
