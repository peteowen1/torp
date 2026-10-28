"""How much the discount rate matters: Blakey, picks 1-32, trade 11, and the
player-vs-pick tilt at 0%, 5% and 10% a season.

Reads 03's undiscounted per-season values (ev_2027..ev_2034), 04's per-draftee
season values (stored discounted at 10%, undone here) and 05's assets.
Pete, 2026-10-28: the default stays at 10%; this is the sensitivity check.
Usage (from the torp root): python data-raw/08-trade-value/08_discount_sensitivity.py
"""
import collections
import csv
import math

O = "C:/dev/torpverse/torpdata/data/raw/trade_value/"
proj = {r["player_name"]: r for r in csv.DictReader(open(O + "03_projected_value_2026.csv", encoding="utf-8"))}
seasons = list(csv.DictReader(open(O + "04_pick_seasons.csv", encoding="utf-8")))
assets = list(csv.DictReader(open(O + "05_trade_assets.csv", encoding="utf-8")))
PICKS = [1, 2, 4, 8, 16, 32]


def run(d):
    v8 = collections.defaultdict(float)
    for r in seasons:
        k = int(r["k"])
        v8[(r["draft_year"], int(r["pick"]))] += float(r["v"]) / 0.9 ** (k - 1) * (1 - d) ** (k - 1)
    xs = [(math.log(p), v) for (_, p), v in v8.items()]
    mx = sum(x for x, _ in xs) / len(xs); my = sum(y for _, y in xs) / len(xs)
    sl = sum((x - mx) * (y - my) for x, y in xs) / sum((x - mx) ** 2 for x, _ in xs)
    ic = my - sl * mx

    def pv(p):
        return max(ic + sl * math.log(max(p, 1)), 0)

    def val(a):
        if a["type"] == "player":
            r = proj[a["item"]]
            return sum(float(r[f"ev_{y}"]) * (1 - d) ** (y - 2027) for y in range(2027, 2035))
        if a["type"] == "pick_2026":
            return pv(int(float(a["pick"])))
        return pv(float(a["exp_pick"])) * (1 - d) ** (int(a["year"]) - 2026)

    pl = collections.defaultdict(float); pk = collections.defaultdict(float); t11 = 0.0
    for a in assets:
        v = val(a)
        dd = pl if a["type"] == "player" else pk
        dd[(a["trade"], a["to_club"])] += v; dd[(a["trade"], a["from_club"])] -= v
        if a["trade"] == "11":
            t11 += v if a["to_club"] == "North Melbourne" else -v
    by = collections.defaultdict(list)
    for k in set(pl) | set(pk):
        by[k[0]].append(k)
    ahead = behind = 0; spl = snet = 0.0
    for ks in by.values():
        k = max(ks, key=lambda k: pl.get(k, 0))
        if pl.get(k, 0) <= 0.5:
            continue
        net = pl.get(k, 0) + pk.get(k, 0)
        ahead += net > 1; behind += net < -1; spl += pl.get(k, 0); snet += net
    blakey = val({"type": "player", "item": "Nick Blakey"})
    return dict(blakey=blakey, picks=[pv(p) for p in PICKS], t11=t11, ahead=ahead, behind=behind,
                cover=100 * (spl - snet) / spl)


res = {d: run(d) for d in (0.0, 0.05, 0.10)}
print("value over 8 seasons at each discount rate (higher is more valuable)")
print(f"{'':16s}" + "".join(f"{int(d * 100):>9d}%" for d in res))
print(f"{'Blakey':16s}" + "".join(f"{r['blakey']:10.1f}" for r in res.values()))
for i, p in enumerate(PICKS):
    print(f"{'pick ' + str(p):16s}" + "".join(f"{r['picks'][i]:10.1f}" for r in res.values()))
print(f"{'Blakey / pick 1':16s}" + "".join(f"{r['blakey'] / r['picks'][0]:10.2f}" for r in res.values()))
print(f"{'trade 11, North':16s}" + "".join(f"{r['t11']:10.1f}" for r in res.values()))
print(f"{'player side wins':16s}" + "".join(f"{r['ahead']:>6d}/{r['ahead'] + r['behind']:<3d}" for r in res.values()))
print(f"{'picks cover %':16s}" + "".join(f"{r['cover']:10.0f}" for r in res.values()))
