# Pick value by band on a QUALITY measure, against career games and the AFL index
# (Pete, 2026-11-26: Brownlow votes plus a bonus per All-Australian selection).
# Input: 04d_draftee_quality.csv (04d_match_draftee_quality.py), 06_dvi.csv.
# Output: torpdata/data/raw/trade_value/04e_pick_curve_quality.csv
# Every value is a band's mean as a share of the picks 1-3 mean; higher = worth more.
#
# Run: python -I data-raw/08-trade-value/04e_pick_curve_quality.py

import csv, random

T = "C:/dev/torpverse/torpdata/data/raw/trade_value/"
AA_WEIGHT = 10   # votes per All-Australian selection; 0 to 20 moves picks 7-20 by <= 4 points
BANDS = [(1, 3), (4, 6), (7, 10), (11, 15), (16, 20), (21, 30), (31, 45), (46, 60)]

r = list(csv.DictReader(open(T + "04d_draftee_quality.csv", encoding="utf-8")))
assert len(r) > 1400, len(r)
assert all(x["match"] != "UNMATCHED" for x in r if float(x["games_since_drafted"] or 0) > 0)
# Father-son (F) and academy (A) players were taken at late picks by rule, not
# bought on the open market (pre-bidding: Ablett pick 40, Hawkins 41), so they
# say nothing about what a tradeable pick is worth. They lifted picks 31-45 from
# 19% to 24% of a top-3 pick (2026-11-26 run). Left out; their count is printed.
n0 = len(r)
r = [x for x in r if x["flag"] not in ("F", "A")]
print(f"left out {n0 - len(r)} father-son / academy picks; {len(r)} remain")
# First-time draftees only: a player with AFL games at or before his draft year
# (a redraft, or a delisted veteran taken again) is a known quantity, not a
# draft gamble, and 04d would credit his later career to every pick that took
# him (Martin Pike: picks 9, 42 and 33). 48 of the 62 sit past pick 30.
seasons = {}
for x in csv.DictReader(open(T + "afltables_player_seasons.csv", encoding="utf-8")):
    seasons.setdefault(x["player_href"], set()).add(int(x["season"]))
n1 = len(r)
r = [x for x in r if not (x["afl_href"] and x["match"] != "no games"
                          and min(seasons[x["afl_href"]]) <= int(x["draft_year"]))]
print(f"left out {n1 - len(r)} players with AFL games at or before their draft year; {len(r)} remain")
dvi = {int(x["pick"]): float(x["points"]) for x in csv.DictReader(open(T + "06_dvi.csv", encoding="utf-8"))}

measures = {
    "quality": lambda x: float(x["brownlow_votes"] or 0) + AA_WEIGHT * float(x["aa_selections"] or 0),
    "votes_only": lambda x: float(x["brownlow_votes"] or 0),
    "games_beyond_100": lambda x: max(float(x["games_since_drafted"] or 0) - 100, 0),
}
by = {b: [x for x in r if b[0] <= int(x["pick"]) <= b[1]] for b in BANDS}


def ratio(f, samp):
    t = sum(map(f, samp[BANDS[0]])) / len(samp[BANDS[0]])
    return {b: sum(map(f, samp[b])) / len(samp[b]) / t for b in BANDS}


# Paired bootstrap: the same resampled players score both measures, so the
# quality-minus-games difference has its own interval (the two separate
# intervals overlap and cannot answer "do games overstate the middle picks").
random.seed(42)
boots, diffs = [], []
for _ in range(2000):
    samp = {b: random.choices(v, k=len(v)) for b, v in by.items()}
    q, g = ratio(measures["quality"], samp), ratio(measures["games_beyond_100"], samp)
    boots.append(q)
    pooled = [x for b in ((7, 10), (11, 15)) for x in samp[b]]
    pq = sum(map(measures["quality"], pooled)) / len(pooled) / (sum(map(measures["quality"], samp[BANDS[0]])) / len(samp[BANDS[0]]))
    pg = sum(map(measures["games_beyond_100"], pooled)) / len(pooled) / (sum(map(measures["games_beyond_100"], samp[BANDS[0]])) / len(samp[BANDS[0]]))
    diffs.append({**{b: q[b] - g[b] for b in BANDS}, "7-15": pq - pg})
top = sum(dvi[p] for p in range(1, 4)) / 3
point = {m: ratio(f, by) for m, f in measures.items()}
rows = []
for b in BANDS:
    q = sorted(x[b] for x in boots)
    picks = [p for p in range(b[0], b[1] + 1) if p in dvi]
    afl = (sum(dvi[p] for p in picks) / len(picks) / top) if len(picks) == b[1] - b[0] + 1 else None
    rows.append({"band": f"{b[0]}-{b[1]}", "n": len(by[b]),
                 **{m: round(point[m][b], 3) for m in measures},
                 "quality_lo95": round(q[50], 3), "quality_hi95": round(q[1949], 3),
                 "q_minus_games_lo95": round(sorted(d[b] for d in diffs)[50], 3),
                 "q_minus_games_hi95": round(sorted(d[b] for d in diffs)[1949], 3),
                 "afl_index": None if afl is None else round(afl, 3)})
with open(T + "04e_pick_curve_quality.csv", "w", newline="", encoding="utf-8") as fh:
    w = csv.DictWriter(fh, fieldnames=list(rows[0])); w.writeheader(); w.writerows(rows)
for x in rows: print(x)
d = sorted(x["7-15"] for x in diffs)
print(f"picks 7-15 pooled, quality minus games-past-100 (share of top-3): 95% interval {d[50]:+.3f} to {d[1949]:+.3f}")
# Anchors, chosen before reading the bands: a top-3 pick beats picks 21+ and 46-60 on every measure.
for m in measures:
    assert point[m][(21, 30)] < 1 and point[m][(46, 60)] < point[m][(4, 6)], m
