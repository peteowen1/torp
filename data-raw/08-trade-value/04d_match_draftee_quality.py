# Join 1991-2015 National Draft picks (footywire) to career Brownlow votes and
# All-Australian selections (afltables, Wikipedia), for the pick value curve.
# Inputs: footywire_drafts.csv, afltables_player_seasons.csv, all_australian.csv
#   (the last two from 04c_parse_quality_sources.py).
# Output: torpdata/data/raw/trade_value/04d_draftee_quality.csv, one row per pick,
#   every input column kept, plus the match fields and how each was matched.
#
# Run: python -I data-raw/08-trade-value/04d_match_draftee_quality.py

import csv, re, unicodedata, collections, os

RAW = "C:/dev/torpverse/torpdata/data/raw"
TV = os.path.join(RAW, "trade_value")


def norm(s):
    s = unicodedata.normalize("NFKD", s).encode("ascii", "ignore").decode()
    s = re.sub(r"\(.*?\)", " ", s).lower().replace("-", " ")
    s = " ".join(t for t in s.replace(".", " ").split() if t not in ("jr", "sr", "jnr", "snr"))
    return " ".join(re.sub(r"[^a-z ]", "", s).split())


ALIAS = {"kangaroos": "north melbourne", "brisbane bears": "brisbane", "brisbane lions": "brisbane",
         "greater western sydney": "gw sydney", "gws": "gw sydney", "footscray": "western bulldogs",
         "south melbourne": "sydney"}


def team_key(s):
    s = norm(s)
    for k, v in ALIAS.items():
        if s.startswith(k): return v
    return s


def num(x):
    try: return float(x)
    except (TypeError, ValueError): return 0.0


# ---- afltables careers by player page ----
seasons = list(csv.DictReader(open(os.path.join(TV, "afltables_player_seasons.csv"), encoding="utf-8")))
car = collections.defaultdict(lambda: {"name": None, "by_season": collections.defaultdict(lambda: [0.0, 0.0])})
for r in seasons:
    last, _, first = r["Player"].partition(", ")
    c = car[r["player_href"]]
    c["name"] = norm(f"{first} {last}")
    c.setdefault("teams", collections.defaultdict(set))[int(r["season"])].add(team_key(r["team"]))
    s = c["by_season"][int(r["season"])]
    s[0] += num(r["GM"]); s[1] += num(r["BR"])
by_name = collections.defaultdict(list)
by_initial = collections.defaultdict(list)   # "surname|first initial": fallback for Matthew vs Matt
for href, c in car.items():
    by_name[c["name"]].append(href)
    if c["name"]: by_initial[c["name"].split()[-1] + "|" + c["name"][0]].append(href)


def initial_key(n):
    return n.split()[-1] + "|" + n[0] if n else ""
print("afltables players:", len(car))

# ---- All-Australian: player cells only ----
aa_rows = [r for r in csv.DictReader(open(os.path.join(TV, "all_australian.csv"), encoding="utf-8"))
           if "Australian_rules_football_positions" not in r["wiki_href"]
           and "Coach" not in r["wiki_href"] and r["row_label"] != "Coach:"]
aa_by = collections.defaultdict(int)   # afltables href -> selections
aa_unmatched = []
for r in aa_rows:
    y, n = int(r["year"]), norm(r["player_text"])
    cands = [h for h in by_name.get(n, []) if y in car[h]["by_season"]]
    if len(cands) > 1:
        m = re.search(r"\(([^)]*)\)", r["player_text"])
        tk = team_key(m.group(1)) if m else ""
        cands = [h for h in cands if any(tk and (tk in t or t in tk) for t in car[h]["teams"][y])]
    if len(cands) == 1: aa_by[cands[0]] += 1
    else: aa_unmatched.append((y, r["player_text"], len(cands)))
per_year = collections.Counter(int(r["year"]) for r in aa_rows)
print("AA player cells:", len(aa_rows), "per year min/max", min(per_year.values()), max(per_year.values()),
      "; matched", len(aa_rows) - len(aa_unmatched), "; unmatched", len(aa_unmatched))
for u in aa_unmatched[:15]: print("  AA unmatched:", u)

# ---- draftees ----
fw = [r for r in csv.DictReader(open(os.path.join(RAW, "footywire_drafts.csv"), encoding="utf-8"))
      if r["is_pass"] == "FALSE" and r["draft_year"].isdigit() and 1991 <= int(r["draft_year"]) <= 2015
      and r["flag"] != "R" and r["pick"].isdigit() and int(r["pick"]) <= 60 and r["games_since_drafted"] != ""]
out, how = [], collections.Counter()
for r in fw:
    dy, fg = int(r["draft_year"]), num(r["games_since_drafted"])
    best, bestgap, via = None, None, "name+games"
    n = norm(r["player"])
    pool = by_name.get(n, [])
    if not pool:
        pool, via = by_initial.get(initial_key(n), []), "surname+initial+games"
    for h in pool:
        bs = car[h]["by_season"]
        if not any(dy < s <= dy + 8 for s in bs): continue  # must play within 8 seasons of the draft
        g = sum(v[0] for s, v in bs.items() if s > dy)
        gap = abs(g - fg)
        if bestgap is None or gap < bestgap: best, bestgap = h, gap
    rec = dict(r)
    tol = max(5, 0.1 * fg) if via == "name+games" else (max(2, 0.03 * fg) if fg > 0 else -1)
    if best is not None and bestgap <= tol:
        bs = car[best]["by_season"]
        rec.update(afl_href=best, afl_games=sum(v[0] for s, v in bs.items() if s > dy),
                   brownlow_votes=sum(v[1] for s, v in bs.items() if s > dy),
                   aa_selections=aa_by.get(best, 0), match=via)
    elif fg == 0:
        rec.update(afl_href="", afl_games=0, brownlow_votes=0, aa_selections=0, match="no games")
    else:
        rec.update(afl_href=best or "", afl_games="", brownlow_votes="", aa_selections="",
                   match="UNMATCHED" if best is None else f"games gap {bestgap:.0f}")
    how[rec["match"] if not rec["match"].startswith("games gap") else "games gap"] += 1
    out.append(rec)

cols = list(fw[0].keys()) + ["afl_href", "afl_games", "brownlow_votes", "aa_selections", "match"]
with open(os.path.join(TV, "04d_draftee_quality.csv"), "w", newline="", encoding="utf-8") as fh:
    w = csv.DictWriter(fh, fieldnames=cols); w.writeheader(); w.writerows(out)
played = [r for r in out if num(r["games_since_drafted"]) > 0]
ok = sum(r["match"] in ("name+games", "surname+initial+games") for r in played)
print(f"draftees: {len(out)}; with games {len(played)}, matched {ok} ({100*ok/len(played):.1f}%)", dict(how))
aa_draftees = sum(num(r["aa_selections"]) for r in out if r["match"] in ("name+games", "surname+initial+games"))
print("AA selections landing on matched 1991-2015 draftees:", int(aa_draftees))
for r in [r for r in played if r["match"] not in ("name+games", "surname+initial+games")][:12]:
    print("  unmatched:", r["draft_year"], r["pick"], r["player"], r["games_since_drafted"], r["match"])
