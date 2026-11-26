# Parse the raw quality sources for the pick value curve (Pete, 2026-11-26:
# Brownlow votes plus an All-Australian bonus for 1991-2015 draftees).
#
# Inputs (saved raw, exactly as fetched, by the 26 Nov scrape):
#   torpdata/data/raw/trade_value/afltables_raw/stats_<YYYY>.html   1991-2026
#   torpdata/data/raw/trade_value/afltables_raw/wiki_all_australian_team.json
# Outputs (every column the source has; nothing judged here):
#   torpdata/data/raw/trade_value/afltables_player_seasons.csv
#   torpdata/data/raw/trade_value/all_australian.csv
#
# Run: python -I data-raw/08-trade-value/04c_parse_quality_sources.py

import csv, glob, html, json, os, re, sys
from html.parser import HTMLParser

RAW = "C:/dev/torpverse/torpdata/data/raw/trade_value"
SRC = os.path.join(RAW, "afltables_raw")


class Tables(HTMLParser):
    """Collect every table as rows of (text, first href) cells, plus the
    heading text seen most recently before each table."""
    def __init__(self):
        super().__init__(convert_charrefs=True)
        self.tables, self.heading, self._hbuf, self._inh = [], None, [], False
        self._t = self._r = self._c = None

    def handle_starttag(self, tag, attrs):
        a = dict(attrs)
        if tag in ("h2", "h3", "h4"):
            self._inh, self._hbuf = True, []
        elif tag == "table":
            self._t = {"heading": self.heading, "rows": []}
        elif tag == "tr" and self._t is not None:
            self._r = []
        elif tag in ("td", "th") and self._r is not None:
            self._c = {"text": [], "href": None, "th": tag == "th"}
        elif tag == "a" and self._c is not None and self._c["href"] is None:
            self._c["href"] = a.get("href")

    def handle_endtag(self, tag):
        if tag in ("h2", "h3", "h4") and self._inh:
            self._inh = False
            self.heading = "".join(self._hbuf).strip()
        elif tag in ("td", "th") and self._c is not None and self._r is not None:
            self._c["text"] = " ".join("".join(self._c["text"]).split())
            self._r.append(self._c); self._c = None
        elif tag == "tr" and self._r is not None and self._t is not None:
            if self._r: self._t["rows"].append(self._r)
            self._r = None
        elif tag == "table" and self._t is not None:
            self.tables.append(self._t); self._t = None

    def handle_data(self, d):
        if self._inh: self._hbuf.append(d)
        if self._c is not None: self._c["text"].append(d)


# ---- afltables season pages ----
out_rows, header_ref = [], None
files = sorted(glob.glob(os.path.join(SRC, "stats_*.html")))
assert len(files) == 36, len(files)
for f in files:
    season = int(re.search(r"stats_(\d{4})", f).group(1))
    p = Tables(); p.feed(open(f, encoding="latin-1").read())
    n_season = 0
    for t in p.tables:
        rows = t["rows"]
        if len(rows) < 3 or not rows[0] or not rows[0][0]["th"]:
            continue
        team = rows[0][0]["text"].split("[")[0].strip()
        cols = [c["text"] for c in rows[1]]
        if "Player" not in cols or "BR" not in cols:
            continue
        if header_ref is None: header_ref = cols
        for r in rows[2:]:
            if r[0]["th"] or len(r) != len(cols):
                continue  # totals / opponent rows
            rec = {"season": season, "team": team,
                   "player_href": r[1]["href"]}
            for k, c in zip(cols, r):
                rec[k] = c["text"]
            if not rec["player_href"] or "players/" not in rec["player_href"]:
                continue
            out_rows.append(rec); n_season += 1
    print(f"{season}: {n_season} player-team rows")
    assert n_season > 300, (season, n_season)

allcols = ["season", "team", "player_href"] + [c for c in header_ref]
extra = sorted({k for r in out_rows for k in r} - set(allcols))
allcols += extra
with open(os.path.join(RAW, "afltables_player_seasons.csv"), "w", newline="", encoding="utf-8") as fh:
    w = csv.DictWriter(fh, fieldnames=allcols); w.writeheader(); w.writerows(out_rows)
print("afltables:", len(out_rows), "rows,", len(allcols), "columns; extra columns:", extra)

# ---- All-Australian (Wikipedia) ----
j = json.load(open(os.path.join(SRC, "wiki_all_australian_team.json"), encoding="utf-8"))
p = Tables(); p.feed(j["parse"]["text"])
aa = []
for t in p.tables:
    h = t["heading"] or ""
    if not re.fullmatch(r"(199[1-9]|20[0-2]\d)", h):
        continue
    year = int(h)
    for r in t["rows"]:
        for c in r:
            href = c["href"] or ""
            if c["th"] or not href.startswith("/wiki/") or not c["text"]:
                continue
            aa.append({"year": year, "player_text": c["text"], "wiki_href": href,
                       "row_label": r[0]["text"]})
with open(os.path.join(RAW, "all_australian.csv"), "w", newline="", encoding="utf-8") as fh:
    w = csv.DictWriter(fh, fieldnames=["year", "player_text", "wiki_href", "row_label"])
    w.writeheader(); w.writerows(aa)
by_year = {}
for r in aa: by_year[r["year"]] = by_year.get(r["year"], 0) + 1
print("all-australian cells per year:", dict(sorted(by_year.items())))
