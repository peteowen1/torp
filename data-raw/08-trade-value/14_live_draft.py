"""Live draft feed for the inthegame.blog draft page (inthegame-blog#1059).

RE-USING IT FOR ANOTHER DRAFT (review note, 24 Nov 2026): the 2026 Wikipedia
page name, the draft_2026.json input, the output paths and the R2 key
(afl/draft-2026-live.json) are hard-coded below, so edit all of them first or
the run overwrites the 2026 file. Also delete live/last_upload.json: it keeps
the 2026 pick count, and the "fewer picks than the last upload" check would
then block every upload of the new draft, which shows only as a log line.

Every --interval seconds:
  1. read the "2026 national draft" table from Wikipedia's 2026 AFL draft page
     (MediaWiki API, named client, one request a cycle) and save each NEW
     revision raw before parsing it;
  2. merge any hand-entered picks from picks_override.csv (they win over
     Wikipedia, pick by pick), for when Wikipedia is slow or wrong;
  3. add `picks_made`, `updated_at` and `live_source` to the page export from
     09_export_draft_json.R (draft_2026.json; nothing else in it changes);
  4. check the result, and upload it to R2 only if the checks pass.

A tied prospect (the seven priced bids plus Hodge and McCartney) taken by a
club he is tied to counts as a matched bid; the bidding club is the holder of
the next pick on the table. Taken by any other club, that club's bid won.
Points paid is the bid's cost at the pick he went at (06b's grid; it depends
only on the pick and the club's finish). These are readings of the table, not
statements from it: `live_source.method` says so.

Checks that block an upload (the last good file stays up):
  - fewer picks made than the last upload (a vandalised or half-edited table);
  - a player twice, or pick numbers not strictly increasing;
  - a club name not in the 18-club list.

Run from anywhere:
  python data-raw/08-trade-value/14_live_draft.py --once --dry-run     # parse + write, no upload
  python data-raw/08-trade-value/14_live_draft.py --until 2026-11-19T23:30+11:00
Output: torpdata/data/raw/trades_2026/live/ (gitignored): wiki/rev_<id>.json,
draft-2026-live.json, live_draft.log.
"""
import argparse, csv, datetime as dt, json, os, re, subprocess, sys, time, unicodedata, urllib.request
from pathlib import Path

RAW = Path("C:/dev/torpverse/torpdata/data/raw")
BASE = RAW / "trade_value" / "draft_2026.json"
GRID = RAW / "trade_value" / "06_all_bids_grid.csv"
LIVE = RAW / "trades_2026" / "live"
OVERRIDE = LIVE / "picks_override.csv"
OUT = LIVE / "draft-2026-live.json"
LOG = LIVE / "live_draft.log"
STATE = LIVE / "last_upload.json"
API = ("https://en.wikipedia.org/w/api.php?action=parse&page=2026_AFL_draft"
       "&prop=wikitext|revid&formatversion=2&format=json")
UA = "torp-draft-tracker/0.1 (personal sports analytics; one request a minute)"
R2_KEY = "inthegame-data/afl/draft-2026-live.json"

CLUBS = {"Ade": "Adelaide", "BL": "Brisbane", "Car": "Carlton", "Col": "Collingwood",
         "Ess": "Essendon", "Fre": "Fremantle", "Gee": "Geelong", "GC": "Gold Coast",
         "GWS": "Greater Western Sydney", "Haw": "Hawthorn", "Mel": "Melbourne",
         "NM": "North Melbourne", "PA": "Port Adelaide", "Ric": "Richmond", "StK": "St Kilda",
         "Syd": "Sydney", "WC": "West Coast", "WB": "Western Bulldogs",
         # spellings editors also use (seen on the 2025 and 2026 draft pages)
         "Bri": "Brisbane", "STK": "St Kilda", "Stk": "St Kilda"}
CODE = {"ADEL": "Adelaide", "BL": "Brisbane", "CARL": "Carlton", "COLL": "Collingwood",
        "ESS": "Essendon", "FRE": "Fremantle", "GEEL": "Geelong", "GC": "Gold Coast",
        "GWS": "Greater Western Sydney", "HAW": "Hawthorn", "MELB": "Melbourne",
        "NMFC": "North Melbourne", "PORT": "Port Adelaide", "RICH": "Richmond", "STK": "St Kilda",
        "SYD": "Sydney", "WCE": "West Coast", "WB": "Western Bulldogs"}
DUAL_ALT = {"Cooper Hodge": "BL", "Aidan McCartney": "SYD"}  # second tied club (see #1059)


def log(msg):
    line = f"{dt.datetime.now().strftime('%Y-%m-%d %H:%M:%S')} {msg}"
    print(line, flush=True)
    with open(LOG, "a", encoding="utf-8") as f:
        f.write(line + "\n")


def norm(name):
    s = unicodedata.normalize("NFKD", name).encode("ascii", "ignore").decode().lower()
    return re.sub(r"[^a-z]", "", s)


def clean_player(cell):
    s = re.sub(r"<ref[^>]*/>|<ref[^>]*>.*?</ref>", "", cell, flags=re.S)
    s = re.sub(r"\{\{\s*sortname\s*\|([^|}]*)\|([^|}]*)[^}]*\}\}", r"\1 \2", s, flags=re.I)
    s = re.sub(r"\[\[(?:[^|\]]*\|)?([^\]]*)\]\]", r"\1", s)
    s = re.sub(r"\{\{[^}]*\}\}|'''?|<[^>]+>", "", s)
    # Wikipedia is open to edit and this text reaches a public page: drop every
    # markup character outright rather than trust the one-pass tag strip above.
    s = re.sub(r"[<>&{}\"\[\]|=\\]", "", s)
    return " ".join(s.split())


# A player name is letters (accents allowed), spaces, apostrophes, dots and
# hyphens, 2-40 characters. Anything else blocks the upload (see check()).
NAME_OK = re.compile(r"^[A-Za-zÀ-ɏ' .\-]{2,40}$")
SCOPE_ROW = re.compile(r'scope\s*=\s*"?row', re.I)
AFL_TPL = re.compile(r"\{\{\s*AFL\s*[ |]\s*(\w+)\s*\}\}")  # {{AFL GC}} and {{AFL|GC}} both occur


def fetch_wiki():
    req = urllib.request.Request(API, headers={"User-Agent": UA})
    with urllib.request.urlopen(req, timeout=30) as r:
        body = r.read()
    j = json.loads(body)["parse"]
    rev = j["revid"]
    raw = LIVE / "wiki" / f"rev_{rev}.json"
    if not raw.exists():
        raw.write_bytes(body)  # raw, exactly as received, before any parsing
        log(f"new Wikipedia revision {rev} saved")
    return rev, j["wikitext"]


def parse_table(wikitext):
    """Rows of the national draft table: pick, club (full name), player ('' if not yet), notes."""
    start = wikitext.find("== 2026 national draft ==")
    if start < 0:
        raise ValueError("no '2026 national draft' section")
    sec = wikitext[start:]
    nxt = re.search(r"\n==[^=]", sec[5:])
    sec = sec[:nxt.start() + 5] if nxt else sec
    # The section can hold more than one table: on 20 Nov editors put an empty
    # "Draft order prior to the draft" table (in {{hidden begin}}) above the
    # real selections table. Parse every table; use the one with the most
    # players named (ties: the last, which is the selections table).
    tables = [t for t in re.split(r"\n\{\|", sec)[1:]]
    best = []
    for t in tables:
        end = t.find("\n|}")
        rows = _parse_rows(t[:end if end > 0 else len(t)])
        if rows and sum(1 for r in rows if r["player"]) >= sum(1 for r in best if r["player"]):
            best = rows
    return best


def _parse_rows(body):
    rows = []
    for block in body.split("\n|-")[1:]:
        lines = [l.strip() for l in block.split("\n") if l.strip()]
        pick = next((int(m.group(1)) for l in lines if (m := re.fullmatch(r"\|\s*(\d+)\s*", l))), None)
        if pick is None:
            continue
        # Real tables write scope="row" (quoted); 2025's draft-night revisions do.
        player_line = next((l for l in lines if l.startswith("!") and SCOPE_ROW.search(l)), "")
        player = clean_player(player_line.split("|", 1)[1]) if "|" in player_line else ""
        club_line = next((l for l in lines if l.startswith("|") and "align" not in l
                          and AFL_TPL.search(l)), "")
        m = AFL_TPL.search(club_line)
        club = CLUBS.get(m.group(1), f"?{m.group(1)}") if m else ""
        notes_line = next((l for l in lines if "align" in l), "")
        notes_raw = notes_line.split("|", 1)[1] if "|" in notes_line else ""
        b = re.search(r"bid\s+by\s+" + AFL_TPL.pattern, notes_raw, re.I)
        rows.append({"pick": pick, "club": club, "player": player,
                     "notes": clean_player(notes_raw),
                     "bid_by": CLUBS.get(b.group(1), f"?{b.group(1)}") if b else None})
    return rows


def load_override():
    if not OVERRIDE.exists():
        return {}
    with open(OVERRIDE, encoding="utf-8") as f:
        return {int(r["pick"]): r for r in csv.DictReader(f) if r.get("pick", "").strip()}


def build_picks_made(rows, base):
    tied = {}  # norm(name) -> (name, {tied club full names}, home code)
    for b in base["bids"]:
        tied[norm(b["player"])] = (b["player"], {CODE[b["club"]]}, b["club"])
    for name, alt in DUAL_ALT.items():
        home = next(r["club"] for r in base["bid_grid"] if r["player"] == name)
        tied[norm(name)] = (name, {CODE[home], CODE[alt]}, home)
    cost = {}
    with open(GRID, encoding="utf-8") as f:
        for r in csv.DictReader(f):
            cost[(r["player"], CODE[r["club"]], int(r["bid"]))] = int(float(r["cost_points"]))
    for r in base["bid_grid"]:
        cost[(r["player"], CODE[r["club"]], int(r["bid"]))] = int(r["cost_points"])
    made = []
    for i, r in enumerate(rows):
        if not r["player"]:
            continue
        t = tied.get(norm(r["player"]))
        rec = {"pick": r["pick"], "club": r["club"], "player": r["player"],
               "bid": False, "matched": None, "bidding_club": None, "points_paid": None}
        if t:
            name, clubs, _ = t
            rec["player"] = name
            if "no bid" in r["notes"].lower():
                pass
            elif r["club"] in clubs:
                # The notes usually name the bidder ("matched bid by {{AFL WC}}");
                # otherwise read it as the next pick's holder.
                nxt = r.get("bid_by") or (rows[i + 1]["club"] if i + 1 < len(rows) else None)
                rec.update(bid=True, matched=True, bidding_club=nxt,
                           points_paid=cost.get((name, r["club"], r["pick"])))
            else:
                rec.update(bid=True, matched=False, bidding_club=r["club"])
        made.append(rec)
    return made


def apply_override(made, over):
    by_pick = {m["pick"]: m for m in made}
    for p, o in over.items():
        def opt(k):
            v = (o.get(k) or "").strip()
            return None if v == "" else v
        rec = by_pick.get(p, {"pick": p})
        rec.update(club=opt("club") or rec.get("club"), player=opt("player") or rec.get("player"))
        for k in ("bid", "matched"):
            v = opt(k)
            if v is not None:
                rec[k] = v.lower() == "true"
            rec.setdefault(k, False if k == "bid" else None)
        rec["bidding_club"] = opt("bidding_club") if opt("bidding_club") else rec.get("bidding_club")
        pp = opt("points_paid")
        rec["points_paid"] = int(pp) if pp else rec.get("points_paid")
        by_pick[p] = rec
    return [by_pick[k] for k in sorted(by_pick)]


def check(made, last_n):
    problems = []
    picks = [m["pick"] for m in made]
    if any(b <= a for a, b in zip(picks, picks[1:])):
        problems.append("pick numbers not strictly increasing")
    names = [norm(m["player"]) for m in made]
    if len(set(names)) != len(names):
        problems.append("a player appears twice")
    bad = [m["club"] for m in made if m["club"] not in CLUBS.values()]
    if bad:
        problems.append(f"unknown club {bad[:3]}")
    odd = [m["player"] for m in made if not NAME_OK.match(m["player"] or "")]
    if odd:
        problems.append(f"implausible player name {odd[:3]!r}")
    for m in made:  # override rows can carry free text too
        if m.get("bidding_club") not in (None, *CLUBS.values()):
            problems.append(f"unknown bidding club {m['bidding_club']!r}")
            break
    if len(made) < last_n:
        problems.append(f"picks made fell from {last_n} to {len(made)}")
    return problems


def upload(path):
    cmd = ["npx.cmd" if os.name == "nt" else "npx", "wrangler@4", "r2", "object", "put", R2_KEY,
           "--file", str(path), "--content-type", "application/json",
           "--cache-control", "max-age=30", "--remote"]
    r = subprocess.run(cmd, capture_output=True, text=True, encoding="utf-8", errors="replace", timeout=120)
    if r.returncode != 0:
        raise RuntimeError(f"wrangler exit {r.returncode}: {(r.stderr or r.stdout)[-400:]}")


def cycle(args):
    base = json.loads(BASE.read_text(encoding="utf-8"))
    last_n = json.loads(STATE.read_text())["n"] if STATE.exists() else 0
    try:
        rev, wt = fetch_wiki()
        rows = parse_table(wt)
        src = {"source": "Wikipedia, 2026 AFL draft", "revid": rev, "rows_parsed": len(rows)}
    except Exception as e:  # keep serving the override and the last good file
        log(f"Wikipedia read failed: {e!r}")
        rows, src = [], {"source": "Wikipedia unavailable this cycle", "revid": None, "rows_parsed": 0}
    made = apply_override(build_picks_made(rows, base), load_override())
    problems = check(made, last_n)
    out = dict(base)
    out["picks_made"] = made
    # Who's on the clock, from Wikipedia's rows with a club but no player yet
    # (editors fill the live order in ahead of the picks). Live numbering, like
    # picks_made; empty when the table has no unfilled rows.
    last = made[-1]["pick"] if made else 0
    upcoming = [{"pick": r["pick"], "club": r["club"]} for r in rows
                if not r["player"] and r["pick"] > last and r["club"] in CLUBS.values()]
    out["on_clock"] = upcoming[0] if upcoming else None
    out["to_come"] = upcoming[:10]
    out["updated_at"] = dt.datetime.now(dt.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    out["live_source"] = dict(src, override_rows=len(load_override()), method=(
        "Picks read from Wikipedia's draft table each minute; hand-entered picks override it. "
        "A tied prospect taken by his own club is read as a matched bid, the bidding club as the "
        "next pick's holder, and points paid as the bid's cost at that pick."))
    tmp = OUT.with_suffix(".tmp")
    tmp.write_text(json.dumps(out, ensure_ascii=False, indent=1), encoding="utf-8")
    if problems:
        log(f"NOT uploaded ({len(made)} picks): " + "; ".join(problems))
        return
    tmp.replace(OUT)
    latest = made[-1] if made else None
    if args.dry_run:
        log(f"dry run: {len(made)} picks made, rows parsed {src['rows_parsed']}, latest {latest}")
        return
    upload(OUT)
    STATE.write_text(json.dumps({"n": len(made), "at": out["updated_at"]}))
    log(f"uploaded: {len(made)} picks made, latest {latest}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--once", action="store_true")
    ap.add_argument("--dry-run", action="store_true")
    ap.add_argument("--interval", type=int, default=60)
    ap.add_argument("--start", default=None, help="ISO time with offset to begin polling, e.g. 2026-11-19T18:30+11:00")
    ap.add_argument("--until", default=None, help="ISO time with offset, e.g. 2026-11-19T23:30+11:00")
    args = ap.parse_args()
    (LIVE / "wiki").mkdir(parents=True, exist_ok=True)
    if os.name == "nt":  # stop Windows sleeping or hibernating while this runs (AC hibernate is 3 h idle)
        import ctypes
        ok = ctypes.windll.kernel32.SetThreadExecutionState(0x80000000 | 0x00000001)  # CONTINUOUS | SYSTEM_REQUIRED
        log(f"keep-awake {'on' if ok else 'FAILED'}")
    until =dt.datetime.fromisoformat(args.until) if args.until else None
    log(f"start: once={args.once} dry_run={args.dry_run} interval={args.interval}s from={args.start} until={args.until}")
    if args.start:
        wait = (dt.datetime.fromisoformat(args.start) - dt.datetime.now(dt.timezone.utc)).total_seconds()
        if wait > 0:
            log(f"waiting {wait / 60:.0f} min for {args.start}")
            # Re-check the wall clock every minute: one long time.sleep() does not
            # count time the PC spends asleep, so night one's run (19 Nov) woke a
            # day late and missed the whole night.
            start_at = dt.datetime.fromisoformat(args.start)
            while dt.datetime.now(dt.timezone.utc) < start_at:
                time.sleep(min(60, max(1, (start_at - dt.datetime.now(dt.timezone.utc)).total_seconds())))
    while True:
        t0 = time.time()
        try:
            cycle(args)
        except Exception as e:
            log(f"cycle failed: {e!r}")
        if args.once or (until and dt.datetime.now(dt.timezone.utc) >= until):
            break
        time.sleep(max(5, args.interval - (time.time() - t0)))
    log("stop")


if __name__ == "__main__":
    sys.stdout.reconfigure(encoding="utf-8")
    main()
