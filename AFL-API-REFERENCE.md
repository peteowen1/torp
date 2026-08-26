# AFL API Reference (torp in-house client)

Complete data dictionary for the in-house AFL API client that replaced
`fitzRoy` in the `torp` package. Covers every endpoint the package fetches,
its response structure, every field, and which torp table / release / loader
each endpoint feeds.

The fetch functions live in `R/afl_api.R` (structured fixture / result / lineup
/ player-stats / player-detail endpoints) and `R/scraper.R` (chain PBP plus the
internal round/player enumeration helpers). All endpoint field enumeration
below was captured **live** on 2026-06-18 against the 2026 season
(`compSeasonId = 85`, sample match `CD_M20260140603` Sydney v GWS R6).

---

## Base URLs and auth model

Three base URLs, defined in `R/constants_afl.R`:

| Constant | Value | Auth |
|----------|-------|------|
| `AFL_API_BASE_URL` | `https://aflapi.afl.com.au/afl/v2/` | **Public** — no token. Plain `httr::GET()`. |
| `AFL_CFS_API_BASE_URL` | `https://api.afl.com.au/cfs/afl/` | **Token** — `x-media-mis-token` header. |
| `AFL_SAPI_BASE_URL` | `https://sapi.afl.com.au/afl/` | **Token** — `x-media-mis-token` header. |
| *(new, not yet a torp constant)* `https://api.afl.com.au/statspro/` | **Token** — same `x-media-mis-token` flow as CFS, same host (`api.afl.com.au`), sibling path to `/cfs/afl/`. Confirmed live 2026-08-25. See "Endpoint family: `statspro`" below. |

**Live-request gotcha found confirming the `statspro` family, worth recording:** `POST .../WMCTok` with **no request body** gets Akamai's edge WAF to reject it with `411 Length Required` before it ever reaches the AFL backend — `httr::POST()` (what `get_token()` uses) always sends a `Content-Length` header even for an empty body, so torp's own R code is unaffected, but a bare `curl -X POST` (or any client that omits `Content-Length` on a bodyless POST) gets a **content-free 411**, not a 401, which reads as "wrong endpoint" rather than "malformed request." Fix for anyone reproducing this outside R: `curl -X POST ... -d ""`.

**Auth flow** (`R/scraper.R`):
- `get_token()` — POSTs `AFL_CFS_API_BASE_URL + "WMCTok"`, returns a bearer
  token, cached 5 min in `.torp_token_cache`.
- `access_api(url)` — GETs `url` with the `x-media-mis-token` header, parses
  the JSON body with `jsonlite::fromJSON(flatten = TRUE)`. Used for CFS + SAPI
  endpoints.
- The public `aflapi.afl.com.au/afl/v2` endpoints need **no** auth — they use
  bare `httr::GET()` or an unauthenticated `curl` pool.

This in-house client replaced the external `fitzRoy` package: torp now scrapes
the AFL's own apps' backend APIs directly.

All parsers run `jsonlite::fromJSON(flatten = TRUE)`, so nested JSON objects
become dot-notation columns (`home.team.name`). `.drop_list_cols()` then drops
any remaining **list-columns** (nested arrays/data.frames) because Arrow/parquet
cannot serialize them. Downstream, the `*_COL_MAP` rename maps + `.bulk_snake_case()`
(in `R/column_schema.R`) convert these to canonical snake_case.

---

## Dropped fields brought through in this pass (changelog)

Additive, low-risk captures added so the raw scraped tables faithfully archive
the API. **Re-scrape implication:** each adds a new column to its release; no
existing column changes. Verified live unless noted.

| Endpoint | Fetch fn | New column(s) | Source field | Notes |
|----------|----------|---------------|--------------|-------|
| `matches` (fixtures) | `.fetch_fixtures_for_season_id` (`R/afl_api.R`) | `round_byes` | `matches[].round.byes` (list-col of bye-team data.frames) | Flattened to comma-separated team abbreviations. 63/207 fixtures populated (one per non-bye round). Verified. |
| `squads` (player details) | `.parse_squad_json` (`R/afl_api.R`) | `team_id`, `team_abbreviation`, `team_nickname`, `team_team_type` | `squad.team.{id,abbreviation,nickname,teamType}` | Numeric team id + identity. `squad.team.club.*`, `squad.team.metadata.*` (social URLs), and `squad.compSeason.*` intentionally skipped (redundant/noise). Verified. |
| `matchRoster/full` (lineups) | `.parse_match_roster` (`R/afl_api.R`) | `team_status` | `matchRoster.{home,away}Team.teamStatus` | Per-team lineup status (e.g. `FINAL_TEAM`). Verified through `get_afl_lineups()`. |
| `matchPlays` (chains) | `get_game_chains` (`R/scraper.R`) | `homeTeamId`, `awayTeamId`, `chain_period_seconds` | top-level + `matchChains[].periodSeconds` | **Pre-existing** (done before this pass). `filter` (empty NULL query-echo) intentionally skipped. |

Naming note: `team_team_type` is the literal snake_case of `team.teamType`
(the `team.` prefix is preserved before snake-casing, mirroring how the existing
`team.providerId` → `team_provider_id` mapping behaves). Renamed only where a
genuine collision would otherwise occur; none did here.

---

## Endpoint: `competitions`  (public)

**URL:** `AFL_API_BASE_URL + "competitions"`
**Fetched by:** `.afl_all_comp_seasons()` (`R/afl_api.R:301`) — step 1 of resolving a season's numeric `compSeasonId`.

Returns the list of AFL competitions. torp picks the row whose `code` is one of
`AFL` / `AFLM` / `CD_AFLPrem` (the men's Toyota AFL Premiership), takes its `id`,
and uses it for the `compseasons` call.

**Nesting:** `{ meta: {code, pagination{page,numPages,pageSize,numEntries}}, competitions: data.frame }`

| Field | Type | Description | Sample | Captured? |
|-------|------|-------------|--------|-----------|
| `competitions.id` | int | Numeric competition id | `1` | Used (not persisted) |
| `competitions.providerId` | chr | Champion-Data provider id | `CD_C014` | Used (not persisted) |
| `competitions.code` | chr | Competition code (filtered on) | `AFL` | Used (not persisted) |
| `competitions.name` | chr | Competition display name | `Toyota AFL Premiership` | Used (not persisted) |
| `meta.code` | int | HTTP-style status echo | `200` | No (transport meta) |
| `meta.pagination.*` | int | Page / numPages / pageSize / numEntries | `numEntries=16` | No (transport meta) |

**Feeds →** internal only. No torp table/release; consumed transiently to look up `compSeasonId`. Cached in-session as `afl_all_comp_seasons` / `afl_comp_season_id_<year>`.

---

## Endpoint: `competitions/{id}/compseasons`  (public)

**URL:** `AFL_API_BASE_URL + "competitions/" + comp_id + "/compseasons"`
**Fetched by:** `.afl_all_comp_seasons()` (`R/afl_api.R:334`) — step 2. Maps a 4-digit year to the AFL's numeric `compSeasonId` (e.g. 2026 → 85).

**Nesting:** `{ meta{...}, compSeasons: data.frame }`

| Field | Type | Description | Sample | Captured? |
|-------|------|-------------|--------|-----------|
| `compSeasons.id` | int | Numeric comp-season id (the value torp needs) | `85` | Used (not persisted) |
| `compSeasons.providerId` | chr | Provider id encoding the year (`CD_S{year}014`) | `CD_S2026014` | Used (year parsed) |
| `compSeasons.name` | chr | Season display name | `2026 Toyota AFL Premiership` | Used (not persisted) |
| `compSeasons.shortName` | chr | Short label | `Premiership` | No |
| `compSeasons.currentRoundNumber` | int | The round the season is currently up to | `15` | No |
| `meta.*` | — | Transport metadata + pagination | `code=200` | No |

**Feeds →** internal only. `id` becomes `compSeasonId` for the `matches` and `squads` calls. The `id` is also used by `get_afl_fixtures(TRUE)` to iterate all seasons.

---

## Endpoint: `matches?compSeasonId={id}`  (public)  — FIXTURES / RESULTS

**URL:** `AFL_API_BASE_URL + "matches?compSeasonId=" + season_id + "&pageSize=1000"`
**Fetched by:** `.fetch_fixtures_for_season_id()` (`R/afl_api.R:579`), via `get_afl_fixtures()` / `get_afl_results()` / `get_afl_ladder()`.

Single call per season returns every match with **scores included** for
concluded games — so `get_afl_results()` needs zero extra calls (filters
`status == "CONCLUDED"`) and `get_afl_ladder()` computes standings from these.

**Nesting:** `{ meta{...,pagination}, matches: data.frame (57 flattened cols) }`.
Exactly one list-column, `round.byes`; all other 56 fields are scalar and are
captured. After this pass, `round.byes` is flattened to `round_byes` before being
dropped.

Column names below are the **raw** flattened API names; the canonical torp name
(after `.normalise_fixture_columns()` → `FIXTURE_COL_MAP` + snake_case) is given
in the Description where it differs.

| Field (raw) | Type | Description (→ canonical) | Sample | Captured? |
|-------------|------|----------------------------|--------|-----------|
| `id` | int | Numeric match id | `8041` | Yes (`id`) |
| `providerId` | chr | Match provider id → `match_id` | `CD_M20260140001` | Yes |
| `utcStartTime` | chr | Match start (UTC) → `utc_start_time` | `2026-03-05T08:30:00.000+0000` | Yes |
| `status` | chr | Match status (`SCHEDULED`/`CONCLUDED`...) | `CONCLUDED` | Yes |
| `compSeason.id` | int | Numeric comp-season id → `comp_season_id` | `85` | Yes |
| `compSeason.providerId` | chr | → `comp_season_provider_id`; year parsed → `season` | `CD_S2026014` | Yes |
| `compSeason.name` | chr | → `comp_season_name` | `2026 Toyota AFL Premiership` | Yes |
| `compSeason.shortName` | chr | → `comp_season_short_name` | `Premiership` | Yes |
| `compSeason.currentRoundNumber` | int | → `comp_season_current_round_number` | `15` | Yes |
| `round.id` | int | Numeric round id → `round_id` | `1343` | Yes |
| `round.providerId` | chr | → `round_provider_id` | `CD_R202601400` | Yes |
| `round.abbreviation` | chr | Round abbr (`OR` = Opening Round) → `round_abbreviation` | `OR` | Yes |
| `round.name` | chr | → `round_name` | `Opening Round` | Yes |
| `round.roundNumber` | int | → `round_number` (0 = Opening Round) | `0` | Yes |
| `round.byes` | list&lt;df&gt; | Teams on the bye this round (id, providerId, name, abbreviation, nickname, teamType, club.*) | df of 8 teams | **Now captured** → `round_byes` (comma-joined abbreviations) |
| `round.utcStartTime` | chr | Round window start → `round_utc_start_time` | `2026-03-05T08:30:00.000+0000` | Yes |
| `round.utcEndTime` | chr | Round window end → `round_utc_end_time` | `2026-03-08T08:20:00.000+0000` | Yes |
| `home.team.id` | int | Home numeric team id → `home_team_api_id` | `13` | Yes |
| `home.team.providerId` | chr | → `home_team_id` | `CD_T160` | Yes |
| `home.team.name` | chr | → `home_team_name` | `Sydney Swans` | Yes |
| `home.team.abbreviation` | chr | → `home_team_abbreviation` | `SYD` | Yes |
| `home.team.nickname` | chr | → `home_team_nickname` | `Swans` | Yes |
| `home.team.teamType` | chr | → `home_team_team_type` (`MEN`) | `MEN` | Yes |
| `home.team.club.id` | int | Club numeric id → `home_team_club_id` | `24` | Yes |
| `home.team.club.providerId` | chr | → `home_team_club_provider_id` | `CD_O28` | Yes |
| `home.team.club.name` | chr | → `home_team_club_name` | `Sydney Swans` | Yes |
| `home.team.club.abbreviation` | chr | → `home_team_club_abbreviation` | `Swans` | Yes |
| `home.team.club.nickname` | chr | → `home_team_club_nickname` | `Swans` | Yes |
| `home.score.goals` | int | → `home_goals` | `20` | Yes |
| `home.score.behinds` | int | → `home_behinds` | `12` | Yes |
| `home.score.totalScore` | int | → `home_score` | `132` | Yes |
| `home.score.superGoals` | int | Super-goals (AFLX/preseason) → `home_score_super_goals` | `0` | Yes |
| `away.team.id` | int | → `away_team_api_id` | `5` | Yes |
| `away.team.providerId` | chr | → `away_team_id` | `CD_T30` | Yes |
| `away.team.name` | chr | → `away_team_name` | `Carlton` | Yes |
| `away.team.abbreviation` | chr | → `away_team_abbreviation` | `CARL` | Yes |
| `away.team.nickname` | chr | → `away_team_nickname` | `Blues` | Yes |
| `away.team.teamType` | chr | → `away_team_team_type` | `MEN` | Yes |
| `away.team.club.*` | int/chr | Away club id/providerId/name/abbreviation/nickname | `CD_O5` | Yes (all 5) |
| `away.score.goals` | int | → `away_goals` | `10` | Yes |
| `away.score.behinds` | int | → `away_behinds` | `9` | Yes |
| `away.score.totalScore` | int | → `away_score` | `69` | Yes |
| `away.score.superGoals` | int | → `away_score_super_goals` | `0` | Yes |
| `venue.id` | int | → `venue_id` | `9` | Yes |
| `venue.providerId` | chr | → `venue_provider_id` | `CD_V60` | Yes |
| `venue.name` | chr | → `venue_name` | `SCG` | Yes |
| `venue.abbreviation` | chr | → `venue_abbreviation` | `SCG` | Yes |
| `venue.location` | chr | City → `venue_location` | `Sydney` | Yes |
| `venue.state` | chr | → `venue_state` | `NSW` | Yes |
| `venue.timezone` | chr | → `venue_timezone` | `Australia/Sydney` | Yes |
| `venue.landOwner` | chr | Traditional land owner → `venue_land_owner` | `Gadigal` | Yes |
| `metadata.prematch_label` | chr | → `metadata_prematch_label` | `Opening Round` | Yes |
| `metadata.ticket_link` | chr | → `metadata_ticket_link` | `https://...` | Yes |
| `metadata.sold_out` | (varies) | → `metadata_sold_out` (present some calls) | — | Yes (when present) |

Derived columns added downstream: `season` (from `compSeason.providerId`).

**Feeds →**
- `get_afl_fixtures()` → daily pipeline → torpdata **`fixtures-data`** release → `load_fixtures()`.
- `get_afl_results()` (filter `CONCLUDED`) → **`results-data`** release → `load_results()`.
- `get_afl_ladder()` → `calculate_ladder()` (in-memory; standings).
- Field normalisation map: `FIXTURE_COL_MAP` (`R/column_schema.R:221`).

---

## Endpoint: `squads?teamId={tid}&compSeasonId={id}`  (public)  — PLAYER DETAILS

**URL:** `AFL_API_BASE_URL + "squads?teamId=" + team_id + "&compSeasonId=" + season_id`
**Fetched by:** `.parse_squad_json()` via `get_afl_player_details()` (`R/afl_api.R:845`) and `.fetch_all_player_details()`. One call per team (18) per season, fired in a parallel `curl` pool.

`teamId` here is the **numeric** team id (e.g. `13`), not the `CD_T160`
providerId (passing the providerId returns HTTP 400).

**Nesting:** `{ meta{code}, squad: { compSeason{...}, team{...}, players: data.frame (14 cols) } }`

### `squad.players` (player-level, the row grain)

| Field (raw) | Type | Description (→ canonical) | Sample | Captured? |
|-------------|------|----------------------------|--------|-----------|
| `jumperNumber` | int | Squad jumper number → `jumper_number` | `2` | Yes |
| `position` | chr | Listed position class | `KEY_FORWARD` | Yes |
| `player.id` | int | Numeric player id → `id` | `1956` | Yes |
| `player.providerId` | chr | Player provider id → `player_id` (+ `row_id`) | `CD_I1003203` | Yes |
| `player.firstName` | chr | → `first_name` (+ `player_name`) | `Hayden` | Yes |
| `player.surname` | chr | → `surname` | `McLean` | Yes |
| `player.dateOfBirth` | chr | DOB → `date_of_birth` (+ `age` computed) | `1999-01-20` | Yes |
| `player.draftYear` | chr | → `draft_year` | `2018` | Yes |
| `player.heightInCm` | int | → `height_cm` | `197` | Yes |
| `player.weightInKg` | int | → `weight_kg` (often 0 = unrecorded) | `0` | Yes |
| `player.recruitedFrom` | chr | Recruiting club/pathway → `recruited_from` | `Beaumaris (Vic)/...` | Yes |
| `player.debutYear` | chr | → `debut_year` | `2019` | Yes |
| `player.draftType` | chr | → `draft_type` | `preseason` | Yes |
| `player.draftPosition` | chr | → `draft_position` | `NA` | Yes |

### `squad.team` (team identity — merged onto every player row)

| Field (raw) | Type | Description (→ canonical) | Sample | Captured? |
|-------------|------|----------------------------|--------|-----------|
| `team.name` | chr | → `team` | `Sydney Swans` | Yes |
| `team.providerId` | chr | → `team_provider_id` | `CD_T160` | Yes |
| `team.id` | int | Numeric team id → `team_id` | `13` | **Now captured** |
| `team.abbreviation` | chr | → `team_abbreviation` | `SYD` | **Now captured** |
| `team.nickname` | chr | → `team_nickname` | `Swans` | **Now captured** |
| `team.teamType` | chr | → `team_team_type` | `MEN` | **Now captured** |
| `team.club.{id,providerId,name,abbreviation,nickname}` | int/chr | Parent club identity | `CD_O28` | No — redundant with team for AFL |
| `team.metadata.{social_*Url,captainIds,clubSiteUrl,homeVenue}` | chr | Social media URLs, club captain ids, home venue | `SCG` | No — non-analytical noise |

### `squad.compSeason` (season context)

| Field | Type | Description | Sample | Captured? |
|-------|------|-------------|--------|-----------|
| `compSeason.{id,providerId,name,shortName,currentRoundNumber}` | int/chr | Season identity | `CD_S2026014` | No — redundant with the `season` column derived downstream |

**Feeds →** `get_afl_player_details()` → **`player_details-data`** release (used by `load_player_details()`); also feeds `get_players(use_api = FALSE)` in `R/scraper.R` for chain joins. Map: `PLAYER_DETAILS_COL_MAP` (`R/column_schema.R:406`).

---

## Endpoint: `matchRoster/full/{match_id}`  (CFS, token)  — LINEUPS

**URL:** `AFL_CFS_API_BASE_URL + "matchRoster/full/" + match_id`
**Fetched by:** `.parse_match_roster()` via `get_afl_lineups()` (`R/afl_api.R:724`), one call per match through `.fetch_cfs_batch()`.

The response is large and match-level; `.parse_match_roster()` reads only the
**player rosters** (`matchRoster.{home,away}Team.positions`, 26 rows each = the
named lineup) plus the team identity, producing a player-grained table. The
many match-level branches (weather, umpires, recent form) are **not**
player-grained and belong in other tables — see "intentionally left" below.

**Top-level nesting:** `{ match{...}, venue{...}, matchRoster{...}, teamPlayers{...}, recentMatchScores{...} }`

### Captured player-level fields (from `matchRoster.{home,away}Team.positions`)

| Field (raw) | Type | Description (→ canonical) | Sample | Captured? |
|-------------|------|----------------------------|--------|-----------|
| `position` | chr | Named on-field position → `lineup_position` | `BPL` | Yes |
| `player.playerId` | chr | → `player_id` (+ `row_id`) | `CD_I295222` | Yes |
| `player.captain` | logi | → `captain` | `FALSE` | Yes |
| `player.playerJumperNumber` | int | → `jumper_number` | `29` | Yes |
| `player.playerName.givenName` | chr | → `given_name` | `Joel` | Yes |
| `player.playerName.surname` | chr | → `surname` | `Hamling` | Yes |
| *(added)* `teamId` | chr | Team provider id → `team_id` | `CD_T160` | Yes |
| *(added)* `teamName` | chr | Team abbr → `team_abbr`; resolved → `team_name` | `SYD` | Yes |
| *(added)* `teamType` | chr | `home`/`away` → `team_type` | `home` | Yes |
| *(added)* `providerId` | chr | Match id → `match_id` | `CD_M20260140603` | Yes |
| *(added)* `teamStatus` | chr | Lineup status → `team_status` | `FINAL_TEAM` | **Now captured** |

Derived downstream: `season` (from `match_id`), `round_number` (from `match_id`).

### Intentionally left (match-level, not player-grained)

| Branch | Contents | Why not added here |
|--------|----------|--------------------|
| `match.*` | name, date, status, venue, home/awayTeamId, home/awayTeam{name,abbr,nickname,badgeAssetURL,flagAssetURL}, round, venueLocalStartTime, twitterHashTag | Match metadata duplicated in `fixtures-data`; asset URLs are non-analytical. |
| `venue.*` | address, name, state, timeZone, venueId, abbreviation, landOwner (+ many NULL: capacity, groundDimension, lat/long, stadium links) | Venue dims belong to fixtures/venue refs; most fields NULL. |
| `matchRoster.weather` | description, tempInCelsius, weatherType | Weather is sourced separately (Open-Meteo) into the `weather-data` release. |
| `matchRoster.umpires` | df: umpireId, umpireType, umpireNumber, umpireName.{givenName,surname} | Umpire data is match-level; not part of the player roster grain. |
| `matchRoster.{home,away}Team.{ins,outs}` | df: reason, player.* (selection ins/outs) | Late-change detail; team-level, separate concern. |
| `matchRoster.{home,away}Team.{milestones,clubDebuts,lateChanges}` | usually empty / a free-text string | Sparse, free-text. |
| `matchRoster.recentMatches` | df 5×28 — recent head-to-head form | Derived form data; reproducible from `results-data`. |
| `teamPlayers` | df mirror of positions (with `photoURL`) | Duplicate of `positions` already captured. |
| `recentMatchScores` | df 5×38 — period scores, score worm, score map, score chart | Rich match-summary data; reproducible from results / not roster-grained. |

**Feeds →** `get_afl_lineups()` → daily lineup pipeline → torpdata **`teams-data`** release → `load_teams()` (and `load_lineups`-style joins). Map: `TEAMS_COL_MAP` (`R/column_schema.R:281`).

---

## Endpoint: `playerStats/match/{match_id}`  (CFS, token)  — PLAYER STATS

**URL:** `AFL_CFS_API_BASE_URL + "playerStats/match/" + match_id`
**Fetched by:** `.parse_match_stats()` via `get_afl_player_stats()` (`R/afl_api.R:779`), one call per concluded match through `.fetch_cfs_batch()`.

**Nesting:** `{ homeTeamPlayerStats: data.frame (84 cols), awayTeamPlayerStats: data.frame (84 cols) }`.
All 84 columns are scalar (no list-columns), so the parser keeps **every**
field — only adding `teamStatus` (`home`/`away`) and `providerId`. The
`playerStats.` prefix is stripped during parse; `.normalise_player_stats_columns()`
then strips the `stats.`/`extendedStats.`/`clearances.` prefixes downstream.

Below, raw flattened name → canonical after normalisation (prefix-stripped,
snake_cased). The player-identity block appears **doubly nested**
(`player.player.player.*`) because the endpoint embeds the roster player object
inside the stats object; the parser deduplicates these.

### Player identity

| Field (raw) | Type | Canonical | Sample |
|-------------|------|-----------|--------|
| `teamId` | chr | `team_id` | `CD_T160` |
| `player.jumperNumber` | int | `jumper_number` | `36` |
| `player.photoURL` | chr | `photo_url` | `https://...` |
| `player.player.position` | chr | `position` | `FPL` |
| `player.player.player.playerId` | chr | `player_id` | `CD_I1008091` |
| `player.player.player.captain` | logi | `captain` | `FALSE` |
| `player.player.player.playerJumperNumber` | int | (dedup of jumper) | `36` |
| `player.player.player.playerName.givenName` | chr | `given_name` | `Joel` |
| `player.player.player.playerName.surname` | chr | `surname` | `Amartey` |
| `playerStats.teamId` | chr | `team_id` (dedup) | `CD_T160` |
| `playerStats.gamesPlayed` | logi | `games_played` (often NA) | `NA` |
| `playerStats.timeOnGroundPercentage` | num | `time_on_ground_percentage` | `74` |
| `playerStats.lastUpdated` | chr | `last_updated` | `2026-06-06T01:49:05.445+0000` |

### Core stats (`playerStats.stats.*` → `*`)

`goals, behinds, superGoals, kicks, handballs, disposals, marks, bounces,
tackles, contestedPossessions, uncontestedPossessions, totalPossessions,
inside50s, marksInside50, contestedMarks, hitouts, onePercenters,
disposalEfficiency, clangers, freesFor, freesAgainst, dreamTeamPoints,
rebound50s, goalAssists, goalAccuracy, ratingPoints, ranking, turnovers,
intercepts, tacklesInside50, shotsAtGoal, goalEfficiency, shotEfficiency,
interchangeCounts, scoreInvolvements, metresGained` — all `num` (some NA-only:
`superGoals, ranking, goalEfficiency, shotEfficiency, interchangeCounts`).
Sample: `goals=2, disposals=8, hitouts=1, metresGained=153`. **All captured.**

### Clearances (`playerStats.stats.clearances.*`)

`centreClearances, stoppageClearances, totalClearances` (num). All captured.

### Extended stats (`playerStats.stats.extendedStats.*`)

`effectiveKicks, kickEfficiency, kickToHandballRatio, effectiveDisposals,
marksOnLead, interceptMarks, contestedPossessionRate, hitoutsToAdvantage,
hitoutWinPercentage, hitoutToAdvantageRate, groundBallGets, f50GroundBallGets,
scoreLaunches, pressureActs, defHalfPressureActs, spoils, ruckContests,
contestDefOneOnOnes, contestDefLosses, contestDefLossPercentage,
contestOffOneOnOnes, contestOffWins, contestOffWinsPercentage,
centreBounceAttendances, kickins, kickinsPlayon` (num). **All captured.**
`.normalise_player_stats_columns()` zero-fills any of these the API omits in a
given season.

### Added by parser

| Field | Type | Description | Sample |
|-------|------|-------------|--------|
| `teamStatus` | chr | `home`/`away` indicator → `team_status` | `home` |
| `providerId` | chr | Match id → `match_id` (after join) | `CD_M20260140603` |

Match context joined in from fixtures: `venue_name, round_number,
home_team_name, away_team_name, utc_start_time`.

**Status:** This endpoint is already **fully faithful** — every API field is
captured; no fields were dropped, so no additive change was needed.

**Feeds →** `get_afl_player_stats()` → **`player_stats-data`** release → `load_player_stats()`. Map: `PLAYER_STATS_COL_MAP` + `.normalise_player_stats_columns()` (`R/column_schema.R:464`).

---

## Endpoint: `fixturesAndResults/season/CD_S{season}014/round/CD_R{season}014{round}`  (CFS, token)

**URL:** `AFL_CFS_API_BASE_URL + "fixturesAndResults/season/CD_S{season}014/round/CD_R{season}014{round}"` (round zero-padded)
**Fetched by:** `get_round_games()` (`R/scraper.R:226`) and `get_season_games()` (`R/scraper.R:264`) — the CFS-side round enumerator used to list matches before scraping chains.

**Nesting:** `{ seasonId, roundId, teamId(NULL), venueId(NULL), fixtures: data.frame (32 cols), byes: list }`

`get_round_games()` returns the whole `fixtures` data.frame untouched (only
filtering `status == "CONCLUDED"` when `concluded_only`, and adding `date` +
`season`). So **all 32 fixture fields pass through** to the chain-join pipeline.

| Field (raw) | Type | Description | Sample | Captured? |
|-------------|------|-------------|--------|-----------|
| `status` | chr | Match status | `CONCLUDED` | Yes (filter key) |
| `matchId` | chr | Match id (join key to chains) | `CD_M20260140101` | Yes |
| `utcStartTime` | chr | Start (UTC) | `2026-03-12T08:30:00.000+0000` | Yes (→ `date`) |
| `homeTeamId` / `awayTeamId` | chr | Team provider ids | `CD_T30` / `CD_T120` | Yes |
| `competitionId` | chr | Comp-season provider id | `CD_S2026014` | Yes |
| `roundNumber` | int | Round number | `1` | Yes |
| `roundId` | chr | Round provider id | `CD_R202601401` | Yes |
| `venueLocalStartTime` | logi/chr | Local start (often NA) | `NA` | Yes |
| `lastUpdated` | chr | Record update timestamp | `2026-06-17T08:21:44.015+0000` | Yes |
| `providerMatchId` | chr | Champion-Data numeric match id | `183201296` | Yes |
| `venue.name` | chr | Venue | `MCG` | Yes |
| `venue.timeZone` | chr | Venue timezone | `Australia/Melbourne` | Yes |
| `venue.venueId` | chr | Venue provider id | `CD_V40` | Yes |
| `venue.abbreviation` | chr | Venue abbr | `MCG` | Yes |
| `venue.location` | chr | City | `Melbourne` | Yes |
| `venue.state` | chr | State | `VIC` | Yes |
| `venue.landOwner` | chr | Traditional owner | `Wurundjeri` | Yes |
| `homeTeam.{teamAbbr,teamName,teamNickname}` | chr | Home team identity | `CARL` | Yes |
| `awayTeam.{teamAbbr,teamName,teamNickname}` | chr | Away team identity | `RICH` | Yes |
| `homeTeamScore.{totalScore,goals,behinds,superGoals}` | int/logi | Home score breakdown | `75/10/15/NA` | Yes |
| `awayTeamScore.{totalScore,goals,behinds,superGoals}` | int/logi | Away score breakdown | `71/9/17/NA` | Yes |
| top-level `seasonId` / `roundId` | chr | Echo of requested season/round | `CD_S2026014` | Used |
| top-level `teamId` / `venueId` | NULL | Query echo (always NULL for round query) | `NULL` | No — empty echo |
| `byes` | list() | Empty for this query shape | `list()` | No — empty |

**Feeds →** internal chain pipeline only (`get_match_chains()` joins these
fixture columns onto chains). The processed chains go to torpdata **`chains-data`**
(→ `load_chains()`) and **`pbp-data`** (→ `load_pbp()`). This endpoint is a
CFS-side mirror of the public `matches` fixtures.

---

## Endpoint: `players`  (CFS, token)

**URL:** `AFL_CFS_API_BASE_URL + "players"`
**Fetched by:** `get_players(use_api = TRUE)` (`R/scraper.R:330`). Default path is `use_api = FALSE` (reads the `player_details` release locally); the API path is a fallback that fetches the **current-season** full player list.

**Nesting:** `{ pageNumber, pageSize, totalPages, totalResults, players: data.frame (10 cols) }`

| Field (raw) | Type | Description | Sample | Captured? |
|-------------|------|-------------|--------|-----------|
| `playerId` | chr | Player provider id | `CD_I1019038` | Yes |
| `jumperNumber` | int | Jumper number | `5` | Yes |
| `playerPosition` | chr | Listed position class | `KEY_FORWARD` | Yes |
| `photoURL` | chr | Headshot URL | `https://...` | Yes |
| `playerName.givenName` | chr | First name | `Aaron` | Yes |
| `playerName.surname` | chr | Surname | `Cadman` | Yes |
| `team.teamId` | chr | Team provider id | `CD_T1010` | Yes |
| `team.teamAbbr` | chr | Team abbreviation | `GWS` | Yes |
| `team.teamName` | chr | Team name | `GWS GIANTS` | Yes |
| `team.teamNickname` | chr | Team nickname | `GIANTS` | Yes |
| `pageNumber/pageSize/totalPages/totalResults` | int | Pagination | `totalResults=812` | No — transport meta |

`get_players()` adds `season = get_afl_season()`. All player fields pass
through; only used as a chain-join lookup.

**Feeds →** internal only — `get_match_chains()` left-joins player name/position
onto chains by `(playerId, season)`. The local (default) path reads from the
`player_details-data` release instead.

---

## Endpoint: `matchPlays/{match_id}`  (SAPI, token)  — CHAINS / PBP

**URL:** `AFL_SAPI_BASE_URL + "matchPlays/" + match_id`
**Fetched by:** `get_game_chains()` (`R/scraper.R:379`), one call per match, via `get_many_game_chains()` / `get_match_chains()`.

The core play-by-play source: each match is a list of possession **chains**,
each chain a list of action **stats** (events) with on-ground `(x, y)`
coordinates. The parser explodes chains → action rows, attaching chain-level and
match-level context to every row.

**Top-level nesting:** `{ matchId, filter{...}, venueWidth, venueLength, homeTeamId, awayTeamId, homeTeamDirectionQtr1, matchChains: data.frame }`

### Match-level fields (attached to every action row)

| Field (raw) | Type | Description (→ canonical) | Sample | Captured? |
|-------------|------|----------------------------|--------|-----------|
| `matchId` | chr | Match id → `match_id` | `CD_M20260140603` | Yes |
| `venueWidth` | int | Ground width (m) → `venue_width` | `136` | Yes |
| `venueLength` | int | Ground length (m) → `venue_length` | `155` | Yes |
| `homeTeamDirectionQtr1` | chr | Home attacking direction in Q1 (`left`/`right`) → `home_team_direction_qtr1` | `right` | Yes |
| `homeTeamId` | chr | Home team id (same id-space as chain `teamId`) → captured as `mp_home_team_id` | `CD_T160` | **Captured (this audit)** |
| `awayTeamId` | chr | Away team id → captured as `mp_away_team_id` | `CD_T1010` | **Captured (this audit)** |
| `filter` | list (NULL×4) | Query echo: teamId/period/outcome/playerId (all NULL) | `NULL` | No — empty NULL query-echo, intentionally skipped |

> **Naming note:** captured under the `mp_` (matchPlays) prefix, NOT bare `homeTeamId` — `get_match_chains` joins `homeTeamId` in from the games/fixtures data (`inner_join(games, by="matchId")`), so a bare capture collides and suffixes both to `home_team_id_x`/`_y`, breaking `home_team_id` resolution. `mp_home_team_id`/`mp_away_team_id` stay distinct; `clean_pbp` prefers them for coordinate orientation (#92).
>
> **Completeness guarantee:** `get_game_chains` captures fields at all three levels *generically*, not by allow-list: action-level via `as.data.table(stats)`, and **every** other chain-level scalar field (prefixed `chain_`) and match-level scalar field (prefixed `mp_`). The known fields above keep fixed names for the pipeline; anything an older/future schema adds is archived automatically. So `chains-data` is a complete archive of every matchPlays scalar column — a missing column never forces a re-scrape; only a genuine API *schema change* does. (The exploded `matchChains`/`stats` arrays and the empty `filter` echo are the only exclusions.)

### Chain-level fields (`matchChains` rows → attached per action)

| Field (raw) | Type | Description (→ canonical) | Sample | Captured? |
|-------------|------|----------------------------|--------|-----------|
| `teamId` | chr | Chain-possessing team (defines coordinate frame) → `chain_team_id` | `CD_T160` | Yes |
| `initialState` | chr | How the chain started → `initial_state` | `centreBounce` | Yes |
| `finalState` | chr | How the chain ended → `final_state` | `turnover` | Yes |
| `period` | int | Quarter → `period` | `1` | Yes |
| `periodSeconds` | int | Chain start time in period → `chain_period_seconds` | `0` | **Captured (this audit)** |
| `stats` | list&lt;df&gt; | Action rows (exploded — see below) | df | Yes (exploded) |
| *(added)* `chain_number` | int | Sequential chain index within match | `1` | Yes |

### Action-level fields (`matchChains.stats[[i]]` → one row each)

| Field (raw) | Type | Description (→ canonical) | Sample | Captured? |
|-------------|------|----------------------------|--------|-----------|
| `displayOrder` | int | Event order within chain → `display_order` | `1` | Yes |
| `description` | chr | Event type | `Centre Bounce` | Yes |
| `periodSeconds` | int | Event time in period → `period_seconds` | `0` | Yes |
| `playerId` | chr | Acting player → `player_id` | `CD_I…` (NA for bounce) | Yes |
| `teamId` | chr | Acting team (NOT the coordinate-frame team) → `team_id` | `CD_T…` | Yes |
| `disposal` | chr | Disposal type | `NA`/`kick`/`handball` | Yes |
| `shotAtGoal` | logi | Whether the action was a shot | `NA`/`TRUE` | Yes |
| `behindInfo` | logi | Behind-type info | `NA` | Yes |
| `x` | int | On-ground x coordinate | `0` | Yes |
| `y` | int | On-ground y coordinate | `0` | Yes |

Note: the API renamed the action array `actions` → `stats` circa 2026; the
parser supports both names.

**Feeds →** `get_match_chains()` → daily pipeline → torpdata **`chains-data`**
(raw chains → `load_chains()`) and, after `clean_pbp()` / EP-WP-xG enrichment,
**`pbp-data`** (→ `load_pbp()`) and **`xg-data`** (→ `load_xg()`). Map:
`CHAINS_COL_MAP` (`R/column_schema.R:302`). The chain `teamId` (→ `chain_team_id`)
sets the attacking coordinate frame; the action `teamId` does not — see
`fix_chain_coordinates_dt()` and issue #92.

---

## Endpoint family: `statspro` and public-host additions (undocumented until 2026-08-25)

**Origin:** a GitHub commit comment from an outside contributor (`jhol3990`,
[torp commit `abe27f56`](https://github.com/peteowen1/torp/commit/abe27f56a55cd746ac11af310942e7cdd8409a60#commitcomment-197552272))
found that CFS's `playerStats/match/{matchId}` `extendedStats` field is present
in the schema but genuinely empty for every AFLW match sampled — matching
independently what the AFLW rating build hit the same night — and located a
working season-total alternative via AFL.com.au's own React bundle. That one
endpoint was then extended into a fuller reconnaissance pass, described here:
the site's production JS bundles (`https://www.afl.com.au/resources/{version}/scripts/{main,react,vendors}.bundle.js`,
version read off any live page's `<script src>` tags) were downloaded and
grepped for RTK-Query path templates
(`grep -oE '[a-zA-Z][a-zA-Z]{2,25}/\$\{[a-zA-Z0-9_.]+\}[a-zA-Z0-9/_{}\.\$-]*'`),
surfacing far more endpoint candidates than the one already found. Each was
then tested LIVE against the real AFLW 2025 season before being written up
here — **nothing below is from the bundle text alone; every "Confirmed" entry
was hit with a real request this session, and every "Found in bundle, NOT
confirmed" entry was genuinely tried and failed**, not omitted for being
inconvenient.

### Confirmed: `playersStats/seasons/{seasonProviderId}`  (statspro, token)  — AFLW EXTENDED STATS

**URL:** `https://api.afl.com.au/statspro/playersStats/seasons/{seasonProviderId}?includeBenchmarks=false`
**seasonProviderId:** the comp-season **provider id**, not the numeric id — e.g.
`CD_S2025264` (2025 AFLW). 2022 is split into two comp seasons:
`CD_S2022264` (Season 6) and `CD_S2101264` (Season 7).

Verified live 2026-08-25 against `CD_S2025264`: `totalResults: 580` (580 AFLW
players with a 2025 record), 63-field `totals`/`averages` block per player,
**genuinely populated, not zero-filled** — sampled directly (Jodie Hicks,
Richmond, `CD_I1001681`, 6 games): `spoils=5, intercepts=18, pressureActs=66,
effectiveKicks=35, metresGained=1017, scoreInvolvements=5`. This is the exact
set that reads empty from `playerStats/match/{matchId}`'s `extendedStats` for
AFLW (see that endpoint's entry above).

**Nesting:** `{ search: {...}, totalResults: int, players: [{ playerId,
playerDetails: {...}, team: {...}, gamesPlayed, totals: {63 fields}, averages:
{same 63 fields} }] }`

| Field | Type | Description | Sample |
|-------|------|-------------|--------|
| `players[].playerId` | chr | Provider id | `CD_I1001681` |
| `players[].playerDetails.{givenName,surname,age,heightCm,weightKg,jumperNumber,kickingFoot,stateOfOrigin,draftYear,debutYear,recruitedFrom}` | mixed | Player bio | `age=29, debutYear="2018"` |
| `players[].team.{teamId,teamAbbr,teamName,teamNickname}` | chr | Current team | `RICH` |
| `players[].gamesPlayed` | num | Games played this comp-season | `6` |
| `players[].totals.*` (63 fields) | num | Season totals — see full list below | `spoils=5` |
| `players[].averages.*` (same 63 fields) | num | Per-game averages of the same fields | — |

Full `totals`/`averages` field list (63): `matchesPlayed, timeOnGroundPercentage,
behinds, bounces, centreBounceAttendances, centreClearances, clangers,
contestDefLosses, contestDefLossPercentage, contestDefOneOnOnes,
contestedMarks, contestedPossessionRate, contestedPossessions,
contestOffOneOnOnes, contestOffWins, contestOffWinsPercentage,
defHalfPressureActs, disposalEfficiency, disposals, dreamTeamPoints,
effectiveDisposals, effectiveKicks, f50GroundBallGets, freesAgainst, freesFor,
goalAccuracy, goalAssists, goals, groundBallGets, handballs, hitouts,
hitoutsToAdvantage, hitoutToAdvantageRate, hitoutWinPercentage, inside50s,
interceptMarks, intercepts, kickEfficiency, kickins, kickinsPlayon, kicks,
kickToHandballRatio, marks, marksInside50, marksOnLead, metresGained,
onePercenters, pressureActs, ranking, ratingPoints, rebound50s, ruckContests,
scoreInvolvements, scoreLaunches, shotsAtGoal, spoils, stoppageClearances,
tackles, tacklesInside50, totalClearances, totalPossessions, turnovers,
uncontestedPossessions`.

**Caveat corrected 2026-08-25 (was: "no round-level source exists" — that
overstated it):** season-total/season-average granularity gives the full
63-field set. The sibling `playersStats/rounds/{roundProviderId}` endpoint,
and the per-player equivalent `playerSeasonRoundStats/{playerId}?seasonId={id}`
(see below — both verified to return the **identical** 35-field set), are
genuinely narrower than the season endpoint — `spoils`, `pressureActs`,
`effectiveKicks`, `effectiveDisposals`, `hitoutsToAdvantage`, `groundBallGets`
and `kickins` are confirmed absent, not just null, at round granularity. **But
the round-level set is not "basic"** — it includes `intercepts`,
`metresGained`, `contestedMarks`, `contestedPossessions` and
`scoreInvolvements`, five fields from the "extended" 63 that a truly basic
box-score would not have. `intercepts` specifically is what
`build_team_spm_features()`'s defense sign-constraint (`intercepts_prate`)
already expects — round-level, match-tied intercepts genuinely exist for
AFLW, verified live (Jodie Hicks R6 2025 vs St Kilda: `intercepts=3`,
`metresGained=128`, tied to `matchId=CD_M20252640608`). CFS match-level
`extendedStats` is still confirmed empty — this is a different, working path
to *some* of the same information, not a contradiction of that finding.

**Feeds →** nothing in torp yet — not wired into any loader. Directly relevant
to `build_team_spm_features()` (`R/team_spm.R`), which already references
`intercepts_prate` in its defense sign-constraints and is season-level by
design (`build_team_spm_features()` aggregates box-score per season already)
— a clean granularity match for enriching AFLW's SPM inputs at either
granularity, not yet built.

### Confirmed: `playerSeasonRoundStats/{playerId}?seasonId={seasonProviderId}`  (statspro, token)  — PER-PLAYER ROUND BREAKDOWN

**URL:** `https://api.afl.com.au/statspro/playerSeasonRoundStats/{providerPlayerId}?seasonId={seasonProviderId}`
— **`seasonId` must be the `CD_S...` provider id, not the numeric id** (numeric
form returns `400 CFSAPI001 "Invalid season id"`, confirmed). Found via the
react bundle's source map (`statsPro-api.jsx`, `getPlayerSeasonRoundStats`),
not the minified text — the source map (`react.bundle.min.js.map`, `HTTP 200`,
1728 embedded original source files) de-minifies the whole bundle; `main.js`/
`vendors.js` have no accessible source map (`403`).

Verified live against `CD_I1001681` (Jodie Hicks), `seasonId=CD_S2025264`: one
row per round played (6 rounds), each with `roundId, matchId, roundName,
roundNumber, result, opponent{...}, stats{35 fields}` — see the field-set note
above. Response is gzip-encoded; plain `curl` without `--compressed` returns
binary garbage, easy to misread as a broken endpoint.

**Feeds →** nothing in torp yet. This is the natural per-round input for any
future point-in-time/decay-weighted AFLW rating work (separate from tonight's
AFLM decay-xRAPM investigation, `feat/aflw-decay-xrapm`) — round-tied, not
just season-tied, extended stats.

### Confirmed: `playerCareerSeasonStats/{playerId}/benchmarked?competitionType=AFLW`  (statspro, token)  — CAREER + YEARLY BREAKDOWN

**URL:** `https://api.afl.com.au/statspro/playerCareerSeasonStats/{providerPlayerId}/benchmarked?competitionType=AFLW`
— the `competitionType=AFLW` **query param** (not path segment) is required;
the earlier pass tried this without it and recorded a false dead end (see the
corrected bundle-only table below). Also found via the source map — the app's
own `getPlayerCareerSeasonStats` queryFn tries `/benchmarked` first and falls
back to the non-benchmarked path on error, same param.

Verified live against `CD_I1001681`: `{ playerId, careerTotals: {62 fields,
includes spoils/pressureActs/effectiveKicks — the full extended set at career
grain}, benchmarkedCareerAverages, yearlySeasonStats: [8 entries, one per
season played — { seasonId, year, team, seasonTotals, benchmarkedSeasonAverages }] }`.
**One call returns a player's entire AFLW career, season-by-season, at the
same 63-field richness as the season endpoint** — more efficient than calling
`playersStats/seasons/{id}` once per season per player if building a
multi-season training set.

**Feeds →** nothing in torp yet. Worth comparing against `playersStats/seasons`
for agreement before treating either as authoritative — not done this pass.

### Confirmed: `playerProfile/{playerId}?seasonId={seasonProviderId}`  (statspro, token)  — PLAYER BIO

**URL:** `https://api.afl.com.au/statspro/playerProfile/{providerPlayerId}?seasonId={seasonProviderId}`
(alternatively `?competitionCode={code}` per the source, not tried). Also a
corrected dead end — the earlier pass didn't have the param shape.

Verified live: bio fields (`givenName, surname, age, heightCm, weightKg,
jumperNumber, kickingFoot, stateOfOrigin, draftYear, debutYear, recruitedFrom`)
— overlaps with `playerDetails` already embedded in `playersStats/seasons`,
not obviously additive. Low priority.

### Confirmed: `teams?compSeasonId={numericId}&pageSize=100`  (public, no token)  — TEAM LIST

**URL:** `AFL_API_BASE_URL + "teams?compSeasonId=" + numeric_id + "&pageSize=100"`
Found via live interactive browser reconnaissance (network capture while
switching the season filter on `afl.com.au/aflw/stats/leaders`), not bundle
mining. Verified live against AFLW 2025 (`compSeasonId=84`): `HTTP 200`, 18
teams, `{ id, providerId, name, abbreviation, nickname, club{id,providerId,
name,abbreviation,nickname}, metadata{social links, homeVenue, captainIds,
...} }` per team. `club` vs `team` distinction may matter for AFLW where one
club can field both an AFLM and AFLW team under different `team.id`s — not
investigated further.

**Feeds →** nothing in torp yet; `AFL_TEAM_ALIASES`-style team resolution
already exists elsewhere in torp, not compared against this source.

### Confirmed: `compseasons/{numericId}/ladders`  (public, no token)  — LADDER

**URL:** `AFL_API_BASE_URL + "compseasons/" + numeric_id + "/ladders"` —
**numeric** comp-season id (e.g. `84` for AFLW 2025), NOT the `CD_S...`
provider id (provider id on this host → `HTTP 400`). Resolve via the existing
`.afl_comp_season_id()` (`R/afl_api.R`).

Not previously documented despite living on the already-public, already-used
host — genuinely missed in the original pass, found only by mining the bundle.

Verified live 2026-08-25 against AFLW 2025 (`numericId=84`): `HTTP 200`, rich,
fully-populated ladder with per-team win/loss records, home/interstate/last-5
splits and quarter-by-quarter breakdown — not a thin summary.

**Nesting:** `{ meta{code}, compSeason{id,providerId,name,shortName,currentRoundNumber}, round{...}, lastUpdated, ladders: [{ conference, entries: [{...}] }] }`

| Field (`ladders[].entries[]`) | Type | Description | Sample |
|-------|------|-------------|--------|
| `position` | int | Ladder position | `1` |
| `team.{id,providerId,name,abbreviation,nickname,teamType}` | mixed | Team identity, `teamType="WOMEN"` for AFLW | `North Melbourne` |
| `played` | int | Games played | `12` |
| `pointsFor` / `pointsAgainst` | int | Season points scored/conceded | `868` / `270` |
| `minScore` / `maxScore` | int | Season score range | `50` / `114` |
| `avgWinMargin` / `avgLossMargin` | num | Average margins | `49.8` / `0.0` |
| `playersUsed` | int | Distinct players fielded this season | `27` |
| `quartersWon.{winQ1..winQ4,total}` | int | Quarters won by number | `winQ1=10` |
| `thisSeasonRecord.{ladderPosition,aggregatePoints,percentage,winLossRecord,winRatio}` | mixed | Current standing detail | `percentage=321.5` |
| `thisRoundLastSeason.*` | mixed | Same shape, prior-season comparison at this round | — |
| `homeRecord` / `interstateRecord` / `lastFiveGamesRecord` / `localRecord` | `{wins,losses,draws,played}` | Situational win/loss splits | `homeRecord.wins=6` |

**Feeds →** nothing in torp yet. `get_afl_ladder()` currently computes
standings itself from the `matches` fixture data rather than calling this
endpoint — this is a genuine alternative source (richer situational splits:
home/interstate/last-5/percentage-vs-last-season), not yet compared for
agreement or wired in.

### Confirmed: `compseasons/{numericId}/rounds`  (public, no token)

**URL:** `AFL_API_BASE_URL + "compseasons/" + numeric_id + "/rounds"`

Verified live: `HTTP 200` against AFLW 2025, paginated round list (`numEntries=16`
for the 2025 AFLW season). Same shape as the `round.*` block already embedded
in the `matches` endpoint (`id, providerId, abbreviation, name, roundNumber,
byes, utcStartTime, utcEndTime`) — **largely redundant** with data torp
already captures via `matches`; useful only if round metadata is needed
without pulling full fixture/score data.

### Found in bundle, NOT confirmed (genuinely tried, genuinely failed)

Every one of these path templates was extracted from bundle mining and
actually tried this session — against `api.afl.com.au/statspro/`,
`api.afl.com.au/cfs/afl/`, `sapi.afl.com.au/afl/`, and `aflapi.afl.com.au/afl/v2/`,
with both the numeric and provider-id forms where applicable, with the working
anonymous token. All returned `404` (or `400`/`403` where noted) everywhere
tried — most likely either a different, unauthenticated-by-this-flow host (an
internal "coach portal" behind real login, not the anonymous WMCTok flow) or a
genuinely nonexistent path. **Two entries below the original pass recorded as
dead ends — `playerCareerSeasonStats` and `playerProfile` — turned out to be
false negatives: the source map (found 2026-08-25, see above) revealed both
need a query param the original guesses didn't include
(`competitionType=AFLW`, `seasonId=...` respectively). Both now confirmed
working, moved to their own entries above.** Left here as a reminder that a
404 without the source-derived exact param shape is not always a real dead
end — check the source map before trusting a guessed signature.

| Path template (from bundle) | Tried against | Result |
|---|---|---|
| `compseasons/{id}/award/brownlow`, `.../award/brownlow/player-search` | statspro, sapi (numeric + provider id) | 404 everywhere |
| `bfawards/season/{seasonPid}` | statspro, sapi (numeric + provider id) | 404 everywhere |
| `coach/match/{matchId}/teamStats` | statspro (numeric + provider match id) | 404 everywhere |
| `match/{matchId}/teamStats` (no `coach` prefix) | statspro | 404 |
| `matchInterchange/{matchId}` | statspro | 404 |
| `matchItem/{matchId}` | not tried — no real id format found in the bundle context for this one | — |
| `year/{year}/prospectProfile/{playerId}` | not tried — likely AFLM draft-specific, not chased given AFLW is the priority | — |
| `playersStats/matches/{matchId}` (guessed sibling of `seasons`/`rounds`) | statspro | 404 — confirms Holden's own finding that no match-level extended-stats source exists |

### Not reached this pass (honest gaps, not omissions)

- **Compare Players page** (`afl.com.au/aflw/stats/compare`) — interactive
  browser clicks on featured comparison cards didn't register during live
  reconnaissance; the underlying endpoint (likely `getPlayerHeadToHead`,
  `/playerHeadToHead/featured?seasonId={id}` — found in the source map,
  **not yet tried live**) was not confirmed.
- **`getLeadingPlayerMatchRoundTotals`/`getLeadingPlayerMatchSeasonTotals`**
  (`/leadingPlayerMatchTotals/round/{id}`, `/leadingPlayerMatchTotals/season/{id}`)
  and **`getLeadingPlayerStats`** (`/leadingPlayerStats/season/{id}?limit=5`)
  and **`getSearchResults`** (`/playersStats/seasons/{id}?playerNameLike=...`)
  — all found in the same source-mapped `statsPro-api.jsx`, none tried live
  this pass.
- Ladder page live UI check (does the real site call `compseasons/{id}/ladders`
  the way the bundle mining inferred) — not done, though the endpoint itself
  is independently confirmed working.
- Sibling paths off the confirmed public host (`compseasons/{id}/teams`,
  `.../venues`, `.../byes`) — not tried.
- `main.js`/`vendors.js` have no accessible source map (`403`) — only
  `react.bundle.min.js.map` was available; other route-specific chunk bundles
  beyond the three top-level ones were not enumerated or mined.

---

## Endpoint: `cfs/commentaryFeed/{match_id}`  (CFS-family, token)  — LIVE COMMENTARY (not chains)

**Found** via the `react.bundle.min.js.map` source map (`cfsCommentary-api.jsx` module) —
**not** from grepping minified text; the map de-minifies the whole bundle into 1728 original
source files and made this trivial to find where earlier passes were guessing. **Base URL**
found in the live page HTML (not the bundle — it's injected as a `PULSE.app.environment` runtime
value): `//api.afl.com.au/cfs/commentaryFeed`. Auth: the same anonymous `x-media-mis-token`
flow as the rest of the CFS family.

**URL:** `https://api.afl.com.au/cfs/commentaryFeed/{match_id}` (CFS `CD_M...` provider id)

**Verified live, AFLW, 2025 R1** (`CD_M20252640101`) — genuinely populated, not empty:

```json
{"matchId":"CD_M20252640101","lastUpdated":"2025-08-14T11:39:32.766+0000",
 "commentaryEvent":[{"comment":"BEHIND - Blues (Madeleine Guerin)","periodNumber":4,
   "periodSeconds":962,"playerId":"CD_I1009888","teamId":"CD_T8096","scoreEvent":true}, ...]}
```

Compared 2020 (`CD_M20202640101`, empty — `commentaryEvent: []`) against 2025 (populated) —
**this feed is not retained/generated for older AFLW matches**, only recent ones. Worth
checking the actual cutoff before relying on it for historical work.

**This is NOT chain/possession data — characterise it honestly.** 70 total `commentaryEvent`
rows for the entire 2025 R1 match (a real match has 300+ individual disposals alone). Breakdown:
21 `scoreEvent: true` rows (every goal/behind, correctly tagged with real `playerId`/`teamId`
and `periodSeconds` timestamp — genuinely useful for a scoring-progression timeline), the rest
are auto-generated narrative blurbs (leading disposal-getters, quarter-time/full-time markers,
interchange notes, "X% of play in Y's forward half" summaries) with `playerId`/`teamId` almost
always `null`. **No individual disposals, no non-scoring possessions, no coordinates, no event
sequencing beyond goals/behinds.** Does not answer Pete's "is there chains somewhere for AFLW"
question in the way that would matter for chain-derived EPV/credit work — it's a live-blog feed,
not the event-tracking data that produces `intercepts`/`contestedMarks` in `playerSeasonRoundStats`.
That underlying event-tracking almost certainly exists (Champion Data has to log something to
compute those per-round counts) but is not exposed anywhere on afl.com.au's public-facing site —
this reconnaissance found no endpoint that exposes it directly, only its aggregated output.

**AFLM comparison, same endpoint, same match round** (`CD_M20250140101`): same shape, similarly
sparse (goals/behinds tagged, narrative blurbs otherwise) — **this is not an AFLW-specific
limitation, the commentary feed is this coarse for men's matches too.**

## AFLM vs AFLW field-population check on `playersStats/seasons` — no comp-specific gap found

Direct test, same endpoint, same season (2025), sampled the first player from each response:
`CD_S2025264` (AFLW, 580 players) vs `CD_S2025014` (AFLM, 808 players). **Identical 63-field
schema on both sides** — `set(AFLW keys) == set(AFLM keys)`, zero fields present on one comp and
absent on the other.

Zero-rate comparison across the 10 fields most relevant to torp's SPM defense fit
(`zeros/non-null/total` across all players in each comp):

| Field | AFLW | AFLM |
|---|---|---|
| `intercepts` | 76/580/580 | 150/808/808 |
| `spoils` | 120/580/580 | 189/808/808 |
| `pressureActs` | 68/580/580 | 140/808/808 |
| `metresGained` | 68/580/580 | 141/808/808 |
| `scoreInvolvements` | 78/580/580 | 151/808/808 |
| `effectiveDisposals` | 68/580/580 | 143/808/808 |
| `contestedMarks` | 256/580/580 | 309/808/808 |
| `hitouts` | 468/580/580 | 676/808/808 |
| `ruckContests` | 418/580/580 | 628/808/808 |
| `kickins` | 452/580/580 | 610/808/808 |

Zero-rates are proportionally similar between comps for every field (roughly 12-18% for the
general-purpose stats; 70-80% for `hitouts`/`ruckContests`/`kickins`, which is expected — most
players never ruck or take a kick-in, on either comp, so a high zero-rate there is normal
selection, not a data gap). **Conclusion: no AFLW-specific field-population gap in this specific
endpoint** — earlier concern (CFS `extendedStats` being empty for AFLW) was about a *different*
endpoint family; `playersStats/seasons` and its round-level sibling `playerSeasonRoundStats`
(documented above) do not reproduce that gap.

**Not reached this pass** (time-boxed, logged rather than silently dropped): Job 3 items from
the directive — Compare Players (`playerHeadToHead/featured`), the live ladder page UI check,
sibling paths off `compseasons/{id}`, and whether a second/session-based auth tier unlocks the
10 already-404'd endpoints from the first reconnaissance pass.

---

## Endpoint → torp release → loader summary

| Endpoint | Fetch fn | torpdata release | Loader |
|----------|----------|------------------|--------|
| `competitions`, `compseasons` | `.afl_all_comp_seasons()` | — (internal lookup) | — |
| `matches` | `get_afl_fixtures()` | `fixtures-data` | `load_fixtures()` |
| `matches` (CONCLUDED) | `get_afl_results()` | `results-data` | `load_results()` |
| `squads` | `get_afl_player_details()` | `player_details-data` | `load_player_details()` |
| `matchRoster/full` | `get_afl_lineups()` | `teams-data` | `load_teams()` |
| `playerStats/match` | `get_afl_player_stats()` | `player_stats-data` | `load_player_stats()` |
| `fixturesAndResults/...round...` | `get_round_games()` / `get_season_games()` | (chain enumerator) | — |
| `players` | `get_players(use_api=TRUE)` | (chain join, fallback) | — |
| `matchPlays` | `get_match_chains()` | `chains-data`, `pbp-data`, `xg-data` | `load_chains()`, `load_pbp()`, `load_xg()` |
| `statspro/playersStats/seasons` | none yet | none yet | none yet — AFLW extended-stats source, not wired in |
| `compseasons/{id}/ladders` | none yet | none yet | none yet — alternative to torp's own `get_afl_ladder()` computation |
| `compseasons/{id}/rounds` | none yet | none yet | none yet — largely redundant with `matches`' embedded `round.*` |

*Live-verification:* every endpoint above the `statspro` family was reached
live on 2026-06-18. The `statspro`/public-host additions were reached live on
2026-08-25 (see "Endpoint family: `statspro`" above for the full verification
detail, including the endpoints found but NOT confirmed). None were
documented purely from code/cache.
