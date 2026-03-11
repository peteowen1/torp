# Normalise results to a common schema

Handles three result schemas:

- **Canonical** (normalised fixtures/results): `match_id`, `home_score`,
  etc.

- **CFS** (old torpdata releases): `match.matchId`,
  `homeTeamScore.matchScore.*`

- **Fixture** (raw API, pre-normalisation): `providerId`, `home.score.*`

Returns a tibble with canonical column names.

## Usage

``` r
.normalise_results_schema(results)
```

## Arguments

- results:

  Results data (may be mixed schema across seasons)

## Value

Tibble with columns: match_id, home_score, home_goals, home_behinds,
away_score, away_goals, away_behinds, utc_start_time
