# Normalise results to a common schema

Handles both the old CFS schema (from historical torpdata releases with
`match.matchId`, `homeTeamScore.matchScore.*`) and the new fixture
schema (from
[`get_afl_results()`](https://peteowen1.github.io/torp/reference/get_afl_results.md)
with `providerId`, `home.score.*`). Returns a tibble with the legacy
column names used downstream.

## Usage

``` r
.normalise_results_schema(results)
```

## Arguments

- results:

  Raw results data (may be mixed schema across seasons)

## Value

Tibble with columns: providerId, homeTeamScore.matchScore.totalScore,
homeTeamScore.matchScore.goals, homeTeamScore.matchScore.behinds,
awayTeamScore.matchScore.totalScore, awayTeamScore.matchScore.goals,
awayTeamScore.matchScore.behinds, match.utcStartTime
