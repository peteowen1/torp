# GitHub Actions Workflows

## Active Workflows

### 1. Daily Ratings & Predictions (`daily-ratings-predictions.yml`)

**Trigger:** `repository_dispatch` (type: `ratings-trigger`) or manual
`workflow_dispatch`

Two-job pipeline that computes TORP ratings and then builds match
predictions.

**What it does:**

1.  **compute-ratings** – Runs
    `data-raw/03-ratings/run_ratings_pipeline.R` to rebuild player game
    data and TORP ratings, uploads to torpdata
2.  **build-predictions** – Runs
    `data-raw/02-models/build_match_predictions.R` to generate weekly
    match predictions

**Manual trigger:** Actions tab \> “Daily Ratings & Predictions” \> “Run
workflow”

- `force` – Force run, skip checks
- `week_override` – Override prediction week (leave empty for
  auto-detect)

**On failure:** Automatically creates/updates a GitHub issue with
`bug` + `automation` labels.

### 2. R Package Tests (`test-package.yml`)

**Trigger:** Push to main/develop, Pull Requests

Runs R CMD check across multiple platforms:

- Ubuntu (R release, R devel)
- Windows (R release)
- macOS (R release)
- Test coverage reporting via codecov

### 3. pkgdown Site (`pkgdown.yml`)

**Trigger:** Push to main, manual

Builds and deploys the pkgdown documentation site to GitHub Pages at
<https://peteowen1.github.io/torp/>.

## Configuration

### Required Secrets

- `GITHUB_TOKEN` – Automatically provided by GitHub Actions
- `WORKFLOW_PAT` – Personal access token with write permissions to
  torpdata releases (used by ratings/predictions workflow)

### Key Environment Variables

- `R_KEEP_PKG_SOURCE=yes` – Maintains package source for debugging
- `GITHUB_PAT` – Uses GitHub token for piggyback releases

## Timezone Notes

All cron schedules are in UTC. AFL operates in AEST/AEDT.

| UTC     | AEST             | AEDT             |
|---------|------------------|------------------|
| 4:00 PM | 2:00 AM (+1 day) | 3:00 AM (+1 day) |

## Troubleshooting

- **R dependency failures** – Check DESCRIPTION matches workflow
  dependencies
- **Authentication errors** – Verify GITHUB_TOKEN permissions
- **Data access issues** – Check fitzRoy API status and rate limits
- **Storage limits** – Monitor piggyback release sizes
