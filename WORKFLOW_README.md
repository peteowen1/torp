# GitHub Actions Workflows

## Active Workflows

### 1. Daily Data Release (`daily-data-release.yml`)

**Schedule:** Daily at 4:00 PM UTC (2:00 AM AEST)

Automatically runs the data release pipeline to update all AFL data on
[torpdata](https://github.com/peteowen1/torpdata).

**What it does:**

- Fetches latest AFL data via fitzRoy
- Processes play-by-play, chains, player stats, fixtures, results,
  teams, player details, and xG
- Uploads to torpdata GitHub releases as parquet files via piggyback

**Manual trigger:** Actions tab \> “Daily Data Release” \> “Run
workflow”

- `force_release` – re-release all data even if unchanged
- `rebuild_aggregates` – rebuild aggregate data files

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
