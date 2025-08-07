# GitHub Actions Workflows

This repository uses GitHub Actions for automated data collection, testing, and releases.

## 🤖 Active Workflows

### 1. Weekly Data Release (`weekly-data-release.yml`)
**Schedule:** Every Wednesday at 8:00 PM UTC
**Purpose:** Automatically runs the `data-raw/release-data.R` script to update all AFL data

**What it does:**
- Updates chains data for current round
- Refreshes play-by-play data with latest AFL statistics
- Updates expected goals (xG) data
- Refreshes player statistics, fixtures, team lineups, results, and player details
- Commits changes and creates GitHub releases via piggyback

**Manual trigger:** Available via "Actions" tab → "Weekly Data Release" → "Run workflow"

### 2. R Package Tests (`test-package.yml`)
**Trigger:** Push to main/develop, Pull Requests
**Purpose:** Automated testing across multiple R versions and operating systems

**Testing matrix:**
- Ubuntu (R release, R devel)
- Windows (R release)  
- macOS (R release)
- Test coverage reporting via codecov

## 📅 Future Workflows (Templates)

### Pre-Game Data Update (`pre-game-data-update.yml.template`)
**Status:** Template ready for activation
**Purpose:** Update data 2 hours before each AFL game

**To activate:**
1. Rename file to remove `.template` extension
2. Uncomment the `schedule` section
3. Customize game-specific data collection logic

**Proposed schedule:**
- Thursday: 7:20 PM AEST games (09:20 UTC)
- Friday: 7:50 PM AEST games (09:50 UTC)
- Saturday: 1:30 PM AEST games (03:30 UTC)
- Sunday: 3:20 PM AEST games (05:20 UTC)

## 🔧 Configuration

### Required Secrets
- `GITHUB_TOKEN`: Automatically provided by GitHub Actions
- Additional secrets may be needed for external API access

### Environment Variables
- `R_KEEP_PKG_SOURCE=yes`: Maintains package source for debugging
- `GITHUB_PAT`: Uses GitHub token for piggyback releases

### Dependencies
All workflows automatically install required R packages:
- devtools, piggyback, fitzRoy, tictoc, mgcv
- Testing packages: testthat, covr, rcmdcheck

## 📊 Monitoring

### Success Indicators
- ✅ Green checkmarks in Actions tab
- 📈 Updated data releases in torpdata repository
- 📋 Workflow summary reports

### Failure Handling
- 🚨 Automatic issue creation on workflow failures
- 📧 Email notifications (if configured)
- 🔄 Manual re-run capability

## 🕒 Timezone Notes

**Current Configuration:** UTC times
**AFL Times:** AEST/AEDT (Australian Eastern Time)

**Time Conversion:**
- Wednesday 8:00 PM UTC = Thursday 7:00 AM AEDT (summer)
- Wednesday 8:00 PM UTC = Thursday 6:00 AM AEST (winter)

Adjust cron schedules in workflows if different timing is needed.

## 🚀 Getting Started

1. **Enable Actions:** GitHub Actions should be automatically enabled
2. **Monitor First Run:** Check the first Wednesday after setup
3. **Manual Test:** Use "Run workflow" button to test manually
4. **Configure Notifications:** Set up email alerts in repository settings

## 📞 Troubleshooting

**Common Issues:**
- **R dependency failures:** Check DESCRIPTION file matches workflow dependencies
- **Authentication errors:** Verify GITHUB_TOKEN permissions
- **Data access issues:** Check fitzRoy API status and rate limits
- **Storage limits:** Monitor repository size and piggyback releases

**Debug Steps:**
1. Check workflow logs in Actions tab
2. Verify R package installation in setup step
3. Check data-raw/release-data.R script execution
4. Monitor piggyback release creation

## 📈 Future Enhancements

**Planned Improvements:**
- [ ] ARM compatibility workflows
- [ ] Parallel data processing for faster execution
- [ ] Advanced failure recovery and retry logic
- [ ] Integration with external monitoring services
- [ ] Automated performance benchmarking