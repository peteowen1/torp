# Precompile the network-heavy "shot-accuracy-streaks" article.
#
# The article pulls ~400 MB of play-by-play from torpdata and runs a permutation
# test, so we DON'T want it evaluating on every pkgdown site build. Instead we
# knit it once here, locally, and commit the frozen .Rmd + figure/ PNGs. pkgdown
# then serves pure static content (no network, no compute, can't break on CI).
#
# Refresh workflow:
#   1. edit shot-accuracy-streaks.Rmd.orig
#   2. Rscript vignettes/articles/precompile.R   (run from the package root)
#   3. commit shot-accuracy-streaks.Rmd + figure/*.png
#
# Windows note: arrow can segfault under Git Bash R -- run via PowerShell:
#   powershell.exe -Command 'Rscript "vignettes/articles/precompile.R"'

here <- "vignettes/articles"
if (!dir.exists(here)) stop("Run from the package root (couldn't find ", here, ").")

withr::with_dir(here, {
  knitr::knit("shot-accuracy-streaks.Rmd.orig", "shot-accuracy-streaks.Rmd")
})
message("Done. Commit shot-accuracy-streaks.Rmd and figure/*.png")
