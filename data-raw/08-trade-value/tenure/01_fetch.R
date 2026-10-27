library(fitzRoy)
out <- "C:/dev/torpverse/torpdata/data/raw/tenure"
for (s in 1991:2026) {
  f <- file.path(out, sprintf("afltables_player_stats_%d.csv.gz", s))
  if (file.exists(f)) next
  d <- tryCatch(fitzRoy::fetch_player_stats_afltables(season = s), error=function(e){cat("FAIL", s, conditionMessage(e), "\n"); NULL})
  if (!is.null(d)) { data.table::fwrite(d, f); cat(s, nrow(d), ncol(d), "\n") }
}
