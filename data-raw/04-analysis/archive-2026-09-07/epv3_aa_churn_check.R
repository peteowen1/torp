# Would flipping to v3 change who makes the All-Australian squad?
#
# THE GATE BEFORE ANY REBUILD. TOPV = 0.5*EPV + 0.5*PSV, so changing the EPV
# engine can change the 46 names -- and four posts naming them are already merged
# to main, held only by draft: true. Republishing before knowing this is how a
# published article and the board behind it end up disagreeing.
#
# WHAT THIS IS NOT: the published squad is built from CUMULATIVE season TOPV in
# the site-consumed game-logs, with 14/14/4/14 position quotas and a >=12-game
# floor (inthegame-blog#533/#537). This compares the per-player TORP RATING from
# the gate's own v2 and v3 builds, top-46 by rating, no quotas. So it measures
# CHURN, not the squad. A near-zero churn here is strong evidence the squad holds;
# a large churn is a reason to rebake properly before publishing anything.
suppressPackageStartupMessages({ library(data.table); library(arrow) })

OUT <- "C:/dev/torpverse/torp/data-raw/outputs"
rd <- function(f) as.data.table(read_parquet(file.path(OUT, f)))
v2 <- rd("epv3_4ch_rt_v2prod.parquet")
v3 <- rd("epv3_4ch_rt_v3_stalepg.parquet")

# latest rated round of 2026 per player = the board as it stands
latest <- function(d) {
  x <- d[season == 2026 & is.finite(torp)]
  x[x[, .I[which.max(round)], by = player_id]$V1]
}
b2 <- latest(v2)[, .(player_id, player_name, position_group, gms, torp_v2 = torp)]
b3 <- latest(v3)[, .(player_id, player_name, position_group, gms, torp_v3 = torp)]
b <- merge(b2, b3, by = c("player_id", "player_name", "position_group", "gms"))
cat("players on both boards:", nrow(b), "\n")

# the squad floor the article used
b <- b[gms >= 12]
cat("after the >=12 game floor:", nrow(b), "\n\n")

cat("rating correlation v2 vs v3:", round(cor(b$torp_v2, b$torp_v3), 4), "\n")
cat("Spearman (rank) correlation:", round(cor(b$torp_v2, b$torp_v3, method = "spearman"), 4), "\n\n")

setorder(b, -torp_v2); top2 <- head(b$player_id, 46)
setorder(b, -torp_v3); top3 <- head(b$player_id, 46)
inb <- intersect(top2, top3)
cat("=== TOP 46 CHURN ===\n")
cat("in both:", length(inb), "of 46 | changes:", 46 - length(inb), "\n\n")

if (length(setdiff(top2, top3))) {
  cat("DROPS OUT under v3 (was in the v2 top 46):\n")
  print(b[player_id %chin% setdiff(top2, top3),
          .(player_name, position_group, gms,
            v2 = round(torp_v2, 1), v3 = round(torp_v3, 1))][order(-v2)])
}
if (length(setdiff(top3, top2))) {
  cat("\nCOMES IN under v3:\n")
  print(b[player_id %chin% setdiff(top3, top2),
          .(player_name, position_group, gms,
            v2 = round(torp_v2, 1), v3 = round(torp_v3, 1))][order(-v3)])
}

cat("\n=== biggest rating movers (any rank) ===\n")
b[, delta := torp_v3 - torp_v2]
print(head(b[order(-abs(delta)), .(player_name, position_group, gms,
                                   v2 = round(torp_v2, 1), v3 = round(torp_v3, 1),
                                   delta = round(delta, 1))], 12))

cat("\n=== movement by position (does it hit one group?) ===\n")
print(b[, .(n = .N, mean_delta = round(mean(delta), 2),
            sd_delta = round(sd(delta), 2)), by = position_group][order(-abs(mean_delta))])
