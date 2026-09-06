# Condition on role SHARE, not role VOLUME.
#
# WHERE THIS COMES FROM. Smoothing on ruck_contests removed the bucket's cliff
# and inverted the leaderboard, because conditioning on OPPORTUNITY means a
# player can no longer be rewarded for having more of it -- Grundy attends 75
# contests, produces 5.55 where the curve expects 6.15, and scores negative.
#
# Role SHARE separates the two. It asks WHAT KIND OF PLAYER he is (what fraction
# of the contests available while he was on the ground did he take) rather than
# HOW MUCH he did. A part-time and a full-time ruck with the same share are
# treated as the same kind of player; the full-timer still earns more, because
# the residual is multiplied by TOG at the end.
#
#   share = ruck_contests / (team's ruck contests that match * TOG)
#   adj   = (per80 - E[per80 | share]) * TOG
#
# PREDICTION, STATED FIRST. `per80` already divides out minutes, so two genuine
# rucks with the same share still differ on per80 -- Grundy 6.91 against Cox
# 3.88 -- and the residual should keep that. If instead everyone lands at share
# ~1 and the smooth flattens, this collapses back toward the bucket, which would
# be a fine outcome but a different one.
#
#   PASS  Grundy and Gawn above Cox and the part-timers, AND the 9-vs-10 cliff
#         stays gone.
#   FAIL  either the part-timers return, or a threshold effect reappears.

suppressMessages({
  library(data.table); library(arrow); library(mgcv)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "role_share.txt"), split = TRUE)
cat("=== Conditioning on role SHARE ===\nrun at", format(Sys.time()), "\n")

d <- as.data.table(read_parquet(file.path(OUT_DIR, "v2_schemec_pgd.parquet")))
d[, tog_safe := pmax(fcoalesce(time_on_ground_percentage / 100, 0.1), 0.1)]
d[, p80 := epv_hitout / tog_safe]
d[, rc := fcoalesce(as.numeric(ruck_contests), 0)]
S <- max(d$season, na.rm = TRUE)

cat("\n########## 1. BUILD THE SHARE ##########\n")
d[, team_rc := sum(rc, na.rm = TRUE), by = .(match_id, team)]
cat(sprintf("team ruck contests per match: median %.1f (this is the pool a\n",
            median(d[team_rc > 0]$team_rc, na.rm = TRUE)))
cat("player's share is taken against, computed rather than assumed)\n")
d[, share := fifelse(team_rc > 0, rc / (team_rc * tog_safe), 0)]
d[share > 3, share := 3]   # a handful of tiny-TOG rows blow up; cap rather than drop
cat(sprintf("share: median %.2f | p90 %.2f | max %.2f | share>0.5 in %.1f%% of player-games\n",
            median(d$share), quantile(d$share, .9), max(d$share),
            100 * mean(d$share > 0.5)))

cat("\n########## 2. THE SMOOTH ##########\n")
fit <- bam(p80 ~ s(share, k = 20), data = d, weights = d$tog_safe, discrete = TRUE)
cat(sprintf("  deviance explained %.1f%% | edf %.1f\n",
            100 * summary(fit)$dev.expl, sum(fit$edf)))
grid <- data.table(share = c(0, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5))
grid[, expected_p80 := as.numeric(predict(fit, newdata = grid))]
print(grid)
d[, exp_p80 := as.numeric(predict(fit, newdata = d))]
d[, adj_share := (p80 - exp_p80) * tog_safe]

cat("\n########## 3. THE THREE RUCKS ##########\n")
WHO <- c("Brodie Grundy", "Max Gawn", "Mason Cox")
t <- d[season == S & player_name %chin% WHO,
       .(gm = .N, tog = round(mean(tog_safe), 2), rc = round(mean(rc), 1),
         share = round(mean(share), 2), p80 = round(mean(p80), 2),
         raw = round(mean(epv_hitout, na.rm = TRUE), 3),
         bucket = round(mean(epv_hitout_adj, na.rm = TRUE), 3),
         role_share = round(mean(adj_share, na.rm = TRUE), 3)), by = player_name]
setorder(t, -raw); print(t)

cat("\n########## 4. THE LEADERBOARD ##########\n")
lb <- d[season == S, .(gm = .N, tog = round(mean(tog_safe) * 100, 0),
                       rc = round(mean(rc), 0), share = round(mean(share), 2),
                       raw = round(mean(epv_hitout, na.rm = TRUE), 2),
                       bucket = round(mean(epv_hitout_adj, na.rm = TRUE), 3),
                       shr = round(mean(adj_share, na.rm = TRUE), 3)),
        by = player_name][gm >= 6]
setorder(lb, -shr); cat("\ntop 12 by ROLE-SHARE conditioning:\n"); print(lb[1:12], nrows = 14)

cat("\n########## 5. THE TWO TESTS ##########\n")
top4 <- lb[1:4]$player_name
ok_order <- "Brodie Grundy" %in% lb[1:3]$player_name &&
  lb[player_name == "Brodie Grundy"]$shr > lb[player_name == "Mason Cox"]$shr
cat(sprintf("  Grundy in the top 3 and above Cox: %s\n", ifelse(ok_order, "PASS", "FAIL")))
near <- d[season == S & rc >= 5 & rc <= 15,
          .(n = .N, bucket = round(mean(epv_hitout_adj, na.rm = TRUE), 3),
            role_share = round(mean(adj_share, na.rm = TRUE), 3)),
          by = .(contests = round(rc))][order(contests)]
print(near)
jump_b <- abs(near[contests == 10]$bucket - near[contests == 9]$bucket)
jump_s <- abs(near[contests == 10]$role_share - near[contests == 9]$role_share)
cat(sprintf("\n  9->10 contest jump: bucket %.3f | role-share %.3f (%s)\n",
            jump_b, jump_s, ifelse(jump_s < 0.25, "PASS, no cliff", "FAIL, cliff present")))

act <- lb[raw > quantile(lb$raw, 0.75, na.rm = TRUE)]
cat(sprintf("\n  panel: bucket     cor(adj,raw) %+.3f | cor(adj,tog) %+.3f\n",
            cor(act$raw, act$bucket), cor(act$bucket, act$tog)))
cat(sprintf("         role-share cor(adj,raw) %+.3f | cor(adj,tog) %+.3f\n",
            cor(act$raw, act$shr), cor(act$shr, act$tog)))

saveRDS(list(three = t, board = lb, cliff = near), file.path(OUT_DIR, "role_share.rds"))
cat("\ndone", format(Sys.time()), "\n"); sink(); cat("\nDone\n")
