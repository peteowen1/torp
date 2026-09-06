# Continuous centring: subtract a smooth function of role, not a bucket mean.
#
# THE IDEA. Today's fix cells the hitout channel on ruck_contests >= 10 -- two
# buckets. That works, and it has three flaws a bucket always has:
#
#   * the threshold is arbitrary (10 is a number I chose)
#   * there is a cliff: 9 contests and 11 contests land in different worlds
#   * a swingman is forced wholly in or wholly out, which is exactly why both
#     bucket attempts failed on Mason Cox -- he is PARTLY a ruck
#
# Continuous form:
#     adj = (per80 - E[per80 | role covariates]) * TOG
# with E[.] a GAM. For hitout the covariate is ruck_contests: a player is asked
# to beat what someone with HIS OPPORTUNITY typically produces.
#
# THE CONSTRAINT THAT DECIDES WHERE THIS IS LEGITIMATE. Condition on
# OPPORTUNITY, score CONVERSION. Contests attended is opportunity; hitouts won
# is conversion; conditioning on the first to reward the second is exactly
# right. Conditioning on something that IS the performance would make the
# channel unable to reward doing more of it -- the count-dependence trap in new
# clothing. That is why this is prototyped on hitout and not on recv or disp,
# where opportunity is not separately observed.
#
# Compared against both bucket schemes on the same three rucks and the same
# leaderboard, so "better" is a comparison and not an assertion.

suppressMessages({
  library(data.table); library(arrow); library(mgcv)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "continuous_centring.txt"), split = TRUE)
cat("=== Continuous centring, prototyped on hitout ===\nrun at", format(Sys.time()), "\n")

d <- as.data.table(read_parquet(file.path(OUT_DIR, "v2_schemec_pgd.parquet")))
d[, tog_safe := pmax(fcoalesce(time_on_ground_percentage / 100, 0.1), 0.1)]
d[, p80 := epv_hitout / tog_safe]
d[, rc := fcoalesce(as.numeric(ruck_contests), 0)]
S <- max(d$season, na.rm = TRUE)

cat("\n########## 1. THE SMOOTH ##########\n")
cat("per-80 hitout value against ruck contests attended, TOG-weighted.\n")
fit <- bam(p80 ~ s(rc, k = 20), data = d, weights = d$tog_safe, discrete = TRUE)
cat(sprintf("  deviance explained %.1f%% | edf %.1f\n",
            100 * summary(fit)$dev.expl, sum(fit$edf)))
grid <- data.table(rc = c(0, 5, 10, 15, 20, 30, 40, 55, 70, 85))
grid[, expected_p80 := as.numeric(predict(fit, newdata = grid))]
print(grid)
cat("\nThe bucket version replaces this whole curve with two numbers. The step\n")
cat("at rc = 10 is where a bucket puts a cliff and the smooth does not.\n")

d[, exp_p80 := as.numeric(predict(fit, newdata = d))]
d[, adj_cont := (p80 - exp_p80) * tog_safe]

cat("\n########## 2. THE THREE RUCKS ##########\n")
WHO <- c("Brodie Grundy", "Max Gawn", "Mason Cox")
t <- d[season == S & player_name %chin% WHO,
       .(gm = .N, tog = round(mean(tog_safe), 2), rc = round(mean(rc), 1),
         raw = round(mean(epv_hitout, na.rm = TRUE), 3),
         expected = round(mean(exp_p80), 2),
         bucket = round(mean(epv_hitout_adj, na.rm = TRUE), 3),
         continuous = round(mean(adj_cont, na.rm = TRUE), 3)), by = player_name]
setorder(t, -raw); print(t)
cat("\nCox is PARTLY a ruck. A bucket must call him wholly one or the other;\n")
cat("the smooth asks what a 26-contest player usually does and scores him\n")
cat("against that.\n")

cat("\n########## 3. THE LEADERBOARD ##########\n")
lb <- d[season == S, .(gm = .N, tog = round(mean(tog_safe) * 100, 0),
                       rc = round(mean(rc), 0),
                       raw = round(mean(epv_hitout, na.rm = TRUE), 2),
                       bucket = round(mean(epv_hitout_adj, na.rm = TRUE), 3),
                       cont = round(mean(adj_cont, na.rm = TRUE), 3)),
        by = player_name][gm >= 6]
setorder(lb, -bucket); cat("\ntop 10 by BUCKET (scheme C):\n"); print(lb[1:10])
setorder(lb, -cont);   cat("\ntop 10 by CONTINUOUS:\n");        print(lb[1:10])

cat("\n########## 4. THE PANEL METRICS ##########\n")
act <- lb[raw > quantile(lb$raw, 0.75, na.rm = TRUE)]
cat(sprintf("  bucket      cor(adj,raw) %+.3f | cor(adj,tog) %+.3f\n",
            cor(act$raw, act$bucket), cor(act$bucket, act$tog)))
cat(sprintf("  continuous  cor(adj,raw) %+.3f | cor(adj,tog) %+.3f\n",
            cor(act$raw, act$cont), cor(act$cont, act$tog)))
cat("\nAnd the thing a bucket cannot avoid -- behaviour at the threshold:\n")
near <- d[season == S & rc >= 5 & rc <= 15,
          .(n = .N, bucket = round(mean(epv_hitout_adj, na.rm = TRUE), 3),
            cont = round(mean(adj_cont, na.rm = TRUE), 3)),
          by = .(contests = round(rc))][order(contests)]
print(near)
cat("\nA jump between 9 and 10 in the bucket column that is absent from the\n")
cat("continuous one IS the cliff, quantified.\n")

saveRDS(list(fit_summary = summary(fit)$dev.expl, three = t, board = lb),
        file.path(OUT_DIR, "continuous_centring.rds"))
cat("\ndone", format(Sys.time()), "\n"); sink(); cat("\nDone\n")
