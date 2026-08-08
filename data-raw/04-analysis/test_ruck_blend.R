# The blend: does it get all three at once?
#
# No scheme so far has. Stated before the run:
#
#   1. ORDERING   Grundy > Gawn > Cox, matching raw output 5.55 / 4.98 / 2.14.
#                 The bucket passes this; both continuous versions failed it.
#   2. NO CLIFF   the 9-to-10 contest jump under 0.25. The bucket is 2.601.
#   3. PRODUCTION cor(adj, raw) at or near the bucket's 0.750. This is the one
#                 both continuous versions lost -- 0.482 on volume, 0.023 on
#                 share -- because they conditioned on a variable carrying the
#                 output. The blend should not, since its references are two
#                 fixed cell means and only the weight moves.
#
# Failing 3 while passing 1 and 2 would mean blending is just a smoother way of
# conditioning output away, and the bucket stays.

suppressMessages({
  library(dplyr); library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "ruck_blend_test.txt"), split = TRUE)
cat("=== The blended ruck reference ===\nrun at", format(Sys.time()), "\n")

set_const <- function(l) for (nm in names(l)) assignInNamespace(nm, l[[nm]], ns = "torp")
with_const <- function(l, expr) {
  old <- lapply(names(l), function(nm) get(nm, envir = asNamespace("torp")))
  names(old) <- names(l); set_const(l); on.exit(set_const(old), add = TRUE); force(expr)
}
V2 <- list(EPV_ENGINE = "v2", EPV3_STOP_ZERO_SUM = FALSE,
           EPV_STANDARDISE_CHANNELS = c("recv", "disp", "spoil"),
           EPV_DIFFICULTY_SPLIT = FALSE, EPV_PER_CHANNEL_POINTS_SCALE = FALSE,
           EPV_POINTS_SCALE = 0.919, ROLE_REMAP_BENCH = TRUE,
           EPV_HITOUT_CENTRE_ON_RUCK = TRUE)

pbp <- load_pbp(TRUE); stats_ <- load_player_stats(TRUE)
teams <- load_teams(TRUE); chains <- load_chains(TRUE)

bucket <- as.data.table(read_parquet(file.path(OUT_DIR, "v2_schemec_pgd.parquet")))
f <- file.path(OUT_DIR, "v2_blend_pgd.parquet")
blend <- if (file.exists(f)) {
  cli::cli_alert_info("Reusing blend frame"); as.data.table(read_parquet(f))
} else {
  d <- with_const(c(V2, list(EPV_RUCK_BLEND_WIDTH = 10)),
    as.data.table(create_player_game_data(pbp, stats_, teams, chains, epv_engine = "v2")))
  write_parquet(d, f); d
}
S <- max(bucket$season, na.rm = TRUE)
cat("\nraw hitout identical across arms:",
    isTRUE(all.equal(bucket$epv_hitout, blend$epv_hitout)), "\n")

j <- merge(bucket[, .(match_id, player_id, player_name, season,
                      time_on_ground_percentage, ruck_contests,
                      raw = epv_hitout, bkt = epv_hitout_adj)],
           blend[, .(match_id, player_id, bld = epv_hitout_adj)],
           by = c("match_id", "player_id"))
j[, tog := pmax(fcoalesce(time_on_ground_percentage / 100, 0.1), 0.1)]
j[, rc := fcoalesce(as.numeric(ruck_contests), 0)]

cat("\n########## TEST 1: ORDERING ##########\n")
WHO <- c("Brodie Grundy", "Max Gawn", "Mason Cox")
t <- j[season == S & player_name %chin% WHO,
       .(rc = round(mean(rc), 1), raw = round(mean(raw), 3),
         bucket = round(mean(bkt), 3), blend = round(mean(bld), 3)), by = player_name]
setorder(t, -raw); print(t)
ok1 <- t$blend[1] > t$blend[2] && t$blend[2] > t$blend[3]
cat("  Grundy > Gawn > Cox on the blend:", ifelse(ok1, "PASS", "FAIL"), "\n")

cat("\n########## TEST 2: THE CLIFF ##########\n")
near <- j[season == S & rc >= 5 & rc <= 15,
          .(n = .N, bucket = round(mean(bkt), 3), blend = round(mean(bld), 3)),
          by = .(contests = round(rc))][order(contests)]
print(near)
jb <- abs(near[contests == 10]$bucket - near[contests == 9]$bucket)
jl <- abs(near[contests == 10]$blend - near[contests == 9]$blend)
ok2 <- jl < 0.25
cat(sprintf("  9->10 jump: bucket %.3f | blend %.3f -> %s\n", jb, jl,
            ifelse(ok2, "PASS", "FAIL")))

cat("\n########## TEST 3: PRODUCTION ##########\n")
pl <- j[season == S, .(gm = .N, tog = round(mean(tog) * 100, 0), rc = round(mean(rc), 0),
                       raw = round(mean(raw), 2), bucket = round(mean(bkt), 3),
                       blend = round(mean(bld), 3)), by = player_name][gm >= 6]
act <- pl[raw > quantile(pl$raw, 0.75, na.rm = TRUE)]
cb <- cor(act$raw, act$bucket); cl <- cor(act$raw, act$blend)
cat(sprintf("  cor(adj, raw):  bucket %+.3f | blend %+.3f\n", cb, cl))
cat(sprintf("  cor(adj, tog):  bucket %+.3f | blend %+.3f   (raw itself is %+.3f)\n",
            cor(act$bucket, act$tog), cor(act$blend, act$tog), cor(act$raw, act$tog)))
ok3 <- cl > cb - 0.10
cat("  production held within 0.10 of the bucket:", ifelse(ok3, "PASS", "FAIL"), "\n")

cat("\n########## THE LEADERBOARD ##########\n")
setorder(pl, -blend); print(pl[1:12, .(player_name, tog, rc, raw, bucket, blend)], nrows = 14)

cat("\n########## VERDICT ##########\n")
cat(sprintf("  1 ordering %s | 2 no cliff %s | 3 production %s\n",
            ifelse(ok1, "PASS", "FAIL"), ifelse(ok2, "PASS", "FAIL"),
            ifelse(ok3, "PASS", "FAIL")))
cat(if (ok1 && ok2 && ok3)
  "  ALL THREE -- the first scheme to manage it. Ship candidate.\n" else
  "  NOT all three. The bucket stays until something does better.\n")

saveRDS(list(three = t, cliff = near, board = pl), file.path(OUT_DIR, "ruck_blend_test.rds"))
cat("\ndone", format(Sys.time()), "\n"); sink(); cat("\nDone\n")
