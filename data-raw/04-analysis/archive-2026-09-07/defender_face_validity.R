# defender_face_validity.R ------------------------------------------------
# The eyeball test. Panna's #163 lesson: a derivation can be dimensionally
# correct and the output still absurd, and only a human who knows the sport
# catches it. Nothing in the defender-value program should ship before Pete
# has read these lists.
#
# Prints, for a chosen season:
#   1. Top key defenders by CURRENT published TORP rating
#   2. Top key defenders by FLAT spoil credit (spoils x EPV_SPOIL_WT)
#   3. Top key defenders by CONTEXTUAL spoil credit (WS2a)
#   4. Overall top-20 players by TORP, to show the positional skew
#
# If (3) is a better list of actual key defenders than (2), WS2a is measuring
# something real regardless of the reliability result. If it is worse, the
# reliability drop is telling us the measure is broken, not just noisy.
#
#   powershell.exe -Command 'Rscript "torp/data-raw/04-analysis/defender_face_validity.R"'
# -------------------------------------------------------------------------

suppressMessages({
  library(arrow)
  library(data.table)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

DATA_DIR <- "C:/dev/torpverse/torpdata/data/"
SEASONS  <- c(2024, 2025)
MIN_GAMES <- 12

load_stub <- function(stub, season) {
  fs <- list.files(DATA_DIR, pattern = sprintf("^%s_%d_\\d+\\.parquet$", stub, season),
                   full.names = TRUE)
  rbindlist(lapply(fs, function(f) as.data.table(read_parquet(f))),
            use.names = TRUE, fill = TRUE)
}

show_n <- function(dt, n = 15) print(head(dt, n))

for (S in SEASONS) {
  cat(sprintf("\n\n############################  %d  ############################\n", S))

  pg <- as.data.table(read_parquet(file.path(DATA_DIR, sprintf("player_game_%d.parquet", S))))
  ctx <- compute_spoil_credit(load_stub("chains_data", S), load_stub("pbp_data", S))
  con <- compute_contest_credit(load_stub("chains_data", S), load_stub("pbp_data", S))

  d <- merge(pg[, .(player_id, player_name, match_id, position_group, team,
                    tog = time_on_ground_percentage / 100, spoils)],
             ctx[, .(player_id, match_id, spoil_epv_ctx)],
             by = c("player_id", "match_id"), all.x = TRUE)
  d <- merge(d, con[, .(player_id, match_id, contest_epv)],
             by = c("player_id", "match_id"), all.x = TRUE)
  for (v in c("spoil_epv_ctx", "contest_epv")) set(d, which(is.na(d[[v]])), v, 0)
  d[, `:=`(ctx_total = spoil_epv_ctx + contest_epv,
           flat_total = spoils * EPV_SPOIL_WT)]

  agg <- d[tog > 0.5, .(gms = .N,
                        team = data.table::last(team),
                        spoils = round(mean(spoils), 1),
                        flat = round(mean(flat_total), 3),
                        ctx = round(mean(ctx_total), 3)),
           by = .(player_id, player_name, position_group)][gms >= MIN_GAMES]

  tr <- as.data.table(read_parquet(file.path(DATA_DIR, "torp_ratings.parquet")))
  last_rating <- tr[season == S][order(player_id, round)][
    , .(torp = data.table::last(torp), epr = data.table::last(epr),
        psr = data.table::last(psr)), by = player_id]
  agg <- merge(agg, last_rating, by = "player_id", all.x = TRUE)

  kd <- agg[position_group == "KEY_DEFENDER"]

  cat("\n--- 1. KEY DEFENDERS by CURRENT published TORP rating ---\n")
  show_n(kd[order(-torp), .(player_name, team, gms, torp, epr, psr, spoils)])

  cat("\n--- 2. KEY DEFENDERS by FLAT spoil credit (what torp uses today) ---\n")
  show_n(kd[order(-flat), .(player_name, team, gms, spoils, flat, torp)])

  cat("\n--- 3. KEY DEFENDERS by CONTEXTUAL spoil credit (WS2a) ---\n")
  show_n(kd[order(-ctx), .(player_name, team, gms, spoils, ctx, flat, torp)])

  cat("\n--- 4. OVERALL top 20 by TORP rating (positional skew check) ---\n")
  show_n(agg[order(-torp), .(player_name, position_group, team, torp)], 20)

  cat("\n--- 5. best key defender vs best overall, same season ---\n")
  b_kd <- kd[which.max(torp)]; b_all <- agg[which.max(torp)]
  cat(sprintf("  best KEY_DEFENDER : %-22s TORP %.2f\n", b_kd$player_name, b_kd$torp))
  cat(sprintf("  best overall      : %-22s TORP %.2f  (%s)\n",
              b_all$player_name, b_all$torp, b_all$position_group))
  cat(sprintf("  ratio             : %.2fx\n", b_all$torp / b_kd$torp))
}
