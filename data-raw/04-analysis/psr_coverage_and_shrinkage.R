# B3 + C4 (plan §7.2, §7.3) — two cheap Block 1 items, both designed to CLOSE
# a line of enquiry rather than open one.
#
# B3: what can PSR actually see? PSR is a glmnet over per-stat skill ratings.
#     If the input stat list under-represents defensive acts, no reweighting of
#     the betas can price defenders correctly -- the information is not in the
#     matrix. This bounds everything else in Track B.
# C4: is shrinkage position-differential? .bayesian_shrink()'s factor is
#     wt_gms/(wt_gms + prior_games), which is position-independent GIVEN equal
#     weighted games, applied to already position-centred credit. The only route
#     to a differential effect is the wt_gms distribution. Measure it and close.
suppressMessages({library(arrow); library(data.table)})

D  <- "C:/dev/torpverse/torpdata/data/"
SF <- 2021:2026
BK <- c("KEY_DEFENDER","MEDIUM_DEFENDER","MIDFIELDER","RUCK","MEDIUM_FORWARD","KEY_FORWARD")
rd <- function(pat, ss) rbindlist(lapply(ss, function(s)
  as.data.table(read_parquet(file.path(D, sprintf(pat, s))))), use.names=TRUE, fill=TRUE)

# ============================ B3 =========================================
coefs <- fread("C:/dev/torpverse/torp/inst/extdata/psr_coefficients.csv")
live <- coefs[beta != 0]
cat("=== B3: PSR feature coverage ===\n")
cat(sprintf("stats in coefficient file: %d   with non-zero beta: %d\n",
            nrow(coefs), nrow(live)))

# Which of the live stats are defensive acts? Classified by name against what a
# key defender actually does. Anything not obviously defensive is left "other"
# rather than quietly counted as offensive.
def_pat <- paste("spoil|intercept|one_percent|onepercent|rebound|tackle|"
                ,"pressure|contest_def|def_|defensive|clanger_opp|smother"
                , sep="")
live[, kind := fifelse(grepl(def_pat, stat_name, ignore.case=TRUE), "DEFENSIVE", "other")]
cat("\nlive PSR stats classified:\n")
print(live[order(-abs(beta)), .(stat_name, beta = round(beta,3), kind)][1:min(40,.N)])
cat(sprintf("\ndefensive-act stats with a live beta: %d of %d (%.0f%%)\n",
            sum(live$kind=="DEFENSIVE"), nrow(live), 100*mean(live$kind=="DEFENSIVE")))
cat("defensive stats present: ",
    paste(live[kind=="DEFENSIVE", stat_name], collapse=", "), "\n")

# What defensive stats EXIST in the stat-ratings frame but never reach PSR?
sr1 <- as.data.table(read_parquet(file.path(D, "player_stat_ratings_2024.parquet")))
avail <- sub("_rating$", "", grep("_rating$", names(sr1), value=TRUE))
def_avail <- grep(def_pat, avail, value=TRUE, ignore.case=TRUE)
missing <- setdiff(def_avail, live$stat_name)
zeroed  <- intersect(def_avail, coefs[beta == 0, stat_name])
cat(sprintf("\ndefensive stats AVAILABLE in the ratings frame: %d\n", length(def_avail)))
cat("  -> never offered to PSR (absent from coef file): ",
    paste(setdiff(missing, zeroed), collapse=", "), "\n")
cat("  -> offered but glmnet zeroed them: ", paste(zeroed, collapse=", "), "\n")

# ============================ C4 =========================================
cat("\n\n=== C4: is shrinkage position-differential? ===\n")
sr <- rd("player_stat_ratings_%d.parquet", SF)
pg <- rd("player_game_%d.parquet", SF)[, .(player_id, season, round=as.numeric(round),
        position_group, lineup_position, tog = time_on_ground_percentage/100)]
sr[, round := as.numeric(round)]
m <- merge(pg[position_group %in% BK & !lineup_position %in% c("EMERG","SUB")],
           sr[, .(player_id, season, round, wt_80s, wt_games, n_games)],
           by=c("player_id","season","round"))

# EPR's shrink factor for the recv/disp channels (prior_games = 3.0)
PG <- 3.0
m[, shrink := wt_games / (wt_games + PG)]
out <- m[!is.na(wt_games), .(
  n          = .N,
  med_wt_gms = round(median(wt_games, na.rm=TRUE), 2),
  med_tog    = round(median(tog, na.rm=TRUE), 3),
  med_shrink = round(median(shrink, na.rm=TRUE), 4),
  p10_shrink = round(quantile(shrink, .10, na.rm=TRUE), 4)
), by = position_group][order(position_group)]
print(out)
rng <- range(out$med_shrink)
cat(sprintf("\nmedian shrink factor spans %.4f - %.4f across buckets (ratio %.3f)\n",
            rng[1], rng[2], rng[2]/rng[1]))
cat("A material differential would need this ratio to be well away from 1.000.\n")
