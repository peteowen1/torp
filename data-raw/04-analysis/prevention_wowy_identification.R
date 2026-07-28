# Plan §7.21 — Track D1 precondition: is a PLAYER effect on prevention
# identifiable at all?
#
# §7.20 established that defensive prevention is a stable team trait (r = 0.572,
# sitting at its own measurement ceiling) but is NOT cleanly separable from
# general team strength. That makes the obvious build actively dangerous:
# crediting every defender on the ground with his team's prevention residual
# would re-measure team quality and distribute it by appearance -- the same
# defect as the uncentred position-split features (§6.10) and the
# team-fixed-effects result (§7.8c), in a third costume.
#
# So the question here is not "does prevention persist" (it does) but:
#     does a team prevent MORE when a given defender plays?
# That is a with-or-without-you design, comparing a team against ITSELF, which
# is the only way to strip the team-level trait out.
#
# THE TEST THAT MATTERS IS THE NULL. Every player will show a non-zero WOWY
# delta by chance; with ~10 games out, noise alone produces a wide spread. So
# the observed spread of deltas is compared against a PERMUTATION null that
# preserves each player's games-played count and his team's match set, and
# only shuffles WHICH matches he played. If the observed spread does not
# exceed that null, there is no player effect to attribute and D1 closes.
suppressMessages({library(arrow); library(data.table)})

DD <- "C:/dev/torpverse/torpdata/data/"
SEASONS <- 2021:2026
MIN_IN  <- 20L   # games played for the team
MIN_OUT <- 8L    # games the team played without him
NPERM   <- 2000L
rdp <- function(pat, ss) rbindlist(lapply(ss, function(s) {
  f <- file.path(DD, sprintf(pat, s)); if (!file.exists(f)) return(NULL)
  as.data.table(read_parquet(f)) }), use.names=TRUE, fill=TRUE)

pg <- rdp("player_game_%d.parquet", SEASONS)
ent <- pg[!is.na(team_id), .(entries = sum(inside50s, na.rm=TRUE)),
          by = .(match_id, season, team_id)]
res <- rdp("results_%d.parquet", SEASONS)
res <- res[!is.na(home_score) & !is.na(away_score),
           .(match_id, season, home_team_id, away_team_id, home_score, away_score)]
sides <- rbind(
  res[, .(match_id, season, team_id=home_team_id, opp_id=away_team_id, conceded=away_score)],
  res[, .(match_id, season, team_id=away_team_id, opp_id=home_team_id, conceded=home_score)])
sides <- merge(sides, ent[, .(match_id, opp_id=team_id, opp_entries=entries)],
               by=c("match_id","opp_id"))
sides <- sides[!is.na(opp_entries) & opp_entries > 0]

# prevention residual, additionally adjusted for the OPPONENT's attacking
# quality -- a player's missed games are not a random sample of fixtures, and
# without this a defender who happens to miss games against good attacks looks
# like a preventer.
opp_q <- sides[, .(opp_ppe = sum(conceded)/sum(opp_entries)), by=.(opp_id, season)]
sides <- merge(sides, opp_q, by=c("opp_id","season"))
fit <- lm(conceded ~ opp_entries + opp_ppe, data = sides)
sides[, prevention := predict(fit, sides) - conceded]
cat(sprintf("team-matches: %s | model R2 %.3f (entries + opponent attacking quality)\n",
            format(nrow(sides), big.mark=","), summary(fit)$r.squared))

# --- who played, per team-match ---------------------------------------------
played <- unique(pg[!is.na(team_id), .(match_id, team_id, player_id, position_group)])
mdate <- unique(pg[, .(match_id, mdate = as.Date(utc_start_time))], by = "match_id")
team_matches <- unique(sides[, .(match_id, team_id, prevention)])
team_matches <- merge(team_matches, mdate, by = "match_id")
data.table::setorder(team_matches, team_id, mdate)   # chronological, for block nulls

# candidate (player, team) pairs with enough IN and OUT games
tm_count <- team_matches[, .(team_games = .N), by = team_id]
pl_count <- played[, .(games_in = .N), by = .(player_id, team_id)]
cand <- merge(pl_count, tm_count, by = "team_id")
cand[, games_out := team_games - games_in]
cand <- cand[games_in >= MIN_IN & games_out >= MIN_OUT]
kd <- unique(played[position_group == "KEY_DEFENDER", .(player_id, team_id)])
cand[, is_kd := paste(player_id, team_id) %in% paste(kd$player_id, kd$team_id)]
cat(sprintf("\ncandidate (player, team) pairs with >=%d in and >=%d out: %d (key defenders: %d)\n",
            MIN_IN, MIN_OUT, nrow(cand), sum(cand$is_kd)))

# --- observed WOWY delta ----------------------------------------------------
pl_key <- played[, .(player_id, team_id, match_id)]
data.table::setkey(pl_key, player_id, team_id)
tm_split <- split(team_matches, by = "team_id", keep.by = FALSE)

# NOTE: the candidate ids are pulled into plain vectors with DIFFERENT names
# before the loop. Writing pl_key[player_id == player_id[i] & ...] looks right
# and is silently wrong -- inside a data.table `[`, BOTH sides resolve to
# pl_key's own column, so it self-joins on its i-th row rather than filtering
# to the i-th candidate. The first draft did exactly that and produced WOWY
# deltas of 40 points per game, which is what flagged it.
cand_pid <- cand$player_id
cand_tid <- cand$team_id
deltas <- numeric(nrow(cand))
for (i in seq_len(nrow(cand))) {
  im <- pl_key[.(cand_pid[i], cand_tid[i]), match_id, nomatch = 0L]
  tm <- tm_split[[as.character(cand_tid[i])]]
  inw <- tm$match_id %in% im
  deltas[i] <- mean(tm$prevention[inw]) - mean(tm$prevention[!inw])
}
cand[, delta := deltas]

# Sanity floor: a single player cannot move his team's prevention by anything
# like a goal a game. If this trips, the join is wrong again.
if (max(abs(cand$delta), na.rm = TRUE) > 30) {
  cli::cli_abort("Implausible WOWY delta ({round(max(abs(cand$delta)),1)} pts/game) -- check the player/team join")
}
obs_sd <- sd(cand$delta, na.rm = TRUE)
obs_sd_kd <- sd(cand[is_kd == TRUE]$delta, na.rm = TRUE)

# --- permutation null: same games-played count, shuffled WHICH games ---------
set.seed(20260727)
perm_sd <- numeric(NPERM); perm_sd_kd <- numeric(NPERM)
for (p in seq_len(NPERM)) {
  d <- numeric(nrow(cand))
  for (i in seq_len(nrow(cand))) {
    tm <- tm_split[[as.character(cand$team_id[i])]]
    n <- nrow(tm); k <- cand$games_in[i]
    idx <- sample.int(n, k)
    d[i] <- mean(tm$prevention[idx]) - mean(tm$prevention[-idx])
  }
  perm_sd[p] <- sd(d)
  perm_sd_kd[p] <- sd(d[cand$is_kd])
}

# --- BLOCK permutation null: the one that actually matters ------------------
# The null above shuffles which games a player played UNIFORMLY at random. Real
# absences are contiguous spells (injury, suspension), and team form is
# autocorrelated -- so a player's in/out split correlates with team phase even
# when he has no effect whatsoever. A uniform null does not contain that, the
# observed data does, and the difference would look exactly like a player
# effect. This null preserves each player's absence STRUCTURE (number of spells
# and their lengths) and only slides those spells to random positions in his
# team's chronological match sequence.
runs_of_out <- function(out_flag) {
  r <- rle(out_flag)
  r$lengths[r$values]
}
place_blocks <- function(n, lens) {
  out <- logical(n)
  for (L in sort(lens, decreasing = TRUE)) {
    starts <- which(vapply(seq_len(max(1, n - L + 1)),
                           function(st) !any(out[st:(st + L - 1)]), logical(1)))
    if (!length(starts)) next
    st <- starts[sample.int(length(starts), 1)]
    out[st:(st + L - 1)] <- TRUE
  }
  out
}
out_struct <- vector("list", nrow(cand))
for (i in seq_len(nrow(cand))) {
  im <- pl_key[.(cand_pid[i], cand_tid[i]), match_id, nomatch = 0L]
  tm <- tm_split[[as.character(cand_tid[i])]]
  out_struct[[i]] <- runs_of_out(!(tm$match_id %in% im))
}
cat(sprintf("median absence spells per player: %.1f | median spell length: %.1f games
",
            median(vapply(out_struct, length, integer(1))),
            median(unlist(out_struct))))

set.seed(20260727)
blk_sd <- numeric(NPERM); blk_sd_kd <- numeric(NPERM)
for (p in seq_len(NPERM)) {
  d <- numeric(nrow(cand))
  for (i in seq_len(nrow(cand))) {
    tm <- tm_split[[as.character(cand_tid[i])]]
    o <- place_blocks(nrow(tm), out_struct[[i]])
    d[i] <- if (sum(!o) < 3 || sum(o) < 3) NA_real_ else
      mean(tm$prevention[!o]) - mean(tm$prevention[o])
  }
  blk_sd[p] <- sd(d, na.rm = TRUE)
  blk_sd_kd[p] <- sd(d[cand$is_kd], na.rm = TRUE)
}

cat("\n=== IS A PLAYER EFFECT IDENTIFIABLE? ===\n")
rep_line <- function(lbl, o, nulls) {
  cat(sprintf("  %-16s observed sd %.3f | null median %.3f [%.3f, %.3f] | p = %.3f\n",
              lbl, o, median(nulls), quantile(nulls,.025), quantile(nulls,.975),
              mean(nulls >= o)))
}
cat("-- uniform null (shuffles WHICH games; ignores absence structure) --\n")
rep_line("all players", obs_sd, perm_sd)
rep_line("key defenders", obs_sd_kd, perm_sd_kd)
cat("-- BLOCK null (preserves absence spells; the honest comparison) --\n")
rep_line("all players", obs_sd, blk_sd)
rep_line("key defenders", obs_sd_kd, blk_sd_kd)
cat("\n  p is P(chance spread >= observed). A player effect exists only if the\n")
cat("  observed spread exceeds the BLOCK null -- the uniform null omits the\n")
cat("  team-phase autocorrelation that contiguous absences necessarily pick up.\n")
.sig <- function(o, nulls) { v <- o^2 - stats::median(nulls)^2
  if (is.na(v) || v <= 0) 0 else sqrt(v) }
cat(sprintf("\n  implied TRUE player-effect sd (key defenders): uniform null %.2f | block null %.2f pts/game\n",
            .sig(obs_sd_kd, perm_sd_kd), .sig(obs_sd_kd, blk_sd_kd)))

# --- split-half: does a player's WOWY replicate within himself? -------------
cat("\n=== SPLIT-HALF: does a player's own WOWY delta replicate? ===\n")
sh <- cand[games_in >= 2*MIN_IN & games_out >= 2*MIN_OUT]
if (nrow(sh) >= 15) {
  vals <- t(vapply(seq_len(nrow(sh)), function(i) {
    tm <- tm_split[[as.character(sh$team_id[i])]]
    im <- pl_key[.(sh$player_id[i], sh$team_id[i]), match_id, nomatch = 0L]
    h <- seq_len(nrow(tm)) %% 2 == 0
    f <- function(sel) { z <- tm[sel]; iw <- z$match_id %in% im
      if (sum(iw) < 3 || sum(!iw) < 3) return(NA_real_)
      mean(z$prevention[iw]) - mean(z$prevention[!iw]) }
    c(f(h), f(!h))
  }, numeric(2)))
  ok <- complete.cases(vals)
  cat(sprintf("  n = %d players with both halves estimable; r = %+.3f\n",
              sum(ok), if (sum(ok) > 3) cor(vals[ok,1], vals[ok,2]) else NA_real_))
} else {
  cat(sprintf("  only %d players clear the doubled thresholds -- not estimable\n", nrow(sh)))
}

cat("\n=== biggest key-defender WOWY deltas (descriptive only) ===\n")
nm <- unique(pg[, .(player_id, player_name)], by="player_id")
top <- merge(cand[is_kd == TRUE], nm, by="player_id")
print(head(top[order(-abs(delta)), .(player_name, games_in, games_out,
                                     delta = round(delta, 2))], 10))
