# EPV v3: score the three loser-allocation rules against each other.
#
# Chains names the beaten aerial opponent in only ~12% of contests, so ~86% of
# the contest credit mass has a debit with no name on it. Three ways to handle
# it, all defensible, none obviously right -- so measure rather than pick:
#
#   none      leave it unallocated; the channel becomes upside-only and the
#             per-role centring turns it into "contests won relative to role"
#   contested spread by CONTESTED aerial involvement (spoils, contested marks,
#             contest targets, fumbles)
#   wide      as above plus uncontested marks and marks on lead
#
# The failure mode of each is known and opposite. `contested` is nearly a win
# count, so the best contester absorbs the most debit. `wide` dilutes that but
# charges mark-and-kick defenders for duels they never entered.
#
# Decision criteria, in order:
#   1. cor(cont, contested_marks) and cor(cont, spoils) must be POSITIVE -- if
#      contesting more lowers your contest rating, the channel is inverted.
#   2. cor(cont, disposals) and cor(cont, marks) near zero -- possessions and
#      uncontested marks must not drive it.
#   3. key-defender level closer to zero than v2's -2.176.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_alloc_compare.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 60) for (l in capture.output(print(utils::head(x, n)))) say(l)

pbp    <- load_pbp(TRUE)
chains <- load_chains(TRUE)
stats_ <- as.data.table(load_player_stats(TRUE))
say("=== EPV v3: loser-allocation rules compared ===")
say("pbp ", format(nrow(pbp), big.mark = ","), " | chains ", format(nrow(chains), big.mark = ","))

box <- stats_[, .(player_id, match_id, disposals, marks, contested_marks,
                  spoils, one_percenters, contested_possessions,
                  uncontested_possessions, hitouts)]

arms <- list(
  none      = list(alloc = "none",    descs = EPV3_AERIAL_EXPOSURE_DESCS),
  contested = list(alloc = "prorata", descs = EPV3_AERIAL_EXPOSURE_DESCS),
  wide      = list(alloc = "prorata", descs = EPV3_AERIAL_EXPOSURE_WIDE),
  team      = list(alloc = "team",    descs = "team"),
  ledger    = list(alloc = "ledger",  descs = "ledger")
)

# Score the contests ONCE. The allocation rule changes only where the DEBIT
# lands, never the contest values themselves, so refitting the three branch
# models per arm (18 GAMs each) was pure waste -- it made this script take ~20
# minutes per arm for an answer the arms share.
cst <- build_aerial_contests(chains, pbp)
half <- attr(cst, "half")
cst[, .season := as.integer(substr(match_id, 5, 8))]
seasons <- sort(unique(cst$.season))
scored <- rbindlist(lapply(seasons, function(s) {
  idx <- cst$.season < s
  if (sum(idx) < 5000) idx <- cst$.season == s
  score_contests(cst[.season == s], fit_contest_models(cst, idx))
}))
say("scored contests: ", format(nrow(scored), big.mark = ","))

win <- scored[!is.na(winner_pid), .(cont = sum(winner_credit), won = .N),
              by = .(player_id = winner_pid, match_id)]
los <- scored[!is.na(loser_pid), .(cont = sum(loser_credit), won = 0L),
              by = .(player_id = loser_pid, match_id)]

res <- list()
for (nm in names(arms)) {
  a <- arms[[nm]]
  parts <- list(win, los)
  if (!identical(a$alloc, "none")) {
    al <- allocate_contest_losses(scored, chains, half, a$descs, stats_)
    parts <- c(parts, list(al[, .(player_id, match_id, cont = cont_alloc, won = 0L)]))
  }
  cr <- rbindlist(parts, use.names = TRUE)[
    , .(cont = sum(cont), won = sum(won)), by = .(player_id, match_id)]
  d <- merge(box, cr, by = c("player_id", "match_id"), all.x = TRUE)
  for (cc in c("cont", "won")) set(d, which(is.na(d[[cc]])), cc, 0)
  res[[nm]] <- d
  say("")
  say("--- arm: ", nm, " ---")
  say("n player-games ", format(nrow(d), big.mark = ","),
      " | cont mean ", round(mean(d$cont), 4), " sd ", round(sd(d$cont), 4),
      " | share negative ", round(100 * mean(d$cont < 0), 1), "%")
  say("  conserves? total cont = ", round(sum(d$cont), 1),
      "  [~0 means the debit landed; large positive means upside-only]")
  cc <- function(v) round(cor(d$cont, d[[v]], use = "complete.obs"), 3)
  say("  cor with contested_marks ", cc("contested_marks"),
      " | spoils ", cc("spoils"),
      " | one_percenters ", cc("one_percenters"),
      "   [criterion 1: all should be POSITIVE]")
  say("  cor with disposals ", cc("disposals"),
      " | marks ", cc("marks"),
      " | uncont_poss ", cc("uncontested_possessions"),
      "   [criterion 2: near zero]")
}

# ---- Criterion 1b: PERSISTENCE -- skill or noise? ---------------------------
# The correlations above test whether a channel tracks contest VOLUME. A channel
# that nets wins against losses should be less correlated with volume and that is
# a virtue, not a fault -- so a near-zero correlation is ambiguous on its own. It
# means "measures ability, not volume" if the channel repeats season to season,
# and "is noise" if it does not. This is the test that separates them.
say("")
say("--- criterion 1b: season-to-season persistence (same player, min 8 gms) ---")
say("higher = more skill-like. A channel that does not repeat is not measuring")
say("a player property, whatever its correlations look like.")
seas <- unique(as.data.table(load_player_stats(TRUE))[, .(player_id, match_id, season)])
pers <- rbindlist(lapply(names(res), function(nm) {
  d <- merge(res[[nm]], seas, by = c("player_id", "match_id"))
  ag <- d[, .(gms = .N, cpg = mean(cont)), by = .(player_id, season)][gms >= 8]
  ag[, nxt := season + 1L]
  j <- merge(ag[, .(player_id, season = nxt, prev = cpg)],
             ag[, .(player_id, season, cpg)], by = c("player_id", "season"))
  data.table(arm = nm, n_pairs = nrow(j),
             r_yoy = round(cor(j$prev, j$cpg, use = "complete.obs"), 3))
}))
say_dt(pers, 8)

# ---- Criterion 3: positional level and spread ------------------------------
pos <- unique(as.data.table(load_player_game_ratings())[
  , .(player_id, match_id)])  # placeholder if unavailable
pgd_pos <- unique(as.data.table(pbp)[!is.na(player_id),
  .(position_group = data.table::last(player_position)), by = .(player_id, match_id)])

say("")
say("--- criterion 3: contest credit by listed position (raw, per player-game) ---")
tbl <- rbindlist(lapply(names(res), function(nm) {
  d <- merge(res[[nm]], pgd_pos, by = c("player_id", "match_id"))
  d[!is.na(position_group), .(arm = nm, n = .N,
                              mean = round(mean(cont), 3),
                              sd = round(sd(cont), 3)), by = position_group]
}))
say_dt(dcast(tbl, position_group ~ arm, value.var = c("mean", "sd")), 12)

say("")
say("--- the two named problem cases ---")
say("(these are the players each rejected base got wrong; 2026 season sums)")
nmz <- as.data.table(load_player_stats(TRUE))[, .(player_id, player_name)] |> unique()
for (nm in names(res)) {
  d <- merge(res[[nm]], nmz, by = "player_id")
  d <- d[grepl("Caleb Daniel|Bradley Hill|Harris Andrews|Sam Taylor|Tom Barrass|Jacob Weitering",
               player_name)]
  s <- d[, .(cont = round(sum(cont), 1)), by = player_name][order(player_name)]
  say("arm ", nm, ": ", paste(sprintf("%s %.1f", s$player_name, s$cont), collapse = " | "))
}
say("")
say("Harris Andrews / Sam Taylor / Tom Barrass / Jacob Weitering are the")
say("intercept-defender population: they SHOULD read strongly positive.")
say("Caleb Daniel / Bradley Hill are mark-and-kick rebounders: near zero.")

close(con)
cat("\nWrote ", OUT, "\n")
