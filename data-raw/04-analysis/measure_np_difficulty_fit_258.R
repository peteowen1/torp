# torp#258: published net points from the PRIOR-season difficulty fit.
# Measures old (in-sample) vs new (prior-season) on real seasons and proves the
# wiring. Reads releases only (local torpdata/data is bypassed).
#
# Anchors, set before running:
#   A1  the in-sample arm reproduces the published player_game_<s> exactly
#   A2  the new default is identical() to an explicit
#       np_difficulty_terms_for_season() build (the gate scripts' basis)
#   A3  both arms still sum to each team's margin in every match
#   A4  the season fit bundle from the new arm carries disposal AND contest
#       models, and pricing one match on it gives that match's nightly numbers
# Expected size (worker run on 2026, pre-v17): mean |change| ~0.1 a
# player-match, correlation ~0.999.
#
# Usage: powershell.exe -Command 'Rscript data-raw/04-analysis/measure_np_difficulty_fit_258.R'
#   env SEASONS (default "2026,2021"), OUT_DIR (default tempdir()).
suppressMessages({ library(data.table); devtools::load_all(quiet = TRUE) })
options(torp.local_data_dir = NA)
seasons <- as.integer(strsplit(Sys.getenv("SEASONS", "2026,2021"), ",")[[1]])
out_dir <- Sys.getenv("OUT_DIR", tempdir())
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
ns <- asNamespace("torp")
set_fit <- function(v) {
  unlockBinding("NP_DIFFICULTY_FIT", ns)
  assign("NP_DIFFICULTY_FIT", v, envir = ns)
  lockBinding("NP_DIFFICULTY_FIT", ns)
}
cols <- c("net_points", "epv", "epv_recv", "epv_disp", "epv_spoil")
say <- function(...) cat(sprintf(...), "\n", sep = "")

for (s in seasons) {
  say("\n==================== %d ====================", s)
  pbp <- load_pbp(s, rounds = TRUE); chains <- load_chains(s, rounds = TRUE)
  pstats <- load_player_stats(s); teams <- load_teams(s)
  say("loaded %d pbp rows, %d chains rows, %d matches", nrow(pbp), nrow(chains),
      uniqueN(pbp$match_id))

  t0 <- Sys.time()
  set_fit("in_sample")
  A <- as.data.table(create_player_game_data(pbp, pstats, teams, chains = chains))
  set_fit("prior_season")
  B <- create_player_game_data(pbp, pstats, teams, chains = chains, keep_np_fit = TRUE)
  fitB <- attr(B, "np_fit"); B <- as.data.table(B)
  say("two builds: %.1f min", as.numeric(difftime(Sys.time(), t0, units = "mins")))

  # A1: in-sample arm == published
  pub <- as.data.table(load_player_game_data(s))
  k <- c("player_id", "match_id")
  m <- merge(pub[, c(k, cols), with = FALSE], A[, c(k, cols), with = FALSE], by = k,
             suffixes = c(".pub", ".A"))
  dA <- max(sapply(cols, function(cc) max(abs(m[[paste0(cc, ".pub")]] - m[[paste0(cc, ".A")]]))))
  say("A1 published %d rows, A %d rows, matched %d; max |A - published| = %.3g  %s",
      nrow(pub), nrow(A), nrow(m), dA, if (nrow(m) == nrow(pub) && dA < 1e-9) "PASS" else "FAIL")
  # 2026 run 1: 3 of 218 matches differed, all where the live player-stats API
  # has since revised box-score numbers -- input drift, not code. Listed so a
  # code difference would show as many matches, not a few.
  dm <- m[abs(net_points.pub - net_points.A) > 1e-9, .(n = .N), by = match_id]
  say("   net_points differs in %d of %d matches: %s", nrow(dm), uniqueN(m$match_id),
      paste(head(dm$match_id, 8), collapse = ", "))

  # A3: conservation, each team's players sum to its own margin
  res <- .np_match_results(pbp, NULL)
  for (nm in c("A", "B")) {
    d <- get(nm)
    tot <- d[, .(np = sum(net_points)), by = .(match_id, team)]
    r <- rbind(res[, .(match_id, team = home_team_name, margin = home_score - away_score)],
               res[, .(match_id, team = away_team_name, margin = away_score - home_score)])
    j <- merge(tot, r, by = c("match_id", "team"))
    say("A3 %s: %d team-matches joined of %d; max |sum - margin| = %.3g", nm, nrow(j),
        nrow(tot), max(abs(j$np - j$margin)))
  }

  # A4: bundle carries both model sets
  say("A4 bundle: disposal models %s, contest models %s",
      if (is.null(fitB$disposal_models)) "MISSING" else "present",
      if (is.null(fitB$contest_models)) "MISSING" else "present")

  if (s == max(seasons)) {
    # A2: default == explicit prior-season terms
    terms <- np_difficulty_terms_for_season(s, pbp, chains)
    C <- as.data.table(create_player_game_data(pbp, pstats, teams, chains = chains,
                                               difficulty_terms = terms))
    setkeyv(B, k); setkeyv(C, k)
    say("A2 default vs explicit terms: identical net_points = %s, identical epv = %s",
        identical(B$net_points, C$net_points), identical(B$epv, C$epv))

    # A4b: one match priced on the bundle == that match in the season frame
    bundle <- .np_fit_bundle(structure(B, np_fit = fitB), s)
    mid <- tail(sort(unique(pbp$match_id)), 1)
    # The loaders return tibbles, so subset with base indexing, not dt[i].
    sub <- function(x) {
      key <- intersect(c("match_id", "providerId"), names(x))[1]
      if (is.na(key)) stop("no match key in: ", paste(head(names(x), 20), collapse = ", "))
      # Index computed outside the bracket: chains has its own `x` column,
      # which data.table would read in place of this argument.
      keep <- which(as.character(x[[key]]) == mid)
      x[keep, ]
    }
    one <- as.data.table(create_player_game_data(
      sub(pbp), sub(pstats), sub(teams), chains = sub(chains), np_fit = bundle))
    j <- merge(B[match_id == mid, .(player_id, np_season = net_points)],
               one[, .(player_id, np_live = net_points)], by = "player_id")
    say("A4b live %s: %d players matched; max |live - season| net_points = %.3g",
        mid, nrow(j), max(abs(j$np_season - j$np_live)))
  }

  # size of the change
  d <- merge(A[, c(k, "player_name", "team", cols), with = FALSE],
             B[, c(k, cols), with = FALSE], by = k, suffixes = c(".old", ".new"))
  d[, chg := net_points.new - net_points.old]
  say("player-matches %d: mean |chg| %.3f (mean |net_points| %.2f), p99 %.2f, max %.2f, cor %.5f, spearman %.5f",
      nrow(d), mean(abs(d$chg)), mean(abs(d$net_points.old)), quantile(abs(d$chg), .99),
      max(abs(d$chg)), cor(d$net_points.old, d$net_points.new),
      cor(d$net_points.old, d$net_points.new, method = "spearman"))
  for (cc in setdiff(cols, "net_points")) {
    say("  %-10s mean |chg| %.3f  max %.2f", cc,
        mean(abs(d[[paste0(cc, ".new")]] - d[[paste0(cc, ".old")]])),
        max(abs(d[[paste0(cc, ".new")]] - d[[paste0(cc, ".old")]])))
  }
  sea <- d[, .(games = .N, old = sum(net_points.old), new = sum(net_points.new)),
           by = .(player_id, player_name)][games >= 10]
  sea[, `:=`(rank_old = frank(-old), rank_new = frank(-new))]
  say("season totals, %d players with >= 10 games: cor %.5f, mean |chg| %.2f, max |chg| %.2f; top-20 overlap %d/20; max rank shift in old top 50 = %d",
      nrow(sea), cor(sea$old, sea$new), mean(abs(sea$new - sea$old)),
      max(abs(sea$new - sea$old)), length(intersect(sea[rank_old <= 20, player_id],
                                                     sea[rank_new <= 20, player_id])),
      as.integer(max(abs(sea[rank_old <= 50, rank_new - rank_old]))))
  setorder(sea, rank_old)
  print(sea[1:10, .(player_name, games, old = round(old, 1), new = round(new, 1),
                    rank_old, rank_new)])
  big <- sea[order(-abs(new - old))][1:5]
  say("largest season movers:")
  print(big[, .(player_name, games, old = round(old, 1), new = round(new, 1), rank_old, rank_new)])
  fwrite(d, file.path(out_dir, sprintf("np258_player_match_%d.csv", s)))
  fwrite(sea, file.path(out_dir, sprintf("np258_season_%d.csv", s)))
}
say("\nwritten to %s", out_dir)
