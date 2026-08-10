# Why do 17.9% of rating rows carry NA epr?
#
# 23,574 of 131,740 rows in the ratings frame have epr = NA -- identical in v2
# and both v3 arms, so this is PRE-EXISTING and affects production today. It was
# found incidentally while gating EPV v3 and has not been looked at on its own.
#
# It matters because .build_team_ratings_df() imputes those rows with the prior
# rate (-1.84 total), so a player in that group is scored as a generic
# below-average footballer rather than as himself. If the group is concentrated
# -- debutants, players returning from a long lay-off, a particular round or
# season -- that is a real and fixable accuracy leak in the match model.
#
# centre_epr_by_position() is the proximate cause: "N rows have a non-finite
# channel; epr set to NA rather than a partial sum". This asks which channel,
# and who.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "na_epr_population.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

r <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "epv3_ratings_v2.parquet")))
say("=== NA epr population ===")
say("rating rows ", format(nrow(r), big.mark = ","),
    " | NA epr ", format(sum(is.na(r$epr)), big.mark = ","),
    " (", round(100 * mean(is.na(r$epr)), 1), "%)")

CH <- c("epr_recv", "epr_disp", "epr_spoil", "epr_hitout")
say("")
say("--- which channel is non-finite? ---")
say_dt(rbindlist(lapply(CH, function(c) data.table(
  channel = c,
  n_na = sum(is.na(r[[c]])),
  n_nonfinite = sum(!is.finite(r[[c]])),
  pct = round(100 * mean(!is.finite(r[[c]])), 2)
))), 6)
say("")
say("rows where ALL four channels are non-finite: ",
    sum(Reduce(`&`, lapply(CH, function(c) !is.finite(r[[c]])))))
say("rows where SOME but not all are: ",
    sum(Reduce(`|`, lapply(CH, function(c) !is.finite(r[[c]]))) &
        !Reduce(`&`, lapply(CH, function(c) !is.finite(r[[c]])))))

say("")
say("--- by season and round ---")
say_dt(r[, .(n = .N, na = sum(is.na(epr)), pct = round(100 * mean(is.na(epr)), 1)),
         by = season][order(season)], 10)
say("")
say("round 0 vs the rest (round 0 exists from 2024 and has little history):")
say_dt(r[, .(n = .N, na = sum(is.na(epr)), pct = round(100 * mean(is.na(epr)), 1)),
         by = .(round_grp = fifelse(round <= 1, "round 0-1", "round 2+"))], 5)

say("")
say("--- is it players with no prior games? ---")
# A rating for (player, season, round) needs past player-games. Count how many
# each rated player actually had before that round.
pgd <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "epv3_player_game_v2.parquet")))
gm <- pgd[, .(gms_total = .N), by = player_id]
r2 <- merge(r, gm, by = "player_id", all.x = TRUE)
r2[is.na(gms_total), gms_total := 0L]
say_dt(r2[, .(n = .N, na = sum(is.na(epr)), pct = round(100 * mean(is.na(epr)), 1)),
          by = .(career_games = cut(gms_total, c(-1, 0, 5, 20, 50, 100, 1e6),
                                    labels = c("0", "1-5", "6-20", "21-50",
                                               "51-100", "100+")))][order(career_games)], 8)

say("")
say("--- do the NA rows even correspond to players who PLAYED? ---")
say("(a rating row for a player who never appears in a lineup costs nothing)")
teams <- as.data.table(load_teams(TRUE))
played <- unique(teams[, .(player_id, season, round = round_number)])
played[, .in_lineup := TRUE]
r3 <- merge(r, played, by = c("player_id", "season", "round"), all.x = TRUE)
r3[is.na(.in_lineup), .in_lineup := FALSE]
say_dt(r3[, .(n = .N, na = sum(is.na(epr)), pct = round(100 * mean(is.na(epr)), 1)),
          by = .in_lineup], 4)
say("")
say("NA rows that ARE in a named lineup: ",
    format(r3[.in_lineup == TRUE & is.na(epr), .N], big.mark = ","))
say("  ^ this is the number that actually costs anything. The rest are rating")
say("    rows for players who were not selected that round.")

say("")
say("--- the ones that matter: who are they? ---")
bad <- r3[.in_lineup == TRUE & is.na(epr)]
if (nrow(bad) > 0) {
  say_dt(bad[, .N, by = season][order(season)], 8)
  nm <- if ("player_name" %in% names(bad)) "player_name" else NULL
  if (!is.null(nm)) {
    say("most affected players:")
    say_dt(bad[, .N, by = c(nm)][order(-N)][1:15], 15)
  }
} else {
  say("NONE. Every NA rating row belongs to a player who was not selected,")
  say("so the imputation never reaches a team feature and this costs nothing.")
}

close(con)
cat("\nWrote ", OUT, "\n")
