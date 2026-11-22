# One-off repair: wrong clubs on the 2026 finals injury scrapes.
#
# Before the scraper read each table's club off the page (#181/#183, merged
# 4 Sep 2026), it named clubs by table ORDER. In finals the AFL injury page
# lists only the clubs still playing, so from 24 Aug to 1 Sep most rows were
# filed under the wrong club (e.g. Aaron Naughton, Western Bulldogs, under
# Hawthorn on 25 Aug and Gold Coast on 31 Aug). Every scrape from 4 Sep on
# agrees with the player's own club. Reported by fplverse, 2026-11-22.
#
# What this does, on a FRESH download of injury_list_2026.parquet:
#   - for weekly rows scraped in the bad window, looks the player up by name
#     among 2026 club lists (who he played for in teams_2026, plus the squad
#     lists from get_afl_player_details(2026));
#   - relabels `team` only when the name maps to exactly ONE club (full name
#     first, then first initial + surname for nicknames); a name that maps to
#     two clubs (there are two Max Kings and two Will Edwards) or none is
#     left as scraped and reported;
#   - adds `team_check`: "relabelled", "unverified" (in the bad window but
#     not resolvable, so its club may be wrong) or "as scraped" (every other
#     row in the file). Rows written by later scrapes read NA: the fixed
#     scraper wrote them;
#   - re-applies save_injury_data()'s weekly dedup, since a relabelled row
#     can now duplicate an earlier correct one, and reports rows before/after.
#
# Dry run by default. Upload only with Pete's yes (given 2026-11-22):
#   Rscript data-raw/01-data/repair_injury_clubs_2026.R --upload

devtools::load_all(quiet = TRUE)
library(data.table)

UPLOAD <- "--upload" %in% commandArgs(trailingOnly = TRUE)
WINDOW <- as.Date(c("2026-08-19", "2026-09-03"))   # finals pages, before the fix

dl_dir <- file.path(tempdir(), "injury_repair")
dir.create(dl_dir, showWarnings = FALSE)
status <- system2("gh", c("release", "download", "injury-data", "--repo", get_torp_data_repo(),
                          "--pattern", "injury_list_2026.parquet", "--dir", dl_dir, "--clobber"))
stopifnot(status == 0)
path <- file.path(dl_dir, "injury_list_2026.parquet")
inj <- as.data.table(arrow::read_parquet(path))
cat(sprintf("Fresh release copy: %d rows x %d cols, md5 %s, scraped %s to %s\n",
            nrow(inj), ncol(inj), tools::md5sum(path),
            min(inj$scraped_date), max(inj$scraped_date)))
stopifnot(nrow(inj) > 2000, !"team_repaired" %in% names(inj))

# Name -> set of 2026 clubs, from who he played for and the squad lists.
canon <- function(x) {
  al <- AFL_TEAM_ALIASES
  ifelse(x %in% names(al), unname(al[x]), x)
}
tm <- as.data.table(load_teams(2026))
played <- unique(tm[, .(player_norm = norm_name(paste(given_name, surname)),
                        player_id, club = canon(team_name))])
pd <- as.data.table(get_afl_player_details(2026))
squads <- unique(pd[, .(player_norm = norm_name(player_name), player_id, club = canon(team))])
people <- unique(rbind(played, squads))
cat(sprintf("Club sources: %d lineup players, %d squad players, %d distinct people\n",
            uniqueN(played$player_id), uniqueN(squads$player_id), uniqueN(people$player_id)))
# A player who moved mid-season would have two clubs under one id; count them.
moved <- people[, uniqueN(club), by = player_id][V1 > 1]
cat(sprintf("Players with more than one 2026 club: %d\n", nrow(moved)))
name_clubs <- people[, .(n_clubs = uniqueN(club), club = club[1]), by = player_norm]
# Fallback for nicknames the squad list spells out (Lachie/Lachlan, Matt/
# Matthew, Oli/Oliver): first initial + surname, used only when the full name
# found no one. Still requires exactly one club.
initial_key <- function(x) {
  parts <- strsplit(x, " ", fixed = TRUE)
  vapply(parts, function(p) paste(substr(p[1], 1, 1), paste(p[-1], collapse = " ")), character(1))
}
people[, ikey := initial_key(player_norm)]
initial_clubs <- people[, .(i_n_clubs = uniqueN(club), i_club = club[1],
                            i_names = paste(unique(player_norm), collapse = "/")), by = ikey]

inj[, row := .I]
inj[, team_canon := canon(team)]
target <- inj[source == "weekly" & scraped_date >= WINDOW[1] & scraped_date <= WINDOW[2]]
target <- merge(target, name_clubs, by = "player_norm", all.x = TRUE)
target[, ikey := initial_key(player_norm)]
target <- merge(target, initial_clubs, by = "ikey", all.x = TRUE)
target[, by_initial := is.na(n_clubs) & !is.na(i_n_clubs)]
target[by_initial == TRUE, `:=`(n_clubs = i_n_clubs, club = i_club)]
cat("\nMatched by first initial + surname:\n")
print(unique(target[by_initial == TRUE, .(player, matched = i_names, n_clubs, club)]))
target[, verdict := fifelse(is.na(n_clubs), "no club found",
                    fifelse(n_clubs > 1, "name on two clubs",
                    fifelse(club == team_canon, "already right", "relabel")))]
cat("\nWeekly rows in the window, by scrape date and verdict:\n")
print(dcast(target, scraped_date ~ verdict, fun.aggregate = length, value.var = "row"))
cat("\nNot relabelled (left as scraped):\n")
print(target[verdict %in% c("no club found", "name on two clubs"), .(scraped_date, player, team)])

fix <- target[verdict == "relabel"]
unverified <- target[verdict %in% c("no club found", "name on two clubs")]
inj[, team_check := "as scraped"]
inj[fix$row, `:=`(team = fix$club, team_check = "relabelled")]
inj[unverified$row, team_check := "unverified"]
cat(sprintf("\nRelabelled %d rows. Examples:\n", nrow(fix)))
print(head(fix[, .(scraped_date, player, from = team, to = club)], 10))

# Re-apply save_injury_data()'s dedup (R/injuries_schedule.R).
n_before <- nrow(inj)
setorder(inj, scraped_at)
weekly <- unique(inj[source == "weekly"], by = c("player_norm", "team", "injury", "estimated_return", "updated"))
preseason <- unique(inj[source == "preseason"], by = c("player_norm", "team"))
out <- rbind(weekly, preseason)
setorder(out, player_norm, scraped_at)
out[, c("row", "team_canon") := NULL]
cat(sprintf("Rows: %d before dedup, %d after (%d duplicates of earlier correct rows removed)\n",
            n_before, nrow(out), n_before - nrow(out)))
stopifnot(identical(sort(setdiff(names(out), "team_check")), sort(setdiff(names(inj), c("row", "team_canon", "team_check")))))
stopifnot(nrow(out) >= 0.9 * n_before)
cat("\nteam_check after dedup:\n"); print(out[, .N, by = team_check])

if (UPLOAD) {
  save_to_release(as.data.frame(out), "injury_list_2026", "injury-data")
  cat("Uploaded injury_list_2026.parquet to injury-data.\n")
} else {
  cat("Dry run: nothing uploaded. Re-run with --upload to replace the release asset.\n")
}
