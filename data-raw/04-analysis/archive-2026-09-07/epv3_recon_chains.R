# EPV v3 reconnaissance: what can chains actually price?
#
# Answers the questions the v3 design depends on:
#   1. What event descriptions exist in chains, and how often?
#   2. Which chain events carry a player_id (i.e. are attributable)?
#   3. How much of each box-score stat (tackles, ground ball gets, contested
#      marks, spoils, hitouts) is visible as a chain row?
#   4. What does a ruck contest look like in chains — are the two rucks named?
#   5. What is the two-branch structure of an aerial contest?
#
# Output: data-raw/outputs/epv3_recon.txt (also printed).

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
})

DATA_DIR <- "C:/dev/torpverse/torpdata/data"
SEASONS  <- c(2024, 2025)
OUT      <- "C:/dev/torpverse/torp/data-raw/outputs/epv3_recon.txt"

con <- file(OUT, open = "wt")
say <- function(...) {
  msg <- paste0(...)
  cat(msg, "\n", sep = "")
  cat(msg, "\n", sep = "", file = con)
}
say_dt <- function(x, n = 200) {
  out <- capture.output(print(utils::head(x, n)))
  for (l in out) say(l)
}

say("=== EPV v3 chains reconnaissance ===")
say("seasons: ", paste(SEASONS, collapse = ", "))
say("")

# ---- Load ------------------------------------------------------------------
ch <- rbindlist(lapply(SEASONS, function(s) {
  f <- file.path(DATA_DIR, sprintf("chains_data_%d_all.parquet", s))
  as.data.table(arrow::read_parquet(f))
}), use.names = TRUE, fill = TRUE)

say("chains rows: ", format(nrow(ch), big.mark = ","),
    "  cols: ", ncol(ch))
say("chains columns: ", paste(names(ch), collapse = ", "))
say("")

pbp <- rbindlist(lapply(SEASONS, function(s) {
  f <- file.path(DATA_DIR, sprintf("pbp_data_%d_all.parquet", s))
  d <- as.data.table(arrow::read_parquet(f))
  d
}), use.names = TRUE, fill = TRUE)
say("pbp rows: ", format(nrow(pbp), big.mark = ","), "  cols: ", ncol(pbp))
say("pbp has delta_epv: ", "delta_epv" %in% names(pbp),
    "   exp_pts: ", "exp_pts" %in% names(pbp))
say("")

n_matches <- uniqueN(ch$match_id)
say("matches: ", n_matches)
say("")

# ---- 1. Description inventory ---------------------------------------------
say("--- 1. chains description inventory (per match rate, and player_id share) ---")
desc <- ch[, .(
  n          = .N,
  per_match  = round(.N / n_matches, 2),
  pct_has_pid = round(100 * mean(!is.na(player_id)), 1),
  pct_has_tid = round(100 * mean(!is.na(team_id)), 1)
), by = description][order(-n)]
say_dt(desc, 100)
say("")

# ---- 2. Box-score coverage -------------------------------------------------
# How many of each stat exist in chains as an attributable row?
say("--- 2. chain rows per match for the events v2 pays box weights for ---")
tgt <- c("Tackle", "Spoil", "Contested Mark", "Uncontested Mark", "Mark On Lead",
         "Hard Ball Get", "Loose Ball Get", "Hard Ball Get Crumb",
         "Loose Ball Get Crumb", "Ruck Hard Ball Get",
         "Handball", "Kick", "Ground Kick", "Free For", "Free Against",
         "Centre Bounce", "Ball Up Call", "Out Of Bounds", "Throw In",
         "Shot At Goal", "Goal", "Behind", "Ruck Contest", "Hitout",
         "Knock On", "Gather", "Gather From Hitout", "Gather from Hitout")
cov <- desc[description %chin% tgt]
say_dt(cov, 60)
say("")
say("descriptions matching 'ruck|hitout|tap|knock|bounce|ball up' (case-insensitive):")
say_dt(desc[grepl("ruck|hitout|hit out|tap|knock|bounce|ball up", description,
                  ignore.case = TRUE)], 40)
say("")

# ---- 3. Aerial contest two-branch structure -------------------------------
setorder(ch, match_id, display_order)
ch[, `:=`(
  .next_desc = shift(description, 1L, type = "lead"),
  .next_tid  = shift(team_id, 1L, type = "lead"),
  .next_pid  = shift(player_id, 1L, type = "lead"),
  .next_x    = shift(x, 1L, type = "lead"),
  .next_y    = shift(y, 1L, type = "lead")
), by = match_id]

target_descs <- c("Contest Target", "Kick Inside 50 Result")
aer <- ch[description %chin% target_descs & !is.na(player_id) &
          !is.na(.next_tid) & x == .next_x & y == .next_y]
say("--- 3. aerial contest (target row + same-xy outcome row) ---")
say("contest-target rows with a same-xy outcome: ", format(nrow(aer), big.mark = ","),
    "  (", round(nrow(aer) / n_matches, 1), " per match)")
br <- aer[, .(n = .N), by = .(.next_desc, same_team = team_id == .next_tid)][order(-n)]
say_dt(br, 40)
say("")

# ---- 4. Who is the loser of a WON mark? -----------------------------------
# When the target marks it themselves, is any opponent named nearby?
say("--- 4. when the attacking team WINS the mark, is a defender identifiable? ---")
won <- aer[team_id == .next_tid & .next_desc %chin%
           c("Contested Mark", "Uncontested Mark", "Mark On Lead")]
say("attacking-team mark wins: ", format(nrow(won), big.mark = ","))
say("  of which the mark is CONTESTED: ",
    format(nrow(won[.next_desc == "Contested Mark"]), big.mark = ","))
say("  -> a Contested Mark implies an opponent was there but chains does not name him.")
# Check 2 rows ahead for an opposing-team row at same xy
ch[, `:=`(
  .n2_desc = shift(description, 2L, type = "lead"),
  .n2_tid  = shift(team_id, 2L, type = "lead"),
  .n2_pid  = shift(player_id, 2L, type = "lead"),
  .n2_x    = shift(x, 2L, type = "lead"),
  .n2_y    = shift(y, 2L, type = "lead")
), by = match_id]
won2 <- ch[description %chin% target_descs & !is.na(player_id) &
           !is.na(.next_tid) & x == .next_x & y == .next_y &
           team_id == .next_tid &
           .next_desc %chin% c("Contested Mark", "Uncontested Mark", "Mark On Lead")]
say("  rows 2-ahead at the same xy from the OPPOSING team: ",
    format(nrow(won2[!is.na(.n2_tid) & team_id != .n2_tid & x == .n2_x & y == .n2_y]),
           big.mark = ","))
say("  their descriptions:")
say_dt(won2[!is.na(.n2_tid) & team_id != .n2_tid & x == .n2_x & y == .n2_y,
            .N, by = .n2_desc][order(-N)], 20)
say("")

# ---- 5. Ruck / stoppage structure -----------------------------------------
say("--- 5. stoppage structure: what follows a Centre Bounce / Ball Up / Throw In? ---")
for (d in c("Centre Bounce", "Ball Up Call", "Throw In", "Out Of Bounds")) {
  sub <- ch[description == d]
  if (nrow(sub) == 0) { say("  ", d, ": ABSENT from chains"); next }
  say("  ", d, ": ", format(nrow(sub), big.mark = ","), " rows, ",
      round(100 * mean(!is.na(sub$player_id)), 1), "% carry a player_id")
  say_dt(sub[, .N, by = .next_desc][order(-N)][1:8], 10)
}
say("")

# ---- 6. Tackle structure ---------------------------------------------------
say("--- 6. tackle: is the tackler named, and what follows? ---")
tk <- ch[description == "Tackle"]
if (nrow(tk) > 0) {
  say("Tackle rows: ", format(nrow(tk), big.mark = ","), " (",
      round(nrow(tk) / n_matches, 1), "/match), ",
      round(100 * mean(!is.na(tk$player_id)), 1), "% with player_id")
  say("  next-row descriptions:")
  say_dt(tk[, .N, by = .next_desc][order(-N)][1:12], 15)
  say("  next row is OPPOSING team: ",
      round(100 * mean(tk$team_id != tk$.next_tid, na.rm = TRUE), 1), "%")
} else {
  say("Tackle: ABSENT from chains -- box-score only.")
}
say("")

# ---- 7. Ground ball contests ----------------------------------------------
say("--- 7. ground ball contests (adjacent opposing-team GB rows at same xy) ---")
gb_descs <- c("Hard Ball Get", "Loose Ball Get", "Hard Ball Get Crumb",
              "Loose Ball Get Crumb", "Ruck Hard Ball Get")
gb <- ch[description %chin% gb_descs & .next_desc %chin% gb_descs &
         x == .next_x & y == .next_y & team_id != .next_tid & !is.na(player_id)]
say("contested ground-ball pairs: ", format(nrow(gb), big.mark = ","),
    " (", round(nrow(gb) / n_matches, 1), " per match)")
say("all ground-ball rows: ",
    format(nrow(ch[description %chin% gb_descs]), big.mark = ","),
    " (", round(nrow(ch[description %chin% gb_descs]) / n_matches, 1), " per match)")
say("")

# ---- 8. delta_epv joinability ---------------------------------------------
say("--- 8. can every chain row be joined to a pbp delta_epv? ---")
key <- unique(pbp[, .(match_id, display_order)])
key[, .in_pbp := TRUE]
ch2 <- merge(ch[, .(match_id, display_order, description)], key,
             by = c("match_id", "display_order"), all.x = TRUE)
say("chain rows with a matching pbp row: ",
    round(100 * mean(!is.na(ch2$.in_pbp)), 1), "%")
say("  by description (top 25 by chain count, share present in pbp):")
say_dt(ch2[, .(n = .N, pct_in_pbp = round(100 * mean(!is.na(.in_pbp)), 1)),
           by = description][order(-n)][1:25], 30)

close(con)
cat("\nWrote ", OUT, "\n")
