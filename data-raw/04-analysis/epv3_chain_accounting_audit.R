# Full audit: every play type, what it is, and which EPV sub-category it feeds.
#
# The metric draws on TWO different sources and they are not interchangeable.
# Getting this wrong is the root of several errors already made on this project,
# so the audit establishes it by measurement rather than by reading the code:
#
#   PBP    carries `delta_epv` and `exp_pts`. The disposer/receiver split runs
#          here -- `epv_disp` is grouped on `player_id`, `epv_recv` on
#          `lead_player_id`.
#   CHAINS carries the raw event stream including in-flight annotations that PBP
#          does not keep. The aerial contest table is built here.
#   BOX    `hitouts`, `hitouts_to_advantage`, `ruck_contests`. The one permitted
#          carve-out, feeding cont_stop.
#
# Outputs, in order:
#   1  chains schema and row-role inventory
#   2  worked row sequences -- what a duel, a possession chain and a stoppage
#      actually look like as consecutive rows
#   3  which chains descriptions the contest path consumes, and in what role
#   4  PBP inventory with the CREDIT MASS each description contributes to each
#      sub-category (the actual accounting, measured not asserted)
#   5  reconciliation: does the mass add up to the built channels
#
# ~5 min.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_chain_accounting.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 60) for (l in capture.output(print(utils::head(x, n)))) say(l)

ch <- as.data.table(load_chains(TRUE))
nm_ch <- uniqueN(ch$match_id)
say("=== CHAINS ===")
say("rows ", format(nrow(ch), big.mark = ","), " over ", nm_ch, " matches (",
    round(nrow(ch) / nm_ch), " per match)")
say("")
say("columns: ", paste(names(ch), collapse = ", "))

# ---------------------------------------------------------------------------
# 1. row-role inventory
# ---------------------------------------------------------------------------
say("")
say("=== 1. EVERY CHAINS DESCRIPTION ===")
say("has_pid / has_tid = % of rows carrying a player / team id.")
say("A row with no player_id cannot be credited to anybody through the chain.")
inv <- ch[, .(n = .N, per_match = round(.N / nm_ch, 1),
              pct_pid = round(100 * mean(!is.na(player_id)), 1),
              pct_tid = round(100 * mean(!is.na(team_id)), 1),
              pct_xy = round(100 * mean(!is.na(x) & !is.na(y)), 1)),
          by = description]
setorder(inv, -n)

# Classify each description by the role it plays in the v3 engine.
inv[, role := fcase(
  description %chin% c("Kick", "Handball", "Ground Kick"), "DISPOSAL (anchors a contest)",
  description %chin% EPV3_AERIAL_OUT, "AERIAL OUTCOME (names the winner)",
  description %chin% CHAINS_CONTEST_TARGET_DESCS, "CONTEST TARGET (names the loser)",
  description %chin% CHAINS_INFLIGHT_DESCS, "IN-FLIGHT (skipped when finding the outcome)",
  description %chin% c("Centre Bounce", "Ball Up Call", "Throw In"), "STOPPAGE (no player_id)",
  description %chin% c("Goal", "Behind", "Shot At Goal"), "SCORE",
  default = "possession / other")]
say_dt(inv, 45)

say("")
say("=== the constant sets, for reference ===")
say("CHAINS_INFLIGHT_DESCS       : ", paste(CHAINS_INFLIGHT_DESCS, collapse = ", "))
say("CHAINS_CONTEST_TARGET_DESCS : ", paste(CHAINS_CONTEST_TARGET_DESCS, collapse = ", "))
say("EPV3_AERIAL_OUT             : ", paste(EPV3_AERIAL_OUT, collapse = ", "))
say("EPV3_AERIAL_EXPOSURE_DESCS  : ", paste(EPV3_AERIAL_EXPOSURE_DESCS, collapse = ", "))

# ---------------------------------------------------------------------------
# 2. worked sequences
# ---------------------------------------------------------------------------
say("")
say("=== 2. WHAT THE ROWS ACTUALLY LOOK LIKE ===")
setorder(ch, match_id, display_order)
ch[, rn := .I]
show_seq <- function(idx, label, before = 2, after = 4) {
  say(""); say("--- ", label, " ---")
  rows <- ch[seq(max(1, idx - before), min(nrow(ch), idx + after))]
  say_dt(rows[, .(display_order, description, player_id = substr(player_id, 1, 12),
                  team_id = substr(as.character(team_id), 1, 8),
                  x = round(x), y = round(y))], 10)
}
mid <- ch[, .N, by = match_id][order(-N)][1, match_id]
one <- ch[match_id == mid]
pick_in <- function(d) { i <- one[description == d, rn]; if (length(i)) i[max(1, length(i) %/% 2)] else NA }
for (d in c("Contested Mark", "Spoil", "Uncontested Mark", "Centre Bounce", "Handball Received")) {
  i <- pick_in(d)
  if (!is.na(i)) show_seq(i, paste0("a ", d, " in context"))
}

say("")
say("IS A DUEL TWO PLAYERS ONE AFTER ANOTHER? Measured, not assumed:")
kicks <- ch[description %chin% c("Kick", "Ground Kick")]
say("For each aerial outcome, how many DISTINCT players appear between the kick")
say("and the outcome row (the in-flight span):")
ch[, `:=`(nx1 = shift(description, 1, type = "lead"),
          nx2 = shift(description, 2, type = "lead"),
          np1 = shift(player_id, 1, type = "lead"),
          np2 = shift(player_id, 2, type = "lead"),
          nt1 = shift(team_id, 1, type = "lead"),
          nt2 = shift(team_id, 2, type = "lead")), by = match_id]
k <- ch[description %chin% c("Kick", "Ground Kick") & !is.na(player_id)]
k[, span_two_teams := !is.na(nt1) & !is.na(nt2) & nt1 != nt2]
say("  kicks whose next TWO rows are different teams (a genuine two-sided",
    " contest): ", round(100 * mean(k$span_two_teams, na.rm = TRUE), 1), "%")
say("  kicks followed immediately by an aerial outcome: ",
    round(100 * mean(k$nx1 %chin% EPV3_AERIAL_OUT, na.rm = TRUE), 1), "%")

# ---------------------------------------------------------------------------
# 3. what the contest path consumes
# ---------------------------------------------------------------------------
say("")
say("=== 3. HOW THE CONTEST PATH USES EACH DESCRIPTION ===")
use <- data.table(
  description = unique(c("Kick", "Ground Kick", EPV3_AERIAL_OUT,
                         CHAINS_CONTEST_TARGET_DESCS, CHAINS_INFLIGHT_DESCS)))
use[, anchors_contest := description %chin% c("Kick", "Ground Kick")]
use[, names_winner := description %chin% EPV3_AERIAL_OUT]
use[, names_loser := description %chin% CHAINS_CONTEST_TARGET_DESCS]
use[, skipped_inflight := description %chin% CHAINS_INFLIGHT_DESCS]
use[, counts_as_exposure := description %chin% EPV3_AERIAL_EXPOSURE_DESCS]
use <- merge(use, inv[, .(description, per_match)], by = "description", all.x = TRUE)
setorder(use, -per_match)
say_dt(use, 40)
say("")
say("NOTE `Kick Inside 50 Result` is in EPV3_AERIAL_EXPOSURE_DESCS. It is an")
say("ANNOTATION of where a kick ended up, not a contest anybody entered --")
say("flagged for review alongside the Uncontested Mark finding.")

# ---------------------------------------------------------------------------
# 4. PBP: the actual credit accounting, by description
# ---------------------------------------------------------------------------
say("")
say("=== 4. PBP: WHICH SUB-CATEGORY EACH PLAY TYPE FEEDS, AND HOW MUCH ===")
full <- as.data.table(load_pbp(TRUE))
p <- data.table(match_id = full$match_id, display_order = full$display_order,
                description = full$description, delta_epv = full$delta_epv,
                pos_team = full$pos_team, player_id = full$player_id,
                lead_player_id = full$lead_player_id,
                lead_desc_tot = full$lead_desc_tot,
                contest_target_id = if ("contest_target_id" %in% names(full))
                  full$contest_target_id else NA_character_)
rm(full); invisible(gc())
p <- p[is.finite(delta_epv)]
nm_p <- uniqueN(p$match_id)

DS <- EPV_DISP_SCALE; RS <- EPV_RECV_SCALE
p[, disp_credit := fifelse(!is.na(player_id) &
     description %chin% c("Kick", "Handball", "Ground Kick"), delta_epv * DS, NA_real_)]
p[, recv_credit := fifelse(!is.na(lead_player_id), delta_epv * pos_team * RS, NA_real_)]

say("")
say("--- DISPOSAL channel: which rows generate it ---")
d1 <- p[!is.na(disp_credit), .(n = .N, per_match = round(.N / nm_p, 1),
        mean = round(mean(disp_credit), 4),
        gross = round(sum(abs(disp_credit)))), by = description]
d1[, pct_of_channel := round(100 * gross / sum(gross), 1)]
setorder(d1, -gross); say_dt(d1, 10)

say("")
say("--- RECEPTION channel: which rows generate it ---")
say("Bucketed on lead_desc_tot -- what the RECEIVING act was.")
r1 <- p[!is.na(recv_credit), .(n = .N, per_match = round(.N / nm_p, 1),
        mean = round(mean(recv_credit), 4),
        gross = round(sum(abs(recv_credit)))), by = lead_desc_tot]
r1[, pct_of_channel := round(100 * gross / sum(gross), 1)]
setorder(r1, -gross); say_dt(r1, 25)

say("")
say("--- rows that generate NEITHER (no player_id and no lead_player_id) ---")
n1 <- p[is.na(disp_credit) & is.na(recv_credit),
        .(n = .N, per_match = round(.N / nm_p, 1),
          gross_unallocated = round(sum(abs(delta_epv)))), by = description]
setorder(n1, -gross_unallocated); say_dt(n1, 15)
say("")
say("This is the value the chain path credits to NOBODY. Centre Bounce is the")
say("big one and it is an artifact (exp_pts is exactly 0 there); the rest is")
say("the genuine unallocated remainder that the ruck box terms stand in for.")

# ---------------------------------------------------------------------------
# 5. reconciliation
# ---------------------------------------------------------------------------
say("")
say("=== 5. RECONCILIATION ===")
say("Total credit generated per team-match, by channel, against the built")
say("player-game frame. These should agree; if they do not, this audit is")
say("describing a different accounting from the one that runs.")
say(sprintf("  disposal gross  %10.0f   (%.2f per match)",
            sum(abs(p$disp_credit), na.rm = TRUE),
            sum(abs(p$disp_credit), na.rm = TRUE) / nm_p))
say(sprintf("  reception gross %10.0f   (%.2f per match)",
            sum(abs(p$recv_credit), na.rm = TRUE),
            sum(abs(p$recv_credit), na.rm = TRUE) / nm_p))
f <- file.path(OUT_DIR, "epv3_fin_pgd_ship.parquet")
if (file.exists(f)) {
  g <- as.data.table(read_parquet(f))
  say("")
  say("built v3 player-game frame (", basename(f), "):")
  say(sprintf("  epv_recv  gross %10.0f   sd %.4f", sum(abs(g$epv_recv), na.rm = TRUE), sd(g$epv_recv, na.rm = TRUE)))
  say(sprintf("  epv_disp  gross %10.0f   sd %.4f", sum(abs(g$epv_disp), na.rm = TRUE), sd(g$epv_disp, na.rm = TRUE)))
  say(sprintf("  epv_spoil gross %10.0f   sd %.4f", sum(abs(g$epv_spoil), na.rm = TRUE), sd(g$epv_spoil, na.rm = TRUE)))
  say("")
  say("The disposal figure will NOT match exactly: v3 zeroes the ordinary")
  say("disp_scale on aerial-kick rows and pays V_pre - exp_pts from the contest")
  say("table instead. Reception likewise drops aerial-kick and contest-target")
  say("rows. The gap between the two is the size of the v3 rewrite.")
}

close(con)
cat("\nDone\n")
