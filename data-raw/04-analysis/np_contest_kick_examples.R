# Real contest-kick rows, for designing the rule WITH Pete
# =============================================================================
# The open question (NEXT-STEPS, and the lever today's measurements point at):
# 13,102 of 25,663 lost kicks in 2026 are charged to the KICKER because a
# defender won the ball in the air -- booked identically to an unforced
# turnover. A deliberate long kick to a one-on-one inside 50 currently has no
# way to be distinguished from a kick straight to an opponent in space.
#
# This prints real rows so the rule gets designed on examples rather than in
# the abstract: the kick, where it went, who won it, and exactly what each
# player was charged. Pete's standing rule -- state before, what was expected,
# state after, who gets credit or blame and why.
#
# Values are shown in each player's OWN frame (positive = good for him). The
# raw ledger is in the home team's frame; a payment to an away player is
# flipped here, which is the bug that made the first walkthrough artifact
# unreadable.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_contest_kick_examples.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
stopifnot(is.null(get_local_data_dir()))
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON))
ps  <- as.data.table(load_player_stats(SEASON, refresh = TRUE))
res <- as.data.table(load_results(SEASON))
tm  <- fread("data-raw/outputs/np_difficulty_terms_2025_2026.csv")
tm[, match_id := as.character(match_id)]
tm  <- tm[substr(match_id, 5, 8) == as.character(SEASON)]

say("Building the ledger...")
np <- build_net_points(pbp, ps, res, chains = ch, credit = "difficulty",
                       stoppages = "allocate", difficulty_terms = tm,
                       return_payments = TRUE)
pay <- as.data.table(attr(np, "np_payments"))
pay[, match_id := as.character(match_id)]

ha <- unique(pbp[!is.na(team), .(match_id, team, home_away)])
pay <- merge(pay, ha, by = c("match_id", "team"), all.x = TRUE)
pay[, own := hm * fifelse(home_away == "Home", 1, -1)]   # positive = good for him
nm <- unique(pbp[!is.na(player_id), .(player_id = as.character(player_id), player_name)])
pay[, player_id := as.character(player_id)]
pay <- merge(pay, nm, by = "player_id", all.x = TRUE)
pay[is.na(player_id), player_name := "(team pool)"]

# --- rebuild the ledger's resolution columns so we can find contest kicks ----
led <- torp:::.np_build_ledger(pbp, ch, "allocate")
kicks <- led[description == "Kick" & !is.na(resolve_desc)]
kicks[, lost := !is.na(resolve_team) & resolve_team != team]

CONTEST <- c("Contested Mark", "Uncontested Mark", "Mark On Lead",
             "Pack Mark (P)", "Pack Mark (O)", "Spoil",
             "Spoil gaining possession", "Spoil ineffective")
kicks[, at_contest := resolve_desc %chin% CONTEST]

say("\n=== the population this rule governs, 2026 ===")
say("kicks that resolved somewhere:        ", format(nrow(kicks), big.mark = ","))
say("  ...lost to the opposition:          ", format(kicks[lost == TRUE, .N], big.mark = ","))
say("  ...lost AT A CONTEST (this rule):   ", format(kicks[lost == TRUE & at_contest == TRUE, .N], big.mark = ","))
say("  ...lost in open play (clean t/o):   ", format(kicks[lost == TRUE & at_contest == FALSE, .N], big.mark = ","))
say("\nby how the contest was lost:")
print(kicks[lost == TRUE & at_contest == TRUE, .N, by = resolve_desc][order(-N)])

# --- how much is charged to the kicker in each case --------------------------
actor_pay <- pay[role == "actor", .(match_id, display_order, actor_own = own,
                                    actor = player_name)]
k <- merge(kicks, actor_pay, by = c("match_id", "display_order"), all.x = TRUE)
say("\n=== what the KICKER is charged, by how the kick ended ===")
say("(own frame: negative = charged against him. Mean and median points.)")
summ <- k[lost == TRUE & !is.na(actor_own), .(
  n = .N, mean_charge = round(mean(actor_own), 3), median_charge = round(median(actor_own), 3)
), by = .(ended = fifelse(at_contest, "at a contest the defence won", "open play / clean turnover"))]
print(summ)

# --- pick real examples spanning the spectrum --------------------------------
pbp_loc <- pbp[, .(match_id, display_order, x, y, exp_pts, delta_epv,
                   kicker = player_name, kick_team = team)]
k <- merge(k, pbp_loc, by = c("match_id", "display_order"), all.x = TRUE)
res_loc <- pbp[, .(match_id, display_order = display_order, rx = x)]
k[, dist_fwd := NA_real_]

pick <- function(dt, label, n = 3) {
  if (nrow(dt) == 0) { say("\n(no rows for: ", label, ")"); return(invisible()) }
  dt <- dt[order(-abs(actor_own))][seq_len(min(n, nrow(dt)))]
  for (i in seq_len(nrow(dt))) {
    r <- dt[i]
    say("\n---------------------------------------------------------------")
    say(label)
    say("  match ", r$match_id, "  row ", r$display_order)
    say("  KICK by ", r$kicker, " (", r$kick_team, ") at x=", round(r$x, 1), " y=", round(r$y, 1))
    say("  ended as: ", r$resolve_desc, "  won by ", r$resolve_team,
        if (!is.na(r$resolve_player)) paste0(" (", nm[player_id == r$resolve_player]$player_name, ")") else "")
    say("  state: exp_pts ", round(r$exp_pts, 3), "  ->  delta_epv ", round(r$delta_epv, 3))
    p <- pay[match_id == r$match_id & display_order == r$display_order]
    setorder(p, -own)
    say("  who was paid what (own frame, + = good for him):")
    for (j in seq_len(nrow(p)))
      say("     ", formatC(p$role[j], width = -16), " ",
          formatC(p$player_name[j], width = -22), " ",
          formatC(round(p$own[j], 3), format = "f", digits = 3, width = 8),
          "   [", p$team[j], "]")
  }
}

say("\n\n############ EXAMPLES ############")
say("Read each as: was the kicker's charge fair, given what he was trying to do?")

# A: long kick forward, lost to a CONTESTED MARK by the defence -- the case the
#    current rule arguably gets wrong (a deliberate kick to a 1-on-1).
pick(k[lost == TRUE & resolve_desc == "Contested Mark" & x > 20 & !is.na(actor_own)],
     "A. LONG KICK FORWARD, defence took a CONTESTED MARK", 3)

# B: lost to a SPOIL -- nobody gained possession; the contest just ended.
pick(k[lost == TRUE & resolve_desc %chin% c("Spoil", "Spoil gaining possession") &
       !is.na(actor_own)],
     "B. KICK SPOILED by the defence (no clean possession gained)", 2)

# C: clean turnover in open play -- the case the rule DOES fit.
pick(k[lost == TRUE & at_contest == FALSE & !is.na(actor_own)],
     "C. CLEAN TURNOVER in open play (kicker genuinely at fault)", 2)

# D: contrast -- the ATTACK won the contest. What does the kicker get then?
pick(k[lost == FALSE & resolve_desc == "Contested Mark" & !is.na(actor_own)],
     "D. CONTRAST: same kick, but the ATTACK won the contested mark", 2)

say("\n\n############ THE DESIGN QUESTION ############")
say("Under the current rule A, B and C are all 'the kicker lost it' and are")
say("charged the same way. D is the only one that pays him. The question for")
say("Pete: should A be charged like C, and if not, what should the split be")
say("between the kicker, the defender who won it, and the forward who lost the")
say("contest?")
