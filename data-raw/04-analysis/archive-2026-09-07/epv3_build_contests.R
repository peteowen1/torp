# EPV v3 step 2: build the aerial contest table and fit the three branch models.
#
# ANCHOR ON THE KICK, not on the contest annotation. The first draft anchored on
# `Contest Target` and `Spoil` rows; that was wrong twice over:
#   * the spoil-anchored population is defence-wins-only BY CONSTRUCTION, so a
#     p(defence wins) fitted on the union read 75.1% against the target-anchored
#     set's 57.2% -- pure selection.
#   * it left marks to be paid by BOTH epv_recv and the contest channel.
# Anchoring on the kick makes the outcome always observed (so p is unbiased) and
# makes a marker's reception credit and his contest credit the same quantity,
# which is the double count removed.
#
# Decomposition, kicking-team frame:
#   delta_epv = (V_pre - exp_pts)  +  (V_after - V_pre)
#                 kicker/disposal      contest, split winner/loser
# with V_pre = (1-p) V_att + p V_def and Delta = V_att - V_def.
#
# Coordinates: RAW chains x,y sit in a shared ground frame (opposing rows of one
# contest carry identical x,y -- which is why player_credit.R compares them
# without a sign flip). Cleaned pbp is the opposite: x is in the ACTION team's
# frame and opponents are negated (clean_pbp.R:380). So attacking direction is
# derived empirically per (match, team) from that team's own scoring rows rather
# than assumed either way.

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(mgcv)
})

DATA_DIR <- "C:/dev/torpverse/torpdata/data"
SEASONS  <- 2021:2026
OUT_DIR  <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT      <- file.path(OUT_DIR, "epv3_contests.txt")
TBL      <- file.path(OUT_DIR, "epv3_contest_table.parquet")
MODELS   <- file.path(OUT_DIR, "epv3_contest_models.rds")

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 60) for (l in capture.output(print(utils::head(x, n)))) say(l)

KICK_DESCS <- c("Kick", "Ground Kick")
INFLIGHT   <- c("Contest Target", "Kick Inside 50 Result", "Kick Into F50",
                "Shot At Goal", "Inside 50", "Rebound 50",
                "Kick In Ineffective", "Kickin long", "Kickin short",
                "Kickin play on", "Clearance (Operator)", "Clearance",
                "Clearance (Operator)", "Bounce")
MARK_DESCS <- c("Contested Mark", "Uncontested Mark", "Mark On Lead",
                "Pack Mark (P)", "Pack Mark (O)")
# An "aerial outcome" is a kick resolved in the air by a player, won or lost.
# Mark Fumbled / Dropped Mark are deliberately NOT here: nobody secured the ball,
# so there is no winner to credit. Folding them into the attacking branch would
# drag V_att down for every genuine mark. Those kicks fall through to the
# ordinary non-aerial 50/50 disposer/receiver treatment instead.
AERIAL_OUT <- c(MARK_DESCS, "Spoil",
                "Spoil gaining possession", "Spoil ineffective")

say("=== EPV v3: contest table + branch models (kick-anchored) ===")
say("seasons: ", paste(range(SEASONS), collapse = "-"))

# ---- Load ------------------------------------------------------------------
rd <- function(pat) rbindlist(lapply(SEASONS, function(s) {
  f <- file.path(DATA_DIR, sprintf(pat, s))
  if (!file.exists(f)) return(NULL)
  as.data.table(arrow::read_parquet(f))
}), use.names = TRUE, fill = TRUE)

ch  <- rd("chains_data_%d_all.parquet")
pbp <- rd("pbp_data_%d_all.parquet")
say("chains rows ", format(nrow(ch), big.mark = ","),
    " | pbp rows ", format(nrow(pbp), big.mark = ","),
    " | matches ", uniqueN(ch$match_id))

pbp_k <- pbp[, .(match_id, display_order, exp_pts, delta_epv)]

# ---- Orientation: verified, not assumed ------------------------------------
# Raw chains stores every row of a chain in the CHAIN team's attacking frame --
# including the opposition's rows, which is why player_credit.R compares contest
# coordinates with `x == .next_x` and no sign flip, while cleaned pbp (action-team
# frame) needs `x == -.next_x`. Since a kick's chain belongs to the kicking team,
# +x on these rows already points at the kicking team's goal and no reorientation
# is needed. Asserted rather than trusted: BOTH teams' scoring rows must read
# positive mean x, which is impossible in a fixed venue frame.
score_rows <- ch[description %chin% c("Goal", "Behind", "Shot At Goal") & !is.na(team_id)]
dir_tbl <- score_rows[, .(mean_x = mean(x, na.rm = TRUE), n = .N), by = .(match_id, team_id)]
pos_share <- mean(dir_tbl$mean_x > 0)
say("")
say("--- orientation assertion ---")
say("(match, team) cells with scoring rows: ", nrow(dir_tbl),
    " | share with mean_x > 0: ", round(100 * pos_share, 2), "%",
    "   [expect ~100% under a chain-team frame; ~50% under a venue frame]")
if (pos_share < 0.98) {
  stop("chains coordinates are NOT in the chain team's attacking frame -- ",
       "only ", round(100 * pos_share, 1), "% of scoring cells read positive. ",
       "Reorientation logic is required before any of the features below mean anything.")
}
say("median |mean_x| of scoring rows: ", round(median(abs(dir_tbl$mean_x)), 1))
HALF <- as.numeric(quantile(abs(ch$x), .995, na.rm = TRUE))
say("HALF (|x| p99.5) = ", round(HALF, 1))

# ---- Kick-anchored forward scan to the outcome row -------------------------
setorder(ch, match_id, display_order)
for (k in 1:6) {
  ch[, (paste0(".f", k, "_desc")) := shift(description,   k, type = "lead"), by = match_id]
  ch[, (paste0(".f", k, "_pid"))  := shift(player_id,     k, type = "lead"), by = match_id]
  ch[, (paste0(".f", k, "_tid"))  := shift(team_id,       k, type = "lead"), by = match_id]
  ch[, (paste0(".f", k, "_x"))    := shift(x,             k, type = "lead"), by = match_id]
  ch[, (paste0(".f", k, "_y"))    := shift(y,             k, type = "lead"), by = match_id]
}

kk <- ch[description %chin% KICK_DESCS & !is.na(player_id) & !is.na(team_id)]
say("")
say("--- kick-anchored population ---")
say("kick rows ", format(nrow(kk), big.mark = ","))

# First forward row that is NOT an in-flight annotation.
kk[, .olag := fcase(
  !(.f1_desc %chin% INFLIGHT), 1L,
  !(.f2_desc %chin% INFLIGHT), 2L,
  !(.f3_desc %chin% INFLIGHT), 3L,
  !(.f4_desc %chin% INFLIGHT), 4L,
  !(.f5_desc %chin% INFLIGHT), 5L,
  !(.f6_desc %chin% INFLIGHT), 6L,
  default = NA_integer_
)]
pick <- function(stem) fcase(
  kk$.olag == 1L, kk[[paste0(".f1_", stem)]], kk$.olag == 2L, kk[[paste0(".f2_", stem)]],
  kk$.olag == 3L, kk[[paste0(".f3_", stem)]], kk$.olag == 4L, kk[[paste0(".f4_", stem)]],
  kk$.olag == 5L, kk[[paste0(".f5_", stem)]], kk$.olag == 6L, kk[[paste0(".f6_", stem)]]
)
kk[, `:=`(out_desc = pick("desc"), out_pid = pick("pid"), out_tid = pick("tid"),
          out_x = pick("x"), out_y = pick("y"))]

# Was a Contest Target logged in the in-flight span? That is the only way the
# INTENDED receiver is named, and it is what lets us debit a beaten target.
tgt_in_span <- function() {
  res <- rep(NA_character_, nrow(kk))
  tt  <- rep(NA_character_, nrow(kk))
  for (k in 1:5) {
    d <- kk[[paste0(".f", k, "_desc")]]
    hit <- !is.na(kk$.olag) & k < kk$.olag & d %chin% c("Contest Target", "Kick Inside 50 Result") &
           is.na(res)
    res[hit] <- kk[[paste0(".f", k, "_pid")]][hit]
    tt[hit]  <- kk[[paste0(".f", k, "_tid")]][hit]
  }
  list(pid = res, tid = tt)
}
tg <- tgt_in_span()
kk[, `:=`(target_pid = tg$pid, target_tid = tg$tid)]

kk[, is_aerial := out_desc %chin% AERIAL_OUT & !is.na(out_tid) & !is.na(out_pid)]
say("kicks with a resolvable outcome row: ", round(100 * mean(!is.na(kk$.olag)), 1), "%")
say("of those, AERIAL: ", format(sum(kk$is_aerial, na.rm = TRUE), big.mark = ","),
    " (", round(100 * mean(kk$is_aerial, na.rm = TRUE), 1), "% of kicks, ",
    round(sum(kk$is_aerial, na.rm = TRUE) / uniqueN(kk$match_id), 1), "/match)")
say("outcome descriptions (top 25):")
say_dt(kk[, .N, by = .(out_desc, aerial = out_desc %chin% AERIAL_OUT)][order(-N)], 25)

cst <- kk[is_aerial == TRUE, .(
  match_id, kick_do = display_order, kick_pid = player_id, kick_tid = team_id,
  kick_x = x, kick_y = y,
  out_desc, out_pid, out_tid, out_x, out_y,
  target_pid, target_tid
)]
# The chain's own attribution decides the winner: the outcome row's team.
cst[, def_win := out_tid != kick_tid]

# A "Spoil" logged to the KICKING team is a chain-logging artifact, not an
# attacking win -- v2 drops the same rows for the same reason (~16% of spoils).
n_same_spoil <- cst[grepl("^Spoil", out_desc) & def_win == FALSE, .N]
cst <- cst[!(grepl("^Spoil", out_desc) & def_win == FALSE)]
say("")
say("dropped same-team Spoil rows (chain-logging artifact): ",
    format(n_same_spoil, big.mark = ","))
say("aerial contests ", format(nrow(cst), big.mark = ","),
    " | defence wins ", round(100 * mean(cst$def_win), 1), "%")
say_dt(cst[, .N, by = .(out_desc, def_win)][order(-N)], 20)

# ---- Attach the kick's pbp state -------------------------------------------
cst <- merge(cst, pbp_k, by.x = c("match_id", "kick_do"),
             by.y = c("match_id", "display_order"), all.x = TRUE, sort = FALSE)
say("")
say("contests joined to a pbp kick row: ", round(100 * mean(!is.na(cst$exp_pts)), 1), "%")
cst <- cst[is.finite(exp_pts) & is.finite(delta_epv)]
cst[, V_after := exp_pts + delta_epv]

# ---- Features (already in the kicking team's frame -- see assertion above) --
cst[, `:=`(
  att_x     = out_x,                     # + toward the kicking team's goal
  abs_y     = abs(out_y),
  k_att_x   = kick_x,
  k_abs_y   = abs(kick_y),
  kick_len  = sqrt((out_x - kick_x)^2 + (out_y - kick_y)^2)
)]
cst[, `:=`(
  goal_dist = sqrt(pmax(0, HALF - att_x)^2 + abs_y^2),
  fwd_gain  = att_x - k_att_x,
  i50f      = factor(as.integer(att_x > HALF - 50))
)]
cst <- cst[is.finite(goal_dist) & is.finite(kick_len) & is.finite(fwd_gain)]

say("")
say("--- raw two-branch separation (no model) ---")
say("mean V_after | defence won ", round(mean(cst[def_win == TRUE]$V_after), 4),
    " | attack retained ", round(mean(cst[def_win == FALSE]$V_after), 4))
say("raw gap ", round(mean(cst[def_win == FALSE]$V_after) -
                      mean(cst[def_win == TRUE]$V_after), 4), " points")

# ---- Fit -------------------------------------------------------------------
set.seed(42)
cst[, .fold := sample(c("train", "test"), .N, replace = TRUE, prob = c(0.8, 0.2))]
tr <- cst[.fold == "train"]; te <- cst[.fold == "test"]
say("")
say("train ", format(nrow(tr), big.mark = ","), " | test ", format(nrow(te), big.mark = ","))

# `exp_pts` (the pre-kick state value, kicking-team frame) is the single most
# informative feature for both branches and is legitimately available: it is what
# the situation was worth BEFORE the kick, not anything about how the contest
# resolved. Without it the first fit missed badly in the tails (bin-20 diff
# -0.294) while reading fine through the middle.
# Every term must be knowable BEFORE the contest resolves. A `lead_mark`
# indicator was carried here at first and is leakage: "the mark was taken on a
# lead" is only observable because the attack won it.
RHS <- ~ s(att_x, abs_y) + s(kick_len) + s(fwd_gain) + s(goal_dist) +
         s(exp_pts) + i50f
m_p   <- bam(update(RHS, def_win ~ .), data = tr, family = binomial(), discrete = TRUE, nthreads = 4)
m_att <- bam(update(RHS, V_after ~ .), data = tr[def_win == FALSE], discrete = TRUE, nthreads = 4)
m_def <- bam(update(RHS, V_after ~ .), data = tr[def_win == TRUE],  discrete = TRUE, nthreads = 4)

score <- function(d) {
  d <- copy(d)
  d[, `:=`(
    p_hat     = as.numeric(predict(m_p,   newdata = d, type = "response")),
    V_att_hat = as.numeric(predict(m_att, newdata = d)),
    V_def_hat = as.numeric(predict(m_def, newdata = d))
  )]
  d[, `:=`(V_pre = (1 - p_hat) * V_att_hat + p_hat * V_def_hat,
           Delta = V_att_hat - V_def_hat)]
  d
}
te <- score(te)

say("")
say("--- GATE 2: branch-model self-consistency (held out) ---")
say("mean V_pre ", round(mean(te$V_pre), 4), " vs mean V_after ",
    round(mean(te$V_after), 4), " | diff ", round(mean(te$V_pre - te$V_after), 4))
te[, .bin := cut(V_pre, breaks = quantile(V_pre, 0:20 / 20), include.lowest = TRUE)]
bc <- te[, .(n = .N, V_pre = mean(V_pre), V_after = mean(V_after)), by = .bin][order(V_pre)]
say_dt(bc[, .(n, V_pre = round(V_pre, 3), V_after = round(V_after, 3),
              diff = round(V_after - V_pre, 3))], 20)
say("cor(binned) ", round(cor(bc$V_pre, bc$V_after), 5),
    " | mean|diff| ", round(mean(abs(bc$V_after - bc$V_pre)), 4),
    "   [target: cor > 0.98, mean|diff| < 0.05]")
say("p calibration: predicted ", round(mean(te$p_hat), 4),
    " vs observed ", round(mean(te$def_win), 4))

say("")
say("--- Delta: what an aerial contest is actually worth ---")
say_dt(te[, .(n = .N, mean_Delta = round(mean(Delta), 3),
              p10 = round(quantile(Delta, .1), 3), p50 = round(quantile(Delta, .5), 3),
              p90 = round(quantile(Delta, .9), 3)), by = i50f][order(i50f)], 5)
say("Delta overall: mean ", round(mean(te$Delta), 3), " sd ", round(sd(te$Delta), 3))

say("")
say("--- contest credit magnitudes, vs v2's flat EPV_SPOIL_WT = 0.0737 ---")
te[, cred_win := fifelse(def_win, (1 - p_hat) * Delta, p_hat * Delta)]
say_dt(te[, .(n = .N, mean = round(mean(cred_win), 4), sd = round(sd(cred_win), 4),
              p50 = round(quantile(cred_win, .5), 4),
              p90 = round(quantile(cred_win, .9), 4)), by = def_win], 5)

say("")
say("--- the unnamed-loser problem, sized ---")
say("defence wins, attacking target NAMED: ",
    round(100 * mean(!is.na(te[def_win == TRUE]$target_pid)), 1), "%")
say("attack retains, defender named: 0% by construction (recon confirmed)")
tot <- te[, sum(abs(cred_win))]
unal <- te[def_win == FALSE, sum(abs(cred_win))] +
        te[def_win == TRUE & is.na(target_pid), sum(abs(cred_win))]
say("share of contest credit mass whose LOSER is unnamed: ",
    round(100 * unal / tot, 1), "%")

# ---- Credits ---------------------------------------------------------------
full <- score(cst)
full[, `:=`(
  disp_credit   = V_pre - exp_pts,                                # to the kicker
  # Surprise, in the kicking team's frame.
  cont_att      = fifelse(def_win, -(1 - p_hat) * Delta, p_hat * Delta),
  V_branch      = fifelse(def_win, V_def_hat, V_att_hat)
)]
full[, cont_def := -cont_att]                                     # zero-sum
full[, play_resid := V_after - V_branch]
# The winner always gains |cont_att| and the loser always sheds it, whichever
# side won -- because cont_att is signed in the ATTACKING frame and flips with
# the outcome. Writing it this way instead of branching on def_win is not just
# tidier: the branching version had the debit sign inverted for defence wins.
full[, `:=`(winner_credit = abs(cont_att), loser_credit = -abs(cont_att))]
stopifnot(abs(full[, sum(winner_credit + loser_credit)]) < 1e-6)

say("")
say("--- GATE 1: the three-way decomposition of the kick's delta_epv ---")
say("  delta_epv = (V_pre - exp_pts)   disposal, to the kicker")
say("            + (V_branch - V_pre)  contest surprise, split winner/loser")
say("            + (V_after - V_branch) subsequent play, paid by the NEXT chain row")
full[, .resid := (disp_credit + cont_att + play_resid) - delta_epv]
say("identity residual: max ", signif(max(abs(full$.resid)), 3),
    " | mean ", signif(mean(abs(full$.resid)), 3), "   [must be ~0 -- pure algebra]")
say("subsequent-play term: mean ", round(mean(full$play_resid), 5),
    " sd ", round(sd(full$play_resid), 4),
    "   [must be ~0 mean, or the branch models are biased]")
say("NOTE: the contest credit uses the MODELLED branch value, not the realised")
say("V_after. That is deliberate. Realised would conserve exactly but would pay a")
say("spoiler for whatever his team-mate did next; modelled prices the contest.")

# ---- Loser allocation ------------------------------------------------------
# Chains names the loser in a minority of contests. The debit still has to land
# somewhere or the channel becomes upside-only. Exposure weight is each player's
# count of ALL chain rows for his team in that (match, zone) -- deliberately NOT
# his contest wins, which would make the best contester absorb the most debit.
say("")
say("--- loser allocation ---")
full[, winner_pid := fifelse(def_win, out_pid, out_pid)]  # outcome row is the winner
full[, winner_tid := out_tid]
# The loser is the beaten aerial opponent: the intended target when the defence
# won (named only if a Contest Target row was logged), and the defender when the
# attack retained (never named -- recon found 0 of 7,736).
full[, loser_pid := fifelse(def_win, target_pid, NA_character_)]
full[, loser_tid := fifelse(def_win, kick_tid, out_tid)]
say("contests with a NAMED loser: ", round(100 * mean(!is.na(full$loser_pid)), 1), "%")
say("share of |contest credit| mass with an unnamed loser: ",
    round(100 * full[is.na(loser_pid), sum(abs(cont_att))] / full[, sum(abs(cont_att))], 1), "%")

zone_of <- function(ax) fcase(ax >  HALF / 3, "att", ax < -HALF / 3, "def", default = "mid")
full[, zone := zone_of(att_x)]
ch[, zone := zone_of(x)]
exposure <- ch[!is.na(player_id) & !is.na(team_id),
               .(expo = .N), by = .(match_id, team_id, player_id, zone)]
say("exposure table: ", format(nrow(exposure), big.mark = ","),
    " (match, team, player, zone) cells")

# Unallocated debit per (match, losing team, zone). The losing team's zone is the
# mirror of the kicking team's, so a contest in the kicker's attacking third is
# in the defending team's DEFENSIVE third.
mirror <- c(att = "def", def = "att", mid = "mid")
unnamed <- full[is.na(loser_pid), .(debit = sum(loser_credit)),
                by = .(match_id, team_id = loser_tid, zone = mirror[zone])]
stopifnot(unnamed[, all(debit <= 0)])
alloc <- merge(exposure, unnamed, by = c("match_id", "team_id", "zone"),
               allow.cartesian = TRUE)
alloc[, share := expo / sum(expo), by = .(match_id, team_id, zone)]
alloc[, cont_alloc := debit * share]
say("allocated debit rows: ", format(nrow(alloc), big.mark = ","))
say("conservation of the allocation: total debit ",
    round(unnamed[, sum(debit)], 2), " vs allocated ",
    round(alloc[, sum(cont_alloc)], 2))

arrow::write_parquet(full, TBL)
arrow::write_parquet(alloc[, .(match_id, team_id, player_id, zone, cont_alloc)],
                     file.path(OUT_DIR, "epv3_contest_loss_alloc.parquet"))
saveRDS(list(p = m_p, att = m_att, def = m_def, HALF = HALF, rhs = RHS), MODELS)
say("")
say("wrote ", TBL, " (", format(nrow(full), big.mark = ","), " rows)")
say("wrote ", MODELS)
close(con)
cat("\nWrote ", OUT, "\n")
