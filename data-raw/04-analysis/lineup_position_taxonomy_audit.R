# O1 audit (plan §7.13) — audit ALL 18 on-field lineup codes against objective
# evidence, per the standing rule that one wrong entry in hand-maintained
# reference data predicts siblings.
#
# WHY: the current 20->6 lineup_map (player_skills_data.R:33-43) assigns
# CHB -> MEDIUM_DEFENDER, CHF -> MEDIUM_FORWARD and FPL/FPR -> KEY_FORWARD,
# all three of which look football-wrong, and those codes carry the highest
# disagreement with the PBP-derived position_group (FPL 72.2%, FPR 68.7%,
# CHF 67.3%, CHB 50.4%). Rather than spot-fix the flagged entries, audit
# every code.
#
# EVIDENCE, in order of objectivity:
#   1. HEIGHT (player_details.height_cm) -- the physical discriminator between
#      key posts (talls) and pocket/wing roles (smalls). This is the closest
#      thing to ground truth available and it is independent of any rating.
#   2. LISTED POSITION (player_details.position) -- the club's own classification.
#   3. PBP position_group -- how the player actually played, per chains.
#   4. mean psr_raw -- the level the centring subtracts (consequence, not cause;
#      listed last deliberately so the taxonomy is not fitted to the metric).
suppressMessages({library(arrow); library(data.table)})

D  <- "C:/dev/torpverse/torpdata/data/"
SF <- 2021:2026
DS <- 2021:2025           # player_details availability
rd <- function(pat, ss) rbindlist(lapply(ss, function(s) {
  f <- file.path(D, sprintf(pat, s)); if (!file.exists(f)) return(NULL)
  as.data.table(read_parquet(f)) }), use.names = TRUE, fill = TRUE)

# current map, verbatim from player_skills_data.R:33-43
CUR <- c(FB="KEY_DEFENDER",
         BPL="MEDIUM_DEFENDER", BPR="MEDIUM_DEFENDER",
         CHB="MEDIUM_DEFENDER", HBFL="MEDIUM_DEFENDER", HBFR="MEDIUM_DEFENDER",
         C="MIDFIELDER", WL="MIDFIELDER", WR="MIDFIELDER", R="MIDFIELDER", RR="MIDFIELDER",
         RK="RUCK",
         HFFL="MEDIUM_FORWARD", HFFR="MEDIUM_FORWARD", CHF="MEDIUM_FORWARD",
         FPL="KEY_FORWARD", FPR="KEY_FORWARD", FF="KEY_FORWARD")

# proposed 9-way role. Centre is grouped with the rovers, NOT the wings
# (Pete, 2026-07-27): C/R/RR are the inside-midfield trio at a centre bounce
# and the wings are outside runners. The psr_raw levels agree -- C 11.04,
# RR 10.96, R 10.74 cluster, while WL 10.06 / WR 10.09 sit a tier below.
# CHB is the one genuinely ambiguous code and is left grouped with FB here as a
# *role* judgement (they are the two central defensive posts) even though the
# clubs list CHBs as medium defenders -- see the FIX note below. PBP disagrees
# with whatever you choose 50% of the time, so this is a call, not a fact.
ROLE9 <- c(FB="KEY_BACK", CHB="KEY_BACK",
           BPL="POCKET_BACK", BPR="POCKET_BACK",
           HBFL="HALF_BACK", HBFR="HALF_BACK",
           WL="WING", WR="WING",
           C="INSIDE_MID", R="INSIDE_MID", RR="INSIDE_MID",
           RK="RUCK",
           CHF="KEY_FWD", FF="KEY_FWD",
           FPL="POCKET_FWD", FPR="POCKET_FWD",
           HFFL="HALF_FWD", HFFR="HALF_FWD")

# --- assemble ---------------------------------------------------------------
pg <- rd("player_game_%d.parquet", SF)[, .(player_id, season, round = as.numeric(round),
        position_group, lineup_position)]
pg <- pg[!is.na(lineup_position) & !lineup_position %in% c("INT","EMERG","SUB")]

det <- rd("player_details_%d.parquet", DS)[, .(player_id, season, height_cm,
                                               listed = position)]
det <- unique(det[!is.na(height_cm) & height_cm > 100], by = c("player_id","season"))

coefs <- fread("C:/dev/torpverse/torp/inst/extdata/psr_coefficients.csv")[beta != 0]
sr <- rd("player_stat_ratings_%d.parquet", SF); sr[, round := as.numeric(round)]
v <- numeric(nrow(sr))
for (i in seq_len(nrow(coefs))) {
  cc <- paste0(coefs$stat_name[i], "_rating"); if (!cc %in% names(sr)) next
  sdv <- coefs$sd[i]; if (is.na(sdv) || sdv == 0) sdv <- 1
  x <- sr[[cc]]; x[is.na(x)] <- 0; v <- v + coefs$beta[i]*(x/sdv)
}
sr[, psr_raw := v]

m <- merge(pg, det, by = c("player_id","season"), all.x = TRUE)
m <- merge(m, sr[, .(player_id, season, round, psr_raw, wt_80s)],
           by = c("player_id","season","round"), all.x = TRUE)
m[, `:=`(cur = unname(CUR[lineup_position]), role = unname(ROLE9[lineup_position]))]

modal <- function(x) { x <- x[!is.na(x)]; if (!length(x)) NA_character_ else
  names(which.max(table(x))) }

cat("=== PER-CODE AUDIT (18 on-field lineup codes) ===\n")
a <- m[, .(N = .N,
           height = round(mean(height_cm, na.rm = TRUE), 1),
           listed_modal = modal(listed),
           pbp_modal = modal(position_group),
           pbp_disagree = round(100*mean(position_group != cur, na.rm = TRUE), 0),
           psr_raw = round(weighted.mean(psr_raw, wt_80s, na.rm = TRUE), 2)),
       by = .(lineup_position, cur, role)]
setorder(a, -height)
print(a, nrows = 30)

cat("\n=== HEIGHT ORDERING vs the current KEY/MEDIUM split ===\n")
cat("If the current map were right, every KEY_* code would sit above every\n")
cat("MEDIUM_* code of the same end. Flagging where it does not:\n\n")
tall <- a[order(-height)]
for (i in seq_len(nrow(tall))) {
  r <- tall[i]
  flag <- ""
  if (grepl("^KEY", r$cur) && r$height < 190) flag <- "  <-- KEY but short"
  if (grepl("^MEDIUM", r$cur) && r$height >= 193) flag <- "  <-- MEDIUM but tall"
  cat(sprintf("  %-5s %5.1fcm  cur=%-16s proposed=%-12s%s\n",
              r$lineup_position, r$height, r$cur, r$role, flag))
}

cat("\n=== proposed CORRECTED 6-way map (minimal fix, keeps granularity) ===\n")
# Derived from height + the clubs' own listed position, NOT from psr_raw.
#
# NOTE — one of my three original flags is REFUTED by this audit. I claimed
# CHB was mis-assigned and should be KEY_DEFENDER. The evidence says otherwise:
# the clubs list CHBs as MEDIUM_DEFENDER (listed_modal), the PBP modal is also
# MEDIUM_DEFENDER, and at 191.0cm a CHB is barely taller than a back pocket
# (BPR 190.0, BPL 189.9) and far short of a full-back (196.1). CHB stays where
# it is. Only the two FORWARD-side flags survive, and both survive strongly.
FIX <- c(FB="KEY_DEFENDER", CHB="MEDIUM_DEFENDER",
         BPL="MEDIUM_DEFENDER", BPR="MEDIUM_DEFENDER",
         HBFL="MEDIUM_DEFENDER", HBFR="MEDIUM_DEFENDER",
         C="MIDFIELDER", WL="MIDFIELDER", WR="MIDFIELDER", R="MIDFIELDER", RR="MIDFIELDER",
         RK="RUCK",
         CHF="KEY_FORWARD", FF="KEY_FORWARD",
         FPL="MEDIUM_FORWARD", FPR="MEDIUM_FORWARD",
         HFFL="MEDIUM_FORWARD", HFFR="MEDIUM_FORWARD")
chg <- data.table(code = names(CUR), current = unname(CUR),
                  corrected = unname(FIX[names(CUR)]))
chg <- merge(chg, a[, .(code = lineup_position, height)], by = "code")
print(chg[current != corrected][order(-height)])
cat(sprintf("\n%d of 18 codes change under the corrected 6-way map.\n",
            nrow(chg[current != corrected])))

cat("\n=== bucket sizes and heights: current vs corrected vs 9-way ===\n")
m[, fix := unname(FIX[lineup_position])]
for (col in c("cur","fix","role")) {
  cat(sprintf("\n-- %s --\n", c(cur="CURRENT 6-way", fix="CORRECTED 6-way",
                                role="PROPOSED 9-way")[col]))
  print(m[, .(N = .N, height = round(mean(height_cm, na.rm=TRUE),1),
              psr_raw = round(weighted.mean(psr_raw, wt_80s, na.rm=TRUE),2)),
          by = col][order(-height)])
}
