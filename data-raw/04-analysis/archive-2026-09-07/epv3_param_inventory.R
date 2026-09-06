# EPV v3: what each channel contributes to EPR, and exactly which parameters are
# live, inherited-unoptimised, or dead.
#
# Two questions this answers precisely, because both have been asserted from
# memory in this session and neither should be:
#   1. Channel share of EPR variance (not EPV -- EPR is the decayed, shrunk
#      aggregate, and the shares differ).
#   2. Which v2 constants still do anything under v3, and which are inert.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_param_inventory.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 60) for (l in capture.output(print(utils::head(x, n)))) say(l)

# ---- 1. EPR channel shares -------------------------------------------------
say("=== 1. EPR channel SDs and variance shares ===")
say("(EPR, not EPV -- decayed + Bayesian-shrunk. Memory records the published v2")
say(" baseline as disp 67.3 / recv 30.8 / spoil 1.4 / hitout 0.5.)")
say("")
CH <- c("epr_recv", "epr_disp", "epr_spoil", "epr_hitout")
LBL <- c(epr_recv = "recv", epr_disp = "disp",
         epr_spoil = "cont_aerial", epr_hitout = "cont_stop")

report <- function(f, label) {
  p <- file.path(OUT_DIR, f)
  if (!file.exists(p)) return(NULL)
  x <- as.data.table(arrow::read_parquet(p))
  x <- x[!is.na(epr)]
  s <- vapply(CH, function(c) sd(x[[c]], na.rm = TRUE), numeric(1))
  v <- s^2
  data.table(arm = label, channel = LBL[CH], sd = round(s, 4),
             var_share_pct = round(100 * v / sum(v), 1),
             epr_sd = round(sd(x$epr, na.rm = TRUE), 3), n = nrow(x))
}
tbl <- rbindlist(list(
  report("epv3_ratings_v2.parquet", "v2"),
  report("epv3_ratings_v3.parquet", "v3-4ch"),
  report("epv3_ratings_v3_3ch.parquet", "v3-3ch")
))
say_dt(tbl, 20)

say("")
say("--- channel correlations within EPR (v3-4ch) ---")
x3 <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "epv3_ratings_v3.parquet")))[!is.na(epr)]
m <- as.matrix(x3[, ..CH]); colnames(m) <- LBL[CH]
say_dt(as.data.table(round(cor(m, use = "complete.obs"), 3), keep.rownames = "channel"), 6)

# ---- 2. Which parameters are actually live under v3? -----------------------
say("")
say("=== 2. Parameter inventory ===")
say("")
say("--- (a) STILL LIVE and UNCHANGED from v2 (inherited, never optimised) ---")
live <- data.table(
  constant = c("EPV_DISP_SCALE", "EPV_RECV_SCALE",
               "EPV_RECV_NEG_MULT", "EPV_RECV_POS_MULT",
               "EPV_RECV_NEG_OFFSET", "EPV_RECV_POS_OFFSET",
               "EPV_DISP_NEG_OFFSET", "EPV_DISP_POS_OFFSET",
               "EPV_HITOUT_WT", "EPV_HITOUT_ADV_WT", "EPV_RUCK_CONTEST_WT"),
  value = c(EPV_DISP_SCALE, EPV_RECV_SCALE, EPV_RECV_NEG_MULT, EPV_RECV_POS_MULT,
            EPV_RECV_NEG_OFFSET, EPV_RECV_POS_OFFSET,
            EPV_DISP_NEG_OFFSET, EPV_DISP_POS_OFFSET,
            EPV_HITOUT_WT, EPV_HITOUT_ADV_WT, EPV_RUCK_CONTEST_WT),
  applies_to = c("non-aerial disposals only", "non-aerial receptions only",
                 "non-aerial receptions", "non-aerial receptions",
                 "non-aerial receptions", "non-aerial receptions",
                 "non-aerial disposals", "non-aerial disposals",
                 "cont_stop", "cont_stop", "cont_stop")
)
say_dt(live, 15)

say("")
say("--- (b) DEAD or nearly dead under v3 ---")
say("EPV_RECV_INTERCEPT_MARK_SCALE = ", EPV_RECV_INTERCEPT_MARK_SCALE,
    "  (it is 1, i.e. already a no-op; the claim it should be 2.06 was RETRACTED")
say("   in 2026-07 because production already conserves 50/50 on intercept marks)")
say("EPV_SPOIL_WT = ", EPV_SPOIL_WT, ", EPV_TACKLE_WT = ", EPV_TACKLE_WT,
    ", + ~28 other box weights: NOT READ AT ALL under v3.")
say("EPV_RECV_FAILED_CONTEST_WT = ", EPV_RECV_FAILED_CONTEST_WT,
    " -- only used by the deprecated compute_failed_recv_credit().")

say("")
say("--- (c) how much of recv is still the intercept-mark branch under v3? ---")
say("An intercept mark off a KICK is now an aerial contest and is excluded from")
say("recv entirely. Only intercept marks that are not kick outcomes remain.")
pbp <- load_pbp(TRUE)
dt <- as.data.table(pbp)
dt[, is_intercept_mark := pos_team == -1L & grepl("ted Mark|Mark On", lead_desc_tot)]
say("PBP rows flagged is_intercept_mark: ",
    format(sum(dt$is_intercept_mark, na.rm = TRUE), big.mark = ","),
    " of ", format(nrow(dt), big.mark = ","))
ch <- load_chains(TRUE)
aer <- build_aerial_contests(ch, pbp)
keys <- unique(aer[, .(match_id, display_order = kick_do)])
dt[, .is_aerial_kick := FALSE]
dt[keys, .is_aerial_kick := TRUE, on = .(match_id, display_order)]
say("of those, now consumed by the aerial contest channel: ",
    format(dt[is_intercept_mark == TRUE & .is_aerial_kick == TRUE, .N], big.mark = ","),
    " (", round(100 * mean(dt[is_intercept_mark == TRUE]$.is_aerial_kick), 1), "%)")
say("still priced by the recv intercept branch: ",
    format(dt[is_intercept_mark == TRUE & .is_aerial_kick == FALSE, .N], big.mark = ","))

say("")
say("--- (d) what fraction of ALL credit still goes through the 50/50 split? ---")
say("aerial-kick PBP rows (contest-priced): ",
    format(sum(dt$.is_aerial_kick), big.mark = ","),
    " (", round(100 * mean(dt$.is_aerial_kick), 1), "% of PBP rows)")
say("everything else keeps EPV_DISP_SCALE 0.5 / EPV_RECV_SCALE 0.5.")

say("")
say("--- (e) FITTED, not inherited ---")
say("p(defence wins), V_att, V_def: three bam() fits per season, leak-safe")
say("  (fitted on strictly earlier seasons). These have no hand-set constants --")
say("  the contest split p*Delta / (1-p)*Delta is FORCED by conservation, which")
say("  is the design's whole point: there is no share parameter to tune.")
say("EPV3_POINTS_SCALE: fitted (recv 0.5969, disp 0.6095, cont_aerial 0.2656,")
say("  cont_stop 1.7680) but NOT APPLIED -- still all 1.")

say("")
say("=== 3. What has NOT been optimised (i.e. the headroom) ===")
say("NOTHING structural was optimised in this work. Specifically:")
say(" * the 50/50 non-aerial disposer/receiver split is v2's, untouched")
say(" * the three cont_stop hitout weights are v2's, untouched")
say(" * every EPR_DECAY_* / EPR_PRIOR_GAMES_* / EPR_PRIOR_RATE_* is v2's,")
say("   including for the contest channel, which is now a DIFFERENT quantity")
say("   from the spoil channel those priors were tuned for")
say(" * EPV_POINTS_SCALE 0.919 still applies globally to both arms")
say("The contest channel inheriting the old spoil channel's decay and priors is")
say("the most likely place the 0.184 MAE is hiding: EPR_DECAY_SPOIL = ",
    EPR_DECAY_SPOIL, " and")
say("EPR_PRIOR_RATE_SPOIL = ", round(EPR_PRIOR_RATE_SPOIL, 4),
    " were fitted for a flat per-spoil count, not")
say("for a signed, surprise-weighted, 2.6-sd contest value.")

close(con)
cat("\nWrote ", OUT, "\n")
