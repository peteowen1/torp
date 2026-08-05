# The surprise share, measured WITHIN branch -- because the first attempt
# measured a position classifier and called it a credit share.
#
# WHAT WENT WRONG, AND IT IS VISIBLE IN THE OUTPUT ITSELF. The first run
# reported share_recv 0.885 (variance components) and 0.752 (split-half), both
# far above the assumed 0.5. Then the band table:
#
#     band      r_disp  r_recv   share      turnover rate
#     <15m      0.126   0.157    0.553      9%
#     15-25m    0.206   0.415    0.669      18%
#     25-35m    0.110   0.725    0.868      27%
#     35-45m    0.166   0.803    0.829      44%
#     45-55m    0.116   0.834    0.878      56%
#     >55m      0.069   0.795    0.921      65%
#     handball  0.172   0.191    0.527      10.5%
#     kick      0.241   0.860    0.781      34.7%
#
# r_recv tracks the TURNOVER RATE, not difficulty. That is the confound, not a
# gradient. `surprise` is signed in the disposing team's frame, and the
# "receiver" pool mixes intended teammates (positive) with intercepting
# opponents (negative). A player's mean is therefore dominated by how often he
# is the interceptor rather than the target -- which is his position, is almost
# perfectly repeatable, and is not skill at all. Where turnovers are rare
# (handball, 10.5%) the confound has nothing to work with and the share collapses
# to 0.527, i.e. to the 0.5 it was supposed to be testing.
#
# So 0.885 and 0.752 are withdrawn. They measure how reliably you can tell a key
# defender from a small forward.
#
# THE CORRECTED DESIGN. Measure within branch, where the sign is fixed and
# composition cannot drive anything, and residualise against the branch's own
# fitted value so the situation is gone too:
#
#     retained   resid = V_after - V_ret_hat
#     turnover   resid = V_after - V_trn_hat
#
# Both have conditional mean zero by construction, so a player's mean residual
# is what his identity adds over the situation -- which is the actual question.
#
# The two branches are reported separately and NOT averaged. A disposer's share
# of "I hit a teammate and it was worth a lot" and his share of "I turned it
# over to a man who made it hurt" are different quantities, and one constant for
# both was never argued for, only assumed.
#
# CHECK 0 runs first and is decisive: if a player's mean surprise is explained by
# his intercept share, the original estimate was measuring that and nothing else.
#
# ~6 min, cached table only.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_surprise_share_v2.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 45) for (l in capture.output(print(utils::head(x, n)))) say(l)

say("=== Surprise share, measured within branch ==="); say("run at ", format(Sys.time()))

sc <- as.data.table(read_parquet(file.path(OUT_DIR, "epv3_difficulty_scored.parquet")))
sc[, surprise := V_after - V_pre]
sc <- sc[is.finite(surprise) & !is.na(player_id) & !is.na(out_pid) &
           is.finite(V_ret_hat) & is.finite(V_trn_hat)]
say("disposals ", format(nrow(sc), big.mark = ","),
    " | turnover rate ", round(100 * mean(sc$turnover), 1), "%")

# ------------------------------------------------------------------- check 0
say(""); say("=== 0. IS THE ORIGINAL ESTIMATE JUST AN INTERCEPT-SHARE CLASSIFIER? ===")
rc <- sc[, .(n = .N, intercept_share = mean(turnover),
             mean_surprise = mean(surprise)), by = .(pid = out_pid)][n >= 200]
say(sprintf("  players %d | cor(intercept share, mean surprise as receiver) = %+.4f",
            nrow(rc), cor(rc$intercept_share, rc$mean_surprise)))
say(sprintf("  R2 of mean surprise on intercept share alone: %.4f",
            summary(stats::lm(mean_surprise ~ intercept_share, data = rc))$r.squared))
say("  Near -1 / near 1 means the 'receiver skill' the first run found was the")
say("  player's ROLE. Anything else and the confound is smaller than argued.")

# --------------------------------------------------------------- the estimator
# Split-half on GAME means of the branch residual. Odd games vs even games for
# the same player-season, so the only thing shared between halves is him.
sh <- function(pid, mid, val, min_games = 12) {
  x <- data.table(pid = pid, match_id = mid, v = val)
  x <- x[is.finite(v) & !is.na(pid)]
  x[, season := as.integer(substr(match_id, 5, 8))]
  mg <- unique(x[, .(pid, season, match_id)])
  setorder(mg, pid, season, match_id)
  mg[, gi := seq_len(.N), by = .(pid, season)]
  x <- merge(x, mg, by = c("pid", "season", "match_id"))
  s <- x[, .(a = mean(v[gi %% 2 == 1]), b = mean(v[gi %% 2 == 0]),
             n = uniqueN(match_id)), by = .(pid, season)]
  s <- s[n >= min_games & is.finite(a) & is.finite(b)]
  if (nrow(s) < 50) return(c(n = nrow(s), r = NA_real_))
  c(n = nrow(s), r = round(cor(s$a, s$b), 4))
}

branch_share <- function(d, label) {
  a <- sh(d$player_id, d$match_id, d$resid)
  b <- sh(d$out_pid,   d$match_id, d$resid)
  sh_v <- if (is.na(a[["r"]]) || is.na(b[["r"]])) NA_real_ else {
    ap <- max(a[["r"]], 0); bp <- max(b[["r"]], 0)
    if (ap + bp == 0) NA_real_ else round(bp / (ap + bp), 4)
  }
  say(sprintf("  %-22s disposer r %+.4f (n %d) | resolver r %+.4f (n %d) => share_resolver %s",
              label, a[["r"]], a[["n"]], b[["r"]], b[["n"]],
              ifelse(is.na(sh_v), "n/a", sprintf("%.4f", sh_v))))
  sh_v
}

say(""); say("=== 1. WITHIN-BRANCH RESIDUAL, BY BRANCH ===")
say("resid has conditional mean zero by construction, so nothing here is")
say("explained by the situation or by which branch occurred.")
ret <- sc[turnover == FALSE][, resid := V_after - V_ret_hat]
trn <- sc[turnover == TRUE ][, resid := V_after - V_trn_hat]
say(sprintf("  retained %s (resid sd %.4f, mean %+.5f)",
            format(nrow(ret), big.mark = ","), sd(ret$resid), mean(ret$resid)))
say(sprintf("  turnover %s (resid sd %.4f, mean %+.5f)",
            format(nrow(trn), big.mark = ","), sd(trn$resid), mean(trn$resid)))
say("")
s_ret <- branch_share(ret, "retained")
s_trn <- branch_share(trn, "turnover")

say(""); say("  and the same within branch, by disposal type -- the confound")
say("  predicted a gradient here and a clean measurement should not show one:")
for (ty in c("Handball", "Kick")) {
  branch_share(ret[description == ty], paste0("retained/", ty))
  branch_share(trn[description == ty], paste0("turnover/", ty))
}

# ------------------------------------------------------ 2. variance components
say(""); say("=== 2. VARIANCE COMPONENTS ON THE SAME RESIDUAL ===")
vc <- function(d, label) {
  if (!requireNamespace("lme4", quietly = TRUE)) { say("  lme4 missing"); return(NA_real_) }
  set.seed(42)
  x <- d[sample(.N, min(.N, 200000L))]
  x[, `:=`(disposer = droplevels(factor(player_id)), receiver = droplevels(factor(out_pid)))]
  f <- lme4::lmer(resid ~ 1 + (1 | disposer) + (1 | receiver), data = x,
                  REML = TRUE, control = lme4::lmerControl(calc.derivs = FALSE))
  v <- as.data.table(lme4::VarCorr(f))
  vd <- v[grp == "disposer", vcov]; vr <- v[grp == "receiver", vcov]
  s <- vr / (vd + vr)
  say(sprintf("  %-10s var_disposer %.6f  var_resolver %.6f  => share_resolver %.4f",
              label, vd, vr, s))
  s
}
v_ret <- vc(ret, "retained")
v_trn <- vc(trn, "turnover")

# ------------------------------------------------------------------- verdict
say(""); say("=== WHAT THIS LICENSES ===")
say_dt(data.table(
  branch = c("retained", "turnover"),
  split_half = c(s_ret, s_trn),
  var_comp = round(c(v_ret, v_trn), 4),
  current = c(0.5, 0.5)), 3)
say("")
say("READING IT. A share near 0.5 on both branches means the assumed constant")
say("was right and nothing needs changing -- which is a real result, not a null.")
say("Two shares far apart means one constant is the wrong SHAPE, and the fix is")
say("a second constant, not a different single number.")
say("")
say("CARRIED ASSUMPTION, stated because it is not testable here: the share is")
say("measured on the within-branch residual and would be applied to the whole")
say("surprise, which assumes the split is the same for the branch-selection part")
say("(V_branch - V_pre) as for the residual. The branch-selection part cannot be")
say("split the same way -- on a turnover the INTENDED receiver is not in the")
say("data at all, so there is no second named player to share it with.")

saveRDS(list(ret_sh = s_ret, trn_sh = s_trn, ret_vc = v_ret, trn_vc = v_trn),
        file.path(OUT_DIR, "epv3_surprise_share_v2.rds"))
say(""); say("done ", format(Sys.time())); close(con); cat("\nDone\n")
