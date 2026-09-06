# What share of the surprise belongs to the receiver? Measure it, don't assume it.
#
# EPV_DIFFICULTY_SURPRISE_SHARE is currently 0.5, and 0.5 is an assumption
# wearing the clothes of a finding. It was chosen only because handing the whole
# surprise to the resolver is winner-take-all on the one term carrying skill --
# a reason to move OFF 1.0, not a reason to land on 0.5.
#
# THE QUESTION IN A FORM THAT CAN BE ANSWERED. The surprise on a disposal is one
# number, V_after - V_pre. Two players touched it. The share each deserves is the
# share of that number their IDENTITY reliably explains: if swapping the disposer
# changes the expected surprise a lot and swapping the receiver changes it
# little, the disposer did most of it. That is a variance-components question,
# and it has a standard answer.
#
# Two independent estimators, because either alone can mislead:
#
#   variance components   surprise ~ (1|disposer) + (1|receiver), crossed. The
#                         REML variance components are exactly "how much of this
#                         term is attributable to each role", with shrinkage
#                         already handling players who appear rarely.
#                         share_recv = var_recv / (var_disp + var_recv)
#
#   split-half            each player's mean surprise as disposer, odd games vs
#                         even games; same as receiver. A role whose per-player
#                         mean does not repeat is not carrying signal to pay for.
#                         share_recv = r_recv / (r_disp + r_recv)
#
# They measure different things -- one partitions variance, one partitions
# reliable variance -- so agreement is evidence and disagreement is informative
# rather than a failure. Neither is treated as decisive on its own.
#
# ALSO TESTED: whether the share should be CONSTANT. Oliver's formula makes it
# depend on the two participants' relative difficulty, so it is fitted per
# disposal type and per length band as well as pooled. If it moves a lot across
# bands, a single constant is the wrong shape and that is worth knowing before
# tuning the constant.
#
# ~10 min. Reads the cached scored table; fits on a subsample for lme4's sake.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_surprise_share.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 45) for (l in capture.output(print(utils::head(x, n)))) say(l)

say("=== Fitting the surprise share ==="); say("run at ", format(Sys.time()))

SC_F <- file.path(OUT_DIR, "epv3_difficulty_scored.parquet")
if (!file.exists(SC_F)) stop("missing scored table: ", SC_F)
sc <- as.data.table(read_parquet(SC_F))

# Recompute the surprise from V_after and V_pre. The cached table was written
# before the branch-value correction, so its `surprise` column is the old
# branch-only version -- V_pre and V_after are untouched by that fix, so this is
# the corrected quantity built from stored inputs, not a re-fit.
sc[, surprise := V_after - V_pre]
sc <- sc[is.finite(surprise) & !is.na(player_id) & !is.na(out_pid)]
say("disposals ", format(nrow(sc), big.mark = ","),
    " | surprise sd ", round(sd(sc$surprise), 4),
    " mean ", round(mean(sc$surprise), 4))
say("")
say("NOTE ON FRAME: `surprise` is in the DISPOSING team's frame throughout, so a")
say("turnover carries a negative one. Both roles are modelled on that single")
say("signed number -- flipping the receiver's sign first would make the two")
say("variance components incomparable, which is the point of the exercise.")

sc[, `:=`(disposer = factor(player_id), receiver = factor(out_pid))]
sc[, len_band := cut(kick_len, c(-Inf, 15, 25, 35, 45, 55, Inf),
                     labels = c("<15m", "15-25m", "25-35m", "35-45m", "45-55m", ">55m"))]

# ------------------------------------------------------- 1. variance components
say(""); say("=== 1. VARIANCE COMPONENTS (crossed random effects) ===")
have_lme4 <- requireNamespace("lme4", quietly = TRUE)
vc_share <- NA_real_
if (!have_lme4) {
  say("  lme4 not installed -- skipped. The split-half estimate below stands alone,")
  say("  which is weaker evidence; install lme4 to close this.")
} else {
  set.seed(42)
  N <- min(nrow(sc), 250000L)
  ss <- sc[sample(.N, N)]
  ss[, `:=`(disposer = droplevels(disposer), receiver = droplevels(receiver))]
  say("  fitted on ", format(N, big.mark = ","), " sampled disposals, ",
      uniqueN(ss$disposer), " disposers / ", uniqueN(ss$receiver), " receivers")
  fit <- lme4::lmer(surprise ~ 1 + (1 | disposer) + (1 | receiver),
                    data = ss, REML = TRUE,
                    control = lme4::lmerControl(calc.derivs = FALSE))
  v <- as.data.table(lme4::VarCorr(fit))
  vd <- v[grp == "disposer", vcov]; vr <- v[grp == "receiver", vcov]
  ve <- v[grp == "Residual", vcov]
  vc_share <- vr / (vd + vr)
  say_dt(data.table(component = c("disposer", "receiver", "residual"),
                    variance = round(c(vd, vr, ve), 6),
                    sd = round(sqrt(c(vd, vr, ve)), 4),
                    pct_of_total = round(100 * c(vd, vr, ve) / sum(c(vd, vr, ve)), 2)), 3)
  say(sprintf("  => share_recv = %.4f", vc_share))
  say("  Both player components are small next to the residual, which is expected:")
  say("  most of a single disposal's surprise is the situation, not either player.")
  say("  The SPLIT is what this estimates, and the split is well determined even")
  say("  when both parts are small.")
}

# --------------------------------------------------------------- 2. split-half
say(""); say("=== 2. SPLIT-HALF REPEATABILITY PER ROLE ===")
# Takes plain vectors, never a column name. `sc` carries the six lead-shift
# blocks from build_disposal_events (~45 columns), and get() inside [ on a frame
# that wide breaks data.table's fast column-reference path -- the documented
# trap that left ~5.4GB unreclaimed once before.
sh <- function(d, idcol) {
  x <- data.table(pid = d[[idcol]], match_id = d$match_id, surprise = d$surprise)
  x[, season := as.integer(substr(match_id, 5, 8))]
  setorder(x, pid, season, match_id)
  mg <- unique(x[, .(pid, season, match_id)])
  mg[, gi := seq_len(.N), by = .(pid, season)]
  x <- merge(x, mg, by = c("pid", "season", "match_id"))
  s <- x[, .(a = mean(surprise[gi %% 2 == 1]), b = mean(surprise[gi %% 2 == 0]),
             n = uniqueN(match_id)), by = .(pid, season)]
  s <- s[n >= 12 & is.finite(a) & is.finite(b)]
  c(n = nrow(s), r = round(cor(s$a, s$b), 4))
}
rd <- sh(sc, "player_id"); rr <- sh(sc, "out_pid")
say(sprintf("  disposer  split-half r %.4f (n %d)", rd[["r"]], rd[["n"]]))
say(sprintf("  receiver  split-half r %.4f (n %d)", rr[["r"]], rr[["n"]]))
sh_share <- rr[["r"]] / (rd[["r"]] + rr[["r"]])
say(sprintf("  => share_recv = %.4f", sh_share))

# ------------------------------------------------------- 3. is it constant?
say(""); say("=== 3. SHOULD THE SHARE BE CONSTANT? ===")
say("split-half r per role, within each band. If the two roles' reliability")
say("crosses over as disposals get harder, one constant is the wrong shape.")
bands <- list()
for (b in levels(sc$len_band)) {
  dd <- sc[description == "Kick" & len_band == b]
  if (nrow(dd) < 40000) { bands[[b]] <- data.table(band = b, n = nrow(dd),
    r_disp = NA_real_, r_recv = NA_real_, share = NA_real_); next }
  a <- sh(dd, "player_id"); c2 <- sh(dd, "out_pid")
  bands[[b]] <- data.table(band = b, n = nrow(dd), r_disp = a[["r"]],
    r_recv = c2[["r"]], share = round(c2[["r"]] / (a[["r"]] + c2[["r"]]), 3))
}
say_dt(rbindlist(bands), 8)
say("")
say("per disposal type:")
tt <- list()
for (b in unique(sc$description)) {
  dd <- sc[description == b]
  if (nrow(dd) < 40000) next
  a <- sh(dd, "player_id"); c2 <- sh(dd, "out_pid")
  tt[[b]] <- data.table(type = b, n = nrow(dd), r_disp = a[["r"]], r_recv = c2[["r"]],
                        share = round(c2[["r"]] / (a[["r"]] + c2[["r"]]), 3))
}
say_dt(rbindlist(tt), 5)

say(""); say("=== WHAT THIS LICENSES ===")
say(sprintf("  variance components  %s", ifelse(is.na(vc_share), "not run", sprintf("%.4f", vc_share))))
say(sprintf("  split-half           %.4f", sh_share))
say("  current constant     0.5000  (assumed)")
say("")
say("A change is worth making only if the two estimators agree AND the band")
say("table shows a roughly constant share. If they disagree, the honest reading")
say("is that the split is not yet identified and 0.5 stays as a stated")
say("assumption rather than being replaced by a number with a false pedigree.")

saveRDS(list(vc = vc_share, splithalf = sh_share, bands = rbindlist(bands)),
        file.path(OUT_DIR, "epv3_surprise_share.rds"))
say(""); say("done ", format(Sys.time())); close(con); cat("\nDone\n")
