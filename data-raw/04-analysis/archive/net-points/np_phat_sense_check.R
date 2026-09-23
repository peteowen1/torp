# Is p_hat = 0.821 on Schultz's kick a real football rate, or a model artifact?
# =============================================================================
# Pete's objection, and it is the right one to make: "there's no way someone who
# just took a Mark On Lead with exp_pts 3.58 would kick into an 18% win contest."
#
# Two things to separate:
#   (a) is 82% the true empirical turnover rate for kicks like this one? If so
#       the model is reporting football, not failing.
#   (b) does the model have any way to know he had just marked -- i.e. had time
#       and space and a free choice -- versus being under pressure with nothing
#       on? The formula is:
#         turnover ~ s(x, abs_y) + s(kick_len) + s(fwd_gain) + s(goal_dist) +
#                    s(exp_pts) + is_handball + i50f
#       x/y and exp_pts ARE in it. The preceding event is NOT, and nor is any
#       pressure measure.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_phat_sense_check.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON)); ch[, match_id := as.character(match_id)]

de <- build_disposal_events(ch, pbp)
de <- as.data.table(de)
say("disposal events: ", format(nrow(de), big.mark = ","))
say("columns available to the model: ", paste(sort(names(de)), collapse = ", "))

TARGET <- de[match_id == "CD_M20260142402" & display_order == 1012]
if (nrow(TARGET) != 1) stop("target row not found")
say("\n=== Schultz's kick, as the model sees it ===")
print(TARGET[, .(x, abs_y, kick_len, fwd_gain, goal_dist, exp_pts, is_handball, i50f, turnover)])

# (a) EMPIRICAL rate for comparable kicks -- no model involved.
k <- de[as.character(is_handball) == "0" & !is.na(turnover)]
band <- function(dt, xa, xb, la, lb, label) {
  s <- dt[x >= xa & x < xb & kick_len >= la & kick_len < lb]
  if (nrow(s) < 30) { say("  ", label, ": only ", nrow(s), " rows, skipping"); return(invisible()) }
  say("  ", formatC(label, width = -46), " n=", formatC(nrow(s), width = 6),
      "   turnover rate ", formatC(100 * mean(s$turnover), format = "f", digits = 1), "%")
}
say("\n=== (a) EMPIRICAL turnover rate, kicks only, no model ===")
say("Schultz: x=", round(TARGET$x, 1), " kick_len=", round(TARGET$kick_len, 1),
    " goal_dist=", round(TARGET$goal_dist, 1), " i50f=", TARGET$i50f)
xl <- TARGET$x - 8; xh <- TARGET$x + 8
kl <- TARGET$kick_len - 8; kh <- TARGET$kick_len + 8
band(k, xl, xh, kl, kh, paste0("same zone AND same kick length"))
band(k, xl, xh, 0, 1e9, paste0("same zone, any kick length"))
band(k, -1e9, 1e9, kl, kh, paste0("any zone, same kick length"))
say("  (the model said 82.1% for this row)")

# (b) does the preceding event change the real rate? The model cannot see it.
say("\n=== (b) does what happened BEFORE the kick change the real rate? ===")
say("(the model has no term for it, so any difference here is signal it cannot use)")
seq <- ch[, .(match_id = as.character(match_id), display_order, description)]
setorder(seq, match_id, display_order)
seq[, prev_desc := shift(description), by = match_id]
kk <- merge(k, seq[, .(match_id, display_order, prev_desc)],
            by = c("match_id", "display_order"), all.x = TRUE)
inz <- kk[x >= xl & x < xh & kick_len >= kl & kick_len < kh & !is.na(prev_desc)]
tab <- inz[, .(n = .N, turnover_rate = round(100 * mean(turnover), 1)), by = prev_desc][n >= 25][order(turnover_rate)]
if (nrow(tab)) print(tab) else say("  (too few rows in the band to split by previous event)")

say("\n=== same split, ALL kicks (bigger n) ===")
tab2 <- kk[!is.na(prev_desc), .(n = .N, turnover_rate = round(100 * mean(turnover), 1)),
           by = prev_desc][n >= 500][order(turnover_rate)]
print(head(tab2, 12))
