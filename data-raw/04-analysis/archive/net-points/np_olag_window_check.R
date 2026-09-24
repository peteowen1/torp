# Is the 6-row lookahead window itself the problem? (issue #210)
# =============================================================================
# Pete: "scanning 6 rows ahead seems wild - what about if you just scan 1 row?"
#
# build_disposal_events() finds the resolving row by scanning 1-6 rows forward
# past in-flight annotations, and records `.olag` = how many rows it had to go.
# Everything leaky is read off that row: out_x, out_y -> kick_len / fwd_gain,
# and out_tid -> the turnover label itself.
#
# So the question splits in two, and they have different answers:
#   (a) Does a LONGER scan produce the absurd distances? If .olag 5-6 rows carry
#       the 115m displacements, the window is a real lever.
#   (b) Would scanning only 1 row fix it? Careful: out_tid is the LABEL, so
#       shrinking the window changes what is being predicted, not just the
#       features. The first row after a kick is an in-flight annotation 27.1% of
#       the time, and those carry the KICKING team's id -- so a lost kick would
#       be relabelled "retained". That is not a smaller leak, it is a wrong
#       target.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_olag_window_check.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON)); ch[, match_id := as.character(match_id)]
de  <- as.data.table(build_disposal_events(ch, pbp))
de  <- de[!is.na(turnover)]

say("=== how far ahead does the scan actually go? ===")
tab <- de[, .(n = .N,
              pct = round(100 * .N / nrow(de), 1),
              median_kick_len = round(median(kick_len), 1),
              pct_over_60m = round(100 * mean(kick_len > 60), 1),
              turnover_rate = round(100 * mean(turnover), 1)), by = .olag][order(.olag)]
print(tab)

say("\nReading it: if the absurd distances were a long-scan artifact, the")
say("median kick_len and the >60m share would climb steeply with .olag.")

say("\n=== where do the near-certain rows sit? ===")
tm <- fread("data-raw/outputs/np_difficulty_terms_2025_2026.csv")
tm[, match_id := as.character(match_id)]
hot <- tm[substr(match_id, 5, 8) == as.character(SEASON) & p_hat >= 0.999,
          .(match_id, display_order)]
h <- merge(hot, de[, .(match_id, display_order, .olag, kick_len, turnover)],
           by = c("match_id", "display_order"))
say("p_hat >= 0.999 rows: ", nrow(h))
print(h[, .(n = .N, median_kick_len = round(median(kick_len), 1)), by = .olag][order(.olag)])
say("\nfor comparison, .olag distribution over ALL disposals:")
print(de[, .(n = .N, pct = round(100 * .N / nrow(de), 1)), by = .olag][order(.olag)])

say("\n=== (b) what would a 1-row window do to the LABEL? ===")
say("The first row after a disposal, and whose team it carries:")
c2 <- ch[, .(match_id = as.character(match_id), display_order, description, team_id, x, y)]
setorder(c2, match_id, display_order)
for (s in c("description", "team_id", "x", "y"))
  c2[, (paste0("f1_", s)) := shift(get(s), 1, type = "lead"), by = match_id]
d1 <- merge(de[, .(match_id, display_order, team_id, turnover, .olag, kick_len)],
            c2[, .(match_id, display_order, f1_description, f1_team_id, f1_x, f1_y, x, y)],
            by = c("match_id", "display_order"))
d1[, inflight_next := f1_description %chin% CHAINS_INFLIGHT_DESCS]
d1[, turnover_1row := f1_team_id != team_id]
say("  first row is an in-flight annotation: ",
    round(100 * mean(d1$inflight_next, na.rm = TRUE), 1), "%")
say("  label DISAGREES with the current one: ",
    round(100 * mean(d1$turnover_1row != d1$turnover, na.rm = TRUE), 1), "% of disposals")
say("\n  of the disposals currently labelled TURNOVER, a 1-row window would")
say("  relabel this share as RETAINED: ",
    round(100 * d1[turnover == TRUE, mean(turnover_1row == FALSE, na.rm = TRUE)], 1), "%")
say("  ...and among those, the share whose next row is in-flight: ",
    round(100 * d1[turnover == TRUE & turnover_1row == FALSE,
                   mean(inflight_next, na.rm = TRUE)], 1), "%")

say("\n=== and to the distance? ===")
d1[, kick_len_1row := sqrt((f1_x - x)^2 + (f1_y - y)^2)]
say("  median kick_len, current : ", round(median(d1$kick_len, na.rm = TRUE), 1), "m")
say("  median kick_len, 1-row   : ", round(median(d1$kick_len_1row, na.rm = TRUE), 1), "m")
say("  share >60m, current      : ", round(100 * mean(d1$kick_len > 60, na.rm = TRUE), 1), "%")
say("  share >60m, 1-row        : ", round(100 * mean(d1$kick_len_1row > 60, na.rm = TRUE), 1), "%")
