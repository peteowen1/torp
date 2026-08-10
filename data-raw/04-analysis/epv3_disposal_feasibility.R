# Can the contest identity be applied to EVERY disposal, not just aerial ones?
#
# THE ARCHITECTURE THIS IS TESTING. v3 already implements Oliver's credit model,
# but only for the 8% of kicks that end in an aerial contest:
#
#   disposer gets  V_pre    - exp_pts    the expected value of the DECISION
#   winner gets    V_branch - V_pre      the SURPRISE -- did they beat expectation
#   next row gets  V_after  - V_branch
#
# Applied to every disposal, difficulty weighting falls out for free. A 15m chip
# to an unmarked teammate has p(retain) ~ 0.95, so V_pre is close to V_ret and
# catching it earns almost nothing. A 45m pass under pressure has a large
# expected gain for the kicker and a large surprise for the receiver who takes
# it. That is Oliver's (p_thrower - p_catcher + 1)/2 in spirit, derived from the
# state values rather than assumed -- and it replaces the flat 50/50 that
# currently splits both of those identically.
#
# BUT IT ONLY WORKS IF THE OUTCOME IS RESOLVABLE. The aerial version leans on an
# outcome row that names a player and a team. For an ordinary kick the question
# is just "which team next has the ball", which should be far MORE resolvable --
# but that needs checking, not assuming, and so does whether the resulting p has
# enough spread to be worth modelling. If 95% of kicks are retained with p ~ 0.95
# there is no difficulty signal to extract.
#
# What this measures, before any model is fitted:
#   1. what share of disposals have a resolvable next-possession team
#   2. the retain rate, and how it varies by disposal type, length and zone --
#      i.e. is there difficulty variation to model at all
#   3. how much EPV sits on each side of the split, so the size of the change
#      is known before it is built
#
# ~5 min.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_disposal_feasibility.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 45) for (l in capture.output(print(utils::head(x, n)))) say(l)

ch <- as.data.table(load_chains(TRUE))
nm <- uniqueN(ch$match_id)
setorder(ch, match_id, display_order)
say("=== Can the contest identity generalise to every disposal? ===")
say("run at ", format(Sys.time()))
say("chain rows ", format(nrow(ch), big.mark = ","), " over ", nm, " matches")

# Narrow first -- the full frame is 60+ columns and shifting on it is the
# documented data.table trap.
p <- data.table(match_id = ch$match_id, display_order = ch$display_order,
                description = ch$description, player_id = ch$player_id,
                team_id = ch$team_id, x = ch$x, y = ch$y,
                pos = ch$player_position)
rm(ch); invisible(gc())
setorder(p, match_id, display_order)

INFLIGHT <- CHAINS_INFLIGHT_DESCS
for (k in 1:6) {
  p[, (paste0("f", k, "_d")) := shift(description, k, type = "lead"), by = match_id]
  p[, (paste0("f", k, "_t")) := shift(team_id, k, type = "lead"), by = match_id]
  p[, (paste0("f", k, "_p")) := shift(player_id, k, type = "lead"), by = match_id]
  p[, (paste0("f", k, "_x")) := shift(x, k, type = "lead"), by = match_id]
  p[, (paste0("f", k, "_y")) := shift(y, k, type = "lead"), by = match_id]
}

DISP <- c("Kick", "Handball", "Ground Kick")
d <- p[description %chin% DISP & !is.na(player_id) & !is.na(team_id)]
say("disposals ", format(nrow(d), big.mark = ","), " (", round(nrow(d) / nm, 1), " per match)")

# First following row that is not an in-flight annotation.
d[, olag := fcase(
  !(f1_d %chin% INFLIGHT), 1L, !(f2_d %chin% INFLIGHT), 2L,
  !(f3_d %chin% INFLIGHT), 3L, !(f4_d %chin% INFLIGHT), 4L,
  !(f5_d %chin% INFLIGHT), 5L, !(f6_d %chin% INFLIGHT), 6L,
  default = NA_integer_)]
pick <- function(stem) fcase(
  d$olag == 1L, d[[paste0("f1_", stem)]], d$olag == 2L, d[[paste0("f2_", stem)]],
  d$olag == 3L, d[[paste0("f3_", stem)]], d$olag == 4L, d[[paste0("f4_", stem)]],
  d$olag == 5L, d[[paste0("f5_", stem)]], d$olag == 6L, d[[paste0("f6_", stem)]])
d[, `:=`(out_d = pick("d"), out_t = pick("t"), out_p = pick("p"),
         out_x = pick("x"), out_y = pick("y"))]

say("")
say("=== 1. IS THE OUTCOME RESOLVABLE? ===")
say("resolvable (an outcome row with a team): ",
    round(100 * mean(!is.na(d$out_t)), 1), "%")
say("  ... and with a named player too: ",
    round(100 * mean(!is.na(d$out_p)), 1), "%")
say("")
say("compare the aerial-only path, which resolves 235 contests per match against")
say("the ", round(nrow(d) / nm, 1), " disposals here.")
say("")
say("unresolved, by disposal type:")
say_dt(d[, .(n = .N, pct_unresolved = round(100 * mean(is.na(out_t)), 1)),
         by = description], 5)

r <- d[!is.na(out_t)]
r[, retained := out_t == team_id]
say("")
say("=== 2. IS THERE DIFFICULTY VARIATION TO MODEL? ===")
say("overall retain rate: ", round(100 * mean(r$retained), 1), "%")
say("")
say("by disposal type:")
say_dt(r[, .(n = .N, per_match = round(.N / nm, 1),
             retain_pct = round(100 * mean(retained), 1)), by = description], 5)

r[, kick_len := sqrt((out_x - x)^2 + (out_y - y)^2)]
r[, len_band := cut(kick_len, c(-Inf, 15, 25, 35, 45, 55, Inf),
                    labels = c("<15m", "15-25m", "25-35m", "35-45m", "45-55m", ">55m"))]
say("")
say("by length -- this is the difficulty gradient the flat 50/50 ignores:")
say_dt(r[description == "Kick" & !is.na(len_band),
         .(n = .N, per_match = round(.N / nm, 1),
           retain_pct = round(100 * mean(retained), 1)), by = len_band][order(len_band)], 8)

half <- as.numeric(quantile(abs(p$x), 0.995, na.rm = TRUE))
r[, zone := fcase(x > half - 50, "forward 50", x > 0, "att midfield",
                  x > -(half - 50), "def midfield", default = "defensive 50")]
say("")
say("by zone:")
say_dt(r[description == "Kick", .(n = .N, retain_pct = round(100 * mean(retained), 1)),
         by = zone][order(-n)], 5)

say("")
say("by outcome type -- what actually happens to a kick:")
say_dt(r[description == "Kick", .(n = .N, per_match = round(.N / nm, 1),
         retain_pct = round(100 * mean(retained), 1))
         , by = out_d][order(-n)][1:14], 14)

say("")
say("=== 3. HOW MUCH SPREAD IS THERE IN p? ===")
say("If retain rates ran 90-95% everywhere there would be no difficulty signal")
say("worth modelling. The range across length bands and zones above is the")
say("answer; a spread of tens of points means the flat 50/50 is discarding a")
say("great deal.")
bands <- r[description == "Kick" & !is.na(len_band),
           .(retain = mean(retained)), by = .(len_band, zone)]
say("retain rate across length x zone cells: min ", round(100 * min(bands$retain), 1),
    "%  median ", round(100 * median(bands$retain), 1),
    "%  max ", round(100 * max(bands$retain), 1), "%")

say("")
say("=== VERDICT INPUTS ===")
say("The generalisation is worth building if (1) the outcome resolves for the")
say("large majority of disposals and (2) the retain rate varies widely across")
say("the conditions a model can see. Both are necessary; neither alone is")
say("enough, and a high resolve rate with a flat retain rate would mean there")
say("is nothing for difficulty weighting to do.")

close(con)
cat("\nDone\n")
