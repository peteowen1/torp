# Is the contest population defined wrong? (issues #209, #210)
# =============================================================================
# Pete: "Isn't a contest defined by if there are two rows of players in the
# chains? I think you're defining it wrong and that could be causing the issues."
#
# The vocabulary check says he is substantively right. There is no play type
# literally called "Marking Contest", but chains DOES record a contest
# structurally: a `Contest Target` row naming the intended player, then the
# resolving row at the SAME coordinates naming someone else (90.7% of such
# adjacent same-coordinate pairs are opposing teams).
#
#   Contest Target -> Spoil            1,486
#   Contest Target -> Contested Mark     476
#
# EPV3_CONTEST_TARGET_DESCS already exists, and build_aerial_contests() already
# scans for it -- but only to NAME THE BEATEN TARGET (`target_pid`), never to
# define the population. The population comes from EPV3_AERIAL_OUT, i.e. from
# the OUTCOME description.
#
# Counts that make this urgent: 4,423 Contest Target rows in all of 2026,
# against a contest population of 50,050 of which 69.8% are receptions
# (Uncontested Mark, Mark On Lead).
#
# THE HYPOTHESIS THIS TESTS: the population mixes uncontested receptions with
# genuine aerial duels, and `def_win` is close to determined by WHICH KIND a row
# is. That would explain both unexplained findings at once -- every surviving
# spoil being def_win, and the overconfidence concentrated at 0.8-0.99 -- without
# any leak, because the model is chasing a category its features cannot see.
#
# Verdict rule, written first: if def_win rate differs sharply between rows WITH
# a Contest Target and rows WITHOUT, the population is heterogeneous and the
# model is being asked one question about two different events.
#
#   powershell.exe -Command 'Rscript "data-raw/04-analysis/np_contest_population_vs_chains.R"'
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
options(torp.local_data_dir = NA)
say <- function(...) cat(..., "\n", sep = "")

SEASON <- 2026
pbp <- as.data.table(load_pbp(SEASON)); pbp[, match_id := as.character(match_id)]
ch  <- as.data.table(load_chains(SEASON)); ch[, match_id := as.character(match_id)]

cst <- as.data.table(build_aerial_contests(ch, pbp))
cst[, `:=`(y = as.integer(def_win),
           has_target = !is.na(target_pid),
           otype = fcase(grepl("^Spoil", out_desc), "spoil",
                         out_desc %chin% c("Contested Mark", "Pack Mark (P)", "Pack Mark (O)"),
                           "contested_mark",
                         default = "reception"))]
say("contest population as built: ", format(nrow(cst), big.mark = ","))
say("Contest Target rows in all of chains ", SEASON, ": ",
    format(ch[description %chin% EPV3_CONTEST_TARGET_DESCS, .N], big.mark = ","))

say("\n=== how many of the population have a CHAINS-RECORDED contest? ===")
say("has_target = a Contest Target row was logged in the in-flight span.")
print(cst[, .(n = .N, pct = round(100 * .N / nrow(cst), 1),
              def_win_pct = round(100 * mean(y), 1)), by = has_target][order(-n)])

say("\n=== crossed with outcome type -- this is the decisive table ===")
say("If the population were homogeneous, def_win_pct would be similar across")
say("cells. n is the cell size; def_win_pct is how often the defence won.")
print(dcast(cst[, .(n = .N, dw = round(100 * mean(y), 1)), by = .(otype, has_target)],
            otype ~ has_target, value.var = c("n", "dw")))

say("\n=== what the model is actually being asked ===")
say("Overall def_win rate       : ", round(100 * mean(cst$y), 1), "%")
say("  among has_target rows    : ", round(100 * cst[has_target == TRUE, mean(y)], 1), "%")
say("  among rows without       : ", round(100 * cst[has_target == FALSE, mean(y)], 1), "%")
say("\nAnd the receptions, which are 69.8% of the population:")
say("  reception, no target     : n ", cst[otype == "reception" & !has_target, .N],
    "  def_win ", round(100 * cst[otype == "reception" & !has_target, mean(y)], 1), "%")
say("  reception, with target   : n ", cst[otype == "reception" & has_target, .N],
    "  def_win ", round(100 * cst[otype == "reception" & has_target, mean(y)], 1), "%")

say("\n=== what a chains-defined population would look like ===")
say("Rows where chains actually logged a contest (has_target), or the outcome is")
say("an unambiguous duel (contested/pack mark, spoil):")
duel <- cst[has_target == TRUE | otype %chin% c("spoil", "contested_mark")]
say("  n ", format(nrow(duel), big.mark = ","),
    " (", round(100 * nrow(duel) / nrow(cst), 1), "% of the current population)")
say("  def_win rate ", round(100 * mean(duel$y), 1), "%")
say("\nand the rows that would be DROPPED (receptions with no logged contest):")
drop <- cst[has_target == FALSE & otype == "reception"]
say("  n ", format(nrow(drop), big.mark = ","),
    " (", round(100 * nrow(drop) / nrow(cst), 1), "%)")
say("  def_win rate ", round(100 * mean(drop$y), 1), "%")
say("  these are kicks that simply found a teammate -- a reception, not a duel")

say("\n=== VERDICT ===")
a <- cst[has_target == TRUE, mean(y)]; b <- cst[has_target == FALSE, mean(y)]
say("  def_win with a logged contest: ", round(100 * a, 1),
    "%   without: ", round(100 * b, 1), "%   gap: ", round(100 * abs(a - b), 1), " points")
if (abs(a - b) > 0.15) {
  say("\n  POPULATION IS HETEROGENEOUS. The model is being asked one question")
  say("  about two different events, and which event a row is nearly determines")
  say("  the answer. That is a specification problem upstream of calibration --")
  say("  fixing the population may dissolve the overconfidence without any")
  say("  recalibration at all. Pete's instinct was right.")
} else {
  say("\n  Population looks homogeneous on this split; the calibration problem is")
  say("  not explained by the reception/duel mix.")
}
