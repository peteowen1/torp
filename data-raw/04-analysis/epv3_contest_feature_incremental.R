# Does contest WAE add anything the match model does not already have?
#
# The uncontrolled screen gave d_roll_wae t = 14.93 and R2 = 0.187 from three
# features. That is not evidence of value: a good team wins more contests than
# expected, so WAE plausibly re-measures team quality that elo and the player
# ratings already carry. The whole session's lesson is that the match model
# absorbs anything it already knows.
#
# THE TEST: fit margin on what the model already has, then add the contest
# features and ask whether R2 moves and whether the coefficients survive. If
# WAE dies once epr/psr/elo are present, it is a proxy and the gate is not worth
# running. If it survives, it is new information and the gate is justified.
#
# This costs ~3 minutes against the gate's ~40, and a null here is conclusive.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
OUT <- file.path(OUT_DIR, "epv3_contest_incremental.txt")
con <- file(OUT, open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con) }
say_dt <- function(x, n = 40) for (l in capture.output(print(utils::head(x, n)))) say(l)

say("=== Is contest WAE incremental, or a proxy for team quality? ===")

teams <- load_teams(TRUE)
psr_df <- tryCatch(.compute_psr_from_stat_ratings(load_player_stat_ratings(TRUE)),
                   error = function(e) NULL)
rt <- as.data.frame(arrow::read_parquet(file.path(OUT_DIR, "epv3_ratings_v2.parquet")))

# Production team features, via the production builder.
tr <- as.data.table(.build_team_ratings_df(teams, rt, psr_df))
say("team-match rating rows: ", nrow(tr))

cf <- as.data.table(arrow::read_parquet(file.path(OUT_DIR, "epv3_contest_team_features.parquet")))
tr <- merge(tr, cf[, .(match_id, team_id, roll_wae, roll_stakes, roll_vol)],
            by = c("match_id", "team_id"), all.x = TRUE)

RC <- c("epr", "epr_recv", "epr_disp", "epr_spoil", "epr_hitout", "psr",
        "roll_wae", "roll_stakes", "roll_vol")
RC <- intersect(RC, names(tr))
h <- tr[team_type == "home"]; a <- tr[team_type == "away"]
m <- merge(h[, c("match_id", RC), with = FALSE], a[, c("match_id", RC), with = FALSE],
           by = "match_id", suffixes = c("_h", "_a"))
for (v in RC) m[, (paste0("d_", v)) := get(paste0(v, "_h")) - get(paste0(v, "_a"))]

res <- as.data.table(load_results(TRUE))
m <- merge(m, res[, .(match_id = as.character(match_id),
                      margin = home_score - away_score)], by = "match_id")
m <- m[is.finite(margin)]

BASE <- paste0("d_", intersect(c("epr", "epr_recv", "epr_disp", "epr_spoil",
                                 "epr_hitout", "psr"), RC))
NEW  <- paste0("d_", intersect(c("roll_wae", "roll_stakes", "roll_vol"), RC))
m <- m[complete.cases(m[, c(BASE, NEW), with = FALSE])]
say("matches usable: ", nrow(m))
say("baseline features: ", paste(BASE, collapse = ", "))
say("added features:    ", paste(NEW, collapse = ", "))

f0 <- lm(as.formula(paste("margin ~", paste(BASE, collapse = " + "))), data = m)
f1 <- lm(as.formula(paste("margin ~", paste(c(BASE, NEW), collapse = " + "))), data = m)

say("")
say("=== R2 ===")
say("baseline (ratings only)      ", round(summary(f0)$r.squared, 5))
say("baseline + contest features  ", round(summary(f1)$r.squared, 5))
say("delta                        ", round(summary(f1)$r.squared - summary(f0)$r.squared, 5))
say("")
say("MAE baseline ", round(mean(abs(residuals(f0))), 4),
    " | with contest ", round(mean(abs(residuals(f1))), 4),
    " | delta ", round(mean(abs(residuals(f1))) - mean(abs(residuals(f0))), 4))
an <- anova(f0, f1)
say("anova F ", round(an$F[2], 2), "  p ", signif(an$`Pr(>F)`[2], 4))

say("")
say("=== coefficients in the FULL model (do the contest features survive?) ===")
co <- summary(f1)$coefficients
say_dt(as.data.table(round(co, 4), keep.rownames = "feature")[
  feature %in% c("(Intercept)", BASE, NEW)], 20)

say("")
say("=== is WAE just team quality? correlate it with the ratings ===")
cm <- cor(m[, c("d_roll_wae", BASE), with = FALSE], use = "complete.obs")
say_dt(as.data.table(round(cm["d_roll_wae", , drop = FALSE], 3),
                     keep.rownames = "vs"), 4)
say("")
say("A high correlation with d_epr would mean WAE is mostly re-measuring the")
say("same team strength. A low one, with the coefficient surviving, means it is")
say("carrying something the ratings do not.")

say("")
say("=== VERDICT ===")
dR2 <- summary(f1)$r.squared - summary(f0)$r.squared
wae_t <- if ("d_roll_wae" %in% rownames(co)) co["d_roll_wae", 3] else NA
say("delta R2 ", round(dR2, 5), " | d_roll_wae t = ", round(wae_t, 2),
    " | anova p ", signif(an$`Pr(>F)`[2], 3))
if (is.finite(wae_t) && abs(wae_t) > 3 && dR2 > 0.005) {
  say("INCREMENTAL. The contest features carry information the ratings do not.")
  say("The match gate is justified.")
} else {
  say("NOT INCREMENTAL. WAE is largely a proxy for what the ratings already")
  say("measure. Do not spend the gate; the uncontrolled screen was misleading.")
}

close(con)
cat("\nDone\n")
