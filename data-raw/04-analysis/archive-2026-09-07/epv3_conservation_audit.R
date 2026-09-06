# Where does conservation break, and is there a conserving quantity in here?
#
# Total team EPV converts to margin at 0.556-0.595 rather than 1.0, with
# team-match sd 57-68 against margin's 39.85 -- EPV runs ~1.75x the actual game.
# That measurement used `epv_*_adj`, which by then has been through FOUR
# transformations, every one of them designed for RATING purposes and every one
# of them capable of breaking conservation:
#
#   1. per-80 normalisation      value / tog
#   2. position adjust           (p80 - mean_role)/sd_role * pooled_sd * tog
#   3. opponent adjustment       non-linear, uses sum of abs() across channels
#   4. level centring per round  subtracts a positional mean
#
# So the interesting question is not "does epv_adj conserve" -- it obviously
# cannot -- but whether the RAW `epv` does, and if not, where it goes wrong.
# If raw conserves and adjusted does not, the fix is structural rather than
# numerical: keep a conserving descriptive quantity and let the rating layer
# transform a COPY.
#
# Also checks the theoretical ceiling. Each PBP row's delta_epv is split
# disp_scale + recv_scale = 1.0, so the credited total should equal the sum of
# delta_epv over the match. Whether THAT equals the margin is a separate
# question about the EP model, not about the credit split, and the two need
# separating before anything is "fixed".
#
# ~3 min, cached frames only.

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_conservation_audit.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = con); flush(con) }
say_dt <- function(x, n = 45) for (l in capture.output(print(utils::head(x, n)))) say(l)

res <- as.data.table(load_results(TRUE))
tgt <- res[, .(match_id = as.character(match_id), home = home_team_name,
               away = away_team_name, margin = home_score - away_score,
               total_score = home_score + away_score)][is.finite(margin)]

d <- as.data.table(read_parquet(file.path(OUT_DIR, "epv3_fin_pgd_ship.parquet")))
say("=== Where does conservation break? ===")
say("run at ", format(Sys.time()))
say("player-games ", format(nrow(d), big.mark = ","))

conv <- function(dt, cols, label) {
  ts <- dt[, .(v = sum(Reduce(`+`, lapply(cols, function(c) get(c))), na.rm = TRUE)),
           by = .(match_id, team)]
  h <- merge(tgt, ts, by.x = c("match_id", "home"), by.y = c("match_id", "team"))
  a <- merge(tgt, ts, by.x = c("match_id", "away"), by.y = c("match_id", "team"))
  m <- merge(h[, .(match_id, margin, v_h = v)], a[, .(match_id, v_a = v)], by = "match_id")
  m[, dv := v_h - v_a]
  f <- lm(margin ~ 0 + dv, data = m)
  co <- summary(f)$coefficients
  data.table(quantity = label, n = nrow(m),
             conversion = round(co[1, 1], 4), t = round(co[1, 3], 1),
             r2 = round(summary(f)$r.squared, 3),
             sd_diff = round(sd(m$dv), 2),
             mean_team_total = round(mean(c(m$v_h, m$v_a)), 2))
}

RAW <- c("epv_recv", "epv_disp", "epv_spoil", "epv_hitout")
ADJ <- c("epv_recv_adj", "epv_disp_adj", "epv_spoil_adj", "epv_hitout_adj")

say("")
say("=== 1. RAW against ADJUSTED ===")
say("margin sd is 39.85 -- a conserving quantity should have a similar spread")
say("and a conversion of 1.000.")
rows <- rbindlist(list(
  conv(d, RAW, "raw epv (sum of channels)"),
  conv(d, ADJ, "epv_adj (what EPR consumes)")
))
if ("epv" %in% names(d))     rows <- rbind(rows, conv(d, "epv", "raw epv column"))
if ("epv_adj" %in% names(d)) rows <- rbind(rows, conv(d, "epv_adj", "epv_adj column"))
say_dt(rows, 6)

say("")
say("=== 2. PEELING THE TRANSFORMATIONS BACK ===")
say("Rebuild the adjustment chain step by step from the raw channels and watch")
say("where the conversion moves away from the raw value.")
d[, tog := pmax(dplyr::coalesce(time_on_ground_percentage / 100, 0.1), 0.1)]
wsd <- function(x, w) { ok <- !is.na(x) & !is.na(w); m <- weighted.mean(x[ok], w[ok])
                        sqrt(sum(w[ok] * (x[ok] - m)^2) / sum(w[ok])) }

steps <- list()
steps[["raw"]] <- copy(d[, c("match_id", "team", RAW), with = FALSE])

# step 1+2: per-80 then position adjust, exactly as .position_adjust does
p2 <- copy(d)
for (ch in c("recv", "disp", "spoil")) {
  v <- paste0("epv_", ch); p80 <- p2[[v]] / p2$tog
  p2[, .p := p80]
  S <- wsd(p80, p2$tog)
  p2[, (paste0(v, "_s2")) := {
    m <- weighted.mean(.p, tog, na.rm = TRUE); s <- wsd(.p, tog)
    if (is.na(s) || s < 1e-6) (.p - m) * tog else (.p - m) / s * S * tog
  }, by = lineup_position]
}
p2[, epv_hitout_s2 := 0]
steps[["+ per-80 & position standardise"]] <-
  p2[, c("match_id", "team", paste0(RAW, "_s2")), with = FALSE]

say("")
r2rows <- rbindlist(list(
  conv(steps[["raw"]], RAW, "raw"),
  conv(steps[["+ per-80 & position standardise"]], paste0(RAW, "_s2"),
       "+ per-80 & position standardise")
))
# the shipped frame already carries the opponent-adjusted + centred version
r2rows <- rbind(r2rows, conv(d, ADJ, "+ opponent adj + level centring (= _adj)"))
say_dt(r2rows, 5)

say("")
say("=== 3. IS THE EP MODEL ITSELF THE CEILING? ===")
say("Each row's delta_epv is split disp_scale + recv_scale = 1.0, so the credited")
say("total should equal sum(delta_epv). Whether THAT equals the margin is a")
say("question about the EP model, not the credit split.")
full <- as.data.table(load_pbp(TRUE))
pp <- data.table(match_id = full$match_id, delta_epv = full$delta_epv,
                 pos_team = full$pos_team, team = full$team)
rm(full); invisible(gc())
pp <- pp[is.finite(delta_epv)]
if ("team" %in% names(pp) && !all(is.na(pp$team))) {
  ts <- pp[, .(v = sum(delta_epv * pos_team, na.rm = TRUE)), by = .(match_id, team)]
  h <- merge(tgt, ts, by.x = c("match_id", "home"), by.y = c("match_id", "team"))
  a <- merge(tgt, ts, by.x = c("match_id", "away"), by.y = c("match_id", "team"))
  m <- merge(h[, .(match_id, margin, v_h = v)], a[, .(match_id, v_a = v)], by = "match_id")
  m[, dv := v_h - v_a]
  f <- lm(margin ~ 0 + dv, data = m)
  say(sprintf("  sum(delta_epv * pos_team) by team -> margin: %.4f  (t %.1f, R2 %.3f)",
              coef(f)[[1]], summary(f)$coefficients[1, 3], summary(f)$r.squared))
  say(sprintf("  its team-difference sd %.2f against margin sd %.2f",
              sd(m$dv), sd(m$margin)))
  say("")
  say("If this is already far from 1.0, the credit split is not the problem --")
  say("the EP model's deltas do not sum to the scoreboard, and conservation has")
  say("to be imposed at the top rather than recovered by fixing the split.")
} else {
  say("  pbp has no usable team column here; skipped")
}

say("")
say("=== WHAT THIS DECIDES ===")
say("If RAW conserves and ADJ does not, the fix is structural: keep a conserving")
say("descriptive quantity and let the rating layer transform a COPY. If NEITHER")
say("conserves, the scale error is upstream in the EP deltas and a single")
say("normalisation at the match level is the honest fix -- rescale each match's")
say("credits so they sum to that match's margin, which is exactly what Net")
say("Points does by construction.")

close(con)
cat("\nDone\n")
