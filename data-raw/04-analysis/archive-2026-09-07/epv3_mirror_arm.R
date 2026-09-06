# Does the positional-matchup allocation beat the flat smear?
#
# The flat rule costs the contest channel 0.924 -> 0.384 in conversion to
# margin. "Don't allocate" recovers the conversion but drops ~69% of the debits,
# so a player can barely lose contest value and the channel stops conserving.
# Neither is right. The mirror rule aims to conserve AND convert.
#
# Four arms on the duel population, so the population fix is held constant and
# only the allocation moves:
#   duel_team    flat across all 22          (the current default)
#   duel_none    unnamed debits dropped      (the conversion ceiling)
#   duel_mirror  positional matchup weights  (the candidate)
#   duel_ledger  the AFL one-on-one ledger   (re-tested on duels)
#
# WHAT WOULD COUNT. The mirror should sit near `none` on conversion while
# conserving like `team`. If it lands back near `team`, the positional
# information is not enough to overcome the smearing and the honest conclusion
# is that unnamed debits cannot be allocated well from this data.
#
# ~20 min. Run detached.

suppressMessages({
  library(dplyr); library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})
OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_mirror_arm.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m,"\n",sep=""); cat(m,"\n",sep="",file=con); flush(con) }
say_dt <- function(x, n = 45) for (l in capture.output(print(utils::head(x, n)))) say(l)
set_const <- function(l) for (nm in names(l)) assignInNamespace(nm, l[[nm]], ns = "torp")

pbp <- load_pbp(TRUE); stats_ <- load_player_stats(TRUE)
teams <- load_teams(TRUE); chains <- load_chains(TRUE)
res <- as.data.table(load_results(TRUE))
tgt <- res[, .(match_id = as.character(match_id), home = home_team_name,
               away = away_team_name, margin = home_score - away_score)][is.finite(margin)]

CH <- c("epv_recv", "epv_disp", "epv_spoil")
say("=== Mirror allocation against the alternatives ===")
say("run at ", format(Sys.time()))

conv <- function(d, label) {
  ts <- d[, lapply(.SD, sum, na.rm = TRUE), .SDcols = CH, by = .(match_id, team)]
  h <- merge(tgt, ts, by.x = c("match_id","home"), by.y = c("match_id","team"))
  a <- merge(tgt, ts, by.x = c("match_id","away"), by.y = c("match_id","team"))
  m <- merge(h[, c("match_id","margin",CH), with=FALSE], a[, c("match_id",CH), with=FALSE],
             by = "match_id", suffixes = c("_h","_a"))
  for (v in CH) m[, (paste0("d_",v)) := get(paste0(v,"_h")) - get(paste0(v,"_a"))]
  m[, d_tot := Reduce(`+`, lapply(CH, function(v) get(paste0("d_",v))))]
  co <- summary(lm(as.formula(paste("margin ~ 0 +", paste0("d_",CH,collapse=" + "))), data=m))$coefficients
  sdv <- vapply(CH, function(v) sd(m[[paste0("d_",v)]]), numeric(1))
  tot <- summary(lm(margin ~ 0 + d_tot, data = m))
  say(""); say("=== ", label, " ===")
  say_dt(data.table(channel = c("recv","disp","contest"),
                    conversion = round(co[,1],3), t = round(co[,3],1),
                    share_raw_pct = round(100*sdv^2/sum(sdv^2),1)), 4)
  say(sprintf("  TOTAL -> margin %.4f (t %.1f, R2 %.3f)", tot$coefficients[1,1],
              tot$coefficients[1,3], tot$r.squared))
  say(sprintf("  contest: sd %.3f  mean %+.4f  cor(contested_marks) %.3f",
              sd(d$epv_spoil, na.rm=TRUE), mean(d$epv_spoil, na.rm=TRUE),
              cor(d$epv_spoil, d$contested_marks, use="complete.obs")))
  data.table(arm = label, conv_cont = round(co[3,1],3), t_cont = round(co[3,3],1),
             conv_total = round(tot$coefficients[1,1],3), r2 = round(tot$r.squared,3),
             share_cont = round(100*sdv[3]^2/sum(sdv^2),1),
             cor_cm = round(cor(d$epv_spoil, d$contested_marks, use="complete.obs"),3))
}

ARMS <- list(c("duel_team","team"), c("duel_none","none"),
             c("duel_mirror","mirror"), c("duel_ledger","ledger"))
rows <- list()
for (a in ARMS) {
  tag <- a[1]; alloc <- a[2]
  cli::cli_h1(tag)
  set_const(list(EPV3_CONTEST_POPULATION = "duel", EPV_CONT_LOSS_ALLOC = alloc,
                 EPV3_CHANNELS = 3L, EPV_STANDARDISE_CHANNELS = c("recv","disp"),
                 EPV3_STOP_ZERO_SUM = TRUE))
  f <- file.path(OUT_DIR, paste0("epv3_duel_pgd_", tag, ".parquet"))
  if (file.exists(f)) { cli::cli_alert_info("reuse {tag}"); d <- as.data.table(read_parquet(f))
  } else {
    d <- as.data.table(create_player_game_data(pbp, stats_, teams, chains, epv_engine = "v3"))
    write_parquet(d, f)
  }
  rows[[tag]] <- conv(d, tag)
}
say(""); say("=== SIDE BY SIDE ===")
say_dt(rbindlist(rows), 6)
say("")
say("The mirror earns its place only if it conserves like `team` AND converts")
say("near `none`. Landing near `team` would say the positional information is")
say("not enough, and that unnamed debits cannot be allocated well from this data.")
saveRDS(rbindlist(rows), file.path(OUT_DIR, "epv3_mirror_arm.rds"))
say(""); say("done ", format(Sys.time()))
close(con); cat("\nDone\n")
