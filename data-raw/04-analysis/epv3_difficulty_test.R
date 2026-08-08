# Does difficulty-weighted credit beat the flat 50/50?
#
# Standalone: builds the disposal credit directly and evaluates it at the team
# level, without touching create_player_game_data(). If it does not conserve or
# does not separate, there is no point wiring it into the pipeline.
#
# What matters:
#   conservation   the credited total should still equal the margin. The
#                  identity guarantees it algebraically; this checks the fitted
#                  models did not break it.
#   separation     does the split now vary with difficulty as intended -- an easy
#                  handball paying its receiver near zero, a long contested kick
#                  paying a lot.
#   who moves      which positions gain and lose. Kick-and-mark defenders should
#                  lose (their receptions were easy); contested receivers gain.
#
# ~15 min: 876k disposals, three GAMs, leak-safe per season. Run detached.

suppressMessages({ library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE) })
OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
con <- file(file.path(OUT_DIR, "epv3_difficulty_test.txt"), open = "wt")
say <- function(...) { m <- paste0(...); cat(m,"\n",sep=""); cat(m,"\n",sep="",file=con); flush(con) }
say_dt <- function(x, n=45) for (l in capture.output(print(utils::head(x,n)))) say(l)

say("=== Difficulty-weighted credit on every disposal ==="); say("run at ", format(Sys.time()))
pbp <- load_pbp(TRUE); chains <- load_chains(TRUE)
res <- as.data.table(load_results(TRUE))
tgt <- res[, .(match_id=as.character(match_id), home=home_team_name,
               away=away_team_name, margin=home_score-away_score)][is.finite(margin)]

f <- file.path(OUT_DIR, "epv3_difficulty_credit.parquet")
if (file.exists(f)) { say("reusing cached credit"); cr <- as.data.table(read_parquet(f))
  sc <- as.data.table(read_parquet(file.path(OUT_DIR, "epv3_difficulty_scored.parquet")))
} else {
  cr <- compute_difficulty_credit(chains, pbp)
  sc <- as.data.table(attr(cr, "scored"))
  write_parquet(cr, f); write_parquet(sc, file.path(OUT_DIR, "epv3_difficulty_scored.parquet"))
}
say("player-games ", format(nrow(cr), big.mark=","), " | disposals scored ",
    format(nrow(sc), big.mark=","))

say(""); say("=== 1. DOES THE SPLIT VARY WITH DIFFICULTY? ===")
sc[, len_band := cut(kick_len, c(-Inf,15,25,35,45,55,Inf),
     labels=c("<15m","15-25m","25-35m","35-45m","45-55m",">55m"))]
say("mean credit by kick length -- the flat rule gives 0.5*delta to BOTH sides")
say("in every band; these should now diverge:")
say_dt(sc[description=="Kick" & !is.na(len_band),
   .(n=.N, p_turnover=round(mean(p_hat),3),
     disposer=round(mean(disp_credit),4),
     receiver=round(mean(recv_credit),4)), by=len_band][order(len_band)], 8)
say(""); say("by disposal type:")
say_dt(sc[, .(n=.N, p_turnover=round(mean(p_hat),3),
   disposer=round(mean(disp_credit),4), receiver=round(mean(recv_credit),4)),
   by=description], 5)
say(""); say("receiver credit by whether it was retained -- a completed HARD")
say("disposal should pay the receiver most:")
say_dt(sc[description=="Kick" & !is.na(len_band),
   .(receiver_credit=round(mean(recv_credit),4)), by=.(len_band, turnover)][order(len_band, turnover)], 14)

say(""); say("=== 2. DOES IT STILL CONSERVE? ===")
pl <- copy(cr)[, tot := epv_disp_diff + epv_recv_diff]
tm <- as.data.table(read_parquet(file.path(OUT_DIR,"epv3_fin_pgd_ship.parquet")))[
  , .(match_id, player_id, team)]
pl <- merge(pl, tm, by=c("match_id","player_id"))
ts <- pl[, .(v=sum(tot, na.rm=TRUE), d=sum(epv_disp_diff), r=sum(epv_recv_diff)),
         by=.(match_id, team)]
h <- merge(tgt, ts, by.x=c("match_id","home"), by.y=c("match_id","team"))
a <- merge(tgt, ts, by.x=c("match_id","away"), by.y=c("match_id","team"))
m <- merge(h[, .(match_id, margin, vh=v, dh=d, rh=r)], a[, .(match_id, va=v, da=d, ra=r)], by="match_id")
m[, `:=`(dv=vh-va, dd=dh-da, dr=rh-ra)]
ft <- summary(lm(margin ~ 0 + dv, data=m))
say(sprintf("  TOTAL -> margin %.4f (t %.1f, R2 %.3f)  sd %.1f vs margin %.1f",
    ft$coefficients[1,1], ft$coefficients[1,3], ft$r.squared, sd(m$dv), sd(m$margin)))
f2 <- summary(lm(margin ~ 0 + dd + dr, data=m))
say("  per channel:")
say_dt(data.table(channel=c("disposal","reception"),
   conversion=round(f2$coefficients[,1],3), t=round(f2$coefficients[,3],1),
   sd=round(c(sd(m$dd), sd(m$dr)),2),
   share_pct=round(100*c(sd(m$dd)*f2$coefficients[1,1], sd(m$dr)*f2$coefficients[2,1])^2 /
     sum(c(sd(m$dd)*f2$coefficients[1,1], sd(m$dr)*f2$coefficients[2,1])^2),1)), 3)

say(""); say("=== 3. WHO MOVES? ===")
old <- as.data.table(read_parquet(file.path(OUT_DIR,"epv3_fin_pgd_ship.parquet")))
cmp <- merge(old[, .(match_id, player_id, player_name, position_group,
                     old_disp=epv_disp, old_recv=epv_recv)],
             cr, by=c("match_id","player_id"))
cmp[, `:=`(d_disp=epv_disp_diff-old_disp, d_recv=epv_recv_diff-old_recv)]
say("mean change per player-game, by position:")
say_dt(cmp[!is.na(position_group), .(n=.N,
   disp_change=round(mean(d_disp, na.rm=TRUE),3),
   recv_change=round(mean(d_recv, na.rm=TRUE),3)), by=position_group][order(-recv_change)], 8)
say(""); say("Kick-and-mark players should LOSE reception credit -- their marks")
say("were easy and the flat rule paid them half regardless.")

saveRDS(list(total=ft$coefficients[1,1], r2=ft$r.squared), file.path(OUT_DIR,"epv3_difficulty_test.rds"))
say(""); say("done ", format(Sys.time())); close(con); cat("\nDone\n")
