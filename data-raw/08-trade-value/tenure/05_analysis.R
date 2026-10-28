library(data.table); library(survival)
S <- "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-torpverse-torp/fd72fa3a-4884-46e7-9859-8c01702f8ead/scratchpad/tenure"
fs <- list.files("C:/dev/torpverse/torpdata/data/raw/tenure", full.names=TRUE)
d <- rbindlist(lapply(fs, fread, select=c("Season","Round","Playing.for","ID","First.name","Surname","Age")), fill=TRUE)
d <- unique(d, by=c("ID","First.name","Surname","Season","Round","Playing.for"))   # drops 2010 GF replay dups
d[, name := paste(First.name, Surname)]
# fill missing IDs (2025/26 rows lack ID) from unique name->ID, else name key
nid <- unique(d[!is.na(ID), .(name, ID)])[, if (.N==1) .SD, by=name]
d[is.na(ID), ID := nid$ID[match(name, nid$name)]]
cat("rows still no ID after name lookup:", d[is.na(ID), .N], "\n")
d[is.na(ID), ID := -as.integer(factor(name))]
franch <- c("Brisbane Bears"="Brisbane","Brisbane Lions"="Brisbane","Fitzroy"="Brisbane","Kangaroos"="North Melbourne","Footscray"="Western Bulldogs","Greater Western Sydney"="GWS")
d[, club := fifelse(Playing.for %in% names(franch), franch[Playing.for], Playing.for)]
rk <- function(r){n<-suppressWarnings(as.integer(r)); fifelse(is.na(n),100L+match(r,c("Wildcard Final","EF","QF","SF","PF","GF")),n)}
d[, rr := rk(Round)]
ps <- d[, .(games=.N, first_rr=min(rr), age=min(Age,na.rm=TRUE), name=name[1]), by=.(ID,Season,club)]
setorder(ps, ID, Season, first_rr)
ps[, stint := cumsum(club != shift(club, fill=club[1])) + 1L, by=ID]
st <- ps[, .(name=name[1], club=club[1], first=min(Season), last=max(Season), n_seasons=uniqueN(Season), games=sum(games), age_arr=age[1]), by=.(ID,stint)]
st[, `:=`(span=last-first+1L, nstint=.N), by=ID]
st[, `:=`(prev_last=shift(last), prev_club=shift(club)), by=ID]
st[, outcome := fifelse(stint<nstint,"moved",fifelse(last==2026,"still_there","left_league"))]
st[, gap := first-prev_last-1L]
st[, event := as.integer(outcome != "still_there")]
fwrite(st, file.path(S,"stints.csv"))
cat("players:", uniqueN(st$ID), "stints:", nrow(st), "\n")

kmrow <- function(x, label) {
  f <- survfit(Surv(span, event) ~ 1, data=x)
  med <- summary(f)$table["median"]
  s <- summary(f, times=c(2,4), extend=TRUE)$surv   # P(span>=3), P(span>=5)
  data.table(group=label, n=nrow(x), moved=sum(x$outcome=="moved"), left=sum(x$outcome=="left_league"), still=sum(x$outcome=="still_there"),
    obs_median_span=median(x$span), obs_median_seasons_played=median(x$n_seasons), KM_median_span=med, P_stay_3plus=round(s[1],3), P_stay_5plus=round(s[2],3))
}
# ---- MOVERS
mv <- st[stint>1 & first>=2000 & first<=2020]
mv[, ageband := cut(age_arr, c(-Inf,24,28,31,Inf), right=FALSE, labels=c("under24","24-27","28-30","31plus"))]
cat("\n== MOVERS arrivals 2000-2020: n=", nrow(mv), " NA age:", sum(is.na(mv$age_arr)), "\n")
res <- rbindlist(c(list(kmrow(mv,"all")), lapply(levels(mv$ageband), function(a) kmrow(mv[ageband==a], a)),
  list(kmrow(mv[gap==0],"gap0 (no missed season)"), kmrow(mv[gap>0],"gap1plus"))))
print(res); fwrite(res, file.path(S,"movers_table.csv"))
cat("arrivals 2000-2020 with last season 2025 and left (could be injured, not out):", mv[outcome=="left_league" & last==2025, .N], "\n")
cat("2021-2025 arrivals (censored cohort) n=", st[stint>1 & first>=2021 & first<=2025,.N], "\n")

# ---- DRAFT
fd <- fread("C:/dev/torpverse/torpdata/data/raw/footywire_drafts.csv")
fmap <- c("Brisbane"="Brisbane","Fitzroy"="Brisbane","GWS"="GWS")
fd[, club := fifelse(drafted_by %in% names(fmap), fmap[drafted_by], drafted_by)]
nz <- function(x) gsub("[^a-z]","", tolower(iconv(x, to="ASCII//TRANSLIT")))
fd <- fd[is_pass==FALSE]; fd[, pname := trimws(player)]
cat("\ndraft rows (non-pass):", nrow(fd), " years", range(fd$draft_year), " any NA pick:", sum(is.na(fd$pick)), "\n")
pl <- st[, .(first_career=min(first), first_club=club[which.min(first)], clubs=list(unique(club)), name=name[1]), by=ID]
pl[, nn := nz(name)]
pl[, surn := nz(sub("^\\S+\\s+","",name))]; pl[, firstn := nz(sub("\\s.*$","",name))]
fd[, `:=`(nn=nz(pname), surn=nz(sub("^\\S+\\s+","",pname)), firstn=nz(sub("\\s.*$","",pname)))]
fd[, did := .I]; fd[, ID := NA_integer_]; fd[, stage := NA_character_]
used <- integer(0)
try_stage <- function(stg, cond, relax=FALSE) {
  for (i in fd[is.na(ID), did]) {
    r <- fd[i]
    if (!relax) cand <- pl[cond(pl, r) & first_career >= r$draft_year+1 & !(ID %in% used)]
    else {   # re-drafted / previously listed: career began earlier, but he has a stint at the drafting club starting after the draft
      cand <- pl[cond(pl, r) & !(ID %in% used)]
      ok <- vapply(cand$ID, function(z) st[ID==z & club==r$club & first >= r$draft_year+1, .N] > 0, TRUE)
      cand <- cand[ok]; if (nrow(cand)) cand[, first_career := 0L]
    }
    if (nrow(cand)) { cand <- cand[order(first_career)][1]; fd[i, `:=`(ID=cand$ID, stage=stg)]; used <<- c(used, cand$ID) }
  }
}
pre <- function(p, r) startsWith(p$firstn, substr(r$firstn,1,3)) | startsWith(r$firstn, substr(p$firstn,1,3))
try_stage("1 name+first club", function(p,r) p$nn==r$nn & p$first_club==r$club)
try_stage("2 name+played for club", function(p,r) p$nn==r$nn & vapply(p$clubs, function(z) r$club %in% z, TRUE))
try_stage("3 name only", function(p,r) p$nn==r$nn)
try_stage("4 surname+first-name prefix+first club", function(p,r) p$surn==r$surn & p$first_club==r$club & pre(p,r))
try_stage("5 surname+prefix+played for club", function(p,r) p$surn==r$surn & vapply(p$clubs, function(z) r$club %in% z, TRUE) & pre(p,r))
try_stage("6 RELAXED name+stint at club after draft (earlier AFL games elsewhere/earlier)", function(p,r) p$nn==r$nn, relax=TRUE)
try_stage("7 RELAXED surname+prefix+stint at club after draft", function(p,r) p$surn==r$surn & pre(p,r), relax=TRUE)
try_stage("8 RELAXED surname+same first initial+stint at club after draft (Tom/Thomas, Matt/Matthew)", function(p,r) p$surn==r$surn & substr(p$firstn,1,1)==substr(r$firstn,1,1), relax=TRUE)
fd[, matched := !is.na(ID)]
cat("\n== Draft match: stage counts\n"); print(fd[, .N, by=stage])
cat("matched:", sum(fd$matched), "of", nrow(fd), "\n")
cat("matched rate among draftees with games_since_drafted>0:", fd[games_since_drafted>0, sum(matched)], "of", fd[games_since_drafted>0,.N], "\n")
um <- fd[!matched & games_since_drafted>0, .(draft_year,pick,drafted_by,pname,games_since_drafted,current_team)]
cat("UNMATCHED who played (per footywire):", nrow(um), "\n"); print(um, nrows=200)
cat("matched but footywire says 0 games:", fd[matched & (is.na(games_since_drafted)|games_since_drafted==0), .N], "\n")
cat("unmatched with 0 games (treated never played):", fd[!matched & (is.na(games_since_drafted)|games_since_drafted==0), .N], "\n")
fwrite(fd[, .(draft_year,pick,drafted_by,pname,games_since_drafted,ID,stage)], file.path(S,"draft_match.csv"))
fwrite(um, file.path(S,"draft_unmatched_played.csv"))

fd[, band := cut(pick, c(0,5,10,20,30,45,Inf), labels=c("1-5","6-10","11-20","21-30","31-45","46plus"))]
dm <- merge(fd[matched==TRUE, .(did, ID, dclub=club)], st[, .(ID, stint, sclub=club, span, n_seasons, outcome, event, first, last)], by="ID", allow.cartesian=TRUE)
dm <- merge(dm, fd[, .(did, dy=draft_year)], by="did")
dm1 <- dm[sclub==dclub & first >= dy+1L][order(did, stint)][, .SD[1], by=did]
fd[, firstseen := did %in% dm1$did]
dd <- merge(fd, dm1[, .(did, span, n_seasons, outcome, event, first, last)], by="did", all.x=TRUE)
dd[, cat := fifelse(!matched,"never_played_AFL",fifelse(!firstseen,"played_only_elsewhere", outcome))]
cat("\n== Draftees 1991-2025 by pick band and outcome at drafting club\n")
print(dcast(dd, band ~ cat, value.var="did", fun.aggregate=length))
dd2 <- dd[cat %in% c("moved","left_league","still_there") & draft_year<=2020]
kmr <- function(x,l){ f<-survfit(Surv(span,event)~1,data=x); s<-summary(f,times=c(2,4),extend=TRUE)$surv
 data.table(band=l,n_debuted_at_club=nrow(x),moved=sum(x$outcome=="moved"),left=sum(x$outcome=="left_league"),still=sum(x$outcome=="still_there"),
 obs_median_span=median(x$span),obs_median_seasons_played=median(x$n_seasons),KM_median_span=summary(f)$table["median"],P_3plus=round(s[1],3),P_5plus=round(s[2],3))}
dr <- rbindlist(c(lapply(levels(dd2$band), function(b) kmr(dd2[band==b],b)), list(kmr(dd2,"all"))))
cat("\n== Draftees 1991-2020, debuted at drafting club\n"); print(dr); fwrite(dr, file.path(S,"draftees_table.csv"))
fwrite(dd, file.path(S,"draftees_detail.csv"))

# ---- stay curve
inl_key <- unique(ps[, paste(ID, Season)])
atc_key <- unique(ps[, paste(ID, Season, club)])
mk <- function(anchor_dt, gtype, gcol) {
  rbindlist(lapply(1:8, function(k) {
    a <- copy(anchor_dt); a[, s := anchor + k]; a <- a[s <= 2026]
    a[, inl := paste(ID, s) %in% inl_key]
    a[, atc := paste(ID, s, club) %in% atc_key]
    a[, .(group_type=gtype, k=k, n_in_league=sum(inl), n_at_club=sum(inl & atc)), by=.(group=get(gcol))]
  }))
}
am <- st[stint>1 & first>=2000 & first<=2025, .(ID, club, anchor=first, ageband=as.character(cut(age_arr,c(-Inf,24,28,31,Inf),right=FALSE,labels=c("under24","24-27","28-30","31plus"))))]
am <- am[!is.na(ageband)]
ad <- fd[matched==TRUE, .(ID, club, anchor=draft_year, band=as.character(band))]
cm <- mk(am,"moved","ageband"); cdr <- mk(ad,"draftee","band")
cur <- rbind(cm,cdr); cur[, share_at_club := round(n_at_club/n_in_league,4)]
setcolorder(cur, c("group_type","group","k","n_in_league","n_at_club","share_at_club"))
fwrite(cur, file.path(S,"stay_curve.csv"))
cat("\n== stay curve share\n"); print(dcast(cur, group_type+group ~ k, value.var="share_at_club"))
cat("\n== n in league\n"); print(dcast(cur, group_type+group ~ k, value.var="n_in_league"))
