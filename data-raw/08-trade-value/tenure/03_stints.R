library(data.table); library(survival)
S <- "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-torpverse-torp/fd72fa3a-4884-46e7-9859-8c01702f8ead/scratchpad/tenure"
fs <- list.files("C:/dev/torpverse/torpdata/data/raw/tenure", full.names=TRUE)
d <- rbindlist(lapply(fs, fread, select=c("Season","Round","Playing.for","Player","ID","First.name","Surname","Age")), fill=TRUE)
franch <- c("Brisbane Bears"="Brisbane","Brisbane Lions"="Brisbane","Fitzroy"="Brisbane","Kangaroos"="North Melbourne","Footscray"="Western Bulldogs","Greater Western Sydney"="GWS")
d[, club := fifelse(Playing.for %in% names(franch), franch[Playing.for], Playing.for)]
rk <- function(r) { n <- suppressWarnings(as.integer(r)); fifelse(is.na(n), 100L + match(r, c("Wildcard Final","EF","QF","SF","PF","GF")), n) }
d[, rr := rk(Round)]
cat("rounds unmapped:", d[is.na(rr), .N], "\n")
cat("dup (ID,Season,Round) rows:", sum(duplicated(d[, .(ID,Season,Round)])), "\n")
# one row per player-season-club
ps <- d[, .(games=.N, first_rr=min(rr), age=min(Age,na.rm=TRUE)), by=.(ID, Season, club)]
setorder(ps, ID, Season, first_rr)
ps[, brk := club != shift(club, fill=club[1]), by=ID]
ps[, stint := cumsum(brk) + 1L, by=ID]
st <- ps[, .(club=club[1], first=min(Season), last=max(Season), n_seasons=uniqueN(Season), games=sum(games), age_arr=age[1]), by=.(ID, stint)]
st[, span := last - first + 1L]
st[, nstint := .N, by=ID]
st[, prev_last := shift(last), by=ID]; st[, prev_club := shift(club), by=ID]
st[, next_club := shift(club, -1), by=ID]
st[, outcome := fifelse(stint < nstint, "moved", fifelse(last == 2026, "still_there", "left_league"))]
st[, gap := first - prev_last - 1L]
nm <- unique(d[, .(ID, Player)], by="ID"); st <- merge(st, nm, by="ID")
fwrite(st, file.path(S, "stints.csv"))
cat("stints:", nrow(st), " players:", uniqueN(st$ID), "\n")
print(st[, .N, by=outcome])
# sanity: same-season two clubs
print(ps[, .N, by=.(ID,Season)][N>1, .N])
print(st[gap < 0, .N])  # overlapping (mid-season) 
print(head(st[stint>1 & gap<0],5))
