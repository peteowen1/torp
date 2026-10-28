library(data.table)
S <- "C:/Users/peteo/AppData/Local/Temp/claude/C--dev-torpverse-torp/fd72fa3a-4884-46e7-9859-8c01702f8ead/scratchpad/tenure"
st <- fread(file.path(S,"stints.csv")); um <- fread(file.path(S,"draft_unmatched_played.csv"))
nz <- function(x) gsub("[^a-z]","", tolower(iconv(x, to="ASCII//TRANSLIT")))
st[, nn := nz(name)]; um[, nn := nz(pname)]
um[, surn := nz(sub("^\\S+\\s+","",pname))]; st[, surn := nz(sub("^\\S+\\s+","",name))]
pl <- st[, .(fc=min(first), name=name[1]), by=.(ID, nn, surn)]
res <- um[, {
  e <- pl[pl$nn %in% nn]; s <- pl[pl$surn %in% surn]
  .(exact_fc = paste(e$fc, collapse="/"), surn_cands = paste0(s$name, "@", s$fc, collapse="; "))
}, by=.(draft_year, pick, drafted_by, pname, games_since_drafted, nn, surn)]
res[, nn := NULL]
res[, ex_year_off := fifelse(exact_fc=="", NA_integer_, as.integer(sub("/.*","",exact_fc)) - draft_year)]
cat("unmatched played:", nrow(res), "; exact-name exists:", res[exact_fc!="", .N], "\n")
print(table(res$ex_year_off, useNA="ifany"))
print(table(res$draft_year)[1:8])
fwrite(res, file.path(S,"unmatched_diagnosis.csv"))
print(res[exact_fc=="", .(draft_year,pname,games_since_drafted,surn_cands)][1:40], nrows=40)
