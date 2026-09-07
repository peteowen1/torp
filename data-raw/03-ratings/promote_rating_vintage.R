# Promote the canonical rating vintage -- the manual, one-off step the daily
# pipeline must never do (docs/plans/RATING-VERSIONING-PLAN.md section 2.4).
#
# Generalised from promote_v4_vintage.R, which was written for one bump and then
# copied by sed for the next; the copy came out wrong because the substitutions
# collided. Pass the labels instead.
#
# Order matters and is not optional:
#   1. preserve the outgoing canonical as torp_ratings_<from>.parquet, by
#      re-uploading the SAME downloaded file, so it is byte-identical rather
#      than merely row-identical (md5-checked against a re-download);
#   2. rewrite the manifest: the <from> entry now points at its own file, a
#      <to> entry carries the live constants, and canonical becomes <to>;
#   3. only then merge RATING_VINTAGE <- "<to>" to main, because
#      check_vintage_alignment() refuses a run whenever the code's vintage and
#      the manifest's canonical disagree.
#
# Usage (dry run by default):
#   $env:PROMOTE_FROM='v4'; $env:PROMOTE_TO='v5'
#   $env:PROMOTE_FILE='<the downloaded canonical torp_ratings.parquet>'
#   Rscript data-raw/03-ratings/promote_rating_vintage.R
#   ... then set $env:PROMOTE_APPLY='1' to upload.
suppressMessages({ library(data.table); devtools::load_all(quiet = TRUE) })
FROM  <- Sys.getenv("PROMOTE_FROM")
TO    <- Sys.getenv("PROMOTE_TO")
SNAP  <- Sys.getenv("PROMOTE_FILE")
APPLY <- identical(Sys.getenv("PROMOTE_APPLY"), "1")
stopifnot(nzchar(FROM), nzchar(TO), !identical(FROM, TO), nzchar(SNAP), file.exists(SNAP))
repo <- get_torp_data_repo()
say <- function(...) cat(..., "\n", sep = "")
preserved <- paste0("torp_ratings_", FROM, ".parquet")

m <- read_ratings_manifest()
stopifnot(!is.null(m), identical(m$canonical, FROM), !is.null(m$vintages[[FROM]]))
stopifnot(identical(m$vintages[[FROM]]$file, "torp_ratings.parquet"))
if (!is.null(m$vintages[[TO]])) stop("a ", TO, " entry already exists; refusing to overwrite")

# the snapshot must BE the published canonical, not merely look like it
live <- system2("gh", c("release", "view", "ratings-data", "--repo", repo, "--json", "assets",
                        "--jq", shQuote('.assets[] | select(.name=="torp_ratings.parquet") | [.size, .updatedAt] | @tsv')),
                stdout = TRUE)
live <- gsub("\t", " ", live)
say("published torp_ratings.parquet: ", live, " | snapshot bytes: ", file.size(SNAP))
stopifnot(as.numeric(strsplit(live, " ")[[1]][1]) == file.size(SNAP))
snap <- arrow::read_parquet(SNAP)
say("snapshot rows ", nrow(snap), " | manifest ", FROM, " rows ", m$vintages[[FROM]]$rows)
stopifnot(nrow(snap) == m$vintages[[FROM]]$rows)
md5 <- tools::md5sum(SNAP)[[1]]

# Existence check against the release's own asset list, not a loader that maps a
# transient failure to "absent". An already-preserved file is fine ONLY if it is
# byte-identical to the snapshot (a previous run got as far as uploading and then
# failed on the manifest); anything else aborts.
assets <- system2("gh", c("release", "view", "ratings-data", "--repo", repo, "--json", "assets",
                          "--jq", shQuote('.assets[].name')), stdout = TRUE)
stopifnot(length(assets) > 0, "torp_ratings.parquet" %in% assets)
already <- preserved %in% assets
if (already) {
  chk0 <- file.path(tempdir(), paste0("existing_", FROM, ".parquet"))
  system2("gh", c("release", "download", "ratings-data", "--repo", repo,
                  "--pattern", preserved, "--output", chk0, "--clobber"))
  stopifnot(file.exists(chk0))
  if (!identical(tools::md5sum(chk0)[[1]], md5)) {
    stop(preserved, " already exists on the release and is NOT this snapshot; refusing to touch it")
  }
  say(preserved, " already preserved and byte-identical; only the manifest step remains")
}

m$vintages[[FROM]]$file <- preserved
m$vintages[[TO]] <- torp:::.build_rating_vintage_entry(nrow(snap), version = TO,
                                                       file = "torp_ratings.parquet")
m$canonical <- TO
say("live constants going into the ", TO, " entry: EPV_ENGINE ", EPV_ENGINE,
    " | units ", EPR_UNITS_SCALE_V4, " | turnover-on-all-acts ", NP_TURNOVER_ON_ALL_ACTS)
stopifnot(identical(torp:::RATING_VINTAGE, TO))
tf <- file.path(tempdir(), "ratings_manifest.json")
writeLines(jsonlite::toJSON(m, auto_unbox = TRUE, pretty = TRUE, null = "null"), tf)
say("manifest after promotion: canonical ", m$canonical,
    "; vintages ", paste(names(m$vintages), collapse = ", "))

if (!APPLY) { say("DRY RUN: nothing uploaded. Set PROMOTE_APPLY=1 to apply."); quit(status = 0) }

if (!already) {
  up <- file.path(tempdir(), preserved); file.copy(SNAP, up, overwrite = TRUE)
  piggyback::pb_upload(up, repo = repo, tag = "ratings-data", overwrite = FALSE)
  chk <- file.path(tempdir(), paste0("verify_", FROM, ".parquet"))
  system2("gh", c("release", "download", "ratings-data", "--repo", repo,
                  "--pattern", preserved, "--output", chk, "--clobber"))
  stopifnot(file.exists(chk))
  md5_back <- tools::md5sum(chk)[[1]]
  say("md5 local ", md5, " | md5 on release ", md5_back)
  if (!identical(md5, md5_back)) stop("preserved file is not byte-identical; do NOT proceed")
}
# From here the outgoing file is preserved. If the manifest upload fails,
# re-running with the same snapshot resumes at the manifest step.
ok <- tryCatch({
  piggyback::pb_upload(tf, repo = repo, tag = "ratings-data", overwrite = TRUE); TRUE
}, error = function(e) {
  say("MANIFEST UPLOAD FAILED: ", conditionMessage(e),
      "\n  State: ", preserved, " is preserved and verified; the manifest still says canonical ", FROM,
      ".\n  Remedy: re-run with the same PROMOTE_FILE and PROMOTE_APPLY=1; it resumes at the manifest step.")
  FALSE
})
if (!ok) quit(status = 1)
# Read back past GitHub's CDN cache: the plain download URL serves the old
# manifest for minutes after an overwrite, which turned a successful v4
# promotion into a false abort on 2026-09-07.
m2 <- jsonlite::fromJSON(paste0("https://github.com/", repo,
        "/releases/download/ratings-data/ratings_manifest.json?cb=", as.integer(Sys.time())),
        simplifyVector = FALSE)
stopifnot(identical(m2$canonical, TO), identical(m2$vintages[[FROM]]$file, preserved))
say("PROMOTED: canonical ", TO, "; ", FROM, " preserved byte-identical (md5 ", substr(md5, 1, 8),
    "). Now merge RATING_VINTAGE <- \"", TO, "\".")
