# Promote the rating vintage to v4 (Net Points) -- the manual, one-off step the
# daily pipeline must never do (docs/plans/RATING-VERSIONING-PLAN.md section 2.4).
#
# Order matters and mirrors the v3 promotion of 2026-08-18 (torp fd6282da):
#   1. preserve the outgoing v3 canonical as torp_ratings_v3.parquet, by
#      re-uploading the SAME downloaded file (byte-identical, md5-checked), not
#      through preserve_rating_vintage(), which re-serialises;
#   2. rewrite the manifest: v3 entry now points at torp_ratings_v3.parquet
#      (its provenance untouched), a v4 entry carries the live constants, and
#      canonical becomes "v4";
#   3. only then merge RATING_VINTAGE <- "v4" to main, because
#      check_vintage_alignment() refuses a run whenever the code's vintage and
#      the manifest's canonical disagree.
#
# Dry run by default. PROMOTE_APPLY=1 uploads.
suppressMessages({ library(data.table); devtools::load_all(quiet = TRUE) })
APPLY <- identical(Sys.getenv("PROMOTE_APPLY"), "1")
SNAP <- Sys.getenv("PROMOTE_V3_FILE")   # the downloaded canonical v3 parquet
stopifnot(nzchar(SNAP), file.exists(SNAP))
repo <- get_torp_data_repo()
say <- function(...) cat(..., "\n", sep = "")

m <- read_ratings_manifest()
stopifnot(!is.null(m), identical(m$canonical, "v3"), !is.null(m$vintages$v3))
stopifnot(identical(m$vintages$v3$file, "torp_ratings.parquet"))
if (!is.null(m$vintages$v4)) stop("a v4 entry already exists; refusing to overwrite")

# the snapshot must BE the published canonical, not merely look like it
live <- system2("gh", c("release", "view", "ratings-data", "--repo", repo, "--json", "assets",
                        "--jq", shQuote('.assets[] | select(.name=="torp_ratings.parquet") | [.size, .updatedAt] | @tsv')), stdout = TRUE)
live <- gsub("	", " ", live)
say("published torp_ratings.parquet: ", live, " | snapshot bytes: ", file.size(SNAP))
stopifnot(as.numeric(strsplit(live, " ")[[1]][1]) == file.size(SNAP))
snap <- arrow::read_parquet(SNAP)
say("snapshot rows ", nrow(snap), " | manifest v3 rows ", m$vintages$v3$rows)
stopifnot(nrow(snap) == m$vintages$v3$rows)
md5 <- tools::md5sum(SNAP)[[1]]

existing <- tryCatch(withr::with_envvar(c(VERSEBUS_STRICT = "1"), load_torp_ratings(version = "v3")), error = function(e) NULL)
if (!is.null(existing) && nrow(existing) > 0) stop("torp_ratings_v3.parquet already exists on the release; refusing to overwrite")

m$vintages$v3$file <- "torp_ratings_v3.parquet"
m$vintages$v4 <- torp:::.build_rating_vintage_entry(nrow(snap), version = "v4", file = "torp_ratings.parquet")
m$canonical <- "v4"
say("live constants going into the v4 entry: EPV_ENGINE ", EPV_ENGINE, ", EPR_UNITS_SCALE_V4 ", EPR_UNITS_SCALE_V4)
stopifnot(identical(EPV_ENGINE, "v4"))
tf <- file.path(tempdir(), "ratings_manifest.json")
writeLines(jsonlite::toJSON(m, auto_unbox = TRUE, pretty = TRUE, null = "null"), tf)
say("manifest after promotion: canonical ", m$canonical, "; vintages ", paste(names(m$vintages), collapse = ", "))

if (!APPLY) { say("DRY RUN: nothing uploaded. Set PROMOTE_APPLY=1 to apply."); quit(status = 0) }

up <- file.path(tempdir(), "torp_ratings_v3.parquet"); file.copy(SNAP, up, overwrite = TRUE)
piggyback::pb_upload(up, repo = repo, tag = "ratings-data", overwrite = FALSE)
chk <- file.path(tempdir(), "verify_v3.parquet")
system2("gh", c("release", "download", "ratings-data", "--repo", repo, "--pattern", "torp_ratings_v3.parquet", "--output", chk, "--clobber"))
md5_back <- tools::md5sum(chk)[[1]]
say("md5 local ", md5, " | md5 on release ", md5_back)
if (!identical(md5, md5_back)) stop("preserved file is not byte-identical; do NOT proceed")
piggyback::pb_upload(tf, repo = repo, tag = "ratings-data", overwrite = TRUE)
m2 <- read_ratings_manifest()
stopifnot(identical(m2$canonical, "v4"), identical(m2$vintages$v3$file, "torp_ratings_v3.parquet"))
say("PROMOTED: canonical v4; v3 preserved byte-identical (md5 ", substr(md5, 1, 8), "). Now merge RATING_VINTAGE <- \"v4\".")
