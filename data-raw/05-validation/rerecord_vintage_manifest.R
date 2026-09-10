# Re-record a vintage's manifest entry under the CURRENT constant set
# =============================================================================
# When .rating_defining_constants() gains a constant, the published manifest
# still records the old, narrower set, so check_vintage_alignment() reports
# drift and ABORTS every pipeline run -- even with strict = FALSE. The data is
# fine; only what is written down about it is stale. This re-records the same
# vintage under current code: same label, same file, same rows, wider
# defining_constants.
#
# Written 2026-09-10 for the v8 re-record after torp 1.8.2 wired ten NP_
# constants: 29 leaves newly recorded, 0 removed, 0 value changes.
#
# 29, not 32, and the gap is worth knowing because it is the same quirk that
# broke this script's first polling loop. A loose unlist()-based comparison
# counts 32; .diff_defining_constants() -- the comparison the guard actually
# makes, and therefore the number that matters -- reports 29. The three
# missing ones are NA-valued leaves (NP_TEAM_MARGIN_NAMED_SHARE and
# LINEUP_POSITION_GROUP_MAP's INT/SUB/EMERG), which serialise to JSON null and
# vanish from unlist() on the way back. Quote the guard's number, not a count.
#
# -----------------------------------------------------------------------------
# WHEN TO RUN IT. This is the opposite of the order a VALUE change uses, and
# getting it backwards took the pipeline down on 2026-09-10:
#
#   VALUE change (a constant's value moves, so published numbers change):
#     promote/record the new vintage BEFORE the code reaches `main`. The
#     manifest has to describe the new data before the code that writes it
#     lands. This is the v6 -> v7 -> v8 pattern in NEWS.
#
#   WIDENING change (constants ADDED to what the guard records; values
#     unchanged, published numbers unmoved -- what this script is for):
#     merge the code to `main` FIRST, then run this. A manifest that lists
#     constants the running code does not have IS drift, because
#     .diff_defining_constants() unions both name sets and reports <absent> in
#     EITHER direction. Recording 97 leaves from a dev branch while `main` knew
#     65 made main see 29 phantom differences and abort every run.
#
# The rule of thumb: the manifest may never describe more constants than the
# code on `main` knows about.
#
# -----------------------------------------------------------------------------
# WHY THIS SCRIPT ASSERTS ITS OWN CHECKOUT (added on review, 2026-09-10).
# The closing check below cannot verify the rule above on its own, and it is
# important to understand why before trusting it. check_vintage_alignment()
# compares the manifest against .rating_defining_constants() evaluated LIVE in
# this session -- and publish_ratings_manifest() wrote the manifest from that
# same live call moments earlier. Both sides of the comparison therefore come
# from the same session and are guaranteed to agree, whatever branch is loaded.
# Run from a dev branch, this script would print "ALIGNED" while having proven
# nothing except that it agrees with itself, which is exactly the incident that
# prompted writing it. So the branch check below is the real guard; the closing
# alignment check only confirms the write propagated.
#
#   powershell.exe -Command 'Rscript "data-raw/05-validation/rerecord_vintage_manifest.R"'
#
# Env: RERECORD_PKG      package to load (default: this repo)
#      RERECORD_VINTAGE  vintage to re-record (default: the code's RATING_VINTAGE)
#      RERECORD_ALLOW_NON_MAIN=1  deliberately run off-main; use only when
#                        re-recording to MATCH a non-main checkout on purpose.
say <- function(...) cat(..., "\n", sep = "")
fail <- function(...) { say("\nABORTED: ", ...); quit(status = 1) }

PKG <- Sys.getenv("RERECORD_PKG", "C:/dev/torpverse/torp")

# --- the real guard: is this checkout what production runs? ------------------
gitq <- function(...) suppressWarnings(system2(
  "git", c("-C", shQuote(PKG), ...), stdout = TRUE, stderr = FALSE))
invisible(gitq("fetch", "origin", "main", "--quiet"))
head_sha <- gitq("rev-parse", "HEAD")[1]
main_sha <- gitq("rev-parse", "origin/main")[1]
say("package:      ", PKG)
say("HEAD:         ", head_sha)
say("origin/main:  ", main_sha)

# Both must be real SHAs. If git fails or the fetch does not resolve, both come
# back NA -- and identical(NA, NA) is TRUE, which would sail straight through
# the very check that exists to stop this. Fail closed, not open.
is_sha <- function(x) is.character(x) && length(x) == 1L && !is.na(x) &&
  grepl("^[0-9a-f]{40}$", x)
if (!is_sha(head_sha) || !is_sha(main_sha)) {
  fail("could not resolve HEAD and origin/main as commit SHAs (got ",
       deparse(head_sha), " and ", deparse(main_sha), ").\n",
       "  Refusing to write a manifest without knowing which code this is.")
}

if (!identical(head_sha, main_sha)) {
  if (!identical(Sys.getenv("RERECORD_ALLOW_NON_MAIN"), "1")) {
    fail("this checkout is NOT at origin/main, so the constant set here may be\n",
         "  wider or narrower than what the pipeline runs. Recording it would make\n",
         "  main see phantom drift and abort every run -- the 2026-09-10 incident.\n\n",
         "  Merge first, then re-record. To re-record from main without moving this\n",
         "  working tree, use a worktree:\n\n",
         "    git -C ", PKG, " worktree add C:/Users/<you>/AppData/Local/Temp/torp-main origin/main\n",
         "    RERECORD_PKG=C:/Users/<you>/AppData/Local/Temp/torp-main Rscript <this script>\n",
         "    git -C ", PKG, " worktree remove C:/Users/<you>/AppData/Local/Temp/torp-main\n\n",
         "  If you genuinely mean to record a non-main constant set, set\n",
         "  RERECORD_ALLOW_NON_MAIN=1 and know why.")
  }
  say("\n!! RERECORD_ALLOW_NON_MAIN=1 -- recording a NON-MAIN constant set on purpose.")
}

suppressMessages(devtools::load_all(PKG, quiet = TRUE))
options(torp.local_data_dir = NA)

VERSION <- Sys.getenv("RERECORD_VINTAGE", RATING_VINTAGE)
say("torp ", as.character(utils::packageVersion("torp")), ", re-recording vintage ", VERSION)

before <- read_ratings_manifest()
if (is.null(before)) fail("could not read the published manifest at all.")
entry <- before$vintages[[VERSION]]
if (is.null(entry)) fail("vintage ", VERSION, " is not in the manifest.")

# check_vintage_alignment() ALWAYS evaluates manifest$canonical -- it takes no
# vintage argument -- so on a non-canonical re-record the closing check reports
# on a different vintage than the one just written. Say so rather than let it
# read as verification.
is_canonical <- identical(before$canonical, VERSION)
if (!is_canonical) {
  say("NOTE: ", VERSION, " is not canonical (canonical is ", before$canonical,
      "). The closing alignment check reports on CANONICAL and so says nothing",
      " about this write.")
}

# `file` MUST be passed explicitly: its default is .rating_vintage_file(VERSION)
# -> "torp_ratings_v8.parquet", but the canonical vintage lives at
# "torp_ratings.parquet", so the default would record a path holding no data.
n_rows_before <- entry$rows
file_before   <- entry$file
leaves_before <- length(unlist(entry$defining_constants))
leaves_live   <- length(unlist(torp:::.rating_defining_constants()))
say("file=", file_before, "  rows=", n_rows_before)
say("defining_constants: ", leaves_before, " recorded -> ", leaves_live, " under live code")
stopifnot(is.character(file_before), length(file_before) == 1L,
          is.numeric(n_rows_before) || is.integer(n_rows_before))

# Deliberately NOT wrapped: an upload failure must halt loudly.
publish_ratings_manifest(n_rows = n_rows_before, version = VERSION,
                         file = file_before, set_canonical = FALSE)

# GitHub's CDN served a stale manifest for 84-114 seconds after each overwrite
# on 2026-09-09/10, so poll the uncached url rather than trusting one read.
#
# Poll on .diff_defining_constants(), NOT a leaf count: NA-valued constants
# (NP_TEAM_MARGIN_NAMED_SHARE, LINEUP_POSITION_GROUP_MAP's INT/SUB/EMERG)
# serialise to null and unlist() drops them, so 97 live reads as 94 published
# even when the two agree exactly. The first version polled on the count, never
# matched, and burned all 20 attempts on a run that had already succeeded.
# read_ratings_manifest() returns NULL on failure rather than throwing, so no
# tryCatch is needed here.
say("\npolling the uncached manifest for the re-record to land...")
ok <- FALSE
for (i in 1:25) {
  Sys.sleep(6)
  after <- read_ratings_manifest()
  if (is.null(after)) { say("  attempt ", i, ": manifest unreadable, retrying"); next }
  a <- after$vintages[[VERSION]]
  if (is.null(a)) { say("  attempt ", i, ": vintage absent, retrying"); next }
  n_diff <- length(torp:::.diff_defining_constants(a$defining_constants))
  say("  attempt ", i, ": torp_version=", a$torp_version, "  constant differences=", n_diff)
  if (n_diff == 0L) { ok <- TRUE; break }
}
if (!ok) fail("the re-record did not read back as matching within 150s. The upload\n",
              "  itself reported success, so this is most likely the CDN still serving a\n",
              "  stale copy -- re-read in a minute before re-running anything.")

say("\n=== verification ===")
after <- read_ratings_manifest()
if (is.null(after)) fail("manifest unreadable at verification time.")
a <- after$vintages[[VERSION]]
say("canonical:      ", after$canonical)
say("file:           ", a$file, "   (must be ", file_before, ")")
say("rows:           ", a$rows, "   (must be ", n_rows_before, ")")
say("torp_version:   ", a$torp_version)
say("leaves:         ", length(unlist(a$defining_constants)), " (was ", leaves_before,
    "; live unlist() says ", leaves_live, " -- NA leaves serialise to null)")
say("constant diffs: ", length(torp:::.diff_defining_constants(a$defining_constants)),
    "   (must be 0)")
stopifnot(identical(after$canonical, before$canonical),
          identical(a$file, file_before),
          identical(as.integer(a$rows), as.integer(n_rows_before)))

# This confirms the write PROPAGATED. It does not independently verify the
# constant set is right for production -- see the header. The branch assertion
# above is what does that.
say("\n=== did the write propagate? (guard, against this session's constants) ===")
if (!is_canonical) {
  say("SKIPPED: ", VERSION, " is not canonical, and this check only ever reads canonical.")
} else {
  res <- tryCatch({ check_vintage_alignment(strict = TRUE); "ALIGNED" },
    error = function(e) {
      kind <- if (inherits(e, "torp_error_vintage_constants_drift")) "CONSTANTS STILL DRIFTING"
              else if (inherits(e, "torp_error_vintage_mismatch")) "CANONICAL LABEL MISMATCH (not drift)"
              else if (inherits(e, "torp_error_vintage_manifest_unreadable")) "MANIFEST UNREADABLE (not drift)"
              else if (inherits(e, "torp_error_vintage_undefined")) "VINTAGE HAS NO RECORDED CONSTANTS (not drift)"
              else "UNEXPECTED ERROR"
      paste0(kind, ": ", conditionMessage(e))
    })
  say(res)
  # A drift result must not exit 0 -- a wrapper or a skimming human would read
  # it as success. This is the one line that decides whether the run worked.
  if (!identical(res, "ALIGNED")) fail(res)
}
say("\nDone.")
