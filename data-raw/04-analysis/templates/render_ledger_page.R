# Fill a net-ledger page template with one generator's JSON.
#
# The templates in this folder are the ONE renderer for both sports: torp's
# build_np_categories_artifact.R and panna's build_net_goals_artifacts.R each
# write a JSON file in the shared shape, and this puts it into the page. See
# vault/plans/NET-LEDGER-PARITY.md.
#
#   Rscript render_ledger_page.R <template.html> <data.json> <out.html> "<Page title>"
a <- commandArgs(trailingOnly = TRUE)
stopifnot(length(a) == 4, file.exists(a[1]), file.exists(a[2]))
tpl  <- paste(readLines(a[1], encoding = "UTF-8", warn = FALSE), collapse = "\n")
json <- paste(readLines(a[2], encoding = "UTF-8", warn = FALSE), collapse = "\n")
invisible(jsonlite::parse_json(json))            # fail here, not in a browser
json <- gsub("</", "<\\/", json, fixed = TRUE) # cannot close the <script>
stopifnot(grepl("/*__DATA__*/null", tpl, fixed = TRUE))
out <- sub("/*__DATA__*/null", json, tpl, fixed = TRUE)
# the gallery names the page from <title>, so each sport's page gets its own
out <- sub("<title>[^<]*</title>", paste0("<title>", a[4], "</title>"), out)
writeLines(out, a[3], useBytes = TRUE)
cat("wrote", a[3], "(", round(file.size(a[3]) / 1024), "KB )\n")
