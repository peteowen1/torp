# Build the trade ledger page's data block from the grade files and fill the
# page template: 2026 from 05_grade_trades.R, 2021-2025 from
# 10_grade_past_trades.R (at the time).
#
# Output: torpdata/data/raw/trade_value/trade_ledger.html (gitignored), from
# templates/trade_ledger_template.html (its `const D = __DATA__;` line). Published as
# the private Trade Ledger artifact.

suppressMessages({ library(data.table); library(jsonlite) })
out_dir <- "C:/dev/torpverse/torpdata/data/raw/trade_value"
r1 <- function(x) round(x, 1)
na_null <- function(x) if (length(x) == 0 || is.na(x)) NULL else x

asset_rows <- function(a, y) {
  lapply(seq_len(nrow(a)), function(i) {
    x <- a[i]
    is_pick <- x$type %in% c("pick_2026", "pick_draft")
    list(to = x$to_club, frm = x$from_club, item = x$item,
         type = if (is_pick) "pick" else x$type,
         exp = na_null(if (x$type == "future_pick") r1(x$exp_pick) else NA),
         age = na_null(if (x$type == "player") r1(x$age) else NA),
         torp = na_null(if (x$type == "player") r1(x$torp_then) else NA),
         v = r1(fifelse(is.na(x$value), 0, x$value)),
         pick = na_null(if (is_pick) as.integer(x$pick) else NA),
         pat = na_null(if (is_pick) as.integer(x$pick_at_trade) else NA),
         orig = na_null(if (is_pick) x$original_club else NA),
         unmatched = if (x$type == "player" && is.na(x$value)) TRUE else NULL)
  })
}

year_block <- function(a, gr, y, n_trades) {
  trades <- split(a, a$trade)
  list(n_trades = n_trades,
       trades = lapply(trades, asset_rows, y = y),
       grades = lapply(seq_len(nrow(gr)), function(i) {
         g <- gr[i]
         list(trade = g$trade, club = g$club, net = r1(g$net),
              abc = na_null(if ("net_abc" %in% names(g)) r1(g$net_abc) else NA),
              nclub = r1(g$net_club))
       }))
}

blocks <- list()
# 2026: the published grades.
a26 <- fread(file.path(out_dir, "05_trade_assets.csv"))
g26 <- fread(file.path(out_dir, "05_trade_grades.csv"))
setnames(a26, "torp_2026", "torp_then")
blocks[["2026"]] <- year_block(a26[order(trade)], g26[order(trade, club)], 2026L, uniqueN(g26$trade))
for (y in 2025:2021) {
  a <- fread(file.path(out_dir, sprintf("10_assets_%d.csv", y)))
  gr <- fread(file.path(out_dir, sprintf("10_grades_%d.csv", y)))
  a[, pick_at_trade := pick][, original_club := NA_character_]
  blocks[[as.character(y)]] <- year_block(a[order(trade)], gr[order(trade, club)], y, uniqueN(gr$trade))
}

curve <- fread(file.path(out_dir, "04_pick_curve.csv"))[pick %in% c(1, 2, 3, 5, 6, 10, 15, 20, 25, 30, 40, 50, 60)]
D <- list(years = blocks,
          curve = lapply(seq_len(nrow(curve)), function(i) {
            cc <- curve[i]
            list(pick = cc$pick, v8 = cc$v8_dvi_scale, v5 = cc$v5_dvi_scale, games = cc$games_dvi_scale,
                 dvi = na_null(cc$dvi))
          }))
js <- toJSON(D, auto_unbox = TRUE, null = "null", digits = NA)

tpl <- readLines("data-raw/08-trade-value/templates/trade_ledger_template.html", encoding = "UTF-8", warn = FALSE)
hit <- grep("const D = __DATA__;", tpl, fixed = TRUE)
stopifnot(length(hit) == 1)
js <- gsub("</", "<\\/", js, fixed = TRUE)  # a "</script>" in scraped text cannot close the script
tpl[hit] <- paste0("const D = ", js, ";")
writeLines(tpl, file.path(out_dir, "trade_ledger.html"), useBytes = TRUE)
cat("ledger written:", sprintf("%.0f KB", file.size(file.path(out_dir, "trade_ledger.html")) / 1024),
    "| trades per year:", paste(names(blocks), sapply(blocks, `[[`, "n_trades"), collapse = ", "), "\n")
