# Three design questions about the positional adjustment, answered with data.
#
#   Q1  Would centring on position_group fix it, or do forwards ruck too?
#   Q2  Is the METHOD wrong, or just the group?
#   Q3  Does the ORDER of operations matter?
#
# Context correction from the same session: the hitout channel is NOT
# standardised. EPV_STANDARDISE_CHANNELS is recv/disp/spoil, so for hitout
# .position_adjust returns (per80 - cell_mean) * TOG with no division by the
# cell sd. Reconstructed that way it reproduces all three rucks to within 0.05,
# which the standardised version did not. So the mechanism is a MEAN
# SUBTRACTION and nothing else.

suppressMessages({
  library(data.table); library(arrow)
  devtools::load_all("C:/dev/torpverse/torp", quiet = TRUE)
})

OUT_DIR <- "C:/dev/torpverse/torp/data-raw/outputs"
sink(file.path(OUT_DIR, "centring_design.txt"), split = TRUE)
cat("=== Positional adjustment: group, method, order ===\nrun at", format(Sys.time()), "\n")

d <- as.data.table(read_parquet(file.path(OUT_DIR, "v2_benchremap_pgd.parquet")))
d[, tog_safe := pmax(fifelse(is.na(time_on_ground_percentage), 100,
                             time_on_ground_percentage) / 100, 0.1)]
d[, p80 := epv_hitout / tog_safe]
S <- max(d$season, na.rm = TRUE)

cat("\n########## Q1: WOULD position_group FIX IT? ##########\n")
cat("Every player with real ruck involvement, by their LISTED position_group.\n")
cat("If they are not all RUCK, position_group has the same problem.\n\n")
r <- d[season == S, .(gm = .N, rc = mean(ruck_contests, na.rm = TRUE),
                      p80 = mean(p80, na.rm = TRUE)),
       by = .(player_name, position_group)][gm >= 6 & rc >= 15]
setorder(r, -rc)
print(r[, .(player_name, position_group, gm, contests = round(rc, 1),
            per80 = round(p80, 2))][1:20], nrows = 22)
cat("\nposition_group mix among players with >=15 ruck contests a game:\n")
print(r[, .N, by = position_group][order(-N)])

cat("\nand the reverse -- how much do the non-RUCK listed players actually ruck:\n")
print(r[position_group != "RUCK", .(player_name, position_group,
        contests = round(rc, 1), per80 = round(p80, 2))])

cat("\n########## Q2: IS IT THE METHOD OR THE GROUP? ##########\n")
cat("The hitout channel is CENTRED but NOT standardised:\n")
cat("  EPV_STANDARDISE_CHANNELS =", paste(EPV_STANDARDISE_CHANNELS, collapse = ", "), "\n")
cat("  so adj = (per80 - cell_mean) * TOG, a plain mean subtraction.\n\n")
mk <- function(x) {
  sl <- torp:::.remap_bench_role(as.character(x$lineup_position), x$player_id,
                                 x$season, x$position_group)
  if (isTRUE(ROLE_USE_LINEUP_GROUP)) torp:::.collapse_lineup_group(sl) else sl
}
d[, rk := mk(d)]
WHO <- c("Brodie Grundy", "Max Gawn", "Mason Cox")
cells_slot <- d[, .(m = weighted.mean(p80, tog_safe, na.rm = TRUE)), by = .(cell = rk)]
cells_pg   <- d[, .(m = weighted.mean(p80, tog_safe, na.rm = TRUE)), by = .(cell = position_group)]
cells_ruck <- d[, .(m = weighted.mean(p80, tog_safe, na.rm = TRUE)),
                by = .(cell = fifelse(ruck_contests >= 10, "RUCKS", "OTHER"))]
one <- function(nm, key, cells, lbl) {
  x <- d[season == S & player_name == nm]
  # `cells[cell == get("cell")]` is the documented self-join trap -- BOTH sides
  # resolve to the table's own column, so it matches every row and [1] silently
  # returns the first. It did exactly that here: every scheme reported a cell
  # mean of ~0.01 and the comparison was meaningless. Look it up by a plain
  # vector with a DIFFERENT name instead.
  want <- as.character(x[[key]][1])
  m <- cells$m[match(want, as.character(cells$cell))]
  if (length(m) != 1 || is.na(m)) {
    cli::cli_abort("No cell mean for {.val {want}} under scheme {lbl}.")
  }
  data.table(player = nm, scheme = lbl, cell = want, cell_mean = round(m, 3),
             adj = round(mean((x$p80 - m) * x$tog_safe, na.rm = TRUE), 3))
}
d[, ruckcell := fifelse(ruck_contests >= 10, "RUCKS", "OTHER")]
res <- rbindlist(c(
  lapply(WHO, one, key = "rk", cells = cells_slot, lbl = "A. lineup slot (current)"),
  lapply(WHO, one, key = "position_group", cells = cells_pg, lbl = "B. listed position_group"),
  lapply(WHO, one, key = "ruckcell", cells = cells_ruck, lbl = "C. ruck involvement")))
setorder(res, scheme, -adj)
print(res)
cat("\nScheme C cells on WHAT THE PLAYER DID rather than where he lined up.\n")
cat("Whichever scheme puts Grundy top and Cox below him is the one that works.\n")

cat("\n########## Q3: DOES THE ORDER OF OPERATIONS MATTER? ##########\n")
cat("Current: raw -> per-80 -> centre (+standardise) -> xTOG -> opponent adj\n")
cat("         -> level centre -> POINTS SCALE -> EPR shrink\n\n")
cat("Centring and scaling COMMUTE: k*(x - m) = k*x - k*m, so moving the points\n")
cat("scale before the centring changes nothing, as long as the mean is taken on\n")
cat("the same data. For the hitout channel, which is only centred, order is\n")
cat("genuinely irrelevant.\n\n")
cat("Standardising and scaling DO NOT commute -- they ANNIHILATE:\n")
cat("  (k*x - k*m) / (k*s) = (x - m) / s\n")
cat("The k cancels exactly. So for recv/disp/spoil, applying a points scale\n")
cat("BEFORE standardising would be completely undone. The current order is not\n")
cat("a preference, it is forced: you cannot scale first.\n\n")
cat("The real consequence, and it is a design tension rather than a bug:\n")
cat("standardising DESTROYS 'one unit = one point' within a cell by construction,\n")
cat("and the points scale then re-imposes it globally. Three of four channels\n")
cat("carry that; hitout, being unstandardised, keeps its units throughout --\n")
cat("which is exactly why hitout is the channel where a wrong cell mean shows\n")
cat("up as a visible, interpretable error.\n")

sd_check <- d[, .(sd_p80 = round(sd(p80, na.rm = TRUE), 3)), by = .(cell = rk)][order(-sd_p80)]
cat("\nper-80 hitout spread by cell (why standardising this channel would be worse):\n")
print(sd_check[1:6])

saveRDS(list(rucks = r, schemes = res), file.path(OUT_DIR, "centring_design.rds"))
cat("\ndone", format(Sys.time()), "\n"); sink(); cat("\nDone\n")
