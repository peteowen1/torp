# -----------------------------------------------------------------------------
# create_player_game_data Tests
# -----------------------------------------------------------------------------

test_that("create_player_game_data function exists and is exported", {
  expect_true(exists("create_player_game_data"))
  expect_true("create_player_game_data" %in% getNamespaceExports("torp"))
})

test_that("create_player_game_data has correct function signature", {
  fn_args <- names(formals(create_player_game_data))

  expect_true("pbp_data" %in% fn_args)
  expect_true("player_stats" %in% fn_args)
  expect_true("teams" %in% fn_args)
  expect_true("decay" %in% fn_args)
})

test_that("create_player_game_data output contains required columns", {
  skip_if(is.null(.shared$pbp) || is.null(.shared$player_stats) || is.null(.shared$teams),
          "Could not load required data")

  pgd <- tryCatch(
    create_player_game_data(.shared$pbp, .shared$player_stats, .shared$teams),
    error = function(e) NULL
  )

  skip_if(is.null(pgd), "Could not create player game data")

  required_cols <- c(
    "match_id", "player_id", "epv_adj", "epv_recv_adj",
    "epv_disp_adj", "epv_spoil_adj", "epv_hitout_adj",
    "team", "opponent", "season", "round",
    "position_group", "lineup_position"
  )

  for (col in required_cols) {
    expect_true(col %in% names(pgd), info = paste("Missing column:", col))
  }

  expect_true(nrow(pgd) > 0)

  # WPA columns should be present when PBP has wpa data
  wpa_cols <- c("wp_credit", "wp_disp_credit", "wp_recv_credit",
                "wp_credit_adj", "wp_disp_credit_adj", "wp_recv_credit_adj")
  if ("wpa" %in% names(.shared$pbp)) {
    for (col in wpa_cols) {
      expect_true(col %in% names(pgd), info = paste("Missing WPA column:", col))
    }
    # WPA values should not be all zero (at least some plays have non-zero WPA)
    expect_true(sum(pgd$wp_credit != 0) > 0, info = "All wp_credit values are zero")
  }
})

test_that("create_player_game_data *_adj columns are game-value scale (regression test for #79)", {
  # Regression test: epv_adj was previously stored as a centered per-80 rate
  # while downstream consumers treated it as a game value, inflating low-TOG
  # players' epv_adj by ~1/tog (e.g. Zac Williams R7 2025 showed epv_adj=71
  # vs raw epv=8 with tog=0.10). Fix multiplies by tog_safe after centering
  # so *_adj is a position-adjusted game value. This test asserts that
  # invariant so the bug can't silently return.
  skip_if(is.null(.shared$pbp) || is.null(.shared$player_stats) || is.null(.shared$teams),
          "Could not load required data")

  pgd <- tryCatch(
    create_player_game_data(.shared$pbp, .shared$player_stats, .shared$teams),
    error = function(e) NULL
  )
  skip_if(is.null(pgd), "Could not create player game data")
  skip_if(nrow(pgd) == 0, "Empty player game data")

  # Game-value magnitude invariant: max |epv_adj| should not exceed ~50.
  # Under the bug, low-TOG cameos blew up to |epv_adj| ~= |epv|/tog
  # (Zac Williams R7 2025: 10% TOG, raw epv 8.14, broken epv_adj 71.3).
  # On the fix, max |epv_adj| sits around 34 (a full-game star's game).
  # A 50 cap cleanly discriminates with ~15 points of headroom for future
  # exceptional games. If this ever false-fails on legitimate data, something
  # extraordinary happened (60+ point EPV game) and we should investigate,
  # not just bump the threshold.
  max_abs_adj <- max(abs(pgd$epv_adj), na.rm = TRUE)
  expect_lt(max_abs_adj, 50,
            label = sprintf("max|epv_adj| (= %.2f)", max_abs_adj))

  # Per-CELL, unweighted mean of each centered channel must be ~0.
  # adj = (p80 - wm_K) * tog_safe is centered per cell K with weights tog_safe,
  # so sum_K((p80 - wm_K) * tog_safe) = 0 by construction and the unweighted
  # mean within K is zero up to floating-point noise. If someone groups the
  # centering on the wrong variable (e.g. season, team, position_group), the
  # within-cell means drift well away from zero.
  #
  # The cell is NOT raw lineup_position — that was true until 2026-08-06, when
  # `6a27aba1` shipped ROLE_REMAP_BENCH and EPV_HITOUT_CENTRE_ON_RUCK. Bench
  # starts are now centered against the role they actually filled, so ~20% of
  # rows sit in a different cell than their lineup_position says, and this test
  # failed for two days asserting a premise the code had deliberately dropped.
  # Reconstruct the key the way `create_player_game_data()` builds it — reading
  # the same constants, so a future flag flip moves the test with the code.
  # NB: grouping by position_group would be wrong — position_group comes from
  # PBP player_position (6-way) and is not a strict refinement of the teams-API
  # lineup_position (20-way), so means within it are not guaranteed to be zero.
  if ("lineup_position" %in% names(pgd)) {
    raw_slot <- as.character(pgd$lineup_position)
    slot <- raw_slot
    if (isTRUE(ROLE_REMAP_BENCH)) {
      slot <- torp:::.remap_bench_role(slot, pgd$player_id, pgd$season,
                                       pgd$position_group)
    }
    role_key <- if (isTRUE(ROLE_USE_LINEUP_GROUP)) {
      torp:::.collapse_lineup_group(slot)
    } else {
      slot
    }

    # Non-vacuity guards. Without these the test passes loudest exactly when it
    # has stopped checking anything: an empty grouping makes all() TRUE, and a
    # remap that silently degraded to a no-op would leave role_key equal to
    # lineup_position, i.e. this would drift back to asserting the pre-2026-08-06
    # premise while still reading green. Compare `slot` to the raw value BEFORE
    # any lineup_group collapse, so this measures the remap and nothing else.
    expect_gt(sum(!is.na(role_key)), 0)
    if (isTRUE(ROLE_REMAP_BENCH)) {
      expect_gt(sum(slot != raw_slot, na.rm = TRUE), 0)
    }

    # The three positional channels. epv_adj itself is excluded: it also carries
    # epv_hitout_adj, which is centered on a different key entirely (below).
    # `[[`-extraction rather than get() inside dt[i, j] — get() there breaks
    # data.table's fast column-reference path, which this repo has paid for.
    cell_means <- function(v, key) {
      keep <- !is.na(key) & !is.na(v)
      vapply(split(v[keep], key[keep]), mean, numeric(1))
    }
    for (ch in c("epv_recv_adj", "epv_disp_adj", "epv_spoil_adj")) {
      m <- cell_means(pgd[[ch]], role_key)
      expect_true(all(abs(m) < 1e-6),
                  info = sprintf("Largest per-cell mean of %s: %.3e", ch,
                                 max(abs(m), na.rm = TRUE)))
    }

    # Hitout is the one channel that only exists for players who ruck, so since
    # 2026-08-06 it is celled on ruck INVOLVEMENT, not on any positional slot
    # (docs/reviews/INT-CENTRING-BUG-2026-08-06.md). With a blend width set, the
    # reference is a smooth function of ruck_contests rather than a per-cell
    # mean, so exact zero is not expected and asserting it would be wrong.
    # Bound it against the channel's own SD instead: on 2024 R1 the worst cell
    # sits at 0.05 SD, while celling hitout on the WRONG key put it at 2.26 SD.
    # 0.25 leaves 5x headroom and still fails an order of magnitude short of a
    # real mis-keying.
    hitout_key <- if (isTRUE(EPV_HITOUT_CENTRE_ON_RUCK)) {
      ifelse(dplyr::coalesce(as.numeric(pgd$ruck_contests), 0) >=
               EPV_RUCK_INVOLVEMENT_MIN, "RUCKS", "OTHER")
    } else {
      role_key
    }
    hm <- cell_means(pgd$epv_hitout_adj, hitout_key)
    hitout_sd <- stats::sd(pgd$epv_hitout_adj, na.rm = TRUE)
    blended <- isTRUE(EPV_HITOUT_CENTRE_ON_RUCK) && EPV_RUCK_BLEND_WIDTH > 0
    tol <- if (blended) 0.25 * hitout_sd else 1e-6
    expect_true(all(abs(hm) < tol),
                info = sprintf(
                  "Largest per-cell mean of epv_hitout_adj: %.3e (tol %.3e, blended = %s)",
                  max(abs(hm), na.rm = TRUE), tol, blended))
  }

  # Semantic guards: position_group is the 6-way class, lineup_position is the
  # ~20-way AFL lineup role. If someone swaps their values in a future
  # refactor, these cardinality bounds fire immediately.
  expect_false(any(pgd$position_group == "MIDFIELDER_FORWARD", na.rm = TRUE),
               info = "MIDFIELDER_FORWARD should be collapsed to MEDIUM_FORWARD")
  pg_n <- dplyr::n_distinct(pgd$position_group, na.rm = TRUE)
  expect_lte(pg_n, 7,
             label = sprintf("n_distinct(position_group) (= %d)", pg_n))
  if ("lineup_position" %in% names(pgd)) {
    lp_n <- dplyr::n_distinct(pgd$lineup_position, na.rm = TRUE)
    expect_gte(lp_n, 15,
               label = sprintf("n_distinct(lineup_position) (= %d)", lp_n))
  }
})

# -----------------------------------------------------------------------------
# load_player_game_data Tests
# -----------------------------------------------------------------------------

test_that("load_player_game_data function exists and is exported", {
  expect_true(exists("load_player_game_data"))
  expect_true("load_player_game_data" %in% getNamespaceExports("torp"))
})

test_that("load_player_game_data has correct function signature", {
  fn_args <- names(formals(load_player_game_data))
  expect_true("seasons" %in% fn_args)
})
