#' Simulate a season of games
#'
#' This function simulates a season of games based on team ratings and fixture data.
#'
#' @param sim_teams A data frame containing team ratings.
#' @param sim_games A data frame containing fixture data.
#' @return A data frame of simulated game results.
#' @importFrom dplyr filter mutate select group_by summarise inner_join left_join case_when
#' @importFrom purrr map_dfr
#' @importFrom stats rnorm
#' @export
sim_season <- function(sim_teams, sim_games) {
  rounds_to_sim <- sim_games %>%
    dplyr::filter(is.na(.data$result)) %>%
    dplyr::pull(.data$roundnum) %>%
    unique() %>%
    sort()
  simmed_games <- purrr::map_dfr(rounds_to_sim, function(round_num) {
    result <- process_games(sim_teams, sim_games, round_num)
    sim_teams <<- result$sim_teams  # Note: This is a side effect
    result$sim_games
  })
  return(simmed_games)
}

#' Process games for a single round
#'
#' This function processes games for a single round of the season.
#'
#' @param sim_teams A data frame containing team ratings.
#' @param sim_games A data frame containing fixture data.
#' @param round_num The round number to process.
#' @return A list containing updated sim_teams and sim_games data frames.
#' @importFrom dplyr filter mutate select inner_join left_join case_when
#' @importFrom stats rnorm
process_games <- function(sim_teams, sim_games, round_num) {
  sim_teams$roundnum <- round_num
  sim_ratings <- sim_teams %>%
    dplyr::select(.data$roundnum, .data$team, .data$torp)
  sim_games <- sim_games %>%
    dplyr::filter(is.na(.data$result)) %>%
    dplyr::inner_join(sim_ratings, by = c("roundnum" = "roundnum", "away_team" = "team")) %>%
    dplyr::rename(away_torp = .data$torp) %>%
    dplyr::inner_join(sim_ratings, by = c("roundnum" = "roundnum", "home_team" = "team")) %>%
    dplyr::rename(home_torp = .data$torp) %>%
    dplyr::mutate(
      estimate = 6 + (.data$home_torp - .data$away_torp),
      wp = 1 / (10^(-.data$estimate / 50) + 1),
      result = dplyr::case_when(
        is.na(.data$result) & .data$roundnum == round_num ~
          as.integer(round(stats::rnorm(dplyr::n(), .data$estimate, 26 + (abs(.data$estimate) / 3)))),
        TRUE ~ as.integer(.data$result)
      ),
      outcome = dplyr::case_when(
        is.na(.data$result) ~ NA_real_,
        .data$result > 0 ~ 1,
        .data$result < 0 ~ 0,
        TRUE ~ 0.5
      ),
      torp_shift = 0.1 * (.data$result - .data$estimate)
    )
  sim_teams <- sim_teams %>%
    dplyr::left_join(
      sim_games %>%
        dplyr::filter(.data$roundnum == round_num) %>%
        dplyr::select(.data$roundnum, .data$away_team, .data$torp_shift),
      by = c("roundnum" = "roundnum", "team" = "away_team")
    ) %>%
    dplyr::mutate(torp = .data$torp - ifelse(!is.na(.data$torp_shift), .data$torp_shift, 0)) %>%
    dplyr::select(-.data$torp_shift) %>%
    dplyr::left_join(
      sim_games %>%
        dplyr::filter(.data$roundnum == round_num) %>%
        dplyr::select(.data$roundnum, .data$home_team, .data$torp_shift),
      by = c("roundnum" = "roundnum", "team" = "home_team")
    ) %>%
    dplyr::mutate(torp = .data$torp + ifelse(!is.na(.data$torp_shift), .data$torp_shift, 0)) %>%
    dplyr::select(-.data$torp_shift)

  # Note: The following line is commented out as `max_ratings` is not defined in the provided code
  # %>% dplyr::left_join(max_ratings, by = c('team'='team')) %>%
  # dplyr::mutate(torp = (4*torp.x + 0.9*torp.y)/5) %>%
  # dplyr::select(-torp.x,-torp.y)

  sim_games <- sim_games %>%
    dplyr::mutate(
      torp_home_round = dplyr::case_when(
        .data$roundnum == round_num ~ .data$home_torp,
        TRUE ~ .data$torp_home_round
      ),
      torp_away_round = dplyr::case_when(
        .data$roundnum == round_num ~ .data$away_torp,
        TRUE ~ .data$torp_away_round
      )
    ) %>%
    dplyr::select(-.data$torp_shift, -.data$home_torp, -.data$away_torp)
  return(list(sim_teams = sim_teams, sim_games = sim_games))
}
