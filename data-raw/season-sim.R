### season sim

fix_df <-
  fixtures %>%
  select(providerId, home.team.providerId, away.team.providerId, team_type) %>%
  pivot_longer(
    cols = ends_with("team.providerId"),
    names_to = "team_type",
    values_to = "team.providerId"
  ) %>%
  select(providerId, team.providerId)


result_df <-
  result_df %>%
  left_join(team_mdl_df)
