### overhauls
overhaul out of bounds value
if you kick it out of bounds pos team might be -1 but player still gets positive credit. e.g. CD_M20220142402 play 1149
?? is sign of out of bounds hint at who won hitout?? - NOPE DONT THINK SO
### Errors
CD_M20220142403 play 179, out of bounds wrong x values

# semi fixed
CD_M20220142402 play 398, out of bounds wrong x values
 -- fixed with lag(n=3L) but causes more issues that it fixes.
CD_M20220142402 play 616, free kick / out of bounds wrong x values (theres a 50m penno as well to hayward)

### Fixed
CD_M20220142403 play 81, out of bounds logic is wrong
CD_M20220142403 play 379, team_ids are wrong
CD_M20220142402 play 53, out of bounds wrong x values
CD_M20220142402 play 117, out of bounds wrong x values

CD_M20220142402 play 353, triple throw in
# new x logic
CD_M20220142403 play 34-35, out of bounds wrong x values
CD_M20220142403 play 212-213, out of bounds wrong x values
CD_M20220142403 play 387-388, out of bounds wrong x values
CD_M20220142403 play 659-660, out of bounds wrong x values

### testing

  df <- df %>% dplyr::mutate(
  team_id_mdl2 = dplyr::case_when(
    throw_in == 1 ~ dplyr::lead(team_id),
    TRUE ~ team_id
  ),
   team_id_mdl3 = zoo::na.locf0(team_id_mdl2, fromLast = TRUE),
   team_id_mdl4 = zoo::na.locf0(team_id_mdl3)
  )
