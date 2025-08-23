library(dplyr)
library(stringr)
library(tidyr)
library(ellmer)

inj2 <- inj_df %>% filter(!str_starts(player, "Updated:"))

norm_last <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[^a-z ]", " ") |>
    str_squish() |>
    word(-1)
}

pairs <- tidyr::crossing(
  name_ratings = unique(tr$player_name),
  name_inj     = unique(inj2$player)
) %>%
  filter(norm_last(name_ratings) == norm_last(name_inj))

# ✅ Build a VECTOR of prompts with interpolate()
prompts <- ellmer::interpolate(
  "Answer ONLY TRUE or FALSE.
Are these the same AFL player? Allow common nicknames (Cam=Cameron, Josh=Joshua, Tom=Thomas, Matthew=Matt , etc.).
Name A: {{name_ratings}}
Name B: {{name_inj}}",
  name_ratings = pairs$name_ratings,
  name_inj     = pairs$name_inj
)

# Model setup (use a valid id; e.g. gpt-5-mini)
chat <- ellmer::chat_openai(
  model  = "gpt-5-mini",
  echo   = "none"
  # params = ellmer::params(temperature = 0)
)  # model interface docs: :contentReference[oaicite:2]{index=2}

# Structured boolean output (no string parsing needed)
is_match <- ellmer::parallel_chat_structured(
  chat,
  prompts,
  max_active = 200,
  type = ellmer::type_boolean("TRUE iff same AFL player; be conservative.")  # :contentReference[oaicite:3]{index=3}
)

crosswalk <- pairs %>%
  mutate(is_match = is_match) %>%
  filter(is_match) %>%
  distinct(name_ratings, name_inj)

crosswalk %>% filter(name_ratings != name_inj)

tr_joined <-
  torp_ratings(2025, get_afl_week("next")) %>%
  left_join(crosswalk, by = c("player_name" = "name_ratings")) %>%
  left_join(inj2,        by = c("name_inj"    = "player")) %>%
  mutate(estimated_return = coalesce(estimated_return, "None"))
