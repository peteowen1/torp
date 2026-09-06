# Weather Impact on AFL Scoring — EDA ----
# Explores whether rain, temperature, and wind affect total match points

library(dplyr)
library(ggplot2)
library(arrow)

devtools::load_all()

# Load data ----

weather <- load_weather()
results <- load_results(TRUE)

# Join weather to results for total points
# Results are normalised at load time — canonical column names
match_data <- weather |>
  inner_join(
    results |>
      mutate(total_points = home_score + away_score) |>
      select(match_id, total_points, home_score, away_score,
             afl_weather_type = weather.weatherType,
             afl_weather_desc = weather.description,
             afl_temp = weather.tempInCelsius),
    by = "match_id"
  )

cat("Matched", nrow(match_data), "of", nrow(weather), "weather records to results\n")

# Summary stats ----

cat("\n--- Rain vs Dry ---\n")
match_data |>
  filter(!is_roof) |>
  group_by(is_rain) |>
  summarise(
    n = n(),
    mean_pts = mean(total_points),
    median_pts = median(total_points),
    sd_pts = sd(total_points),
    .groups = "drop"
  ) |>
  print()

cat("\n--- Roofed vs Open-Air ---\n")
match_data |>
  group_by(is_roof) |>
  summarise(
    n = n(),
    mean_pts = mean(total_points),
    .groups = "drop"
  ) |>
  print()

# Simple t-test: rain vs dry (excluding roof) ----

open_air <- match_data |> filter(!is_roof)
t_result <- t.test(total_points ~ is_rain, data = open_air)
cat("\nt-test (rain vs dry, open-air only):\n")
cat("  Dry mean:", round(t_result$estimate[1], 1), "\n")
cat("  Rain mean:", round(t_result$estimate[2], 1), "\n")
cat("  Difference:", round(diff(t_result$estimate), 1), "points\n")
cat("  p-value:", format.pval(t_result$p.value, digits = 3), "\n")

# Linear model: total points ~ weather ----

lm_fit <- lm(total_points ~ temp_avg + precipitation_total + wind_avg + humidity_avg,
             data = open_air)
cat("\n--- Linear model (open-air) ---\n")
print(summary(lm_fit))

# GAM with smooth terms ----

library(mgcv)

gam_fit <- gam(total_points ~ s(temp_avg) + s(wind_avg) + s(humidity_avg) +
                 s(precipitation_total),
               data = open_air, method = "REML")
cat("\n--- GAM (open-air) ---\n")
print(summary(gam_fit))
cat("\nAIC comparison — linear:", round(AIC(lm_fit), 1),
    " GAM:", round(AIC(gam_fit), 1), "\n")

# Plot 1: Total points by rain vs dry ----

p1 <- ggplot(open_air, aes(x = is_rain, y = total_points, fill = is_rain)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("FALSE" = "#F8B400", "TRUE" = "#4A90D9"),
                    labels = c("Dry", "Rain")) +
  labs(title = "Total Match Points: Rain vs Dry",
       subtitle = "Open-air venues only (Marvel Stadium excluded)",
       x = NULL, y = "Total Points", fill = NULL) +
  scale_x_discrete(labels = c("Dry", "Rain")) +
  theme_minimal()

# Plot 2: Total points vs temperature ----

p2 <- ggplot(open_air, aes(x = temp_avg, y = total_points)) +
  geom_point(alpha = 0.15, size = 1) +
  geom_smooth(method = "loess", color = "#E74C3C", se = TRUE) +
  labs(title = "Total Points vs Temperature",
       x = "Average Temperature (°C)", y = "Total Points") +
  theme_minimal()

# Plot 3: Total points vs wind speed ----

p3 <- ggplot(open_air, aes(x = wind_avg, y = total_points)) +
  geom_point(alpha = 0.15, size = 1) +
  geom_smooth(method = "loess", color = "#3498DB", se = TRUE) +
  labs(title = "Total Points vs Wind Speed",
       x = "Average Wind Speed (km/h)", y = "Total Points") +
  theme_minimal()

# Plot 4: Total points vs precipitation ----

p4 <- ggplot(open_air |> filter(precipitation_total > 0),
             aes(x = precipitation_total, y = total_points)) +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth(method = "loess", color = "#27AE60", se = TRUE) +
  labs(title = "Total Points vs Precipitation (rainy matches only)",
       x = "Total Precipitation (mm, 3hr window)", y = "Total Points") +
  theme_minimal()

# Plot 6: GAM partial effects ----

# Extract smooth predictions for each term on a grid
plot_gam_smooth <- function(gam_model, var, label, color) {
  rng <- range(gam_model$model[[var]])
  newdata <- data.frame(x = seq(rng[1], rng[2], length.out = 200))
  names(newdata) <- var
  # Set other variables to their mean
  for (v in setdiff(names(gam_model$model)[-1], var)) {
    newdata[[v]] <- mean(gam_model$model[[v]])
  }
  preds <- predict(gam_model, newdata = newdata, type = "terms", se.fit = TRUE)
  col_idx <- which(grepl(var, colnames(preds$fit)))
  newdata$fit <- preds$fit[, col_idx]
  newdata$se <- preds$se.fit[, col_idx]

  ggplot(newdata, aes(x = .data[[var]], y = fit)) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.4) +
    geom_ribbon(aes(ymin = fit - 2 * se, ymax = fit + 2 * se), alpha = 0.2, fill = color) +
    geom_line(color = color, linewidth = 1.2) +
    labs(title = paste("GAM smooth:", label),
         x = label, y = "Partial effect on total points") +
    theme_minimal()
}

p6a <- plot_gam_smooth(gam_fit, "temp_avg", "Temperature (°C)", "#E74C3C")
p6b <- plot_gam_smooth(gam_fit, "wind_avg", "Wind Speed (km/h)", "#3498DB")
p6c <- plot_gam_smooth(gam_fit, "humidity_avg", "Humidity (%)", "#8E44AD")
p6d <- plot_gam_smooth(gam_fit, "precipitation_total", "Precipitation (mm)", "#27AE60")

# Plot 5: Venue breakdown — roof vs open-air ----

p5 <- ggplot(match_data, aes(x = is_roof, y = total_points, fill = is_roof)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("FALSE" = "#95A5A6", "TRUE" = "#E67E22"),
                    labels = c("Open-Air", "Roofed")) +
  scale_x_discrete(labels = c("Open-Air", "Roofed")) +
  labs(title = "Total Points: Roofed vs Open-Air Venues",
       x = NULL, y = "Total Points", fill = NULL) +
  theme_minimal()

# TODO: Define weather severity thresholds ----
# This is where you decide what counts as "bad weather" for modeling purposes.
# The function below classifies matches into weather categories.
# Consider: should this be a simple binary, or a multi-level factor?
#
# Approaches:
#   A) Binary: rain > 0.5mm OR wind > 25 km/h → "adverse"
#   B) Ordinal: "fine" / "mild" / "adverse" based on combined score
#   C) Continuous index: weighted combination of rain + wind + temp deviation
#
# Fill in the classify_weather() function body below (5-10 lines).
# Input: data frame with columns temp_avg, precipitation_total, wind_avg, humidity_avg
# Output: same data frame with a new `weather_severity` column

classify_weather <- function(df) {
  # TODO: Implement weather severity classification
  # df |> mutate(weather_severity = ...)
  df
}

# Display plots ----

print(p1)
print(p2)
print(p3)
print(p4)
print(p5)
print(p6a)
print(p6b)
print(p6c)
print(p6d)

cat("\n--- Done! Review plots and stats above. ---\n")
cat("If weather shows meaningful signal, next steps:\n")
cat("  1. Define weather severity thresholds (see classify_weather TODO above)\n")
cat("  2. Add weather features to XGBoost prediction model\n")
cat("  3. Add weather fetch to torpdata release pipeline\n")
