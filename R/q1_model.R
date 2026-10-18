# Q1: is the relationship between time in kindergarten and development
# moderated by extracurricular activities?
# Needs R/cleanup.R to have been sourced.

# Number of distinct extracurricular lessons per pupil
activities_per_pupil <- data_extra |>
  filter(!is.na(`Student`)) |>
  group_by(`Student`) |>
  summarise(
    n_activities = n_distinct(Lesson, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(name_key = make_key(Student)) |>
  arrange(desc(n_activities))

# Development = learning-goal scores z-scored per goal, averaged per pupil
dev_per_pupil <- average_scores |>
  mutate(name_key = make_key(Leerling)) |>
  filter(!is.na(name_key), name_key != "") |>
  group_by(Leerpadcode) |>
  mutate(z = as.numeric(scale(mean_score))) |>
  ungroup() |>
  group_by(name_key) |>
  summarise(development = mean(z, na.rm = TRUE),
            n_goals = sum(!is.na(z)),
            .groups = "drop") |>
  filter(!is.nan(development))

q1_data <- data_clean |>
  mutate(name_key = make_key(Name)) |>
  select(name_key, kindergarten_months) |>
  left_join(select(activities_per_pupil, name_key, n_activities),
            by = "name_key", relationship = "one-to-one") |>
  inner_join(dev_per_pupil, by = "name_key", relationship = "one-to-one") |>
  mutate(
    kg_c  = kindergarten_months - mean(kindergarten_months, na.rm = TRUE),
    act_c = n_activities - mean(n_activities, na.rm = TRUE)
  )

# Pupils at each stage
q1_sample_sizes <- tibble(
  stage = c("data_clean", "with learning-goal scores", "complete cases in model"),
  n = c(n_distinct(data_clean$Name),
        nrow(q1_data),
        nrow(drop_na(q1_data, kg_c, act_c, development)))
)

m_q1 <- lm(development ~ kg_c * act_c, data = q1_data)

q1_plot <- ggplot(q1_data, aes(kindergarten_months, development, colour = n_activities)) +
  geom_point(size = 2.5) +
  geom_smooth(aes(group = 1), method = "lm", formula = y ~ x, colour = "black") +
  scale_colour_viridis_c(name = "Number of\nactivities") +
  labs(x = "Months in kindergarten", y = "Development (average z-score)") +
  theme_minimal(base_size = 13)
