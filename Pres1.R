# Test test

library(tidyverse)

# ---------------------------------------------------------
# 1. Inspect the original dataset
# ---------------------------------------------------------

# Show column names, types, and example values
glimpse(Extracurricular)

# Display the first 10 rows
Extracurricular |>
  slice_head(n = 10)

# Count rows and columns
dim(Extracurricular)

# Count missing values in each column
Extracurricular |>
  summarise(across(everything(), ~ sum(is.na(.x))))

# Inspect lesson types and their frequencies
groups <- Extracurricular |>
  count(Lesson, sort = TRUE)


activities_per_pupil <- Extracurricular |>
  filter(!is.na(`Student`)) |>
  group_by(`Student`) |>
  summarise(
    n_activities = n_distinct(Lesson, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(n_activities))

print(activities_per_pupil)


grade3_data <- Cito_UvA_EN |>
  filter(Grade == "Grade 3") |>
  left_join(
    activities_per_pupil,
    by = c("Name" = "Student"),
    relationship = "many-to-one"
  )


# Extract unique name–ID pairs
student_lookup <- Extracurricular |>
  select(Student, `Student ID`) |>
  filter(!is.na(Student), !is.na(`Student ID`)) |>
  distinct()

# Add IDs to the other dataset by matching names
Cito_with_ID <- Cito_UvA_EN |>
  left_join(
    student_lookup,
    by = c("Name" = "Student"),
    relationship = "many-to-one"
  )

# Inspect the matched names and IDs
Cito_with_ID |>
  select(Name, `Student ID`) |>
  distinct() |>
  print(n = 20)

# Check names that did not match
Cito_with_ID |>
  filter(is.na(`Student ID`)) |>
  distinct(Name) |>
  print(n = Inf)

activities_per_pupil <- Extracurricular |>
  filter(!is.na(`Student`)) |>
  group_by(`Student`) |>
  summarise(
    n_activities = n_distinct(Lesson, na.rm = TRUE),
    .groups = "drop"
  )

grade3_data <- Cito_with_ID |>
  filter(Grade == "Grade 3") |>
  left_join(
    activities_per_pupil,
    by = c("Name" = "Student"),
    relationship = "many-to-one"
  )

plot_data <- grade3_data |>
  filter(!is.na(n_activities), !is.na(Score)) |>
  mutate(
    activity_count = factor(
      n_activities,
      levels = sort(unique(n_activities))
    )
  )

ggplot(plot_data, aes(x = activity_count, y = Score)) +
  geom_boxplot(
    width = 0.5,
    fill = "#DBEAFE",
    colour = "#64748B",
    alpha = 0.7,
    outlier.shape = NA
  ) +
  geom_point(
    position = position_jitter(
      width = 0.15,
      height = 0,
      seed = 123
    ),
    colour = "#2563EB",
    alpha = 0.45,
    size = 2
  ) +
  stat_summary(
    fun = median,
    geom = "point",
    shape = 18,
    size = 4,
    colour = "#D97706"
  ) +
  labs(
    title = "Do Grade 3 scores vary with extracurricular participation?",
    subtitle = "Score distributions by number of different activities",
    x = "Number of different extracurricular activities",
    y = "Grade 3 score",
    caption = paste(
      "Blue points = individual records; orange diamonds = medians.",
      "Boxes show the middle 50% of scores."
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0)
  )

ggplot(plot_data, aes(x = n_activities, y = "Performance band")) +
  geom_point(
    position = position_jitter(
      width = 0.15,
      height = 0,
      seed = 123
    ),
    colour = "#2563EB",
    alpha = 0.5,
    size = 2.5
  ) +
  scale_x_continuous(
    breaks = sort(unique(plot_data$n_activities))
  ) +
  labs(
    title = "Extracurricular participation and Grade 3 scores",
    x = "Number of different extracurricular activities",
    y = "Grade 3 score",
    caption = "Points are shifted slightly horizontally to show overlapping records."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0)
  )

a1 <- average_grades |>
  left_join(activities_per_pupil,
            by=c("Name"="Student"),
            relationship="many-to-one"
  )

a1 |>
  ggplot(aes(x=n_activities, y=mean_grade)) +
  geom_point()


#######

make_key <- function(x) str_to_lower(str_squish(x))

# Extracurricular activities (different lessons per pupil)
activities_per_pupil <- Extracurricular |>
  mutate(name_key = make_key(Student)) |>
  filter(!is.na(name_key), name_key != "") |>
  group_by(name_key) |>
  summarise(n_activities = n_distinct(Lesson, na.rm = TRUE), .groups = "drop")

# Achievement per subject (several rows per pupil)
cito_long <- average_grades |>
  mutate(name_key = make_key(Name)) |>
  filter(!is.na(name_key), name_key != "") |>
  select(name_key, Test, mean_grade, n_scores)

# Step 1: pupil-level data (one row per pupil)
pupil_data <- data_clean |>
  mutate(name_key = make_key(Name)) |>
  left_join(activities_per_pupil, by = "name_key", relationship = "one-to-one")

# Step 2: add the subjects (one row per pupil per subject)
model_long <- pupil_data |>
  inner_join(cito_long, by = "name_key", relationship = "one-to-many") |>
  mutate(intervention = factor(n_interventions)) |>
  group_by(Test) |>
  mutate(
    kg_c  = kindergarten_months - mean(kindergarten_months, na.rm = TRUE),
    act_c = n_activities - mean(n_activities, na.rm = TRUE)
  ) |>
  ungroup()

models <- model_long |>
  split(~ Test) |>
  map(~ lm(mean_grade ~ kg_c * act_c + intervention, data = .x))

# Pupils actually used in each model
map_int(models, nobs)

# Coefficients, standard errors, p-values and 95% CIs
results <- map_dfr(models, broom::tidy, conf.int = TRUE, .id = "Test") |>
  select(Test, term, estimate, std.error, p.value, conf.low, conf.high)

print(results, n = Inf)

# Overall fit
map_dfr(models, broom::glance, .id = "Test") |>
  select(Test, r.squared, adj.r.squared, sigma, statistic, p.value, nobs)

# Residuals vs fitted and Q-Q plot, per subject
par(mfrow = c(2, 2))
plot(models[[1]])
par(mfrow = c(1, 1))


