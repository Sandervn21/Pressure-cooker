# Q1 robustness: how much does the activities effect depend on choices made in q1_model.R?
# Walks from the reported model to an alternative specification one change at a time.
# Needs R/q1_model.R and R/pupil_table.R to have been sourced.

# Last observation date in the export, used as "today" for time in school
data_date <- max(as_date_cell(data_observatie$Datum, "mdy"), na.rm = TRUE)

bridge_data <- q1_data %>%
  left_join(
    select(pupils_table, name_key, start_date, g3_year, months_to_g3, transition_share),
    by = "name_key", relationship = "one-to-one"
  ) %>%
  mutate(
    months_in_school = as.numeric(data_date - start_date) / 30.44,
    n_activities_0 = coalesce(n_activities, 0L)
  )

# Activities coefficient from development ~ kindergarten * activities, both centred
fit_step <- function(step, data, outcome, kg, activities, extra = NULL) {
  d <- data %>%
    select(all_of(c(outcome, kg, activities, extra))) %>%
    drop_na() %>%
    mutate(across(all_of(c(kg, activities, extra)), ~ .x - mean(.x)))
  terms <- paste(c(paste(kg, "*", activities), extra), collapse = " + ")
  fit <- lm(reformulate(terms, response = outcome), data = d)
  ci <- confint(fit)[activities, ]
  tibble(
    step = step,
    n = nobs(fit),
    activities_b = coef(fit)[[activities]],
    ci_low = ci[[1]],
    ci_high = ci[[2]],
    p = coef(summary(fit))[activities, "Pr(>|t|)"],
    # sd of the outcome, to compare effects across outcomes
    b_per_sd_outcome = coef(fit)[[activities]] / sd(d[[outcome]])
  )
}

with_g3 <- filter(bridge_data, !is.na(g3_year))

q1_bridge <- bind_rows(
  fit_step("1. As reported (all pupils, no activity record dropped)",
           bridge_data, "development", "kindergarten_months", "n_activities"),
  fit_step("2. No activity record counted as 0",
           bridge_data, "development", "kindergarten_months", "n_activities_0"),
  fit_step("3. + control for months in school (age / cohort)",
           bridge_data, "development", "kindergarten_months", "n_activities_0", "months_in_school"),
  fit_step("4. Only pupils who reached group 3",
           with_g3, "development", "kindergarten_months", "n_activities_0"),
  fit_step("5. + kindergarten = months until group 3 actually started",
           with_g3, "development", "months_to_g3", "n_activities_0"),
  fit_step("6. + outcome = transition goals achieved before group 3",
           with_g3, "transition_share", "months_to_g3", "n_activities_0")
)

# Rank correlation of the step-6 sample (no model)
q1_bridge_rho <- with(
  drop_na(with_g3, transition_share, n_activities_0),
  cor.test(transition_share, n_activities_0, method = "spearman", exact = FALSE)
)

# How the two kindergarten measures relate for pupils who reached group 3
kg_measures <- with_g3 %>%
  select(kindergarten_months, months_to_g3) %>%
  drop_na()

kg_measures_plot <- ggplot(kg_measures, aes(months_to_g3, kindergarten_months)) +
  geom_abline(linetype = "dashed", colour = "grey60") +
  geom_point(size = 2, alpha = 0.7) +
  labs(
    x = "Months from start date to the start of group 3",
    y = "kindergarten_months (cleanup.R)",
    caption = "Dashed line: both measures equal."
  ) +
  theme_minimal(base_size = 13)
