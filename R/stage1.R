# Stage 1 (Q1 and Q2): which groups 1-2 factors go with readiness at the end of kindergarten?
# Needs R/pupil_table.R to have been sourced.

stage1_outcomes <- tribble(
  ~outcome, ~label,
  "transition_share", "Goals before group 3 achieved (share)",
  "basic_share", "Basic goals groups 1-2 achieved (share)",
  "ready_maths", "Maths (last score, 0-3)",
  "ready_language", "Language (last score, 0-3)",
  "ready_social_emotional", "Social-emotional (last score, 0-3)",
  "ready_fine_motor", "Fine motor (last score, 0-3)",
  "ready_gross_motor", "Gross motor (last score, 0-3)"
)

stage1_factors <- c(
  kg_6_months = "6 more months in kindergarten",
  `kg_class1/2b` = "Class 1/2b (vs 1/2a)",
  lessons_5_per_kg_year = "5 more lessons per kindergarten year",
  support_kgTRUE = "Support during kindergarten (vs none)"
)

stage1_data <- pupils_table %>%
  filter(reached_g3) %>%
  mutate(
    kg_6_months = months_to_g3 / 6,
    lessons_5_per_kg_year = lessons_per_kg_year / 5,
    kg_class = factor(kg_class, levels = c("1/2a", "1/2b")),
    cohort = factor(cohort)
  )

# Linear regression of one readiness outcome on the factors, cohort as control
fit_stage1 <- function(outcome, factors) {
  d <- stage1_data %>%
    select(all_of(c(outcome, factors, "cohort"))) %>%
    drop_na() %>%
    droplevels()
  controls <- if (nlevels(d$cohort) > 1) "cohort" else character(0)
  fit <- lm(reformulate(c(factors, controls), response = outcome), data = d)
  est <- coef(summary(fit))
  terms <- intersect(rownames(est), names(stage1_factors))
  tibble(
    factor = unname(stage1_factors[terms]),
    n = nrow(d),
    estimate = est[terms, 1],
    ci_low = est[terms, 1] - qnorm(0.975) * est[terms, 2],
    ci_high = est[terms, 1] + qnorm(0.975) * est[terms, 2],
    p = est[terms, 4],
    outcome_sd = sd(d[[outcome]])
  )
}

fit_stage1_all <- function(factors) {
  stage1_outcomes %>%
    mutate(fit = map(outcome, fit_stage1, factors = factors)) %>%
    unnest(fit) %>%
    mutate(
      per_sd = estimate / outcome_sd,
      per_sd_low = ci_low / outcome_sd,
      per_sd_high = ci_high / outcome_sd
    )
}

# Main model: time in kindergarten, class and lessons together
stage1_main <- fit_stage1_all(c("kg_6_months", "kg_class", "lessons_5_per_kg_year"))
# Smaller model with support during kindergarten (only years the support file covers)
stage1_support <- fit_stage1_all(c("support_kg", "kg_6_months", "kg_class")) %>%
  filter(factor == stage1_factors[["support_kgTRUE"]])

stage1_table <- function(d) {
  d %>% select(outcome = label, factor, n, estimate, ci_low, ci_high, p, per_sd)
}

# Compact overview: effect in SD of the outcome, 95% interval, * when p < .05
stage1_overview <- stage1_main %>%
  mutate(cell = sprintf("%.2f [%.2f, %.2f]%s", per_sd, per_sd_low, per_sd_high, if_else(p < 0.05, "*", ""))) %>%
  select(outcome = label, n, factor, cell) %>%
  pivot_wider(names_from = factor, values_from = cell)

stage1_plot <- bind_rows(stage1_main, stage1_support) %>%
  mutate(
    label = factor(label, levels = rev(stage1_outcomes$label)),
    factor = factor(factor, levels = unname(stage1_factors))
  ) %>%
  ggplot(aes(per_sd, label)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_errorbar(aes(xmin = per_sd_low, xmax = per_sd_high), width = 0.2, orientation = "y") +
  geom_point(size = 2) +
  facet_wrap(~factor, nrow = 1) +
  labs(x = "Difference in readiness, in standard deviations (95% interval)", y = NULL) +
  theme_minimal(base_size = 11)

# Classroom quality of the kindergarten classes, per school year (CLASS domain averages)
class_quality_kg_classes <- CLASS_yearly %>%
  ungroup() %>%
  filter(class_group %in% c("1/2a", "1/2b")) %>%
  transmute(
    school_year = paste0(school_year_start, "/", school_year_start + 1),
    class = class_group,
    observations = n_assessments,
    emotional_support = `mean_Total Emotional Support`,
    classroom_organization = `mean_Total Classroom Organization`,
    instructional_support = `mean_Total Instructional Support`
  ) %>%
  arrange(school_year, class)
