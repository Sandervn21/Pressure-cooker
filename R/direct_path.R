# Direct path (Q1): do pupils with more time in kindergarten do better in group 3 and 4?
# In this export time in kindergarten equals (age at the start of group 3 - 4), so the
# estimates are for time and age together.
# Needs R/pupil_table.R to have been sourced.

direct_outcomes <- tribble(
  ~outcome, ~label, ~type, ~role,
  "cito_g3_maths", "Cito maths, end of group 3", "linear", "primary",
  "cito_g3_tmt", "Cito TMT (reading speed), end of group 3", "linear", "primary",
  "cito_g3_spelling", "Cito spelling, end of group 3", "linear", "primary",
  "support_g3", "Any support in group 3", "logistic", "secondary",
  "wellbeing_g3", "Well-being in group 3 (1-3)", "linear", "secondary",
  "cito_g3_reading", "Cito reading comprehension, end of group 3", "linear", "few pupils",
  "cito_g4_maths", "Cito maths, end of group 4", "linear", "few pupils",
  "cito_g4_tmt", "Cito TMT, end of group 4", "linear", "few pupils"
)

direct_data <- pupils_table %>%
  filter(reached_g3) %>%
  mutate(
    kg_6_months = months_to_g3 / 6,
    under_24_months = as.numeric(months_to_g3 < 24),
    cohort = factor(cohort),
    kg_class = factor(kg_class),
    support_g3 = as.numeric(support_g3)
  )

# One model: the exposure's effect on one outcome, with or without controls
fit_direct <- function(outcome, type, exposure, controlled) {
  d <- direct_data %>%
    select(all_of(c(outcome, exposure, "cohort", "kg_class"))) %>%
    drop_na() %>%
    droplevels()
  controls <- if (controlled) c("cohort", "kg_class")[c(nlevels(d$cohort) > 1, nlevels(d$kg_class) > 1)] else character(0)
  empty <- tibble(n = nrow(d), estimate = NA_real_, ci_low = NA_real_, ci_high = NA_real_, p = NA_real_, per_sd = NA_real_)
  if (nrow(d) < 10 || n_distinct(d[[exposure]]) < 2 || (type == "logistic" && n_distinct(d[[outcome]]) < 2)) {
    return(empty)
  }
  f <- reformulate(c(exposure, controls), response = outcome)
  fit <- if (type == "logistic") glm(f, data = d, family = binomial) else lm(f, data = d)
  est <- coef(summary(fit))[exposure, ]
  b <- est[[1]]
  se <- est[[2]]
  ci <- b + c(-1, 1) * qnorm(0.975) * se
  if (type == "logistic") {
    tibble(n = nrow(d), estimate = exp(b), ci_low = exp(ci[1]), ci_high = exp(ci[2]), p = est[[4]], per_sd = NA_real_)
  } else {
    tibble(n = nrow(d), estimate = b, ci_low = ci[1], ci_high = ci[2], p = est[[4]], per_sd = b / sd(d[[outcome]]))
  }
}

fit_all <- function(exposure) {
  direct_outcomes %>%
    crossing(controls = c("none", "cohort + class")) %>%
    mutate(fit = pmap(list(outcome, type, controls), ~ fit_direct(..1, ..2, exposure, ..3 != "none"))) %>%
    unnest(fit) %>%
    mutate(scale = if_else(type == "logistic", "odds ratio", "points")) %>%
    arrange(match(outcome, direct_outcomes$outcome), match(controls, c("none", "cohort + class"))) %>%
    select(outcome = label, role, controls, n, scale, estimate, ci_low, ci_high, p, per_sd)
}

# Per 6 extra months in kindergarten
direct_months <- fit_all("kg_6_months")
# Less than 24 months in kindergarten (vs 24 or more)
direct_under_24 <- fit_all("under_24_months")

# --- Equity: lowest bands by time in kindergarten -------------------------------------------

kg_thirds <- direct_data %>%
  filter(has_cito_g3) %>%
  mutate(kg_third = factor(c("Short", "Medium", "Long")[ntile(months_to_g3, 3)], levels = c("Short", "Medium", "Long")))

direct_equity <- kg_thirds %>%
  pivot_longer(
    c(cito_g3_maths, cito_g3_tmt, cito_g3_spelling, low_band_g3_maths, low_band_g3_tmt, low_band_g3_spelling),
    names_to = c(".value", "test"),
    names_pattern = "(cito|low_band)_g3_(.*)"
  ) %>%
  filter(!is.na(cito)) %>%
  group_by(test, kg_third) %>%
  summarise(
    pupils = n(),
    months_range = paste0(round(min(months_to_g3)), "-", round(max(months_to_g3))),
    mean_ability = mean(cito),
    pct_band_IV_V = 100 * mean(low_band, na.rm = TRUE),
    .groups = "drop"
  )

# --- Plot -----------------------------------------------------------------------------------

direct_plot <- direct_data %>%
  select(months_to_g3, g3_timing, cito_g3_maths, cito_g3_tmt, cito_g3_spelling) %>%
  pivot_longer(starts_with("cito_"), names_to = "test", values_to = "ability") %>%
  filter(!is.na(ability)) %>%
  mutate(test = recode(test, cito_g3_maths = "Maths", cito_g3_tmt = "TMT (reading speed)", cito_g3_spelling = "Spelling")) %>%
  ggplot(aes(months_to_g3, ability)) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "black", linewidth = 0.8) +
  geom_point(aes(colour = g3_timing), size = 2, alpha = 0.8) +
  facet_wrap(~test) +
  scale_colour_manual(
    values = c("earlier than most" = "#D55E00", "usual" = "grey45", "later than most" = "#0072B2"),
    breaks = c("earlier than most", "usual", "later than most"),
    name = "Group 3"
  ) +
  labs(x = "Months in kindergarten", y = "Cito ability score, end of group 3") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")
