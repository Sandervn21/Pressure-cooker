# Which goals matter? (Q2) Follows the explained-variance section: does what readiness explains
# hold for pupils the model has not seen, and which subjects and goals inside readiness carry it?
# Uses the cross-validated elastic net of R/cv_models.R.
# Needs R/explained_variance.R and R/cv_models.R to have been sourced.

# The client's 15 goals before group 3, short labels (materials: goals_before_group_3.csv)
transition_goal_labels <- c(
  `TG11.2_E2` = "orders numerals to 10", `MK6.3_E1` = "makes a pattern of three objects",
  `SA3.3_E2` = "confidence in own abilities", `TG10.3_E2` = "names numerals to 20",
  `BT3.2_E1` = "perseveres", `AO1.3_E1` = "repeats 4-6 words", `TZ1.2_E1` = "multi-step instruction",
  `VO3.3_E2` = "recognises 10-15 letters", `KM4.2_E2` = "handwriting patterns",
  `AO2.4_E1` = "hears a sound in a sequence", `BV2.4_E2` = "listens critically",
  `MK7.3_E2` = "classifies objects", `KM3.3_E2` = "draws shapes, numerals, letters",
  `TG7.3_E1` = "adds and subtracts with objects", `TG3.3_E2` = "counts on to 20"
)

goals_cito_tests <- c(
  cito_g3_maths = "Cito maths, end of group 3", cito_g3_tmt = "Cito TMT, end of group 3",
  cito_g3_spelling = "Cito spelling, end of group 3"
)

# --- Class check: do the classes differ on Cito as they do on teacher-scored readiness? -------

class_check_measures <- c(
  ready_maths = "Maths readiness (teacher-scored)",
  goals_cito_tests
)

class_cito_check <- imap(class_check_measures, function(label, col) {
  d <- ev_data %>%
    select(all_of(c(col, "class_1_2b", "cohort"))) %>%
    drop_na() %>%
    droplevels() %>%
    mutate(y = as.numeric(scale(.data[[col]])))
  controls <- if (nlevels(d$cohort) > 1) "cohort" else character(0)
  est <- coef(summary(lm(reformulate(c("class_1_2b", controls), response = "y"), data = d)))["class_1_2b", ]
  tibble(
    measure = label,
    n = nrow(d),
    pupils_1_2a = sum(d$class_1_2b == 0),
    pupils_1_2b = sum(d$class_1_2b == 1),
    difference_sd = est[[1]],
    ci_low = est[[1]] - qnorm(0.975) * est[[2]],
    ci_high = est[[1]] + qnorm(0.975) * est[[2]],
    p = est[[4]]
  )
}) %>%
  list_rbind()

# --- Goal-level readiness: last score before group 3, relative to the class ------------------

goal_scores <- last_obs %>%
  filter(code %in% names(transition_goal_labels)) %>%
  select(name_key, code, score) %>%
  inner_join(select(ev_data, name_key, kg_class), by = "name_key") %>%
  group_by(code, kg_class) %>%
  mutate(score = score - mean(score)) %>% # compared with classmates
  ungroup() %>%
  mutate(code = paste0("goal_", code)) %>%
  pivot_wider(id_cols = name_key, names_from = code, values_from = score)
goal_columns <- intersect(paste0("goal_", names(transition_goal_labels)), names(goal_scores))

# --- Domain-level readiness: domain = letters before the first digit of the goal code ----------
# (the Domein column mixes codes and full names across school years, the goal code does not)

domain_names <- c(
  TG = "counting and numbers", MT = "measurement", MK = "geometry",
  AO = "auditory development", VO = "visual development",
  BV = "book orientation and story comprehension", IT = "interactive language use",
  SA = "social autonomy", SF = "social flexibility", SI = "social initiative", IB = "impulse control",
  IV = "empathy", WB = "well-being", BT = "engagement", TZ = "task focus and independence",
  KM = "fine motor skills",
  ZW = "swinging", KL = "climbing", BL = "balancing", OK = "inverting and rolling", HL = "running",
  SR = "jumping", MI = "aiming", JL = "juggling", TS = "tag games", SS = "rough-and-tumble play",
  BM = "moving to music", SP = "play"
)
# Subjects whose domains enter the models (the same subjects as readiness by subject)
domain_subjects <- c("Rekenen", "Taal", "SEO", "Kleine motoriek", "Grote motoriek")
domain_min_coverage <- 0.5 # share of pupils who reached group 3 with the domain observed

goal_domain <- function(code) str_extract(as.character(code), "^[A-Z]+(?=[0-9])")

domain_kind <- function(code) {
  code <- as.character(code)
  case_when(
    !is.na(goal_domain(code)) ~ goal_domain(code),
    str_detect(code, "^[A-Z]+_[0-9]+_") ~ "(code with a level, e.g. BV_100_JA1)",
    str_detect(code, "^[0-9A-F]{6}$") ~ "(number-only code)",
    TRUE ~ "(not a goal code)"
  )
}

domain_pupils_reached <- last_obs %>%
  semi_join(ev_data, by = "name_key") %>%
  mutate(domain = goal_domain(code)) %>%
  filter(!is.na(domain)) %>%
  group_by(domain) %>%
  summarise(share_reached_with_domain = n_distinct(name_key) / nrow(ev_data), .groups = "drop")

domain_overview <- data_observatie %>%
  transmute(name_key = make_key(Leerling), code = as.character(Leerpadcode), vak = as.character(Vak)) %>%
  mutate(domain = domain_kind(code)) %>%
  group_by(domain) %>%
  summarise(
    subject = names(sort(table(vak), decreasing = TRUE))[1],
    goals = n_distinct(code),
    observations = n(),
    pupils = n_distinct(name_key),
    .groups = "drop"
  ) %>%
  left_join(domain_pupils_reached, by = "domain") %>%
  mutate(
    name = coalesce(unname(domain_names[domain]), if_else(str_starts(domain, "\\("), "", "(no name known)")),
    share_reached_with_domain = coalesce(share_reached_with_domain, 0),
    in_models = subject %in% domain_subjects & !str_starts(domain, "\\(") &
      share_reached_with_domain >= domain_min_coverage
  ) %>%
  arrange(desc(in_models), subject, desc(observations)) %>%
  select(domain, name, subject, goals, observations, pupils, share_reached_with_domain, in_models)

model_domains <- domain_overview$domain[domain_overview$in_models]

# Mean of the last scores before group 3 per domain, standardised within each class
domain_scores <- last_obs %>%
  mutate(domain = goal_domain(code)) %>%
  filter(domain %in% model_domains) %>%
  select(name_key, domain, score) %>%
  inner_join(select(ev_data, name_key, kg_class), by = "name_key") %>%
  group_by(name_key, kg_class, domain) %>%
  summarise(level = mean(score), .groups = "drop") %>%
  group_by(domain, kg_class) %>%
  mutate(level = if (n() > 1 && sd(level) > 0) (level - mean(level)) / sd(level) else level - mean(level)) %>%
  ungroup() %>%
  mutate(domain = paste0("dom_", domain)) %>%
  pivot_wider(id_cols = name_key, names_from = domain, values_from = level)
domain_columns <- intersect(paste0("dom_", model_domains), names(domain_scores))

# --- Model data: one row per pupil and Cito test -------------------------------------------

cohort_dummies <- model.matrix(~cohort, ev_data)[, -1, drop = FALSE]
colnames(cohort_dummies) <- sub("^cohort", "cohort_", colnames(cohort_dummies))

goals_base <- bind_cols(ev_data, as_tibble(cohort_dummies)) %>%
  left_join(domain_scores, by = "name_key", relationship = "one-to-one") %>%
  left_join(goal_scores, by = "name_key", relationship = "one-to-one")

goals_model_data <- imap(goals_cito_tests, function(label, col) {
  goals_base %>%
    filter(!is.na(.data[[col]])) %>%
    mutate(Test = label, cito_score = .data[[col]])
}) %>%
  list_rbind()

kg_factor_columns <- unname(unlist(ev_blocks[c("Time", "Classroom", "Extracurricular", "Support")]))
subject_columns <- unname(ev_blocks$Readiness)
cohort_columns <- colnames(cohort_dummies)

# The same blocks as in the explained-variance section, added one step at a time
goals_models <- list(
  `average only` = character(0),
  cohort = cohort_columns,
  `+ kindergarten factors` = c(cohort_columns, kg_factor_columns),
  `+ readiness by subject` = c(cohort_columns, kg_factor_columns, subject_columns),
  `+ readiness by domain` = c(cohort_columns, kg_factor_columns, subject_columns, domain_columns),
  `+ readiness by goal` = c(cohort_columns, kg_factor_columns, subject_columns, domain_columns, goal_columns)
)

set.seed(20261019)
goals_cv <- run_cv(
  goals_model_data, unname(goals_cito_tests), goals_models,
  lm_models = "cohort", coef_models = c("+ readiness by subject", "+ readiness by domain", "+ readiness by goal"),
  n_repeats = 10L, n_outer_folds = 5L
)

# --- Prediction for new pupils, next to the in-sample explained variance ----------------------

in_sample_r2 <- ev_variance_summary %>%
  filter(outcome %in% goals_cito_tests) %>%
  transmute(
    Test = outcome,
    model = if_else(framing == "kindergarten factors", "+ kindergarten factors", "+ readiness by subject"),
    in_sample_r2 = cohort_r2 + total_r2_above_cohort
  ) %>%
  bind_rows(
    ev_variance_summary %>%
      filter(outcome %in% goals_cito_tests) %>%
      distinct(Test = outcome, in_sample_r2 = cohort_r2) %>%
      mutate(model = "cohort")
  )

goals_prediction <- goals_cv$predictions %>%
  group_by(Test, model, repetition) %>%
  summarise(
    rmse = sqrt(mean((observed - predicted)^2)),
    r2 = 1 - mean((observed - predicted)^2) / mean((observed - mean(observed))^2),
    .groups = "drop"
  ) %>%
  group_by(Test, model) %>%
  summarise(
    mean_rmse = mean(rmse),
    predicted_r2 = mean(r2),
    predicted_r2_low = quantile(r2, 0.1, names = FALSE),
    predicted_r2_high = quantile(r2, 0.9, names = FALSE),
    .groups = "drop"
  ) %>%
  left_join(goals_cv$status %>% select(Test, n = n_pupils), by = "Test") %>%
  left_join(in_sample_r2, by = c("Test", "model")) %>%
  mutate(model = factor(model, levels = names(goals_models))) %>%
  arrange(match(Test, goals_cito_tests), model) %>%
  select(Test, model, n, mean_rmse, predicted_r2, predicted_r2_low, predicted_r2_high, in_sample_r2)

goals_prediction_plot <- goals_prediction %>%
  mutate(Test = factor(Test, levels = goals_cito_tests)) %>%
  ggplot(aes(predicted_r2, model)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_errorbar(aes(xmin = predicted_r2_low, xmax = predicted_r2_high), width = 0.2, orientation = "y") +
  geom_point(size = 2.2) +
  geom_point(aes(x = in_sample_r2), shape = 4, size = 2.5, colour = "#D55E00", na.rm = TRUE) +
  facet_wrap(~Test, nrow = 1) +
  scale_y_discrete(limits = rev(names(goals_models))) +
  labs(
    x = "Share of variance predicted for new pupils (dot; bar: 10th-90th percentile over repeats). Cross: in-sample R²",
    y = NULL
  ) +
  theme_minimal(base_size = 11)

# --- Which subjects and goals are kept ------------------------------------------------------

predictor_label <- function(x) {
  goal <- sub("^goal_", "", x)
  case_when(
    x %in% names(ev_feature_labels) ~ unname(ev_feature_labels[x]),
    str_starts(x, "goal_") ~ paste0("goal: ", unname(transition_goal_labels[goal]), " (", goal, ")"),
    str_starts(x, "dom_") & sub("^dom_", "", x) %in% names(domain_names) ~
      paste0("domain: ", unname(domain_names[sub("^dom_", "", x)]), " (", sub("^dom_", "", x), ")"),
    str_starts(x, "dom_") ~ paste0("domain: ", sub("^dom_", "", x)),
    str_starts(x, "cohort_") ~ paste0("leaving year ", sub("cohort_", "", x)),
    TRUE ~ x
  )
}
predictor_block <- function(x) {
  case_when(
    str_starts(x, "goal_") ~ "Readiness by goal",
    str_starts(x, "dom_") ~ "Readiness by domain",
    x %in% names(ev_feature_block) ~ unname(ev_feature_block[x]),
    TRUE ~ "Cohort"
  )
}

goals_selection <- goals_cv$coefficients %>%
  group_by(Test, model, predictor) %>%
  summarise(
    available_pct = 100 * mean(available_in_training),
    selected_pct = 100 * mean(selected),
    mean_coefficient = mean(standardized_coefficient),
    positive_when_selected_pct = if (any(selected)) 100 * mean(standardized_coefficient[selected] > 0) else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(block = predictor_block(predictor), predictor = predictor_label(predictor)) %>%
  filter(block != "Cohort") %>%
  arrange(match(Test, goals_cito_tests), model, desc(selected_pct))

selection_plot <- function(model_name, blocks) {
  goals_selection %>%
    filter(model == model_name, block %in% blocks) %>%
    mutate(
      Test = factor(Test, levels = goals_cito_tests),
      predictor = fct_reorder(predictor, selected_pct, .fun = mean),
      direction = case_when(
        is.na(positive_when_selected_pct) ~ "never kept",
        positive_when_selected_pct >= 50 ~ "mostly positive",
        TRUE ~ "mostly negative"
      )
    ) %>%
    ggplot(aes(selected_pct, predictor, fill = direction)) +
    geom_col(width = 0.7) +
    facet_wrap(~Test, nrow = 1) +
    scale_fill_manual(values = c("mostly positive" = "#0072B2", "mostly negative" = "#D55E00", "never kept" = "grey80")) +
    scale_x_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
    labs(x = "Share of the 50 fits that keep the predictor", y = NULL, fill = NULL) +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom")
}

goals_domain_plot <- selection_plot("+ readiness by domain", c("Readiness", "Readiness by domain", "Time"))
goals_selection_plot <- selection_plot("+ readiness by goal", c("Readiness by goal", "Time"))
