# Q2: which early learning goals relate to later CITO results?
# Needs R/cleanup.R and R/q1_model.R to have been sourced.

compute_cito_correlations <- function(score_level, grouping_columns) {
  
  matched <- learning_goals %>%
    filter(as.character(level) == score_level) %>%
    mutate(Leerling = trimws(as.character(Leerling))) %>%
    filter(!is.na(Leerling), Leerling != "") %>%
    inner_join(
      average_grades %>%
        mutate(Name = trimws(as.character(Name))) %>%
        filter(!is.na(Name), Name != ""),
      by = c("Leerling" = "Name"),
      relationship = "many-to-many"
    ) %>%
    filter(is.finite(mean_score), is.finite(mean_grade))
  
  matched %>%
    group_by(across(all_of(c(grouping_columns, "Test")))) %>%
    group_modify(~ {
      enough_data <- nrow(.x) >= 3 &&
        sd(.x$mean_score) > 0 &&
        sd(.x$mean_grade) > 0
      
      if (enough_data) {
        result <- cor.test(.x$mean_score, .x$mean_grade, method = "pearson")
        
        tibble(
          n_pupils = nrow(.x),
          correlation = unname(result$estimate),
          p_value = result$p.value
        )
      } else {
        tibble(
          n_pupils = nrow(.x),
          correlation = NA_real_,
          p_value = NA_real_
        )
      }
    }) %>%
    ungroup() %>%
    mutate(p_adjusted = p.adjust(p_value, method = "BH")) %>%
    arrange(desc(abs(correlation)))
}

# Correlations per Vak and CITO test

correlations_vak <- compute_cito_correlations(
  score_level = "Vak",
  grouping_columns = "Vak"
)

# Correlations per Domein within Vak and CITO test
correlations_domein <- compute_cito_correlations(
  score_level = "Domein",
  grouping_columns = c("Vak", "Domein")
)

# Reuse the same heatmap layout for both analyses
plot_cito_correlations <- function(data, title) {
  plot_data <- data %>%
    mutate(
      label = ifelse(
        is.na(correlation),
        paste0("NA\nn=", n_pupils),
        paste0(
          sprintf("%.2f", correlation),
          ifelse(!is.na(p_adjusted) & p_adjusted < 0.05, "*", ""),
          "\nn=", n_pupils
        )
      )
    )
  
  ggplot(plot_data, aes(x = Test, y = score_group, fill = correlation)) +
    geom_tile(color = "white") +
    geom_text(aes(label = label), size = 3) +
    scale_fill_gradient2(
      low = "#2166AC",
      mid = "white",
      high = "#B2182B",
      midpoint = 0,
      limits = c(-1, 1),
      na.value = "grey85",
      name = "Pearson r"
    ) +
    labs(
      title = title,
      x = "CITO test",
      y = NULL,
      caption = "* BH-adjusted p < .05; n = matched pupils; NA = insufficient data or no variation"
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
}

# Vak correlations
correlation_plot_vak <- correlations_vak %>%
  mutate(score_group = as.character(Vak)) %>%
  plot_cito_correlations("Correlations between Vak scores and CITO scores")

# only show the strong correlations for domein

strong_correlations_domein <- correlations_domein %>%
  filter(correlation > 0.7 | correlation < -0.7) %>%
  arrange(desc(abs(correlation))) %>%
  transmute(
    Vak,
    Domein,
    CITO_test = Test,
    n_pupils,
    correlation = round(correlation, 3),
    p_value = signif(p_value, 3),
    p_adjusted = signif(p_adjusted, 3),
    significant_BH = p_adjusted < 0.05
  )

cito_gr3 <- data_grade %>%
  filter(trimws(as.character(Grade)) == "Grade 3") %>%
  mutate(name_key = make_key(Name)) %>%
  filter(!is.na(name_key), name_key != "", !is.na(Test)) %>%
  group_by(name_key, Test) %>%
  summarise(
    cito_score = if (all(is.na(Score))) NA_real_ else mean(Score, na.rm = TRUE),
    .groups = "drop"
  )

vak_lookup <- learning_goals %>%
  filter(as.character(level) == "Vak", !is.na(Vak)) %>%
  distinct(Vak) %>%
  arrange(Vak) %>%
  mutate(predictor = paste0("vak_", row_number()))

vak_scores <- learning_goals %>%
  filter(as.character(level) == "Vak", !is.na(Vak)) %>%
  mutate(name_key = make_key(Leerling)) %>%
  filter(!is.na(name_key), name_key != "") %>%
  left_join(vak_lookup, by = "Vak", relationship = "many-to-one") %>%
  group_by(name_key, predictor) %>%
  summarise(
    mean_score = if (all(is.na(mean_score))) NA_real_ else mean(mean_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = predictor, values_from = mean_score)

activity_counts <- activities_per_pupil %>% select(name_key, n_activities)
stopifnot(!anyDuplicated(activity_counts$name_key))

pupil_predictors <- data_clean %>%
  mutate(name_key = make_key(Name)) %>%
  select(Name, name_key, kindergarten_months, n_interventions) %>%
  left_join(vak_scores, by = "name_key", relationship = "one-to-one") %>%
  left_join(activity_counts, by = "name_key", relationship = "one-to-one")

model_data <- cito_gr3 %>%
  inner_join(pupil_predictors, by = "name_key", relationship = "many-to-one")

vak_predictors <- vak_lookup$predictor
model_predictors <- list(
  baseline = character(0),
  time = "kindergarten_months",
  readiness = c("kindergarten_months", vak_predictors),
  context = c("kindergarten_months", vak_predictors, "n_activities", "n_interventions")
)
stopifnot(all(model_predictors$context %in% names(model_data)))
stopifnot(all(vapply(model_data[model_predictors$context], is.numeric, logical(1))))

# All four models use the same pupils with a valid outcome.
# Missing predictors are imputed INSIDE each training split, not before CV.
model_data <- model_data %>% filter(is.finite(cito_score))

predictor_missingness <- model_data %>%
  group_by(Test) %>%
  summarise(
    across(all_of(model_predictors$context), ~ mean(!is.finite(.x))),
    .groups = "drop"
  ) %>%
  pivot_longer(-Test, names_to = "predictor", values_to = "proportion_missing") %>%
  left_join(vak_lookup, by = "predictor")

# Training-only median imputation and standardization.
# Drop predictors that are entirely missing or constant in that training split.
prepare_split <- function(train, validation, predictors) {
  a <- as.matrix(train[, predictors, drop = FALSE])
  b <- as.matrix(validation[, predictors, drop = FALSE])
  storage.mode(a) <- storage.mode(b) <- "double"
  a[!is.finite(a)] <- NA_real_
  b[!is.finite(b)] <- NA_real_
  med <- apply(a, 2, function(x) if (all(is.na(x))) NA_real_ else median(x, na.rm = TRUE))
  keep <- is.finite(med)
  a <- a[, keep, drop = FALSE]
  b <- b[, keep, drop = FALSE]
  med <- med[keep]
  if (ncol(a) == 0) return(list(train = a, validation = b))
  for (j in seq_len(ncol(a))) {
    a[is.na(a[, j]), j] <- med[j]
    b[is.na(b[, j]), j] <- med[j]
  }
  scales <- apply(a, 2, sd)
  keep <- is.finite(scales) & scales > 0
  a <- a[, keep, drop = FALSE]
  b <- b[, keep, drop = FALSE]
  if (ncol(a) == 0) return(list(train = a, validation = b))
  centers <- colMeans(a)
  scales <- scales[keep]
  list(
    train = sweep(sweep(a, 2, centers, "-"), 2, scales, "/"),
    validation = sweep(sweep(b, 2, centers, "-"), 2, scales, "/")
  )
}

# Penalized glmnet needs at least two columns; a zero dummy adds no information.
pad_matrix <- function(x) {
  if (ncol(x) == 1) cbind(x, dummy_zero = 0) else x
}

# Fixed alpha = .5 balances shrinkage and selection for correlated Vakken.
# Tune penalty strength using INNER CV; parameterize it relative to lambda_max.
alpha_value <- 0.5
penalty_fractions <- exp(seq(log(1), log(0.001), length.out = 40))

fit_path <- function(x, y) {
  x <- pad_matrix(x)
  lambda_max <- max(abs(drop(crossprod(x, y - mean(y))))) / (length(y) * alpha_value)
  if (!is.finite(lambda_max) || lambda_max <= .Machine$double.eps) return(NULL)
  lambdas <- lambda_max * penalty_fractions
  fit <- glmnet(x, y, family = "gaussian", alpha = alpha_value,
                lambda = lambdas, standardize = FALSE)
  list(fit = fit, lambdas = lambdas)
}

# Each inner fold fits its own imputation/scaling and penalty path.
tune_penalty <- function(train, predictors, inner_folds) {
  losses <- matrix(NA_real_, nrow(train), length(penalty_fractions))
  for (f in sort(unique(inner_folds))) {
    inner_train <- train[inner_folds != f, ]
    inner_validation <- train[inner_folds == f, ]
    prep <- prepare_split(inner_train, inner_validation, predictors)
    path <- if (ncol(prep$train) > 0) fit_path(prep$train, inner_train$cito_score) else NULL
    if (is.null(path)) {
      predictions <- matrix(mean(inner_train$cito_score), nrow(inner_validation), length(penalty_fractions))
    } else {
      predictions <- as.matrix(predict(path$fit, newx = pad_matrix(prep$validation), s = path$lambdas))
    }
    losses[inner_folds == f, ] <- (predictions - inner_validation$cito_score)^2
  }
  which.min(colMeans(losses))
}

set.seed(123)
n_repeats <- 10L
n_outer_folds <- 5L
prediction_results <- list()
coefficient_results <- list()
status_results <- list()

for (test_name in unique(cito_gr3$Test)) {
  d <- model_data %>% filter(Test == test_name)
  n <- nrow(d)
  if (n < 10L) {
    status_results[[length(status_results) + 1L]] <- tibble(Test = test_name, n_pupils = n, status = "Skipped: fewer than 10 matched pupils with valid outcomes")
    next
  }
  
  for (r in seq_len(n_repeats)) {
    outer_folds <- sample(rep(seq_len(n_outer_folds), length.out = n))
    for (f in seq_len(n_outer_folds)) {
      train <- d[outer_folds != f, ]
      validation <- d[outer_folds == f, ]
      inner_folds <- sample(rep(seq_len(min(5L, nrow(train))), length.out = nrow(train)))
      
      for (model_name in names(model_predictors)) {
        predictors <- model_predictors[[model_name]]
        coefficients <- setNames(rep(0, length(predictors)), predictors)
        available <- setNames(rep(FALSE, length(predictors)), predictors)
        predictions <- rep(mean(train$cito_score), nrow(validation))
        selected_fraction <- NA_real_
        
        if (length(predictors) > 0) {
          prep <- prepare_split(train, validation, predictors)
          available[colnames(prep$train)] <- TRUE
          if (ncol(prep$train) > 0) {
            if (model_name == "time") {
              # Ordinary regression for the one-predictor duration model.
              fit <- lm(train$cito_score ~ prep$train[, 1])
              coefficients[colnames(prep$train)] <- unname(coef(fit)[2])
              predictions <- unname(coef(fit)[1]) + unname(coef(fit)[2]) * prep$validation[, 1]
            } else {
              best_index <- tune_penalty(train, predictors, inner_folds)
              path <- fit_path(prep$train, train$cito_score)
              if (!is.null(path)) {
                selected_fraction <- penalty_fractions[best_index]
                chosen_lambda <- path$lambdas[best_index]
                predictions <- as.numeric(predict(path$fit, newx = pad_matrix(prep$validation), s = chosen_lambda))
                cf <- as.matrix(coef(path$fit, s = chosen_lambda))
                coefficients[colnames(prep$train)] <- cf[colnames(prep$train), 1]
              }
            }
          }
        }
        
        prediction_results[[length(prediction_results) + 1L]] <- tibble(
          Test = test_name, model = model_name, repetition = r, fold = f,
          name_key = validation$name_key, observed = validation$cito_score,
          predicted = as.numeric(predictions), penalty_fraction = selected_fraction
        )
        if (model_name %in% c("readiness", "context")) {
          coefficient_results[[length(coefficient_results) + 1L]] <- tibble(
            Test = test_name, model = model_name, repetition = r, fold = f,
            predictor = predictors, available_in_training = unname(available),
            standardized_coefficient = unname(coefficients),
            selected = abs(unname(coefficients)) > 1e-8
          )
        }
      }
    }
  }
  status_results[[length(status_results) + 1L]] <- tibble(Test = test_name, n_pupils = n, status = "Completed")
}

cv_predictions <- bind_rows(prediction_results)
cv_coefficients <- bind_rows(coefficient_results)
model_status <- bind_rows(status_results)
rmse_results <- tibble()
vak_relevance <- tibble()

if (nrow(cv_predictions) > 0) {
  rmse_by_repeat <- cv_predictions %>%
    group_by(Test, model, repetition) %>%
    summarise(RMSE = sqrt(mean((observed - predicted)^2)), .groups = "drop")
  
  rmse_results <- rmse_by_repeat %>%
    group_by(Test, model) %>%
    summarise(mean_RMSE = mean(RMSE), sd_RMSE_across_repeats = sd(RMSE), .groups = "drop") %>%
    left_join(model_data %>% count(Test, name = "n_pupils"), by = "Test") %>%
    group_by(Test) %>%
    mutate(
      improvement_over_baseline = mean_RMSE[model == "baseline"] - mean_RMSE,
      rank = min_rank(mean_RMSE)
    ) %>%
    ungroup() %>%
    arrange(Test, rank)
  
  vak_relevance <- cv_coefficients %>%
    inner_join(vak_lookup, by = "predictor", relationship = "many-to-one") %>%
    group_by(Test, model, Vak) %>%
    summarise(
      availability_pct = 100 * mean(available_in_training),
      selection_pct = 100 * mean(selected),
      mean_abs_standardized_coefficient = mean(abs(standardized_coefficient)),
      mean_standardized_coefficient = mean(standardized_coefficient),
      positive_when_selected_pct = if (any(selected)) 100 * mean(standardized_coefficient[selected] > 0) else NA_real_,
      .groups = "drop"
    ) %>%
    left_join(
      predictor_missingness %>% filter(!is.na(Vak)) %>% select(Test, Vak, proportion_missing),
      by = c("Test", "Vak")
    ) %>%
    group_by(Test, model) %>%
    mutate(rank = min_rank(desc(mean_abs_standardized_coefficient))) %>%
    ungroup() %>%
    arrange(Test, model, rank)
  
} else {
  message("No tests fitted: inspect model_status and Grade/name matching.")
}

# Pupils per test, for plot captions
n_pupils_label <- function(status) {
  n <- unique(status$n_pupils[status$status == "Completed"])
  paste(n, collapse = "/")
}


# Uses vak_relevance from the all-Vak analysis; no model refitting needed.
plot_data <- vak_relevance %>%
  filter(model == "readiness") %>%
  mutate(
    evaluable = availability_pct > 0,
    label = if_else(evaluable, paste0(round(selection_pct), "%"), "Not evaluable")
  )

# Same Vak order in every panel: highest average selection frequency at the top.
vak_order <- plot_data %>%
  group_by(Vak) %>%
  summarise(
    evaluable = any(evaluable),
    average_selection = mean(selection_pct),
    .groups = "drop"
  ) %>%
  arrange(desc(evaluable), desc(average_selection), Vak) %>%
  pull(Vak)

plot_data <- plot_data %>%
  mutate(
    Vak = factor(Vak, levels = rev(vak_order)),
    Test = factor(Test, levels = c("Mathematics", "Spelling – Non-verbs", "TMT"))
  )

vak_plot <- ggplot(plot_data, aes(x = selection_pct, y = Vak)) +
  geom_col(
    data = filter(plot_data, evaluable),
    fill = "#277F8E", width = 0.65
  ) +
  geom_text(
    data = filter(plot_data, evaluable),
    aes(label = label), hjust = -0.15, size = 3.3, color = "#243746"
  ) +
  geom_text(
    data = filter(plot_data, !evaluable),
    aes(x = 2, label = label), hjust = 0, size = 3,
    color = "grey55", fontface = "italic"
  ) +
  facet_grid(. ~ Test) +
  scale_x_continuous(
    breaks = seq(0, 100, 25), limits = c(0, 116),
    labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "Which early Vakken are consistently retained in CITO prediction?",
    subtitle = "All-Vak elastic-net readiness model: kindergarten months + Vak scores",
    x = "Percentage of cross-validation fits with a nonzero coefficient",
    y = NULL,
    caption = paste(
      paste0(n_pupils_label(model_status), " pupils per test; ", n_repeats * n_outer_folds,
             " outer training fits per test (", n_repeats, " repetitions × ", n_outer_folds, " folds)."),
      "Not evaluable = no usable variation in any training fit; this does not indicate irrelevance.",
      "Selection frequency is exploratory, not statistical significance or predictive accuracy.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    axis.text.y = element_text(color = "#243746"),
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(color = "grey35"),
    plot.caption = element_text(hjust = 0, color = "grey40", size = 9),
    panel.spacing.x = grid::unit(1, "lines"),
    plot.margin = margin(15, 20, 15, 10)
  )

# Hand-picked from the readiness results above: the 2 most-selected Vakken per test
selected_vakken <- tribble(
  ~Test, ~Vak,
  "Mathematics", "Rekenen",
  "Mathematics", "Podiumkunsten",
  "Spelling – Non-verbs", "Rekenen",
  "Spelling – Non-verbs", "Grote motoriek",
  "TMT", "Rekenen",
  "TMT", "Taal"
) %>%
  left_join(vak_lookup, by = "Vak", relationship = "many-to-one")
stopifnot(!anyNA(selected_vakken$predictor))

# Training-only median imputation; do not assume missing counts equal zero.
impute_split <- function(train, validation, predictors) {
  for (p in predictors) {
    values <- train[[p]][is.finite(train[[p]])]
    if (length(values) == 0) stop(paste("No observed training values for", p))
    med <- median(values)
    train[[p]][!is.finite(train[[p]])] <- med
    validation[[p]][!is.finite(validation[[p]])] <- med
  }
  list(train = train, validation = validation)
}

fit_checked <- function(formula, d) {
  fit <- lm(formula, data = d, na.action = na.fail)
  if (fit$rank < ncol(model.matrix(fit))) {
    stop("Constant or collinear predictors: model is not identifiable")
  }
  fit
}

set.seed(123)
n_folds <- 5L
n_repeats <- 10L
prediction_list <- list()
status_list <- list()
coefficient_list <- list()
fitted_models_top2 <- list()
model_formulas_top2 <- list()

for (test_name in unique(selected_vakken$Test)) {
  vak_columns <- selected_vakken %>%
    filter(Test == test_name) %>% pull(predictor)
  
  predictors <- list(
    baseline = character(0),
    time = "kindergarten_months",
    readiness = c("kindergarten_months", vak_columns),
    context = c("kindergarten_months", vak_columns, "n_activities", "n_interventions")
  )
  formulas <- lapply(predictors, function(x) {
    if (length(x) == 0) cito_score ~ 1 else reformulate(x, response = "cito_score")
  })
  model_formulas_top2[[test_name]] <- formulas
  
  # Same pupils and folds for all four models; missing predictors are imputed.
  d <- model_data %>%
    filter(Test == test_name, is.finite(cito_score)) %>%
    select(name_key, cito_score, all_of(predictors$context))
  stopifnot(!anyDuplicated(d$name_key))
  n <- nrow(d)
  
  if (n < n_folds || n - ceiling(n / n_folds) <= length(predictors$context) + 1L) {
    status_list[[length(status_list) + 1L]] <- tibble(
      Test = test_name, n_pupils = n,
      status = "Skipped: too few pupils for context model training folds"
    )
    next
  }
  
  result <- tryCatch({
    test_predictions <- list()
    for (r in seq_len(n_repeats)) {
      folds <- sample(rep(seq_len(n_folds), length.out = n))
      for (f in seq_len(n_folds)) {
        prep <- impute_split(d[folds != f, ], d[folds == f, ], predictors$context)
        for (model_name in names(formulas)) {
          fit <- fit_checked(formulas[[model_name]], prep$train)
          test_predictions[[length(test_predictions) + 1L]] <- tibble(
            Test = test_name, model = model_name, repetition = r, fold = f,
            name_key = prep$validation$name_key,
            observed = prep$validation$cito_score,
            predicted = as.numeric(predict(fit, newdata = prep$validation))
          )
        }
      }
    }
    
    full_data <- impute_split(d, d, predictors$context)$train
    fits <- lapply(formulas, fit_checked, d = full_data)
    list(predictions = bind_rows(test_predictions), fits = fits)
  }, error = function(e) list(error = conditionMessage(e)))
  
  if (!is.null(result$error)) {
    status_list[[length(status_list) + 1L]] <- tibble(
      Test = test_name, n_pupils = n, status = paste("Skipped:", result$error)
    )
    next
  }
  
  prediction_list[[length(prediction_list) + 1L]] <- result$predictions
  fitted_models_top2[[test_name]] <- result$fits
  for (model_name in names(result$fits)) {
    cf <- coef(result$fits[[model_name]])
    coefficient_list[[length(coefficient_list) + 1L]] <- tibble(
      Test = test_name, model = model_name, predictor = names(cf),
      estimate = unname(cf)
    )
  }
  status_list[[length(status_list) + 1L]] <- tibble(
    Test = test_name, n_pupils = n, status = "Completed"
  )
}

cv_predictions_top2 <- bind_rows(prediction_list)
model_status_top2 <- bind_rows(status_list)
coefficient_results_top2 <- bind_rows(coefficient_list)
rmse_results_top2 <- tibble()

if (nrow(cv_predictions_top2) > 0) {
  rmse_by_repeat_top2 <- cv_predictions_top2 %>%
    group_by(Test, model, repetition) %>%
    summarise(RMSE = sqrt(mean((observed - predicted)^2)), .groups = "drop")
  
  rmse_results_top2 <- rmse_by_repeat_top2 %>%
    group_by(Test, model) %>%
    summarise(
      mean_RMSE = mean(RMSE), sd_RMSE_across_repeats = sd(RMSE),
      .groups = "drop"
    ) %>%
    left_join(model_status_top2 %>% select(Test, n_pupils), by = "Test") %>%
    group_by(Test) %>%
    mutate(
      improvement_over_baseline = mean_RMSE[model == "baseline"] - mean_RMSE,
      rank = min_rank(mean_RMSE)
    ) %>%
    ungroup() %>%
    arrange(Test, rank)
  
  coefficient_results_top2 <- coefficient_results_top2 %>%
    left_join(vak_lookup, by = "predictor") %>%
    mutate(predictor = coalesce(Vak, predictor)) %>%
    select(-Vak)
  
}

plot_data <- rmse_results_top2 %>%
  mutate(
    model = factor(model, levels = c("baseline", "time", "readiness", "context"),
                   labels = c("Baseline", "Time", "Readiness", "Context")),
    Test = factor(Test, levels = c("Mathematics", "Spelling – Non-verbs", "TMT")),
    lower = pmax(0, mean_RMSE - sd_RMSE_across_repeats),
    upper = mean_RMSE + sd_RMSE_across_repeats,
    label = sprintf("%.1f", mean_RMSE)
  )

baseline_data <- plot_data %>% filter(model == "Baseline")

performance_plot <- ggplot(plot_data, aes(x = model, y = mean_RMSE, color = model)) +
  geom_hline(
    data = baseline_data, aes(yintercept = mean_RMSE),
    inherit.aes = FALSE, linetype = "dashed", color = "grey60", linewidth = 0.6
  ) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.13, linewidth = 0.8) +
  geom_point(size = 3.8) +
  geom_text(aes(y = upper, label = label), vjust = -0.65, size = 3.6, show.legend = FALSE) +
  facet_wrap(~ Test, nrow = 1, scales = "free_y") +
  scale_color_manual(values = c(
    "Baseline" = "#687582", "Time" = "#D48A29",
    "Readiness" = "#16857C", "Context" = "#8365A8"
  )) +
  scale_y_continuous(expand = expansion(mult = c(0.12, 0.22))) +
  labs(
    title = "Do early-readiness and context predictors improve CITO prediction?",
    subtitle = paste0("Mean held-out RMSE from ", n_folds, "-fold cross-validation repeated ", n_repeats,
                      " times; ", n_pupils_label(model_status_top2), " pupils per test"),
    x = NULL, y = "Cross-validated RMSE (lower is better)",
    caption = paste(
      "Points: mean RMSE. Error bars: ±1 SD across repetitions, not confidence intervals.",
      "Dashed lines: baseline mean RMSE. Panels use different y-axis scales; compare models within each test.",
      "Exploratory: the two Vakken per test were selected using the same pupils, so performance may be optimistic.",
      sep = "\n"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    axis.text.x = element_text(angle = 25, hjust = 1, color = "#263747"),
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 11, color = "grey35"),
    plot.caption = element_text(hjust = 0, size = 9, color = "grey40"),
    panel.spacing = grid::unit(1.2, "lines"),
    plot.margin = margin(15, 15, 15, 10)
  )
