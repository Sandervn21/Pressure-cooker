# Cross-validated elastic net, from the Q2 analysis (Teodora): training-only imputation and
# scaling, penalty tuned by inner cross-validation, repeated outer 5-fold cross-validation.
# Used by R/goals.R. Data must have the outcome in `cito_score` and a
# `name_key` column.

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

# Repeated nested cross-validation per test (`test_column`), for a list of models
# (name -> predictor columns). Models in `lm_models` are fitted by ordinary regression, the
# others by the elastic net; coefficients are kept for the models in `coef_models`.
run_cv <- function(model_data, tests, model_predictors, lm_models = "time",
                   coef_models = c("readiness", "context"), n_repeats = 10L, n_outer_folds = 5L,
                   min_pupils = 10L, test_column = "Test") {
  prediction_results <- list()
  coefficient_results <- list()
  status_results <- list()

  for (test_name in tests) {
    d <- model_data[model_data[[test_column]] == test_name, ]
    n <- nrow(d)
    if (n < min_pupils) {
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
              if (model_name %in% lm_models) {
                # Ordinary regression (e.g. the one-predictor duration model)
                fit <- lm(train$cito_score ~ prep$train)
                b <- coef(fit)
                b[is.na(b)] <- 0
                coefficients[colnames(prep$train)] <- unname(b[-1])
                predictions <- unname(b[1]) + drop(prep$validation %*% b[-1])
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
          if (model_name %in% coef_models) {
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

  list(
    predictions = bind_rows(prediction_results),
    coefficients = bind_rows(coefficient_results),
    status = bind_rows(status_results)
  )
}
