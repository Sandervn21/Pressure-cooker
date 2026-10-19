# The numbers quoted on the report slides, as ready-to-print text. report.qmd uses them inline,
# e.g. `r num$usual_share`, so the slide text can be edited in report.qmd while the numbers come
# from the analysis (and no results are stored in the public source).
# Needs the analysis scripts and R/conclusions.R to have been sourced.

report_numbers <- function() {
  reached <- filter(pupils_table, reached_g3)
  usual <- reached %>% count(g3_year_offset) %>% slice_max(n, n = 1, with_ties = FALSE)
  all_direct <- bind_rows(direct_months, direct_under_24) %>% filter(!is.na(p))
  time_ready <- filter(stage1_main, factor == "6 more months in kindergarten", p < 0.05)
  top_ready <- time_ready[which.max(abs(time_ready$per_sd)), ]
  time_share <- filter(ev_variance_table, framing == "kindergarten factors", block == "Time")$r2_share
  ready_share <- filter(ev_variance_table, block == "Readiness")$r2_share
  fitted <- filter(goals_prediction, str_starts(model, "\\+ readiness"))
  best <- fitted[which.max(fitted$predicted_r2), ]
  in_sample <- filter(goals_prediction, model == "+ readiness by subject")$in_sample_r2
  time_kept <- filter(goals_selection, model == "+ readiness by goal", predictor == "months in kindergarten")$selected_pct
  signed <- function(x) paste0(ifelse(x > 0, "+", ""), fmt(x))

  list(
    # who and how long
    n_reached = nrow(reached),
    n_cito = sum(reached$has_cito_g3),
    usual_share = pct(usual$n / nrow(reached)),
    usual_age = usual$g3_year_offset,
    n_moved = sum(reached$g3_timing %in% c("earlier than most", "later than most")),
    # time in kindergarten and group 3
    max_gain = fmt(direct_max_gain()),
    n_direct = nrow(all_direct),
    n_direct_clear = sum(all_direct$p < 0.05),
    # time and readiness
    top_ready_sd = signed(top_ready$per_sd),
    top_ready_name = readiness_name(top_ready$outcome),
    # what explains group 3
    time_share_max = pct(max(time_share), 1),
    ready_share_max = pct(max(ready_share)),
    # the two classes
    class_ready_sd = signed(class_cito_check$difference_sd[1]),
    # skills and prediction
    time_kept_min = fmt(min(time_kept), 0),
    time_kept_max = fmt(max(time_kept), 0),
    best_prediction = pct(max(best$predicted_r2, 0)),
    best_prediction_test = sub(", end of group 3", "", best$Test),
    in_sample_min = pct(min(in_sample)),
    in_sample_max = pct(max(in_sample))
  )
}

# Without the analysis (skeleton mode) every number shows as "…"
placeholder_numbers <- function() structure(list(), class = "placeholder_numbers")
`$.placeholder_numbers` <- function(x, name) "…"
