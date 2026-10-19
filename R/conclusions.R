# Short conclusions per notebook section, written from the results at render time so that no
# results are stored in the (public) source. Each function returns markdown bullets.
# Needs the analysis scripts to have been sourced.

fmt <- function(x, digits = 2) formatC(x, format = "f", digits = digits)
pct <- function(x, digits = 0) paste0(formatC(100 * x, format = "f", digits = digits), "%")
p_text <- function(p) if (p < 0.001) "p < .001" else paste0("p = ", sub("^0", "", fmt(p, 3)))
bullets <- function(...) paste0("- ", c(...), collapse = "\n")
and_list <- function(x) {
  if (length(x) == 0) return("none")
  if (length(x) == 1) return(x)
  paste(paste(head(x, -1), collapse = ", "), "and", tail(x, 1))
}
n_text <- function(n, one, many) paste(n, if (n == 1) one else many)

conclusion_timing <- function() {
  reached <- filter(pupils_table, reached_g3)
  gap <- as.numeric(reached$start_date - reached$date_of_birth - 365.25 * 4) / 30.44
  later <- filter(reached, g3_timing == "later than most")
  earlier <- filter(reached, g3_timing == "earlier than most")
  usual <- reached %>% count(g3_year_offset) %>% slice_max(n, n = 1, with_ties = FALSE)
  bullets(
    paste0(
      n_text(nrow(reached), "pupil has", "pupils have"), " reached group 3; for ",
      sum(reached$has_cito_g3), " of them the group 3 year comes from Cito. ",
      n_text(sum(pupils_table$repeated_after_g3, na.rm = TRUE), "pupil", "pupils"),
      " most likely repeated a year after group 3."
    ),
    if (max(abs(gap), na.rm = TRUE) < 2) {
      paste0(
        "Start dates lie at the 4th birthday (median ", fmt(median(gap, na.rm = TRUE), 1), ", at most ",
        fmt(max(gap, na.rm = TRUE), 1), " months after it), so time in kindergarten is in effect the age at",
        " the start of group 3 minus 4: time and age cannot be separated in this data."
      )
    } else {
      paste0("Start dates lie between ", fmt(min(gap, na.rm = TRUE), 1), " and ", fmt(max(gap, na.rm = TRUE), 1),
             " months from the 4th birthday.")
    },
    paste0(
      pct(usual$n / nrow(reached)), " of pupils started group 3 in the year they turned ", usual$g3_year_offset,
      ". ", nrow(earlier), " started a year earlier (born in ",
      and_list(unique(as.character(lubridate::month(sort(earlier$date_of_birth), label = TRUE, abbr = FALSE, locale = "C")))),
      "), ", nrow(later), " a year later (", sum(later$autumn_born), " of them born October-December)",
      if (nrow(earlier) + nrow(later) > 0) ": the timing differs per child rather than following one cut-off date." else "."
    )
  )
}

conclusion_goals <- function() {
  g <- goals_overview
  bullets(
    paste0(
      "On average pupils had reached ", fmt(g$mean_pct_achieved[1], 0), "% of the ", g$goals_in_list[1],
      " goals before group 3 when group 3 started; 10% of pupils were below ", fmt(g$p10_pct[1], 0), "%."
    ),
    paste0("For the ", g$goals_in_list[2], " basic goals the average was ", fmt(g$mean_pct_achieved[2], 0), "%.")
  )
}

conclusion_direct <- function() {
  primary <- filter(direct_months, role == "primary", controls == "cohort + class")
  all_rows <- bind_rows(direct_months, direct_under_24) %>% filter(!is.na(p))
  support <- filter(direct_months, outcome == "Any support in group 3", controls == "cohort + class")
  equity <- direct_equity %>%
    filter(kg_third %in% c("Short", "Long")) %>%
    select(test, kg_third, pct_band_IV_V) %>%
    pivot_wider(names_from = kg_third, values_from = pct_band_IV_V)
  # standard deviation of each primary outcome in the model sample, to put the interval in SD
  outcome_sd <- map_dbl(direct_outcomes$outcome[match(primary$outcome, direct_outcomes$label)], function(o) {
    d <- drop_na(select(direct_data, all_of(c(o, "kg_6_months", "cohort", "kg_class"))))
    sd(d[[o]])
  })
  bullets(
    paste0(
      sum(all_rows$p < 0.05), " of the ", nrow(all_rows), " estimates (all outcomes, per 6 months and under",
      " 24 months, with and without controls) have p < .05."
    ),
    paste0(
      "Per 6 more months in kindergarten, the primary Cito outcomes change by ",
      fmt(min(primary$per_sd)), " to ", fmt(max(primary$per_sd)), " SD; the intervals rule out gains larger than ",
      fmt(max(primary$ci_high / outcome_sd)), " SD."
    ),
    paste0(
      "Support in group 3: odds ratio ", fmt(support$estimate), " [", fmt(support$ci_low), ", ", fmt(support$ci_high),
      "] per 6 more months (below 1 = less support)."
    ),
    paste0(
      "Pupils in Cito band IV-V, shortest vs longest third of kindergarten time: ",
      paste0(equity$test, " ", fmt(equity$Short, 0), "% vs ", fmt(equity$Long, 0), "%", collapse = "; "), "."
    )
  )
}

conclusion_stage1 <- function() {
  sig <- function(f) {
    d <- filter(stage1_main, factor == f, p < 0.05)
    if (nrow(d) == 0) return("no readiness measure (p < .05)")
    paste0(and_list(paste0(tolower(sub(" \\(.*", "", d$label)), " (", ifelse(d$per_sd > 0, "+", ""), fmt(d$per_sd), " SD)")))
  }
  bullets(
    paste0("More time in kindergarten goes with higher ", sig("6 more months in kindergarten"), "."),
    paste0(
      "Class 1/2b differs from 1/2a in ", sig("Class 1/2b (vs 1/2a)"), ". Readiness is scored by the class's own",
      " teachers, so a class difference can also be a difference in scoring; compare with Cito before reading it as",
      " a teaching effect."
    ),
    paste0("Extracurricular lessons go with ", sig("5 more lessons per kindergarten year"), "."),
    paste0(
      "Pupils with support during kindergarten have lower readiness (", fmt(min(stage1_support$per_sd)), " to ",
      fmt(max(stage1_support$per_sd)), " SD): support goes to the children who are behind."
    )
  )
}

conclusion_variance <- function() {
  v <- filter(ev_variance_table, framing == "kindergarten factors")
  time <- filter(v, block == "Time")
  smallest <- v %>% group_by(outcome) %>% summarise(time_smallest = block[which.min(r2_share)] == "Time", .groups = "drop")
  clear <- ev_variance_table %>%
    filter(r2_share_low > noise_level) %>%
    group_by(outcome, framing) %>%
    summarise(blocks = and_list(paste0(tolower(block), " (", fmt(r2_share), ")")), .groups = "drop") %>%
    filter(framing == "kindergarten factors")
  ready <- filter(ev_variance_table, block == "Readiness")
  extra <- ev_coefficient_table %>%
    filter(framing == "kindergarten factors", block == "Extracurricular", p < 0.05) %>%
    mutate(txt = paste0(sub("lessons: ", "", feature), " ", ifelse(estimate > 0, "+", ""), fmt(estimate), " SD on ",
                        sub(", end of group 3", "", outcome)))
  bullets(
    paste0(
      "Time is the smallest block in ", sum(smallest$time_smallest), " of ", nrow(smallest),
      " outcomes; its share is ", fmt(min(time$r2_share), 3), " to ", fmt(max(time$r2_share), 3),
      " of the variance, at most ", fmt(max(time$above_noise), 3), " above chance (",
      pct(min(time$pct_of_explained) / 100), " to ", pct(max(time$pct_of_explained) / 100), " of what the model explains)."
    ),
    paste0(
      "Blocks clearly above chance (whole interval above the chance level): ",
      paste0(clear$outcome, ": ", clear$blocks, collapse = "; "), "."
    ),
    paste0(
      "With readiness added, readiness (compared with classmates) accounts for ",
      paste0(sub(", end of group 3", "", ready$outcome), " ", fmt(ready$r2_share), collapse = ", "),
      " of the variance."
    ),
    paste0(
      "Extracurricular kinds with p < .05: ", if (nrow(extra) == 0) "none" else paste(extra$txt, collapse = "; "),
      ". Families choose lessons, and several kinds and outcomes are tested, so these are leads, not effects."
    ),
    if (any(clear$blocks != "none" & str_detect(clear$blocks, "support"))) {
      "Support during kindergarten goes to children who are behind: its share shows that early support needs flag later difficulties, not that support causes them."
    } else {
      "Support during kindergarten goes to children who are behind, so its share describes who needed help, not what the help did."
    }
  )
}

conclusion_q1_reported <- function() {
  b <- q1_bridge
  bullets(
    paste0(
      "As reported, each extra activity goes with ", fmt(b$activities_b[1], 3), " higher development (",
      p_text(b$p[1]), "). Controlling for months in school changes this to ", fmt(b$activities_b[3], 3), " (",
      p_text(b$p[3]), "), and among pupils who reached group 3 it is ", fmt(b$activities_b[4], 3), " (",
      p_text(b$p[4]), ")."
    ),
    if (b$p[1] < 0.05 && b$p[3] >= 0.05) {
      "Older pupils have both more lessons (counted over all years) and higher observation averages, so the reported effect largely reflects age."
    } else {
      "Older pupils have both more lessons (counted over all years) and higher observation averages; compare the steps in the table to see how much of the effect that explains."
    }
  )
}

conclusion_goals_q2 <- function() {
  check <- class_cito_check
  ready <- check[1, ]
  cito <- check[-1, ]
  sub_test <- function(x) sub(", end of group 3", "", x)
  diff_txt <- function(d) paste0(ifelse(d$difference_sd > 0, "+", ""), fmt(d$difference_sd), " SD, ", sapply(d$p, p_text))
  pred <- goals_prediction
  get <- function(test, m, col = "predicted_r2") pred[[col]][pred$Test == test & pred$model == m]
  pred_txt <- map_chr(goals_cito_tests, function(test) {
    paste0(
      sub_test(test), ": kindergarten factors ", fmt(get(test, "+ kindergarten factors")),
      ", with readiness by subject ", fmt(get(test, "+ readiness by subject")),
      " (in-sample ", fmt(get(test, "+ readiness by subject", "in_sample_r2")), "), with domains ",
      fmt(get(test, "+ readiness by domain")), ", with the goals ", fmt(get(test, "+ readiness by goal"))
    )
  })
  kept_domains <- goals_selection %>%
    filter(model == "+ readiness by domain", block %in% c("Readiness", "Readiness by domain"), selected_pct >= 70) %>%
    group_by(Test) %>%
    summarise(txt = and_list(paste0(predictor, " (", fmt(selected_pct, 0), "%)")), .groups = "drop")
  kept <- goals_selection %>%
    filter(model == "+ readiness by goal", selected_pct >= 70) %>%
    group_by(Test) %>%
    summarise(txt = and_list(paste0(predictor, " (", fmt(selected_pct, 0), "%)")), .groups = "drop")
  time_kept <- goals_selection %>% filter(model == "+ readiness by goal", predictor == "months in kindergarten")
  bullets(
    paste0(
      "Class 1/2b vs 1/2a: maths readiness ", diff_txt(ready), "; Cito ",
      paste0(sub("Cito ", "", sub_test(cito$measure)), " ", diff_txt(cito), collapse = "; "), ".",
      if (ready$p < 0.05 && all(cito$p >= 0.05)) " The classes differ in how readiness is scored, not on Cito, so readiness is compared within each class." else ""
    ),
    paste0(
      "Share of variance predicted for new pupils (cross-validated): ", paste(pred_txt, collapse = "; "),
      ". The in-sample shares of the explained-variance section are higher, as expected with this many features and pupils."
    ),
    paste0(
      "Subjects and domains kept in at least 70% of the fits (domain model): ",
      if (nrow(kept_domains) == 0) "none" else paste0(sub_test(kept_domains$Test), ": ", kept_domains$txt, collapse = "; "), "."
    ),
    paste0(
      "Kept in at least 70% of the fits with all blocks: ",
      if (nrow(kept) == 0) "nothing" else paste0(sub_test(kept$Test), ": ", kept$txt, collapse = "; "), "."
    ),
    paste0(
      "Months in kindergarten is kept in ", and_list(paste0(fmt(time_kept$selected_pct, 0), "% (", sub_test(time_kept$Test), ")")),
      " of the fits."
    )
  )
}
