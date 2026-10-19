# Builds the pupil-level tables used by the Q1 and Q2 analyses.
# Needs R/setup.R and R/load_data.R to have been sourced.

# The 15 learning-goal codes Het Epos asked about specifically
codes = c(
  "TG11.2_E2",
  "MK6.3_E1",
  "SA3.3_E2",
  "TG10.3_E2",
  "BT3.2_E1",
  "AO1.3_E1",
  "TZ1.2_E1",
  "VO3.3_E2",
  "KM4.2_E2",
  "AO2.4_E1",
  "BV2.4_E2",
  "MK7.3_E2",
  "KM3.3_E2",
  "TG7.3_E1",
  "TG3.3_E2"
)

#average scores per kid, per leerpadcode (all leerpads)
average_scores <- data_observatie %>%
  group_by(Leerling, Leerpadcode) %>%
  summarise(
    mean_score = if (all(is.na(Score)))
      NA_real_
    else
      mean(Score, na.rm = TRUE),
    n_scores = sum(!is.na(Score)),
    .groups = "drop"
  )

average_scores_subset <- data_observatie %>%
  filter(Leerpadcode %in% codes) %>%
  group_by(Leerling, Leerpadcode) %>%
  summarise(
    mean_score = if (all(is.na(Score)))
      NA_real_
    else
      mean(Score, na.rm = TRUE),
    n_scores = sum(!is.na(Score)),
    .groups = "drop"
  )

# CITO scores come in as text with decimal commas
data_grade <- data_grade %>%
  mutate(Score_original = Score, Score = suppressWarnings(as.numeric(gsub(
    ",", ".", trimws(as.character(Score)), fixed = TRUE
  ))))

failed_conversion <- data_grade %>%
  filter(!is.na(Score_original),!trimws(as.character(Score_original)) %in% c("", "NA", "N/A"),
         is.na(Score)) %>%
  distinct(Score_original)

# Average score per kid, per CITO test result
average_grades <- data_grade %>%
  group_by(Name, Test) %>%
  summarise(
    mean_grade = if (all(is.na(Score)))
      NA_real_
    else
      mean(Score, na.rm = TRUE),
    n_scores = sum(!is.na(Score)),
    .groups = "drop"
  )

matched_data <- average_scores_subset %>%
  mutate(Leerling = trimws(Leerling)) %>%
  filter(!is.na(Leerling), Leerling != "") %>%
  inner_join(
    average_grades %>%
      mutate(Name = trimws(Name)) %>%
      filter(!is.na(Name), Name != ""),
    by = c("Leerling" = "Name"),
    relationship = "many-to-many"
  ) %>%
  filter(is.finite(mean_score), is.finite(mean_grade))

# correlations dataset --> only on the 15 leerpadcodes they specifically mention
correlations <- matched_data %>%
  group_by(Leerpadcode, Test) %>%
  group_modify( ~ {
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

# descriptives
score_summary <- average_scores %>%
  group_by(Leerpadcode) %>%
  summarise(
    n_pupils = sum(is.finite(mean_score)),
    mean = if (n_pupils > 0)
      mean(mean_score[is.finite(mean_score)])
    else
      NA_real_,
    sd = sd(mean_score[is.finite(mean_score)]),
    median = if (n_pupils > 0)
      median(mean_score[is.finite(mean_score)])
    else
      NA_real_,
    min = if (n_pupils > 0)
      min(mean_score[is.finite(mean_score)])
    else
      NA_real_,
    max = if (n_pupils > 0)
      max(mean_score[is.finite(mean_score)])
    else
      NA_real_
  )

grade_summary <- average_grades %>%
  group_by(Test) %>%
  summarise(
    n_pupils = sum(is.finite(mean_grade)),
    mean = if (n_pupils > 0)
      mean(mean_grade[is.finite(mean_grade)])
    else
      NA_real_,
    sd = sd(mean_grade[is.finite(mean_grade)]),
    median = if (n_pupils > 0)
      median(mean_grade[is.finite(mean_grade)])
    else
      NA_real_,
    min = if (n_pupils > 0)
      min(mean_grade[is.finite(mean_grade)])
    else
      NA_real_,
    max = if (n_pupils > 0)
      max(mean_grade[is.finite(mean_grade)])
    else
      NA_real_
  )

# plot
plot_data <- correlations %>%
  mutate(label = ifelse(
    is.na(correlation),
    "NA",
    paste0(
      sprintf("%.2f", correlation),
      ifelse(!is.na(p_adjusted) & p_adjusted < 0.05, "*", ""),
      "\nn=",
      n_pupils
    )
  ))

correlation_plot <- ggplot(plot_data, aes(x = Test, y = Leerpadcode, fill = correlation)) +
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
    title = "Correlations between leerpad scores and test scores",
    subtitle = "Each correlation is calculated across matched pupils",
    x = "Test",
    y = "Leerpadcode",
    caption = "* BH-adjusted p < .05; n = matched pupils; NA = insufficient data or no variation"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

data_clean <- data_kids %>%
  transmute(
    Name = trimws(Pupil),
    Current_class = `Current class`,
    Date_of_birth = `Date of birth`
  )

# Intervention data

names(data_intervention) <- make.unique(trimws(as.character(
  unlist(data_intervention[1, ], use.names = FALSE)
)))

data_intervention <- data_intervention[-1, ] %>%
  mutate(
    across(
      c(`School yr`, Term, Record, `Weeks (est.)`),
      ~ as.numeric(trimws(.x))
    ),
    across(
      c(`Referral date`, `Start date`),
      ~ as.Date(as.numeric(.x), origin = "1899-12-30")
    ),
    across(c(Need, Focus, `Support type`, `Next step`), ~ factor(trimws(.x))),
    `Started?` = case_when(
      tolower(trimws(`Started?`)) == "yes" ~ TRUE,
      tolower(trimws(`Started?`)) == "no" ~ FALSE,
      TRUE ~ NA
    ),
    Name = trimws(as.character(`Student name`))
  )

format_interventions <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A")] <- NA_character_
  
  if (length(x) == 0 || all(is.na(x)))
    return(NA_character_)
  
  paste0(seq_along(x), ". ", ifelse(is.na(x), "Unknown", x), collapse = "; ")
}

intervention_summary <- data_intervention %>%
  filter(!is.na(Name), Name != "") %>%
  group_by(Name) %>%
  summarise(
    n_interventions = n(),
    intervention_types = format_interventions(Need),
    intervention_statuses = format_interventions(`Next step`),
    .groups = "drop"
  )

data_clean <- data_clean %>%
  select(-any_of(
    c(
      "n_interventions",
      "intervention_types",
      "intervention_statuses"
    )
  )) %>%
  mutate(Name = trimws(as.character(Name))) %>%
  left_join(intervention_summary, by = "Name") %>%
  mutate(
    n_interventions = coalesce(n_interventions, 0L),
    intervention_types = if_else(n_interventions == 0, NA_character_, intervention_types),
    intervention_statuses = if_else(n_interventions == 0, NA_character_, intervention_statuses)
  )


#CLASS data

parse_school_date <- function(x) {
  if (inherits(x, "Date"))
    return(x)
  if (inherits(x, "POSIXt"))
    return(as.Date(x))
  
  x <- trimws(as.character(x))
  numeric_date <- suppressWarnings(as.numeric(x))
  is_excel <- !is.na(numeric_date) & numeric_date > 10000 &
    grepl("^[0-9]+(\\.[0-9]+)?$", x)
  
  result <- as.Date(lubridate::parse_date_time(
    replace(x, is_excel, NA_character_),
    orders = c("d.b.y", "ymd", "dmy"),
    locale = "C",
    quiet = TRUE
  ))
  
  result[is_excel] <- as.Date(numeric_date[is_excel], origin = "1899-12-30")
  
  result
}

school_year_start <- function(x) {
  lubridate::year(x) - as.integer(lubridate::month(x) < 8)
}

normalise_class <- function(x) {
  x <- tolower(gsub("\\s+", "", as.character(x)))
  
  case_when(
    x %in% c("group1/2a", "groep1/2a", "1/2a") ~ "1/2a",
    x %in% c("group1/2b", "groep1/2b", "1/2b") ~ "1/2b",
    x %in% c("group3", "groep3", "gr3", "3") ~ "3",
    TRUE ~ NA_character_
  )
}

support_columns <- c(
  "Total Emotional Support",
  "Total Classroom Organization",
  "Total Instructional Support"
)

data_CLASS_prepared <- data_CLASS %>%
  mutate(
    assessment_date = parse_school_date(Date),
    class_group = normalise_class(Group),
    school_year_start = school_year_start(assessment_date),
    expected_quarter = if_else(
      !is.na(assessment_date),
      paste0(
        lubridate::year(assessment_date),
        "_Q",
        lubridate::quarter(assessment_date)
      ),
      NA_character_
    ),
    quarter_mismatch = !is.na(Quarter) &
      !is.na(expected_quarter) &
      trimws(as.character(Quarter)) != expected_quarter,
    across(all_of(support_columns), ~ as.numeric(gsub(
      ",", ".", trimws(as.character(.x)), fixed = TRUE
    )))
  )

CLASS_yearly <- data_CLASS_prepared %>%
  filter(!is.na(school_year_start), !is.na(class_group)) %>%
  group_by(school_year_start, class_group) %>%
  summarise(
    n_assessments = n(),
    n_quarter_mismatches = sum(quarter_mismatch),
    across(
      all_of(support_columns),
      ~ if (all(is.na(.x)))
        NA_real_
      else
        mean(.x, na.rm = TRUE),
      .names = "mean_{.col}"
    )
  )

kids_CLASS <- data_kids %>%
  transmute(
    Name = trimws(as.character(Pupil)),
    Current_class = as.character(`Current class`),
    Start_date = parse_school_date(`Start date`),
    entry_school_year = school_year_start(Start_date),
    current_group = normalise_class(`Current class`)
  ) %>%
  filter(!is.na(Name), Name != "") %>%
  distinct()

stopifnot(!anyDuplicated(kids_CLASS$Name))

data_CLASS_clean <- tidyr::crossing(kids_CLASS, Grade = 1:3) %>%
  mutate(
    school_year_start = entry_school_year + Grade - 1L,
    school_year = if_else(
      !is.na(school_year_start),
      paste0(school_year_start, "/", school_year_start + 1L),
      NA_character_
    ),
    grade_period_start = as.Date(paste0(school_year_start, "-08-01")),
    grade_period_end = as.Date(paste0(school_year_start + 1L, "-07-31")),
    grade_period_start = if_else(Grade == 1L, Start_date, grade_period_start),
    class_group = case_when(
      Grade == 3L ~ "3",
      current_group %in% c("1/2a", "1/2b") ~ current_group,
      TRUE ~ NA_character_
    )
  ) %>%
  left_join(
    CLASS_yearly,
    by = c("school_year_start", "class_group"),
    na_matches = "never"
  ) %>%
  select(
    Name,
    Current_class,
    Start_date,
    Grade,
    school_year,
    grade_period_start,
    grade_period_end,
    class_group,
    n_assessments,
    n_quarter_mismatches,
    starts_with("mean_")
  ) %>%
  arrange(Name, Grade)

# CLASS rows whose date is unparseable or disagrees with the Quarter column
class_date_issues <- data_CLASS_prepared %>%
  filter(is.na(assessment_date) | quarter_mismatch) %>%
  select(Date, Quarter, assessment_date, expected_quarter)

data_CLASS_clean <- data_CLASS_clean %>%
  filter(!is.na(grade_period_start), grade_period_start <= Sys.Date()) %>%
  select(-any_of(c(
    "n_assessments", "n_mismatches", "n_quarter_mismatches"
  )))


#to clean up the CLASS data i had to compute when each kid was in each class. because i already had this divided
#i also computed the time in kindergarten cus why not
data_CLASS_clean <- data_CLASS_clean %>%
  mutate(kindergarten_months = round(pmax(
    0,
    lubridate::time_length(lubridate::interval(Start_date, pmin(
      Sys.Date(), as.Date(paste0(
        school_year_start(Start_date) + 2L, "-08-01"
      ))
    )), unit = "month")
  ), 1))

data_clean <- data_clean %>%
  select(-any_of("kindergarten_months")) %>%
  left_join(data_CLASS_clean %>%
              select(Name, kindergarten_months) %>%
              distinct(),
            by = "Name") %>%
  relocate(kindergarten_months, .after = Date_of_birth)

# Learning-goal scores per pupil at three levels: Vak, Domein, Leerpadcode
scores_by_vak <- data_observatie %>%
  group_by(Leerling, Vak) %>%
  summarise(
    mean_score = if (all(is.na(Score)))
      NA_real_
    else
      mean(Score, na.rm = TRUE),
    n_scores = sum(!is.na(Score)),
    .groups = "drop"
  ) %>%
  mutate(level = "Vak")

scores_by_domein <- data_observatie %>%
  group_by(Leerling, Vak, Domein) %>%
  summarise(
    mean_score = if (all(is.na(Score)))
      NA_real_
    else
      mean(Score, na.rm = TRUE),
    n_scores = sum(!is.na(Score)),
    .groups = "drop"
  ) %>%
  mutate(level = "Domein")

scores_by_leerpadcode <- data_observatie %>%
  group_by(Leerling, Vak, Domein, Leerpadcode) %>%
  summarise(
    mean_score = if (all(is.na(Score)))
      NA_real_
    else
      mean(Score, na.rm = TRUE),
    n_scores = sum(!is.na(Score)),
    .groups = "drop"
  ) %>%
  mutate(level = "Leerpadcode")


learning_goals <- bind_rows(scores_by_vak, scores_by_domein, scores_by_leerpadcode) %>%
  mutate(level = factor(level, levels = c("Vak", "Domein", "Leerpadcode"))) %>%
  select(Leerling, level, Vak, Domein, Leerpadcode, mean_score, n_scores) %>%
  arrange(Leerling, Vak, level, Domein, Leerpadcode)
