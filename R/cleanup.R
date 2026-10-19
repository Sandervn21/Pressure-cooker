# Cleans the raw tables (Cito scores, support records, CLASS) and summarises them for the data checks.
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

# CLASS rows whose date is unparseable or disagrees with the Quarter column
class_date_issues <- data_CLASS_prepared %>%
  filter(is.na(assessment_date) | quarter_mismatch) %>%
  select(Date, Quarter, assessment_date, expected_quarter)
