# Pupil-level table: one row per pupil in the master list, with
#   - timing: birth, start at school, start of group 3, time in kindergarten
#   - groups 1-2 factors: kindergarten class, lessons, support and classroom quality during kindergarten
#   - readiness at the end of kindergarten: last observation of each goal before group 3
#   - outcomes: Cito at the end of group 3 and 4, support and well-being in group 3 and 4
# Needs R/cleanup.R to have been sourced.

# Assumptions (one place to change them)
g3_start_month <- 8L # group 3 starts on 20 August
g3_start_mday <- 20L
export_school_year <- 2026L # school year of the export; `Current year group` refers to it
achieved_min <- 2 # observation score (0-3) that counts as a goal achieved

# The client's goal lists (materials: "basic goals" groups 1-2 and "goals before group 3")
basic_codes <- c(
  "BV4.1_S1", "TZ1.1_S1", "TG7.1_S1", "0001A6", "IT2.1_S1", "0001AB", "TG10.2_E2",
  "AO5.2_E2", "TG1.2_E1", "TG3.2_E2", "KM4.1_E1", "BT2.2_E1", "KM1.2_E1", "KM3.1_E1",
  "MK1.3_E1", "AO3.2_E1", "KM1.4_E2", "AO3.3_E2", "TG6.2_E2"
)
transition_codes <- codes # the 15 goals in cleanup.R

# Observation subjects used as readiness domains
readiness_domains <- c(
  Rekenen = "maths", Taal = "language", SEO = "social_emotional", KindBegrip = "kindbegrip",
  `Kleine motoriek` = "fine_motor", `Grote motoriek` = "gross_motor"
)
cito_tests <- c(
  Mathematics = "maths", `Reading Comprehension` = "reading", TMT = "tmt",
  `Spelling – Non-verbs` = "spelling"
)

# School-year start from a 4-digit code ("2324" -> 2023) or a period ("end 2022-2023" -> 2022)
sy_from_code <- function(x) 2000L + as.integer(substr(sprintf("%04d", as.integer(x)), 1, 2))
sy_from_period <- function(x) as.integer(str_extract(x, "\\d{4}"))

# Dates arrive as date cells, Excel serial numbers or text in the table's own format
as_date_cell <- function(x, text_orders) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  x <- trimws(as.character(x))
  serial <- suppressWarnings(as.numeric(x))
  out <- as.Date(lubridate::parse_date_time(x, orders = text_orders, quiet = TRUE))
  is_serial <- !is.na(serial) & serial > 10000
  out[is_serial] <- as.Date(serial[is_serial], origin = "1899-12-30")
  out
}

# Numbers stored as text, some with a decimal comma ("3,1")
as_number <- function(x) suppressWarnings(as.numeric(gsub(",", ".", trimws(as.character(x)), fixed = TRUE)))

# School years [from, to] as a list column; empty when from > to
year_span <- function(from, to) map2(from, to, ~ if (is.na(.x) || is.na(.y) || .x > .y) integer(0) else seq(.x, .y))

# "2025_Q3" -> school year 2025; "2026_Q1" -> 2025
sy_from_quarter <- function(x) {
  as.integer(str_extract(x, "^\\d{4}")) - as.integer(as.integer(str_extract(x, "(?<=Q)\\d")) <= 2)
}

per_pupil <- function(d, f) {
  d %>%
    filter(!is.na(name_key), !is.na(year)) %>%
    group_by(name_key) %>%
    summarise(year = f(year), .groups = "drop")
}

mode_int <- function(x) as.integer(names(sort(table(x), decreasing = TRUE))[1])

# --- Timing ---------------------------------------------------------------------------------

# School year in which group 3 started, per source. On the real data the dated sources agree
# for every pupil they share; counting back from the current year group differs only for
# pupils who repeated a year after group 3.
g3_from_cito <- data_grade %>%
  filter(str_squish(as.character(Grade)) == "Grade 3") %>%
  transmute(name_key = make_key(Name), year = sy_from_period(`Assessment period`)) %>%
  per_pupil(min) %>%
  rename(g3_cito = year)

g3_from_maths <- if (is.null(data_math)) {
  tibble(name_key = character(), g3_maths = integer())
} else {
  data_math %>%
    transmute(name_key = make_key(leerling), grade = floor(as_number(blok)), date = as_date_cell(datum, "mdy")) %>%
    filter(grade == 3, !is.na(date)) %>%
    transmute(name_key, year = school_year_start(date)) %>%
    per_pupil(min) %>%
    rename(g3_maths = year)
}

g3_from_questionnaire <- if (is.null(data_questionnaire)) {
  tibble(name_key = character(), g3_questionnaire = integer())
} else {
  data_questionnaire %>%
    filter(as_number(Leerjaar) == 3) %>%
    transmute(name_key = make_key(Leerling), year = sy_from_quarter(as.character(Periode))) %>%
    per_pupil(min) %>%
    rename(g3_questionnaire = year)
}

# Leaving year in Cito; the school moves it when a pupil repeats a year
cito_leaving_year <- data_grade %>%
  transmute(name_key = make_key(Name), year = as.integer(as_number(`Cohort year`))) %>%
  per_pupil(mode_int) %>%
  rename(cito_leaving_year = year)

pupil_timing <- data_kids %>%
  transmute(
    name = trimws(as.character(Pupil)),
    name_key = make_key(Pupil),
    current_class = as.character(`Current class`),
    current_year_group = as.integer(as.character(`Current year group`)),
    date_of_birth = parse_school_date(`Date of birth`),
    start_date = parse_school_date(`Start date`)
  ) %>%
  filter(!is.na(name_key), name_key != "") %>%
  distinct(name_key, .keep_all = TRUE) %>%
  left_join(g3_from_cito, by = "name_key", relationship = "one-to-one") %>%
  left_join(g3_from_maths, by = "name_key", relationship = "one-to-one") %>%
  left_join(g3_from_questionnaire, by = "name_key", relationship = "one-to-one") %>%
  left_join(cito_leaving_year, by = "name_key", relationship = "one-to-one") %>%
  mutate(
    g3_count_back = if_else(current_year_group >= 3L, export_school_year - (current_year_group - 3L), NA_integer_),
    # first available source, in order of how direct it is
    g3_year = coalesce(g3_cito, g3_maths, g3_questionnaire, g3_count_back),
    g3_source = case_when(
      !is.na(g3_cito) ~ "Cito grade 3",
      !is.na(g3_maths) ~ "maths tests block 3",
      !is.na(g3_questionnaire) ~ "questionnaire group 3",
      !is.na(g3_count_back) ~ "current year group",
      TRUE ~ NA_character_
    ),
    # later sources say group 3 (or the leaving year) came a year later than it did
    repeated_after_g3 = if_else(
      is.na(g3_year) | current_year_group < 3L, NA,
      coalesce(g3_count_back > g3_year, FALSE) | coalesce(cito_leaving_year - 6L > g3_year, FALSE)
    ),
    birth_month = lubridate::month(date_of_birth),
    birth_quarter = c("Jan-Mar", "Apr-Jun", "Jul-Sep", "Oct-Dec")[lubridate::quarter(date_of_birth)],
    autumn_born = birth_month >= 10,
    age_at_start = lubridate::time_length(lubridate::interval(date_of_birth, start_date), "years"),
    entry_school_year = school_year_start(start_date),
    reached_g3 = !is.na(g3_year) & g3_year <= export_school_year & coalesce(current_year_group >= 3L, FALSE),
    has_cito_g3 = !is.na(g3_cito),
    g3_start = lubridate::make_date(g3_year, g3_start_month, g3_start_mday), # NA when not reached
    # In this export the start date lies at the 4th birthday for every pupil, so time in
    # kindergarten is in effect (age at the start of group 3 - 4) in months
    months_to_g3 = as.numeric(g3_start - start_date) / 30.44,
    age_at_g3 = lubridate::time_length(lubridate::interval(date_of_birth, g3_start), "years"),
    # group 3 year minus birth year: 6 for most pupils, 5 or 7 when the school decided otherwise
    g3_year_offset = g3_year - lubridate::year(date_of_birth),
    cohort = g3_year + 6L # leaving year
  ) %>%
  mutate(
    usual_offset = mode_int(g3_year_offset[reached_g3]),
    g3_timing = case_when(
      !reached_g3 ~ NA_character_,
      g3_year_offset < usual_offset ~ "earlier than most",
      g3_year_offset > usual_offset ~ "later than most",
      TRUE ~ "usual"
    )
  ) %>%
  select(-usual_offset)

# --- Observations before group 3 ------------------------------------------------------------

obs_before_g3 <- data_observatie %>%
  transmute(
    name_key = make_key(Leerling),
    vak = as.character(Vak),
    code = as.character(Leerpadcode),
    score = as_number(Score),
    date = as_date_cell(Datum, "mdy"),
    kg_class = normalise_class(Groep)
  ) %>%
  filter(!is.na(name_key), score %in% 0:3, !is.na(date)) %>%
  inner_join(select(pupil_timing, name_key, g3_start), by = "name_key") %>%
  filter(!is.na(g3_start), date < g3_start)

# Last observation of each goal before group 3
last_obs <- obs_before_g3 %>%
  arrange(desc(date)) %>%
  distinct(name_key, code, .keep_all = TRUE)

readiness <- last_obs %>%
  filter(vak %in% names(readiness_domains)) %>%
  mutate(domain = paste0("ready_", readiness_domains[vak])) %>%
  group_by(name_key, domain) %>%
  summarise(level = mean(score), .groups = "drop") %>%
  pivot_wider(names_from = domain, values_from = level)

goal_lists <- last_obs %>%
  group_by(name_key) %>%
  summarise(
    goals_observed = n(),
    basic_share = if (any(code %in% basic_codes)) mean(score[code %in% basic_codes] >= achieved_min) else NA_real_,
    basic_observed = sum(code %in% basic_codes),
    transition_share = if (any(code %in% transition_codes)) mean(score[code %in% transition_codes] >= achieved_min) else NA_real_,
    transition_observed = sum(code %in% transition_codes),
    .groups = "drop"
  )

# Kindergarten class: the class a pupil was observed in most
kg_class <- obs_before_g3 %>%
  filter(kg_class %in% c("1/2a", "1/2b")) %>%
  count(name_key, kg_class) %>%
  arrange(desc(n)) %>%
  distinct(name_key, .keep_all = TRUE) %>%
  select(name_key, kg_class)

# --- Kindergarten school years, limited to the years each export covers ----------------------

kg_years <- pupil_timing %>%
  filter(reached_g3) %>%
  transmute(name_key, school_year = year_span(entry_school_year, g3_year - 1L)) %>%
  unnest(school_year)

extra <- data_extra %>%
  transmute(name_key = make_key(Student), lesson = as.character(Lesson), school_year = sy_from_code(`School Year`)) %>%
  filter(!is.na(name_key), !is.na(school_year))
extra_years <- range(extra$school_year)

lessons_kg <- kg_years %>%
  filter(between(school_year, extra_years[1], extra_years[2])) %>%
  left_join(extra, by = c("name_key", "school_year"), relationship = "one-to-many") %>%
  group_by(name_key) %>%
  summarise(
    kg_years_with_lesson_data = n_distinct(school_year),
    lessons_kg = n_distinct(lesson, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(lessons_per_kg_year = lessons_kg / kg_years_with_lesson_data)

support <- data_intervention %>%
  transmute(name_key = make_key(Name), school_year = sy_from_code(`School yr`)) %>%
  filter(!is.na(name_key), !is.na(school_year)) %>%
  distinct()
support_years <- range(support$school_year)

# TRUE/FALSE when the support export covers that school year, NA otherwise
had_support <- function(keys, years) {
  covered <- between(years, support_years[1], support_years[2])
  hit <- paste(keys, years) %in% paste(support$name_key, support$school_year)
  if_else(covered, hit, NA)
}

support_kg <- kg_years %>%
  filter(between(school_year, support_years[1], support_years[2])) %>%
  mutate(any = had_support(name_key, school_year)) %>%
  group_by(name_key) %>%
  summarise(support_kg = any(any), .groups = "drop")

# Classroom quality (CLASS domain totals) of the kindergarten class, averaged over kindergarten years
class_quality_kg <- kg_years %>%
  inner_join(kg_class, by = "name_key") %>%
  inner_join(CLASS_yearly, by = c(school_year = "school_year_start", kg_class = "class_group")) %>%
  group_by(name_key) %>%
  summarise(
    class_emotional_kg = mean(`mean_Total Emotional Support`, na.rm = TRUE),
    class_organization_kg = mean(`mean_Total Classroom Organization`, na.rm = TRUE),
    class_instructional_kg = mean(`mean_Total Instructional Support`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(starts_with("class_"), ~ if_else(is.nan(.x), NA_real_, .x)))

# --- Outcomes in group 3 and 4 ---------------------------------------------------------------

cito_end <- data_grade %>%
  transmute(
    name_key = make_key(Name),
    grade = readr::parse_number(as.character(Grade)),
    moment = tolower(word(str_squish(as.character(`Assessment period`)), 1)),
    test = cito_tests[str_squish(as.character(Test))],
    ability = as_number(`Ability score`),
    band = as.character(`Performance band`)
  ) %>%
  filter(!is.na(name_key), moment == "end", grade %in% 3:4, !is.na(test), !is.na(ability)) %>%
  group_by(name_key, grade, test) %>%
  summarise(ability = mean(ability), low_band = any(band %in% c("IV", "V")), .groups = "drop")

cito_wide <- cito_end %>%
  pivot_wider(
    id_cols = name_key,
    names_from = c(grade, test),
    values_from = c(ability, low_band),
    names_glue = "{if_else(.value == 'ability', 'cito', 'low_band')}_g{grade}_{test}"
  )

wellbeing <- if (is.null(data_questionnaire)) {
  tibble(name_key = character(), wellbeing_g3 = double(), wellbeing_g4 = double())
} else {
  data_questionnaire %>%
    mutate(
      name_key = make_key(Leerling),
      year_group = as.integer(as.character(Leerjaar)),
      across(starts_with("Vraag_"), ~ suppressWarnings(as.numeric(.x)))
    ) %>%
    mutate(happiness = rowMeans(across(starts_with("Vraag_")), na.rm = TRUE)) %>%
    filter(!is.na(name_key), is.finite(happiness), year_group %in% 3:4) %>%
    group_by(name_key, year_group) %>%
    summarise(happiness = mean(happiness), .groups = "drop") %>%
    pivot_wider(names_from = year_group, values_from = happiness, names_prefix = "wellbeing_g")
}

# --- Assemble --------------------------------------------------------------------------------

pupils_table <- pupil_timing %>%
  left_join(kg_class, by = "name_key", relationship = "one-to-one") %>%
  left_join(readiness, by = "name_key", relationship = "one-to-one") %>%
  left_join(goal_lists, by = "name_key", relationship = "one-to-one") %>%
  left_join(lessons_kg, by = "name_key", relationship = "one-to-one") %>%
  left_join(support_kg, by = "name_key", relationship = "one-to-one") %>%
  left_join(class_quality_kg, by = "name_key", relationship = "one-to-one") %>%
  left_join(cito_wide, by = "name_key", relationship = "one-to-one") %>%
  left_join(wellbeing, by = "name_key", relationship = "one-to-one") %>%
  mutate(
    support_g3 = if_else(reached_g3, had_support(name_key, g3_year), NA),
    support_g4 = if_else(reached_g3, had_support(name_key, g3_year + 1L), NA)
  )

# --- The two tables the school asked for -----------------------------------------------------

reached <- filter(pupils_table, reached_g3)

summarise_kg <- function(d) {
  summarise(
    d,
    pupils = n(),
    mean_months = mean(months_to_g3),
    sd_months = sd(months_to_g3),
    min_months = min(months_to_g3),
    max_months = max(months_to_g3),
    group3_earlier_than_most = sum(g3_timing == "earlier than most"),
    group3_later_than_most = sum(g3_timing == "later than most"),
    mean_age_at_g3 = mean(age_at_g3)
  )
}

# Group 3 year minus birth year, by birth month: what the school does, without assuming a rule
g3_by_birth_month <- reached %>%
  mutate(
    birth_month = lubridate::month(date_of_birth, label = TRUE, abbr = TRUE, locale = "C"),
    offset = paste0("birth year + ", g3_year_offset)
  ) %>%
  count(birth_month, offset) %>%
  pivot_wider(names_from = offset, values_from = n, values_fill = 0) %>%
  select(birth_month, sort(setdiff(names(.), "birth_month")))

# Where each pupil's group 3 year comes from, and pupils who repeated a year after group 3
g3_sources_overview <- pupils_table %>%
  filter(reached_g3) %>%
  count(source = g3_source, name = "pupils") %>%
  bind_rows(tibble(
    source = "repeated a year after group 3 (current year group or leaving year a year behind)",
    pupils = sum(pupils_table$repeated_after_g3, na.rm = TRUE)
  ))

kg_overview <- bind_rows(
  summarise_kg(reached) %>% mutate(group = "All pupils who reached group 3"),
  reached %>% group_by(group = birth_quarter) %>% summarise_kg() %>% ungroup(),
  summarise_kg(filter(reached, autumn_born)) %>% mutate(group = "Born Oct-Dec (autumn)")
) %>%
  relocate(group)

summarise_goals <- function(d, share, list_name, n_goals) {
  x <- d[[share]][!is.na(d[[share]])]
  tibble(
    goal_list = list_name,
    goals_in_list = n_goals,
    pupils = length(x),
    mean_pct_achieved = 100 * mean(x),
    min_pct = 100 * min(x),
    p10_pct = 100 * quantile(x, 0.1, names = FALSE),
    p90_pct = 100 * quantile(x, 0.9, names = FALSE),
    max_pct = 100 * max(x)
  )
}

goals_overview <- bind_rows(
  summarise_goals(reached, "transition_share", "Goals before group 3", length(transition_codes)),
  summarise_goals(reached, "basic_share", "Basic goals groups 1-2", length(basic_codes))
)

# What each column means and how many pupils who reached group 3 have it
pupils_table_columns <- tribble(
  ~column, ~meaning,
  "g3_year / g3_source", "School year group 3 started, and where that comes from (Cito, maths tests, questionnaire, current year group)",
  "months_to_g3", "Months from start at school to 20 August of the group 3 year; in this export the same as (age at group 3 - 4)",
  "age_at_start / age_at_g3", "Age in years at start of school / at start of group 3",
  "g3_year_offset / g3_timing", "Group 3 year minus birth year, and whether that is the usual offset or earlier / later than most",
  "repeated_after_g3", "Current year group or Cito leaving year is a year behind the group 3 year",
  "has_cito_g3", "Has a grade 3 Cito result (pupils who just started group 3 do not yet)",
  "kg_class", "Kindergarten class the pupil was observed in most (1/2a or 1/2b)",
  "ready_<domain>", "Mean of the last score (0-3) per goal before group 3, per subject",
  "transition_share / basic_share", "Share of the client's goals at score >= 2 on the last observation before group 3",
  "lessons_kg / lessons_per_kg_year", "Distinct extracurricular lessons in kindergarten school years the export covers",
  "support_kg / support_g3 / support_g4", "Any support record in those school years (empty when the export does not cover them)",
  "class_<domain>_kg", "CLASS domain total of the kindergarten class, averaged over kindergarten years",
  "cito_g<n>_<test>", "Cito ability score (0-5) at the end of group 3 or 4",
  "low_band_g<n>_<test>", "Cito band IV or V at the end of group 3 or 4",
  "wellbeing_g3 / wellbeing_g4", "Mean of the 7 questionnaire items (1-3) in group 3 or 4"
)

pupils_table_coverage <- reached %>%
  summarise(across(
    c(months_to_g3, kg_class, starts_with("ready_"), transition_share, basic_share, lessons_kg,
      support_kg, starts_with("class_"), starts_with("cito_"), support_g3, support_g4, starts_with("wellbeing_")),
    ~ sum(!is.na(.x))
  )) %>%
  pivot_longer(everything(), names_to = "column", values_to = "pupils_with_value")
