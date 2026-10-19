# What contributes to the outcomes? Kindergarten factors in blocks (time, classroom,
# extracurricular, support, readiness), each split into kinds where possible; how much of the
# explained variance each block accounts for.
# Needs R/pupil_table.R to have been sourced.

ev_bootstrap_reps <- 300 # resamples for the intervals around each block's share
ev_noise_reps <- 300 # shuffles of the outcome for each block's share by chance alone

# --- Lesson categories ----------------------------------------------------------------------

# Lesson names seen in the export, by kind; names not listed fall back on keyword rules
lesson_kinds <- c(
  sport = "Sport & movement", arts_culture = "Arts & culture", technology_science = "Technology, science & strategy",
  language_numbers = "Language & numbers", life_skills_play = "Life skills & play"
)
lesson_map <- list(
  sport = c(
    "Football", "Multi-sport", "Korfball", "Yoga", "Judo", "Street Dance", "Circus", "Self-defence",
    "Outdoor Sports", "Tiger Boxing", "Climbing and Scrambling", "Capoeira", "Handball and Other Ball Sports",
    "Dance", "Hockey", "Taekwondo", "Breakdance", "Handball", "Girls' Football", "Just Dance", "Basketball",
    "Physical Activity", "BeeBall", "Kidsfit", "Sports and Games", "Gamegym"
  ),
  arts_culture = c(
    "Drama", "Fashion and Design", "Music", "Art Workshop", "Art", "Ebru Art", "Singing and Music",
    "Drawing Comics", "Art and Creation", "Craft Studio", "Ceramics", "Children's Choir", "Drawing", "Painting",
    "Art Appreciation", "Music and Theatre", "Photography", "Artists", "Craft Party", "Creativity and Art",
    "Waste and Art", "Creative Drawing", "Creative Drawing and Play", "Puppet Theatre", "Movement and Drumming",
    "Theatre", "Sustainable Fashion Class", "Arts and Crafts", "DJ", "Musical", "Singing and Dance",
    "Dance and Theatre", "Upcycle Your Toys", "Art Lab", "Video Game Music", "Graffiti", "Jewellery Making",
    "Fuse Beads", "Creative Class"
  ),
  technology_science = c(
    "Plants and Gardening", "Chess", "Technology", "Technical LEGO", "Science", "Gardening Lessons",
    "Nature and the Environment", "Nature", "Media and ICT", "Chemical Chaos", "Machine Builders",
    "Working with Sustainable Energy", "Making a Podcast", "Draughts"
  ),
  language_numbers = c(
    "English", "Arabic Lessons", "Beginner Arabic Lessons", "Advanced Arabic Lessons", "Reading for Pleasure",
    "Making a School Newspaper", "Reading Aloud", "A Treasure Trove of Words", "Rhyming", "Stories",
    "Playing with Numbers", "Managing Money", "Money", "Homework Support and Physical Activity"
  ),
  life_skills_play = c(
    "Cooking", "High Tea", "Taste Lessons", "Mental Resilience", "Rock and Water", "Pirate Club", "Outdoor Play",
    "Play and Fun", "Learning through Play", "Free-choice Lesson", "Games"
  )
)
lesson_lookup <- set_names(rep(names(lesson_map), lengths(lesson_map)), tolower(unlist(lesson_map)))

# Fallback for names not in the list (English and Dutch); the first matching kind wins
lesson_rules <- c(
  language_numbers = "english|engels|spanish|spaans|turkish|turks|arabic|arabisch|reading|lezen|lees|debat|writing|schrij|book|boek|newspaper|krant|story|stories|verhaal|language|taal|poetry|spoken|number|getal|reken|money|geld|homework|huiswerk",
  life_skills_play = "cook|kook|bak|tea|taste|proef|resilien|weerbaar|play|spel|pirate",
  technology_science = "coding|program|robot|lego|minecraft|3d|science|wetenschap|electron|animation|techn|nature|natuur|garden|tuin|chess|schaak|dam|ict|media|podcast|energy|energie|machine|chemi",
  arts_culture = "drama|theat|toneel|choir|koor|sing|zang|music|muziek|guitar|gitaar|piano|drum|violin|viool|art|kunst|paint|schilder|draw|teken|ceramic|keramiek|photo|foto|film|fashion|mode|craft|knutsel|rap|dj|musical|percussion|ukulele|comic|strip|clay|klei|wood|hout|sew|naai|puppet|poppen|magic|goochel|graffiti|bead|jewel|upcycl|creativ",
  sport = "football|voetbal|soccer|basket|judo|kickbox|box|gym|turn|swim|zwem|athlet|atletiek|tennis|hockey|korfbal|yoga|climb|klim|skate|schaats|volley|badminton|freerun|capoeira|karate|taekwondo|rugby|handbal|baseball|honkbal|cycling|fiets|padel|golf|archery|fenc|frisbee|floorball|polo|triathlon|movers|sport|fitness|run|bewegen|physical|self-defen|zelfverdediging|dance|dans|ballet|circus|ball|fit"
)

lesson_category <- function(lesson) {
  x <- str_squish(tolower(as.character(lesson)))
  out <- unname(lesson_lookup[x])
  for (cat in names(lesson_rules)) {
    hit <- is.na(out) & str_detect(x, lesson_rules[[cat]])
    hit[is.na(hit)] <- FALSE
    out[hit] <- cat
  }
  out[is.na(out)] <- "other"
  out
}

lesson_categories <- data_extra %>%
  count(lesson = as.character(Lesson), name = "rows") %>%
  mutate(
    category = lesson_category(lesson),
    kind = coalesce(unname(lesson_kinds[category]), "Other"),
    assigned_by = if_else(str_squish(tolower(lesson)) %in% names(lesson_lookup), "list", "keywords")
  ) %>%
  arrange(kind, desc(rows)) %>%
  select(lesson, rows, kind, assigned_by)

# Distinct lessons per kindergarten year, per category
lessons_kg_by_category <- kg_years %>%
  filter(between(school_year, extra_years[1], extra_years[2])) %>%
  group_by(name_key) %>%
  mutate(kg_years_with_lesson_data = n_distinct(school_year)) %>%
  ungroup() %>%
  left_join(extra, by = c("name_key", "school_year"), relationship = "one-to-many") %>%
  mutate(category = if_else(is.na(lesson), NA_character_, lesson_category(lesson))) %>%
  group_by(name_key, kg_years_with_lesson_data) %>%
  summarise(
    across_cats = list(map_dbl(names(lesson_kinds), function(cat) n_distinct(lesson[category %in% cat]))),
    .groups = "drop"
  ) %>%
  mutate(across_cats = map(across_cats, ~ set_names(as.list(.x), paste0("lessons_", names(lesson_kinds))))) %>%
  unnest_wider(across_cats) %>%
  mutate(across(starts_with("lessons_"), ~ .x / kg_years_with_lesson_data)) %>%
  select(-kg_years_with_lesson_data)

# --- Support during kindergarten, by kind of need --------------------------------------------

support_kind <- function(need) {
  need <- tolower(as.character(need))
  case_when(
    str_detect(need, "language") ~ "language",
    str_detect(need, "learning") ~ "learning",
    is.na(need) ~ "other",
    TRUE ~ "other"
  )
}

support_kg_by_kind <- kg_years %>%
  filter(between(school_year, support_years[1], support_years[2])) %>%
  distinct(name_key) %>%
  left_join(
    data_intervention %>%
      transmute(name_key = make_key(Name), school_year = sy_from_code(`School yr`), kind = support_kind(Need)) %>%
      semi_join(kg_years, by = c("name_key", "school_year")),
    by = "name_key", relationship = "one-to-many"
  ) %>%
  group_by(name_key) %>%
  summarise(
    support_language = as.numeric(any(kind %in% "language")),
    support_learning = as.numeric(any(kind %in% "learning")),
    support_other = as.numeric(any(kind %in% "other")),
    .groups = "drop"
  )

# --- One table of features per pupil -------------------------------------------------------

within_class_z <- function(x, class) {
  ave(x, class, FUN = function(v) (v - mean(v, na.rm = TRUE)) / sd(v, na.rm = TRUE))
}

ev_data <- pupils_table %>%
  filter(reached_g3, !is.na(kg_class)) %>%
  left_join(lessons_kg_by_category, by = "name_key", relationship = "one-to-one") %>%
  left_join(support_kg_by_kind, by = "name_key", relationship = "one-to-one") %>%
  mutate(
    class_1_2b = as.numeric(kg_class == "1/2b"),
    # one CLASS total: the three domains are measured on the same class-years and nearly collinear
    class_total_kg = (class_emotional_kg + class_organization_kg + class_instructional_kg) / 3,
    # pupils whose kindergarten years the support file does not cover (the first cohorts) count
    # as without support; the cohort control keeps that from looking like an effect
    across(c(support_language, support_learning, support_other), ~ coalesce(.x, 0)),
    across(starts_with("lessons_"), ~ coalesce(.x, 0)),
    # readiness compared with classmates: teachers in the two classes score differently
    ready_motor = (ready_fine_motor + ready_gross_motor) / 2,
    across(c(ready_maths, ready_language, ready_social_emotional, ready_motor), ~ within_class_z(.x, kg_class), .names = "{.col}_z"),
    support_g3 = as.numeric(support_g3),
    cohort = factor(cohort)
  )

ev_blocks <- list(
  Time = c(`months in kindergarten` = "months_to_g3"),
  Classroom = c(
    `class 1/2b (vs 1/2a)` = "class_1_2b",
    `CLASS total (average of the domains)` = "class_total_kg"
  ),
  Extracurricular = set_names(paste0("lessons_", names(lesson_kinds)), paste0("lessons: ", tolower(lesson_kinds))),
  Support = c(
    `support: language` = "support_language",
    `support: learning` = "support_learning",
    `support: other` = "support_other"
  ),
  Readiness = c(
    `readiness: maths` = "ready_maths_z",
    `readiness: language` = "ready_language_z",
    `readiness: social-emotional` = "ready_social_emotional_z",
    `readiness: motor` = "ready_motor_z"
  )
)
ev_feature_labels <- set_names(names(unlist(unname(ev_blocks))), unlist(unname(ev_blocks)))
ev_feature_block <- set_names(rep(names(ev_blocks), lengths(ev_blocks)), unlist(unname(ev_blocks)))

ev_outcomes <- tribble(
  ~outcome, ~label, ~with_readiness,
  "transition_share", "Goals before group 3 achieved", FALSE,
  "cito_g3_maths", "Cito maths, end of group 3", TRUE,
  "cito_g3_tmt", "Cito TMT, end of group 3", TRUE,
  "cito_g3_spelling", "Cito spelling, end of group 3", TRUE,
  "support_g3", "Any support in group 3", TRUE
)

# --- Features against outcomes ---------------------------------------------------------------

binary_features <- c("class_1_2b", "support_language", "support_learning", "support_other")

ev_feature_plot <- function(outcome, outcome_label) {
  features <- unname(unlist(ev_blocks))
  if (outcome == "transition_share") features <- setdiff(features, unname(ev_blocks$Readiness))
  features <- features[map_lgl(features, ~ n_distinct(ev_data[[.x]], na.rm = TRUE) > 1)]
  ev_data %>%
    select(all_of(c(outcome, features))) %>%
    pivot_longer(-all_of(outcome), names_to = "feature", values_to = "x") %>%
    rename(y = all_of(outcome)) %>%
    filter(!is.na(x), !is.na(y)) %>%
    mutate(
      # yes/no features are spread a little sideways (fixed seed) so the points do not overlap
      x_shown = if_else(feature %in% binary_features, x + withr::with_seed(1, runif(n(), -0.08, 0.08)), x),
      feature = factor(ev_feature_labels[feature], levels = ev_feature_labels[features])
    ) %>%
    ggplot(aes(x, y)) +
    geom_point(aes(x = x_shown), alpha = 0.5, size = 1.3) +
    geom_smooth(method = "lm", formula = y ~ x, colour = "#0072B2", linewidth = 0.7) +
    facet_wrap(~feature, scales = "free_x", ncol = 5) +
    labs(x = NULL, y = outcome_label) +
    theme_minimal(base_size = 9)
}

# --- Model data and fits -------------------------------------------------------------------

# Complete cases for one outcome; continuous features and the outcome in standard deviations,
# 0/1 features unchanged
ev_model_data <- function(outcome, with_readiness) {
  blocks <- if (with_readiness) ev_blocks else ev_blocks[names(ev_blocks) != "Readiness"]
  blocks <- map(blocks, unname) # labels as names would rename columns in select()
  features <- unname(unlist(blocks))
  d <- ev_data %>%
    select(all_of(c(outcome, features, "cohort"))) %>%
    drop_na() %>%
    droplevels()
  constant <- features[map_lgl(features, ~ n_distinct(d[[.x]]) < 2)]
  blocks <- map(blocks, ~ .x[!.x %in% constant])
  blocks <- blocks[lengths(blocks) > 0]
  features <- unname(unlist(blocks))
  continuous <- setdiff(features, binary_features)
  d <- d %>% mutate(across(all_of(c(outcome, continuous)), ~ as.numeric(scale(.x))))
  list(data = d, blocks = blocks, outcome = outcome)
}

ev_fit <- function(md) {
  controls <- if (nlevels(md$data$cohort) > 1) "cohort" else character(0)
  lm(reformulate(c(unlist(unname(md$blocks)), controls), response = md$outcome), data = md$data)
}

ev_coefficients <- function(md, label) {
  fit <- ev_fit(md)
  est <- coef(summary(fit))
  features <- intersect(unlist(unname(md$blocks)), rownames(est))
  tibble(
    outcome = label,
    n = nrow(md$data),
    block = unname(ev_feature_block[features]),
    feature = unname(ev_feature_labels[features]),
    estimate = est[features, 1],
    ci_low = est[features, 1] - qnorm(0.975) * est[features, 2],
    ci_high = est[features, 1] + qnorm(0.975) * est[features, 2],
    p = est[features, 4]
  )
}

# --- Explained variance per block ----------------------------------------------------------

# R2 above a cohort-only model for every combination of blocks, then each block's average
# contribution over all orders in which blocks can be added (shares add up to the total)
block_r2 <- function(y, x_blocks, x_controls) {
  k <- length(x_blocks)
  tss <- sum((y - mean(y))^2)
  r2_of <- function(cols) {
    x <- cbind(1, x_controls, cols)
    1 - sum(.lm.fit(x, y)$residuals^2) / tss
  }
  base <- r2_of(NULL)
  subsets <- expand.grid(rep(list(c(FALSE, TRUE)), k))
  r2 <- apply(subsets, 1, function(s) r2_of(do.call(cbind, x_blocks[unlist(s)])) - base)
  key <- apply(subsets, 1, function(s) paste(as.integer(s), collapse = ""))
  r2 <- set_names(r2, key)
  share <- map_dbl(seq_len(k), function(j) {
    others <- subsets[!subsets[[j]], , drop = FALSE]
    sum(apply(others, 1, function(s) {
      size <- sum(s)
      with_j <- s
      with_j[j] <- TRUE
      w <- factorial(size) * factorial(k - size - 1) / factorial(k)
      w * (r2[[paste(as.integer(with_j), collapse = "")]] - r2[[paste(as.integer(s), collapse = "")]])
    }))
  })
  full <- r2[[paste(rep(1, k), collapse = "")]]
  dropped <- map_dbl(seq_len(k), function(j) {
    s <- rep(1L, k)
    s[j] <- 0L
    full - r2[[paste(s, collapse = "")]]
  })
  list(share = set_names(share, names(x_blocks)), dropped = set_names(dropped, names(x_blocks)), total = full, base = base)
}

ev_variance <- function(md, label, framing) {
  d <- md$data
  y <- d[[md$outcome]]
  x_blocks <- map(md$blocks, ~ as.matrix(d[, .x, drop = FALSE]))
  x_controls <- if (nlevels(d$cohort) > 1) model.matrix(~cohort, d)[, -1, drop = FALSE] else NULL
  point <- block_r2(y, x_blocks, x_controls)
  boot <- map(seq_len(ev_bootstrap_reps), function(b) {
    i <- sample(nrow(d), replace = TRUE)
    xc <- if (is.null(x_controls)) NULL else x_controls[i, , drop = FALSE]
    block_r2(y[i], map(x_blocks, ~ .x[i, , drop = FALSE]), xc)$share
  }) %>%
    do.call(rbind, .)
  # with the outcome shuffled nothing is truly explained: what each block gets by chance,
  # which grows with the number of features in the block
  noise <- map(seq_len(ev_noise_reps), function(b) {
    block_r2(sample(y), x_blocks, x_controls)$share
  }) %>%
    do.call(rbind, .)
  tibble(
    outcome = label,
    framing = framing,
    n = nrow(d),
    block = names(md$blocks),
    r2_share = point$share,
    r2_share_low = apply(boot, 2, quantile, 0.025, names = FALSE),
    r2_share_high = apply(boot, 2, quantile, 0.975, names = FALSE),
    noise_level = colMeans(noise),
    above_noise = point$share - colMeans(noise),
    features_in_block = lengths(md$blocks),
    pct_of_explained = 100 * point$share / point$total,
    r2_lost_without_block = point$dropped,
    total_r2_above_cohort = point$total,
    cohort_r2 = point$base
  )
}

set.seed(20261019)
ev_runs <- ev_outcomes %>%
  select(outcome, label, with_readiness) %>%
  pmap(function(outcome, label, with_readiness) {
    runs <- list(list(md = ev_model_data(outcome, FALSE), framing = "kindergarten factors"))
    if (with_readiness) {
      runs <- c(runs, list(list(md = ev_model_data(outcome, TRUE), framing = "kindergarten factors + readiness")))
    }
    map(runs, ~ list(
      coefficients = ev_coefficients(.x$md, label) %>% mutate(framing = .x$framing, .after = outcome),
      variance = ev_variance(.x$md, label, .x$framing)
    ))
  }) %>%
  flatten()

ev_coefficient_table <- map(ev_runs, "coefficients") %>% list_rbind()
ev_variance_table <- map(ev_runs, "variance") %>% list_rbind()

ev_variance_summary <- ev_variance_table %>%
  distinct(outcome, framing, n, total_r2_above_cohort, cohort_r2)

ev_variance_plot <- ev_variance_table %>%
  mutate(
    outcome = factor(outcome, levels = rev(ev_outcomes$label)),
    block = factor(block, levels = names(ev_blocks))
  ) %>%
  ggplot(aes(r2_share, outcome, colour = block)) +
  geom_vline(xintercept = 0, colour = "grey70") +
  geom_point(aes(x = noise_level, group = block), shape = 124, size = 4, colour = "grey40", position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(xmin = r2_share_low, xmax = r2_share_high), width = 0.25, orientation = "y",
                position = position_dodge(width = 0.7)) +
  geom_point(size = 2, position = position_dodge(width = 0.7)) +
  facet_wrap(~framing) +
  scale_colour_manual(values = c(Time = "#D55E00", Classroom = "#0072B2", Extracurricular = "#009E73",
                                 Support = "#CC79A7", Readiness = "#E69F00")) +
  labs(x = "Share of explained variance (R², above cohort; 95% interval). Grey tick: level expected by chance", y = NULL, colour = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ev_coefficient_plot <- ev_coefficient_table %>%
  filter(framing == "kindergarten factors + readiness" | outcome == "Goals before group 3 achieved") %>%
  mutate(
    feature = factor(feature, levels = rev(unname(ev_feature_labels))),
    block = factor(block, levels = names(ev_blocks))
  ) %>%
  ggplot(aes(estimate, feature, colour = block)) +
  geom_vline(xintercept = 0, colour = "grey70") +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), width = 0.25, orientation = "y") +
  geom_point(size = 1.8) +
  facet_wrap(~outcome, nrow = 1) +
  scale_colour_manual(values = c(Time = "#D55E00", Classroom = "#0072B2", Extracurricular = "#009E73",
                                 Support = "#CC79A7", Readiness = "#E69F00")) +
  labs(x = "Effect in standard deviations of the outcome (95% interval)", y = NULL, colour = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom")
