# Set-Up
pacman::p_load(dplyr, magrittr, tidyr, splitstackshape)
set.seed(831)

# Create generic class-level data
class_data <- data.frame(
  class_id = LETTERS,
  class_frl_cat = rep(c("Under 50% FRL", "At least 50% FRL"), each = 13),
  class_soc_cat = c(
    rep(c("0-25% SOC", "26-50% SOC", "51-75% SOC", "76-100% SOC"), each = 6),
    rep("76-100% SOC", 2)
  ),
  class_mean_0_3_t1 = seq(2.25, 0.25, length.out = 26),
  class_mean_0_3_t2 = seq(2.5, 0.5, length.out = 26),
  class_sd_0_3 = c(rep(0.25, 5), rep(0.5, 16), rep(0.25, 5)),
  stringsAsFactors = F
)

# Student Survey Data. Generate 1000 surveys per class with forced inequities. Then split in half
# for two time points
ss_data_initial <-
  data.frame(
    class_id = sample(LETTERS, 1000, replace = T),
    response_id = 1:1000,
    stringsAsFactors = F
  ) %>%
  left_join(class_data) %>%
  expandRows(12, count.is.col = F) %>%
  mutate(
    response = round(rnorm(NROW(.), mean = .$class_mean_0_3_t1, sd = .$class_sd_0_3)),
    response = ifelse(!response %in% 0:3, NA, response),
    item = rep(c(
      "eng_like",
      "eng_losttrack",
      "eng_interest",
      "eng_moreabout",
      "tch_problem",
      "bel_ideas",
      "bel_fitin",
      "tch_interestedideas",
      "rel_asmuch",
      "rel_future",
      "rel_outside",
      "rel_rightnow"),
      1000
    )
  ) %>%
  spread(key = item, value = response) %>%
  select(-(class_mean_0_3_t1:class_sd_0_3))

ss_data_final <-
  data.frame(
    class_id = sample(LETTERS, 1000, replace = T),
    response_id = 1:1000,
    stringsAsFactors = F
  ) %>%
  left_join(class_data) %>%
  expandRows(12, count.is.col = F) %>%
  mutate(
    response = round(rnorm(NROW(.), mean = .$class_mean_0_3_t2, sd = .$class_sd_0_3)),
    response = ifelse(!response %in% 0:3, NA, response),
    item = rep(c(
      "eng_like",
      "eng_losttrack",
      "eng_interest",
      "eng_moreabout",
      "tch_problem",
      "bel_ideas",
      "bel_fitin",
      "tch_interestedideas",
      "rel_asmuch",
      "rel_future",
      "rel_outside",
      "rel_rightnow"),
      1000
    )
  ) %>%
  spread(key = item, value = response) %>%
  select(-(class_mean_0_3_t1:class_sd_0_3))

usethis::use_data(ss_data_initial, overwrite = TRUE)
usethis::use_data(ss_data_final, overwrite = TRUE)

# Create fake IPG data (without class characteristics)
ipg_data <-
  data.frame(
    observation_number = 1:100,
    form = sample(c("Literacy", "Math", "Science", "Social Studies"), 100, replace = T),
    grade_level = sample(0:12, 100, replace = T),
    ca1_a = sample(0:1, 100, replace = T),
    ca1_b = sample(0:1, 100, replace = T),
    ca1_c = sample(0:1, 100, replace = T),
    ca1_d = sample(0:1, 100, replace = T),
    ca1_e = sample(0:1, 100, replace = T),
    ca1_f = sample(0:1, 100, replace = T),
    ca2_overall = sample(1:4, 100, replace = T),
    ca3_overall = sample(1:4, 100, replace = T),
    col = sample(1:4, 100, replace = T),
    rfs_overall = sample(1:4, 100, replace = T),
    science_filter = sample(
      c("Text", "Inquiry and Scientific Practice", "Both", "Neither"),
      100,
      replace = T
    ),
    stringsAsFactors = F
  ) %>%
  mutate(
    grade_level = ifelse(
      grade_level <= 5 & form %in% c("Science", "Social Studies"),
      grade_level + 6,
      grade_level
    ),
    ca1_c = ifelse(
      form == "Science" & science_filter %in% c("Inquiry and Scientific Practice", "Neither"),
      NA,
      ca1_c
    ),
    ca1_d = ifelse(form == "Science" & !science_filter %in% c("Text", "Both"), NA, ca1_d),
    ca1_e = ifelse(form == "Science" & !science_filter %in% c("Text", "Both"), NA, ca1_e),
    ca1_f = ifelse(
      form == "Science" & !science_filter %in% c("Inquiry and Scientific Practice", "Both"),
      NA,
      ca1_f
    ),
    rfs_overall = ifelse(form == "Literacy" & grade_level <= 5, rfs_overall, NA)
  ) %>%
  mutate_at(
    vars(ca1_d, ca1_e, ca1_f, science_filter),
    ~ ifelse(form != "Science", NA, .)
  )

usethis::use_data(ipg_data, overwrite = TRUE)

# CMS Data from previous project.
cms_data <-
  readr::read_csv("~/repos/os/nc_cms/data/clean/school_level/nc_cms_school_sampling_frame.csv")

cms_data %<>%
  mutate(
    grade_level_cat = stringr::str_sub(grade_level_cat, 3),
    soc_percent = 1 - white_percent
  ) %>%
  select(
    school_name, grade_level_cat,
    frl_percent, soc_percent,
    spg_score, spg_grade,
    total_students_2018
  )

# Impute missing data with district median
cms_data %<>%
  mutate_at(
    vars(frl_percent, soc_percent, spg_score, total_students_2018),
    ~ ifelse(is.na(.), median(., na.rm = T), .)
  ) %>%
  mutate(
    spg_grade = ifelse(is.na(spg_grade), "I", spg_grade)
  )

usethis::use_data(cms_data, overwrite = TRUE)
