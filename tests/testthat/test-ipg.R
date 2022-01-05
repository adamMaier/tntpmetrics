# DESCRIPTION:
#   This test file checks to see if IPG scores are made correctly for various
#   subject, grade-level, and rating combinations.


# SET-UP -----------------------------------------------------------------------

library(tntpmetrics)
library(dplyr)
library(magrittr)
set.seed(831)

context("IPG Scoring")



# TEST THAT SCORES MATCH MANUAL CALCULATIONS -----------------------------------

# Drop RFS overall from some K-5 Literacy observations, and then also some Core
# Actions, since in reality these aren't always observed together.
k5_obs <- ipg_data %>%
  filter(grade_level <= 5 & form == "Literacy") %>%
  select(observation_number) %>%
  pull()

ipg_data %<>%
  mutate(
    rfs_overall = ifelse(observation_number == k5_obs[1], NA, rfs_overall),
    ca1_a = ifelse(observation_number == k5_obs[2], NA, ca1_a),
    ca1_b = ifelse(observation_number == k5_obs[2], NA, ca1_b),
    ca1_c = ifelse(observation_number == k5_obs[2], NA, ca1_c),
    ca2_overall = ifelse(observation_number == k5_obs[2], NA, ca2_overall),
    ca3_overall = ifelse(observation_number == k5_obs[2], NA, ca3_overall),
    ca1_a = ifelse(observation_number == k5_obs[3], NA, ca1_a),
    ca2_overall = ifelse(observation_number == k5_obs[4], NA, ca2_overall)
  )

# Manually calculate IPG scores based on code from recent Opportunity Scorecard
ipg_data_comparison <- ipg_data %>%
  mutate(
    ca1_overall = ca1_a + ca1_b + ca1_c,
    ca2_overall = ca2_overall - 1,
    ca3_overall = ca3_overall - 1,
    rfs_overall = rfs_overall - 1,
    col = col - 1,
    science1_overall = as.double(ca1_a + ca1_b),
    science1_overall = case_when(
      science_filter == "Text" & (ca1_c + ca1_d + ca1_e == 3) ~ science1_overall + 1,
      science_filter == "Text" & (ca1_c + ca1_d + ca1_e != 3) ~ science1_overall,
      science_filter == "Inquiry and Scientific Practice" ~ science1_overall + ca1_f,
      science_filter == "Both" & (ca1_c + ca1_d + ca1_e + ca1_f == 4) ~ science1_overall + 1,
      science_filter == "Both" & (ca1_c + ca1_d + ca1_e + ca1_f != 4) ~ science1_overall,
      science_filter == "Neither" ~ science1_overall
    ),
    ca1_overall = ifelse(form == "Science", science1_overall, ca1_overall),
    science1_overall = NULL,
    obs_composite = (col + ca1_overall + ca2_overall + ca3_overall) / 4,
    obs_composite = case_when(
      grade_level <= 5 & form == "Literacy" & !is.na(rfs_overall) & !is.na(obs_composite) ~
        0.75 * ((rfs_overall + ca1_overall + ca2_overall + ca3_overall )/ 4) + 0.25 * col,
      grade_level <= 5 & form == "Literacy" & !is.na(rfs_overall) & is.na(obs_composite) ~
        0.75 * rfs_overall + 0.25 * col,
      TRUE ~ obs_composite
    )
  )



# RUN TESTS --------------------------------------------------------------------

# Test that calculated scores are the same
test_that("IPG scores match manual calculations", {
  expect_equal(
    make_metric(ipg_data, "ipg")$cm_ipg,
    ipg_data_comparison$obs_composite)
})

# Test that function returns all domains on original scale
test_that("IPG scores match manual calculations", {
  expect_equal(
    select(make_metric(ipg_data, "ipg"), -cm_ipg, -ca1_overall, -cm_binary_ipg),
    ipg_data
  )
})

# Test that make_metric still runs if no K-5 Literacy observations or no science
# observations
ipg_nosci <- ipg_data %>%
  filter(form != "Science")

ipg_noscica <- ipg_data %>%
  filter(form != "Science") %>%
  select(-science_filter, -(ca1_d:ca1_f))

ipg_nok5lit <- ipg_data %>%
  filter(form != "Literacy" | grade_level >= 6) %>%
  select(-rfs_overall)

test_that("IPG make_metric runs without K-5 Lit or Science", {
  expect_error(make_metric(ipg_nosci, "ipg"), NA)
  expect_error(make_metric(ipg_noscica, "ipg"), NA)
  expect_error(make_metric(ipg_nok5lit, "ipg"), NA)
})

# Test if error is produced if missing needed variable
test_that("Error produced without required IPG variable", {
  expect_error(make_metric(select(ipg_data, -form), "ipg"), "missing the following")
  expect_error(make_metric(select(ipg_data, -grade_level), "ipg"), "missing the following")
  expect_error(make_metric(select(ipg_data, -ca1_a), "ipg"), "missing the following")
  expect_error(make_metric(select(ipg_data, -ca1_b), "ipg"), "missing the following")
  expect_error(make_metric(select(ipg_data, -ca1_c), "ipg"), "missing the following")
  expect_error(make_metric(select(ipg_data, -ca1_d), "ipg"), "missing the following")
  expect_error(make_metric(select(ipg_data, -ca1_e), "ipg"), "missing the following")
  expect_error(make_metric(select(ipg_data, -ca1_f), "ipg"), "missing the following")
  expect_error(make_metric(select(ipg_data, -ca2_overall), "ipg"), "missing the following")
  expect_error(make_metric(select(ipg_data, -ca3_overall), "ipg"), "missing the following")
  expect_error(make_metric(select(ipg_data, -col), "ipg"), "missing the following")
  expect_error(make_metric(select(ipg_data, -starts_with("ca1")), "ipg"), "missing the following")
  expect_error(make_metric(select(ipg_data, -science_filter), "ipg"), "missing the following")
  expect_error(make_metric(select(ipg_data, -rfs_overall), "ipg"), "Data contains K-5")
})

# Test if error is produced if values are out of scale
test_that("Error produced with out of scale IPG variable", {
  expect_error(make_metric(mutate(ipg_data, form = "99"), "ipg"), "values other than")
  expect_error(make_metric(mutate(ipg_data, form = NA), "ipg"), "NAs are not allowed")
  expect_error(make_metric(mutate(ipg_data, grade_level = "99"), "ipg"), "values other than")
  expect_error(make_metric(mutate(ipg_data, grade_level = NA), "ipg"), "NAs are not allowed")
  expect_error(make_metric(mutate(ipg_data, science_filter = "99"), "ipg"), "unexpected value")
  expect_error(make_metric(mutate(ipg_data, science_filter = NA), "ipg"), "NAs are not allowed")
  expect_error(make_metric(mutate(ipg_data, ca1_a = 99), "ipg"), "out of scale")
  expect_error(make_metric(mutate(ipg_data, ca1_b = 99), "ipg"), "out of scale")
  expect_error(make_metric(mutate(ipg_data, ca1_c = 99), "ipg"), "out of scale")
  expect_error(make_metric(mutate(ipg_data, ca1_d = 99), "ipg"), "out of scale")
  expect_error(make_metric(mutate(ipg_data, ca1_e = 99), "ipg"), "out of scale")
  expect_error(make_metric(mutate(ipg_data, ca1_f = 99), "ipg"), "out of scale")
  expect_error(make_metric(mutate(ipg_data, ca2_overall = 99), "ipg"), "out of scale")
  expect_error(make_metric(mutate(ipg_data, ca3_overall = 99), "ipg"), "out of scale")
  expect_error(make_metric(mutate(ipg_data, rfs_overall = 99), "ipg"), "out of scale")
  expect_error(make_metric(mutate(ipg_data, col = 99), "ipg"), "out of scale")
})

# Test if warnings are produced if NA on key variables
test_that("Error produced with out of scale IPG variable", {
  expect_warning(make_metric(mutate(ipg_data, ca1_a = NA), "ipg"), "missing score")
  expect_warning(make_metric(mutate(ipg_data, ca1_b = NA), "ipg"), "missing score")
  expect_warning(make_metric(mutate(ipg_data, ca1_c = NA), "ipg"), "missing score")
  expect_warning(make_metric(mutate(ipg_data, ca1_d = NA), "ipg"), "missing score")
  expect_warning(make_metric(mutate(ipg_data, ca1_e = NA), "ipg"), "missing score")
  expect_warning(make_metric(mutate(ipg_data, ca1_f = NA), "ipg"), "missing score")
  expect_warning(make_metric(mutate(ipg_data, ca2_overall = NA), "ipg"), "missing score")
  expect_warning(make_metric(mutate(ipg_data, ca3_overall = NA), "ipg"), "missing score")
  expect_warning(make_metric(mutate(ipg_data, rfs_overall = NA), "ipg"), "missing an overall RFS score")
  expect_warning(make_metric(mutate(ipg_data, col = NA), "ipg"), "missing score")
})
