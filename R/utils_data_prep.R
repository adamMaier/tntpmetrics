# These are helper functions to prepare data for analysis. Most of these functions check for common
# errors in data, such as ensuring all metric indicators are included and on the right scale. IPG
# has a unique scoring approach and needs, so IPG related helper functions are separate.

# Function to make sure items are in data and spelled as needed. Lists those that are missing.
data_name_check <- function(data, needed_items) {
  var_check <- needed_items[!needed_items %in% names(data)]
  if (!rlang::is_empty(var_check)) {
    stop(
      paste(
        "Data set", deparse(substitute(data)), "is missing the following variable(s):",
        paste0(var_check, collapse = ", "), "\n",
        "Make sure spelling is correct."
      ),
      call. = FALSE
    )
  }
}

# Function to check if a class_id variable is present
data_classid_check <- function(data) {
  cid_check <- "class_id" %in% names(data)
  if (!cid_check) {
    stop(
      paste(
        "Data set", deparse(substitute(data)), "is missing a variable called class_id.",
        "Make sure spelling is correct."
      ),
      call. = FALSE
    )
  }
}

# Function to make sure items are in IPG data and spelled as needed. Lists those that are missing.
# This is a sequential wave, where first, we check grade-level and subject area as that determines
# what is needed for the indicators.
data_name_check_ipg <- function(data) {

  # First, checking presence of grade_level and form subject
  var_check <- c("form", "grade_level")[!c("form", "grade_level") %in% names(data)]
  if (!rlang::is_empty(var_check)) {
    stop(
      paste(
        "Data is missing the following variable(s):",
        paste0(var_check, collapse = ", "), "\n",
        "Make sure spelling is correct."
      ),
      call. = FALSE
    )
  }

  # Ensuring form subject and grade_level meet value requirements
  form_check <- all(
    na.omit(unique(data$form)) %in% c("Math", "Literacy", "Science", "Social Studies")
  )
  gl_check <- all(na.omit(unique(data$grade_level)) %in% -1:12)
  if (!form_check) {
    stop(
      paste(
        "form variable has values other than Math, Literacy, Science, or Social Studies",
        "\n",
        "Make sure subject options are spelled correctly with correct capitalization."
      ),
      call. = FALSE
    )
  }
  if (!gl_check) {
    stop(
      paste(
        "grade_level has values other than -1, 0, 1, 2, ...12.",
        "\n",
        "Make sure grade_level variable is an integer between -1 and 13."
      ),
      call. = FALSE
    )
  }

  # All gl-subject combinations need the following core action ratings
  core_actions <- c("ca1_a", "ca1_b", "ca1_c", "ca2_overall", "ca3_overall", "col")
  ca_check <- core_actions[!core_actions %in% names(data)]
  if (!rlang::is_empty(ca_check)) {
    stop(
      paste(
        "Data is missing the following variables:",
        paste0(ca_check, collapse = ", "), "\n",
        "Make sure they are spelled correctly."
      ),
      call. = FALSE
    )
  }

  # Science form also has CA 1 D, E and F, and needs a filter on observation type
  if (NROW(dplyr::filter(data, form == "Science")) > 0) {
    ca1_science <- c("ca1_d", "ca1_e", "ca1_f", "science_filter")
    ca1_science_check <- ca1_science[!ca1_science %in% names(data)]
    if (!rlang::is_empty(ca1_science_check)) {
      stop(
        paste(
          "Data is missing the following variables:",
          paste0(ca1_science_check, collapse = ", "), "\n",
          "Make sure they are spelled correctly."
        ),
        call. = FALSE
      )
    }
  }

  # K-5 Literacy form should also have RFS
  rfs_check <- "rfs_overall" %in% names(data)
  rfs_elig <- dplyr::filter(data, form == "Literacy")
  rfs_elig <- suppressWarnings(min(rfs_elig$grade_level, na.rm = T) <= 5)
  if (!rfs_check & rfs_elig) {
    stop(
      paste(
        "Data contains K-5 Literacy observation(s), but no rfs_overall variable.", "\n",
        "Make sure it is spelled correctly."
      ),
      call. = FALSE
    )
  }

}


# Function to ensure items are on proper scale. Lists items that are not.
data_scale_check <- function(data, needed_items, item_scale) {
  scale_check <- purrr::map_lgl(data[, needed_items], ~ all(na.omit(.) %in% item_scale))
  scale_check <- needed_items[!scale_check]
  if (!rlang::is_empty(scale_check)) {
    stop(
      paste(
        "In", deparse(substitute(data)), "the following variable(s) have a value out of scale:",
        "\n", paste0(scale_check, collapse = ", "), "\n",
        "They should only take values of",
        paste0(item_scale, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}


# Function to ensure IPG items are on proper scale. Lists items that are not.
data_scale_check_ipg <- function(data) {

  # All observations must have a value for form and grade level.
  if (any(is.na(data$grade_level))) {
    stop("All observations must have a value for grade-level. NAs are not allowed.")
  }

  if (any(is.na(data$form))) {
    stop("All observations must have a value for form. NAs are not allowed.")
  }

  # Core Action 1 indicators should all be 0 (no) and 1 (yes)
  ca_1 <- c("ca1_a", "ca1_b", "ca1_c")
  ca1_check <- purrr::map_lgl(data[, ca_1], ~ all(na.omit(.) %in% 0:1))
  ca1_check <- ca_1[!ca1_check]
  if (!rlang::is_empty(ca1_check)) {
    stop(
      paste(
        "The following variables have a value out of scale:", "\n",
        paste0(ca1_check, collapse = ", "), "\n",
        "All Core Action 1 indicators should be a 0 for 'No' or 1 for 'Yes'."
      ),
      call. = FALSE
    )
  }

  # Repeating with science Core Action 1 indicators and science text filter, if they exist
  if (NROW(dplyr::filter(data, form == "Science")) > 0) {
    ca_1_science <- c("ca1_d", "ca1_e", "ca1_f")
    ca1_science_check <- purrr::map_lgl(data[, ca_1_science], ~ all(na.omit(.) %in% 0:1))
    ca1_science_check <- ca_1_science[!ca1_science_check]
    if (!rlang::is_empty(ca1_science_check)) {
      stop(
        paste(
          "The following variables have a value out of scale:", "\n",
          paste0(ca1_science_check, collapse = ", "), "\n",
          "All Core Action 1 indicators should be a 0 for 'No' or 1 for 'Yes'."
        ),
        call. = FALSE
      )
    }

    science_filter_options <- c("Text", "Inquiry and Scientific Practice", "Both", "Neither")
    science_filter_check <- all(na.omit(data$science_filter) %in% science_filter_options)
    if (!science_filter_check) {
      stop(
        paste(
          "The variable science_filter has an unexpected value. Values must be one of the",
          "following options:", "\n",
          paste0(science_filter_options, collapse = "; ")
        )
      )
    }
  }

  # All other variables should be on a 1-4 scale
  other_vars <- c("ca2_overall", "ca3_overall", "col")
  other_vars_check <- purrr::map_lgl(data[, other_vars], ~ all(na.omit(.) %in% 1:4))
  other_vars_check <- other_vars[!other_vars_check]
  if (!rlang::is_empty(other_vars_check)) {
    stop(
      paste(
        "The following variables have a value out of scale:", "\n",
        paste0(other_vars_check, collapse = ", "), "\n",
        "Core Actions 2 and 3, and Culture of Learning should be a",
        "1 for 'Not Yet', 2 for 'Somewhat', 3 for 'Mostly', and 4 for 'Yes."
      ),
      call. = FALSE
    )
  }

  # RFS should be on a 1-4 scale (if it exists)
  if ("rfs_overall" %in% names(data)) {
    rfs_check <- all(na.omit(data[, "rfs_overall"]) %in% 1:4)
    if (!rfs_check) {
      stop(
        paste(
          "The following variable has a value out of scale:", "\n",
          "rfs_overall", "\n",
          "RFS Overall should be should be a",
          "1 for 'Not Yet', 2 for 'Somewhat', 3 for 'Mostly', and 4 for 'Yes."
        ),
        call. = FALSE
      )
    }
  }

}


# Function to see if all values on scale are used. For example, if scale is supposed to be 0-3 but
# only 1-3 is in data, this could be a sign the data is miscoded. Used to create a warning message
# rather than an error.
data_scaleuse_check <- function(data, needed_items, item_scale) {
  scale_use_check <- purrr::map(
    data[, needed_items],
    ~ sort(item_scale)[!sort(item_scale) %in% sort(unique(.))]
  )
  scale_use_check <- scale_use_check[purrr::map_lgl(scale_use_check, ~ !rlang::is_empty(.))]
  if (length(needed_items) == 1) names(scale_use_check) <- needed_items

  if (!rlang::is_empty(scale_use_check)) {
    warning(
      paste(
        "Not all the possible values for each variable were used in", deparse(substitute(data)),
        "The following variables did NOT use the following values:", "\n",
        paste(names(scale_use_check), paste(scale_use_check), sep = ": ", collapse = ";   "), "\n",
        "This is not an error, but you should confirm that all values are on the scale:",
        paste0(item_scale, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}


# Function to create construct given raw items, reverse coded items, and item scale. Takes the sum
# of needed items
construct_maker_sum <- function(data, needed_items, item_scale, reversed_items = NULL) {

  # Create construct
  positive_items <- needed_items[!needed_items %in% reversed_items]
  pos_sums <- rowSums(data[, positive_items, drop = FALSE])
  rev_sums <- 2 * median(item_scale) - data[, reversed_items, drop = FALSE]
  rev_sums <- rowSums(rev_sums)

  if (length(reversed_items) == 0) {
    construct <- pos_sums
  } else {
    construct <- pos_sums + rev_sums
  }

  # Print number of rows dropped because of missingness
  print(
    paste(
      NROW(data) - sum(!is.na(construct)),
      "Row(s) in",
      deparse(substitute(data)),
      "were NOT used because missing at least one value needed to create common measure."
    )
  )

  # Return data with construct
  cbind(data, construct)
}

# Function to create construct given raw items, reverse coded items, and item scale. Takes the mean
# of needed items
construct_maker_mean <- function(data, needed_items, item_scale, reversed_items = NULL) {

  # Create construct
  positive_items <- needed_items[!needed_items %in% reversed_items]
  pos_means <- rowMeans(data[, positive_items, drop = FALSE])
  rev_means <- rowMeans(data[, reversed_items, drop = FALSE])
  rev_means <- 2 * median(item_scale) - rev_means
  if (length(reversed_items) == 0) {
    construct <- pos_means
  } else {
    construct <-
      (pos_means * length(positive_items) + rev_means * length(reversed_items)) /
      length(needed_items)
  }

  # Print number of rows dropped because of missingness
  print(
    paste(
      NROW(data) - sum(!is.na(construct)),
      "Row(s) in",
      deparse(substitute(data)),
      "were NOT used because missing at least one value needed to create common measure."
    )
  )

  # Return data with construct
  cbind(data, construct)
}


# Function to calculate overall IPG score. This function assumes all observations that needed to be
# removed have already been removed. It also requires values for all core actions and prints a
# message about how many observations were dropped because of missingness.
construct_maker_ipg <- function(data) {

  # Create overall core action 1 score and adjust scale of other variables
  data <- dplyr::mutate(
    data,
    ca1_overall = ca1_a + ca1_b + ca1_c,
    ca2_overall = ca2_overall - 1,
    ca3_overall = ca3_overall - 1,
    col = col - 1
  )

  # Adjust Core Action 1 for science observations, if any
  if (NROW(dplyr::filter(data, form == "Science")) > 0) {
    data <- dplyr::mutate(
      data,
      science1_overall = as.double(ca1_a + ca1_b),
      science1_overall = dplyr::case_when(
        science_filter == "Text" & (ca1_c + ca1_d + ca1_e == 3) ~ science1_overall + 1,
        science_filter == "Text" & (ca1_c + ca1_d + ca1_e != 3) ~ science1_overall,
        science_filter == "Inquiry and Scientific Practice" ~ science1_overall + ca1_f,
        science_filter == "Both" & (ca1_c + ca1_d + ca1_e + ca1_f == 4) ~ science1_overall + 1,
        science_filter == "Both" & (ca1_c + ca1_d + ca1_e + ca1_f != 4) ~ science1_overall,
        science_filter == "Neither" ~ science1_overall
      ),
      ca1_overall = ifelse(form == "Science", science1_overall, ca1_overall),
      science1_overall = NULL
    )
  }

  # Adjust RFS if exists and create it as missing if not
  if ("rfs_overall" %in% names(data)) {
    data <- dplyr::mutate(data, rfs_overall = rfs_overall - 1)
    rfs_init <- T
  } else {
    data <- dplyr::mutate(data, rfs_overall = NA_integer_)
    rfs_init <- F
  }

  # Identify rows that need to be dropped because of missingness. Then print and drop
  data <- dplyr::mutate(
    data,
    miss_col = is.na(col),
    miss_ca = is.na(ca1_overall) | is.na(ca2_overall) | is.na(ca3_overall),
    miss_litk5 = form == "Literacy" & grade_level <= 5 & is.na(rfs_overall) & miss_ca,
    miss_ca_nolitk5 = dplyr::case_when(
      miss_litk5 ~ F,
      form == "Literacy" & grade_level <= 5 & !is.na(rfs_overall) & miss_ca ~ F,
      TRUE ~ miss_ca
    )
  )

  if (sum(data$miss_litk5, na.rm = T) > 0 & !is.infinite(sum(data$miss_litk5, na.rm = T))) {
    warning(
      paste(
        sum(data$miss_litk5, na.rm = T), "K-5 Literacy observation(s)",
        "were missing an overall RFS score and some Core Actions.",
        "These observations need an overall RFS score or all three Core",
        "Actions, or both in order to have an overall IPG score calculated."
      ),
      call. = F
    )
  }

  if (sum(data$miss_ca_nolitk5) > 0) {
    warning(
      paste(
        sum(data$miss_ca_nolitk5), "Observation(s) have a missing score",
        "on at least one of the Core Actions. Observations must be rated on all",
        "Core Actions in order to have an overall IPG score calculated.",
        "In many cases, missing scores should be set to the lowest possible value",
        "or, if it's ineligible, the entire observation should be removed before scoring.",
        "Check the scoring guide for more details."
      ),
      call. = F
    )
  }

  if (sum(data$miss_col) > 0) {
    warning(
      paste(
        sum(data$miss_col), "Observation(s) have a missing score on Culture of",
        "Learning. Observations must be rated on Culture of Learning in order",
        "to have an overall IPG score calculated."
      ),
      call. = F
    )
  }

  # Create overall observation scores and return core actions to original scales
  data <- dplyr::mutate(
    data,
    construct = dplyr::case_when(
      form == "Literacy" & grade_level <= 5 & miss_ca & !is.na(rfs_overall) ~
        0.75 * rfs_overall + 0.25 * col,
      form == "Literacy" & grade_level <= 5 & !miss_ca & !is.na(rfs_overall) ~
        0.25 * col + 0.75 * ((rfs_overall + ca1_overall + ca2_overall + ca3_overall) / 4),
      TRUE ~ (col + ca1_overall + ca2_overall + ca3_overall) / 4
    ),
    ca2_overall = ca2_overall + 1,
    ca3_overall = ca3_overall + 1,
    col = col + 1,
    miss_col = NULL,
    miss_ca = NULL,
    miss_litk5 = NULL,
    miss_ca_nolitk5 = NULL
  )

  # Adjust back RFS if exists and delete it as missing if not
  if (rfs_init) {
    data <- dplyr::mutate(data, rfs_overall = rfs_overall + 1)
  } else {
    data <- dplyr::select(-rfs_overall)
  }

  return(data)
}


# Function to create equity_group variable from function input and check it contains fewer than five
# choices.
equity_check <- function(data, equity_group) {
  names(data)[names(data) == equity_group] <- "equity_group"
  data <- dplyr::mutate(data, equity_group = as.character(equity_group))
  if (length(unique(na.omit(data$equity_group))) >= 5) {
    warning(
      paste(equity_group, "has more than 5 different groups. Are you sure that's what you want?"),
      call. = FALSE
    )
  }
  data
}
