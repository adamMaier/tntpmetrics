# These are helper functions to calculate common metric means and changes over time. They are used
# similarly across metric types.

# Function to estimate single time-point common metric mean. Assumes construct and class_id (if
# needed) is already made.
cm_mean <- function(data, need_classid) {

  # Calculate metric means. Use multi-level model to account for uneven class sizes or standard
  # OLS if classes aren't accounted for. Both models are unconditional.
  data <- dplyr::filter(data, !is.na(construct))
  if (need_classid) {
    data <- dplyr::mutate(data, class_id = as.character(class_id))
    model <- suppressMessages(
      lmerTest::lmer(construct ~ 1 + (1 | class_id), data = data)
    )
  } else {
    model <- stats::lm(construct ~ 1, data = data)
  }
  est_means <- emmeans::emmeans(model, ~ 1, mode = "satterthwaite")
  out_list <- list(
    "Overall mean" =
      est_means,
    "Number of data points" =
      length(summary(model)$residuals)
  )
  if (need_classid) out_list["Number of included classes"] <- as.integer(summary(model)$ngrps)
  out_list
}


# Function to estimate change in mean construct between two timepoints. Assumes construct and
# class_id (if needed) is already made.
cm_growth <- function(data_1, data_2, need_classid) {

  # Stack data into one data frame
  if (need_classid) {
    full_data <-
      dplyr::bind_rows(
        "Initial" = dplyr::select(data_1, construct, class_id),
        "Final" = dplyr::select(data_2, construct, class_id),
        .id = "time"
      )
  } else {
    full_data <-
      dplyr::bind_rows(
        "Initial" = dplyr::select(data_1, construct),
        "Final" = dplyr::select(data_2, construct),
        .id = "time"
      )
  }

  # Make time an ordered factor
  full_data <- dplyr::mutate(full_data, time = ordered(time, levels = c("Final", "Initial")))

  # Calculate metric means. Use multi-level model to account for uneven class sizes or standard
  # OLS if classes aren't accounted for. Both models are unconditional.
  full_data <- dplyr::filter(full_data, !is.na(construct))
  if (need_classid) {
    full_data <- dplyr::mutate(full_data, class_id = as.character(class_id))
    model <- suppressMessages(
      lmerTest::lmer(construct ~ 1 + time + (1 | class_id), data = full_data)
    )
  } else {
    model <- stats::lm(construct ~ 1 + time, data = full_data)
  }
  est_means <- emmeans::emmeans(model, pairwise ~ time, mode = "satterthwaite")
  out_list <- list(
    "Means at each timepoint" =
      est_means$emmeans,
    "Differences between timepoints" =
      est_means$contrasts,
    "Number of data points" =
      length(summary(model)$residuals)
  )
  if (need_classid) out_list["Number of included classes"] <- as.integer(summary(model)$ngrps)
  out_list
}


# Function to calculate differences by group (e.g., percent FRL students) at a single timepoint.
# Assumes construct and equity_group variables are already made.
cm_equity_mean <- function(data, need_classid) {

  # Calculate metric means. Use multi-level model to account for uneven class sizes or standard
  # OLS if classes aren't accounted for. Both models are unconditional.
  data <- dplyr::filter(data, !is.na(construct))
  if (need_classid) {
    data <- dplyr::filter(data, !is.na(equity_group))
    data <- dplyr::mutate(data, class_id = as.character(class_id))
    model <- suppressMessages(
      lmerTest::lmer(construct ~ 1 + equity_group + (1 | class_id), data = data)
    )
  } else {
    model <- stats::lm(construct ~ 1 +  equity_group, data = data)
  }
  est_means <- emmeans::emmeans(model, pairwise ~ equity_group, mode = "satterthwaite")
  out_list <- list(
    "Group means" =
      est_means$emmeans,
    "Difference(s) between groups" =
      est_means$contrasts,
    "Number of data points" =
      length(summary(model)$residuals)
  )
  if (need_classid) out_list["Number of included classes"] <- as.integer(summary(model)$ngrps)
  out_list
}


# Function to calculate diff-in-diff between equity groups (e.g. percent FRL students) over time.
# Assumes construct and equity_group variables are already made.
cm_equity_growth <- function(data_1, data_2, need_classid) {

  # Make sure equity_group values are the same in both data sets
  if (
    !all(sort(unique(stats::na.omit(data_1$equity_group))) == sort(unique(stats::na.omit(data_2$equity_group))))
  ) {
    stop("Some values of equity group are not present in BOTH data sets.", call. = F)
  }

  # Stack data into one data frame
  if (need_classid) {
    full_data <-
      dplyr::bind_rows(
        "Initial" = dplyr::select(data_1, construct, equity_group, class_id),
        "Final" = dplyr::select(data_2, construct, equity_group, class_id),
        .id = "time"
      )
  } else {
    full_data <-
      dplyr::bind_rows(
        "Initial" = dplyr::select(data_1, construct, equity_group),
        "Final" = dplyr::select(data_2, construct, equity_group),
        .id = "time"
      )
  }

  # Make time an ordered factor
  full_data <- dplyr::mutate(full_data, time = ordered(time, levels = c("Initial", "Final")))

  # Calculate metric means. Use multi-level model to account for uneven class sizes or standard
  # OLS if classes aren't accounted for. Both models are unconditional.
  full_data <- dplyr::filter(full_data, !is.na(construct))
  if (need_classid) {
    full_data <- dplyr::filter(full_data, !is.na(equity_group))
    full_data <- dplyr::mutate(full_data, class_id = as.character(class_id))
    model <- suppressMessages(
      lmerTest::lmer(construct ~ 1 + time * equity_group + (1 | class_id), data = full_data)
    )
  } else {
    model <- stats::lm(construct ~ 1 + time * equity_group, data = full_data)
  }
  est_means <- emmeans::emmeans(model, pairwise ~ equity_group | time, mode = "satterthwaite")
  out_list <- list(
    "Group means at each timepoint" =
      est_means$emmeans,
    "Differences between groups at each timepoint" =
      est_means$contrasts,
    "Change in differences between groups over time" =
      emmeans::contrast(est_means$contrasts, "consec", simple = "each")$"simple contrasts for time",
    "Number of data points" =
      length(summary(model)$residuals)
  )
  if (need_classid) out_list["Number of included classes"] <- as.integer(summary(model)$ngrps)
  out_list
}
