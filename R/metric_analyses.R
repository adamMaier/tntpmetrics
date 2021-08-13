#' Compute single common metric mean, difference between groups, and/or change over time.
#'
#' \code{metric_mean} computes the mean common metric score at a single point in time.
#'   \code{metric_growth} computes the mean change in the common metric score between two points
#'   in time. Both functions can disaggregate  results based on a group characteristic used for
#'   equity comparisons. They can also account for metrics where multiple data points come from the
#'   same classroom, like those based on student surveys or assignments.
#'
#' @section Data and Variable Format:
#'   \code{metric_mean} and \code{metric_growth} should be used with the raw metric data.
#'   Each row of data should represent a single rated outcome. For example, each row of data will be
#'   a single completed survey, a single rated assignment, a single classroom observation, etc.
#'   The data should not have the metric already calculated but instead have the components needed
#'   to make this calculation. For example, data on student engagement should not have a column or
#'   variable titled engagement, but should have variables corresponding the four survey questions
#'   used to calculate engagement. Leave all items in their raw form - the functions automatically
#'   account for items that need to be reverse coded. The only requirement is that the data contains
#'   the needed variables and that the variables are numeric (i.e., data values should be 0s and 1s,
#'   not 'No' and 'Yes'. This ensures that the common metrics are calculated correctly and
#'   consistently across projects. Each metric has its own set of needed variables that must be
#'   spelled exactly as shown below. They are:
#'   \describe{
#'     \item{engagement:}{eng_like, eng_losttrack, eng_interest, eng_moreabout}
#'     \item{belonging:}{tch_problem, bel_ideas, bel_fitin, tch_interestedideas}
#'     \item{relevance:}{rel_asmuch, rel_future, rel_outside, rel_rightnow}
#'     \item{expectations_old:}{exp_allstudents, exp_toochallenging, exp_oneyear, exp_different, exp_overburden, exp_began}
#'     \item{expectations:}{exp_fairtomaster, exp_oneyearenough, exp_allstudents, exp_appropriate}
#'     \item{tntpcore:}{ec, ao, dl, cl}
#'     \item{ipg:}All observations must have: {form, grade_level, ca1_a, ca1_b, ca1_c, ca2_overall, ca3_overall, col}; K-5 Literacy observations must also have {rfs_overall}; Science observations must also have: {ca1_d, ca1_e, ca1_f, science_filter}
#'     \item{assignments:}{content, relevance, practice}
#'    }
#'    Note that these are the NAMES of the variables needed in your data. It can be okay if some of these
#'    variables have NA values for specific rows. For example, K-5 Literacy observations on the IPG require
#'    either all of the Core Actions (ca1_a, ca1_b, ca1_c, ca2_overall, ca3_overall) and/or rfs_overall. If
#'    an observation has all the core actions it still needs a variable called rfs_overall, but the value
#'    can just be NA. See the \code{vignette("analyzing_metrics")} for more details.
#'    Note on Expectations. The items used to measure expectations shifted from a collection of six,
#'    mostly reverse-coded worded items to four positively worded items. Both expectations metrics are available,
#'    with the current 4-item expectations metric known as "expectations" and the older 6-item expectations
#'    metric known as "expectations_old". See the \code{vignette("analyzing_metrics")} for more details.
#'
#' @param data Data from a single timepoint. Used in \code{metric_mean}.
#' @param data1 Data from the initial timepoint. Used in \code{metric_growth}.
#' @param data2 Data from the final timepoint. Used in \code{metric_growth}.
#' @param metric Quoted name of the common metric. Options are "engagement", "belonging",
#'   "relevance", "assignments", "tntpcore", or "ipg".
#' @param use_binary A logical (T/F) option to use the binary version of the metric. The default is
#'   FALSE so that the mean and growth calculations are based on the overall metric value. If you
#'   want these calculations done on the binary version (e.g., looking at the percent of teachers with
#'   'high expectations' rather than the average expectations score) then set this option to TRUE.
#'   Note that the metric tntpcore has no binary version.
#' @param equity_group Optional quoted name of the categorical column/variable in data that contains
#'   the equity group designation. For example, if data has an indicator variable called
#'   \code{class_frl} that is either "Under 50% students with FRL" or "50% or more students with
#'   FRL", then analyst could set \code{equity_group = "class_frl"} to get differences in metric
#'   between these two groups. Default is no equity comparison.
#' @param by_class A logical (T/F) option indicating if multiple rows of data come from the same
#'   class. When \code{by_class = T}, analysis will automatically account for different sample sizes
#'   between classes and adjust the standard errors to account for the lack of independence between
#'   data deriving from the same class. If set to \code{FALSE}, data must have a variable titled
#'   \code{class_id}. Default if \code{FALSE}.
#' @param scaleusewarning A logical (T/F) indicating whether function should generate a warning when
#'   not all values of a scale are used. For example, student survey data that only contains values
#'   of 1s and 2s could mean that data is on a 1-4 scale, when it should be on a 0-3 scale. When
#'   \code{scaleusewarning = T}, the function will warn you of this. This warning does not mean your
#'   data is wrong. For example, the Academic Ownership domain from TNTP CORE has 5 potential
#'   values: 1, 2, 3, 4, or 5. It's not uncommon to have data where teachers were never rated above
#'   a 4 on this domain. In this case, the printed warning can be ignored. Default if \code{TRUE}.
#'   If you are confident your data is on the right scale, you can suppress the warning by setting
#'   to \code{TRUE}.
#'
#' @return A list of results including the overall mean or mean by equity group (for
#'   \code{metric_mean}), the mean change over time or mean change for each group (for
#'   \code{metric_growth}). Means are accompanied by standard errors and 95% confidence
#'   intervals. Also included are list elements for number of data points used in analysis.
#'
#' @examples
#' # Compute the mean engagement score for an entire project at a single time point. Setting
#' # by_class = T because multiple surveys come from the same class.
#' metric_mean(practice_data, metric = "engagement", by_class = T)
#'
#' # Do the same, but now compare results by a class's FRL population
#' metric_mean(practice_data, metric = "engagement", equity_group = "frl_cat", by_class = T)
#'
#' # Look at change in engagement over time, then look at how differences in engagement between a
#' # class's FRL population change over time
#' metric_growth(
#'   practice_data_initial,
#'   practice_data_final,
#'   metric = "engagement",
#'   by_class = T
#'  )
#'  metric_growth(
#'   practice_data_initial,
#'   practice_data_final,
#'   metric = "engagement",
#'   equity_group = "class_frl_cat",
#'   by_class = T
#'  )
#'
#' @name metric_analyses
NULL

# Function for means at one time-point
#' @rdname metric_analyses
#' @export
metric_mean <- function(data, metric, use_binary = F, equity_group = NULL, by_class = F,
                        scaleusewarning = T) {

  # Check if data has class ID if specified
  if (by_class) data_classid_check(data)

  # Print warning message if using a metric that should have a class ID
  if (metric %in% c("engagement", "belonging", "relevance", "assignments") & !by_class) {
    warning(
      paste(
        "To properly analyze the", metric, "metric, you should have a variable called class_id in",
        "your data, and set by_class = T.",
        "If you did not collect a class ID your results might not be appropriate.",
        "Contact Cassie Coddington to discuss."
      ),
      call. = F
    )
  }

  data <- make_construct(data = data, metric = metric, scaleusewarning = scaleusewarning)

  # Use binary version of construct? If TRUE, then override output to be tested
  if (use_binary) {
    data <- dplyr::mutate(data, construct = construct_binary, construct_binary = NULL)
  }

  # Get means
  if (!is.null(equity_group)) {
    data <- equity_check(data, equity_group = equity_group)
    out <- cm_equity_mean(data, need_classid = by_class)
  } else {
    out <- cm_mean(data, need_classid = by_class)
  }
  out
}

# Function for change in scores over time
#' @rdname metric_analyses
#' @export
metric_growth <- function(data1, data2, metric, use_binary = T, equity_group = NULL, by_class = F,
                              scaleusewarning = T) {

  # Check if data has class ID if specified
  if (by_class) {
    data_classid_check(data1)
    data_classid_check(data2)
  }

  # Print warning message if using a metric that should have a class ID
  if (metric %in% c("engagement", "belonging", "relevance", "assignments") & !by_class) {
    warning(
      paste(
        "To properly analyze the", metric, "metric, you should have a variable called class_id in",
        "your data, and set by_class = T.",
        "If you did not collect a class ID your results might not be appropriate.",
        "Contact Cassie Coddington to discuss."
      ),
      call. = F
    )
  }

  data1 <- make_construct(data = data1, metric = metric, scaleusewarning = scaleusewarning)
  data2 <- make_construct(data = data2, metric = metric, scaleusewarning = scaleusewarning)

  # Use binary version of construct? If TRUE, then override output to be tested
  if (use_binary) {
    data1 <- dplyr::mutate(data1, construct = construct_binary, construct_binary = NULL)
    data2 <- dplyr::mutate(data2, construct = construct_binary, construct_binary = NULL)
  }

  # Get growth
  if (!is.null(equity_group)) {
    data1 <- equity_check(data1, equity_group = equity_group)
    data2 <- equity_check(data2, equity_group = equity_group)
    out <- cm_equity_growth(data1, data2, need_classid = by_class)
  } else {
    out <- cm_growth(data1, data2, need_classid = by_class)
  }
  out
}
