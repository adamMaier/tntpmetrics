#' Calculate raw common metric values
#'
#' \code{make_metric} takes raw data containing all of the indicators, survey items, etc. needed
#'   to calculate a score on a common metric. It creates a new column/variable in your data with the
#'   value of the scored metric.
#'
#' @section Data and Variable Format:
#'   \code{make_metric} should be used with the raw metric data.
#'   Each row of data should represent a single rated outcome. For example, each row of data will be
#'   a single completed survey, a single rated assignment, a single classroom observation, etc.
#'   The data must have the components needed to score the construct. For example, data on student
#'   engagement should have variables corresponding to the four survey questions used to calculate
#'   engagement. Leave all items in their raw form - the functions automatically account for items
#'   that need to be reverse coded (if any). The only requirement is that the data contains the
#'   needed variables and that the variables are numeric (i.e., data values should be 0s and 1s,
#'   not 'No' and 'Yes'. This ensures that the common metrics are calculated correctly and
#'   consistently across projects. Each metric has its own set of needed variables that must be
#'   spelled exactly as shown below. They are:
#'   \describe{
#'     \item{engagement:}{eng_like, eng_losttrack, eng_interest, eng_moreabout}
#'     \item{belonging:}{tch_problem, bel_ideas, bel_fitin, tch_interestedideas}
#'     \item{relevance:}{rel_asmuch, rel_future, rel_outside, rel_rightnow}
#'     \item{expectations:}{exp_fairtomaster, exp_oneyearenough, exp_allstudents, exp_appropriate}
#'     \item{expectations_old:}{exp_allstudents, exp_toochallenging, exp_oneyear, exp_different, exp_overburden, exp_began}
#'     \item{tntpcore:}{ec, ao, dl, cl}
#'     \item{ipg:}{form, grade_level, ca1_a, ca1_b, ca1_c, ca2_overall, ca3_overall, col.
#'     K-5 Literacy observations must also have rfs_overall.
#'     Science observations must also have ca1_d, ca1_e, ca1_f, and science_filter}
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
#' @param data Data from a single timepoint. Used in \code{tntpmetric_mean}.
#' @param metric Quoted name of the common metric. Options are "engagement", "belonging",
#'   "relevance", "assignments", "expectations", "expectations_old", tntpcore", or "ipg".
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
#' @return A data.frame identical to the original except with new columns/variables named as the
#'   input to metric with a cm_ prefix (e.g., \code{cm_engagement}, \code{cm_ipg}, etc.) that has
#'   the value of the scored metric. For metrics that have a specific cut-point above which scores
#'   designate something meaningful (e.g., expectations score of at least 12 represent "high expectations")
#'   another new variable is also created with a cm_binary_ prefix (e.g., \code{cm_binary_expectations}).
#'   This variable is logical (TRUE/FALSE) with values of true implying the row has, for example, high
#'   expectations. Currently, attributes of original data.frame (like groups) are not preserved.
#'
#' @examples
#' # Compute the engagement score for each collected survey
#' test_data <- make_metric(ss_data_initial, metric = "engagement")
#' head(test_data)
#'
#' @name make_metric
NULL

# Non-exported helper function to prepare needed items and scales for each metric.
cm_iteminfo <- function(metric) {

  # Enter needed indicator variable names and scales for each metric
  if (metric == "engagement") {
    ni <- c("eng_like", "eng_losttrack", "eng_interest", "eng_moreabout")
    ri <- NULL
    sc <- 0:3
    cp <- 8
  }

  if (metric == "belonging") {
    ni <- c("tch_problem", "bel_ideas", "bel_fitin", "tch_interestedideas")
    ri <- NULL
    sc <- 0:3
    cp <- 8
  }

  if (metric == "relevance") {
    ni <- c("rel_asmuch", "rel_future", "rel_outside", "rel_rightnow")
    ri <- NULL
    sc <- 0:3
    cp <- 8
  }

  if (metric == "expectations_old") {
    ni <- c("exp_allstudents", "exp_toochallenging", "exp_oneyear", "exp_different", "exp_overburden", "exp_began")
    ri <- c("exp_toochallenging", "exp_different", "exp_overburden", "exp_began")
    sc <- 0:5
    cp <- 18
  }

  if (metric == "expectations") {
    ni <- c("exp_fairtomaster", "exp_oneyearenough", "exp_allstudents", "exp_appropriate")
    ri <- NULL
    sc <- 0:5
    cp <- 12
  }

  if (metric == "assignments") {
    ni <- c("content", "practice", "relevance")
    ri <- NULL
    sc <- 0:2
    cp <- 4
  }

  if (metric == "tntpcore") {
    ni <- c("ec", "ao", "dl", "cl")
    ri <- NULL
    sc <- 1:5
    cp <- NULL
  }

  list("ni" = ni, "ri" = ri, "sc" = sc, "cp" = cp)
}

# Unexported function to make construct called "construct". Exported version below names it the
# metric. For some metrics, there are two constructs made, the linear scale version and the binary
# version, e.g. "high expectations".
make_construct <- function(data, metric, scaleusewarning = T) {

  # Strip data of attributes
  data <- as.data.frame(data)

  if (metric != "ipg") {
    iteminfo <- cm_iteminfo(metric)
    ni <- iteminfo[["ni"]]
    ri <- iteminfo[["ri"]]
    sc <- iteminfo[["sc"]]
    cp <- iteminfo[["cp"]]
  }

  if (metric %in% c("engagement", "belonging", "relevance", "expectations", "expectations_old", "assignments")) {
    data_name_check(data, needed_items = ni)
    data_scale_check(data, needed_items = ni, item_scale = sc)
    if (scaleusewarning) data_scaleuse_check(data, needed_items = ni, item_scale = sc)
    data <- construct_maker_sum(data, needed_items = ni, item_scale = sc, reversed_items = ri)
    data <- dplyr::mutate(data, construct_binary = construct >= cp)
  }

  if (metric == "tntpcore") {
    data_name_check(data, needed_items = ni)
    data_scale_check(data, needed_items = ni, item_scale = sc)
    if (scaleusewarning) data_scaleuse_check(data = data, needed_items = ni, item_scale = sc)
    data <- construct_maker_mean(data, needed_items = ni, item_scale = sc, reversed_items = ri)
  }

  if (metric == "ipg") {
    data_name_check_ipg(data)
    data_scale_check_ipg(data)
    if (scaleusewarning) {
      data_scaleuse_check(
        data,
        needed_items = c("ca1_a", "ca1_b", "ca1_c"),
        item_scale = 0:1
      )
      data_scaleuse_check(
        data,
        needed_items = c("ca2_overall", "ca3_overall", "col"),
        item_scale = 1:4
      )
    }
    data <- construct_maker_ipg(data)
    data <- dplyr::mutate(data, construct_binary = construct >= 2)
  }

  data
}

# Exported function to make common metric with similar variable name.
#' @rdname make_metric
#' @export
make_metric <- function(data, metric, scaleusewarning = T) {
  out <- make_construct(data, metric, scaleusewarning)
  names(out)[names(out) == "construct"] <- paste0("cm_", metric)
  names(out)[names(out) == "construct_binary"] <- paste0("cm_binary_", metric)
  out
}
