#' Sample data with implicit stratification
#'
#' \code{sample_implicit} draws a random sample of n units from a data.frame in a way that maximizes
#'   variation on variables of interest. For example, it can randomly sample schools in a way that
#'   ensures the sampled schools have as much variation as possible on key characteristics, like the
#'   percent of students of color or average achievement. Implicit stratification is a common method
#'   to sample units in an educational setting: the NCES frequently uses this approach when
#'   deciding who to survey or test, including for
#'   \href{https://nces.ed.gov/nationsreportcard/assessment_process/samplesfaq.aspx}{NCES}.
#'
#' \code{sample_implicit} implicitly samples units by first sorting the data on the key variables
#'   indicated. It uses a serpentine sort, which alternates between ascending and descending orders
#'   so that any two adjacent rows in the sorted data are as similar as possible. See
#'   \code{\link{serpentine}} for more details about serpentine sorting and
#'   \code{vignette("sample_implicit")} for a longer discussion of why it's useful. Serpentine
#'   sorting is commonly used by NCES to achieve implicit stratification.
#'
#' @param data is the data.frame on which rows will be sampled
#' @param n is the number of rows to be sampled
#' @param ... are the variables on which to implicitly stratify. In effect, these are the variables
#'   on which the data is first sorted. The order in which the variables are listed matters: the
#'   first variable listed will have the most variability in the sampled data, so you should list
#'   the variables on which you want to stratify in order of decreasing importance, as the variables
#'   listed near the end won't have as large of an effect on the stratification.
#' @param size_var is a variable indicating the size of the row. This allows you to select a sample
#'   that accounts for differences in the size of each unit. For example, if each row represents
#'   a school, an appropriate size_var could be the number of students attending the school so that
#'   schools serving more students are more likley to be selected. This is important when you are
#'   doing multiple stages of sampling, like first sampling schools and then sampling classrooms
#'   within schools. Without setting the size_va in this example, each shcool would be equally
#'   likely selected, meaning classrooms in small schools would be more likely to be selected
#'   because their small school with only a few classrooms has the same chance as being selected as
#'   a large school with many classrooms. Default is NULL.
#' @param random_num is a random number to control the random sampling process so that results are
#'   reproducible. Default is 1.
#'
#' @return A data.frame with equal size as the original data, but sorted differently and with a new
#'   variabled called \code{in_sample} that is TRUE if the row was selected for the same or FALSE
#'   otherwise.
#'
#' @examples
#' # Sample 7 cars after implicitly stratifying on gear and mpg.
#' sampled_cars <- sample_implicit(data = mtcars, n = 7, am, mpg)
#' sampled_cars
#'
#' # Once the sample is complete, it's easy to compare sampled to non-sampled cars
#' library(dplyr)
#' sampled_cars %>%
#'   group_by(in_sample) %>%
#'   summarize(mean_mpg = mean(mpg))
#'
#' # Using implicit stratification gets us more variation on variables of interest than just randomly
#' # selecting rows. For example, if we chose 3 cars, we might not get variability on the variables
#' # of interest. In this case, sample_implicit got us more variablity on mpg than a simple random
#' # sample
#' set.seed(12)
#' simplesample <- sample_n(mtcars, 3)
#' implicitsample <- sample_implicit(data = mtcars, n = 3, am, mpg)
#' count(simplesample, am)
#' implicitsample %>%
#'   filter(in_sample) %>%
#'   count(am)
#'
#' # You'll get different, but reproducible results if you change the random number
#' sampled_cars1 <- sample_implicit(data = mtcars, n = 5, am, mpg)
#' sampled_cars2 <- sample_implicit(data = mtcars, n = 5, am, mpg, random_num = 2)
#' sampled_cars1
#' sampled_cars2
#'
#' # If you have a variable that represents size, it's easy to account for that when selecting
#' # the sample
#' sample_implicit(data = mtcars, n = 5, am, mpg, size_var = hp)
#'
#' @import rlang
#' @name sample_implicit
NULL


#' @rdname sample_implicit
#' @export
sample_implicit <- function(data, n, ..., size_var = NULL, random_num = 1) {

  # Remove attributes of data
  out <- as.data.frame(data)

  # Apply a serpentine sort
  out <- serpentine(data, ..., random_num = random_num)

  # Set cumulative size vector and skip value
  if (is.null(substitute(size_var))) {
    cs <- 1:NROW(out)
    skip <- NROW(out) / n
  } else {
    size_var <- enquo(size_var)
    cs <- cumsum(sqrt(dplyr::pull(out, !!size_var)))
    skip <- sum(sqrt(dplyr::pull(out, !!size_var))) / n
  }

  # Pick a random value between 0 and skip, then iterate over skip and take the first row that is
  # less then each chosen value.
  set.seed(random_num)
  start <- stats::runif(1, min = 0, max = skip)
  sampled_rows <- seq(from = start, by = skip, length.out = n)
  sampled_rows <- purrr::map_int(sampled_rows, ~ min(which(.x <= cs)))

  # Return data, but with new in_sample variable
  dplyr::mutate(out, in_sample = dplyr::row_number() %in% sampled_rows)
}
