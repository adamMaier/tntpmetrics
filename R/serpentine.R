#' Perform serpentine sorts on multiple variables.
#'
#' \code{serpentine} sorts data in a serpentine fashion (alternating between ascending and
#'   descending orders) for all variables specified. \code{mixed_serpentine} sorts the data with
#'   ascending or descending sorts for every variable specified except the last, which is serpentine
#'   sorted.
#'
#' This is helpful in complex sampling designs with implicit stratification, as it reduces the
#'   variation in the stratified outcome for adjacent sampled units and thus reduces the overall
#'   sampling error. Serpentine sorts are commonly used in NCES surveys.
#'
#' @param data is the data.frame to be sorted
#' @param ... are the variables to serpentine sort, in the given order. In
#'   \code{serpentine}, the first variable listed will be sorted in ascending order, the second
#'   variable will alternate between ascending and descending order by the value of the first
#'   variable, and so on. In \code{mixed_serpentine}, it is assumed all variables listed should be
#'   sorted in ascending order except the last, which is serpentine sorted. The user can choose a
#'   descending sort for any variable except the last by using the \code{desc()} wrapper.
#' @param random_num is a random number to break ties randomly. This is most helpful when all
#'   variables on which the data is sorted are categorical as it is more liekly there are several
#'   rows of data with identical values on each category. Default is 1 so that results are
#'   reproducable.
#'
#' @return A data.frame with equal size as the original data, but sorted differently.
#'
#' @examples
#' # All variables except first are serpentine sorted
#' serpentine(data = mtcars, cyl, mpg)
#' serpentine(data = mtcars, cyl, vs, mpg)
#'
#' # Same sort variables, but different resulting order because changing random number
#' serpentine(data = mtcars, vs, am)
#' serpentine(data = mtcars, vs, am, random_num = 5)
#'
#' # Changing the random number has minimal effect when a non-cateogrical variable is included
#' serpentine(data = mtcars, cyl, vs, mpg)
#' serpentine(data = mtcars, cyl, vs, mpg, random_num = 23)
#'
#' # cyl, and vs are ascending sorted while mpg is serpentine sorted
#' mixed_serpentine(mtcars, cyl, vs, mpg)
#'
#' # cyl is ascending, vs is descending, and mpg is serpentine sorted
#' mixed_serpentine(mtcars, cyl, dplyr::desc(vs), mpg)
#'
#' @import rlang
#' @name serpentine
NULL


# HELPER FUNCTIONS ---------------------------------------------------------------------------------

# Single Seprentine Sort
# Function to take a data frame and two variables and return a serpentine sort on the second var
# with a descending sort on the first var.
# Note that to use this function outsede of exported functions below, you must put var_desc and
# var_serp in quosures in the function call, i.e., var_desc = rlang::quo(am)
single_serp <- function(data = NULL, var_desc = NULL, var_serp = NULL) {

  # Split data into lists by value of var_desc
  split_data <- split(data, dplyr::pull(data, !!var_desc))

  # Apply ascending sort to odd list elements
  split_data[c(T, F)] <- purrr::map(split_data[c(T, F)], ~ dplyr::arrange(.x, !!var_serp))

  # Apply descending sort to even list elements
  split_data[c(F, T)] <- purrr::map(split_data[c(F, T)], ~ dplyr::arrange(.x, dplyr::desc(!!var_serp)))

  # Recombine in data by appending list elements
  dplyr::bind_rows(split_data)

}


# EXPORTED FUNCTIONS -------------------------------------------------------------------------------

#' @rdname serpentine
#' @export
serpentine <- function(data = NULL, ..., random_num = 1) {

  # Extract key parts of provided function arguments
  orig_names <- names(data)
  full_vars <- rlang::quos(..., random_num)

  # Add random number to data to break ties
  set.seed(random_num)
  out <- dplyr::mutate(data, random_num = runif(NROW(data)))

  # If only one variable specified, returning a simple sort.
  if (length(full_vars) < 2) {
    out <- dplyr::arrange(out, ...)
  } else {

    # Run serpentine sort on first two variables
    out <- single_serp(out, var_desc = full_vars[[1]], var_serp = full_vars[[2]])
    out <- dplyr::mutate(out, row_n = dplyr::row_number())
    out <- dplyr::group_by(out, !!!full_vars[1:2])
    out <- dplyr::mutate(out, new_group_num = min(row_n))
    out <- dplyr::ungroup(out)

    # Loop throuh next j values
    if (length(full_vars) >= 3) {
      for (j in 3:length(full_vars)) {
        out <- single_serp(out, var_desc = rlang::quo(new_group_num), var_serp = full_vars[[j]])
        out <- dplyr::mutate(out, row_n = dplyr::row_number())
        out <- dplyr::group_by(out, !!!full_vars[1:j])
        out <- dplyr::mutate(out, new_group_num = min(row_n))
        out <- dplyr::ungroup(out)
      }
    }
  }

  # Clean data and export
  out <- dplyr::select(out, dplyr::one_of(orig_names))
  out
}

#' @rdname serpentine
#' @export
mixed_serpentine <- function(data = NULL, ...) {

  # Extract key parts of provided function arguments
  orig_names <- names(data)
  full_vars <- rlang::quos(...)
  ascdesc_vars <- full_vars[1:(length(full_vars) - 1)]
  serp_var <- full_vars[[length(full_vars)]]

  # If only one variable specified, returning a simple sort.
  if (length(full_vars) < 2) {
    out <- dplyr::arrange(data, ...)
  } else {

    # Creating error if desc() used on last variable, which must be serpentined
    if (grepl("^desc\\(", rlang::quo_name(serp_var))) {
      stop(
        paste(
          "Cannot apply a descending sort to last variable specified.",
          "It will automatically be serpentine sorted. Remove the desc() wrapper."
        ),
        call. = F
      )
    }

    # Perform sorts
    out <- dplyr::arrange(data, !!!ascdesc_vars)
    out <- dplyr::mutate(out, row_n = dplyr::row_number())
    out <- dplyr::group_by(out, !!!ascdesc_vars)
    out <- dplyr::mutate(out, min_row = min(row_n))
    out <- dplyr::ungroup(out)
    out <- serpentine(out, min_row, !!serp_var)

    # Clean data and export
    out <- dplyr::select(out, dplyr::one_of(orig_names))
  }
  out
}
