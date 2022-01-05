
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tntpmetrics

<!-- badges: start -->
<!-- badges: end -->

[TNTP](https://tntp.org/) uses common metrics to learn from the work
project teams are doing. By using similar metrics across different
projects, TNTP teams are better able to track their progress reliably
and coordinate work across contexts. Common metrics also serve as the
core of organization-wide goals. Though nearly all projects are using
common metrics somewhere in their work, collecting common data does not
guarantee each project will score or aggregate the metrics similarly.
And despite scoring guidance, using valuable analyst time to walk
through the steps to calculate metrics, or score teams’ goals is not
ideal.

The `tntpmetrics` package includes three handy functions that start with
raw, project-collected data and calculate individual common metric
scores, summarize average common metrics scores for an entire project,
compare common metric scores between (typically student sub-) groups,
and analyze changes in metric scores over time. Most of the work of
these functions is checking for potential errors or inconsistencies in
the data – such as data not being on the proper scale, or missing but
needed variables. These functions attempt to anticipate all of the
potential issues that could occur between collecting raw data and
calculating simple means from it.

### Installation

You can install `tntpmetrics` from GitHub:

``` r
devtools::install_github("adamMaier/tntpmetrics")
```

### Using `tntpmetrics`

To learn more about how the package works, please read through the
following articles:

-   Overview of creating metrics with `make_metric`
-   **Student surveys:** Engagement, Relevance, and Belonging
-   **Observation tools:** IPG and TNTP Core
-   **Teacher and leader surveys:** Expectations (both the current items
    and the older items)
-   **Assignments:** Grade-Appropriate Assignments

## Quick example

`tntpmetrics` contains a simple function called `make_metric` to attach
a new column/variable to your data with the value of the scored common
metric. The new column will always have the prefix `cm_` followed by the
name of the metric. For example, the *engagement* metric is simply the
sum of the four engagement survey items. To use `make_metric`, simply
provide the data set you want to use, and the metric you want
calculated, making sure to put the latter in quotes. The result is your
data but with new variable `cm_engagement`. The function also tells you
how many rows of data did not have a construct created because at least
one of the survey items was missing.

``` r
make_metric(data = ss_data_initial, metric = "engagement") %>%
  select(response_id, starts_with("eng_"), starts_with("cm_")) %>%
  head() %>%
  kable()
#> [1] "6 Row(s) in data were NOT used because missing at least one value needed to create common measure."
```

| response\_id | eng\_interest | eng\_like | eng\_losttrack | eng\_moreabout | cm\_engagement | cm\_binary\_engagement |
|-------------:|--------------:|----------:|---------------:|---------------:|---------------:|:-----------------------|
|            3 |             2 |         2 |              2 |              2 |              8 | TRUE                   |
|           39 |             2 |         3 |              2 |              2 |              9 | TRUE                   |
|           73 |             2 |         2 |              2 |              2 |              8 | TRUE                   |
|           84 |             2 |         2 |              2 |              2 |              8 | TRUE                   |
|           85 |             2 |         3 |              2 |              2 |              9 | TRUE                   |
|           94 |             2 |         2 |              2 |              2 |              8 | TRUE                   |

### Binary version of common metric

Note that in the above, there were **two** new variables created:
`cm_engagement` and `cm_binary_engagement`. For many common metrics,
there is a cut-point on the metric scale above which, scores take on a
special meaning. For engagement, for example, scores of 8 or above imply
that the student in that particular response was “engaged”. The variable
`cm_binary_engagement` will be `TRUE` if the engagement score is above
this cut-point and FALSE if not. For most common metrics, the guidance
is to set goals around the actual metric score, not the binary version,
as the binary version reduces the nuance of the data. However, we know
teams are interested in these binary classifications to `cm_binary_`
variables are always created when you run `make_metric` as long as the
metric has a defined cut-point. (The metric tntpcore does not have a
defined cut-point.)

## Checking for common errors

`make_metric` automatically checks for the most common data issues.

### Misspelled Variables

First, it requires that the data have the variable names spelled exactly
as above. There is nothing special about these variable names, and the
function had to choose some as the default. If your data has the
variable names spelled differently, then you’ll have to change them
before using `make_metric`. Otherwise, you’ll get an error:

``` r
ss_data_initial %>%
  rename(eng_interest_wrongspelling = eng_interest) %>%
  make_metric(metric = "engagement")
#> Error: Data set data is missing the following variable(s): eng_interest 
#>  Make sure spelling is correct.
```

Which variable names are needed for each metric can always be found by
typing `? make_metric`; they are also detailed in metric vignettes.

#### Items on the wrong scale

Second, `make_metric` will check each item to ensure it’s on the
expected scale. For student survey items, it expects the scale of 0-3
outlined above. If any data value is outside of this scale, you’ll get
an error telling you which variables are out of scale and the proper
scale on which they should be:

``` r
ss_data_initial %>%
  mutate(
    eng_interest = eng_interest + 1,
    eng_like = eng_like - 1
  ) %>%
  make_metric(metric = "engagement")
#> Error: In data the following variable(s) have a value out of scale: 
#>  eng_like, eng_interest 
#>  They should only take values of 0, 1, 2, 3
```

You will also get an error if your scales are not numeric:

``` r
ss_data_initial %>%
  mutate(
    eng_interest = case_when(
        eng_like == 0 ~ "Not True",
        eng_like == 1 ~ "A Little True",
        eng_like == 2 ~ "Mostly True",
        eng_like == 3 ~ "Very True"
    )
  ) %>%
  make_metric(metric = "engagement")
#> Error: In data the following variable(s) have a value out of scale: 
#>  eng_interest 
#>  They should only take values of 0, 1, 2, 3
```

The scales needed for each metric are detailed in vignettes.
