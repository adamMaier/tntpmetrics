
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

-   [**Overview of creating metrics**](metric_overview.html)
-   [**Student surveys:**](student_surveys.html) Engagement, Relevance,
    and Belonging
-   [**Observation tools:**](observation_tool.html) IPG and TNTP Core
-   [**Teacher and leader surveys:**](teacher_expectations.html)
    Expectations (both the current items and the older items)
-   [**Assignments:**](assignments.html) Grade-Appropriate Assignments
