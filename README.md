# matools

This package provides a set of tools to transform (.asc) files exported from Mini Analysis (Synaptosoft) into R data objects; calculate and plot common metrics used in neuroscience.

Mini Analysis by Synaptosoft is a widely used program among neuroscientist to automatically detect (and manually select) synaptic events from electrophysiological recordings. While the software still executes on the latest windows OS, Windows 10, it is no longer actively developed and several features do not work. Users are still able to export the core file (\*.asc) and perform manual analysis. This R package, matools, is designed to automate analysis and provide functional figures.

This package was created to solve the following problems:

1. Import synaptosoft's files (\*.asc) into data.frame/tibbles for wide-spread analysis within R. Additionally, you can directly convert .asc to .csv file with proper column names.

2. Extract and transform the raw values into common metrics within the neuroscience community including:

   - Inter-event Interval (ms)
   - Paired-Pulse Ratio (PPR)
   - Jitter (ms)

3. Generate a standard set of graphs for quick analysis, including:

   - Amplitude heatmaps & scatterplots
   - User-defined metric (e.g. amplitude, failure-rate, ect...) group average
   - Spike rasters

4. This analysis package is also expandable to automatically check and run such analysis when a new data file or meta file entry is added to a public Google Sheets file via the R Package `googlesheets`. See [github.com/trfore/chat-project] for documentation and a working example; or the help page for `import_experiment_parameters()`.

5. Additionally, the graphs (.pdf) used in the publication Fore et al. 2020 (DOI: ) can be generated and modified to fit your own analysis. See [github.com/trfore/chat-project] for documentation on generating and modifying such figures.

## Installation

The latest development version can be installed from github:

```r
# install.packages("devtools")
devtools::install_github("trfore/matools")
```

## matools Example

- Basic example that will import a demo .asc file; add sweep and stimulus numbers; and plot the amplitude of the first event after a stimulus.

```r
library(matools)

test_file_path <- system.file("extdata/demo_data",
  "cell2.ASC",
  package = "matools",
  mustWork = TRUE
)
test_param_path <- system.file("extdata/demo_data",
  "demo_data_project_parameters.csv",
  package = "matools",
  mustWork = TRUE
)

# load and modify the data
test_df <- asc_to_tibble(file_path = test_file_path, merge_iei = FALSE)

# create env to store data parameters
set_pkg_environment(force_new = TRUE)
import_experiment_parameters(file = test_param_path)
set_experiment_parameters(matools_env$parameters, filename = "cell2")

test_df <- add_sweep_number_to_rows(
  df = test_df,
  sweep_duration = matools_env$sweep_duration_sec,
  sweep_count = matools_env$sweep_total,
  time_ref = "rec_time_ms"
)

test_df <- add_stimulus_index(
  df = test_df,
  time_of_stim = c(478.3, 518.3),
  isi = 40,
  sweep_count = 281,
  add_missing = TRUE
)

# create drug parameter matrix
experiment_parameters <- dplyr::tibble(
  condition_start = matools_env$condition_starts,
  condition_end = matools_env$condition_ends,
  condition = matools_env$condition_names
)

test_df <- add_condition_tag(
  df = test_df,
  parameters = experiment_parameters
)

View(test_df)

# simple plot
plot_data <- test_df %>% dplyr::filter(stimulus == 1)
plot_scatterplot_amplitude(df = plot_data, sweep_duration = 5, ymax = 250)
```

- More complex data processing workflows are provided in `R/workflows.R` and are viewable within R by calling `getAnywhere()` on `analysis_evoked_ap()` or `analysis_evoked_psp()`.
- Additionally, see [github.com/trfore/chat-project] for full analysis workflows.

[github.com/trfore/chat-project]: https://www.github.com/trfore/chat-project
