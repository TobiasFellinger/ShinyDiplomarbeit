library("SimNPH")
library("tidyverse")

# TODO: 
# * calculate rejection from multiple columns
# * rename columns
# * delete all unused columns
# 
# * maybe: create own datasets for scenario plots with only one line per relevant scenario selected

# read data ---------------------------------------------------------------

design_vars_all <- c("random_withdrawal", "n_pat_design", "recruitment")
design_vars_delayed     <- c("delay", "hr_after_onset", "median_survival_ctrl", design_vars_all)
design_vars_crossing    <- c("crossing", "hr_before", "hr_after", "median_survival_ctrl", design_vars_all)
design_vars_progression <- c("hr_death_before_prog", "hr_after_prog_ctrl", "median_time_to_prog_ctrl", "hr_prog", "median_survival_before_prog", design_vars_all)
design_vars_subgroup    <- c("hr_trt", "hr_subgroup_display", "prevalence", "median_survival_ctrl", design_vars_all)

data_dir <- "../Diplomarbeit/Simulations/data/"

delayed <- readRDS(paste0(data_dir, "simulation_delayed_effect_WTGP024_2023-10-24_160438/results.Rds"))

crossing <- readRDS(paste0(data_dir, "simulation_crossing_harzards_WTGP024_2023-11-06_101033/results.Rds"))

subgroup <- readRDS(paste0(data_dir, "simulation_subgroup_WTGP024_2023-12-05_095206/results.Rds"))

progression <- readRDS(paste0(data_dir, "simulation_disease_progression_WTGP024_2023-11-16_094021/results.Rds"))

# columns to tansform from days to months ---------------------------------

stats_scale <- c("mean_est", "median_est", "sd_est", "bias", "sd_bias", "mse", "sd_mse", "mae", "sd_mae", "width", "sd_width", "mean_sd", "sd_sd")
methods_scale <- c("diff_med_weibull", "median_surv", "rmst_diff_12m", "rmst_diff_6m")

time_varnames <- c(
  "delay", "crossing", "recruitment", "median_survival_trt", 
  "median_survival_ctrl", "rmst_trt_6m", "rmst_ctrl_6m", "rmst_trt_12m", 
  "rmst_ctrl_12m", "descriptive.max_followup", "descriptive.study_time", 
  "descriptive.sd_max_followup", "descriptive.sd_study_time", 
  as.character(outer(methods_scale, stats_scale, \(x,y){str_c(x, ".", y)}))
  )

# custom transformations --------------------------------------------------

progression <- progression |>
  mutate(
    median_survival_before_prog = SimNPH::r2m(hazard_ctrl),
    median_time_to_prog_ctrl = SimNPH::r2m(prog_rate_ctrl)
  )

subgroup <- subgroup |>
  mutate(
    hr_subgroup_display = ifelse(
      hr_subgroup > 1, 
      paste0("1/", 1/hr_subgroup), 
      as.character(hr_subgroup)
      ) |>
      factor(levels=c("0.7", "0.8", "0.9", "1/0.9", "1/0.8", "1/0.7"), ordered = TRUE)
  )


# prepare data for app ----------------------------------------------------

prepare_data <- function(dataset, design_varnames, time_varnames=character(0), time_trafo=identity){
  dataset <- dataset |> 
    mutate(
      across(any_of(time_varnames), time_trafo)
    )
  
  dataset <- results_pivot_longer(dataset)
  
  list(
    data = dataset,
    design_variables = design_varnames,
    methods = unique(dataset$method),
    filter_values = lapply(
      design_varnames,
      \(var){
        unique(dataset[[var]]) |>
          sort()
      }
    ) |> 
      setNames(design_varnames)
  )
}

datasets <- list(
  delayed     = prepare_data(delayed,     design_vars_delayed, time_varnames, SimNPH::d2m),
  crossing    = prepare_data(crossing,    design_vars_crossing, time_varnames, SimNPH::d2m),
  subgroup    = prepare_data(subgroup,    design_vars_subgroup, time_varnames, SimNPH::d2m),
  progression = prepare_data(progression, design_vars_progression, time_varnames, SimNPH::d2m)
)


saveRDS(datasets, "datasets.Rds")

# render markdown ---------------------------------------------------------

rmarkdown::render(
  "description.md",
  rmarkdown::html_document(
    template = "pandoc_template.html"
  ),
  output_file="app/description.html"
) 
