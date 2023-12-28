library("SimNPH")

design_vars_all <- c("hazard_ctrl", "random_withdrawal", "n_pat", "recruitment")
design_vars_delayed     <- c("delay", "hr_after_onset", design_vars_all)
design_vars_crossing    <- c("crossing", "hr_before", "hr_after", design_vars_all)
design_vars_progression <- c("hr_death_before_prog", "hr_after_prog_ctrl", "prog_rate_ctrl", "hr_prog", design_vars_all)
design_vars_subgroup    <- c("hr_trt", "hr_subgroup", "prevalence", design_vars_all)


delayed <- readRDS("../github/Simulations/data/simulation_delayed_effect_WTGP024_2023-10-24_160438/results.Rds") |>
  results_pivot_longer()

crossing <- readRDS("../github/Simulations/data/simulation_crossing_harzards_WTGP024_2023-11-06_101033/results.Rds") |>
  results_pivot_longer()

subgorup <- readRDS("../github/Simulations/data/simulation_subgroup_WTGP024_2023-12-05_095206/results.Rds") |>
  results_pivot_longer()

progression <- readRDS("../github/Simulations/data/simulation_disease_progression_WTGP024_2023-11-16_094021/results.Rds") |>
  results_pivot_longer()

prepare_data <- function(dataset, design_varnames){
  list(
    data = dataset,
    design_variables = design_varnames,
    methods = unique(dataset$method),
    filter_values = lapply(
      design_varnames,
      \(var){
        unique(dataset[[var]])
      }
    ) |> 
      setNames(design_varnames)
  )
}

datasets <- list(
  delayed     = prepare_data(delayed,     design_vars_delayed),
  crossing    = prepare_data(crossing,    design_vars_crossing),
  subgroup    = prepare_data(subgorup,    design_vars_subgroup),
  progression = prepare_data(progression, design_vars_progression)
)

saveRDS(datasets, "app/datasets.Rds")
 