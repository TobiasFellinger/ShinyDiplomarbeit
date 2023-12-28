
library(shiny)
library(sortable)
library(ggplot2)

# datasets <- readRDS("../datasets.Rds")

datasets <- list(delayed = list(design_variables = c("delay", "hr_after_onset", 
                                                     "hazard_ctrl", "random_withdrawal", "n_pat", "recruitment"), 
                                methods = c("ahr_6m", "ahr_12m", "gahr_6m", "gahr_12m", "median_surv", 
                                            "milestone_6m", "milestone_12m", "rmst_diff_6m", "rmst_diff_12m", 
                                            "cox", "aft_weibull", "aft_lognormal", "diff_med_weibull", 
                                            "fh_0_0", "fh_0_1", "fh_1_0", "fh_1_1", "logrank", "max_combo", 
                                            "modest_6", "modest_8", "fh_gs_0_0", "fh_gs_0_1", "fh_gs_1_0", 
                                            "fh_gs_1_1", "logrank_gs", "max_combo_gs", "modest_gs_6", 
                                            "modest_gs_8"), filter_values = list(delay = c(0, 60.875, 
                                                                                           121.75, 182.625, 243.5), hr_after_onset = c(0.7, 0.8, 0.9, 
                                                                                                                                       1), hazard_ctrl = c(0.00189773355389444, 0.000948866776947221, 
                                                                                                                                                           0.000632577851298148), random_withdrawal = c(0, 0.000189773355389444
                                                                                                                                                           ), n_pat = c(NA, 300, 600, 1000), recruitment = c(0, 182.625
                                                                                                                                                           ))), crossing = list(design_variables = c("crossing", "hr_before", 
                                                                                                                                                                                                     "hr_after", "hazard_ctrl", "random_withdrawal", "n_pat", "recruitment"
                                                                                                                                                           ), methods = c("ahr_6m", "ahr_12m", "gahr_6m", "gahr_12m", "median_surv", 
                                                                                                                                                                          "milestone_6m", "milestone_12m", "rmst_diff_6m", "rmst_diff_12m", 
                                                                                                                                                                          "cox", "aft_weibull", "aft_lognormal", "diff_med_weibull", "fh_0_0", 
                                                                                                                                                                          "fh_0_1", "fh_1_0", "fh_1_1", "logrank", "max_combo", "modest_6", 
                                                                                                                                                                          "modest_8", "fh_gs_0_0", "fh_gs_0_1", "fh_gs_1_0", "fh_gs_1_1", 
                                                                                                                                                                          "logrank_gs", "max_combo_gs", "modest_gs_6", "modest_gs_8"), 
                                                                                                                                                           filter_values = list(crossing = c(0, 60.875, 121.75, 182.625, 
                                                                                                                                                                                             243.5), hr_before = c(1.42857142857143, 1.25, 1.11111111111111
                                                                                                                                                                                             ), hr_after = c(0.7, 0.8, 0.9), hazard_ctrl = c(0.00189773355389444, 
                                                                                                                                                                                                                                             0.000948866776947221, 0.000632577851298148), random_withdrawal = c(0, 
                                                                                                                                                                                                                                                                                                                0.000189773355389444), n_pat = c(NA, 300, 600, 1000), recruitment = c(0, 
                                                                                                                                                                                                                                                                                                                                                                                      182.625))), subgroup = list(design_variables = c("hr_trt", 
                                                                                                                                                                                                                                                                                                                                                                                                                                       "hr_subgroup", "prevalence", "hazard_ctrl", "random_withdrawal", 
                                                                                                                                                                                                                                                                                                                                                                                                                                       "n_pat", "recruitment"), methods = c("ahr_6m", "ahr_12m", "gahr_6m", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "gahr_12m", "median_surv", "milestone_6m", "milestone_12m", "rmst_diff_6m", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "rmst_diff_12m", "cox", "aft_weibull", "aft_lognormal", "diff_med_weibull", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "fh_0_0", "fh_0_1", "fh_1_0", "fh_1_1", "logrank", "max_combo", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "modest_6", "modest_8", "fh_gs_0_0", "fh_gs_0_1", "fh_gs_1_0", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "fh_gs_1_1", "logrank_gs", "max_combo_gs", "modest_gs_6", "modest_gs_8"
                                                                                                                                                                                                                                                                                                                                                                                                                                       ), filter_values = list(hr_trt = c(0.8, 0.9, 1, 0.7), hr_subgroup = c(0.7, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             0.8, 0.9, 1.42857142857143, 1.25, 1.11111111111111), prevalence = c(0.1, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 0.3, 0.5), hazard_ctrl = c(0.00189773355389444, 0.000948866776947221, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            0.000632577851298148), random_withdrawal = c(0, 0.000189773355389444
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            ), n_pat = c(NA, 300, 600, 1000), recruitment = c(0, 182.625))), 
                 progression = list(design_variables = c("hr_death_before_prog", 
                                                         "hr_after_prog_ctrl", "prog_rate_ctrl", "hr_prog", "hazard_ctrl", 
                                                         "random_withdrawal", "n_pat", "recruitment"), methods = c("ahr_6m", 
                                                                                                                   "ahr_12m", "gahr_6m", "gahr_12m", "median_surv", "milestone_6m", 
                                                                                                                   "milestone_12m", "rmst_diff_6m", "rmst_diff_12m", "cox", 
                                                                                                                   "aft_weibull", "aft_lognormal", "diff_med_weibull", "fh_0_0", 
                                                                                                                   "fh_0_1", "fh_1_0", "fh_1_1", "logrank", "max_combo", "modest_6", 
                                                                                                                   "modest_8", "fh_gs_0_0", "fh_gs_0_1", "fh_gs_1_0", "fh_gs_1_1", 
                                                                                                                   "logrank_gs", "max_combo_gs", "modest_gs_6", "modest_gs_8"
                                                         ), filter_values = list(hr_death_before_prog = c(1, 0.7, 
                                                                                                          0.8, 0.9), hr_after_prog_ctrl = c(2, 3), prog_rate_ctrl = 0.00189773355389444, 
                                                                                 hr_prog = c(1, 0.7, 0.8, 0.9), hazard_ctrl = c(0.00189773355389444, 
                                                                                                                                0.000948866776947221, 0.000632577851298148), random_withdrawal = c(0, 
                                                                                                                                                                                                   0.000189773355389444), n_pat = c(NA, 300, 600, 1000), 
                                                                                 recruitment = c(0, 182.625))))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "results_scenarioclass",
        label = "Scenario",
        choices = names(datasets)
      ),
      uiOutput(
        outputId = "results_methods_ui"
      ),
      uiOutput(
        outputId = "results_yvar_ui"
      ),
      uiOutput(
        outputId = "results_scenariofilter_ui"
      ),
      uiOutput(
        outputId = "results_filters_ui"
      )
    ),
    
    mainPanel(
      plotOutput("results_plot"),
      actionButton(
        inputId = "results_draw",
        label = "redraw",
        icon = icon("redo")
      )
    )
  )
)

server <- function(input, output) {
  
  scenario_class <- reactive({
    datasets[[input$results_scenarioclass]]
  })
  
  filter_vars <- reactive({
    input$filter_vars
  })
  
  output$results_methods_ui <- renderUI({
    selectInput(
      inputId = "results_methods",
      label = "Methods",
      choices = scenario_class()$methods,
      multiple = TRUE
    )
  })
  
  output$results_yvar_ui <- renderUI({
    selectInput(
      inputId = "results_yvar",
      label = "Plot Variable",
      choices = names(scenario_class()$data)
    )
  })
  
  output$results_scenariofilter_ui <- renderUI({
    bucket_list(
      header="Select Scenario Parameters",
      group_name="scenario_params",
      orientation = "vertical",
      add_rank_list(
        text = "x-Axis",
        labels=scenario_class()$design_variables,
        input_id = "loop_xvars"
      ),
      add_rank_list(
        text = "Facets, Columns",
        labels=NULL,
        input_id = "facets_cols"
      ),
      add_rank_list(
        text = "Facets, Rows",
        labels=NULL,
        input_id = "facets_rows"
      ),
      add_rank_list(
        text = "Filter",
        labels=NULL,
        input_id = "filter_vars"
      )
    )
  })
  
  output$results_filters_ui <- renderUI({
    lapply(filter_vars(), \(var){
      selectInput(
        inputId = paste0("filter_", var),
        label = var,
        choices = scenario_class()$filter_values[[var]]
      )
    })
  })
  
  output$results_plot <- renderPlot(
    ggplot(NULL, aes(x=0, y=0, label=paste0(input$filter_vars, collapse="\n"))) + 
      geom_text()
  ) |> bindEvent(input$results_draw)
  
}

shinyApp(ui = ui, server = server)
