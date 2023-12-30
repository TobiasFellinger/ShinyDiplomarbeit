
# packages ----------------------------------------------------------------

library("shiny")
library("sortable")
library("ggplot2")
library("patchwork")

# data --------------------------------------------------------------------

datasets <- readRDS("../datasets.Rds")


# datasets <- list(delayed = list(design_variables = c("delay", "hr_after_onset", 
#                                                      "hazard_ctrl", "random_withdrawal", "n_pat", "recruitment"), 
#                                 methods = c("ahr_6m", "ahr_12m", "gahr_6m", "gahr_12m", "median_surv", 
#                                             "milestone_6m", "milestone_12m", "rmst_diff_6m", "rmst_diff_12m", 
#                                             "cox", "aft_weibull", "aft_lognormal", "diff_med_weibull", 
#                                             "fh_0_0", "fh_0_1", "fh_1_0", "fh_1_1", "logrank", "max_combo", 
#                                             "modest_6", "modest_8", "fh_gs_0_0", "fh_gs_0_1", "fh_gs_1_0", 
#                                             "fh_gs_1_1", "logrank_gs", "max_combo_gs", "modest_gs_6", 
#                                             "modest_gs_8"), filter_values = list(delay = c(0, 60.875, 
#                                                                                            121.75, 182.625, 243.5), hr_after_onset = c(0.7, 0.8, 0.9, 
#                                                                                                                                        1), hazard_ctrl = c(0.00189773355389444, 0.000948866776947221, 
#                                                                                                                                                            0.000632577851298148), random_withdrawal = c(0, 0.000189773355389444
#                                                                                                                                                            ), n_pat = c(NA, 300, 600, 1000), recruitment = c(0, 182.625
#                                                                                                                                                            ))), crossing = list(design_variables = c("crossing", "hr_before", 
#                                                                                                                                                                                                      "hr_after", "hazard_ctrl", "random_withdrawal", "n_pat", "recruitment"
#                                                                                                                                                            ), methods = c("ahr_6m", "ahr_12m", "gahr_6m", "gahr_12m", "median_surv", 
#                                                                                                                                                                           "milestone_6m", "milestone_12m", "rmst_diff_6m", "rmst_diff_12m", 
#                                                                                                                                                                           "cox", "aft_weibull", "aft_lognormal", "diff_med_weibull", "fh_0_0", 
#                                                                                                                                                                           "fh_0_1", "fh_1_0", "fh_1_1", "logrank", "max_combo", "modest_6", 
#                                                                                                                                                                           "modest_8", "fh_gs_0_0", "fh_gs_0_1", "fh_gs_1_0", "fh_gs_1_1", 
#                                                                                                                                                                           "logrank_gs", "max_combo_gs", "modest_gs_6", "modest_gs_8"), 
#                                                                                                                                                            filter_values = list(crossing = c(0, 60.875, 121.75, 182.625, 
#                                                                                                                                                                                              243.5), hr_before = c(1.42857142857143, 1.25, 1.11111111111111
#                                                                                                                                                                                              ), hr_after = c(0.7, 0.8, 0.9), hazard_ctrl = c(0.00189773355389444, 
#                                                                                                                                                                                                                                              0.000948866776947221, 0.000632577851298148), random_withdrawal = c(0, 
#                                                                                                                                                                                                                                                                                                                 0.000189773355389444), n_pat = c(NA, 300, 600, 1000), recruitment = c(0, 
#                                                                                                                                                                                                                                                                                                                                                                                       182.625))), subgroup = list(design_variables = c("hr_trt", 
#                                                                                                                                                                                                                                                                                                                                                                                                                                        "hr_subgroup", "prevalence", "hazard_ctrl", "random_withdrawal", 
#                                                                                                                                                                                                                                                                                                                                                                                                                                        "n_pat", "recruitment"), methods = c("ahr_6m", "ahr_12m", "gahr_6m", 
#                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "gahr_12m", "median_surv", "milestone_6m", "milestone_12m", "rmst_diff_6m", 
#                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "rmst_diff_12m", "cox", "aft_weibull", "aft_lognormal", "diff_med_weibull", 
#                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "fh_0_0", "fh_0_1", "fh_1_0", "fh_1_1", "logrank", "max_combo", 
#                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "modest_6", "modest_8", "fh_gs_0_0", "fh_gs_0_1", "fh_gs_1_0", 
#                                                                                                                                                                                                                                                                                                                                                                                                                                                                             "fh_gs_1_1", "logrank_gs", "max_combo_gs", "modest_gs_6", "modest_gs_8"
#                                                                                                                                                                                                                                                                                                                                                                                                                                        ), filter_values = list(hr_trt = c(0.8, 0.9, 1, 0.7), hr_subgroup = c(0.7, 
#                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              0.8, 0.9, 1.42857142857143, 1.25, 1.11111111111111), prevalence = c(0.1, 
#                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  0.3, 0.5), hazard_ctrl = c(0.00189773355389444, 0.000948866776947221, 
#                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             0.000632577851298148), random_withdrawal = c(0, 0.000189773355389444
#                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             ), n_pat = c(NA, 300, 600, 1000), recruitment = c(0, 182.625))), 
#                  progression = list(design_variables = c("hr_death_before_prog", 
#                                                          "hr_after_prog_ctrl", "prog_rate_ctrl", "hr_prog", "hazard_ctrl", 
#                                                          "random_withdrawal", "n_pat", "recruitment"), methods = c("ahr_6m", 
#                                                                                                                    "ahr_12m", "gahr_6m", "gahr_12m", "median_surv", "milestone_6m", 
#                                                                                                                    "milestone_12m", "rmst_diff_6m", "rmst_diff_12m", "cox", 
#                                                                                                                    "aft_weibull", "aft_lognormal", "diff_med_weibull", "fh_0_0", 
#                                                                                                                    "fh_0_1", "fh_1_0", "fh_1_1", "logrank", "max_combo", "modest_6", 
#                                                                                                                    "modest_8", "fh_gs_0_0", "fh_gs_0_1", "fh_gs_1_0", "fh_gs_1_1", 
#                                                                                                                    "logrank_gs", "max_combo_gs", "modest_gs_6", "modest_gs_8"
#                                                          ), filter_values = list(hr_death_before_prog = c(1, 0.7, 
#                                                                                                           0.8, 0.9), hr_after_prog_ctrl = c(2, 3), prog_rate_ctrl = 0.00189773355389444, 
#                                                                                  hr_prog = c(1, 0.7, 0.8, 0.9), hazard_ctrl = c(0.00189773355389444, 
#                                                                                                                                 0.000948866776947221, 0.000632577851298148), random_withdrawal = c(0, 
#                                                                                                                                                                                                    0.000189773355389444), n_pat = c(NA, 300, 600, 1000), 
#                                                                                  recruitment = c(0, 182.625))))


# functions ---------------------------------------------------------------

library("ggplot2")
library("patchwork")

combined_plot <- function(
    data,
    methods,
    xvars,
    yvar,
    facet_x_vars=c(),
    facet_y_vars=c(),
    split_var = 1,
    heights_plots = c(3,1),
    scale_stairs = 0.75,
    grid_level = 2,
    scales = "fixed",
    hlines = numeric(0),
    use_colours = NULL,
    use_shapes  = NULL
){
  
  stopifnot(split_var <= length(xvars))
  stopifnot(split_var > 0)
  
  facet_vars_y_sym <- rlang::syms(facet_y_vars)
  facet_vars_x_sym <- rlang::syms(facet_x_vars)
  yvar  <- rlang::sym(yvar)
  
  data <- data |>
    subset(method %in% methods)
  
  xvars <- rev(xvars)
  
  # create combined x variable
  data$x <- dplyr::dense_rank(do.call(interaction, data[, xvars]))
  
  # remove rows with missing yvar
  data <- data |> 
    subset(!is.na(get(yvar)))
  
  # split lines
  group_vars <- xvars[(length(xvars)-split_var):length(xvars)]
  data$x_split <- dplyr::dense_rank(do.call(interaction, data[, group_vars]))
  
  plot_1 <- ggplot2::ggplot(data, ggplot2::aes(
    x=x,
    y=!!yvar,
    group=interaction(method, x_split),
    colour=method,
    shape=method
  )) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size=4) +
    # ggplot2::scale_x_discrete(breaks = attr(data, "x_axis_breaks")) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank()
    )  +
    ggplot2::facet_grid(
      cols = dplyr::vars(!!!facet_vars_x_sym),
      rows = dplyr::vars(!!!facet_vars_y_sym),
      labeller = ggplot2::label_both,
      scales = scales
    ) +
    ggplot2::geom_hline(yintercept=hlines)
  
  data_plot2 <- data[!duplicated(do.call(interaction, data[,c("x", facet_x_vars)])), ]
  
  plot_2 <- lapply(xvars, \(xx){
    data_plot2 <- data_plot2 |> 
      within(
        tmp_yvar <- factor(format(get(xx), digits=3))
      )
    
    ggplot2::ggplot(data_plot2, ggplot2::aes(x=x, y=tmp_yvar, group=method)) +
      ggplot2::geom_step(linewidth=0.25) +
      # ggplot2::geom_point(shape=4) +
      ggplot2::theme_void(
        base_size = 9
      ) +
      ggplot2::theme(
        axis.text.y = ggplot2::element_text(),
        axis.title.y = ggplot2::element_text(angle=75),
        strip.background = ggplot2::element_blank(),
        strip.text = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_line(
          linewidth = 0.125,
          colour="lightgray"
        )
      ) +
      ggplot2::ylab(as.character(xx))  +
      ggplot2::facet_grid(
        cols = dplyr::vars(!!!facet_vars_x_sym)
      )
  })
  
  plot_2 <- patchwork::wrap_plots(plot_2, ncol=1)
  
  if(!is.null(use_colours)){
    plot_1 <- plot_1 +
      ggplot2::scale_colour_manual(values=use_colours)
  }
  
  if(!is.null(use_shapes)){
    plot_1 <- plot_1 +
      ggplot2::scale_shape_manual(values=use_shapes)
  }
  result <- (plot_1 / plot_2) + patchwork::plot_layout(heights=heights_plots)
  result
}

# ui ----------------------------------------------------------------------

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

# server ------------------------------------------------------------------

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
  
  output$results_plot <- renderPlot({
    tmp_data <- scenario_class()$data 
    tmp_methods <- input$results_methods
    tmp_xvars <- input$loop_xvars
    tmp_yvar <- input$results_yvar
    tmp_cols <- input$facets_cols
    tmp_rows <- input$facets_rows
    tmp_filter <- input$filter_vars
    
    tmp_filter_values <- sapply(
      tmp_filter,
      \(i){input[[paste0("filter_", i)]]}
    )
    names(tmp_filter_values) <- tmp_filter
    
    for(i in tmp_filter){
      tmp_data <- tmp_data[tmp_data[, i] == tmp_filter_values[i], ]
    }
    
    combined_plot(
      tmp_data,
      tmp_methods,
      tmp_xvars,
      tmp_yvar,
      tmp_cols,
      tmp_rows
    )
  }) |> bindEvent(input$results_draw)
  
}


# run ---------------------------------------------------------------------



shinyApp(ui = ui, server = server)
