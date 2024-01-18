
# TODO: 
# * change column names
# * plot annotations for coverage (rect), rejection (hline)

# packages ----------------------------------------------------------------

library("shiny")
library("sortable")
library("ggplot2")
library("patchwork")
library("miniPCH")

# data --------------------------------------------------------------------

# adjust depending on where this is hosted:
# shiny-live served with httpuv
# my_url <- "http://127.0.0.1:7446/"
# shiny-live on github pages
# my_url <- "https://tobiasfellinger.github.io/ShinyDiplomarbeit/"
# datasets <- readRDS(gzcon(url(paste0(my_url, "datasets.Rds"))))
# shiny local
my_file <- "../"
datasets <- readRDS(paste0(my_file, "datasets.Rds"))
exclude_from_scenario_vars <- c("recruitment", "random_withdrawal", "n_pat_design")
filter_scenario_values <- c("recruitment"=0, "random_withdrawal"=0, "n_pat_design"=1000, "method"="logrank")
scenario_table_vars <- c(c("median_survival_trt", "median_survival_ctrl", "rmst_trt_6m", "rmst_ctrl_6m", "gAHR_6m", "AHR_6m", "rmst_trt_12m", "rmst_ctrl_12m", "gAHR_12m", "AHR_12m", "milestone_survival_trt_6m", "milestone_survival_ctrl_6m", "milestone_survival_trt_12m", "milestone_survival_ctrl_12m"))

# functions ---------------------------------------------------------------

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
    use_shapes  = NULL,
    yrange = c(NA_real_, NA_real_)
){
  
  stopifnot(split_var <= length(xvars))
  stopifnot(split_var > 0)
  
  stopifnot(grid_level > 0)
  grid_level <- min(grid_level, length(xvars))
  
  facet_vars_y_sym <- rlang::syms(facet_y_vars)
  facet_vars_x_sym <- rlang::syms(facet_x_vars)
  yvar  <- rlang::sym(yvar)
  
  data <- data |>
    subset(method %in% methods)
  
  # create combined x variable
  data$x <- do.call(interaction, c(data[, xvars], lex.order=TRUE, drop=TRUE)) |>
    as.integer()
  
  # remove rows with missing yvar
  data <- data |> 
    subset(!is.na(get(yvar)))
  
  # split lines
  group_vars <- xvars[1:split_var]
  data$x_split <- do.call(interaction, c(data[, group_vars], lex.order=TRUE, drop=TRUE)) |>
    as.integer()
  
  # grid breaks
  grid_vars <- xvars[1:grid_level]
  data$x_grid <-  do.call(interaction, c(data[, grid_vars], lex.order=TRUE))
  grid_breaks <- data$x[order(data$x)][!duplicated(data$x_grid[order(data$x)])]
  
  plot_1 <- ggplot2::ggplot(data, ggplot2::aes(
    x=x,
    y=!!yvar,
    group=interaction(method, x_split),
    colour=method,
    shape=method
  )) +
    ggplot2::annotate("rect", xmin=-Inf, xmax=Inf, ymin=yrange[1], ymax=yrange[2], colour="grey", alpha=0.5) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size=4) +
    ggplot2::scale_x_continuous(
      breaks = grid_breaks,
      minor_breaks = NULL,
      expand = ggplot2::expansion(0,0)
    ) +
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
    
    if(is.factor(data_plot2[, xx, drop=TRUE])){
      data_plot2$tmp_yvar <- data_plot2[, xx, drop=TRUE]
    } else {
      data_plot2$tmp_yvar <- factor(format(data_plot2[, xx, drop=TRUE], digits=3))
    }
    
    ggplot2::ggplot(data_plot2, ggplot2::aes(x=x, y=tmp_yvar, group=method)) +
      ggplot2::geom_step(linewidth=0.25) +
      # ggplot2::geom_point(shape=4) +
      ggplot2::scale_x_continuous(
        breaks = grid_breaks,
        minor_breaks = NULL,
        expand = ggplot2::expansion(0,0)
      ) +
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

scenario_plot <- function(scenario, type){
  range_t  <- c(0, 1095.75)
  range_hr <- c(0.7, 1/0.7)
  range_s  <- c(0,1)
  
  if(nrow(scenario) == 0){
    return(
      ggplot(data=NULL) + 
      geom_text(aes(x=0,y=0, label="No scenarios for selected parameter combination.")) + 
      theme_void()
    )
  }
  
  # construct functions
  if(type %in% c("delayed", "crossing", "subgroup")){
    funs_a <- miniPCH::pch_functions(
      t=0, 
      lambda=scenario$hazard_ctrl
    )
  } else {
    funs_a <- miniPCH::multistate_functions(
      t = 0,
      Q = array(matrix(c(
          -(scenario$hazard_ctrl + scenario$prog_rate_ctrl),     scenario$prog_rate_ctrl,       scenario$hazard_ctrl,
                                                          0, -scenario$hazard_after_prog, scenario$hazard_after_prog,
                                                          0,                           0,                          0
        ), 3, 3, byrow = TRUE),
        dim=c(3,3,1)
      ),
      pi = c(1,0,0),
      abs = c(0,0,1)
    )
  }
  
  switch(
    type,
    delayed = {
      if(scenario$delay == 0){
        funs_b <- miniPCH::pch_functions(
          t = c(0),
          lambda = c(scenario$hazard_trt)
        )
      } else {
        funs_b <- miniPCH::pch_functions(
          t = c(0, scenario$delay),
          lambda = c(scenario$hazard_ctrl, scenario$hazard_trt)
        )
      }
    },
    crossing = {
      if(scenario$crossing == 0){
        funs_b <- miniPCH::pch_functions(
          t = c(0),
          lambda = c(scenario$hazard_trt_after)
        )
      } else {
       funs_b <- miniPCH::pch_functions(
         t = c(0, scneario$crossing),
         lambda = c(scenario$hazard_trt_before, scenario$hazard_trt_after)
       )
      }
    },
    subgroup = {
      funs_b <- miniPCH::multistate_functions(
        t = 0,
        Q = array(matrix(c(
            -scenario$hazard_subgroup,                    0, scenario$hazard_subgroup,
                                    0, -scenario$hazard_trt,      scenario$hazard_trt,
                                    0,                    0,                        0
          ), 3,3, byrow = TRUE),
          dim=c(3,3,1)
        ),
        pi = c(scenario$prevalence, (1-scenario$prevalence),0),
        abs = c(0,0,1)
      )
    },
    progression = {
      funs_b <- miniPCH::multistate_functions(
        t = 0,
      Q = array(matrix(c(
          -(scenario$hazard_trt + scenario$prog_rate_trt),      scenario$prog_rate_trt,        scenario$hazard_trt,
                                                        0, -scenario$hazard_after_prog, scenario$hazard_after_prog,
                                                        0,                           0,                          0
        ), 3, 3, byrow = TRUE),
        dim=c(3,3,1)
      ),
        pi = c(1,0,0),
        abs = c(0,0,1)
      )
    }
  )
  
  hr <- \(t){
    funs_b$h(t) / funs_a$h(t)
  }
  
  # plot
  plot_s <- ggplot(NULL) + 
    stat_function(aes(colour="control"  , lty="control"  ),   fun=funs_a$s) + 
    stat_function(aes(colour="treatment", lty="treatment"), fun=funs_b$s) + 
    scale_x_continuous(limits=range_t, expand=expansion(0,0), name="time [days]") + 
    scale_y_continuous(limits=range_s, expand=expansion(0,0), name="survival")
  
  plot_h <- ggplot(NULL) + 
    stat_function(aes(colour="control"  , lty="control"  ),   fun=funs_a$h) + 
    stat_function(aes(colour="treatment", lty="treatment"), fun=funs_b$h) + 
    scale_x_continuous(limits=range_t, expand=expansion(0,0), name="time [days]") +
    scale_y_continuous(name="hazard") +
    expand_limits(y=0)
  
  plot_hr <- ggplot(NULL) + 
    stat_function(fun=hr) + 
    geom_hline(yintercept = 1, colour="darkgray") +
    scale_x_continuous(limits=range_t, expand=expansion(0,0), name="time [days]") +
    scale_y_continuous(limits=range_hr, expand=expansion(0,0.1), name="hazard ratio")
  
  my_colors <- palette.colors(3, "Okabe-Ito")[c(2,3)] |>
    setNames(c("control", "treatment"))
  
  my_lty <- c(1, 3) |>
    setNames(c("control", "treatment"))
  
  
  patchwork::wrap_plots(plot_s, plot_h, plot_hr) +
    patchwork::plot_layout(guides = "collect") &
    ggplot2::theme_bw() &
    ggplot2::theme(legend.position="bottom") &
    ggplot2::scale_colour_manual(
      values = my_colors,
      name = "group"
    ) &
    ggplot2::scale_linetype_manual(
      values = my_lty,
      name = "group"
    )
}

# ui ----------------------------------------------------------------------

ui <- fluidPage(
  titlePanel(title="Simulation Results", windowTitle = "Simulation Results"),
  tabsetPanel(
# Tab: Description --------------------------------------------------------
    tabPanel(
      "Description",
      includeHTML("description.html"),
      style="margin-top:1rem;"
    ),
# Tab: Scenarios ----------------------------------------------------------
    tabPanel(
      "Scenarios",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "scenarios_scenarioclass",
            label = "Scenario",
            choices = names(datasets)
          ),
          uiOutput(
            outputId = "scenario_scenariofilter_ui"
          )
        ),
        mainPanel(
          actionButton(
            inputId = "scenario_draw",
            label = "redraw",
            icon = icon("redo")
          ),
          plotOutput(
            "scenario_plot",
            width="100%",
            height="600px"
          ),
          tableOutput(
            "scenario_table"
          )
        )
      ),
      style="margin-top:1rem;"
    ),
# Tab: Results ------------------------------------------------------------
    tabPanel(
      "Simulation Results",
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
          actionButton(
            inputId = "results_draw",
            label = "redraw",
            icon = icon("redo")
          ),
          plotOutput(
            "results_plot",
            width="100%",
            height="800px"
          ),
          checkboxInput("hline0025", "horizontal line at 0.025 (nominal alpha)"),
          checkboxInput("hline0031", "horizontal line at 0.031 (nominal alpha + CI)"),
          checkboxInput("hline095",  "horizontal line at 0.95 (nominal CI coverage)"),
          checkboxInput("rectCoverage", "ribbon from 0.9584 to 0.9412 (nominal coverage + CI)")
        )
      ),
      style="margin-top:1rem;"
    )
  )
)

# server ------------------------------------------------------------------

server <- function(input, output) {

# Tab Results: reactive values --------------------------------------------
  
  scenario_class <- reactive({
    datasets[[input$results_scenarioclass]]
  })
  
  filter_vars <- reactive({
    input$filter_vars
  })
  

# Tab Results: render UI --------------------------------------------------

  output$results_methods_ui <- renderUI({
    selectInput(
      inputId = "results_methods",
      label = "Methods",
      choices = scenario_class()$methods,
      multiple = TRUE,
      selected = c("logrank", "max_combo")
    )
  })
  
  output$results_yvar_ui <- renderUI({
    selectInput(
      inputId = "results_yvar",
      label = "Plot Variable",
      selected = "rejection_0.025",
      choices = names(scenario_class()$data)
    )
  })
  
  output$results_scenariofilter_ui <- renderUI({
    bucket_list(
      header="Scenario Parameters",
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

# Tab Results: Plot -------------------------------------------------------

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
    
    hlines <- c()
    if(input$hline0025){
      hlines <- c(hlines, 0.025)
    }
    if(input$hline0031){
      hlines <- c(hlines, 0.031)
    }
    if(input$hline095){
      hlines <- c(hlines, 0.95)
    }
    
    if(input$rectCoverage){
      yrange=c(0.9412, 0.9584)
    } else {
      yrange = c(NA_real_, NA_real_)
    }
          
          
    combined_plot(
      tmp_data,
      tmp_methods,
      tmp_xvars,
      tmp_yvar,
      tmp_cols,
      tmp_rows,
      hlines = hlines,
      yrange = yrange
    )
  }) |> bindEvent(input$results_draw)
  

# Tab Scenarios: reactive values ------------------------------------------

  scenario_filter_vars <- reactive({
    res <- datasets[[input$scenarios_scenarioclass]]$filter_values
    res <- res[!(names(res) %in% exclude_from_scenario_vars)]
    res
  })
  
  scenario_data <- reactive({
    tmp_data <- datasets[[input$scenarios_scenarioclass]]$data
    tmp_filter <- names(scenario_filter_vars())
    
    tmp_filter_values <- sapply(
      tmp_filter,
      \(i){input[[paste0("scenario_filter_", i)]]}
    )
    names(tmp_filter_values) <- tmp_filter
    
    tmp_filter_values <- c(tmp_filter_values, filter_scenario_values)
    
    for(i in names(tmp_filter_values)){
      tmp_data <- tmp_data[tmp_data[, i] == tmp_filter_values[i], ]
    }
    
    tmp_data
  })|> 
    bindEvent(input$scenario_draw)

# Tab Scenarios: renderUI -------------------------------------------------

  output$scenario_scenariofilter_ui <- renderUI({
    lapply(names(scenario_filter_vars()), \(var){
      selectInput(
        inputId = paste0("scenario_filter_", var),
        label = var,
        choices = scenario_filter_vars()[[var]]
      )
    })
  })

# Tab Scenarios: Plot -----------------------------------------------------
  
  output$scenario_plot <- renderPlot({
    scenario_plot(scenario_data(), input$scenarios_scenarioclass)
  }) |> 
    bindEvent(input$scenario_draw)
  

# Tab Scenario: Table -----------------------------------------------------
  
  output$scenario_table <- renderTable({
    tmp_data <- scenario_data()[, ] |>
      subset(select=scenario_table_vars)
    
    data.frame(
      `Summary Statistic` = names(tmp_data),
      `Value`             = t(tmp_data[1,]),
      check.names = FALSE
    )
  }) 
  
  
  
} # end server


# run ---------------------------------------------------------------------



shinyApp(ui = ui, server = server)
