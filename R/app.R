stability_app <- function(){
  ui <- shiny::fluidPage(
    shiny::column(
      width = 4,
      shiny::h3("Argument Specification"),
      shiny::fileInput("datafile", "Choose CSV File", accept = c(".csv")),
      shiny::fileInput("newdatafile", "Choose New CSV File (Optional)", accept = c(".csv")),
      shiny::textInput("formula", "Enter the formula (e.g., y ~ x1 + x2)"),
      shiny::radioButtons("model_type", "Choose Model Type:", choices = c("lm", "glm")),
      shiny::conditionalPanel(
        condition = "input.model_type == 'glm'",
        shiny::selectInput("family", "Choose Family:", choices = c("binomial", "gaussian", "poisson", "Gamma", "inverse.gaussian", "quasi"))
      ),
      shiny::numericInput("nboot", "Number of Bootstrap Resamples (Optional)", value = NULL, min = 1, max = 10000),
      shiny::textInput("variable_to_remove", "Variable to Remove (Optional)"),
      shiny::textInput("variable_of_interest", "Variable of Interest (Optional)"),
      shiny::actionButton("go", "Perform Stability Assessment"),
      shiny::selectInput("stability_type", "Choose Stability Assessment Type:", choices = c("Replication Stability", "Statistical Stability", "Stability under Data Selection", "Stability under Model Selection", "Numerical Stability", "Analytic and Algebraic Stability", "Stability under Selection of Technique")),
      shiny::uiOutput("subtype_select")  # Create UI output for subtype selection
    ),
    shiny::column(
      width = 8,
      shiny::h3("Main Model"),
      DT::dataTableOutput("main_summary"),
      shiny::tableOutput("summary_table"),
      shiny::plotOutput("stability_plot"),
      shiny::verbatimTextOutput("explanation")
    )
  )

  server <- function(input, output, session) {
    # Define the subtypes for each stability assessment
    stability_subtypes <- list(
      "Replication Stability" = c("New Model", "Bootstrap Models"),
      "Statistical Stability" = NULL,
      "Stability under Model Selection" = NULL,
      "Stability under Data Selection" = c("Bootstrap Model", "No Outlier Model", "Strata Bootstrap Model"),
      "Numerical Stability" = NULL,
      "Analytic and Algebraic Stability" = NULL,
      "Stability under Selection of Technique" = NULL
    )

    # Update the choices of the subtype input when the type input changes
    output$subtype_select <- shiny::renderUI({
      if (!is.null(stability_subtypes[[input$stability_type]])) {
        shiny::selectInput("stability_subtype", "Choose Subtype (If Applicable):", choices = stability_subtypes[[input$stability_type]])
      } else {
        NULL
      }
    })

    stability_results <- shiny::eventReactive(input$go, {
      shiny::req(input$datafile)  # Ensure a file has been uploaded
      data <- utils::read.csv(input$datafile$datapath)
      new_data <- if (!is.null(input$newdatafile$datapath)) utils::read.csv(input$newdatafile$datapath) else NULL
      formula <- stats::as.formula(input$formula)
      engine <- if (input$model_type == "lm") stats::lm else stats::glm
      family <- if (input$model_type == "glm") get(input$family)() else NULL
      stability_assessment(data, formula, engine, new_data = new_data, nboot = input$nboot, variable_to_remove = input$variable_to_remove, variable_of_interest = input$variable_of_interest, family = family)
    }, ignoreNULL = FALSE)

    output$main_summary <- DT::renderDataTable({
      shiny::req(stability_results())
      summary <- stability_results()$gstab_summary$original_summary
      numeric_cols <- which(sapply(summary, is.numeric))
      DT::datatable(summary) |> DT::formatRound(columns = numeric_cols, digits = 3)
    })

    is_list_of_dataframes <- function(x) {
      is.list(x) && all(sapply(x, function(y) inherits(y, "data.frame")))
    }

    # An observer that creates a new table output for each tibble in the selected summary
    shiny::observe({
      tryCatch({
        shiny::req(stability_results())
        summary_type <- switch(input$stability_type,
                               "Replication Stability" = switch(input$stability_subtype,
                                                                "New Model" = "replication_stability_summary$new_model",
                                                                "Bootstrap Models" = "replication_stability_summary$boot_models"),
                               "Statistical Stability" = "statistical_stability_summary",
                               "Stability under Data Selection" = switch(input$stability_subtype,
                                                                         "Bootstrap Model" = "data_selection_stability_summary$bootstrap_model",
                                                                         "No Outlier Model" = "data_selection_stability_summary$no_outlier_model",
                                                                         "Strata Bootstrap Model" = "data_selection_stability_summary$strata_boot_model"),
                               "Stability under Model Selection" = "model_selection_stability_summary",
                               "Numerical Stability" = "numerical_stability_summary",
                               "Analytic and Algebraic Stability" = "analytic_and_algebraic_stability_summary",
                               "Stability under Selection of Technique" = "technique_stability_summary")
        summary <- eval(parse(text = paste0("stability_results()$gstab_summary$", summary_type)))
        if (is_list_of_dataframes(summary)) {
          lapply(names(summary), function(name) {
            output[[name]] <- DT::renderDataTable({
              numeric_cols <- which(sapply(summary[[name]], is.numeric))
              DT::datatable(summary[[name]]) |> DT::formatRound(columns = numeric_cols, digits = 3)
            })
          })
        } else {
          output$single_summary <- DT::renderDataTable({
            numeric_cols <- which(sapply(summary, is.numeric))
            DT::datatable(summary) |> DT::formatRound(columns = numeric_cols, digits = 3)
          })
        }
      }, error = function(e) {
        message("Error in observe: ", e)
      })
    })

    output$summary_table <- shiny::renderUI({
      shiny::req(stability_results())
      summary_type <- switch(input$stability_type,
                             "Replication Stability" = switch(input$stability_subtype,
                                                              "New Model" = "replication_stability_summary$new_model",
                                                              "Bootstrap Models" = "replication_stability_summary$boot_models"),
                             "Statistical Stability" = "statistical_stability_summary",
                             "Stability under Data Selection" = switch(input$stability_subtype,
                                                                       "Bootstrap Model" = "data_selection_stability_summary$bootstrap_model",
                                                                       "No Outlier Model" = "data_selection_stability_summary$no_outlier_model",
                                                                       "Strata Bootstrap Model" = "data_selection_stability_summary$strata_boot_model"),
                             "Stability under Model Selection" = "model_selection_stability_summary",
                             "Numerical Stability" = "numerical_stability_summary",
                             "Analytic and Algebraic Stability" = "analytic_and_algebraic_stability_summary",
                             "Stability under Selection of Technique" = "technique_stability_summary")
      summary <- eval(parse(text = paste0("stability_results()$gstab_summary$", summary_type)))
      if (is_list_of_dataframes(summary)) {
        do.call(shiny::tagList, lapply(names(summary), function(name) {
          list(
            shiny::h3(tools::toTitleCase(gsub("_", " ", name))),
            DT::dataTableOutput(name)
          )
        }))
      } else {
        single_summary_title <- unlist(strsplit(summary_type, "\\$"))[2]
        list(
          shiny::h3(tools::toTitleCase(gsub("_", " ", single_summary_title))),
          DT::dataTableOutput("single_summary")
        )
      }
    })

    output$stability_plot <- shiny::renderPlot({
      shiny::req(stability_results())
      plot_type <- switch(input$stability_type,
                          "Replication Stability" = switch(input$stability_subtype,
                                                           "New Model" = "replication_stability_plot$new",
                                                           "Bootstrap Models" = "replication_stability_plot$boot"),
                          "Statistical Stability" = "statistical_stability_plot",
                          "Stability under Data Selection" = switch(input$stability_subtype,
                                                                    "Bootstrap Model" = "data_selection_stability_plot$bootstrap",
                                                                    "No Outlier Model" = "data_selection_stability_plot$no_outlier",
                                                                    "Strata Bootstrap Model" = "data_selection_stability_plot$strata_bootstrap"),
                          "Stability under Model Selection" = "model_selection_stability_plot",
                          "Numerical Stability" = "numerical_stability_plot",
                          "Analytic and Algebraic Stability" = "analytic_and_algebraic_stability_plot",
                          "Stability under Selection of Technique" = "technique_stability_plot")
      print(eval(parse(text = paste0("stability_results()$gstab_plot$", plot_type))))
    })

    output$explanation <- shiny::renderPrint({
      shiny::req(stability_results())
      stability_results()$gstab_explainer[[input$stability_type]]
    })
  }

  shiny::shinyApp(ui, server)
}
