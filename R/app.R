stability_app <- function(){

  ui <- shiny::fluidPage(
    shiny::column(
      width = 4,
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
      shiny::selectInput("stability_type", "Choose Stability Assessment Type:", choices = c("Replication Stability", "Statistical Stability", "Stability under Data Selection", "Stability under Model Selection", "Numerical Stability", "Analytic and Algebraic Stability", "Stability under Selection of Technique"))
    ),
    shiny::column(
      width = 8,
      shiny::tableOutput("main_summary"),
      shiny::tableOutput("summary_table"),
      shiny::plotOutput("stability_plot"),
      shiny::verbatimTextOutput("explanation")
    )
  )

  server <- function(input, output, session) {
    stability_results <- shiny::eventReactive(input$go, {
      shiny::req(input$datafile)  # Ensure a file has been uploaded
      data <- utils::read.csv(input$datafile$datapath)
      new_data <- if (!is.null(input$newdatafile$datapath)) utils::read.csv(input$newdatafile$datapath) else NULL
      formula <- stats::as.formula(input$formula)
      engine <- if (input$model_type == "lm") stats::lm else stats::glm
      family <- if (input$model_type == "glm") get(input$family)() else NULL
      stability_assessment(data, formula, engine, new_data = new_data, nboot = input$nboot, variable_to_remove = input$variable_to_remove, variable_of_interest = input$variable_of_interest, family = family)
    }, ignoreNULL = FALSE)

    output$main_summary <- shiny::renderTable({
      shiny::req(stability_results())
      stability_results()$gstab_summary$original_summary
    })

    output$summary_table <- shiny::renderTable({
      shiny::req(stability_results())
      summary_type <- switch(input$stability_type,
                             "Replication Stability" = "replication_stability_summary",
                             "Statistical Stability" = "statistical_stability_summary",
                             "Stability under Data Selection" = "data_selection_stability_summary",
                             "Stability under Model Selection" = "model_selection_stability_summary",
                             "Numerical Stability" = "numerical_stability_summary",
                             "Analytic and Algebraic Stability" = "analytic_and_algebraic_stability_summary",
                             "Stability under Selection of Technique" = "technique_stability_summary")
      stability_results()$gstab_summary[[summary_type]]
    })

    output$stability_plot <- shiny::renderPlot({
      shiny::req(stability_results())
      plot_type <- switch(input$stability_type,
                          "Replication Stability" = "replication_stability_plot",
                          "Statistical Stability" = "statistical_stability_plot",
                          "Stability under Data Selection" = "data_selection_stability_plot",
                          "Stability under Model Selection" = "model_selection_stability_plot",
                          "Numerical Stability" = "numerical_stability_plot",
                          "Analytic and Algebraic Stability" = "analytic_and_algebraic_stability_plot",
                          "Stability under Selection of Technique" = "technique_stability_plot")
      print(stability_results()$gstab_plot[[plot_type]])
    })

    output$explanation <- shiny::renderPrint({
      shiny::req(stability_results())
      stability_results()$gstab_explainer[[input$stability_type]]
    })
  }

  shiny::shinyApp(ui, server)

}
