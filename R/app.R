#' Stability App
#'
#' This Shiny web application allows users to perform stability assessments on
#' data loaded from a CSV file. Users can specify the formula for the model,
#' choose the model type, and specify various other parameters.
#' The app will then perform the stability assessment and display the results as
#' tables, plots, and explanations.
#'
#' @return A Shiny web application.
#' @export
#' @keywords interactive
#'
#' @examples
#' # Only run this example in interactive R sessions
#' if(interactive()){
#'   stability_app()
#' }
stability_app <- function(){
  ui <- shiny::fluidPage(
    theme = shinythemes::shinytheme("spacelab"),
    shinyjs::useShinyjs(),
    shiny::column(
      width = 4,
      shiny::h3("Arguments"),
      shiny::fileInput("datafile", "Choose CSV File", accept = c(".csv")),
      shiny::fileInput("newdatafile", "Choose New CSV File (Optional)", accept = c(".csv")),
      shiny::textInput("formula", "Enter the formula (e.g., y ~ x1 + x2)"),
      shiny::radioButtons("model_type", "Choose Model Type:", choices = c("lm", "glm")),
      shiny::conditionalPanel(
        condition = "input.model_type == 'glm'",
        shiny::selectInput("family", "Choose Family:", choices = c("binomial", "gaussian", "poisson", "Gamma", "inverse.gaussian", "quasipoisson", "quasibinomial"))
      ),
      shiny::numericInput("nboot", "Number of Bootstrap Resamples (Optional)", value = NA, min = 1, max = 10000),
      shiny::textInput("variable_to_remove", "Variable to Remove (Optional)"),
      shiny::textInput("variable_of_interest", "Variable of Interest (Optional)"),
      shiny::numericInput("nf", "Noise Factor", value = 0.05, min = 0, max = 1),
      shiny::numericInput("conf_level", "Confidence Level:", value = 0.95, min = 0, max = 1),
      shiny::numericInput("seed", "Random Number Seed (Optional for Reproducibility)", value = NA, min = 1, max = .Machine$integer.max),
      shiny::actionButton("go", "Perform Stability Assessment"),
      shiny::br(),  # Add a line break
      shiny::br(),  # Add a line break
      shiny::selectInput("stability_type", "Choose Stability Assessment Type:", choices = c("Replication Stability", "Statistical Stability", "Stability under Data Selection", "Stability under Model Selection", "Numerical Stability", "Analytic and Algebraic Stability", "Stability under Selection of Technique")),
      shiny::uiOutput("subtype_select")  # Create UI output for subtype selection
    ),
    shiny::column(
      width = 8,
      shiny::uiOutput("summary_title"),  #UI output for the title and subtitle
      shiny::uiOutput("main_summary_table"),
      shiny::tableOutput("summary_table"),
      shiny::br(),  # Add a line break
      shiny::br(),  # Add a line break
      shiny::plotOutput("stability_plot"),
      shiny::uiOutput("explanation"),
      shiny::uiOutput("references")
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

    # Function to get the stability summary type
    get_summary_type <- function(input) {
      switch(input$stability_type,
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
    }

    # Function to render DataTable
    render_data_table <- function(name, data) {
      output[[name]] <- DT::renderDataTable({
        numeric_cols <- which(sapply(data, is.numeric))
        DT::datatable(data) |> DT::formatRound(columns = numeric_cols, digits = 3)
      })
    }

    # Update the choices of the subtype input when the type input changes
    output$subtype_select <- shiny::renderUI({
      if (!is.null(stability_subtypes[[input$stability_type]])) {
        shiny::selectInput("stability_subtype", "Choose Subtype (If Applicable):", choices = stability_subtypes[[input$stability_type]])
      } else {
        shinyjs::hidden(shiny::textInput("stability_subtype", NULL))  # Reset the subtype value to NULL when it's not applicable
      }
    })

    stability_results <- shiny::eventReactive(input$go, {
      shiny::req(input$datafile)  # Ensure a file has been uploaded
      data <- utils::read.csv(input$datafile$datapath)
      new_data <- if (!is.null(input$newdatafile$datapath)) utils::read.csv(input$newdatafile$datapath) else NULL
      formula <- if (!is.null(input$formula) && nzchar(input$formula)) stats::as.formula(input$formula) else NULL
      engine <- if (input$model_type == "lm") stats::lm else stats::glm
      family <- if (input$model_type == "glm" && !is.null(input$family) && nzchar(input$family)) get(input$family)() else NULL
      nboot <- if (!is.na(input$nboot)) input$nboot else NULL
      variable_to_remove <- if (!is.null(input$variable_to_remove) && nzchar(input$variable_to_remove)) input$variable_to_remove else NULL
      variable_of_interest <- if (!is.null(input$variable_of_interest) && nzchar(input$variable_of_interest)) input$variable_of_interest else NULL
      seed <- if (!is.na(input$seed)) input$seed else NULL

      stability_assessment(data, formula, engine, new_data = new_data, nboot = nboot, variable_to_remove = variable_to_remove, variable_of_interest = variable_of_interest, conf.level = input$conf_level, seed = seed, nf = input$nf)
    }, ignoreNULL = FALSE)

    # Server code to render the title and subtitle
    output$summary_title <- shiny::renderUI({
      shiny::req(stability_results())
      title <- input$stability_type
      subtitle <- if (!is.null(stability_subtypes[[input$stability_type]]) && !is.null(input$stability_subtype)) paste(": ", input$stability_subtype) else ""
      shiny::tags$h2(paste0(title, subtitle))
    })

    output$main_summary <- DT::renderDataTable({
      shiny::req(stability_results())
      summary <- stability_results()$gstab_summary$original_summary
      numeric_cols <- which(sapply(summary, is.numeric))
      DT::datatable(summary) |> DT::formatRound(columns = numeric_cols, digits = 3)
    })

    output$main_summary_table <- shiny::renderUI({
      shiny::req(stability_results())
      if (!is.null(stability_results()$gstab_summary$original_summary)) {
        list(
          shiny::h3("Main Model"),
          DT::dataTableOutput("main_summary")
        )
      } else {
        NULL
      }
    })

    is_list_of_dataframes <- function(x) {
      is.list(x) && all(sapply(x, function(y) inherits(y, "data.frame")))
    }

    # An observer that creates a new table output for each tibble in the selected summary
    shiny::observe({
      tryCatch({
        shiny::req(stability_results())
        summary_type <- get_summary_type(input)
        summary <- eval(parse(text = paste0("stability_results()$gstab_summary$", summary_type)))
        if (is_list_of_dataframes(summary)) {
          lapply(names(summary), function(name) {
            render_data_table(name, summary[[name]])
          })
        } else {
          render_data_table("single_summary", summary)
        }
      }, error = function(e) {
        message("Error in observe: ", e)
      })
    })

    output$summary_table <- shiny::renderUI({
      # Check if "Perform Stability Assessment" button has been clicked
      if (input$go > 0) {
        # Check if Replication Stability and Bootstrap Models are selected and nboot is not provided
        if (input$stability_type == "Replication Stability" && input$stability_subtype == "Bootstrap Models" && is.na(input$nboot)) {
          shiny::h3("Please complete the Number of Bootstrap Resamples argument to view this stability assessment")
        } else if (input$stability_type == "Replication Stability" && input$stability_subtype == "New Model" && is.null(input$newdatafile$datapath)) {
          shiny::h3("Please complete the New CSV File argument to view this stability assessment.")
        } else {
          summary_type <- get_summary_type(input)
          # Check if Stability under Model Selection is selected and either variable_to_remove or variable_of_interest are not provided
          if (input$stability_type == "Stability under Model Selection") {
            if (input$variable_to_remove == "" && input$variable_of_interest == "") {
              renderSummaryTable(summary_type, c("toggle_intercept"))
            } else if (input$variable_to_remove == "") {
              renderSummaryTable(summary_type, c("toggle_intercept", "remove_least_useful"))
            } else if (input$variable_of_interest == "") {
              renderSummaryTable(summary_type, c("toggle_intercept", "remove_variable"))
            } else {
              renderSummaryTable(summary_type, c("toggle_intercept", "remove_variable", "remove_least_useful"))
            }
          } else {
            renderSummaryTable(summary_type)
          }
        }
      }
    })

    renderSummaryTable <- function(summary_type, tables = NULL) {
      shiny::req(stability_results())
      summary <- eval(parse(text = paste0("stability_results()$gstab_summary$", summary_type)))
      if (is_list_of_dataframes(summary)) {
        if (!is.null(tables)) {
          summary <- summary[tables]
        }
        do.call(shiny::tagList, lapply(names(summary), function(name) {
          list(
            shiny::h3(tools::toTitleCase(gsub("summary", "", gsub("_", " ", name)))),
            DT::dataTableOutput(name)
          )
        }))
      } else {
        # Check if there's a second element in the summary_type string
        summary_type_split <- unlist(strsplit(summary_type, "\\$"))
        if (length(summary_type_split) > 1) {
          single_summary_title <- summary_type_split[2]
        } else {
          single_summary_title <- summary_type_split[1]
        }
        list(
          shiny::h3(tools::toTitleCase(gsub("summary", "", gsub("_", " ", single_summary_title)))),
          DT::dataTableOutput("single_summary")
        )
      }
    }


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

    output$explanation <- shiny::renderUI({
      shiny::req(stability_results())
      explanation <- stability_results()$gstab_explainer[[input$stability_type]]

      html_text <- paste0(
        "<h3>Definition</h3>",
        "<p>", explanation$definition, "</p>",
        "<h3>Explanation</h3>",
        "<p>", explanation$explanation, "</p>",
        "<h3>Interpretation</h3>",
        "<p>", explanation$interpretation, "</p>",
        "<h3>Caution</h3>",
        "<p>", explanation$caution, "</p>"
      )

      shiny::HTML(html_text)
    })

    output$references <- shiny::renderUI({
      shiny::req(stability_results())
      shiny::HTML("
    <h3>References</h3>
    <ul>
      <li>Michailides, G., & de Leeuw, J. (1998). The Gifi system for nonlinear multivariate analysis. eScholarship, University of California, Los Angeles. <a href='https://escholarship.org/uc/item/0789f7d3'>https://escholarship.org/uc/item/0789f7d3</a></li>
      <!-- Add additional references here -->
    </ul>
  ")
    })
  }

  shiny::shinyApp(ui, server)
}
