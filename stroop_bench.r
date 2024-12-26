# Created for psychology data visualization by jviars

# Install required packages if not already installed
if (!require("shiny")) install.packages("shiny")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("DT")) install.packages("DT")
if (!require("tidyr")) install.packages("tidyr")

# Load required libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(tidyr)

# Analysis Functions ----

#' Analyze Stroop Effect Data
analyze_stroop_effect <- function(congruent_times, incongruent_times) {
  # Perform t-test
  stroop_test <- t.test(congruent_times, incongruent_times, paired = TRUE)
  
  # Calculate effect size (Cohen's d)
  cohens_d <- (mean(incongruent_times) - mean(congruent_times)) / 
    sd(c(congruent_times, incongruent_times))
  
  # Create data frame for plotting
  df <- data.frame(
    condition = rep(c("Congruent", "Incongruent"), 
                    c(length(congruent_times), length(incongruent_times))),
    response_time = c(congruent_times, incongruent_times)
  )
  
  # Create violin plot
  plot <- ggplot(df, aes(x = condition, y = response_time, fill = condition)) +
    geom_violin(alpha = 0.5) +
    geom_boxplot(width = 0.2, alpha = 0.8) +
    theme_minimal() +
    labs(title = "Stroop Effect Analysis",
         x = "Condition",
         y = "Response Time (ms)",
         subtitle = paste("Cohen's d =", round(cohens_d, 3))) +
    theme(legend.position = "none")
  
  return(list(
    t_test = stroop_test,
    effect_size = cohens_d,
    visualization = plot,
    summary_stats = list(
      congruent_mean = mean(congruent_times),
      incongruent_mean = mean(incongruent_times),
      congruent_sd = sd(congruent_times),
      incongruent_sd = sd(incongruent_times)
    )
  ))
}

#' Analyze Confirmation Bias
analyze_confirmation_bias <- function(beliefs, evidence_ratings, evidence_type) {
  # Data validation
  if (length(unique(table(evidence_type))) != 1) {
    stop("Unequal number of supporting and opposing ratings. Each participant should have exactly one supporting and one opposing rating.")
  }
  
  # Create a data frame to ensure proper matching
  df <- data.frame(
    belief = beliefs,
    rating = evidence_ratings,
    type = evidence_type
  )
  
  # Calculate means by evidence type
  supporting_mean <- mean(df$rating[df$type == "supporting"])
  opposing_mean <- mean(df$rating[df$type == "opposing"])
  bias_score <- supporting_mean - opposing_mean
  
  # Calculate rating differences for correlation
  df_wide <- reshape2::dcast(df, belief ~ type, value.var = "rating")
  rating_diff <- df_wide$supporting - df_wide$opposing
  
  # Correlation test
  bias_correlation <- cor.test(df_wide$belief, rating_diff)
  
  # Create visualization
  plot <- ggplot(df, aes(x = belief, y = rating, color = type)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE) +
    theme_minimal() +
    labs(title = "Confirmation Bias Analysis",
         x = "Initial Belief Strength",
         y = "Evidence Rating",
         color = "Evidence Type",
         subtitle = paste("Bias Score =", round(bias_score, 3))) +
    scale_color_brewer(palette = "Set1")
  
  return(list(
    bias_score = bias_score,
    correlation = bias_correlation,
    visualization = plot,
    summary_stats = list(
      supporting_mean = supporting_mean,
      opposing_mean = opposing_mean,
      belief_mean = mean(beliefs)
    )
  ))
}

#' Generate Sample Data
generate_sample_data <- function(n_participants, bias_strength = 0.5) {
  # Stroop effect data
  congruent_times <- rnorm(n_participants, mean = 500, sd = 50)
  incongruent_times <- rnorm(n_participants, 
                             mean = 500 + (100 * bias_strength), 
                             sd = 50)
  
  # Confirmation bias data
  beliefs <- runif(n_participants, 1, 7)
  evidence_type <- rep(c("supporting", "opposing"), each = n_participants)
  evidence_ratings <- c(
    beliefs + rnorm(n_participants, mean = bias_strength * 2, sd = 0.5),
    beliefs - rnorm(n_participants, mean = bias_strength * 2, sd = 0.5)
  )
  
  return(list(
    stroop = list(
      congruent = congruent_times,
      incongruent = incongruent_times
    ),
    confirmation = list(
      beliefs = beliefs,
      evidence_ratings = evidence_ratings,
      evidence_type = evidence_type
    )
  ))
}

# UI Definition ----
ui <- dashboardPage(
  dashboardHeader(title = "Stroop-Bench"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Stroop Effect", tabName = "stroop", icon = icon("clock")),
      menuItem("Confirmation Bias", tabName = "confirmation", icon = icon("check-double")),
      menuItem("Data Generation", tabName = "generate", icon = icon("random"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Home Tab
      tabItem(tabName = "home",
              fluidRow(
                box(
                  title = "Welcome to Stroop-Bench",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  p("This application helps analyze and visualize cognitive bias data."),
                  p("Choose from the following analyses:"),
                  tags$ul(
                    tags$li("Stroop Effect Analysis - Measure cognitive interference"),
                    tags$li("Confirmation Bias Analysis - Examine belief-based bias"),
                    tags$li("Data Generation - Create sample datasets for testing")
                  )
                )
              )
      ),
      
      # Stroop Effect Tab
      tabItem(tabName = "stroop",
              fluidRow(
                box(
                  title = "Data Input",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  fileInput("stroop_file", "Upload CSV file with columns: 'participant', 'condition', 'response_time'"),
                  actionButton("analyze_stroop", "Analyze Stroop Effect", class = "btn-primary")
                )
              ),
              fluidRow(
                box(
                  title = "Results",
                  status = "info",
                  width = 6,
                  verbatimTextOutput("stroop_stats")
                ),
                box(
                  title = "Visualization",
                  status = "info",
                  width = 6,
                  plotOutput("stroop_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Data Table",
                  status = "info",
                  width = 12,
                  DTOutput("stroop_table")
                )
              )
      ),
      
      # Confirmation Bias Tab
      tabItem(tabName = "confirmation",
              fluidRow(
                box(
                  title = "Data Input",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  fileInput("conf_file", "Upload CSV file with columns: 'participant', 'initial_belief', 'evidence_type', 'evidence_rating'"),
                  actionButton("analyze_conf", "Analyze Confirmation Bias", class = "btn-primary")
                )
              ),
              fluidRow(
                box(
                  title = "Results",
                  status = "info",
                  width = 6,
                  verbatimTextOutput("conf_stats")
                ),
                box(
                  title = "Visualization",
                  status = "info",
                  width = 6,
                  plotOutput("conf_plot")
                )
              ),
              fluidRow(
                box(
                  title = "Data Table",
                  status = "info",
                  width = 12,
                  DTOutput("conf_table")
                )
              )
      ),
      
      # Data Generation Tab
      tabItem(tabName = "generate",
              fluidRow(
                box(
                  title = "Generate Sample Data",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  numericInput("n_participants", "Number of participants:", 100, min = 10, max = 1000),
                  sliderInput("bias_strength", "Bias strength:", min = 0, max = 1, value = 0.5, step = 0.1),
                  actionButton("generate_data", "Generate Data", class = "btn-primary"),
                  downloadButton("download_data", "Download Data")
                )
              ),
              fluidRow(
                box(
                  title = "Preview",
                  status = "info",
                  width = 12,
                  DTOutput("generated_data_preview")
                )
              )
      )
    )
  )
)

# Server Logic ----
server <- function(input, output, session) {
  # Reactive values to store data
  values <- reactiveValues(
    stroop_data = NULL,
    conf_data = NULL,
    generated_data = NULL
  )
  
  # Stroop Effect Analysis
  observeEvent(input$stroop_file, {
    req(input$stroop_file)
    tryCatch({
      values$stroop_data <- read.csv(input$stroop_file$datapath)
      required_cols <- c("participant", "condition", "response_time")
      missing_cols <- required_cols[!required_cols %in% names(values$stroop_data)]
      if (length(missing_cols) > 0) {
        stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
      }
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
      values$stroop_data <- NULL
    })
  })
  
  observeEvent(input$analyze_stroop, {
    req(values$stroop_data)
    tryCatch({
      # Prepare data
      congruent <- values$stroop_data$response_time[values$stroop_data$condition == "congruent"]
      incongruent <- values$stroop_data$response_time[values$stroop_data$condition == "incongruent"]
      
      if(length(congruent) == 0 || length(incongruent) == 0) {
        stop("No data found for one or both conditions. Check that your condition column contains 'congruent' and 'incongruent' values.")
      }
      
      # Analyze
      results <- analyze_stroop_effect(congruent, incongruent)
      
      # Display results
      output$stroop_stats <- renderPrint({
        cat("T-test Results:\n")
        print(results$t_test)
        cat("\nEffect Size (Cohen's d):", round(results$effect_size, 3))
      })
      
      output$stroop_plot <- renderPlot({
        results$visualization
      })
      
      output$stroop_table <- renderDT({
        datatable(values$stroop_data, options = list(pageLength = 10))
      })
    }, error = function(e) {
      showNotification(paste("Error in analysis:", e$message), type = "error")
    })
  })
  
  # Confirmation Bias Analysis
  observeEvent(input$conf_file, {
    req(input$conf_file)
    tryCatch({
      values$conf_data <- read.csv(input$conf_file$datapath)
      required_cols <- c("participant", "initial_belief", "evidence_type", "evidence_rating")
      missing_cols <- required_cols[!required_cols %in% names(values$conf_data)]
      if (length(missing_cols) > 0) {
        stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
      }
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
      values$conf_data <- NULL
    })
  })
  
  observeEvent(input$analyze_conf, {
    req(values$conf_data)
    tryCatch({
      # Verify evidence types
      valid_types <- c("supporting", "opposing")
      if (!all(values$conf_data$evidence_type %in% valid_types)) {
        stop("Evidence type must be either 'supporting' or 'opposing'")
      }
      
      # Analyze
      results <- analyze_confirmation_bias(
        values$conf_data$initial_belief,
        values$conf_data$evidence_rating,
        values$conf_data$evidence_type
      )
      
      # Display results
      output$conf_stats <- renderPrint({
        cat("Bias Score:", round(results$bias_score, 3), "\n\n")
        cat("Correlation Results:\n")
        print(results$correlation)
      })
      
      output$conf_plot <- renderPlot({
        results$visualization
      })
      
      output$conf_table <- renderDT({
        datatable(values$conf_data, options = list(pageLength = 10))
      })
    }, error = function(e) {
      showNotification(paste("Error in analysis:", e$message), type = "error")
    })
  })
  
  # Data Generation
  observeEvent(input$generate_data, {
    values$generated_data <- generate_sample_data(
      input$n_participants,
      input$bias_strength
    )
    
    # Prepare data for display
    stroop_data <- data.frame(
      participant = rep(1:input$n_participants, 2),
      condition = rep(c("congruent", "incongruent"), each = input$n_participants),
      response_time = c(values$generated_data$stroop$congruent,
                        values$generated_data$stroop$incongruent)
    )
    
    conf_data <- data.frame(
      participant = rep(1:input$n_participants, 2),
      initial_belief = rep(values$generated_data$confirmation$beliefs, 2),
      evidence_type = values$generated_data$confirmation$evidence_type,
      evidence_rating = values$generated_data$confirmation$evidence_ratings
    )
    
    values$generated_data_combined <- list(
      stroop = stroop_data,
      confirmation = conf_data
    )
    
    output$generated_data_preview <- renderDT({
      datatable(
        bind_rows(
          mutate(stroop_data, type = "Stroop Effect"),
          mutate(conf_data, type = "Confirmation Bias")
        ),
        options = list(pageLength = 10)
      )
    })
  })
  
  # Download handler for generated data
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("cognitive_bias_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      req(values$generated_data_combined)
      
      # Create temporary directory
      temp_dir <- tempdir()
      
      # Write CSV files
      write.csv(values$generated_data_combined$stroop,
                file.path(temp_dir, "stroop_data.csv"), row.names = FALSE)
      write.csv(values$generated_data_combined$confirmation,
                file.path(temp_dir, "confirmation_bias_data.csv"), row.names = FALSE)
      
      # Create zip file
      zip(file, c(file.path(temp_dir, "stroop_data.csv"),
                  file.path(temp_dir, "confirmation_bias_data.csv")))
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
