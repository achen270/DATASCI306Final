# app.R

library(shiny)
library(dplyr)
library(ggplot2)
library(broom)

#--------------------------------------------------
# 0. Load your data
#--------------------------------------------------

obj_name <- load("NHANES_Medical_Conditions_Pre_Post_Covid.RData")
nhanes <- get(obj_name)  # or set nhanes <- your_data_frame_name explicitly

#--------------------------------------------------
# 0.1 Define condition variables and labels
#--------------------------------------------------

condition_vars <- c(
  "asthma",
  "hay_fever_past_yr",
  "anemia_past_3_mon",
  "arthritis",
  "congestive_heart_failure",
  "coronary_heart_disease",
  "angina",
  "heart_attack",
  "stroke",
  "thyroid_prob",
  "COPD_Emph_ChB",
  "liver_cond",
  "gallstones"
)

condition_labels <- c(
  "Asthma",
  "Hay fever (past year)",
  "Anemia (past 3 months)",
  "Arthritis",
  "Congestive heart failure",
  "Coronary heart disease",
  "Angina",
  "Heart attack",
  "Stroke",
  "Thyroid problem",
  "COPD/Emphysema/Chronic bronchitis",
  "Liver condition",
  "Gallstones"
)

# user sees label, app gets variable name
condition_choices <- setNames(condition_vars, condition_labels)

#--------------------------------------------------
# 0.2 Recode chronic condition variables to 0/1
#--------------------------------------------------

nhanes <- nhanes |>
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(condition_vars),
      ~ dplyr::case_when(
        . %in% c(1, "Yes")       ~ 1,
        . %in% c(0, 2, "No")     ~ 0,
        TRUE                     ~ NA_real_
      )
    )
  )

#--------------------------------------------------
# 1. UI
#--------------------------------------------------

ui <- fluidPage(
  
  titlePanel("NHANES Chronic Conditions Pre vs Post COVID"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "condition",
        label   = "Chronic condition:",
        choices = condition_choices,
        selected = "asthma"
      ),
      
      sliderInput(
        inputId = "age_range",
        label   = "Age range (years):",
        min     = min(nhanes$Age_yr, na.rm = TRUE),
        max     = max(nhanes$Age_yr, na.rm = TRUE),
        value   = c(18, max(nhanes$Age_yr, na.rm = TRUE))
      ),
      
      selectInput(
        inputId = "gender",
        label   = "Gender:",
        #choices = c("All", sort(unique(nhanes$Gender))),
        choices = setNames(c("All", 1, 2), c("All", "Male", "Female")),
        selected = "All"
      ),
      
      selectInput(
        inputId = "race",
        label   = "Race:",
        choices = c(
          "All"               = "All",
          "Other Hispanic"    = "1",
          "Non-Hispanic White"= "2",
          "Non-Hispanic Black"= "3",
          "Non-Hispanic Asian"= "4",
          "Other"             = "5"
        ),
        selected = "All"
      )
      ,
      
      checkboxInput(
        inputId = "show_reg",
        label   = "Show logistic regression results",
        value   = TRUE
      ),
      
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Prevalence",
          br(),
          plotOutput("prev_plot"),
          br(),
          tableOutput("prev_table")
        ),
        tabPanel(
          "Proportion Test",
          br(),
          h4("Two-sample test of proportions: Pre vs Post COVID"),
          tableOutput("prop_table")
        ),
        tabPanel(
          "Chronic burden",
          br(),
          plotOutput("burden_plot")
        ),
        tabPanel(
          "Regression",
          br(),
          verbatimTextOutput("reg_text"),
          br(),
          tableOutput("reg_table")
        ),
        tabPanel(
          "Data",
          br(),
          dataTableOutput("data_table")
        )
      )
    )
  )
)

#--------------------------------------------------
# 2. SERVER
#--------------------------------------------------

server <- function(input, output, session) {
  
  #------------------------------
  # 2.1 Reactive filtered data
  #------------------------------
  filtered_data <- reactive({
    req(input$age_range, input$race, input$gender, input$condition)
    
    df <- nhanes
    
    # Age filter
    df <- df |>
      dplyr::filter(
        Age_yr >= input$age_range[1],
        Age_yr <= input$age_range[2]
      )
    
    # Gender filter
    if (!is.null(input$gender) && input$gender != "All") {
      df <- df |>
        dplyr::filter(Gender == input$gender)
    }
    
    # Race filter
    if (!is.null(input$race) && input$race != "All") {
      df <- df |>
        dplyr::filter(Race == input$race)
    }
    
    # Make sure selected condition is not NA
    df <- df |>
      dplyr::filter(!is.na(.data[[input$condition]]))
    
    df
  })
  
  #------------------------------
  # 2.2 Add chronic burden score
  #------------------------------
  data_with_burden <- reactive({
    df <- filtered_data()
    
    df |>
      dplyr::mutate(
        chronic_count = rowSums(
          dplyr::across(all_of(condition_vars)),
          na.rm = TRUE
        )
      )
  })
  
  #------------------------------
  # 2.3 Prevalence summary + plot
  #------------------------------
  prev_summary <- reactive({
    df <- data_with_burden()
    
    df |>
      dplyr::group_by(post_covid) |>
      dplyr::summarize(
        n          = dplyr::n(),
        prevalence = mean(.data[[input$condition]], na.rm = TRUE),
        .groups    = "drop"
      ) |>
      dplyr::mutate(
        period = ifelse(post_covid == 0, "Pre-COVID", "Post-COVID"),
        prevalence_pct = 100 * prevalence
      )
  })
  
  output$prev_table <- renderTable({
    prev_summary() |>
      dplyr::select(period, n, prevalence_pct) |>
      dplyr::rename(
        Period           = period,
        N                = n,
        `Prevalence (%)` = prevalence_pct
      )
  })
  
  output$prev_plot <- renderPlot({
    ps <- prev_summary()
    
    # get pretty label for title
    cond_label <- condition_labels[match(input$condition, condition_vars)]
    
    ggplot(ps, aes(x = period, y = prevalence_pct)) +
      geom_col() +
      labs(
        x = "Period",
        y = "Prevalence (%)",
        title = paste0(
          "Pre vs Post COVID Prevalence: ",
          cond_label
        )
      ) +
      ylim(0, max(ps$prevalence_pct, na.rm = TRUE) * 1.1)
  })
  
  output$prop_table <- renderTable({
    pt <- prop_test()
    
    # Extract readable summary
    data.frame(
      Group            = c("Pre-COVID", "Post-COVID"),
      Proportion       = round(pt$estimate, 4),
      `95% CI Lower`   = round(pt$conf.int[1], 4),
      `95% CI Upper`   = round(pt$conf.int[2], 4),
      `p-value`        = round(pt$p.value, 4)
    )
  })
  
  #------------------------------
  # 2.4 Chronic burden plot
  #------------------------------
  output$burden_plot <- renderPlot({
    df <- data_with_burden()
    
    ggplot(df, aes(x = chronic_count, fill = factor(post_covid))) +
      geom_histogram(position = "identity", alpha = 0.5, bins = 10) +
      scale_fill_discrete(
        name = "Period",
        labels = c("0" = "Pre-COVID", "1" = "Post-COVID")
      ) +
      labs(
        x = "Number of chronic conditions",
        y = "Count",
        title = "Distribution of chronic condition burden\nPre vs Post COVID"
      )
  })
  
  #------------------------------
  # 2.5 Logistic regression
  #------------------------------
  reg_model <- reactive({
    if (!isTRUE(input$show_reg)) return(NULL)
    req(input$condition)
    
    df <- filtered_data()
    req(nrow(df) > 0)
    
    formula_str <- paste(input$condition, "~ post_covid + Age_yr + Gender + Race")
    glm(stats::as.formula(formula_str), family = binomial, data = df)
  })
  
  reg_tidy <- reactive({
    m <- reg_model()
    if (is.null(m)) return(NULL)
    broom::tidy(m, conf.int = TRUE, exponentiate = TRUE)
  })
  
  output$reg_text <- renderPrint({
    if (!isTRUE(input$show_reg)) {
      cat("Regression results are hidden. Check the box in the sidebar to view them.")
    } else {
      cat("Logistic regression model:\n")
      cat(paste(input$condition, "~ post_covid + Age_yr + Gender + Race\n\n"))
      cat("Odds ratios (OR) with 95% confidence intervals are shown in the table below.\n")
    }
  })
  
  output$reg_table <- renderTable({
    if (!isTRUE(input$show_reg)) return(NULL)
    
    reg_tidy() |>
      dplyr::rename(
        term        = term,
        OR          = estimate,
        `Lower 95%` = conf.low,
        `Upper 95%` = conf.high,
        `p-value`   = p.value
      )
  })
  
  #------------------------------
  # 2.6 Data table
  #------------------------------
  output$data_table <- renderDataTable({
    data_with_burden()
  })
  #------------------------------
  # 2.7 Proportion test (Pre vs Post)
  #------------------------------
  prop_test <- reactive({
    df <- filtered_data()
    
    # Count successes and totals
    tab <- df |>
      group_by(post_covid) |>
      summarize(
        total = n(),
        positives = sum(.data[[input$condition]] == 1, na.rm = TRUE),
        .groups = "drop"
      )
    
    # Must have both groups
    req(nrow(tab) == 2)
    
    # Extract counts
    x <- tab$positives      # number of cases
    n <- tab$total          # sample size
    
    # Run 2-sample test for equality of proportions
    stats::prop.test(x = x, n = n, correct = FALSE)
  })
}




#--------------------------------------------------
# 3. Run the app
#--------------------------------------------------

shinyApp(ui = ui, server = server)
