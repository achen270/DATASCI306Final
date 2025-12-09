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
  "gallstones",
  "cancer"
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
  "Liver Conditions",
  "Gallstones",
  "Cancer"
)

# user sees label, app gets variable name
condition_choices <- setNames(condition_vars, condition_labels)

#--------------------------------------------------
# 0.2 Recode chronic condition variables to 0/1
#--------------------------------------------------

nhanes <- nhanes |>
  mutate(
    across(
      all_of(condition_vars),
      ~ case_when(
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
        label   = "Age Range (years):",
        min     = min(nhanes$Age_yr, na.rm = TRUE),
        max     = max(nhanes$Age_yr, na.rm = TRUE),
        value   = c(18, max(nhanes$Age_yr, na.rm = TRUE))
      ),
      
      checkboxGroupInput(
        inputId = "gender",
        label   = "Gender:",
        #choices = c("All", sort(unique(nhanes$Gender))),
        choices = setNames(c(1, 2), c("Male", "Female")),
        selected = c(1,2)
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
        label   = "Show Logistic Regression Results",
        value   = TRUE
      ),
      
      radioButtons(
        "hist_mode",
        "Histogram Display:",
        choices = c("Overlap" = "overlap", "Side-by-side" = "facet"),
        selected = "overlap"
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
      filter(
        Age_yr >= input$age_range[1],
        Age_yr <= input$age_range[2]
      )
    
    # Gender filter
    if (!is.null(input$gender) && length(input$gender) > 0) {
      df <- df |>
        filter(Gender == input$gender)
    }
    
    # Race filter
    if (!is.null(input$race) && input$race != "All") {
      df <- df |>
        filter(Race == input$race)
    }
    
    # Make sure selected condition is not NA
    df <- df |>
      filter(!is.na(.data[[input$condition]]))
    
    df
  })
  
  #------------------------------
  # 2.2 Add chronic burden score
  #------------------------------
  data_with_burden <- reactive({
    df <- filtered_data()
    
    df |>
      mutate(
        chronic_count = rowSums(
          across(all_of(condition_vars)),
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
      group_by(post_covid) |>
      summarize(
        n          = n(),
        prevalence = mean(.data[[input$condition]], na.rm = TRUE),
        .groups    = "drop"
      ) |>
      mutate(
        period = ifelse(post_covid == 0, "Pre-COVID", "Post-COVID"),
        prevalence_pct = 100 * prevalence
      )
  })
  
  output$prev_table <- renderTable({
    prev_summary() |>
      select(period, n, prevalence_pct) |>
      rename(
        Period           = period,
        N                = n,
        `Prevalence (%)` = prevalence_pct
      )
  })
  
  output$prev_plot <- renderPlot({
    ps <- prev_summary()
    
    # get pretty label for title
    cond_label <- condition_labels[match(input$condition, condition_vars)]
    
    ggplot(ps, aes(x = period, y = prevalence_pct, fill = period)) +
      geom_col(alpha = 0.5) +
      
      scale_fill_manual(
        values = c("Pre-COVID" = "salmon", "Post-COVID" = "cyan"),
        labels = c("FALSE" = "Pre-COVID", "TRUE" = "Post-COVID"))  +
        
      labs(
        x = "Time Frame",
        y = "Prevalence (%)",
        title = paste0(
          "Pre vs Post COVID Prevalence: ",
          cond_label
        )
      ) +
      ylim(0, max(ps$prevalence_pct, na.rm = TRUE) * 1.1) + theme_minimal()
  })
  

  
  #------------------------------
  # 2.4 Chronic burden plot
  #------------------------------
  output$burden_plot <- renderPlot({
    df <- data_with_burden()
    
    if (input$hist_mode == "overlap") {
      ggplot(df, aes(x = chronic_count, fill = factor(post_covid))) +
        geom_histogram(position = "identity", alpha = 0.5, bins = 10) +
        
        scale_fill_manual(
        name = "Time Frame",
        values = c("FALSE" = "salmon", "TRUE" = "cyan"),
        labels = c("FALSE" = "Pre-COVID", "TRUE" = "Post-COVID")) +
        
        labs(
          x = "Number of Chronic Conditions Per Person",
          y = "Count",
          title = "Distribution of Chronic Condition Burden\nPre vs Post COVID"
        ) + theme_minimal()
    }
      
    else {
    ggplot(df, aes(x = chronic_count, fill = factor(post_covid))) +
      geom_histogram(position = "identity", alpha = 0.5, bins = 10) +
      facet_wrap(~ post_covid, labeller = as_labeller(
        c("FALSE" = "Pre-COVID", "TRUE" = "Post-COVID")
      )) + guides(fill = "none") + 

      labs(
        x = "Number of Chronic Conditions Per Person",
        y = "Count",
        title = "Distribution of Chronic Condition Burden\nPre vs Post COVID"
       ) + theme_minimal()
    }
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
      rename(
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
  
  filter_for_df <- reactive({
    req(input$age_range, input$race, input$gender)
    
    df1 <- nhanes
    
    # Age filter
    df1 <- df1 |>
      filter(
        Age_yr >= input$age_range[1],
        Age_yr <= input$age_range[2]
      )
    

    # Gender filter
    if (!is.null(input$gender) && length(input$gender) > 0) {
      df1 <- df1 |>
        filter(Gender == input$gender)
    }
    
    # Race filter
    if (!is.null(input$race) && input$race != "All") {
      df1 <- df1 |>
        filter(Race == input$race)
    }
    
    df1
  })
  
  output$data_table <- renderDataTable({
    prop_df <- filter_for_df() |>
      mutate(across(all_of(condition_vars), ~ if_else(.x == 1, 1, 0))) |>
      pivot_longer(cols = all_of(condition_vars), 
                   names_to = "disease", 
                   values_to = "has_condition") |>
      group_by(post_covid, disease) |>
      summarise(proportion = mean(has_condition, na.rm = TRUE), .groups = "drop") |> 
      pivot_wider(names_from = post_covid, values_from = proportion) |> select(-any_of('NA')) |> 
      rename(pre_covid = "FALSE",
             post_covid = "TRUE") |> 
      mutate('difference (pre - post)' =  pre_covid - post_covid,
             disease = factor(disease, 
                              levels = condition_vars, 
                              labels = condition_labels))
    })
  #------------------------------
  # 2.7 Proportion test (Pre vs Post)
  #------------------------------
  prop_test <- reactive({
    df <- filtered_data()
    
    # Count successes and totals
    x_pre <- df |> filter(post_covid == FALSE, 
                          .data[[input$condition]] == 1) |> nrow()
    x_post <- df |> filter(post_covid == TRUE, 
                           .data[[input$condition]] == 1) |> nrow()
    n_pre <- df |> filter(post_covid == FALSE) |> nrow()
    n_post <- df |> filter(post_covid == TRUE) |> nrow()
    
    req(n_pre > 0, n_post > 0)
    
    # Run 2-sample test for equality of proportions
    prop.test(x = c(x_pre, x_post), n = c(n_pre, n_post), correct = FALSE)
  })
  
  output$prop_table <- renderTable({
    pt <- prop_test()
    
    pre_prop  <- pt$estimate[1]
    post_prop <- pt$estimate[2]
    
    df <- filtered_data()
    n_pre <- df |> filter(post_covid == FALSE) |> nrow()
    n_post <- df |> filter(post_covid == TRUE) |> nrow()
    
    data.frame(
      `Pre-COVID proportion`    = round(pre_prop, 4),
      'Number of Pre-COVID' = n_pre,
      `Post-COVID proportion`   = round(post_prop, 4),
      'Number of Pre-COVID' = n_post,
      `Difference (pre - post)` = round(post_prop - pre_prop, 4),
      `95% CI Lower`            = round(pt$conf.int[1], 4),
      `95% CI Upper`            = round(pt$conf.int[2], 4),
      `p-value`                 = round(pt$p.value, 4)
    )
  })
}




#--------------------------------------------------
# 3. Run the app
#--------------------------------------------------

shinyApp(ui = ui, server = server)
