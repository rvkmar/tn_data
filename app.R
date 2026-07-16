# Load required libraries

library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(DT)
library(dplyr)

# 1. DATA GENERATION BASED ON ACTUAL REPORTS

set.seed(42)
districts <- c("Ariyalur", "Chengalpattu", "Chennai", "Coimbatore", "Cuddalore",
               "Dharmapuri", "Dindigul", "Erode", "Kallakurichi", "Kanchipuram",
               "Kanniyakumari", "Karur", "Krishnagiri", "Madurai", "Mayiladuthurai",
               "Nagapattinam", "Namakkal", "Nilgiris", "Perambalur", "Pudukkottai",
               "Ramanathapuram", "Ranipet", "Salem", "Sivagangai", "Tenkasi",
               "Thanjavur", "Theni", "Thoothukudi", "Tiruchirappalli", "Tirunelveli",
               "Tirupathur", "Tiruppur", "Tiruvallur", "Tiruvannamalai", "Tiruvarur",
               "Vellore", "Viluppuram", "Virudhunagar")

grades <- c("Grade 3", "Grade 5", "Grade 8")
subjects <- c("Language", "Mathematics", "Science", "Social Science", "TWAU")
years <- c("2021-2022", "2023-2024", "2024-2025")

# Realistic base student populations for TN districts (Total Govt/Aided Enrollment approximation)

district_base_students <- c(
  "Ariyalur" = 55000, "Chengalpattu" = 140000, "Chennai" = 280000, "Coimbatore" = 160000,
  "Cuddalore" = 155000, "Dharmapuri" = 120000, "Dindigul" = 135000, "Erode" = 110000,
  "Kallakurichi" = 125000, "Kanchipuram" = 115000, "Kanniyakumari" = 90000, "Karur" = 75000,
  "Krishnagiri" = 145000, "Madurai" = 170000, "Mayiladuthurai" = 65000, "Nagapattinam" = 70000,
  "Namakkal" = 95000, "Nilgiris" = 40000, "Perambalur" = 45000, "Pudukkottai" = 130000,
  "Ramanathapuram" = 110000, "Ranipet" = 90000, "Salem" = 190000, "Sivagangai" = 85000,
  "Tenkasi" = 105000, "Thanjavur" = 140000, "Theni" = 80000, "Thoothukudi" = 115000,
  "Tiruchirappalli" = 160000, "Tirunelveli" = 120000, "Tirupathur" = 85000, "Tiruppur" = 130000,
  "Tiruvallur" = 175000, "Tiruvannamalai" = 180000, "Tiruvarur" = 95000, "Vellore" = 125000,
  "Viluppuram" = 195000, "Virudhunagar" = 125000
)

# Function to get the baseline average based on the NAS 2021 / PARAKH 2024 / SLAS 2025 reports

get_baseline_score <- function(grade, subject, year) {
  year_modifier <- ifelse(year == "2024-2025", 2, ifelse(year == "2023-2024", 0, -2))
  
  if (grade == "Grade 3") {
    if (subject == "Language") return(67.7 + year_modifier)
    if (subject == "Mathematics") return(54.23 + year_modifier)
    return(NA)
  } else if (grade == "Grade 5") {
    if (subject == "Language") return(76.14 + year_modifier)
    if (subject == "Mathematics") return(57.07 + year_modifier)
    if (subject == "TWAU") return(57.17 + year_modifier)
    return(NA)
  } else if (grade == "Grade 8") {
    if (subject == "Language") return(51.9 + year_modifier)
    if (subject == "Mathematics") return(38.69 + year_modifier)
    if (subject == "Science") return(37.56 + year_modifier)
    if (subject == "Social Science") return(54.63 + year_modifier)
    return(NA)
  }
  return(NA)
}

# Create a massive empty dataset to hold all permutations

education_data <- data.frame()

# Generate pseudo-realistic mock data respecting cohort logic

for(y in years) {
  for(g in grades) {
    for(s in subjects) {
      
      base_score <- get_baseline_score(g, s, y)
      
      # Skip if subject isn't taught/tested in that grade
      if(is.na(base_score)) next 
      
      # Realistic Samagra Shiksha grant per student averages (reduced in 2024 due to fund withholding)
      base_grant_per_student <- if(y == "2024-2025") 4500 else 8500
      
      # Estimate roughly 10% of district's total enrollment falls into a single grade cohort
      base_students <- unname(district_base_students)
      student_counts <- round(base_students * rnorm(38, mean = 0.10, sd = 0.01))
      
      # Total grants released for this cohort based on EMIS estimations
      grants_released <- student_counts * rnorm(38, mean = base_grant_per_student, sd = 800)
      per_student_exp <- grants_released / student_counts
      
      # Generate district scores centered around the state average with realistic standard deviation
      # Adding a slight correlation to expenditure to simulate real-world (though often weak) links
      exp_influence <- (per_student_exp - base_grant_per_student) / 1000 
      district_scores <- rnorm(38, mean = base_score + exp_influence, sd = 5)
      
      # Ensure scores stay between 0 and 100
      district_scores <- pmax(0, pmin(100, district_scores))
      
      temp_df <- data.frame(
        District = districts,
        Year = as.factor(y),
        Grade = g,
        Subject = s,
        Student_Count = student_counts,
        Grants_Released = round(grants_released),
        Per_Student_Expenditure = per_student_exp,
        Learning_Outcome = district_scores
      )
      
      education_data <- rbind(education_data, temp_df)
    }
    
    
  }
}

# 3. UI DEFINITION

ui <- page_sidebar(
  title = "Tamil Nadu: Expenditure vs Learning Outcomes",
  theme = bs_theme(version = 5, preset = "litera"),
  
  sidebar = sidebar(
    title = "Estimate(s)",
    
    checkboxGroupInput(
      "year", 
      "Academic Year(s):", 
      choices = levels(education_data$Year), 
      selected = NULL,
      inline = TRUE
    ),
    
    selectInput(
      "grade", 
      "PARAKH Grade(s):", 
      choices = grades, 
      selected = NULL,
      multiple = FALSE
    ),
    
    checkboxGroupInput(
      "subject", 
      "Subject(s):", 
      choices = subjects, 
      selected = NULL,
      inline = TRUE
    ),
    
    selectInput("districts", "Select Districts:", 
                choices = districts, 
                selected = districts, 
                multiple = TRUE),
    
    hr(),
    checkboxGroupInput(
      "display_options", 
      "Display Elements:",
      choices = c("Scatter Points" = "points", "Trend Lines" = "trend"),
      selected = c("points", "trend")
    ),
    
    hr(),
    checkboxInput("show_table", "Show Data Explorer Table", value = TRUE),
    helpText("Note: 2024 expenditure data reflects the withholding of central Samagra Shiksha funds.")
    
    
  ),
  
  # Main Layout
  
  layout_columns(
    col_widths = c(8, 4),
    card(
      full_screen = TRUE,
      card_header("Correlation: Expenditure vs Outcomes"),
      plotlyOutput("scatterPlot")
    ),
    card(
      full_screen = TRUE,
      card_header("State Average Trend Over Time"),
      plotlyOutput("trendPlot")
    )
  ),
  
  # Bottom Row: Data Table (Hideable)
  
  conditionalPanel(
    condition = "input.show_table == true",
    card(
      full_screen = TRUE,
      card_header("District-wise Data Explorer"),
      DTOutput("dataTable")
    )
  )
)

# 4. SERVER LOGIC

server <- function(input, output, session) {
  
  # Dynamic Subject Mapping
  
  grade_subject_map <- list(
    "Grade 3" = c("Language", "Mathematics"),
    "Grade 5" = c("Language", "Mathematics", "TWAU"),
    "Grade 8" = c("Language", "Mathematics", "Science", "Social Science")
  )
  
  observeEvent(input$grade, {
    if(length(input$grade) == 0) {
      updateCheckboxGroupInput(session, "subject", choices = subjects, selected = NULL)
      return()
    }
    
    valid_subjects <- unique(unlist(grade_subject_map[input$grade]))
    current_selection <- input$subject
    
    updateCheckboxGroupInput(session, "subject", 
                             choices = valid_subjects, 
                             selected = intersect(current_selection, valid_subjects),
                             inline = TRUE)
    
    
  }, ignoreNULL = FALSE)
  
  # Core data filter
  
  filtered_data <- reactive({
    req(input$districts, input$year, input$grade, input$subject)
    
    education_data %>%
      filter(District %in% input$districts,
             Year %in% input$year,
             Grade %in% input$grade,
             Subject %in% input$subject)
    
    
  })
  
  # MATHEMATICAL AGGREGATION PIPELINE
  
  aggregate_metrics <- function(data) {
    if(nrow(data) == 0) return(data.frame())
    
    data %>%
      # Step 1: Average across subjects (Student counts are identical across subjects within the same grade cohort)
      group_by(District, Year, Grade) %>%
      summarise(
        Student_Count = max(Student_Count, na.rm = TRUE),
        Grants_Released = max(Grants_Released, na.rm = TRUE),
        Learning_Outcome = mean(Learning_Outcome, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      # Step 2: Sum across grades & calculate weighted averages for outcomes
      group_by(District, Year) %>%
      summarise(
        Student_Count = sum(Student_Count, na.rm = TRUE),
        Grants_Released = sum(Grants_Released, na.rm = TRUE),
        Per_Student_Expenditure = sum(Grants_Released, na.rm = TRUE) / sum(Student_Count, na.rm = TRUE),
        Learning_Outcome = weighted.mean(Learning_Outcome, Student_Count, na.rm = TRUE),
        .groups = "drop"
      )
    
    
  }
  
  # Scatter Plot rendering
  
  output$scatterPlot <- renderPlotly({
    data <- filtered_data()
    if(nrow(data) == 0) return(NULL)
    
    agg_data <- aggregate_metrics(data)
    
    p <- ggplot(agg_data, aes(x = Per_Student_Expenditure, y = Learning_Outcome, color = Year)) +
      labs(x = "Per Student Expenditure (INR)", y = "Learning Outcome (%)") +
      theme_minimal() +
      scale_color_brewer(palette = "Set1")
    
    if("points" %in% input$display_options) {
      p <- p + geom_point(aes(text = District), alpha = 0.7, size = 3)
    }
    
    if("trend" %in% input$display_options) {
      p <- p + geom_smooth(aes(group = 1), color = "black", method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.8)
    }
    
    ggplotly(p, tooltip = c("text", "x", "y", "color"))
    
    
  })
  
  # Trend Line Plot rendering
  
  output$trendPlot <- renderPlotly({
    req(input$districts, input$grade, input$subject)
    
    # Needs all years for the trend line, regardless of year filter
    trend_raw <- education_data %>%
      filter(District %in% input$districts,
             Grade %in% input$grade,
             Subject %in% input$subject)
    
    if(nrow(trend_raw) == 0) return(NULL)
    
    state_avg <- aggregate_metrics(trend_raw) %>%
      # Final rollup to state average across all filtered districts
      group_by(Year) %>%
      summarise(
        Learning_Outcome = weighted.mean(Learning_Outcome, Student_Count, na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- ggplot(state_avg, aes(x = Year, y = Learning_Outcome, group = 1)) +
      geom_line(color = "steelblue", linewidth = 1.5) +
      geom_point(color = "darkblue", size = 4) +
      labs(x = "Academic Year", y = "Average Learning Outcome (%)") +
      theme_minimal() +
      ylim(0, 100) 
    
    ggplotly(p)
    
    
  })
  
  # Data Table rendering
  
  output$dataTable <- renderDT({
    data <- filtered_data()
    if(nrow(data) == 0) return(NULL)
    
    table_agg <- aggregate_metrics(data)
    
    table_agg$Per_Student_Expenditure <- round(table_agg$Per_Student_Expenditure, 2)
    table_agg$Learning_Outcome <- round(table_agg$Learning_Outcome, 2)
    
    dt <- datatable(table_agg, 
                    options = list(pageLength = 10, scrollX = TRUE), 
                    rownames = FALSE,
                    colnames = c("District", "Year", "Student Count", "Grants Released (₹)", "Expenditure/Student (₹)", "Learning Outcome (%)"))
    
    dt <- formatCurrency(dt, columns = c('Grants_Released', 'Per_Student_Expenditure'), currency = '₹', digits = 0)
    dt <- formatRound(dt, columns = 'Student_Count', digits = 0)
    
    dt
    
    
  })
}

# Run the application

shinyApp(ui = ui, server = server)