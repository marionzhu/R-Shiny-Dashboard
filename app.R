library(tidyverse)       
library(shiny)
library(shinydashboard)
library(DT)
library(lubridate)
library(stringi)
library(reactable)
library(ggthemes)
library(shinyWidgets)
library(plotly)
library(bs4Dash)

load('jobs.RData')

ui <- dashboardPage(
  skin = 'blue', 
  dashboardHeader(title = "Jobs & Salary in Data Field",titleWidth = 350,
                  tags$li(class = "dropdown",
                          tags$a(href="https://www.linkedin.com/in/zhusijie/", 
                                 target="_blank", 
                                 icon("linkedin", "fa-1x", lib = "font-awesome")  # fa-3x mean 3 times bigger
                          )
                  ),
                  tags$li(class = "dropdown",
                          tags$a(href="https://app.datacamp.com/learn/courses/intro-to-python-for-data-science", 
                                 target="_blank", 
                                 icon("facebook", "fa-1x", lib = "font-awesome")  # fa-3x mean 3 times bigger
                          )
                  ),
                  tags$li(class = "dropdown",
                          tags$a(href="https://github.com/marionzhu", 
                                 target="_blank", 
                                 icon("github", "fa-1x", lib = "font-awesome")  # fa-3x mean 3 times bigger
                          )
                  ),
                  tags$li(class = "dropdown", column(12))), 
  
  dashboardSidebar(
    checkboxGroupInput("work_year", label = h4("Years"), 
                       choices = jobs$work_year|> unique(),
                       selected = jobs$work_year|> unique()
    ),
    pickerInput(
      inputId = "employee_residence",
      label = h4("Country picker"),
      choices = jobs$employee_residence |> unique(),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `deselect-all-text` = "None...",
        `select-all-text` = "Yeah, all !",
        `none-selected-text` = "zero"
      ),
      selected = jobs$employee_residence |> unique()
    ),
    selectInput("experience_level", h4("Experience Level"),
                choices = jobs$experience_level |> unique(),
                selected = c("Senior", "Mid-level"),
                multiple = TRUE
    ),
    selectInput("job_category", h4("Job Category"),
                choices = jobs$job_category |> unique(),
                selected = c("Data Science and Research", "Data Engineering", "Data Analysis", "Machine Learning and AI"),
                multiple = TRUE
    )),
  dashboardBody(
    tabBox(
      title = "", height = "920px", width = 12,
      tabPanel("Number of jobs", 
               plotOutput("plot1", height = 600)
      ),
      tabPanel("Average Salary of jobs", 
               plotOutput("plot2", height = 600)
      ),
      tabPanel("Table",
               DT::dataTableOutput("table")
      ),
      tabPanel("Details",
               fluidRow(
                 column(6,plotlyOutput("plot3")),
                 column(6,plotlyOutput("plot4"))
               )
      )
    ) )            # Body zone
)
server <- function(input, output){
  react_data <- reactive({
    jobs |> 
      filter(work_year %in% input$work_year,
             employee_residence %in% input$employee_residence,
             experience_level %in% input$experience_level,
             job_category %in% input$job_category)
    
  })
  output$table <- renderDataTable(react_data())
  
  output$plot1 <- renderPlot({
    react_data() |>
      group_by(job_title) |>
      summarise(nb_jobs = n()) |>
      arrange(desc(nb_jobs)) |>
      slice(1:15) |>
      ggplot(aes(x= nb_jobs, y=reorder(job_title, nb_jobs), fill=nb_jobs)) + geom_col() + 
      theme_classic() + ylab("") +xlab("")+ 
      scale_fill_viridis_c(option = "diff", begin=0.3, end = 0.95, direction = -1) + ggtitle("Number of Jobs")+
      theme(plot.title = element_text(face = "bold", size = 16), 
            axis.title.x = element_text(face = "bold")) +
      geom_text(aes(x=nb_jobs, label = nb_jobs), hjust=1.4, color="white", fontface = 3)
  })
  
  output$plot2 <- renderPlot({
    react_data()|>
      group_by(job_title) |>
      summarise(avg_salary = round(mean(salary_in_usd))) |>
      arrange(desc(avg_salary)) |>
      slice(1:15) |>
      ggplot(aes(x= avg_salary, y=reorder(job_title, avg_salary), fill=avg_salary)) + geom_col() + 
      theme_classic() + ylab("") +xlab("")+ 
      scale_fill_viridis_c(option = "diff", begin=0.3, end = 0.95, direction = -1) + ggtitle("Average salary of Jobs")+
      theme(plot.title = element_text(face = "bold", size = 16), 
            axis.title.x = element_text(face = "bold")) +
      geom_text(aes(x=avg_salary, label = avg_salary), hjust=1.4, color="white", fontface = 3)
  })
  
  output$plot3 <- renderPlotly({
    b <- jobs |>
      group_by(company_size) |> 
      summarise(nb_company = n())
    fig <- plot_ly(b, labels = b$company_size, values = b$nb_company, type = 'pie')
    fig <- fig |> layout(title = 'Company Size Percentage',
                         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  output$plot4 <- renderPlotly({
    a <- jobs |>
      group_by(experience_level) |> 
      summarise(nb_jobs = n())
    fig <- plot_ly(a, labels = a$experience_level, values = a$nb_jobs, type = 'pie')
    fig <- fig |> layout(title = 'Experience Level Percentage',
                         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)