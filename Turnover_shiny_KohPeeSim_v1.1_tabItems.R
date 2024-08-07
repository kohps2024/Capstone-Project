#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(readr)
library(shiny)

turnover <- read.csv("turnover_balance.csv")
turnover <-turnover %>% 
  mutate(satisfaction_level1 = floor((satisfaction_level-0.01)/0.1)+1,
         last_evaluation1 = floor((last_evaluation-0.01)/0.1)+1)
categorical_var <- c( "satisfaction_level1", "last_evaluation1",  "Work_accident",
                      "promotion_last_5years", "division",
                      "salary", "left")

turnover <-  turnover %>% mutate_at(categorical_var, factor)
turnover$salary<-factor(turnover$salary,
                        levels = c("low", "medium", "high")) 
# point chart list
select_list1 <- list("Current Staff" = "0",
                    "Resign Staff" = "1",
                    "Both" = "2")

selectb_list1 <- list("0" = "Current Staff",
                     "1" = "Resign Staff" ,
                     "2" = "Both"  )

x_list1 <- list( "Satisfaction Level" = "satisfaction_level",
                "Average Hrs Per Month" = "average_montly_hours",
                "Years of Service" ="time_spend_company",
                "Last Evaluation" = "last_evaluation",
                "Division" = "division",
                "Number of Project" = "number_project",
                "Years of Service" = "time_spend_company")

xb_list1 <- list("satisfaction_level" = "Satisfaction Level" ,
                "average_montly_hours" = "Average Hrs Per Month" ,
                "last_evaluation" = "Last Evaluation"  ,
                "division" = "Division",
                "number_project" = "Number of Project" ,
                "time_spend_company" = "Years of Service")

y_list1 <- list( "Satisfaction Level" = "satisfaction_level",
                "Last Evaluation" = "last_evaluation",
                "Average Hrs Per Month" = "average_montly_hours",
                "Division" = "division",
                "Number of Project" = "number_project",
                "Years of Service" = "time_spend_company")

yb_list1 <- list("satisfaction_level" = "Satisfaction Level" ,
                "last_evaluation" = "Last Evaluation"  ,
                "average_montly_hours" = "Average Hrs Per Month",
                "division" = "Division",
                "number_project" = "Number of Project" ,
                "time_spend_company" = "Years of Service")

color_list1 <- list("Work Accident" = "Work_accident",
                   "Promotion" = "promotion_last_5years",
                   "Resignation Status" = "left",
                   "Division" = "division",
                   "Income Level" ="salary")

colorb_list1 <- list("Work_accident" = "Work Accident",
                    "promotion_last_5years" = "Promotion",
                    "left" = "Resignation Status" ,
                    "division" = "Division",
                    "salary" = "Income Level" )

# Histogram list
select_list2 <- list("Current Staff" = "0",
                    "Resign Staff" = "1",
                    "Both" = "2")

selectb_list2 <- list("0" = "Current Staff",
                     "1" = "Resign Staff" ,
                     "2" = "Both"  )

x_list2 <- list("Work Accident" = "Work_accident",
               "Satisfaction Level" = "satisfaction_level1",
               "Last Evaluation" = "last_evaluation1",
               "Number of Project" = "number_project",
               "Promotion" = "promotion_last_5years",
               "Division" = "division",
               "Years of Service" = "time_spend_company",
               "Income Level" ="salary")

xb_list2 <- list("Work_accident" = "Work Accident",
                "satisfaction_level1" = "Satisfaction Level" ,
                "last_evaluation1" = "Last Evaluation"  ,
                "number_project" = "Number of Project" ,
                "promotion_last_5years" = "Promotion",
                "division" = "Division",
                "time_spend_company" = "Years of Service" ,
                "salary" = "Income Level" )

fill_list2 <- list("Work Accident" = "Work_accident",
                  "Satisfaction Level" = "satisfaction_level1",
                  "Last Evaluation" = "last_evaluation1",
                  "Number of Project" = "number_project",
                  "Promotion" = "promotion_last_5years",
                  "Division" = "division",
                  "Years of Service" = "time_spend_company",
                  "Income Level" ="salary")

fillb_list2 <- list("Work_accident" = "Work Accident",
                   "satisfaction_level1" = "Satisfaction Level" ,
                   "last_evaluation1" = "Last Evaluation"  ,
                   "number_project" = "Number of Project" ,
                   "promotion_last_5years" = "Promotion",
                   "division" = "Division",
                   "time_spend_company" = "Years of Service" ,
                   "salary" = "Income Level" )

facet_list2 <- list("Work Accident" = "Work_accident",
                   "Satisfaction Level" = "satisfaction_level1",
                   "Last Evaluation" = "last_evaluation1",
                   "Number of Project" = "number_project",
                   "Promotion" = "promotion_last_5years",
                   "Division" = "division",
                   "Years of Service" = "time_spend_company",
                   "Income Level" ="salary")

facetb_list2 <- list("Work_accident" = "Work Accident",
                    "satisfaction_level1" = "Satisfaction Level" ,
                    "last_evaluation1" = "Last Evaluation"  ,
                    "number_project" = "Number of Project" ,
                    "promotion_last_5years" = "Promotion",
                    "division" = "Division",
                    "time_spend_company" = "Years of Service" ,
                    "salary" = "Income Level" )
# Boxplot list
select_list3 <- list("Current Staff" = "0",
                    "Resign Staff" = "1",
                    "Both" = "2")

selectb_list3 <- list("0" = "Current Staff",
                     "1" = "Resign Staff" ,
                     "2" = "Both"  )

y_list3 <- list("Employee Satisfaction Level" = "satisfaction_level",
               "Employee Evaluation Level" = "last_evaluation",
               "Average Hrs Per Month" = "average_montly_hours",
               "Years of Service" ="time_spend_company",
               "No of Project" ="number_project")

yb_list3 <- list("satisfaction_level" = "Employee Satisfaction Level",
                "last_evaluation" = "Employee Evaluation Level",
                "average_montly_hours" = "Average Hrs Per Month" ,
                "time_spend_company" = "Years of Service" ,
                "number_project" = "No of Project" )

x_list3 <- list("Work Accident" = "Work_accident",
               "Promotion" = "promotion_last_5years",
               "Division" = "division",
               "Income Level" ="salary",
               "Resignation Status" =  "left")

xb_list3 <- list("Work_accident" = "Work Accident",
                "promotion_last_5years" = "Promotion",
                "division" = "Division",
                "salary" = "Income Level",
                "left" = "Resignation Status"  )

# PIE list
select_list4 <- list("Current Staff" = "0",
                    "Resign Staff" = "1",
                    "Both" = "2")

selectb_list4 <- list("0" = "Current Staff",
                     "1" = "Resign Staff" ,
                     "2" = "Both"  )

fill_list4 <- list("Work Accident" = "Work_accident",
                  "Satisfaction Level" = "satisfaction_level1",
                  "Last Evaluation" = "last_evaluation1",
                  "Number of Project" = "number_project",
                  "Promotion" = "promotion_last_5years",
                  "Resignation Status" = "left",
                  "Division" = "division",
                  "Income Level" ="salary")

fillb_list4 <- list("Work_accident" = "Work Accident",
                   "satisfaction_level1" = "Satisfaction Level" ,
                   "last_evaluation1" = "Last Evaluation"  ,
                   "number_project" = "Number of Project" ,
                   "promotion_last_5years" = "Promotion",
                   "left" = "Resignation Status",
                   "division" = "Division",
                   "salary" = "Income Level" )

facet_list4 <- list("Work Accident" = "Work_accident",
                   "Number of Project" = "number_project",
                   "Promotion" = "promotion_last_5years",
                   "Resignation Status" = "left",
                   "Income Level" ="salary")

facetb_list4 <- list("Work_accident" = "Work Accident",
                    "number_project" = "Number of Project" ,
                    "promotion_last_5years" = "Promotion",
                    "left" = "Resignation Status",
                    "salary" = "Income Level" )

theme_kps <- theme(plot.background = element_rect(fill = "grey",
                                                   color = "blue",
                                                   size = 3),
                    panel.background = element_rect(fill = "white",
                                                    colour = "red"),
                    panel.grid.major = element_line(color = "orange"),
                    panel.grid.minor = element_line(color = "grey"),
                    axis.line = element_line(size = 1,
                                             color = "green"),
                    axis.text.x = element_text(size = 8,
                                               angle = 45,
                                               color = "red",
                                               hjust = 1),
                    axis.text.y = element_text(size = 8,
                                               color = "red",
                                               hjust = 1),
                    legend.key = element_rect(fill = "white",
                                              color = "black"),
                    legend.background = element_rect(fill = "pink"),
                    legend.key.size = unit(0.6, "cm"),
                    legend.position = "bottom")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Human Resource Data Visualization"),
  
    # Create tabs
    tabsetPanel(
      # Tab 1
      tabPanel("Point Plot",
               sidebarLayout(
                 div(sidebarPanel(
                   
                   selectInput(inputId = "obs01",
                               label = "For: ",
                               choices = select_list1,
                               selected="2"),
                   br(),
                   selectInput(inputId = "obs11",
                               label = "Analyse By: ",
                               choices = x_list1,
                               selected="satisfaction_level"),
                   br(),
                   selectInput(inputId = "obs21",
                               label = "vs: ",
                               choices = y_list1,
                               selected="last_evaluation"),
                   br(),
                   selectInput(inputId = "obs31",
                               label = "Group by: ",
                               choices = color_list1,
                               selected="left")
                   ), style = "width: 850px;" ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                   plotOutput("pointPlot")
                 )
               )
      ),
      
      # Tab 2
      tabPanel("Histogram Plot",
      # Sidebar with a slider input for number of bins 
                sidebarLayout(
                div(sidebarPanel(
                  selectInput(inputId = "obs02",
                              label = "For: ",
                              choices = select_list2,
                              selected="2"),
                  br(),
                        selectInput(inputId = "obs12",
                                    label = "Analyse By: ",
                                    choices = x_list2,
                                    selected="satisfaction_level1"),
                        br(),
                        selectInput(inputId = "obs22",
                                    label = "Breakdown By: ",
                                    choices = fill_list2,
                                    selected="division"),
                        br(),
                        selectInput(inputId = "obs32",
                                    label = "Group by: ",
                                    choices = facet_list2,
                                    selected="last_evaluation1")
                        ), style = "width: 850px;"),

        # Show a plot of the generated distribution
                mainPanel(
                  plotOutput("histPlot")
                )
              )
        ),
    
    # Tab 3
      tabPanel("Box Plot",
                sidebarLayout(
                  div(sidebarPanel(
                  selectInput(inputId = "obs03",
                              label = "For: ",
                              choices = select_list3,
                              selected="2"),
                  br(),
                  
                 selectInput(inputId = "obs13",
                             label = "Analyse By: ",
                             choices = y_list3,
                             selected="satisfaction_level"),
                 br(),
                 selectInput(inputId = "obs23",
                             label = "Breakdown By: ",
                             choices = x_list3,
                             selected="division")
                ), style = "width: 850px;" ),
               
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("boxPlot")
               )
             )
        ),
    # Tab 4
    tabPanel("PIE Plot",
             sidebarLayout(
               div(sidebarPanel(
                 selectInput(inputId = "obs04",
                             label = "For: ",
                             choices = select_list4,
                             selected="2"),
                 br(),
                 
                 selectInput(inputId = "obs14",
                             label = "Breakdown By: ",
                             choices = fill_list4,
                             selected="division"),
                 br(),
                 selectInput(inputId = "obs24",
                             label = "Group By: ",
                             choices = facet_list4,
                             selected="left")
             ), style = "width: 850px;" ),
    
               # Show a plot of the generated distribution
               mainPanel(
                 plotOutput("piePlot")
               )
             )
    )
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$pointPlot <- renderPlot({
    # All input from input screen
    if (input$obs01== "2")  {
      turnover_n <- turnover
    } else {
      turnover_n <- turnover %>% 
        filter (left == input$obs01)
    }     
    ggplot(data = turnover_n, aes(x = .data[[input$obs11]], 
                                  y = .data[[input$obs21]], 
                                  color = factor(.data[[input$obs31]]))) +
      geom_point(position="jitter") + 
      geom_smooth()  +
      labs(title = paste("Analysis of staff count for ",xb_list1[input$obs11],
                         "vs ", yb_list1[input$obs21], " For ",selectb_list1[input$obs01]),
           subtitle = paste( " Group by ",colorb_list1[input$obs31]),
           x = xb_list1[input$obs11],
           y = yb_list1[input$obs21],
           color = colorb_list1[input$obs31])+theme_kps
    })
  
    output$histPlot <- renderPlot({
      # All input from input screen
      if (input$obs02== "2")  {
        turnover_n <- turnover
      } else {
        turnover_n <- turnover %>% 
          filter (left == input$obs02)
      }
      
      turnover_n %>% 
        group_by(.data[[input$obs32]], .data[[input$obs22]] ,.data[[input$obs12]]) %>%
        summarise(cnt = n()) %>%
        ggplot(aes(x = .data[[input$obs12]], y = cnt, fill = .data[[input$obs22]])) +
        geom_col() +
        labs(title = paste("Analysis of Staff Count By ",xb_list2[input$obs12],
                           " For ",selectb_list2[input$obs02]),
             subtitle = paste("Breakdown by ",fillb_list2[input$obs22],
                              " Group by ",facetb_list2[input$obs32]),
             x = xb_list2[input$obs12],
             y = "Staff Count",
             fill = fillb_list2[input$obs22])+
        theme(axis.text.x = element_text(angle = 90))+
        facet_wrap(~.data[[input$obs32]])+theme_kps
      
    })
    output$boxPlot <- renderPlot({
      # All input from input screen
      if (input$obs03== "2")  {
        turnover_n <- turnover
      } else {
        turnover_n <- turnover %>% 
          filter (left == input$obs03)
      }
      
      turnover_n %>% 
        ggplot(aes(y=.data[[input$obs13]] ,x=.data[[input$obs23]])) + 
        geom_boxplot() + 
        labs (title =paste("Box Plot For ", yb_list3[input$obs13]," vs ", xb_list3[input$obs23],
                           " For ",selectb_list3[input$obs03]), 
              x=xb_list3[input$obs23], y=yb_list3[input$obs13]) +
        theme(axis.text.x = element_text(angle = 90))+theme_kps
      
    })
    output$piePlot <- renderPlot({
      # All input from input screen
      if (input$obs04== "2")  {
        turnover_n <- turnover
      } else {
        turnover_n <- turnover %>% 
          filter (left == input$obs04)
      }     
      
      turnover_n %>% 
        group_by(.data[[input$obs24]], .data[[input$obs14]]) %>% 
        summarise(n = n()) %>% 
        mutate(percentage = n/sum(n)) %>% 
        ggplot(aes(x = "", y = percentage, fill = .data[[input$obs14]])) +
        geom_col(position = "stack") +
        facet_wrap(~.data[[input$obs24]]) +
        geom_text(stat = "identity", 
                  aes(label = paste0(round(percentage * 100,2), "%")),
                  position = position_stack(vjust = 0.5), size=3) +
        labs (title =paste("Box Plot For ", fillb_list4[input$obs14]," (In Percentage) For ",
                          selectb_list4[input$obs04]),
              subtitle = paste("Group by ",facetb_list4[input$obs24]),
              x="", y="", fill = fillb_list4[input$obs14],size=10) +
        coord_polar((theta = "y")) +theme_kps
      
    })
  }

# Run the application 
shinyApp(ui = ui, server = server)
