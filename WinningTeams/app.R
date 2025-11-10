#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(tidyverse)
library(plotly)
library(here)
source("R_Data_Prep.R")


'Calculate the new variable for winning teams 
2. TMR_tbl Filter teams with winning arams, classics based on rank.
Bar graph & scatter plot x = rank, y = # of wins, color by queue types, density. face wrap.'

  
color_options <- c("Side","QueueType","RankName")
x_axis_options <- c( "TeamID","GameDuration")
y_axis_options <- c("Kills","BaronKills","RiftHeraldKills","TowerKills","DragonKills")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Winning Teams Match Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "color",
                    label = "Color your points by:",
                    choices = color_options  ),
        selectInput(inputId = "x",
                    label = "Variable on the X axis:",
                    choices = x_axis_options, 
                    selected = "GameDuration"),
        selectInput(inputId = "y",
                    label = "Variable on the Y axis:",
                    choices = y_axis_options, 
                    selected = "Kills"  )
      ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("TeamPlot"),
           plotOutput("ColorPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  my_Team_Winner <- Team_Winner_Tbl %>% mutate(Side = factor(Side),QueueType = factor(QueueType),RankName = factor(RankName))
  
   output$TeamPlot <- renderPlot({
    p <- ggplot(my_Team_Winner, aes(x = .data[[input$x]], y = .data[[input$y]], col = .data[[input$color]])) + geom_point()
    p
  })
   output$ColorPlot <- renderPlot(({
    p2 <- ggplot(my_Team_Winner, aes(x = .data[[input$color]], fill =.data[[input$color]] )) + geom_bar()
    p2
  }))
}

# Run the application 
shinyApp(ui = ui, server = server)
