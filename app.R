#Interactive Visualization Tool Assignment 
#Visualization 

library(readr)
library(ggplot2)
library(dplyr)
library(plyr)

# Define UI ----

#setwd("C:/Users/leang/OneDrive/Maestría/I Semestre/Visualization/Final assigment/Int_Visualization")
bd <- read.csv(file = "data/nba_2017_br.csv", header = TRUE, sep = ",")

bd<- bd%>%
  mutate(FG. = FG.*100,
         X3P. = X3P.*100,
         X2P. = X2P. *100,
         FT. = FT.*100)
bd<-bd%>%
  filter(Tm != "TOT")

ui <- fluidPage (
  titlePanel("NBA Stats - Regular Season 2017"),
  sidebarLayout(
    sidebarPanel(
      img(src = "nba.png", height = 50, width = 100),
      helpText("General NBA Stats for each player per game of the 2017 regular season"),
      selectInput("select", h4("Choose a NBA team to display"), 
                  choices = bd$Tm, selected = 1),
      
      sliderInput("slider", p(h4("Games played per player in regular season"),
                            h5("Range of interest:")),
                            min(bd$G), max(bd$G), value = c(min(bd$G),max(bd$G))),
      
      sliderInput("slider2",p(h4("Average minutes played per game in regular season"),
                           h5("Range of interest:")),
                           min(bd$MP), max(bd$MP), value = c(min(bd$MP),max(bd$MP))),
      
      sliderInput("slider3",p(h4("Player age"),
                            h5("Range of interest:")),
                            min(bd$Age), max(bd$Age), value = c(min(bd$Age),max(bd$Age))),
        
    ),
    
    mainPanel(
      
      tabsetPanel(type="tabs",
                  tabPanel("Field Goals Percentage",
                           plotOutput("plot1")),
                  tabPanel("2 Point Field Goals Percentage",
                           plotOutput("plot2")),
                  tabPanel("3 Point Field Goals Percentage",
                           plotOutput("plot3")),
                  tabPanel("Free Throw Percentage",
                           plotOutput("plot4")),
                  tabPanel("Average Field Goals per Game",
                           plotOutput("plot5")),
                  tabPanel("Average Points per Game",
                           plotOutput("plot6"))
      )
  )
  )
)

# Define server logic ----

server <- function(input, output) {
  
  dataInput <- reactive({
    dataset <- subset(bd, G >= input$slider[1] & G <= input$slider[2] 
                      & Tm==input$select 
                      & MP >= input$slider2[1] & MP <= input$slider2[2]
                      & Age >= input$slider3[1] & Age <= input$slider3[2])
    dataset
  })
  
  output$plot1 <- renderPlot({
    
    data <- dataInput()
    
    ggplot(data, aes(x=Player, y=FG.)) +
      geom_segment(aes(x=Player, xend=Player, y=0, yend=FG.), color="black")+
      geom_point( color="gray", size=4, alpha=1)+
      theme_light() +
      coord_flip()+
      xlab("Player")+
      ylab("Field Goal Percentage")+
      ggtitle("Field Goals Accuracy Percentage per Player")+
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank())

  })
  
  output$plot2 <- renderPlot({
    
    data <- dataInput()
    
    ggplot(data, aes(x=Player, y=X2P.)) +
      geom_segment(aes(x=Player, xend=Player, y=0, yend=X2P.), color="black")+
      geom_point( color="gray", size=4, alpha=1)+
      theme_light() +
      coord_flip()+
      xlab("Player")+
      ylab("2 Point Field Goals Percentage")+
      ggtitle("2 Point Field Goals Accuracy Percentage per Player")+
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank())
  })
  
  output$plot3 <- renderPlot({
    
    data <- dataInput()
    
    ggplot(data, aes(x=Player, y=X3P.)) +
      geom_segment(aes(x=Player, xend=Player, y=0, yend=X3P.), color="black")+
      geom_point( color="gray", size=4, alpha=1)+
      theme_light() +
      coord_flip()+
      xlab("Player")+
      ylab("3 Point Field Goals Percentage")+
      ggtitle("3 Point Field Goals Accuracy Percentage per Player")+
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank())
  })
  
  output$plot4 <- renderPlot({
    
    data <- dataInput()
    
    ggplot(data, aes(x=Player, y=FT.)) +
      geom_segment(aes(x=Player, xend=Player, y=0, yend=FT.), color="black")+
      geom_point( color="gray", size=4, alpha=1)+
      theme_light() +
      coord_flip()+
      xlab("Player")+
      ylab("Free Throw Percentage")+
      ggtitle("Free Throw Accuracy Percentage")+
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank())
  })
  
  output$plot5 <- renderPlot({
    
    data <- dataInput()
    
    ggplot(data, aes(x=Player, y=FG)) +
      geom_segment(aes(x=Player, xend=Player, y=0, yend=FG), color="black")+
      geom_point( color="gray", size=4, alpha=1)+
      theme_light() +
      coord_flip()+
      xlab("Player")+
      ylab("Average Field Goals per Game")+
      ggtitle("Average Field Goals per Game")+
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank())
  })
  
  output$plot6 <- renderPlot({
    
    data <- dataInput()
    
    ggplot(data, aes(x=Player, y=PS.G)) +
      geom_segment(aes(x=Player, xend=Player, y=0, yend=PS.G), color="black")+
      geom_point( color="gray", size=4, alpha=1)+
      theme_light() +
      coord_flip()+
      xlab("Player")+
      ylab("Average Points per Game")+
      ggtitle("Average Points per Game")+
      theme(
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank())
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
