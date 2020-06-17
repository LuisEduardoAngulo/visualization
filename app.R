#Interactive Visualization Tool Assignment 
#Visualization 

library(readr)
library(ggplot2)
library(plyr)
library(grDevices)
library(stats)
library(dplyr)
library(plotly)
library(FNN)
library(knitr)
library(scales)

# Define UI ----

bd <- read.csv(file = "data/nba_2017_br.csv", header = TRUE, sep = ",")

bd<- bd%>%
  mutate(FG. = FG.*100,
         X3P. = X3P.*100,
         X2P. = X2P. *100,
         FT. = FT.*100)%>%
         #Pos = as.character(Pos)) %>%
  filter(Tm != "TOT")
  

bd <- bd[,-1]


##Grouping by team for the KNN

bd2 <- bd %>%
 dplyr::group_by(Tm) %>%
 dplyr::summarize("Average 3 points" = round(mean(X3P., na.rm = TRUE),2),
                  "Average 2 points" = round(mean(X2P., na.rm = TRUE),2),
                  "Average free throw" = round(mean(FT., na.rm = TRUE),2))

vars <- setdiff(names(bd2), "Tm")

##

bd3 <- bd[1:100,] %>%
  tidyr::drop_na() %>%
  filter(X3P > 0.0)


##
ui <- fluidPage (
  navbarPage("Statistics:",
             tabPanel("For each player",
                      titlePanel("NBA Stats - Regular Season 2017"),
                      sidebarLayout(
                        sidebarPanel(
                          img(src = "nba.png", height = 50, width = 100),
                          helpText("General NBA Stats for each player per game of the 2017 regular season"),
                          selectInput("select", h4("Choose a NBA team to display"), 
                                      choices = sort(bd$Tm), selected = 1),
                          
                          sliderInput("slider", p(h4("Games played per player in regular season"),
                                                  h5("Range of interest:")),
                                      min(bd$G), max(bd$G), value = c(min(bd$G),max(bd$G))),
                          
                          sliderInput("slider2",p(h4("Average minutes played per game in regular season"),
                                                  h5("Range of interest:")),
                                      min(bd$MP), max(bd$MP), value = c(min(bd$MP),max(bd$MP))),
                          
                          sliderInput("slider3",p(h4("Player age"),
                                                  h5("Range of interest:")),
                                      min(bd$Age), max(bd$Age), value = c(min(bd$Age),max(bd$Age))),
              
                        ),#sbp
                        mainPanel(
                          tabsetPanel(type = "tabs",
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
                                               plotOutput("plot6")),
                                      tabPanel("Average Assists per Game",
                                               plotOutput("plot7")))
                        )#mainpanel
                      )#sbl
                      ),#tabpanel
             tabPanel("For each team",
                      pageWithSidebar(
                        headerPanel("Performance by field goals throws of NBA teams"),
                        sidebarPanel(
                          img(src = "nba.png", height = 50, width = 100),
                          helpText("Grouping teams according to their players performance per game in the 2017 regular season"),
                          selectInput('xcol', 'X Variable', names(bd2)[2]),
                          selectInput('ycol', 'Y Variable', vars, selected = vars[[2]]),
                          numericInput('clusters', 'Cluster count', 3, min = 1, max = 6)
                        ),#sbp
                        mainPanel(plotOutput("knn"))
                      )#pws
                      ),#tabpanel2
             
             tabPanel("Players performance comparison",
                      titlePanel("Players performance comparison - NBA Regular Season 2017 Stats"),
                      sidebarLayout(
                        sidebarPanel(width = 4,
                          img(src = "nba.png", height = 50, width = 100),
                                     helpText("Performance comparison between a player and the 
                                              three most similar players to him, regarding the stats per 
                                              game - Knn was used to identify the player's similarity."),
                                     
                                     selectInput("player", h4("Choose a player"),
                                                 choices = (paste(bd3$Player,"-",bd3$Pos,",",bd3$Tm)), 
                                                                 selected = paste(bd3$Player,"-",bd3$Pos,",",bd3$Tm)[1]),
                                     
                                     checkboxGroupInput("position",h4("Player position"),
                                                       choices = c("C" = "C", "PF" = "PF",
                                                                   "PG" = "PG", "SF" = "SF",
                                                                   "SG" = "SG"), selected = c("PG" = "PG"),
                                                       inline = TRUE),
                                     
                                     sliderInput("select_age",p(h4("Player age"),
                                                            h5("Range of interest:")),
                                                            min(bd3$Age), max(bd3$Age), value = c(min(bd3$Age),max(bd3$Age))),
                                     
                                     
                                     ),#sbp
                        mainPanel(
                          column(8, plotlyOutput("radial_plot",  width = 800, height=700)),
                                                 )
                      )#sidebarlayout
                      )#tabpanel3
    
             )#NBP
)#fluidpage

# Define server logic ----

server <- function(input, output, session) {
  
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
  
  output$plot7 <- renderPlot({
    
    data <- dataInput()
    
    ggplot(data, aes(x=Player, y=AST)) +
     geom_bar(color="gray",stat = "identity",width=0.2, )+
      theme_light() +
      theme(axis.text.x = element_text(angle = 90))+
      xlab("Player")+
      ylab("Average Assists per Game")+
      ggtitle("Average Assists per Game")+
      theme(
       panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank())
  })
  
  selectedData <- reactive({bd2[,c(input$xcol, input$ycol)]})
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$knn <- renderPlot({
  
      palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                "#FF7F00"))
    
      par(mar = c(4.5, 4.5, 0, 1)) #set the margins
      plot(selectedData(),
           col = clusters()$cluster,
           pch = 20, cex = 3)
      text(selectedData(), labels=bd2$Tm, cex= 0.7, pos=2, offset = 0.5)
      })
  
  selectedData1 <- reactive({
    bd3 %>%
      filter(bd3$Player != gsub("[[:space:]]*$","",gsub("- .*",'',input$player)))})
  
  selectedData2 <- reactive ({
    selectedData1() %>%
      select(1,2,3,4,8,11,14,18,21,22,24,25) %>%
    filter(selectedData1()$Pos %in% input$position) %>%
    filter(Age >= input$select_age[1]) %>%
    filter(Age <= input$select_age[2])
    })
  
  selectedData3 <- reactive({
    bd3 %>%
      select(1,2,3,4,8,11,14,18,21,22,24,25) %>%
      filter(bd3$Player == gsub("[[:space:]]*$","",gsub("- .*",'',input$player)))})
  
  selectedData4 <- reactive({
    rbind(selectedData3(),selectedData2())})
  
  selectedData5 <- reactive({
    selectedData4() %>%
      select(5:12)})
  
  selectedData6 <- reactive({
    as.numeric(knnx.index(selectedData5(), selectedData5()[1, , drop = FALSE], k=6))})
  
  selectedData7 <- reactive({
    selectedData4()[selectedData6(),]
    })
  
  selectedData8 <- reactive({
    selectedData7() %>%
      select(5:12)})
  
  output$radial_plot <- renderPlotly({
    validate(
      need(dim(selectedData2())[1] >=3, "Sorry, no three similar players were found. 
           Please change the input filters."))#validate
    
    plot_ly(type = 'scatterpolar',
            mode = "closest",
            fill = 'toself') %>%
      add_trace(r = as.matrix(selectedData8()[1,]),
                theta = c("FG", "3P", "2P", "FT", "ORB", "DRB",
                          "AST", "STL"),
                showlegend = TRUE,
                mode = "markers",
                name = selectedData7()[1,1])%>%
      add_trace(r = as.matrix(selectedData8()[2,]),
                theta = c("FG", "3P", "2P", "FT", "ORB", "DRB",
                          "AST", "STL"),
                showlegend = TRUE,
                mode = "markers",
                visible= "legendonly",
                name = selectedData7()[2,1])%>%
      add_trace(r = as.matrix(selectedData8()[3,]),
                theta = c("FG", "3P", "2P", "FT", "ORB", "DRB",
                          "AST", "STL"),
                showlegend = TRUE,
                mode = "markers",
                visible = "legendonly",
                name = selectedData7()[3,1])%>%
      add_trace(r = as.matrix(selectedData8()[4,]),
                theta = c("FG", "3P", "2P", "FT", "ORB", "DRB",
                          "AST", "STL"),
                showlegend = TRUE,
                mode = "markers",
                visible = "legendonly",
                name = selectedData7()[4,1])%>%
      layout(
        polar = list (
          radialaxis = list(
            visible = T,
            range = c(0,15)
          )
        ),#list
        showlegend = TRUE
      )#layout
  })#renderplotly
  
  
}


# Run the app ----
shinyApp(ui = ui, server = server)
