library(shiny); 
library(shinydashboard);
library(ggplot2);
library(dplyr);
library(plotly);
library(lubridate);
library(shinythemes);
library(shinyWidgets)

setwd("C:\\Users\\kenne\\OneDrive\\Desktop\\Bx Ubiqum\\C3Task2-Analyze energy data")
energy_comp <- readRDS("energy_comp.rds")

ui <- dashboardPage(
        skin= "yellow",
        dashboardHeader(title = "IoT Analytics Power Management",
                        titleWidth = 450),
        dashboardSidebar(width = 250,
            # dateRangeInput("dates", label = h3("Overall Trend")),
            #     hr(),   # add horrizontal line 
            # fluidRow(column(4, verbatimTextOutput("value"))),
            sliderInput("DateRange",
                        "Choose range for Overall Trend",
                        min = as.Date("2007-01-01"),
                        max = as.Date("2011-12-31"),
                        value= c(as.Date("2007-01-01"), as.Date("2011-12-31")),
                        timeFormat="%Y-%B"),
            hr(),
            selectInput(                    
              inputId = "input1",
              label= "Select the year",
              choices = list("2007", "2008", "2009", "2010", "2011")),
            selectInput(                    
              inputId = "input2",
              label= "Look closer in one month",
              choices = list( "Jan","Feb","Mar","Apr","May","Jun",
                              "Jul","Aug","Sep","Oct","Nov","Dec")),
            textInput("week", label = ("Look at one week"), value = "Enter week: eg. week 51..."),
            radioButtons(
              inputId = "input3",
              label="Did I use too much last week?",
              c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))),
          # navbarPage("Consumption",theme = shinytheme("flatly"),
        dashboardBody(
         #   mainPanel(
              fluidRow(
                column(8,
            tabsetPanel(
             # setBackgroundColor("green"),
              tabPanel("Overall Trend", plotlyOutput(outputId = "overview"), style= "background-color: orange"),
              tabPanel("Total Usage",
                       fluidRow(
                         splitLayout(cellWidths = c("60%", "40%"),
                            plotlyOutput("box1"), plotlyOutput("box5"))
                       )),
                       # plotlyOutput(outputId = "box1")),
              tabPanel("Kitchen", 
                       fluidRow(
                          splitLayout(cellWidths = c("40%", "25%", "35%"),
                            plotlyOutput("box2"), plotlyOutput("box6"), plotlyOutput("box9"))
                        )),
              tabPanel("Laundry",  
                       fluidRow(
                         splitLayout(cellWidths = c("40%", "25%", "35%"),
                                     plotlyOutput("box3"), plotlyOutput("box7"), plotlyOutput("box10"))
                       )),
              tabPanel("Temperature Control", 
                       fluidRow(
                         splitLayout(cellWidths = c("40%", "25%", "35%"),
                                     plotlyOutput("box4"), plotlyOutput("box8"), plotlyOutput("box11"))
                       )),
              tabPanel("Living Room"),
              tabPanel("Master Bedroom"),
              tabPanel("+ Your Choice +"))
            ), #column 
            column(4,
              tabBox(
                title = "Home Automation", 
                id = "tabset1", height = "500px", width = "300px",
                tabPanel("Out of house",
                        HTML( "Turn off all smart plugs. <br/>
                              EXCEPT  <br/>
                              security, fridge and heating."),
                       radioButtons("outofhouse", label = ("Turn on/off all:"),
                                     choices = list("Confirm to turn off all" = 1), selected = 1)),
                tabPanel("Rooms", 
                      radioButtons("radio", label = ("Turn on/off these:"),
                            choices = list("Kitchen" = 1, "Laundry" = 2, "Temperature Con." = 3, "Living Room" =4), 
                            selected = 1)),
                tabPanel("Appliances",
                     radioButtons("appliances", label = ("Turn on/off these:"),
                           choices = list("Fridge" = 1, "Oven" = 2, "Lightings" = 3, "Entertainments" =4), 
                           selected = 1)),
                infoBox("Money Saved", "25% saved!!!" , icon = icon("credit-card"), color= "yellow")
            ) #tabbox
              ) #column
                )#fluidrow 
   #   )
    ) #dashbody
) #dashpage 
    # box(plotlyOutput(outputId = "box1"),width = 6), #Global= Total usage
    # box(plotlyOutput(outputId = "box2"),width = 6), #Kitchen
    # box(plotlyOutput(outputId = "box3"),width = 6), #Laundry
  #   # box(plotlyOutput(outputId = "box4"),width = 6)  #Temp
server <- function(input, output) { 
 # Daterange<-input$Daterange    #create data linage 
  output$overview <- renderPlotly({
    plot_ly(daily_total_11,
      name = 'Year over year trend', type= 'bar') %>%
                            #type = 'scatter', mode = 'lines')  %>%
      add_trace(data = daily_sum_con %>% filter(Date >= input$DateRange[1],
                                                Date <= input$DateRange[2]), x = ~Date, y = ~Total,
                color = I('blue'), name = "consumption") %>%
      add_trace(data = temp_11 %>% filter(Date >= input$DateRange[1],
                                          Date <= input$DateRange[2]), x = ~Date, y = ~Total, 
                color = I('red'), name = "forecasts") %>%
      layout(title = list(text = 'Year over year trend',
                          font = list(color = "tomato")),
             xaxis = list(title = 'Time',
                          color = "orangered",
                          zeroline = TRUE),
             yaxis = list(title = 'kilowatt',
                          color = "orangered"))
  })
  
  output$box1 <- renderPlotly({ 
    plot_ly(daily_total_11 %>% filter(year== input$input1, 
                                      month== input$input2),
            x = ~Date, y = ~Total, 
            name = 'Total Usage', type = 'bar')  %>%
                layout(title = list(text = 'Total Usage-Month', 
                                    font = list(color = "tomato")),
                       xaxis = list(title = 'Time',
                                    color = "orangered"),
                       yaxis = list(title = 'kilowatt',
                                    color = "orangered"))
  
            })
  output$box2 <- renderPlotly({ 
    plot_ly(daily_total_11 %>% filter(year== input$input1, 
                                      month== input$input2), 
            x = ~Date, y = ~Kitchen, 
            name = 'Kitchen', type = 'bar') %>%
    layout(title = list(text = 'Kitchen-Month', 
                        font = list(color = "tomato",
                                    size =18)),
           xaxis = list(title = 'Time',
                        color = "orangered",
                        zeroline = TRUE),
           yaxis = list(title = 'watt-hour',
                        color = "orangered"))#%>% 
#      add_trace(y=~Kitchen[which(daily_total$Date > "2010-11-27")], line= list(color = 'rgb(205, 12, 24)', width = 4))
 
       })
  output$box3 <- renderPlotly({ 
    plot_ly(daily_total_11 %>% filter(year== input$input1, month== input$input2),
          x = ~Date, y = ~Laundry, 
          name = 'Laundry', type = 'bar')%>%
      layout(title = list(text = 'Laundry-Month', 
                          font = list(color = "tomato")),
             xaxis = list(title = 'Time',
                          color = "orangered",
                          zeroline = TRUE),
             yaxis = list(title = 'watt-hour',
                          color = "orangered"))
  })
  output$box4 <- renderPlotly({ 
    plot_ly(daily_total_11 %>% filter(year== input$input1, month== input$input2),
            x = ~Date, y = ~Temp, name = 'Temperature Con.', type = 'bar')%>%
      layout(title = list(text = 'Temperature Con.- Month', 
                          font = list(color = "tomato")),
             xaxis = list(title = 'Time',
                          color = "orangered",
                          zeroline = TRUE),
             yaxis = list(title = 'watt-hour',
                          color = "orangered"))
  })
  output$box5 <- renderPlotly({
  plot_ly(daily_total_11  %>%filter(year== input$input1, month== input$input2, wday==input$input3),
          x = ~Date, y= ~Total, name='Total Usage', type = 'bar')%>%
      layout(title = list(text = 'Total Usage-Weekday', 
                          font = list(color = "tomato")),
             xaxis = list(title = 'Weekday',
                          color = "orangered",
                          zeroline = TRUE),
             yaxis = list(title = 'kilowatt',
                          color = "orangered"))
  })
  output$box6 <- renderPlotly({
    plot_ly(daily_total_11  %>%filter(year== input$input1, month== input$input2, wday==input$input3),
            x = ~Date, y= ~Kitchen, name='Kitchen', type = 'bar')%>%
      layout(title = list(text = 'Kitchen-Weekday', 
                          font = list(color = "tomato")),
             xaxis = list(title = 'Weekday',
                          color = "orangered",
                          zeroline = TRUE),
             yaxis = list(title = 'watt-hour',
                          color = "orangered"))
  })
  output$box7 <- renderPlotly({
    plot_ly(daily_total_11  %>%filter(year== input$input1, month== input$input2, wday==input$input3),
            x = ~Date, y= ~Laundry, name='Laundry', type = 'bar')%>%
      layout(title = list(text = 'Laundry-Weekday', 
                          font = list(color = "tomato")),
             xaxis = list(title = 'Weekday',
                          color = "orangered",
                          zeroline = TRUE),
             yaxis = list(title = 'watt-hour',
                          color = "orangered"))
  })
  output$box8 <- renderPlotly({
    plot_ly(daily_total_11  %>%filter(year== input$input1, month== input$input2, wday==input$input3),
            x = ~Date, y= ~Temp, name='Temperature Con.', type = 'bar')%>%
      layout(title = list(text = 'Temperature Con.', 
                          font = list(color = "tomato")),
             xaxis = list(title = 'Weekday',
                          color = "orangered",
                          zeroline = TRUE),
             yaxis = list(title = 'watt-hour',
                          color = "orangered"))
  })
  output$box9 <- renderPlotly({
    plot_ly(daily_sum_week %>% filter(year== input$input1,week== "51"),
            x = ~wday, y = ~Kitchen_w, name = 'Kitchen', type = 'bar')%>%
      layout(title = list(text = 'Kitchen- week 51 ',
                          font = list(color = "tomato")),
             xaxis = list(title = 'Weekdays',
                          color = "orangered",
                          zeroline = TRUE),
             yaxis = list(title = 'watt-hour',
                          color = "orangered"))
  })
  output$box10 <- renderPlotly({
    plot_ly(daily_sum_week %>% filter(year== input$input1,week== "51"),
            x = ~wday, y = ~Laundry_w, name = 'Laundry', type = 'bar')%>%
      layout(title = list(text = 'Laundry- week 51 ',
                          font = list(color = "tomato")),
             xaxis = list(title = 'Weekdays',
                          color = "orangered",
                          zeroline = TRUE),
             yaxis = list(title = 'watt-hour',
                          color = "orangered"))
  })
  output$box11 <- renderPlotly({
    plot_ly(daily_sum_week %>% filter(year== input$input1,week== "51"),
            x = ~wday, y = ~Temp_w, name = 'Temp', type = 'bar')%>%
      layout(title = list(text = 'Temp- week 51 ',
                          font = list(color = "tomato")),
             xaxis = list(title = 'Weekdays',
                          color = "orangered",
                          zeroline = TRUE),
             yaxis = list(title = 'watt-hour',
                          color = "orangered"))
  })
}


shinyApp(ui, server)
