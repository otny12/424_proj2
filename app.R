#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(ggplot2)
library(lubridate)
library(DT)
library(scales)
library(htmltools)

years <- c(2001:2021)
#read ridership data
years_files <- list.files(pattern = "\\d\\d\\d\\d\\.tsv")
riders <- do.call(rbind, lapply(years_files, read.delim, header=TRUE,sep = "\t"))
#riders <- read.delim(file = "2021.tsv", sep="\t", fileEncoding = "UTF-8")
#update with lubridate dates
riders$date <- mdy(riders$date)

station_coord <- read.delim(file="station_coor.tsv", sep="\t", fileEncoding = "UTF-8")

# temp <- merge(subset(riders,date==ymd("2021-08-23")),
#       station_coord,
#       by="station_id"
# )

# Define UI for application that draws a histogram
ui <- fillPage(
  fillRow(
    fillCol(plotOutput("day_chart",height = "100%"),
            # radioButtons("radio_day_chart",
            #              "",
            #              choices = list("Bar Chart"="bar","Table" = "table"),
            #              inline=TRUE
            # verbatimTextOutput("test1"),
            actionButton("show", "About"),
            flex = c(95,5)
    ),
    leafletOutput("ctaStat", height = "100%"),
    fillCol(
      DT::dataTableOutput("day_table",height="100%"),
      selectInput("select_year", "Year for Right Half", 
                  choices = years, selected = 2021),
      radioButtons("background","Select Background",
                   choices = list("Simple View" = "1", "Amenities/Business Names" = "2", "Large Text" = "3"),
                   selected ="3",
                   inline=TRUE),
      radioButtons("order",
                   "Select Barchart Order",
                   choices = list("Alphabetical" = "asc",
                                  "Ridership Descending" = "desc"),
                   selected = "asc",
                   inline=TRUE),
      radioButtons("range_singular", "Select Date Selection mode",
                   choices = list("Single Date" = "1", "Range Selection" = "2"),
                   selected = "1",
                   inline=TRUE),
      fluidRow(
        actionButton(inputId="prevDay",
                     label = "Previous Day"),
        actionButton(inputId="nextDay",
                     label = "Next Day")
      ),
      dateInput("date", label = "Date selection", value = "2021-08-23"),
      dateRangeInput("date_range", label = "Date Range",
                     start = "2021-08-23",
                     end = "2021-08-24",
                     min = "2001-01-01",
                     max = "2021-11-30"),
      
      flex = c(50,5,5,5,5,5,5,5)
    ),#end fluidRow
    fillCol(
      plotOutput("station_chart"),
      DT::dataTableOutput("station_table"),
      plotOutput("year_by_day"),
      DT::dataTableOutput("year_by_day_table")
      # flex = c(30, 20,30,20)
    ),
    fillCol(
      plotOutput("year_by_month"),
      DT::dataTableOutput("year_by_month_table"),
      plotOutput("week_by_day"),
      DT::dataTableOutput("week_by_day_table")
    )
  )#end fillrow
  
)#end fillpage

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #reactive dataset for large barchart and map
  day_data <- reactive({
    switch(input$range_singular,
           "1" = single_data(),
           "2" = range_data())
  })
  single_data <- reactive(merge(subset(riders,date==cur_date()),
                             station_coord,
                             by="station_id"
  )
  )
  #multi-date selection things
  range_data <- reactive({
    first <- merge(subset(riders,date==ymd(input$date_range[1])),
                   station_coord,
                   by="station_id"
    )
    second <-merge(subset(riders,date==ymd(input$date_range[2])),
                   station_coord,
                   by="station_id"
    )
    if(input$date_range[1]!=input$date_range[2])
      first$rides <- second$rides- first$rides
    first
  })
  
  #single date selection things
  cur_date <- reactiveVal(ymd("2021-08-23"))
  cur_background <-reactive({
    switch(input$background,
           "1" = providers$CartoDB.Positron,
           "2" = providers$OpenStreetMap.HOT,
           "3" = providers$Stamen.Terrain
    )
  })
  
  observeEvent(input$nextDay,{
    cur_date(cur_date()+days(1))
    updateDateInput(inputId = "date", value = cur_date())
  })
  observeEvent(input$prevDay,{
    cur_date(cur_date()-days(1))
    updateDateInput(inputId = "date", value = cur_date())
  })
  observeEvent(input$date,{
    cur_date(ymd(input$date))
  })
  #DELETE
  output$out <-renderPrint({cur_date()})

  #reactive dataset for right side
  single_station <- reactive({
    if(is.null(event()))
      subset(riders, stationname=="UIC-Halsted")
    else
      subset(riders,station_id == station_coord[station_coord[,3] == event()$lat,1])
    })
    # subset(riders, stationname=="UIC-Halsted"))

  
  #Render map
  output$ctaStat <- renderLeaflet({
    leaflet(options=leafletOptions(zoomControl = FALSE)) %>% 
      addProviderTiles(providers$CartoDB.Positron)%>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'bottomleft' }).addTo(this)
    }")%>%
      setView(lng=-87.63144, lat=41.88094,zoom=11)
  })
  
  
  ordering <- reactive({
    switch(input$order,
           "desc" = aes(x=reorder(stationname,-rides),y=rides),
           "asc"= aes(x=reorder(stationname,stationname),y=rides)
    )
  })
  #Render day chart of all stations
  output$day_chart <- renderPlot({
    ggplot(day_data(), ordering()) +
      geom_bar(stat="identity", fill="steelblue") + 
      theme(axis.text.x = element_text(angle = 45))+
      coord_flip()
  })
  
  #add circles to map
  observe({
    leafletProxy("ctaStat", data=day_data()) %>%
      clearShapes() %>%
      addCircles(radius = ~abs(rides)*.05,
                 weight=1,
                 opacity=10,
                 color = ~ifelse(rides > 0, "#0000FF", "#FF0000"),
                 popup=~htmlEscape(stationname)) %>%
      addProviderTiles(cur_background())
  })
  event <- reactive(input$ctaStat_shape_click)
  output$test1 <-renderPrint(
    subset(riders,station_id == station_coord[station_coord[,3] == event()$lat,1])
    )

  
  #render table
  output$day_table <- DT::renderDataTable({
    DT::datatable(day_data()[,c(3,5)],
                  options = list(searching = FALSE,
                                 pageLength = 12,
                                 lengthChange = FALSE,
                                 order = list(list(0, order)),
                                 columnDefs = list(list(className = 'dt-left', targets = 0)),
                                 rownames=FALSE)
    )
  })
  
  #render single station chart
  output$station_chart <- renderPlot({
    annual <-  aggregate(single_station()$rides, list(single_station()$y),sum)
    ggplot(annual, aes(x=Group.1, y=x)) +
      geom_bar(stat="identity", fill="steelblue") +
      labs(title = "Ridership from 2001 to 2021 by Year", x="Year", y = "Ridership") +
      scale_x_discrete(name="Year",limits=years)+
      scale_y_continuous(labels = comma)
  })
  output$year_by_day <- renderPlot({
    y <-subset(single_station(), y == input$select_year)
    ggplot(y, aes(x=date, y=rides)) + geom_bar(stat="identity", fill="steelblue") +
      labs(title = "Ridership from 2001 to 2021 by Day", x="Date", y = "Ridership") +
      scale_y_continuous(labels = comma)
  })
  output$year_by_month <- renderPlot({
    y <-subset(single_station(), y == input$select_year )
    y$month <- month(y$date)
    monthly <- aggregate(y$rides, list(y$month),sum) 
    #show barchart for each month of 2021
    ggplot(monthly, aes(x=Group.1, y=x)) +
      geom_bar(stat="identity", fill="steelblue") +
      labs(title = "Monthly Ridership for the Current Year", x="Month", y = "Ridership")+
      scale_x_discrete(name="Year",limits =month.name)+
      scale_y_continuous(labels = comma)
  })
  output$week_by_day <- renderPlot({
    y <-subset(single_station(), y == input$select_year)
    y$daynames <- wday(y$date, label=TRUE)
    weekdays <- aggregate(y$rides, list(y$daynames),sum) 
    ggplot(weekdays, aes(x=Group.1, y=x)) +
      geom_bar(stat="identity", fill="steelblue") +
      labs(Title = "Day of the Week Ridership for the Current Year", x="Days of the Week", y="Ridership")+
      scale_y_continuous(labels = comma)
  })
  
  #render data tables for selected station
  output$station_table <- DT::renderDataTable({
    p <- single_station()
    names(p)[6] = "Date"
    names(p)[5] = "Rides"
    tabdat <- p[, 6:5]
    DT::datatable(tabdat,
                  options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(0, order)),
                                 columnDefs = list(list(className = 'dt-left', targets = 0))),
                  rownames=FALSE)
  })
  
  output$year_by_day_table <- DT::renderDataTable({
    p <- single_station()
    names(p)[6] = "Date"
    names(p)[5] = "Rides"
    tabdat <- p[, 6:5]
    DT::datatable(tabdat,
                  options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(0, order)),
                                 columnDefs = list(list(className = 'dt-left', targets = 0))),
                  rownames=FALSE)
  })
  
  output$year_by_month_table <- DT::renderDataTable({
    y <-subset(single_station(), y == input$select_year )
    months <- aggregate(y$rides, list(month(y$date)),sum)
    names(months)[1] <- "Month"
    names(months)[2] <- "Rides"
    tabdat <- months[,1:2]
    DT::datatable(tabdat,
                  options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(0, order)),
                                 columnDefs = list(list(className = 'dt-left', targets = 0))),
                  rownames=FALSE)
  })
  
  output$week_by_day_table <- DT::renderDataTable({
    y <-subset(single_station(), y == input$select_year)
    day <-aggregate(y$rides, list(wday(y$date, label=TRUE)),sum)
    names(day)[1] <- "Day"
    names(day)[2] <- "Rides"
    tabdat <-day[,1:2]
    DT::datatable(tabdat,
                  options = list(searching = FALSE, pageLength = 7, lengthChange = FALSE, order = list(list(0, order)),
                                 columnDefs = list(list(className = 'dt-left', targets = 0))),
                  rownames=FALSE)
  })
  
  observeEvent(input$show, {
    showModal(modalDialog(
      title = "About",
      "This visualization was done by Tony Lau, using data provided by the CTA. Data is available for download at 
      https://data.cityofchicago.org/Transportation/CTA-System-Information-List-of-L-Stops/8pix-ypme
      Date of publication is 3/12/2022.  This was created as Project 2 of CS 424 taught by Proffessor Johnson.  This app is designed to be run on a 5760x1620 wall display, to view best in browser zoom a 1920x1080 display to 50%"
    ))
  })
  
}#end server

# Run the application 
shinyApp(ui = ui, server = server)
