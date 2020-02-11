library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)

litterData = read.csv("litterati_challenge-65.csv",stringsAsFactors = FALSE) #reads in the csv
#remove entries out of range
litterData$bools[litterData$lat > 40 & litterData$lat < 41.87 & litterData$lon < -87 & litterData$lon > -87.82] <- TRUE
litterData$bools[litterData$bools == ""] <- FALSE #creates a vector of booleans which determine if the row is good or not
rowsToKeep = which(litterData$bools) #turns the bools into indices.
litterData <- litterData[rowsToKeep,] #removes any rows without indices.

litterData$tags[litterData$tags==""] <- "UNTAGGED" #converts blank entries to untagged
litterData$username <- gsub("litterati-",litterData$username, replacement = "user ") #replaces usernames

litterData$time <- ymd_hms(litterData$litterTimestamp, tz = "GMT") #stores the time
litterData$time <- with_tz(litterData$time, tzone = "America/Chicago") #converts to chicago time
litterData$Weekday <- wday(litterData$time, label = TRUE) #store day of week
litterData$date <- date(litterData$time)
litterData$Hour <-hour(litterData$time)


litterData$seperatedTags <- (strsplit(litterData$tags,","))

userTable <- table(litterData$username)
userTable <- sort(userTable, decreasing = TRUE)

# m <- leaflet() 
# m <- addTiles(m)
# m <- addMarkers(m,lng=litterData$lon, lat=litterData$lat, popup="Litterati data",clusterOptions = markerClusterOptions())


userTableFrame <- as.data.frame(userTable)


fixedTags <- unlist(litterData$seperatedTags)
tagTable <- table(fixedTags)
tagTable <- sort(tagTable,decreasing = TRUE)
tagTableFrame <- as.data.frame(tagTable)
colnames(tagTableFrame) <- c("Tags","Amount")


weekDayTable <- table(litterData$Weekday)
weekDayFrame<- as.data.frame(weekDayTable)
colnames(weekDayFrame) <- c("Day","Amount")

newDateTable <- table(litterData$date)
newDateFrame <- as.data.frame(newDateTable)
colnames(newDateFrame) <- c("Date","Amount")

timeTable <- table(litterData$Hour)
timeTableFrame <- as.data.frame(timeTable)
colnames(timeTableFrame) <- c("Hour","Amount")


staticWeekDayFrame <- weekDayFrame
staticTagTableFrame <- tagTableFrame
staticTimeTableFrame <- timeTableFrame
staticNewDateFrame <- newDateFrame


breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
labels <- c("Night", "Morning", "Afternoon", "Evening")
litterData$timeOfDay <- cut(x=litterData$Hour, breaks = breaks, labels = labels, include.lowest=TRUE)



regNameVector <- as.character(userTableFrame$Var1[1:10])
regNameVector[11] <- "."
regTagVector <- as.character(tagTableFrame$Tags[1:10])
regTagVector[11] <- "."


ui <- fluidPage(
  
  # Application title
  titlePanel("Litterati visualization"),
  
  
  navlistPanel(
    "Bar charts",
    tabPanel("Top 10 users",tableOutput("userTable"),"Total pieces of litter picked: ",textOutput("TotalPicked")),
    
    tabPanel("Amount picked by day of week",
             
            tabsetPanel(
              type = "tabs",
              tabPanel("Bar Chart",plotOutput("staticWeekdayTableFrame"),plotOutput("weekdayTableFrame")) ,
              tabPanel("Table", tableOutput("weekdayTable")) 
              
                       )
            ),
    tabPanel("Amount picked each day",
             
             tabsetPanel(
               type = "tabs", 
               tabPanel("Bar Chart",plotOutput("staticNewDateTableFrame"),plotOutput("newDateTableFrame")) ,
               tabPanel("Table 1", tableOutput("newDateTable")), 
               tabPanel("Table 2", tableOutput("newDateTable2")),
               tabPanel("Table 2", tableOutput("newDateTable3"))
             )
    ),
    tabPanel("Amount picked by hour",
             
             tabsetPanel(
               type = "tabs", 
               tabPanel("Bar Chart",plotOutput("staticTimeTableFrame"),plotOutput("timeTableFrame")) ,
               tabPanel("Table", tableOutput("timeTable")) 
               
             )
    ),
    tabPanel("Amount picked by tag",
             
             tabsetPanel(
               
               type = "tabs", 
               
               tabPanel("Bar Chart",plotOutput("staticTagTableFrame"),plotOutput("tagTableFrame")) ,
               tabPanel("Table", tableOutput("tagTable")) 
               
             )
    ),
  
    "Map",
    tabPanel("Leaflet Map",
             
             leafletOutput("leaf")),
    tabPanel(
      "Interactivity",
      
             radioButtons("radio1",label = h3("Select a tag"), inline = TRUE, choiceNames = list("All",textOutput("tag1"),textOutput("tag2"),
                                    textOutput("tag3"),textOutput("tag4"),textOutput("tag5"),textOutput("tag6"),
                                    textOutput("tag7"),textOutput("tag8"),textOutput("tag9"),textOutput("tag10")
                                    ),
                          choiceValues = list(11,1,2,3,4,5,6,7,8,9,10)),

    radioButtons("radio2",label = h3("Select a user"), inline = TRUE, choiceNames = list("All",textOutput("user1"),textOutput("user2"),
                           textOutput("user3"),textOutput("user4"),textOutput("user5"),textOutput("user6"),
                           textOutput("user7"),textOutput("user8"),textOutput("user9"),textOutput("user10")
    ),
    choiceValues = list(11,1,2,3,4,5,6,7,8,9,10)),
    radioButtons("radioTOD",label = h3("Select a time of day"), inline = TRUE, choiceNames = list("All","Night","Morning","Afternoon","Evening"),
                 choiceValues = list(".","Night","Morning","Afternoon","Evening")
                 
                 
                 ),
    radioButtons("radioMonth",label = h3("Select a month"), inline = TRUE, choiceNames = list("All","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),
                 choiceValues = list(".","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    
    
    )),
    
    
    
    tabPanel("About","Data is from https://www.litterati.org/, This data was analysed using R. The libraries that were included are
             Shiny, ggplot2, lubridate, DT, jped, grid, leaflet, and scales. This was created by Sean Stiely on 2/9/2020")
    
  )
  
)
  

server <- function(input, output) {
  output$userTable <- renderTable(userTable[1:10])
  
  userIndices <- reactive({
    userIndices <- grepl(regNameVector[as.numeric(input$radio2)],litterData$username)
  })
  tagIndices <- reactive({
    tagIndices <- grepl(regTagVector[as.numeric(input$radio1)],litterData$seperatedTags)
  })
  timeOfDayIndices <- reactive({
    timeOfDayIndices <- grepl(as.character(input$radioTOD),litterData$timeOfDay)
  })
  monthIndices <- reactive({
    monthIndices <- grepl(as.character(input$radioMonth),month(litterData$time,label = TRUE))
  })
  
  
  masterVector <- reactive({
    masterVector <- userIndices() & tagIndices() & timeOfDayIndices() & monthIndices()
  })
  
  
  
  output$staticWeekdayTableFrame <- renderPlot({
    ggplot(data=staticWeekDayFrame, aes(x=Day, y=Amount)) +
      geom_bar(stat="identity",fill = "blue2")
  })
  
  
  output$weekdayTableFrame <- renderPlot({
    weekDayTable <- table(litterData$Weekday[masterVector()])
    weekDayFrame<- as.data.frame(weekDayTable)
    colnames(weekDayFrame) <- c("Day","Amount")
    ggplot(data=weekDayFrame, aes(x=Day, y=Amount)) +
    geom_bar(stat="identity",fill = "blue2")
    })
  
  output$staticNewDateTableFrame <- renderPlot({
    staticNewDateFrame$Date <- as.Date(staticNewDateFrame$Date)
    ggplot(data=staticNewDateFrame, aes(x=Date, y=Amount)) +
      geom_bar(stat="identity", fill = "thistle2") +
    scale_x_date(date_breaks = "1 month", date_labels =  "%b", expand = c(0, 0))
  })
  
  output$newDateTableFrame <- renderPlot({
    newDateTable <- table(litterData$date[masterVector()])
    newDateFrame <- as.data.frame(newDateTable)
    colnames(newDateFrame) <- c("Date","Amount")
    newDateFrame$Date <- as.Date(newDateFrame$Date)
    ggplot(data=newDateFrame, aes(x=Date, y=Amount)) +
    geom_bar(stat="identity", fill = "thistle2") +
      scale_x_date(date_breaks = "1 month", date_labels =  "%b", expand = c(0, 0))
    })
  output$staticTimeTableFrame <- renderPlot({
    ggplot(data=staticTimeTableFrame, aes(x=Hour, y=Amount)) +
      geom_bar(stat="identity",fill = "orange")
  })

  output$timeTableFrame <- renderPlot({
    timeTable <- table(litterData$Hour[masterVector()])
    timeTableFrame <- as.data.frame(timeTable)
    colnames(timeTableFrame) <- c("Hour","Amount")
    ggplot(data=timeTableFrame, aes(x=Hour, y=Amount)) +
    geom_bar(stat="identity",fill = "orange")
    })

  output$staticTagTableFrame <- renderPlot({
    ggplot(data=staticTagTableFrame[1:10,], aes(x=Tags, y=Amount)) +
      geom_bar(stat="identity", fill = "tan2")
  })
  output$tagTableFrame <- renderPlot({
    fixedTags <- unlist(litterData$seperatedTags[masterVector()])
    tagTable <- table(fixedTags)
    tagTable <- sort(tagTable,decreasing = TRUE)
    tagTableFrame <- as.data.frame(tagTable)
     if (length(tagTableFrame) == 1) {
       tagTableFrame$Amount[1] <- as.numeric(tagTable[1])
       tagTableFrame$Tags[1] <- fixedTags[1]
     }
    
    colnames(tagTableFrame) <- c("Tags","Amount")
    ggplot(data=tagTableFrame[1:10,], aes(x=Tags, y=Amount)) +
    geom_bar(stat="identity", fill = "tan2")})
    
  
  output$weekdayTable <- renderTable(weekDayTable) 
  output$newDateTable <- renderTable(newDateTable[1:100])
  output$newDateTable2 <- renderTable(newDateTable[100:200])
  output$newDateTable3 <- renderTable(newDateTable[2:314])
  output$timeTable <- renderTable(timeTable)
  output$tagTable <- renderTable(tagTable[1:10])
  
  
  output$leaf <- renderLeaflet({
    m <- leaflet()
    m <- addTiles(m)                           
    m <- addMarkers(m,lng = litterData$lon[masterVector()],lat = litterData$lat[masterVector()],popup="Litterati data",clusterOptions = markerClusterOptions())                           
    })
  #output$TotalPicked <- renderText(userIndices())
  output$TotalPicked <- renderText(as.character(length(litterData$challengeId)))
  output$tag1 <- renderText(as.character(tagTableFrame$Tags[1]))
  output$tag2 <- renderText(as.character(tagTableFrame$Tags[2]))
  output$tag3 <- renderText(as.character(tagTableFrame$Tags[3]))
  output$tag4 <- renderText(as.character(tagTableFrame$Tags[4]))
  output$tag5 <- renderText(as.character(tagTableFrame$Tags[5]))
  output$tag6 <- renderText(as.character(tagTableFrame$Tags[6]))
  output$tag7 <- renderText(as.character(tagTableFrame$Tags[7]))
  output$tag8 <- renderText(as.character(tagTableFrame$Tags[8]))
  output$tag9 <- renderText(as.character(tagTableFrame$Tags[9]))
  output$tag10 <- renderText(as.character(tagTableFrame$Tags[10]))
  output$user1 <- renderText(as.character(userTableFrame$Var1[1]))
  output$user2 <- renderText(as.character(userTableFrame$Var1[2]))
  output$user3 <- renderText(as.character(userTableFrame$Var1[3]))
  output$user4 <- renderText(as.character(userTableFrame$Var1[4]))
  output$user5 <- renderText(as.character(userTableFrame$Var1[5]))
  output$user6 <- renderText(as.character(userTableFrame$Var1[6]))
  output$user7 <- renderText(as.character(userTableFrame$Var1[7]))
  output$user8 <- renderText(as.character(userTableFrame$Var1[8]))
  output$user9 <- renderText(as.character(userTableFrame$Var1[9]))
  output$user10 <- renderText(as.character(userTableFrame$Var1[10]))
  
  
  
  
}
shinyApp(ui = ui, server = server)





