library(shiny)
library(Lahman)
library(xml2)
library(ggplot2)
library(rsconnect)
library(dplyr)
library(xml2)
library(rvest)
library(xlsx)
library(lubridate)

# BATTING DATA
data(Batting)
b<- Batting %>%
  group_by(yearID) %>%
  summarise(AB = sum(AB), R = sum(R), H = sum(H), HR = sum(HR), SB= sum(SB), CS= sum(CS), BB= sum(BB), SO= sum(SO))

p<- read_html("https://www.foxsports.com/mlb/team-stats?season=2017&category=BATTING&sort=13&time=0")
x<- p %>%
  html_nodes(xpath= '//*[@id="wisfoxbox"]/section[2]/div[1]/table') %>%
  html_table()
b2017<- x[[1]]
b17<- c(yearID= 2017, AB=sum(b2017$AB), R= sum(b2017$R), H= sum(b2017$H), HR= sum(b2017$HR), 
        SB= sum(b2017$SB), CS= sum(b2017$CS), BB= sum(b2017$BB), SO= sum(b2017$SO))
bDF<- rbind(b,b17)
bDF$SOrate<- round(with(bDF, SO/AB),3)
bDF$HRrate<- round(with(bDF, HR/AB), 3)
bDF$BBrate<- round(with(bDF, BB/AB),3)
bDF$succsteal<- round(with(bDF, SB/(SB + CS)),3)
bDF$BA<- round(with(bDF, H/AB),3)

# ATTENDANCE DATA
gt<- read.csv("gametime.csv")
gt$Time.9I<- hm(gt$Time.9I)
gt$Time.9I<- with(gt, (hour(Time.9I) * 60) + minute(Time.9I))

gt1<- select(gt, c(Year, Time.9I, R.G, Pitchers.G, Attend.G))
gt1$timeCat<- ifelse(gt1$Time.9I<= 150, "Under 2.5 Hours", ifelse(gt1$Time.9I<= 165, "Between 2.5 & 2.75 Hours", 
                                                                  ifelse(gt1$Time.9I<= 180, "Between 2.75 Hours & 3 Hours", "Over 3 Hours")))
newDF<- merge(gt1, bDF, by.x= "Year", by.y = "yearID")
nDF<- select(newDF, c(Year, Time.9I, R.G, Pitchers.G, Attend.G, timeCat, SOrate, HRrate, BBrate, succsteal, BA, BBrate, SB, HR))
colnames(nDF) <- c("year", "time", "Runs per Game", "PperG", "AperG", "timeCat", "SO Rate", "HR Rate",
                   "Walk Rate", "Steal Success Rate", "Batting Average", "SB", "HR")

nDF$timeCat<- factor(nDF$timeCat, levels= c("Under 2.5 Hours", "Between 2.5 & 2.75 Hours", "Between 2.75 & 3 Hours", "Over 3 Hours"))
# Define UI ----
ui <- fluidPage(
  # Title Panel
  titlePanel("Data Visualization") ,
  
  # Side Bar with input options
  sidebarLayout(sidebarPanel(
    selectInput("variable", "Variable for Time Series Plot:", 
               c("Runs per Game" = "Runs per Game",
                 "Homerun Rate" = "HR Rate",
                 "Homeruns" = "HR",
                 "Walk Rate" = "Walk Rate",
                 "Stolen Bases" = "SB",
                 "Stolen Bases Success Rate" = "Steal Success Rate",
                 "Batting Average" = "Batting Average"
                 )),
    sliderInput("num", "Year Range:",min = 1947, max = 2017, step= 1, value = c(1947, 2017), sep = "")
    
  ),
  mainPanel(
    plotOutput("seasonplot") 
  )
))


# Define server logic ----
server <- function(input, output) {
  
  dat <- reactive({
    nDF[,c("year", input$variable, "timeCat", "AperG")]
  })
  
  df <- reactive({
    subset(dat(), year>= min(input$num) & year <= max(input$num))
  })
  
  output$seasonplot <- renderPlot({
    ggplot(df(), aes(x= year, y= df()[,2])) + geom_line() + geom_point(aes(color= timeCat, size= AperG), alpha= 0.5) +
      labs(y= input$variable, title= "How Baseball has Changed- Time Series", x= "Year", 
           color= "Length of Game", size= "Average Attendance per Game")
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)

