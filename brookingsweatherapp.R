
library(readxl)
weather <- read_excel("weather.xlsx", 
                      col_types = c("date", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "text"))
#View(weather)
library(dplyr)
weather1<- weather[-60,2:8]
weather1$Month<- factor(weather1$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August","September","October","November", "December"))
weather2 <- as.data.frame(weather1)


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Brookings Weather"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(selectInput( "cid" , "column", choices= colnames(weather2[,1:6])),
                 sliderInput("bins",
                             "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 10)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- weather2[, input$cid]
    if (input$cid %in% colnames(weather2[,1:2])){
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      titl <-paste("Histogram of",  input$cid , sep =" ")
      hist(x, breaks = bins, col = rainbow(10), main = titl , 
               xlab = "Degrees Fahrenheit", ylab= "Days",border = 'white')
    }else if(input$cid %in% colnames(weather2[,c(3,5)])){
      titl <-paste("Boxplot plot of", input$cid, sep =" ")
      boxplot(x~weather2$Month, col=rainbow(12), ylab="Inches", xlab= "Month", main= titl, las=2 )
    }else{
      titl <-paste("Plot of",  input$cid , sep =" ")
      plot(x, type= "l",col = "Blue", lwd= 3, xlab= "Days", ylab= "Total Precip (in)", 
               main = titl )
    }
  })
}

# Run the application 

shinyApp(ui = ui, server = server)

