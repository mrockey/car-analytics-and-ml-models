library(shiny)
library(ggplot2)
library(dplyr)
library(htmlwidgets)
library(shinythemes)

#Changes Ready for Final_App
 # Options list for data table

# Data loading ----
MyData <- read.csv('prediction_data-Copy1.csv')

#Reducing data
       MyData <- MyData %>% select(Make, Model, Year, Price, Predicted_price, Diff, Avg_Price,Min_Price,Max_Price, Mileage, Avg_Mileage, Min_Mileage, Max_Mileage, YoY_price_pct_change, YoY_mileage_pct_change, Count, State, City, Dealer_Name)

#Setting options for DataTables
options <- list(
  autoWidth = FALSE,
  searching = FALSE,
  ordering = FALSE,
  lengthChange = FALSE,
  lengthMenu = FALSE,
  #pageLength = 5, # Only show 5 rows per page.
  paging = FALSE, # Enable pagination. Must be set for pageLength to work.
  info = FALSE
)



# Define UI ----
ui <- fluidPage(theme = shinytheme("sandstone"),
  titlePanel("Finding the Best Deal on Your Next Vehicle"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Refine your search to show vehicles that meet your criteria."),
      
      selectInput("inCity", 
                  "Choose a City to display",
                  paste(sort(unique(MyData$City))),
                  selected = "Kansas City", multiple = TRUE),
        
      selectInput("Make", 
                  "Choose a Make(s)",
                  paste(sort(unique(MyData$Make))),
                  selected = 'Ford', multiple = TRUE),
        
        sliderInput("range",
                  label = "Max Price:",
                  min = 0, max = 150000, value = c(150000)),
                
        sliderInput("range2", 
                  label = "Max Mileage:",
                  min = 0, max = 150000, value = c(150000)),
        
        sliderInput("range3",
                  label = "Amount Below Average Price:",
                  min = -20000, max = 5000, value = c(5000)),
        
        h4("Click a Point on the Plot for More Details on the Vehicle"),
        dataTableOutput("click_info")
        
    ),
      
       mainPanel(
         plotOutput("plot1", click = "plot_click",
         brush = brushOpts(id = "plot1_brush"))),
      
  
  )#,
       #    fluidRow(
       # column(height = 6, width = 6,
       # h4("Click a Point on the Plot for More Details on the Vehicle"),
       # dataTableOutput("click_info")))
         
#verbatimTextOutput("click_info")    
    
        #   fluidRow(
        #column(height = 6, width = 6,
        #h4("Brushed points"),
        #dataTableOutput("click_info2")))
)

# Define server logic ---------------------------------------
server <- function(input, output) {
  

 ### Filtering the data based on reactive user inputs   
data <- reactive({filter(MyData, City == input$inCity , Price <= input$range , Mileage <= input$range2 ,
                        Make == input$Make , Diff <= input$range3)}) 


    
 ### Rendering the Plot - Name is cityPlot from above                 
output$plot1 <- renderPlot({ 
    
    cur_data <- data()
    cur_data2 <<- cur_data
   # y_axis <- reorder(cur_data$Model, -cur_data$Diff)
   # y_list <<- levels(y_axis)
    
    ggplot(data = data(),
             mapping = aes(x = Diff,
                           y = Model)) + geom_point(color = 'blue',size = 2.5) +
                                            scale_x_continuous(breaks = c(-20000,-15000,-10000,-8000,-6000,-4000,-2000,0,2000),
                                            labels = c('-20000','-15000','-10000','-8000','-6000','-4000','-2000','0','2000')) +
                                            guides(color=FALSE) + labs(x = "Price Difference From Predicted", y = "Models") +
                                            theme(axis.text=element_text(size=12),text=element_text(size=25))+ 
                                            labs(title = "The Best Deals by Model and Amount Below Predicted Price")  +
                                            geom_vline(xintercept = 0, color = "gray30") 
    
    })
    

 ### Created the object for displayed text
 output$click_info  <- renderDataTable({
     
      file <- data()
      options = options
     
         if (!is.null(input$plot_click$y)) # if clicked 

               {
      # factors of the continent attribute
      lvls <- levels(file$Model)
      lvls2 <-  levels(data()$Diff)      

      # ROUND the x coordinate to the nearest integer and FIND the name of that level
        
      name <- lvls[round(input$plot_click$y)]
      name2 <- lvls2[input$plot_click$x]
      
     nearPoints(file, input$plot_click, allRows = FALSE, threshold = 250, maxpoints=1)
             
                       
             
     # HTML("You've selected <code>", name, "</code>",
     #      "<br><br>Here are the first 10 rows that ",
     #     "match that category:")
    }  
        
  },options = options)   


#nearPoints(data(), input$plot_click,xvar=x, yvar=name     
    
 #output$click_info2 <- renderDataTable({
     
     
  #   file <- data()
     
   # if (!is.null(input$plot_click$y)) # if clicked 
   # {
      
      # the following line creates a boolean list: TRUE/FALSE values for matching continents
      # based on the mouse click x coordinate. 
     # keeprows <- round(input$plot_click$y) == as.numeric(file$Model) # (factors are ceonverted to numbers here)
     # keeprows_x <- input$plot_click$x 
      #keeprows_x <- input$plot_click$x == MyData$Diff
        
      # print head data, first 10 rows 
     # MyData[keeprows, ]
   # }

     
  #})
    
}

shinyApp(ui = ui, server = server)
