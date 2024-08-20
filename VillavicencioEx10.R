#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("VillavicencioEx08.R")
source("VillavicencioEx09.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    navbarPage( "VillavicencioEx10",
        tabPanel("Quadratic Spline Interpolation",
                 sidebarPanel(
                    tags$h3("Input: "),
                    tags$small("Example: 1, 4.3, 2.5, 6"),
                    
                    textInput( "x", "x",value = "3.0, 4.5, 7.0, 9.0", placeholder ="Enter at least 3 values of x"),
                    textInput( "y", "y", value = "2.5, 1.0, 2.5, 0.5", placeholder="Enter at least 3 values of y"),
                    hr(),
                    tags$h3("Predict the value of y: "),
                    textInput( "predict", "When x =", value = "5", placeholder="Enter at a value of x"),
                    
                    verbatimTextOutput("predict_ans"),
                 ), #sidebarPanel
                 
                mainPanel(
                    tags$b("Your data:"),
                    br(),
                    tableOutput("tbl1"),
                    br(),
                    tags$b("Output functions:"),
                    verbatimTextOutput("fxns"),
                    br(),
                ),#mainpanel
        ), # tabpanel
        
        tabPanel("Optimizing Shipments",
             sidebarPanel(
                 tags$h3("Supply: "),
                 tags$small("Input in this order: DEN, PHO, DAL"),
                 textInput( "supply", NULL,value = "310, 260, 280", placeholder ="DEN, PHO, DAL"),
                 hr(),
                 tags$h3("Demand: "),
                 tags$small("Input in this order: SAC, SL, ALB, CHI, NYC"),
                 textInput( "demand", NULL,value = "180, 80, 200, 160, 220", placeholder ="SAC, SL, ALB, CHI, NYC"),
                 hr(),
                 tags$h3("Shipping Cost: "),
                 tags$small("Input in this order: SAC, SL, ALB, CHI, NYC"),
                 br(),
                 tags$b("DEN --> SAC, SL, ALB, CHI, NYC"),
                 textInput( "shipden", NULL,value = "10, 8, 6, 5, 4", placeholder ="SAC, SL, ALB, CHI, NYC"),
                 tags$b("PHO --> SAC, SL, ALB, CHI, NYC"),
                 textInput( "shippho", NULL,value = "6, 5, 4, 3, 6", placeholder ="SAC, SL, ALB, CHI, NYC"),
                 tags$b("DEN --> SAC, SL, ALB, CHI, NYC"),
                 textInput( "shipdal", NULL,value = "3, 4, 5, 5, 9", placeholder ="SAC, SL, ALB, CHI, NYC"),
             ), #sidebarPanel
             mainPanel(
                 tags$b("Number of items to be shipped from a plant to a warehouse:"),
                 br(),
                 tableOutput("tbl2"),
                 br(),
                 verbatimTextOutput("optval"),
                 br(),
             ),#mainpanel
        ),#tabpanel
    )
)

server <- function(input, output) {
    extract <- function(text) {
        text <- gsub(" ", "", text)
        text <- gsub("[\n]", "", text)
        split <- strsplit(text, ",", fixed = FALSE)[[1]]
        as.numeric(split)
    }
    output$fxns <- renderPrint ({
        x <- extract(input$x)
        y <- extract(input$y)
        
        my.data <- data.frame(x,y)
        names(my.data) <- c("x", "y")
        output$tbl1 <- renderTable(my.data, rownames = FALSE, colnames = TRUE)
        result <- poly.qsi(my.data, extract(input$predict))
        
        temp= c()
        for(i in result$qsi.fxns)
            temp = c(temp, deparse(i)[[2]])
        range = c()
        x_sort = sort(x)
        for(i in 1:(length(x)-1))
            range = c(range, paste(sort(x)[i],"<= x <=", sort(x)[i+1]))
        
        result2 <- data.frame(temp,range)
        names(result2) <- c("functions", "range")
        rownames(result2) <- paste("f",1:length(result2$functions),"(x)", sep="")
        result2
        
    })
    output$predict_ans <- renderPrint({
        x <- extract(input$x)
        y <- extract(input$y)
        
        my.data <- data.frame(x,y)
        names(my.data) <- c("x", "y")
        result <- poly.qsi(my.data, extract(input$predict))
        
        HTML(paste("then y is equal to", round((result$y)[1],digits = 4)))
    })
    
    
    
    output$tbl2 <- DT::renderDataTable({
        supplies <- extract(input$supply)
        # supplies <- c(supplies, 0, 0) # so that there will be equal lengths when it is added to the data frame
        demands <- extract(input$demand)
        cost1 <- extract(input$shipden)
        cost2 <- extract(input$shippho)
        cost3 <- extract(input$shipdal)
        costs <- list(cost1, cost2, cost3)
            
        if(sum(supplies)>sum(demands)){
            inputs <- list(supplies, demands, costs)
            names(inputs) <- c("supplies", "demands", "costs")
            tableau <- construct_tableau(inputs)
            result <- simplex(tableau, FALSE, TRUE) 
            display <- as.data.frame(result$shipping.num)
            DT::datatable(display, class = 'cell-border stripe')
        }
    })
    output$optval <- renderPrint({
        supplies <- extract(input$supply)
        demands <- extract(input$demand)
        cost1 <- extract(input$shipden)
        cost2 <- extract(input$shippho)
        cost3 <- extract(input$shipdal)
        costs <- list(cost1, cost2, cost3)
        
        if(sum(supplies)>sum(demands)){
            inputs <- list(supplies, demands, costs)
            names(inputs) <- c("supplies", "demands", "costs")
            tableau <- construct_tableau(inputs)
            result <- simplex(tableau, FALSE, TRUE) 
            output$tbl2 <- renderTable(result$shipping.num, rownames = TRUE, colnames = TRUE)
            HTML(paste("Minimum shipping cost: ", result$opt.val))
        }else{
            HTML(paste("Demand is higher than supply. No feasible slution."))
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
