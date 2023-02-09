library(shiny)
library(readxl)
library(ggplot2)
library(data.table)
library(ChainLadder)

ui <- fluidPage(
  titlePanel("R Shiny Assessment"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Upload file', accept = c(".xlsx")),
      numericInput('tail1', 'Tail factor', value = 1.1)
    ),
    
    mainPanel(
      tableOutput('table1'),
      tags$hr(),
      plotOutput('plot1', width = "100%"))
  )
)

server <- function(input, output) {
  
  output$table1 <- renderTable(
    {
      if(is.null(input$file1))     
        return(NULL) 
      
      data <- read_excel(input$file1$datapath, col_names = TRUE)
      
      names(data) <- c("LossYear", "DevYear", "PaidClaims")
      
      actual_data <- data.table(data)
      actual_data[, CumulativeSum := cumsum(PaidClaims), by = c('LossYear')]
      actual_cumsum <- actual_data$CumulativeSum
      
      claims.tri <- as.triangle(actual_data,
                                origin= "LossYear",
                                dev="DevYear",
                                value= "CumulativeSum")
      
      # Calculate age-to-age factors for triangle
      n <- length(unique(unlist(data[, "DevYear"])))
      f <- sapply(1:(n-1),
                  function(i){
                    sum(claims.tri[c(1:(n-i)),i+1])/sum(claims.tri[c(1:(n-i)),i])
                  }
      )
      
      # Tail factor
      f.tail <- input$tail1
      
      # Forecast
      f <- c(f, f.tail)
      full_claims <- cbind(claims.tri,  "4" = rep(0, n))
      for(k in 1:n){
        full_claims[(n-k+1):n, k+1] <- full_claims[(n-k+1):n,k]*f[k]
      }
      
      # Create data frame
      full_claims <- format(full_claims, nsmall=2, big.mark=",")
      LossYear <- rownames(full_claims)
      full_claims.df <- as.data.frame(cbind(LossYear,(full_claims)))
      full_claims.df
    },
    
    include.rownames=FALSE,
    add.to.row = list(pos = list(0), command = "<tr><th></th><th colspan='5'>Development Year</td></tr>
<tr> <th> Loss Year </th> <th> 1 </th> <th> 2 </th> <th> 3 </th> <th> 4 </th>  </tr>"),
    include.colnames=FALSE,
    caption.placement = "top",
    caption = "Cumulative Paid Claims ($)"
  )
  
  
  output$plot1 <- renderPlot({
    
    inFile <- input$file1
    if(is.null(inFile))
      return(NULL)
    
    data <- read_excel(input$file1$datapath, col_names = TRUE)
    
    names(data) <- c("LossYear", "DevYear", "PaidClaims")
    
    actual_data <- data.table(data)
    actual_data[, CumulativeSum := cumsum(PaidClaims), by = c('LossYear')]
    actual_cumsum <- actual_data$CumulativeSum
    
    claims.tri <- as.triangle(actual_data,
                              origin= "LossYear",
                              dev="DevYear",
                              value= "CumulativeSum")
    
    # Calculate age-to-age factors for triangle
    n <- length(unique(unlist(data[, "DevYear"])))
    f <- sapply(1:(n-1),
                function(i){
                  sum(claims.tri[c(1:(n-i)),i+1])/sum(claims.tri[c(1:(n-i)),i])
                }
    )
    
    # Tail factor
    f.tail <- input$tail1
    
    # Forecast
    f <- c(f, f.tail)
    full_claims <- cbind(claims.tri, "4" = rep(0, n))
    for(k in 1:n){
      full_claims[(n-k+1):n, k+1] <- full_claims[(n-k+1):n,k]*f[k]
    }
    
    # Create data frame for plotting
    full_claims.df <- as.data.frame(full_claims)
    full_claims.df["LossYear"] <- rownames(full_claims.df)
    df.molten <- melt(full_claims.df, id.vars="LossYear", value.name="CPC", variable.name="DevYear" )
    
    #Plot
    ggplot(df.molten, aes( x = DevYear, group= LossYear, color=factor(LossYear), fill=factor(LossYear)) ) +
      geom_line(aes(y = CPC), position = "identity", stat = "identity", alpha = .3 ) + geom_point(aes(y=CPC),stat="identity") + geom_text(aes(label=format(CPC, nsmall=2, big.mark=","), y=CPC, color=factor(LossYear)),position = "identity",vjust=-1, hjust=0.5, size=3, show.legend = FALSE) + ylim(min(full_claims),max(full_claims)) + labs(title="Cumulative paid claims ($)",x="Development year", y="Claims paid", color="Loss year", fill="Loss year", size=4) + theme(plot.title=element_text(hjust=0.5))
  })
}
shinyApp(ui, server)
