library(shiny)
library(networkD3)
library(ggplot2)


# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Interactive Sankey Diagram with NetworkD3"),
  
  # Sidebar with a slider input for filtering observations on field1
  sidebarPanel(
    selectInput("filter1", "Filter by Priority:",
                list("All" = "All",
                     "High" = "HI", 
                     "Low" = "LO")),

    sliderInput("filter2", 
                "Adjust by Estimated Value Threshold:", 
                min = 0,
                max = 10000, 
                value = 0)  #,
    
    # selectInput("cost_or_rev", "View Cost or Revenue:",
    #             list("Cost" = "cost",
    #                  "Revenue" = "revenue"))
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Volume", sankeyNetworkOutput("sankey")), 
      tabPanel("Cost/Revenue", 
               selectInput("cost_or_rev", "View Cost or Revenue:",
                    list("Cost" = "cost",
                         "Revenue" = "revenue")), 
        plotOutput("ggplot"),
      selectInput("stack", "Grouped or Stacked:",
                  list("Stacked" = "stacked",
                       "Grouped" = "grouped")))
    ))
))