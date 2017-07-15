library(shiny)
library(dplyr)
library(networkD3)


# prepare dataset
nodes <- read.csv("data/nodes.csv")
raw_df <- read.csv("data/raw.csv", colClasses = c("numeric","character","character",
                                             "numeric","numeric"))

df <- raw_df
df$p1 <- substr(df$dna, start = 1, stop = 2)
df$p2 <- substr(df$dna, start = 2, stop = 3)
df$p3 <- substr(df$dna, start = 3, stop = 4)


shinyServer(function(input, output) {


  output$sankey <- renderSankeyNetwork({
    
    #filter the df
    if(input$filter1 == "All") {
    df <- filter(df, filt2 > input$filter2)
    }
    else
      df <- filter(df, filt1 == input$filter1, filt2 > input$filter2)
    
    # generate the sankey
    pair <- c(df$p1, df$p2, df$p3)
    pair <- pair[!pair %in% c("0", "1", "2","3","4","5","6","7","8","9")]
    tally <- data.frame(pair)

    links <- tally %>% group_by(pair) %>% summarize(value = n())
    links$source <- substr(links$pair, start = 1, stop = 1)
    links$target <- substr(links$pair, start = 2, stop = 2)
    links <- links %>% select(source, target, value)
    links$source <- as.numeric(links$source)
    links$target <- as.numeric(links$target)
    

    sankeyNetwork(Links = links, Nodes = nodes, Source = "source",
    Target = "target", Value = "value", NodeID = "name",
    units = "cases", fontSize = 12, nodeWidth = 30)
  })
  
  output$ggplot<-renderPlot({

    df$finalnode <- substr(df$dna, start = nchar(df$dna), stop = nchar(df$dna))
    
    #filter the df
    if(input$filter1 == "All") {
      df <- filter(df, filt2 > input$filter2)
    }
    else
      df <- filter(df, filt1 == input$filter1, filt2 > input$filter2)
    
    df_sum <- df %>% group_by(finalnode, outcome) %>%
      summarize(cost_average = mean(cost), revenue_average = mean(revenue))

    if(input$cost_or_rev == "cost"){

        ggplot(data = df, aes(factor(finalnode), cost, fill = outcome)) +
        geom_bar(stat = "identity") + coord_flip()
      }
    
    else 
        ggplot(data = df, aes(factor(finalnode), revenue, fill = outcome)) +
        geom_bar(stat = "identity") + coord_flip()
  })
  
})