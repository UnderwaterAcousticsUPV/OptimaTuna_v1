library(networkD3)
library(dplyr)

# 1-Enero-2022 : 31-Enero-2022
step1_source <- c()
step1_target <- c()
value1 <- c()

# 1-Febrero-2022 : 28-Febrero-2022
step2_source <- c()
step2_target <- c()
value2 <- c()

# 1-Marzo-2022 : 31-Marzo-2022
step3_source <- c()
step3_target <- c()
value3 <- c()

# 1-Abril-2022 : 30-Abril-2022
step4_source <- c()
step4_target <- c()
value4 <- c()

# 1-Mayo-2022 : 31-Mayo-2022
step5_source <- c()
step5_target <- c()
value5 <- c()

# 1-Junio-2022 : 30-Junio-2022
step6_source <- c()
step6_target <- c()
value6 <- c()

# Make a connection data frame
links <- data.frame(
  source=c(step1_source, step2_source, step3_source, step4_source, step5_source, step6_source), 
  target=c(step1_target, step2_target, step3_target, step4_target, step5_target, step6_target), 
  value=c(value1, value2, value3, value4, value5, value6)
)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% 
    unique()
)

# prepare color scale: I give one specific color for each node.
my_color <- 'd3.scaleOrdinal() .range([
"lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue",
"gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray",
"gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray",
"gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "red",
"gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "red",
"gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "red",
"gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "red"])'


# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network. I call my colour scale with the colourScale argument
p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name", units = "ind.", fontSize = 12, colourScale=my_color)
p