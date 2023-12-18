
library(networkD3)
library(dplyr)

# prepare color scale: I give one specific color for each node.
my_color <- 'd3.scaleOrdinal() .range([
"lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue", "lightskyblue",
"gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray",
"gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray",
"gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "red",
"gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "red",
"gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "red",
"gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "red",
"gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "red",
"gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "red", "gray", "gray",
"gray", "gray", "gray", "gray", "gray", "gray", "gray", "red", "gray", "gray", "gray", "gray", "gray",
"gray", "gray", "gray", "gray", "red", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray",
"red", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "gray", "red", "gray", "gray",])'

# Enjaulamientos : 30-Junio-2021
step1_source <- c("JAL02_Enjaulamientos ", "JAL02_Enjaulamientos ", "JAL03_Enjaulamientos ", "JAL04_Enjaulamientos ", "JAL05_Enjaulamientos ", "JAL06_Enjaulamientos ", "JAL07_Enjaulamientos ", "JAL11_Enjaulamientos ", "JAL14_Enjaulamientos ", "JAL18_Enjaulamientos ", "JAL19_Enjaulamientos ", "JAL20_Enjaulamientos ")
step1_target <- c("JAL01_Jun ", "JAL02_Jun ", "JAL03_Jun ", "JAL04_Jun ", "JAL05_Jun ", "JAL06_Jun ", "JAL07_Jun ", "JAL11_Jun ", "JAL14_Jun ", "JAL18_Jun ", "JAL19_Jun ", "JAL20_Jun ")
value1 <- c(640, 661, 2237, 2199, 2236, 874, 636, 1193, 2333, 2191, 930, 872)

# 1-Julio-2021 : 31-Julio-2021
step2_source <- c("JAL01_Jun ", "JAL02_Jun ", "JAL03_Jun ", "JAL04_Jun ", "JAL05_Jun ", "JAL06_Jun ", "JAL07_Jun ", "JAL11_Jun ", "JAL14_Jun ", "JAL18_Jun ", "JAL19_Jun ", "JAL20_Jun ")
step2_target <- c("JAL01_Jul ", "JAL02_Jul ", "JAL03_Jul ", "JAL04_Jul ", "JAL05_Jul ", "JAL06_Jul ", "JAL07_Jul ", "JAL11_Jul ", "JAL14_Jul ", "JAL18_Jul ", "JAL19_Jul ", "JAL20_Jul ")
value2 <- c(641, 661, 2237, 2199, 2236, 874, 636, 1193, 2333, 2191, 930, 872)

# 1-Agosto-2021 : 31-Agosto-2021
step3_source <- c("JAL01_Jul ", "JAL02_Jul ", "JAL03_Jul ", "JAL04_Jul ", "JAL05_Jul ", "JAL06_Jul ", "JAL07_Jul ", "JAL11_Jul ", "JAL14_Jul ", "JAL18_Jul ", "JAL19_Jul ", "JAL20_Jul ", "JAL06_Jul ")
step3_target <- c("JAL01_Aug ", "JAL02_Aug ", "JAL03_Aug ", "JAL04_Aug ", "JAL05_Aug ", "JAL06_Aug ", "JAL07_Aug ", "JAL11_Aug ", "JAL14_Aug ", "JAL18_Aug ", "JAL19_Aug ", "JAL20_Aug ", "Sacrificios_Aug ")
value3 <- c(640, 661, 2237, 2199, 2236, 561, 636, 1193, 2333, 2191, 930, 872, 313)

# 1-Septiembre-2021 : 30-Septiembre-2021
step4_source <- c("JAL01_Aug ", "JAL02_Aug ", "JAL03_Aug ", "JAL04_Aug ", "JAL05_Aug ", "JAL06_Aug ", "JAL07_Aug ", "JAL11_Aug ", "JAL14_Aug ", "JAL18_Aug ", "JAL19_Aug ", "JAL20_Aug ", "JAL03_Aug ", "JAL04_Aug ", "JAL06_Aug ", "JAL07_Aug ", "JAL19_Aug ", "Sacrificios_Aug ")
step4_target <- c("JAL01_Sep ", "JAL02_Sep ", "JAL03_Sep ", "JAL04_Sep ", "JAL05_Sep ", "JAL06_Sep ", "JAL07_Sep ", "JAL11_Sep ", "JAL14_Sep ", "JAL18_Sep ", "JAL19_Sep ", "JAL20_Sep ", "Sacrificios_Sep ", "Sacrificios_Sep ", "Sacrificios_Sep ", "Sacrificios_Sep ", "Sacrificios_Sep ", "Sacrificios_Sep ")
value4 <- c(640, 661, 1731, 2100, 2236, 202, 633, 1193, 2333, 2191, 720, 872, 506, 99, 359, 3, 210, 313)

# 1-Octubre-2021 : 31-Octubre-2021
step5_source <- c("JAL01_Sep ", "JAL02_Sep ", "JAL03_Sep ", "JAL04_Sep ", "JAL05_Sep ", "JAL06_Sep ", "JAL06_Sep ", "JAL07_Sep ", "JAL11_Sep ", "JAL14_Sep ", "JAL18_Sep ", "JAL19_Sep ", "JAL20_Sep ", "JAL03_Sep ", "JAL04_Sep ", "JAL11_Sep ", "Sacrificios_Sep ")
step5_target <- c("JAL01_Oct ", "JAL02_Oct ", "JAL03_Oct ", "JAL04_Oct ", "JAL05_Oct ", "JAL06_Oct ", "JAL05_Oct ", "JAL07_Oct ", "JAL11_Oct ", "JAL14_Oct ", "JAL18_Oct ", "JAL19_Oct ", "JAL20_Oct ", "Sacrificios_Oct ", "Sacrificios_Oct ", "Sacrificios_Oct ", "Sacrificios_Oct ")
value5 <- c(640, 661, 1474, 1071, 2236, 45, 157, 633, 558, 2333, 2191, 720, 872, 257, 1029, 635, 1490)

# 1-Noviembre-2021 : 30-Noviembre-2021
step6_source <- c("JAL01_Oct ", "JAL02_Oct ", "JAL03_Oct ", "JAL04_Oct ", "JAL05_Oct ", "JAL06_Oct ", "JAL06_Oct ", "JAL07_Oct ", "JAL11_Oct ", "JAL14_Oct ", "JAL18_Oct ", "JAL19_Oct ", "JAL20_Oct ", "JAL03_Oct ", "JAL04_Oct ", "JAL11_Oct ", "Sacrificios_Oct ")
step6_target <- c("JAL01_Nov ", "JAL02_Nov ", "JAL03_Nov ", "JAL04_Nov ", "JAL05_Nov ", "JAL05_Nov ", "JAL06_Nov ", "JAL07_Nov ", "JAL11_Nov ", "JAL14_Nov ", "JAL18_Nov ", "JAL19_Nov ", "JAL20_Nov ", "Sacrificios_Nov ", "Sacrificios_Nov ", "Sacrificios_Nov ", "Sacrificios_Nov ")
value6 <- c(640, 661, 1474, 1071, 2236, 157, 45, 633, 558, 1488, 2191, 720, 872, 257, 1029, 635, 3411)

# 1-Diciembre-2021 : 31-Diciembre-2021
step7_source <- c("JAL01_Nov ", "JAL02_Nov ", "JAL03_Nov ", "JAL04_Nov ", "JAL05_Nov ", "JAL06_Nov ", "JAL07_Nov ", "JAL11_Nov ", "JAL14_Nov ", "JAL18_Nov ", "JAL19_Nov ", "JAL20_Nov ", "JAL04_Nov ", "JAL05_Nov ", "JAL11_Nov ", "JAL14_Nov ", "JAL18_Nov ", "JAL20_Nov ", "Sacrificios_Nov ")
step7_target <- c("JAL01_Dec ", "JAL02_Dec ", "JAL03_Dec ", "JAL04_Dec ", "JAL05_Dec ", "JAL06_Dec ", "JAL07_Dec ", "JAL11_Dec ", "JAL14_Dec ", "JAL18_Dec ", "JAL19_Dec ", "JAL20_Dec ", "Sacrificios_Dec ", "Sacrificios_Dec ", "Sacrificios_Dec ", "Sacrificios_Dec ", "Sacrificios_Dec ", "Sacrificios_Dec ", "Sacrificios_Dec ")
value7 <- c(640, 611, 1474, 922, 1365, 45, 633, 205, 1488, 1035, 720, 678, 149, 1028, 353, 845, 1156, 194, 5332)

# 1-Enero-2022 : 31-Enero-2022
step8_source <- c("JAL01_Dec ", "JAL02_Dec ", "JAL03_Dec ", "JAL04_Dec ", "JAL05_Dec ", "JAL06_Dec ", "JAL07_Dec ", "JAL11_Dec ", "JAL14_Dec ", "JAL18_Dec ", "JAL19_Dec ", "JAL20_Dec ", "JAL03_Dec ", "JAL04_Dec ", "Sacrificios_Dec ")
step8_target <- c("JAL01_Jan ", "JAL02_Jan ", "JAL03_Jan ", "JAL04_Jan ", "JAL04_Jan ", "JAL06_Jan ", "JAL04_Jan ", "JAL11_Jan ", "JAL14_Jan ", "JAL18_Jan ", "JAL19_Jan ", "JAL20_Jan ", "Sacrificios_Jan ", "Sacrificios_Jan ", "Sacrificios_Jan ")
value8 <- c(640, 245, 245, 848, 537, 45, 206, 4, 1689, 1035, 720, 194, 1158, 74, 9057)

# 1-Febrero-2022 : 28-Febrero-2022
step9_source <- c("JAL01_Jan ", "JAL02_Jan ", "JAL03_Jan ", "JAL04_Jan ", "JAL06_Jan ", "JAL11_Jan ", "JAL14_Jan ", "JAL18_Jan ", "JAL19_Jan ", "JAL20_Jan ", "JAL04_Jan ", "JAL18_Jan ", "Sacrificios_Jan ")
step9_target <- c("JAL01_Feb ", "JAL02_Feb ", "JAL03_Feb ", "JAL03_Feb ", "JAL06_Feb ", "JAL11_Feb ", "JAL14_Feb ", "JAL18_Feb ", "JAL19_Feb ", "JAL20_Feb ", "Sacrificios_Feb ", "Sacrificios_Feb ", "Sacrificios_Feb ")
value9 <- c(640, 661, 245, 1073, 45, 4, 1689, 1026, 720, 678, 518, 9, 10289)

# 1-Marzo-2022 : 31-Marzo-2022
step10_source <- c("JAL01_Feb ", "JAL02_Feb ", "JAL02_Feb ", "JAL03_Feb ", "JAL06_Feb ", "JAL11_Feb ", "JAL14_Feb ", "JAL18_Feb ", "JAL19_Feb ", "JAL20_Feb ", "JAL01_Feb ", "JAL02_Feb ", "JAL18_Feb ", "Sacrificios_Feb ")
step10_target <- c("JAL01_Mar ", "JAL02_Mar ", "JAL03_Mar ", "JAL03_Mar ", "JAL06_Mar ", "JAL11_Mar ", "JAL14_Mar ", "JAL18_Mar ", "JAL19_Mar ", "JAL20_Mar ", "Sacrificios_Mar ", "Sacrificios_Mar ", "Sacrificios_Mar ", "Sacrificios_Mar ")
value10 <- c(143, 321, 340, 1318, 45, 4, 1689, 786, 720, 678, 497, 2, 240, 10816)

# 1-Abril-2022 : 30-Abril-2022
step11_source <- c("JAL01_Mar ", "JAL03_Mar ", "JAL02_Mar ", "JAL06_Mar ", "JAL11_Mar ", "JAL14_Mar ", "JAL18_Mar ", "JAL19_Mar ", "JAL20_Mar ", "JAL02_Mar ", "JAL03_Mar ", "JAL14_Mar ", "Sacrificios_Mar ")
step11_target <- c("JAL01_Apr ", "JAL03_Apr ", "JAL07_Apr ", "JAL06_Apr ", "JAL11_Apr ", "JAL14_Apr ", "JAL18_Apr ", "JAL19_Apr ", "JAL20_Apr ", "Sacrificios_Apr ", "Sacrificios_Apr ", "Sacrificios_Apr ", "Sacrificios_Apr ")
value11 <- c(143, 1179, 118, 45, 4, 1455, 786, 720, 678, 201, 479, 234, 11555)

# 1-Mayo-2022 : 31-Mayo-2022
step12_source <- c("JAL01_Apr ", "JAL01_Apr ", "JAL03_Apr ", "JAL06_Apr ", "JAL11_Apr ", "JAL14_Apr ", "JAL18_Apr ", "JAL19_Apr ", "JAL20_Apr ", "JAL03_Apr ", "JAL14_Apr ", "Sacrificios_Apr ")
step12_target <- c("JAL01_May ", "JAL03_May ", "JAL03_May ", "JAL06_May ", "JAL11_May ", "JAL14_May ", "JAL18_May ", "JAL19_May ", "JAL20_May ", "Sacrificios_May ", "Sacrificios_May ", "Sacrificios_May ")
value12 <- c(78, 65, 723, 45, 4, 782, 786, 720, 678, 456, 673, 12469)

# Make a connection data frame
links <- data.frame(
  source=c(step1_source, step2_source, step3_source, step4_source, step5_source, step6_source, step7_source, step8_source, step9_source, step10_source, step11_source, step12_source), 
  target=c(step1_target, step2_target, step3_target, step4_target, step5_target, step6_target, step7_target, step8_target, step9_target, step10_target, step11_target, step12_target), 
  value=c(value1, value2, value3, value4, value5, value6, value7, value8, value9, value10, value11, value12)
)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% 
    unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network. I call my colour scale with the colourScale argument
p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name", units = "ind.", fontSize = 12, colourScale=my_color)

# Create plot
print(p)