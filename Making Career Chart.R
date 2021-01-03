#CODE TO PRODUCE CAREER PATHWAYS VISUALIZATION

library(ipumsr)
library(dplyr)

#create data export on https://usa.ipums.org/usa/ for DEGFIELD and OCC variables for #2019 American Community and Save to Desktop.

ddi <- read_ipums_ddi("~/Desktop/usa_00005.xml")
data <- read_ipums_micro(ddi)

table(data$DEGFIELDD)

table(data$YEAR)

latest_data<-data %>%
  filter(YEAR==2019 & DEGFIELDD!=0 & OCC!=0)

edges<-as.data.frame(cbind(latest_data$DEGFIELDD, latest_data$OCC))

#create weighted ties
names(edges)<-c("source","target")


edges<-edges %>% group_by(source, target) %>%
  summarize(Count = n()) 

edges<-edges %>%
          filter(Count>200)

#drop problem edges
edges<-edges[edges$source!=2001 & edges$target!=2001,]
edges<-edges[edges$source!=2100 & edges$target!=2100,]
edges<-edges[edges$source!=2300 & edges$target!=2300,]



library(readr)
occ_codes<-read_csv('~/Desktop/occ_codes.csv')
degfield_codes<-read_csv('~/Desktop/degfield codes.csv')

nodes<-rbind(occ_codes, degfield_codes)
#drop nodes not in edgelist



nodes<-nodes[nodes$Code %in% c(edges$source, edges$target),]


#make network

edges$IDsource=match(edges$source, nodes$Code)-1 
edges$IDtarget=match(edges$target, nodes$Code)-1




#now make Sankey diagram
library(networkD3)

sankeyNetwork(Links = edges, Nodes = nodes,
              Source = "IDsource", Target = "IDtarget",
              Value = "Count", NodeID = "Label",
              height=2000, width=1300,
              units="people (out of 683,278) took this path", fontSize = 12,
              #nodePadding=2, 
              sinksRight = FALSE)

%>%
  saveNetwork(file = 'Career_Sankey.html')


?sankeyNetwork()




