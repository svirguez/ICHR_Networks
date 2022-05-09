library(igraph)
library(tidyr)
data_CCC <- read.csv("CCC_Amici.csv")
##New column count
data_CCC$count <- 1

##Gather the data at case level
lev_data_CCC <- pivot_wider(data_CCC,id_cols = case, names_from = `amici_name`, 
                            values_from = count, values_fn = list(count = length), 
                            values_fill = list(count = 0))

##transpose lev_data to have amici grouped by case
library(data.table)
T_lev_data_CCC <- transpose(lev_data_CCC,)
rownames(T_lev_data_CCC) <- colnames(lev_data_CCC)
colnames(T_lev_data_CCC) <- lev_data_CCC$case
T_lev_data_CCC <- T_lev_data_CCC[-c(1),]

##create affiliation network graph using 'igraph'
Aff_network_CCC <- graph.incidence(T_lev_data_CCC)

#One-Mode matrix (actor x actor)

##extracting the one-mode projection
Aff_network_CCC.pr <- bipartite.projection(Aff_network_CCC)

##Actor x actor adjacency matrix

amici_CCC <- Aff_network_CCC.pr$proj1

amici_CCC <- graph.adjacency(get.adjacency(amici_CCC, sparse = FALSE,attr = "weight"))


summary(rowSums(select(T_lev_data_CCC,-1)>0, na.rm = TRUE))


visIgraph(amici_CCC)

visigr

V(Aff_network_CCC)$color <- c("orange", "steel blue")[V(Aff_network_CCC)$type+1]
V(Aff_network_CCC)$shape <- c("circle", "square")[V(Aff_network_CCC)$type+1]
plot(Aff_network_CCC, vertex.label=NA, vertex.size=7, layout=layout.bipartite) 

################### VisNetwork + Co-Ocurrence #######################

library(haven)
library(tidyverse)
library(cooccur)
library(visNetwork)

# read in data
data2 <- read_csv("CCC_Amici.csv")
data2$count <- 1

dim(data)

# select only columns we need
data2 <- data2 %>% select(case, amici_name, amici_type, count)

# gather data at case level
# coocData <- data %>% pivot_wider(names_from = amici_name, values_from = count, values_fn = length, values_fill = 0)
coocData <- pivot_wider(data2, id_cols = case, names_from = amici_name, values_from = count, values_fn = list(count = length), values_fill = list(count = 0))

# create co-occurrence matrix
myMatrix <- t(as.matrix(select(coocData, -case)))

# calculate co-occurrence statistics
coocs <- cooccur(myMatrix, spp_names = TRUE)

# create nodes data
nodes2 <- data.frame(id = 1:nrow(myMatrix),
                    label = rownames(myMatrix))

##NODE BY TYPE
node_type <- node_type %>% distinct(id, .keep_all = TRUE)

names(node_type)[names(node_type) == "amici_type"] <- "group"

# create edges data
edges <- data.frame(from = coocs$results$sp1, to = coocs$results$sp2,
                    width = log(coocs$results$obs_cooccur))


# create network graph
visNetwork(nodes = nodes2, edges = edges) %>%
  visIgraphLayout(layout = "layout_with_kk")

visNetwork(nodes = node_type, edges = edges) %>% 
  # darkblue square with shadow for group "A"
  visGroups(groupname = "THINK", color = "darkorange") %>% 
  visGroups(groupname = "UNIVERSITY", color = "green") %>%
  visGroups(groupname = "ADVOCACY", color = "lightblue") %>% 
  visGroups(groupname = "LABOR", color = "red") %>% 
  visGroups(groupname = "TRADE", color = "yellow") %>% 
  visGroups(groupname = "INDIVIDUAL", color = "blue") %>%
  visLegend(width = 0.1, position = "right", main = "Amici Type") %>%
  visOptions(highlightNearest = TRUE) %>%
  visLayout(randomSeed = 123)


############### Network Description ###################

vcount(amici_CCC)
ecount(amici_CCC)
igraph::components(amici_CCC)
diameter(amici_CCC, directed = F)
centr_degree(amici_CCC, loops = FALSE, mode = "total")$centralization
transitivity(amici_CCC)

############### Centrality Measures ####################

library(dplyr)
#dataframe for the nodes + degree
amici.nodesCCC<-data.frame(name=V(amici_CCC)$name, degree=igraph::degree(amici_CCC))
amici.nodesCCC <- subset(amici.nodesCCC, select = -name)

#eigenvector centrality
temp<-centr_eigen(amici_CCC,directed=F)
amici.nodesCCC$eigen<-temp$vector

#bonacich power centrality
amici.nodesCCC$bonpow <- power_centrality(amici_CCC, exponent = 0.9)

#top 5 eigenvector centrality
amici.nodesCCC%>%
  arrange(desc(close))%>%
  slice(1:5)


amici.nodesCCC%>%
  arrange(desc(degree))%>%
  slice(1:5)

#Betweenness centrality
amici.nodesCCC$between<-igraph::betweenness(amici_CCC, directed=FALSE)

amici.nodesCCC%>%
  arrange(desc(between))%>%
  slice(1:5)


################################# vertex attributes ###################

CCC_nodes <- read.csv('CCC_nodes.csv')

df <- igraph::as_data_frame(amici_CCC, 'both')

df$vertices <- df$vertices %>% 
  left_join(CCC_nodes, c('name'='amici_name'))

updated_g <- graph_from_data_frame(df$edges,
                                   directed = F,
                                   vertices = df$vertices)

amici_CCC <- updated_g


################# ERGM ####################

library(intergraph)
library(ergm)
library(jtools)

# Creating the new format network
CCC_network <- intergraph::asNetwork(amici_CCC)


DSmod2 <- ergm(CCC_network ~ edges +
                 nodematch('amici_type', diff=T),
               control = control.ergm(seed = 40))

summary(DSmod2)




