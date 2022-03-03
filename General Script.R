#Create an adjacency network from the scratch

library(tidyr)
data <- read.csv("CleanedData.csv")
##New column count
data$count <- 1

##Gather the data at case level
lev_data <- pivot_wider(data,id_cols = CaseID, names_from = `Name.of.Amicus`, values_from = count, values_fn = list(count = length), values_fill = list(count = 0))

##Create the adjacency matrix
mat <- as.matrix(lev_data[-1])
ad_matrix <- t(mat) %*% mat

##Ignore the ties from a node to itself 
diag(ad_matrix) <- 0


#adjacency graph object
library(igraph)
amici_network <- graph.adjacency(ad_matrix, mode = "undirected", weighted = TRUE)

####################################################################################################

#Affiliation network data
library(igraph)
library(tidyr)
data <- read.csv("CleanedData.csv")
##New column count
data$count <- 1

##Gather the data at case level
lev_data <- pivot_wider(data,id_cols = CaseID, names_from = `Name.of.Amicus`, values_from = count, values_fn = list(count = length), values_fill = list(count = 0))

##transpose lev_data to have amici grouped by case
library(data.table)
T_lev_data <- transpose(lev_data,)
rownames(T_lev_data) <- colnames(lev_data)
colnames(T_lev_data) <- lev_data$CaseID
T_lev_data <- T_lev_data[-c(1),]

##create affiliation network graph using 'igraph'
Aff_network <- graph.incidence(T_lev_data)

#########

#Description of the graph

##size
vcount(Aff_network)
ecount(Aff_network)

##ties
is_bipartite(Aff_network)
is_directed(Aff_network)
is_weighted(Aff_network)

#########

#One-Mode matrix (actor x actor / case x case)

##extracting the one-mode projections 
Aff_network.pr <- bipartite.projection(Aff_network)
Aff_network.pr

##two different adjacency matrices

amici_net <- Aff_network.pr$proj1
case_net <- Aff_network.pr$proj2

amici_ad <- graph.adjacency(get.adjacency(amici_net, sparse = FALSE,attr = "weight"))
case_ad <- graph.adjacency(get.adjacency(case_net, sparse = FALSE,attr = "weight"))

#########

#Density, Degree, and Centrality of the Network

##density
graph.density(amici_ad)
graph.density(case_ad)

graph.density(Aff_network)

##centralization network (amici_ad)
centr_degree(amici_ad, loops = FALSE, mode = "total")$centralization

centr_degree(Aff_network, loops = FALSE, mode = "total")$centralization

##centrality by node (amici_ad)

###data frame
amici.nodes<-data.frame(name=V(amici_ad)$name, degree=igraph::degree(amici_ad))

aff.nodes<-data.frame(name=V(Aff_network)$name, degree=igraph::degree(Aff_network))

aff.nodes_amici <- aff.nodes[-c(404:482),]
aff.nodes_cases <- aff.nodes[-c(1:403),]

###distribution
hist(amici.nodes$degree, main = "Amici Influence: Degree Distribution", xlab = "Times filed a brief with other actor")

hist(aff.nodes_amici$degree, main = "Amici Influence: Degree Distribution", xlab = "Times filed a brief with other actor")

###arrange first three
library(dplyr)
arrange(amici.nodes, desc(degree))%>%slice(1:3)

arrange(aff.nodes_amici, desc(degree))%>%slice(1:3)
arrange(aff.nodes_cases, desc(degree))%>%slice(1:3)

#########

#Plot the affiliation network

##normal plot
V(Aff_network)$color <- c("orange", "steel blue")[V(Aff_network)$type+1]
V(Aff_network)$shape <- c("circle", "square")[V(Aff_network)$type+1]

plot(Aff_network, vertex.label=NA)

##bipartite plot
plot(Aff_network, vertex.label=NA, vertex.size=7, layout=layout.bipartite) 

#########

#eigen vector centrality

temp<-centr_eigen(Aff_network,directed=F)
aff.nodes$eigen<-temp$vector

head(centr_eigen(amici_net,directed=F)$vector)

#b power
head(power_centrality(Aff_network, exponent = 0.9))
aff.nodes$bonpow <- power_centrality(Aff_network, exponent = 0.9)


aff.nodes_amici <- aff.nodes[-c(404:482),]
aff.nodes_cases <- aff.nodes[-c(1:403),]

arrange(aff.nodes_amici, desc(degree))%>%
  slice(1:3)

arrange(amici.nodes, desc(bonpow))%>%
  slice(1:3)

#plot distribution centrality measures
library(ggplot2)
aff.nodes_amici%>%
  select(-name) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(~key, scales = "free")

#correlation
library(corrr)
library(seriation)

A<-aff.nodes_amici %>% 
  select(degree,eigen,bonpow)%>%
  correlate() %>%
  rearrange()

install.packages('seriation')

####################################################################################################

#Betweenness centrality
aff.nodes$between<-igraph::betweenness(Aff_network, directed=TRUE)
amici.nodes$between<-igraph::betweenness(amici_ad, directed=TRUE)
case.nodes$between<-igraph::betweenness(case_ad, directed=TRUE)

#Betweenness centralization

centr_betw(Aff_network,directed=F)$centralization
centr_betw(amici_ad,directed=F)$centralization
centr_betw(case_ad,directed=F)$centralization

#Constraint

aff.nodes$constraint<-constraint(Aff_network)
amici.nodes$constraint<-constraint(amici_ad)
case.nodes$constraint<-constraint(case_ad)

#clossenes

aff.nodes$close<-igraph::closeness(Aff_network)
amici.nodes$close<-igraph::closeness(amici_ad)
case.nodes$close<-igraph::closeness(case_ad)


plot(Aff_network)

library(dplyr)

amici.nodes%>%
  arrange(desc(between))%>%
  slice(1:5)

##########################################Co-Ocurrence Data##################################

library(cooccur)
library(visNetwork)
library(tidyr)


data <- read.csv("Cleaned_Data.csv")
##New column count
data$count <- 1

##Gather the data at case level
lev_data <- pivot_wider(data,id_cols = CaseID, names_from = "Name", values_from = count, values_fn = list(count = length), values_fill = list(count = 0))

library(data.table)
T_lev_data <- transpose(lev_data,)
rownames(T_lev_data) <- colnames(lev_data)
colnames(T_lev_data) <- lev_data$CaseID
T_lev_data <- T_lev_data[-c(1),]


co <- print(cooccur(T_lev_data, spp_names = TRUE))









