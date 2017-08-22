######################
## prep
######################

# set working dir
setwd('C:/Users/Andrew/Documents/pmc-analytical-data-mart/celeb_vectors')

# load libs
library(igraph)
library(plotly)
library(plyr)
library(curl)
library(sqldf)
library(magrittr)
library(networkD3)

######################
## data
######################

# read in graph
G <- read.graph("edges.gml", format = c("gml"))

# set to be directed
#G <- as.directed(G,mode="arbitrary")
G <- as.directed(G,mode="mutual")

# play around with some layouts
#L <- layout_nicely(G)
#L <- layout.circle(G)
#L <- layout.grid.3d(G)
#L <- layout.auto(G)
#L <- layout.bipartite(G)
#L <- layout.davidson.harel(G)
#L <- layout.drl(G)
#L <- layout.gem(G)
#L <- layout.mds(G)
#L <- layout.norm(G)
#L <- layout.show(G)
#L <- layout.sphere(G)
L <- layout.star(G, center = V(G)[1])
#L <- layout.fruchterman.reingold(G)
#L <- layout.kamada.kawai(G)
#L <- layout.spring(G)

# get vertices
vs <- V(G)

# get edglist
es <- as.data.frame(get.edgelist(G,names=TRUE))

# get counts
counts <- rbind(count(es$V1),count(es$V2))
node_size <- sqldf("select x as V2, sum(freq) as freq from counts group by 1")

#edge_attr(G, index = V(G))

# add some random noise to scale the node sizes a bit
#node_size$freq <- node_size$freq+(rnorm(n=length(node_size$freq))^2)
node_size$freq <- node_size$freq

Nv <- length(vs)
Ne <- length(es[1]$V1)

Xn <- L[,1]
Yn <- L[,2]

network <- plot_ly(type = "scatter", 
                   x = Xn, 
                   y = Yn, 
                   size=node_size$freq ,
                   mode = "markers", #mode = "text", 
                   text = vs$label, 
                   hoverinfo = "text", #hoverinfo = "text",
                   textfont = list(size = 14),
                   textposition = 'middle top',
                   marker = list(color = "#512E5F")
                   )

edge_shapes <- list()
for(i in 1:Ne) {
  v0 <- es[i,]$V1
  v1 <- es[i,]$V2
  
  edge_shape = list(
    type = "line",
    line = list(color = "#b2b2d8", width = 0.5),
    x0 = Xn[v0],
    y0 = Yn[v0],
    x1 = Xn[v1],
    y1 = Yn[v1]
  )
  
  edge_shapes[[i]] <- edge_shape
}

network <- layout(
  network,
  title = 'Justin Bieber - Word Vector Network',
  shapes = edge_shapes,
  xaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
  yaxis = list(title = "", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
)
#network

# export to html
htmlwidgets::saveWidget(as.widget(network), "network.html")

######################
## networkD3 - simple
######################

# prep data for d3 network graph
id_to_label <- data.frame(cbind(id=vs$id+1,label=vs$label))
my_network <- merge(es,id_to_label,by.x='V1',by.y='id')
my_network <- merge(my_network,id_to_label,by.x='V2',by.y='id')
names(my_network) <- c('V1','V2','from','to')
my_network <- my_network[c('from','to')]

# make network D3 graph
simpleNetwork(my_network,
              #height = 900,
              #width = 600,
              #zoom=T,
              #charge=-100,
              linkDistance = 200,
              opacity = 0.99,
              linkColour = "#f4f4f4",
              nodeColour = "#3182bd",
              fontSize = 12,
              fontFamily = "calibri") %>%
  saveNetwork(file = 'networkD3_simple.html')

######################
## networkD3 - force
######################

# make graph
g <- graph.data.frame(my_network, directed=F) # raw graph

# Make a vertices df
vertices<-data.frame(
  name = V(g)$name,
  group = edge.betweenness.community(g)$membership,
  betweenness = (betweenness(g,directed=F,normalized=T)*150)+0.1 #so size isn't tiny
) 

# create indices (indexing needs to be JS format)
my_network$from.index = match(my_network$from, vertices$name)-1
my_network$to.index = match(my_network$to, vertices$name)-1

forceNetwork(Links = my_network, 
             Nodes = vertices,
             Source = 'from.index', Target = 'to.index',
             NodeID = 'name',
             Nodesize = 'betweenness',
             Group = 'group', # color nodes by group calculated earlier
             charge = -300, # node repulsion
             linkDistance = 50,
             opacity = 0.99,
             linkColour = "#AAB7B8",
             fontSize = 12,
             fontFamily = "calibri",
             opacityNoHover = 0.99,
             bounded=T,
             legend = F,
             colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);")) %>%
  saveNetwork(file = 'networkD3_force.html')