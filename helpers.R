library('igraph')
library('dplyr')

# this function takes a vector in the form vertices=c("bananans", "lemons", "yogurt")
#   and returns the subgraph that contains the intersection of their 
getIntersection <- function(graph, vertices){
  length.vertices <- length(vertices)
  
  if(length.vertices == 0){
    return(graph)
  }
  
  for(i in 1:length.vertices){
    subgraph.temp <- neighbors(graph, v=vertices[i], mode="all")
    if(i == 1){
      intersection.temp <- subgraph.temp
    }else{
      intersection.temp <- intersection(subgraph.temp, intersection.temp)
    }
  }
  intersection.vector <- as.vector(intersection.temp)
  intersection.subgraph <- induced.subgraph(graph, vids=intersection.vector)
  
  return(intersection.subgraph)
}


# this will plot the intersection using the output of getIntersection
# it uses the same input as getIntersection a vector in the form 
#   vertices=c("bananans", "lemons", "yogurt")
# color selection is "reds" or "greys"
plotGetIntersection <- function(graph, vertices, colors="reds", weightLabel=FALSE){
  intersection.subgraph <- getIntersection(graph, vertices)
  width <- E(intersection.subgraph)$weight
  if(colors == "greys"){
    color <- c("gray75", "gray50", "gray25", "gray0")[E(intersection.subgraph)$weight]
  }else if(colors == "grays"){
    color <- c("gray75", "gray50", "gray25", "gray0")[E(intersection.subgraph)$weight]
  }else if(colors == "reds"){
    color <- c("brown1","orangered", "orangered3", "firebrick4")[E(intersection.subgraph)$weight]
  }else{
    color <- rep("black", length(E(intersection.subgraph)))
  }
  
#  layout.fruchterman.reingold
#  layout <- layout_with_gem(intersection.subgraph)
  layout <- layout_with_fr(intersection.subgraph, 
                           weights=E(intersection.subgraph)$weight)
  
      intersection.subgraph$degree <- degree(intersection.subgraph)
  
      # Add cluster labels to coords
      coords = data.frame(layout, degree=intersection.subgraph$degree)
      
      # Move closer by a fraction "f" of mean distance between clusters
      f = 0.6
      
      # Shift each node closer to the overall center of mass of the node
      coords = coords %>% 
        mutate(X1 = ifelse(degree >= 1, X1 - f*(mean(X1[degree >= 1]) - mean(X1)), X1 - f*(mean(X1[degree == 0]) - mean(X1))),
               X2 = ifelse(degree >= 1, X2 - f*(mean(X2[degree >= 1]) - mean(X2)), X2 - f*(mean(X2[degree == 0]) - mean(X2))))
      
      # Convert coords back to original matrix form
      layout <- as.matrix(coords[,1:2])
  
  
  if(weightLabel==TRUE){
    plot(intersection.subgraph, vertex.size=2, vertex.label.cex=1, edge.arrow.size=.1*width^2,
         layout=layout, edge.width=1.5*sqrt(width), edge.label.cex=.8,
         edge.label=E(intersection.subgraph)$weight, main=paste("Intersection: ",
         paste(vertices, collapse=", ")), edge.color=color)
  }
  if(weightLabel==FALSE){
    plot(intersection.subgraph, vertex.size=2, vertex.label.cex=1, edge.arrow.size=.1*width^2,
         layout=layout, edge.width=1.5*sqrt(width), main=paste("Intersection: ",
         paste(vertices, collapse=", ")), edge.color=color)
  }
}

# this function returns a 
getIntersectionNames <- function(graph, vertexNames){
  if(length(vertexNames) == 0){
    return(sort(V(graph)$name))
  }else{
    return(sort(V(getIntersection(graph, vertexNames))$name))
  }  
}

getDegree <- function(graph){
  degree.graph <- as.data.frame(degree(graph))
  degree.graph[, 2] <- as.vector(V(graph)$name)
  degree.graph <- degree.graph[ , c(2, 1)]
  colnames(degree.graph) <- c("Flavors", "Degree")
  return(degree.graph)
}


food.graph <- readRDS(file='igraph-food.Rds')
# diameter(food.graph)
vertexNames <- getIntersectionNames(food.graph, vertexNames=c())


#vertices <- c("curry powder", "chicken")
#vertices <- c("chickpeas", "apples")

#plotGetIntersection(intersection.subgraph, vertices)
#intersection.vertices <- V(getIntersection(food.graph, vertices))$name
#intersection.subgraph <- getIntersection(food.graph, vertices)
#E(getIntersection(food.graph, vertices))$weight


# see if I can put the number of edges in the choosing list
#   can use render table and degree(food.graph)
# selecting the -- in general should pick all of the subgroups as well (add on the edges?)
# add a random flavors button -- it would have to be iterative
# could show more data in the dataframe -- even data about the whole graph
# I could use the 'click' and 'dblclick' arguments to get the location of clicks on the 
#   plot. I could use this to select things, too

# figure out how to make the plot fill up the avialable space

# this is going to need to explanation as well. For example, reminding the 
#   viewer that all the vertices in the intersection are all connected to the 
#   choosen flavors as well
