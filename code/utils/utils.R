# CONVERT: FROM mesh.1.5D TO SpatialLinesDataFrame --------------------------
setGeneric("as.SpatialLinesDataFrame", function(x) standardGeneric("as.SpatialLinesDataFrame"))

# CONVERT: TO mesh.1.5D --------------------------------------------------------
setGeneric("as.mesh.1.5D", function(x) standardGeneric("as.mesh.1.5D"))

# as.mesh.1.5D function overloading for SpatialLinesDataFrames
setMethod("as.mesh.1.5D", signature="SpatialLinesDataFrame", function(x){
  spat.stat.linnet = maptools::as.linnet.SpatialLines(x)
  nodes = cbind(spat.stat.linnet$vertices$x, spat.stat.linnet$vertices$y)
  edges = cbind(spat.stat.linnet$from, spat.stat.linnet$to)
  mesh = create.mesh.1.5D(nodes, edges)
})

# as.mesh.1.5D function overloading for linnet
setMethod("as.mesh.1.5D", signature="linnet", function(x){
  nodes = cbind(x$vertices$x, x$vertices$y)
  edges = cbind(x$from, x$to)
  mesh = create.mesh.1.5D(nodes, edges)
  return(mesh)
})

# as.mesh.1.5D function overloading for sfnetwork
setMethod("as.mesh.1.5D", signature="sfnetwork", function(x){
  nodes = st_coordinates(x, "nodes")
  edges = st_as_sf(x, "edges")
  edges = cbind(edges$from, edges$to) 
  mesh = create.mesh.1.5D(nodes, edges)
  return(mesh)
})

# CONVERT: FROM mesh.1.5D ------------------------------------------------------

# Converts fdaPDE Linear Network mesh into lattice objects. 
# returns a list containing a matrix of lattice nodes 
# sand a sparse adjacency matrix.
as.lattice <- function(mesh){
  
  # diffusionMaps nb. z coords are set to 0. 
  nodes.lattice = cbind(mesh$nodes, rep(0,times=nnodes))
  
  # Sparse matrix (spam package)
  adj_matrix = spam(0,nrow=nnodes, ncol=nnodes)
  nedges = nrow(mesh$edges)
  for( e in 1:nedges){
    sx = mesh$edges[e,1]
    dx = mesh$edges[e,2]
    
    adj_matrix[sx, dx] = 1
    adj_matrix[dx, sx] = 1
  }
  adj_matrix = spam::cleanup(adj_matrix)
  
  out = list(nodes.lattice = nodes.lattice,
             adj_matrix = adj_matrix)
  
  return(out)
}

# as.linnet function overloading for mesh.1.5D class 
as.linnet.mesh.1.5D <- function(X,...){
  vertices = ppp(x = X$nodes[,1], 
                 y = X$nodes[,2], 
                 window = owin(xrange = c(min(X$nodes[,1]),max(X$nodes[,1])),
                               yrange = c(min(X$nodes[,2]),max(X$nodes[,2]))))
  spat.stat.linnet = linnet(vertices = vertices, edges = X$edges, sparse = T)
  
  return(spat.stat.linnet)
}

# computing network distance matrix --------------------------------------------
as.LPP <- function(points_,L){
  return(as.lpp(x=points_[,1],y=points_[,2],L=L))
}

compute_dist_matrix <- function(points1, points2, L){
  lpp1 <- as.LPP(points1, L)
  lpp2 <- as.LPP(points2, L)
  ND <- crossdist.lpp(lpp1,lpp2)
  return( ND )
}

# MESH NORMALIZATION -----------------------------------------------------------
normalize.mesh <- function(mesh){
  
  x.m = mean(mesh$nodes[,1])
  y.m = mean(mesh$nodes[,2])
  
  x.sd = sd(mesh$nodes[,1])
  y.sd = sd(mesh$nodes[,1])
  
  x.norm = (mesh$nodes[,1] - x.m)/x.sd
  y.norm = (mesh$nodes[,2] - y.m)/y.sd
  
  mesh.norm = create.mesh.1.5D(nodes=cbind(x.norm,y.norm), edges = mesh$edges)  
  return(mesh.norm)
}

normalize.mesh.unit <- function(mesh){
  
  x.min = min(mesh$nodes[,1])
  y.min = min(mesh$nodes[,2])
  
  x.max = max(mesh$nodes[,1])
  y.max = max(mesh$nodes[,2])
  
  x.norm = (mesh$nodes[,1] - x.min)/(x.max-x.min)
  y.norm = (mesh$nodes[,2] - y.min)/(y.max-y.min)
  
  mesh.norm = create.mesh.1.5D(nodes=cbind(x.norm,y.norm), edges = mesh$edges)  
  
  ret = list(mesh=mesh.norm,
             x.min=x.min, y.min = y.min, x.max= x.max, y.max= y.max)
  
  return(ret)
}