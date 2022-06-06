# make a list of associations for turning into graph
make_association_list <- function(cophy) {
  association_matrix <- treeducken::association_mat(cophy)
  ht <- treeducken::drop_extinct(treeducken::host_tree(cophy))
  st <- treeducken::drop_extinct(treeducken::symb_tree(cophy))
  hosts <- ht$tip.label
  symbs <- st$tip.label
  cophy_list <- matrix(0, nrow = 1, ncol = 2)
  for(i in 1:nrow(association_matrix)) {
    is_zero <- sum(association_matrix[i,])
    if(is_zero){
      x <- which(association_matrix[i,] == 1)
      for(j in 1:length(x)) {
        h <- stringr::str_remove(hosts[i], pattern = "[H]")
        s <- stringr::str_remove(symbs[x[j]], pattern = "[S]")
        r <- c(as.numeric(h), as.numeric(s))
        cophy_list <- rbind(r, cophy_list)
        rownames(cophy_list) <- NULL
        #cophy_list <- rbind(r, cophy_list)
      }
    }
  }
  cophy_list <- cophy_list[1:nrow(cophy_list)-1,]
  return(cophy_list)
}

# convert cophylogeny into graph
cophylo_to_graph <- function(cophylo,
                             distHP=10){
  ht <- treeducken::drop_extinct(cophylo$host_tree)
  st <- treeducken::drop_extinct(cophylo$symb_tree)
  num_hosts <- length(ht$tip.label)
  num_symbs <- length(st$tip.label)

  cophy_list <- cophyloML::make_association_list(cophylo)

  cophy_list_p <- cophy_list
  cophy_list_p[,1] <- cophy_list_p[,1]


  btH = which( ape::branching.times(ht)>max(ape::branching.times(st)) ) + num_hosts

  ghp <- igraph::graph.edgelist(rbind(ht$edge,
                              st$edge+2*num_hosts - 1,
                              cophy_list_p,
                              cophy_list_p[,2:1]) )

  igraph::V(ghp)$size <- c(rep(3,num_hosts),
                   rep(1,num_hosts-1),
                   rep(3,num_symbs),
                   rep(1,num_symbs-1)) #tips twice as big
  igraph::V(ghp)$type <- c(rep(1,2*num_hosts-1),
                   rep(2,2*num_symbs-1))
  #4 types of vertices : (1, black) internal host vertex,
  # (3, green) external host vertex,  (grey) (2, red) idem for parasites
  igraph::V(ghp)$type[1:num_symbs+2*num_hosts-1] <- 4
  igraph::V(ghp)$type[1:num_hosts] <- 3
  igraph::V(ghp)$color = c(rep("black",2*num_hosts-1),
                   rep("grey",2*num_symbs-1))

  igraph::E(ghp)$type  = c(rep(1,nrow(ht$edge)),
                   rep(3,nrow(st$edge)),
                   rep(2,2*nrow(cophy_list)) )
  #3 types of edges : (1, black) H-H, (grey) P-P and (2, red) H-P
  igraph::E(ghp)$color = c(rep("black",nrow(ht$edge)),
                   rep("grey",nrow(st$edge)),
                   rep("red",2*nrow(cophy_list)) )
  igraph::E(ghp)$weight = abs( c(ht$edge.length,
                         st$edge.length,
                         rep(1,length(cophy_list))) )

  ghp2 <- igraph::delete.vertices(ghp,btH) #remove H nodes that are older than the P root
  layo <- matrix(0,nrow=length(igraph::V(ghp)),ncol=2)
  layo[1:(2*num_hosts-1),1] <- ape::node.depth.edgelength(ht)
  layo[1:(2*num_hosts-1),2] <- ape::node.height(ht)/max(ape::node.height(ht))
  layo[(2*num_hosts):(2*num_hosts+2*num_symbs-2),1] <- distHP +
    max(ape::node.depth.edgelength(ht)) + max(ape::node.depth.edgelength(st)) -
    ape::node.depth.edgelength(st)
  layo[(2*num_hosts):(2*num_hosts+2*num_symbs-2),2] <- ape::node.height(st)/max(ape::node.height(st))
  ghp$layout <- layo
  ghp2$layout <- ghp$layout[-btH,]
  return(ghp2)
}

# get treeducken stats
get_td_stats <- function(host_symb_sets) {
  td_stats <- treeducken::summarize_cophy(host_symb_sets, parafit = FALSE)
  td_stats[is.na(td_stats)] <- 0
  host_trees <- treeducken::host_tree(host_symb_sets)
  host_trees <- lapply(host_trees, FUN = treeducken::drop_extinct)
  host_tree_size <- list(length = length(host_trees))
  for(i in 1:length(host_trees)) {
    host_tree_size[[i]] <- length(host_trees[[i]]$tip.label)
  }
  symb_trees <- treeducken::symb_tree(host_symb_sets)
  symb_trees <- lapply(symb_trees, FUN = treeducken::drop_extinct)
  symb_tree_size <- list(length = length(symb_trees))
  for(i in 1:length(symb_trees)) {
    symb_tree_size[[i]] <- length(symb_trees[[i]]$tip.label)
  }
  td_stats <- cbind(td_stats,
                    HostTreeSize = unlist(host_tree_size),
                    SymbTreeSize = unlist(symb_tree_size))
  td_stats
}

make_stats_df <- function(graph_list) {
  stats_df <- matrix(nrow = 0, ncol = 11)
  for(i in graph_list) {
    s <- cophyloML::calc_graph_stats(i)
    n <- names(s)
    names(s) <- NULL
    stats_df <- rbind(stats_df, s)
  }
  colnames(stats_df) <- n

  as.data.frame(stats_df)
}
