# calculate graph statistics
calc_graph_stats <- function(graph) {
  stats <- c()
  idh <- which(igraph::V(graph)$type==3)
  ids <-  which(igraph::V(graph)$type==4)
  diam <- igraph::diameter(graph)
  centev <- igraph::centralization.evcent(graph)$centralization

  dens   <- igraph::graph.density(graph)

  div    <- igraph::graph.diversity(graph)
  divHm  <- mean(div[idh])
  divSm  <- mean(div[ids])
  divHsd <- sd(div[idh])
  divSsd <- sd(div[ids])

  coren    <- igraph::graph.coreness(graph)
  corenHm  <- mean(coren[idh])
  corenSm  <- mean(coren[ids])
  corenHsd <- sd(coren[idh])
  corenSsd <- sd(coren[ids])

  simjHtmp <- igraph::similarity.jaccard(graph,vids=idh)
  simjStmp <- igraph::similarity.jaccard(graph,vids=ids)
  simjHm  <-  mean( simjHtmp )
  simjSm   <- sd( simjStmp )
  simjHsd  <-  mean( simjHtmp )
  simjSsd  <- sd( simjStmp )

  stats <- c(stats,
             diam,
             centev,
             dens,
             corenHm,
             corenSm,
             corenHsd,
             corenSsd,
             simjHm,
             simjHsd,
             simjSm,
             simjSsd )
  names(stats) <- c(names(stats),
                    "Diameter",
                    "Centralized_Eigenvaluecentrality",
                    "Density",
                    "mean_coreness_Hosts",
                    "mean_coreness_Symbs",
                    "stdev_coreness_Hosts",
                    "stdev_coreness_Symbs",
                    "mean_similarityjaccard_Hosts",
                    "mean_similarityjaccard_Symbs",
                    "sd_similarityjaccard_Hosts",
                    "sd_similarityjaccard_Sarasites")
  stats
}
