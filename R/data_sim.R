
sim_dataset <- function(ext_rate, cosp_rate, hs_rate, N) {
  # host birth and death rates
  birth_rates <- 1.5
  h_lambda <- birth_rates

  # symbiont birth and death rates
  s_lambda <- birth_rates

  host_symb_sets <- treeducken::sim_cophyBD(hbr = birth_rates,
                                            hdr = ext_rate,
                                            sbr = birth_rates,
                                            cosp_rate = cosp_rate,
                                            sdr = ext_rate,
                                            host_exp_rate = hs_rate,
                                            hs_mode = "switch",
                                            time_to_sim = 1.0,
                                            host_limit = 4,
                                            numbsim = N)
  host_symb_sets
}

get_summary_stats <- function(cophy, td_stats) {
  #graphs <- cophyloML::cophylo_to_graph(cophylo = cophy)
  graphs <- lapply(cophy, cophyloML::cophylo_to_graph)
  stats_df <- cophyloML::make_stats_df(graphs)
  stats_i_want <- cbind(td_stats$Host_Speciations,
                        td_stats$Symbiont_Speciations,
                        td_stats$Cospeciations,
                        td_stats$`Host_Spread/Switches`,
                        td_stats$Dispersals)
  stats_i_want[is.na(stats_i_want)] <- 0

  stats_df <- cbind(stats_df, stats_i_want)
  stats_df
}

