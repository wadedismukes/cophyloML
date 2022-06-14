library(ape)
library(treeducken)
library(geiger)
library(stringr)
host_tree <- read.tree(file = "empirical_data/ficus_snapp_2m_run1_newick.tre")
symb_tree <- read.tree(file = "empirical_data/fw_sb_50L_strC_run0_newick.tre")
assoc_mat <- as.matrix(read.table(file = "empirical_data/fw_table.txt", header = TRUE))

h_bd <- phytools::fit.bd(host_tree)
s_bd <- phytools::fit.bd(symb_tree)

h_lam <- h_bd$b
h_mu <- h_bd$d

s_lam <- s_bd$b
s_mu <- s_bd$d

h_t <- max(phytools::nodeHeights(host_tree))
s_t <- max(phytools::nodeHeights(symb_tree))


csp_rates <- c(0.1, 0.2)
hs_rates <- c(0.5, 0.8)
N <- 9999

for(j in 1:length(csp_rates)) {
  for(k in 1:length(hs_rates)) {
      out_file_base <- "empirical_test"
      out_filename <- str_c(out_file_base, "_csp")
      out_filename <- str_c(out_filename, csp_rates[j], "_hs")
      out_filename <- str_c(out_filename, hs_rates[k], ".csv")
      sbr <- s_lam
      hdr_frac <- h_lam * csp_rates[j]
      sdr_frac <- s_lam * csp_rates[j]
      if(hdr_frac < sdr_frac) {
        x <- hdr_frac
      } else {
        x <- sdr_frac
      }

      h_lam <- h_lam - x
      s_lam <- s_lam - x

      cosp_rate <- x

      hs_rate <- sbr * hs_rates[k]
      sbr <- sbr - hs_rate

      d1 <- treeducken::sim_cophyBD(hbr = h_lam,
                                     hdr = 0,
                                     sbr = s_lam,
                                     cosp_rate = cosp_rate,
                                     sdr = 0,
                                     host_exp_rate = hs_rate,
                                     hs_mode = "switch",
                                     time_to_sim = s_t,
                                     host_limit = 2,
                                     numbsim = N)
      td_stats <- cophyloML::get_td_stats(d1)
      sum_stats <- cophyloML::get_summary_stats(d1, td_stats)
      write.csv(sum_stats, file = out_filename, row.names = FALSE)
  }
}
host_names <- seq(from = 1, to = length(host_tree$tip.label))
symb_names <- seq(from = 1, to = length(symb_tree$tip.label))
cbind(host_tree$tip.label, row.names(assoc_mat))
cbind(symb_tree$tip.label, colnames(assoc_mat))
host_names <- str_c("H", host_names)
symb_names <- str_c("S", symb_names)
host_tree$tip.label <- host_names
symb_tree$tip.label <- symb_names
colnames(assoc_mat) <- symb_names
row.names(assoc_mat) <- host_names
edited_cophy <- treeducken::to_cophy(host_tree, symb_tree, assoc_mat)
empirical_cophy_graph <- cophyloML::cophylo_to_graph(empirical_cophy)
empirical_stats <- cophyloML::calc_graph_stats(empirical_cophy_graph)

names(empirical_stats) <- c("Diameter",
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
empirical_stats_df <- as.data.frame(t(empirical_stats))
write.csv(empirical_stats_df, file = "empirical_data.csv")

reticulate::source_python('~/Documents/cophyloML/src/empirical_cophyloML.py')


emp_pred_df <- data.frame(empirical_preds)
row.names(emp_pred_df) <- c("Host speciation",
                            "Symbiont speciation",
                            "Cospeciation",
                            "Host-switching")
colnames(emp_pred_df) <- c("Cospeciation 0.1, host-switching 0.5",
                           "Cospeciation 0.1, host-switching 0.8",
                           "Cospeciation 0.2, host-switching 0.5",
                           "Cospeciation 0.2, host-switching 0.8")

knitr::kable(t(emp_pred_df), format = "latex")
