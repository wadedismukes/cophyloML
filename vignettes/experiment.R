library(stringr)

csp_rates <- c(0, 1, 2)
hs_rates <- c(0, 1, 2)
symb_n_host_rate <- 1.5
ext_rates <- c(0, 0.1, 0.2)
out_file_base <- "cophyloML_ext"
N <- 10000

for(i in 1:length(ext_rates)) {
  for(j in 1:length(csp_rates)) {
    for(k in 1:length(hs_rates)) {
      out_file_base <- "cophyloML_ext"
      out_filename <- str_c(out_file_base, ext_rates[i], "_csp")
      out_filename <- str_c(out_filename, csp_rates[j], "_hs")
      out_filename <- str_c(out_filename, hs_rates[k], ".csv")
      d1 <- cophyloML::sim_dataset(ext_rates[i], csp_rates[j], hs_rates[k], N)
      td_stats <- cophyloML::get_td_stats(d1)
      sum_stats <- cophyloML::get_summary_stats(d1, td_stats)
      write.csv(sum_stats, file = out_filename, row.names = FALSE)
    }
  }
}
