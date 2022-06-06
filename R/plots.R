library(ggplot2)
library(cowplot)
library(ggthemes)
r2_df <- read.csv("r2_vals.csv")
r2_df$X <- NULL


ggplot(data = r2_df, aes(x = Extinction.rate, y = R.2)) +
  facet_grid(Cospeciation.rate ~ Host.switching.rate) +
  geom_bar(stat = 'identity') +
  labs(x ="Extinction Rate", y = "R Squared") +
  theme_bw()

ggplot(data = r2_df, aes(x = Extinction.rate, y = MAE)) +
  facet_grid(Cospeciation.rate ~ Host.switching.rate) +
  geom_bar(stat = 'identity') +
  labs(x ="Extinction Rate", y = "Median Absolute Error") +
  theme_bw()

ggplot(data = r2_df, aes(x = Extinction.rate, y = MSE)) +
  facet_grid(Cospeciation.rate ~ Host.switching.rate) +
  geom_bar(stat = 'identity') +
  labs(x ="Extinction Rate", y = "Mean Squared Error") +
  theme_bw()



accuracy_df <- read.csv("pred_vs_true.csv")
sims <- simulation_settings[rep(seq_len(nrow(simulation_settings)), each = 3000), ]
plot_df <- cbind(sims, accuracy_df)



ggplot(data = plot_df, aes(x = Cospeciation..true., y = Cospeciation..predicted., color = as.factor(Extinction.rate))) +
  facet_grid(Cospeciation.rate ~ Host.switching.rate) +
  geom_point() +
  labs(x ="Cospeciation (true)", y = "Cospeciation (predicted)") +
  labs(color = "Extinction Rate") +
  theme_bw()


ggplot(data = plot_df, aes(x = Host.switching..true., y = Host.switching..predicted., color = as.factor(Extinction.rate))) +
  facet_grid(Cospeciation.rate ~ Host.switching.rate) +
  geom_point() +
  labs(x ="Host-switching (true)", y = "Host-switching (predicted)") +
  labs(color = "Extinction Rate") +
  theme_bw()


ggplot(data = plot_df, aes(x = Host.Speciation..true., y = Host.Speciation..predicted., color = as.factor(Extinction.rate))) +
  facet_grid(Cospeciation.rate ~ Host.switching.rate) +
  geom_point() +
  labs(x ="Host speciation (true)", y = "Host speciation (predicted)") +
  labs(color = "Extinction Rate") +
  theme_bw()


ggplot(data = plot_df, aes(x = Symbiont.Speciation..true., y = Symbiont.Speciation..predicted., color = as.factor(Extinction.rate))) +
  facet_grid(Cospeciation.rate ~ Host.switching.rate) +
  geom_point() +
  labs(x ="Symbiont speciation (true)", y = "Symbiont speciation (predicted)") +
  labs(color = "Extinction Rate") +
  theme_hc()

