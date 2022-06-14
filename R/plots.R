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
  theme_bw()



ggplot(data = plot_df, aes(x = Cospeciation..predicted., y = Symbiont.Speciation..predicted., color = as.factor(Extinction.rate))) +
  facet_grid(Cospeciation.rate ~ Host.switching.rate) +
  geom_point() +
  labs(x ="Cospeciation (predicted)", y = "Symbiont Speciation (predicted)") +
  labs(color = "Extinction Rate") +
  theme_bw()

plot_df$X <- NULL
plot_df$Num.tips.host <- (accuracy_df$Host.Speciation..true. + plot_df$Cospeciation..true. + 1)
plot_df$Num.tips.symb <- (accuracy_df$Symbiont.Speciation..true. + plot_df$Cospeciation..true. + 1)


plot_df$Cospeciation.abs.error <- abs(plot_df$Cospeciation..true. - plot_df$Cospeciation..predicted.)


ggplot(data = plot_df, aes(x = Num.tips.host, y = Cospeciation.abs.error, color = as.factor(Extinction.rate))) +
  facet_grid(Cospeciation.rate ~ Host.switching.rate) +
  geom_point() +
  labs(x ="Number host tips", y = "Cospeciation (absolute error)") +
  labs(color = "Extinction Rate") +
  theme_bw()


plot_df$Host.switching.abs.error <- abs(plot_df$Host.switching..true. - plot_df$Host.switching..predicted.)


ggplot(data = plot_df, aes(x = Num.tips.host, y = Host.switching.abs.error, color = as.factor(Extinction.rate))) +
  facet_grid(Cospeciation.rate ~ Host.switching.rate) +
  geom_point() +
  labs(x ="Number host tips", y = "host-switching (absolute error)") +
  labs(color = "Extinction Rate") +
  theme_bw()


