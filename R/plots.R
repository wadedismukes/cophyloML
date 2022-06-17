library(ggplot2)
library(grid)
library(ggthemes)
library(gtable)
library(RColorBrewer)
r2_df <- read.csv("r2_vals.csv")
r2_df$X <- NULL

p1 <- ggplot(data = r2_df, aes(x = Extinction.rate, y = R.2)) +
  facet_grid(Cospeciation.rate ~ Host.switching.rate) +
  geom_bar(stat = 'identity') +
  labs(x ="Extinction Rate", y = "R Squared") +
  theme_bw()

# Labels
labelR = "Cospeciation Rate"
labelT = "Host-Switching Rate"

# Get the ggplot grob
z <- ggplotGrob(p1)

# Get the positions of the strips in the gtable: t = top, l = left, ...
posR <- subset(z$layout, grepl("strip-r", name), select = t:r)
posT <- subset(z$layout, grepl("strip-t", name), select = t:r)

# Add a new column to the right of current right strips,
# and a new row on top of current top strips
width <- z$widths[max(posR$r)]    # width of current right strips
height <- z$heights[min(posT$t)]  # height of current top strips

z <- gtable_add_cols(z, width, max(posR$r))
z <- gtable_add_rows(z, height, min(posT$t)-1)

# Construct the new strip grobs
stripR <- grid::gTree(name = "Strip_right", children = grid::gList(
  grid::rectGrob(gp = grid::gpar(col = "black", fill = "grey85")),
  grid::textGrob(labelR, rot = -90, gp = grid::gpar(fontsize = 8.8, col = "grey10"))))

stripT <- grid::gTree(name = "Strip_top", children = grid::gList(
  grid::rectGrob(gp = grid::gpar(col = "black", fill = "grey85")),
  grid::textGrob(labelT, gp = grid::gpar(fontsize = 8.8, col = "grey10"))))

# Position the grobs in the gtable
z <- gtable_add_grob(z, stripR, t = min(posR$t)+1, l = max(posR$r) + 1, b = max(posR$b)+1, name = "strip-right")
z <- gtable_add_grob(z, stripT, t = min(posT$t), l = min(posT$l), r = max(posT$r), name = "strip-top")

# Add small gaps between strips
z <- gtable_add_cols(z, grid::unit(1/5, "line"), max(posR$r))
z <- gtable_add_rows(z, grid::unit(1/5, "line"), min(posT$t))

# Draw it
grid::grid.newpage()
grid::grid.draw(z)

p2 <- ggplot(data = r2_df, aes(x = Extinction.rate, y = MAE)) +
  facet_grid(Cospeciation.rate ~ Host.switching.rate) +
  geom_bar(stat = 'identity') +
  labs(x ="Extinction Rate", y = "Median Absolute Error") +
  theme_bw()

# Labels
labelR = "Cospeciation Rate"
labelT = "Host-Switching Rate"

# Get the ggplot grob
z <- ggplotGrob(p2)

# Get the positions of the strips in the gtable: t = top, l = left, ...
posR <- subset(z$layout, grepl("strip-r", name), select = t:r)
posT <- subset(z$layout, grepl("strip-t", name), select = t:r)

# Add a new column to the right of current right strips,
# and a new row on top of current top strips
width <- z$widths[max(posR$r)]    # width of current right strips
height <- z$heights[min(posT$t)]  # height of current top strips

z <- gtable_add_cols(z, width, max(posR$r))
z <- gtable_add_rows(z, height, min(posT$t)-1)

# Construct the new strip grobs
stripR <- grid::gTree(name = "Strip_right", children = grid::gList(
  grid::rectGrob(gp = grid::gpar(col = "black", fill = "grey85")),
  grid::textGrob(labelR, rot = -90, gp = grid::gpar(fontsize = 8.8, col = "grey10"))))

stripT <- grid::gTree(name = "Strip_top", children = grid::gList(
  grid::rectGrob(gp = grid::gpar(col = "black", fill = "grey85")),
  grid::textGrob(labelT, gp = grid::gpar(fontsize = 8.8, col = "grey10"))))

# Position the grobs in the gtable
z <- gtable_add_grob(z, stripR, t = min(posR$t)+1, l = max(posR$r) + 1, b = max(posR$b)+1, name = "strip-right")
z <- gtable_add_grob(z, stripT, t = min(posT$t), l = min(posT$l), r = max(posT$r), name = "strip-top")

# Add small gaps between strips
z <- gtable_add_cols(z, grid::unit(1/5, "line"), max(posR$r))
z <- gtable_add_rows(z, grid::unit(1/5, "line"), min(posT$t))

# Draw it
grid::grid.newpage()
grid::grid.draw(z)

p3 <- ggplot(data = r2_df, aes(x = Extinction.rate, y = MSE)) +
  facet_grid(Cospeciation.rate ~ Host.switching.rate) +
  geom_bar(stat = 'identity') +
  labs(x ="Extinction Rate", y = "Mean Squared Error") +
  theme_bw()

# Labels
labelR = "Cospeciation Rate"
labelT = "Host-Switching Rate"

# Get the ggplot grob
z <- ggplotGrob(p3)

# Get the positions of the strips in the gtable: t = top, l = left, ...
posR <- subset(z$layout, grepl("strip-r", name), select = t:r)
posT <- subset(z$layout, grepl("strip-t", name), select = t:r)

# Add a new column to the right of current right strips,
# and a new row on top of current top strips
width <- z$widths[max(posR$r)]    # width of current right strips
height <- z$heights[min(posT$t)]  # height of current top strips

z <- gtable_add_cols(z, width, max(posR$r))
z <- gtable_add_rows(z, height, min(posT$t)-1)

# Construct the new strip grobs
stripR <- grid::gTree(name = "Strip_right", children = grid::gList(
  grid::rectGrob(gp = grid::gpar(col = "black", fill = "grey85")),
  grid::textGrob(labelR, rot = -90, gp = grid::gpar(fontsize = 8.8, col = "grey10"))))

stripT <- grid::gTree(name = "Strip_top", children = grid::gList(
  grid::rectGrob(gp = grid::gpar(col = "black", fill = "grey85")),
  grid::textGrob(labelT, gp = grid::gpar(fontsize = 8.8, col = "grey10"))))

# Position the grobs in the gtable
z <- gtable_add_grob(z, stripR, t = min(posR$t)+1, l = max(posR$r) + 1, b = max(posR$b)+1, name = "strip-right")
z <- gtable_add_grob(z, stripT, t = min(posT$t), l = min(posT$l), r = max(posT$r), name = "strip-top")

# Add small gaps between strips
z <- gtable_add_cols(z, grid::unit(1/5, "line"), max(posR$r))
z <- gtable_add_rows(z, grid::unit(1/5, "line"), min(posT$t))

# Draw it
grid::grid.newpage()
grid::grid.draw(z)


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


