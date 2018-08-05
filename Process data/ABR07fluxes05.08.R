#intake: flux data from ABR07

library(gridExtra)

pdf("ABR07fluxes05.08.pdf", onefile = TRUE)

for (i in unique(cc.data$Tree)) {
  grid.arrange(ggflux1(i), ggflux2(i), ggflux3(i), ggflux4(i), ncol = 2, nrow = 2)
  grid.arrange(ggfluxsmooth(i))
}
dev.off()