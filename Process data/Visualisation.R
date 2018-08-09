#intake: flux data from ABR07

library(gridExtra)

pdf("GRA09allnormalised2.pdf", onefile = TRUE)

showdata(cc.data)

dev.off()

pdf("GRA09allnormalised3.pdf", onefile = TRUE)

for (i in unique(cc.data$Tree)) {
  grid.arrange(ggfluxsmoothcrop(i))
}

dev.off()
