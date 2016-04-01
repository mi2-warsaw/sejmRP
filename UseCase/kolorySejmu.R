setwd("~/GitHub/7kadencji/")

partie <- read.csv("partie.csv")
partie <- rbind(partie, data.frame(partia = "Partia Razem", kolor="#870F57", wikilink="", kadencja_pierwsza=1, kadencja_ostatnia=1))
kols <- partie$kolor

(cols <- t(col2rgb(kols)))
zapsmall(luv <- convertColor(cols, from = "sRGB", to = "Luv", scale.in = 255))

params <- data.frame(luv, name=partie$partia, col=kols)

library(ggplot2)
library(ggrepel)
ggplot(params, aes(u, v, label=name)) +
  geom_point(size=5, color=params$col) + 
  geom_text_repel(size=3) + 
  theme_classic() + theme(legend.position="none")



