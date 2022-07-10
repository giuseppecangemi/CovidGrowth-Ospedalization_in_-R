#DECESSI


p$lag_dec <- lag(p$deceduti, 1)
p$var_dec <- p$deceduti - p$lag_dec

p$ma_dec <- rollmean(p$var_dec, k=7, fill=NA)

p$novax <- (p$var_dec*2)/3
p$ma_novax <- rollmean(p$novax, k=7, fill=NA)

p$sivax <- (p$var_dec*1)/3
p$ma_sivax <- rollmean(p$sivax, k=7, fill=NA)

p$somma <- p$sivax+p$novax
p$somma_mediamobile <- rollmean(p$somma, k=7, fill=NA)
pd <- melt(p[c("t", "novax", "ma_novax",
               "sivax", "ma_sivax", "somma_mediamobile")], id="t")

ggplot(data=pd, aes(x=t, y=value, colour=variable, group=variable)) +
  geom_line() + theme_linedraw() + 
    labs(title="Decessi giornalieri per status vaccinale e incidenza (si assume 2/3 novax)") +
      xlab("Giorni") + annotate("text", x =300, y = -80, label = "@giuseppecangemi")+
        coord_cartesian(ylim = c(-10, 700), clip = "off")
