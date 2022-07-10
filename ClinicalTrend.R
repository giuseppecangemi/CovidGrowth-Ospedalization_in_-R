
d1$t1 <- d1

d_melt <- melt(d[c("t", "ricoverati_con_sintomi", "terapia_intensiva", 
                "totale_ospedalizzati")], id="t")
                
ggplot(data=mee, aes(x=t, y=value,colour=variable,group=variable))+ geom_line()+
        labs(title="Andamento Ricoveri-Terapie Intensive da inizio pandemia")+
          annotate("text", x =0, y = -4300, label = "Kangemi_Edu")+
            coord_cartesian(ylim = c(-100, 40000), clip = "off")


d2_melt <- melt(d1[c("t", "ricoverati_con_sintomi", "terapia_intensiva", 
                 "totale_ospedalizzati")], id="t")

ggplot(data=d2_melt, aes(x=t, y=value,colour=variable,group=variable))+ geom_line()+
  labs(title="Andamento Ricoveri-Terapie Intensive da OTTOBRE2021")+
    annotate("text", x =490, y = -4300, label = "Kangemi_Edu")+
      coord_cartesian(ylim = c(-100, 22000), clip = "off")

#da t>=313
p <- d %>%
  filter(t >= 313)

p$ti_media_mobile <- rollmean(p$ingressi_terapia_intensiva, k=7, fill=NA)
d3_melt <- melt(p[c("t", "ingressi_terapia_intensiva", "ti_media_mobile")], id="t")

ggplot(data=d3_melt, aes(x=t, y=value, colour=variable, group=variable))+
  geom_line() + theme_linedraw() + 
    labs(title="Andamento Ingressi Terapia Intensiva") +
      scale_fill_discrete(name="Gear", breaks=c("ingressi_terapia_intensiva", "ti_ma"), labels=c("3 Gear", "4 Gear"))
                        
p$ric_sint_lag <- lag(p$ricoverati_con_sintomi, 1)
p$ricoveri <- p$ricoverati_con_sintomi-p$ric_sint_lag
view(p)

p$ricoveri_media_mobile <- rollmean(p$ricoveri, k=7, fill=NA)

d4_melt <- melt(p[c("t", "ingressi_terapia_intensiva", "ti_media_mobile",
                 "ricoveri","ricoveri_media_mobile")], id="t")

ggplot(data=d4_melt, aes(x=t, y=value, colour=variable, group=variable))+
  geom_line() + theme_linedraw() + labs(title="Andamento Giornaliero Ingressi Terapia Intensiva e Ricoveri (2021)") +
    xlab("Giorni") + #geom_vline(xintercept = 385, lty=2) + annotate("text", x =313, y = -960, label = "Kangemi_Edu")+
      coord_cartesian(ylim = c(-800, 800), clip = "off")                        

#######from the beginning!!!!!!
d$ti_media_mobile <- rollmean(d$ingressi_terapia_intensiva, k=7, fill=NA)
d$ric_sint_lag <- lag(d$ricoverati_con_sintomi, 1)
d$ricoveri <- d$ricoverati_con_sintomi-d$ric_sint_lag
d$ricoveri_media_mobile <- rollmean(d$ricoveri, k=7, fill=NA)

d5_melt <- melt(d[c("t", "ingressi_terapia_intensiva", "ti_media_mobile",
                 "ricoveri","ricoveri_media_mobile")], id="t")

ggplot(data=d5_melt, aes(x=t, y=value, colour=variable, group=variable))+ geom_line() + theme_linedraw() + 
  labs(title="Andamento Giornaliero Ingressi Terapia Intensiva e Ricoveri da inizio pandemia") +
    xlab("Giorni") + annotate("text", x =0, y = -1530, label = "Kangemi_Edu")+ coord_cartesian(ylim = c(-1200, 2200), clip = "off")
