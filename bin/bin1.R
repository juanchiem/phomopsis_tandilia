
```{r}
# unique(dat$variedad)
dat %>%
  filter(str_detect(cultivo_de_cosecha, 'Tr|Ceb')) %>%
  count(cultivo_de_cosecha, variedad) %>%
  arrange(cultivo_de_cosecha, desc(n))
```


```{r}
dat %>% 
  filter(variedad %in% c("BAGUETTE_802",
                         # "BASILIO",
                         "BAGUETTE_501")) %>% 
  drop_na(fecha_de_siembra_dia_mes) %>%
  select(Zona, fecha_de_siembra_dia_mes, variedad,rinde, campana) %>% 
  # group_by(campana) %>% slice_sample(n = 3) %>%  
  mutate(
    year = factor(lubridate::year(fecha_de_siembra_dia_mes)),     
    date = update(fecha_de_siembra_dia_mes, year = 1)  
  ) %>% 
  filter(!(date > '1-07-15' & variedad == "BAGUETTE_802")) %>% 
  filter(rinde < 10000) %>% 
  ggplot(aes(date, rinde, col =variedad)) +
  # facet_wrap("Zona")+
  geom_point()+
  # geom_point(aes(color = year))+
  theme_dens+
  geom_smooth(se = F, method = 'lm', formula = y ~ poly(x, 2)) +
  labs(x = "Fecha de siembra", y = "Rendimiento (kg/ha)", col ="", 
       title = "Rinde Trigo")

ggsave(last_plot(), file = "plots/B802_B501.png", w=5, h=5)
drive_upload(path = as_dribble("juanchi_guille/JMF_fina_2020"), "plots/B802_B501.png", overwrite = TRUE)
```



```{r}
dat %>% 
  filter(cultivo_de_cosecha %in% c("Cebada")) %>% 
  # filter(variedad %in% c("ANDREIA",
  #                        "SHAKIRA",
  #                        "OVERTURE")) %>% 
  drop_na(fecha_de_siembra_dia_mes) %>%
  # select(fecha_de_siembra_dia_mes, variedad,rinde, campana) %>% 
  # group_by(campana) %>% slice_sample(n = 3) %>%  
  mutate(
    year = factor(lubridate::year(fecha_de_siembra_dia_mes)),     
    date = update(fecha_de_siembra_dia_mes, year = 1)  
  ) %>% 
  filter(!(date > '1-08-10')) %>%
  filter(between(rinde, 2000,7500)) %>%
  ggplot(aes(date, rinde))+#, col =variedad)) +
  facet_grid(.~Zona, scales="free_x")+
  geom_point(alpha=0.5)+
  # geom_point(aes(color = year))+
  geom_smooth(aes(col = Zona), se = F,
              method = 'lm', formula = y ~ poly(x, 2)) +
  guides(col = F)+
  labs(x = "Fecha de siembra", y = "Rendimiento (kg/ha)", 
       col ="", 
       title = "Cebada")+
  theme_dens1

ggsave(last_plot(), file = "plots/cebada_FS1.png", w=5, h=5)
drive_upload(path = as_dribble("juanchi_guille/JMF_fina_2020"), 
             "plots/cebada_FS1.png", overwrite = TRUE)

```

```{r}
dat %>%
  filter(str_detect(cultivo_de_cosecha, 'Tr|Ceb')) %>%
  select(campana,Zona, cultivo_de_cosecha, variedad,rinde ) %>% 
  group_by(cultivo_de_cosecha, Zona, variedad) %>%
  filter(n() > 5) %>% ungroup %>% 
  mutate_if(is.character, as.factor) ->dat1

dat1%>%
  count(cultivo_de_cosecha, Zona, variedad) %>%
  arrange(cultivo_de_cosecha, desc(n))

trigo_zona = levels(dat1$Zona) %>% 
  map(~ subset(dat1, cultivo_de_cosecha== "Trigo" & Zona == . ) %>%   
        ggplot()+
        aes(x=reorder(variedad, rinde, mean), y=rinde)+
        geom_point(alpha=0.3)+
        stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 0.5)+
        coord_flip()+
        theme_bw2+
        stat_summary(aes(label=round(..y..,0)), 
                     fun=mean, geom="text", size=4,vjust = -0.5)+
        scale_y_continuous(breaks= scales::pretty_breaks())+
        labs(title = paste0(.), 
             x="", y="kg/ha")
  ) 

trigo_zona[[1]] %>% ggsave(file = "plots/trigo_var_Costa.png", w=5, h=5)
trigo_zona[[2]] %>% ggsave(file = "plots/trigo_var_Madariaga.png", w=5, h=5)
trigo_zona[[3]] %>% ggsave(file = "plots/trigo_var_Sierra.png", w=5, h=5)

drive_upload(path = as_dribble("juanchi_guille/JMF_fina_2020"),
             "plots/trigo_var_Costa.png", overwrite = TRUE)
drive_upload(path = as_dribble("juanchi_guille/JMF_fina_2020"),
             "plots/trigo_var_Madariaga.png", overwrite = TRUE)
drive_upload(path = as_dribble("juanchi_guille/JMF_fina_2020"),
             "plots/trigo_var_Sierra.png", overwrite = TRUE)
```

```{r}
cebada_zona = levels(dat1$Zona) %>% 
  map(~ subset(dat1, cultivo_de_cosecha== "Cebada" & Zona == . ) %>%   
        ggplot()+
        aes(x=reorder(variedad, rinde, mean), y=rinde)+
        geom_point(alpha=0.3)+
        stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 0.5)+
        coord_flip()+
        theme_bw2+
        stat_summary(aes(label=round(..y..,0)), 
                     fun=mean, geom="text", size=4,vjust = -0.5)+
        scale_y_continuous(breaks= scales::pretty_breaks())+
        labs(title = paste0(.), 
             x="", y="kg/ha")
  ) 
cebada_zona[[1]] %>% ggsave(file = "plots/cebada_var_Costa.png", w=5, h=3)
cebada_zona[[2]] %>% ggsave(file = "plots/cebada_var_Madariaga.png", w=5, h=3)
cebada_zona[[3]] %>% ggsave(file = "plots/cebada_var_Sierra.png", w=5, h=3)
drive_upload(path = as_dribble("juanchi_guille/JMF_fina_2020"),
             "plots/cebada_var_Costa.png", overwrite = TRUE)
drive_upload(path = as_dribble("juanchi_guille/JMF_fina_2020"),
             "plots/cebada_var_Madariaga.png", overwrite = TRUE)
drive_upload(path = as_dribble("juanchi_guille/JMF_fina_2020"),
             "plots/cebada_var_Sierra.png", overwrite = TRUE)

```

# Superficies (tamaño de cuadrados) y rendimientos (intensidad de color) 

```{r}
library(treemap)
```

```{r treemap zonas}
dat_sum <- 
  dat %>% 
  # filter(cultivo_de_cosecha == "Trigo") %>% 
  # filter(campana == "19-20") %>% 
  # group_by(variedad) %>%
  group_by(campana, cultivo_de_cosecha, variedad) %>%
  filter(n()>5) %>% #ungroup %>%
  summarise(lotes= n(), 
            sup = sum(superficie, na.rm = TRUE),
            rinde_medio = median(rinde, na.rm = TRUE), .groups = 'drop') %>% 
  mutate(label = paste(variedad, round(sup,0), sep = "\n")) %>%
  mutate_if(is.character, as.factor)
# mutate(cultivo = recode(cultivo, soja_1 = "Soja 1°", soja_2 = "Soja 2°")) 

png(filename="plots/treemap_trigo_19-20.png", 
    width = 120, height = 120, units='mm', res = 300)

subset(dat_sum, 
       cultivo_de_cosecha == "Trigo"& 
         campana == ''8==) %>% 
  treemap(index="variedad",
          vSize="sup",
          vColor="rinde_medio",
          # border.col=c("grey70", "grey90"),
          # palette="Spectral",
          type = "value",
          title = "Superficie de trigo (has) / 19-20",  
          title.legend = "Rendimiento medio (qq/ha)",
          overlap.labels=1, 
          bg.labels=c("transparent"),              # Background color of labels
          inflate.labels=F,
          align.labels=list(
            c("left", "top"),
            c("center", "center")))

dev.off()
drive_upload(path = as_dribble("juanchi_guille/JMF_fina_2020"), "plots/treemap_trigo_18-19", overwrite = TRUE)
```

```{r}
png(filename="plots/treemap_cebada.png", 
    width = 120, height = 120, units='mm', res = 300)

subset(dat_sum, 
       cultivo_de_cosecha == "Cebada" ) %>% 
  treemap(index=c("campana", "label"),
          vSize="sup",
          vColor="rinde_medio",
          # border.col=c("grey70", "grey90"),
          # palette="Spectral",
          type = "value",
          title = "Superficie de cebada (has)",
          title.legend = "Rendimiento medio (qq/ha)",
          overlap.labels=1, 
          bg.labels=c("transparent"),              # Background color of labels
          inflate.labels=F,
          align.labels=list(
            c("left", "top"),
            c("center", "center"))) 
dev.off()

drive_upload(path = as_dribble("juanchi_guille/JMF_fina_2020"), "plots/treemap_cebada.png", overwrite = TRUE)

```

```{r}
sum <- 
  dat %>% 
  # filter(cultivo_de_cosecha == "Cebada") %>%
  group_by(cultivo_de_cosecha, Zona, campana, variedad) %>%
  summarise(lotes = n()) %>%
  # group_by(zona, campana, tecno_rr) %>%
  # summarise(lotes = n()) %>%
  group_by(cultivo_de_cosecha, Zona, campana) %>%
  mutate(perc = lotes/sum(lotes)) %>%
  separate(campana, c("campana",NA), "/") %>% 
  ungroup()
sum
```

```{r}
sum %>% 
  filter(cultivo_de_cosecha == "Cebada") %>%
  # drop_na(rinde) %>%
  ggplot(aes(x = factor(campana), 
             y = perc*100, 
             col = variedad, 
             group =variedad))+
  geom_line()+geom_point()+
  facet_grid(. ~Zona)+
  labs(x = NULL, y = "% del área de cada zona", col="",
       title = "Evolucion de superficie de cebada")+
  # guides(col="none")+
  ggrepel::geom_text_repel(
    data = ceb_sum %>% 
      filter(campana == "19-20"),
    # drop_na(tecno_sts),
    aes(label = variedad), size =3, nudge_x = 0.1) +
  theme_bw2+
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

ggsave(last_plot(), file = "plots/evolucion_cebadas.png", w =6, h=3)

sum %>% 
  filter(cultivo_de_cosecha == "Trigo") %>%
  # filter(variedad == "Andreia") 
  # drop_na(rinde) %>%
  ggplot(aes(x = factor(campana), 
             y = perc*100, 
             col = variedad, 
             group =variedad))+
  geom_line()+geom_point()+
  facet_grid(. ~Zona)+
  labs(x = NULL, y = "% del área de cada zona",  col="",
       title = "Evolucion de superficie de trigo")+
  # guides(col="none")+
  ggrepel::geom_text_repel(
    data = sum %>% 
      filter(cultivo_de_cosecha == "Trigo") %>%
      filter(campana == "19-20"),
    # drop_na(tecno_sts),
    aes(label = variedad), size =3, nudge_x = 0.1) +
  theme_bw()
# theme(axis.text.x = element_text(angle = 60, hjust = 1)) +

ggsave(last_plot(), file = "plots/evolucion_trigo.png", w =6, h=3)

```
