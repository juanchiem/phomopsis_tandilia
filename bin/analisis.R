library("googlesheets4")
library("tidyverse")

gs4_auth(email = "edwardsmolina@gmail.com")
# gs4_find() %>% view
aapre <- gs4_get("1jJYThqPMd-ZMWmJyq-CDhzVhNTFC1zoJ3RS75-taUOc")
# aapre %>% gs4_browse()
raw <- aapre %>% read_sheet() %>% 
  janitor::clean_names() %>% 
  mutate_if(is.character, as.factor) 

raw %>% glimpse

df <- raw %>% 
  group_by(localidad, hibrido) %>% 
  summarise(inc_pc = mean(inc_pc) %>% floor(), 
            sev_pc = mean(sev_pc), 
            kg_ha = first(kg_ha), 
            mg_percent = first(mg_percent), 
            .groups="drop") %>% 
  mutate(n_plantas=100) %>% 
  mutate(rinde_bonif = kg_ha*(1+((mg_percent-42)*2)/100)) 
  

# df %>% 
#   select(localidad, hibrido, inc_pc, sevm_pc)%>%
#   filter(hibrido %in% c("Z74L60", "Syn3970", "Adv5205")) %>% 
#   # datapasta::tribble_paste()

p1 <- df %>%
  ggplot()+
  aes(x=reorder(hibrido, inc_pc, mean), y=inc_pc, col=localidad)+
  stat_summary(fun.data = "mean_cl_boot", colour = "gray70", size = 0.5)+
  geom_point()+
  coord_flip()+
  theme_bw()+
  labs(x="", y="Incidencia de PC-phomopsis", col="")

p2 <- df %>%
  ggplot()+
  aes(x=reorder(hibrido, sevm_pc, mean), y=sevm_pc, col=localidad)+
  stat_summary(fun.data = "mean_cl_boot", colour = "gray70", size = 0.5)+
  geom_point()+
  coord_flip()+
  theme_bw()+
  labs(x="", y="Severidad media de PC-phomopsis", col="")

library(patchwork)
combined <- p1 + p2 & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")

p1 <- df %>% 
  ggplot()+
  aes(x=reorder(hibrido, inc_pc, mean), y=inc_pc, col=localidad)+
  stat_summary(fun.data = "mean_cl_boot", colour = "gray70", size = 0.5)+
  facet_grid(semillero~., scales = "free")+
  geom_point()+
  coord_flip()+
  theme_bw()+
  labs(x="", y="Incidencia de PC-phomopsis", col="")

p2 <- df %>% 
  ggplot()+
  aes(x=reorder(hibrido, inc_pc, mean), y=kg_ha, col=localidad)+
  stat_summary(fun.data = "mean_cl_boot", colour = "gray70", size = 0.5)+
  geom_point(alpha=.9)+
  facet_grid(semillero~., scales = "free")+
    coord_flip()+
  theme_bw()+
  guides(col=FALSE)+
  labs(x="", y="kg/ha bonificado", col="")

library(patchwork)
combined <- p1 + p2 & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")

# df %>% 
#   ggplot()+
#   aes(x=inc_pc, y=kg_ha, col=semillero)+
#   # stat_summary(fun.data = "mean_cl_boot", colour = "gray70", size = 0.5)+
#   geom_point(alpha=.5)+
#   geom_smooth(se=F)+
#   facet_wrap(~localidad)+
#   theme_bw()+
#   labs(x="", y="Incidencia de PC-phomopsis", col="")

library(lme4)
library(emmeans)
library(car)

mod_inc <- glmer(inc_pc/n_plantas ~ hibrido + 
                   (1|localidad/hibrido), 
                 weights=n_plantas, 
                 family="binomial", 
                 df)
plot(mod_inc)
summary(mod_inc)
Anova(mod_inc)


em <- emmeans (mod_inc, ~hibrido)
res <- multcomp::cld(em, Letters = letters, alpha = .05, type = "response") %>% 
  mutate_at(vars(matches('prob|asymp')), ~(. * 100 )%>% round(2)) %>% 
  mutate(let= str_remove_all(.group, " "))
knitr::kable(res)

res %>% #View 
  ggplot(aes(x=fct_reorder(hibrido, prob, .desc = TRUE), y=prob)) + 
  geom_point(data=df, 
             aes(x =fct_reorder(hibrido, inc_pc, .desc = TRUE, .fun = mean),
                 y=inc_pc), alpha=.2)+
  geom_pointrange(aes(ymin = asymp.LCL, ymax = asymp.UCL, col= let), size=.3)+
  geom_text(aes(label = let, y =prob),
            size=2.5, position = position_dodge(0.9), 
            vjust =-.5, hjust="center")+
  labs(x=NULL, y="Incidencia de phomoposis de capítulo", 
       col= "Test Tukey")+
  # scale_y_continuous(breaks=seq(0.2, 0.5, 0.05))+
  coord_flip()+
  theme_bw()+
  # theme(plot.margin = unit(c(0.1,0.1,0,0), "cm"))+ 
  theme(axis.text=element_text(size=10), 
        axis.title=element_text(size=12, face = "bold"), 
        legend.title = element_text(size=12, face = "bold"))


df %>% 
  janitor::tabyl(localidad, hibrido)

df %>% 
  ggplot()+
  aes(x=hibrido, y =kg_ha)+
  geom_point(aes(col=localidad))

df$kg_ha %>% summary

# mod_rinde <- lmer(rinde_bonif ~ hibrido + (1|localidad),
mod_rinde <- lm(rinde_bonif ~ hibrido + localidad,
                data=df)
plot(mod_rinde, which=1)
summary(mod_rinde)
Anova(mod_rinde)

em_rinde <- emmeans(mod_rinde, ~hibrido)
res_rinde <- multcomp::cld(em_rinde, 
                           Letters = letters, alpha = .05, type = "response") %>% 
  mutate(let= str_remove_all(.group, " "))
knitr::kable(res_rinde)

res_rinde %>% #View 
  ggplot(aes(x=fct_reorder(hibrido, emmean, .desc = F), y=emmean)) + 
  geom_point(data=df, 
             aes(x =fct_reorder(hibrido, rinde_bonif, .desc = F, .fun = mean),
                 y=rinde_bonif), alpha=.2)+
  geom_pointrange(aes(ymin = lower.CL, ymax = upper.CL, col= let), size=.3)+
  geom_text(aes(label = emmean %>% round(), y =emmean),
            size=2.5, position = position_dodge(0.9), 
            vjust =-.5, hjust="center")+
  labs(x=NULL, y="Rendimiento bonificado", 
       col= "Test Tukey")+
  coord_flip()+
  theme_bw()+
  theme(axis.text=element_text(size=10), 
        axis.title=element_text(size=12, face = "bold"), 
        legend.title = element_text(size=12, face = "bold"))


res_rinde %>% 
  inner_join(res, by="hibrido") %>% 
  rownames_to_column("id") %>%  
  mutate(id_hibrido = factor(paste(id, hibrido, sep = ": "))) %>% 
  mutate(id_hibrido = fct_relevel(id_hibrido, "1: LG50760", "2: Z74L60", 
                                  "3: Adv5205", 
                    "4: Z74H55", "5: Syn3939", "6: NS1109",
                    "7: NU4170", "8: B304", "9: P106", 
                    "10: Syn3970", "11: Syn4070", "12: Syn3975",
                  "13: Syn3990")) %>%  
  # str
  ggplot(aes(prob, emmean, col=id_hibrido)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), 
                width = 0, size = 0.8, alpha=.5) +
  geom_errorbarh(aes(xmin = asymp.LCL, xmax = asymp.UCL),  
                 height = 0, size = 0.8, alpha=.5)+ 
  geom_text(aes(label=id), col="black", fontface="bold", size=3)+
  theme_bw()+
  labs(x="Incidencia podredumbre de capítulo (%)", 
       y="Rinde bonificado", 
       col= "Híbrido")+
  theme(axis.text=element_text(size=10), 
        axis.title=element_text(size=12, face = "bold"), 
        legend.title = element_text(size=12, face = "bold"))
