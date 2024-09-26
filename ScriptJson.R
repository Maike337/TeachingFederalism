setwd("~/Documents/JGU/Lehrveranstaltungen/Fiskalföderalismus/WinterTerm2324/Umfrage Studierende Fiskalföderalismus/Zweiter Durchlauf/")
library(jsonlite)
library(rjson)
library(dplyr)
library(ggplot2)
library(reshape2)
library(ggpp)
library(viridis)
library(ggpubr)
library(rstatix)


numcol <- c(paste0("q", 3:22), paste0("q", 24:27),
            paste0("q", 29:33))

df <- data.frame(jsonlite::fromJSON("survey2.json")) %>%
  setNames(c("ID", "Date", "Last.Page", "Language", "Seed", 
             paste0("q", 1:36))) %>%
  mutate(Durchlauf = c(rep(1, 56), rep(2, (82 - 56))), .after = "ID" ) %>%
  mutate(Last.Page = as.numeric(Last.Page)) %>% 
  filter(., Last.Page == 14 & is.na(Last.Page) == F) %>%
  mutate_at(numcol, as.numeric)



d1 <- df %>% filter(Durchlauf == 1)
d2 <- df %>% filter(Durchlauf == 2)
table(d1$q1)
table(d2$q1)
df %>% group_by(Durchlauf, q7) %>% reframe(count = n()) 

survey.melt <- df %>% select(., c(contains("q"), "Durchlauf") ) %>%
  mutate(id = 1:nrow(.)) %>%
  melt(., id.vars = c("id", "Durchlauf"))


mapping.df <- data.frame(variable = paste0("q",7:10),
                         Layer = c("EU", "Bund", "Land", "Gemeinde"))

rolle <- survey.melt %>% 
  filter(., variable %in% paste0("q",7:10)) %>%
  left_join(x = ., y = mapping.df, by = "variable") %>%
  mutate(value = as.numeric(value))
rolle.mn <- rolle %>% group_by(Layer, Durchlauf) %>% 
  reframe(mean = mean(value, na.rm = T)) %>%
  mutate(mean = round(mean, 2))

layer1 <- ggplot(data = rolle %>% filter(., Durchlauf == 1), 
       aes(x = factor(Layer), fill = factor(value)), 
       group = Durchlauf, colour = Durchlauf ) +
  stat_count(position = "dodge") +  # dodge
  scale_fill_viridis_d(option = "plasma", na.value="white") +
  annotate(geom = "table", x = 3.5, y = 25, label = list(rolle.mn), 
           vjust = 1, hjust = 0, size = 6, col = "black") +
  theme_gray(base_size = 22) +
  labs(title = "Welche Ebene sollte eine größere / kleinere Rolle spielen",
       subtitle = "1 = 'sehr wichtige Rolle' bis 5 = 'keine Rolle' ",
       y = "", x = "", fill = "Wert")

layer2 <- ggplot(data = rolle %>% filter(., Durchlauf == 2), 
                 aes(x = factor(Layer), fill = factor(value)), 
                 group = Durchlauf, colour = Durchlauf ) +
  stat_count(position = "dodge") +  # dodge
  scale_fill_viridis_d(option = "plasma", na.value="white") +
  annotate(geom = "table", x = 3.5, y = 25, label = list(rolle.mn), 
           vjust = 1, hjust = 0, size = 6, col = "black") +
  theme_gray(base_size = 22) +
  labs(title = "Welche Ebene sollte eine größere / kleinere Rolle spielen",
       subtitle = "1 = 'sehr wichtige Rolle' bis 5 = 'keine Rolle' ",
       y = "", x = "", fill = "Wert")

layer.plot <- ggarrange(layer1, layer2, ncol = 2)
layer.plot

## q11:  Wir brauchen die Länder nicht mehr (1 = Zustimmung, 5 = Ablehnung)
df %>% group_by(Durchlauf, q11) %>% reframe(count = n()) 
#bedeutung

mapping.df <- data.frame(variable = paste0("q",11:17),
                         Item = paste0("q",11:17))
bedeutung <- survey.melt %>% 
  filter(., variable %in% paste0("q",11:17)) %>%
  left_join(x = ., y = mapping.df, by = "variable") %>%
  mutate(value = as.numeric(value))
bedeutung.mn <- bedeutung %>% group_by(Item, Durchlauf) %>% 
  reframe(mean = mean(value, na.rm = T)) %>%
  mutate(mean = round(mean, 2))

ggplot(data = bedeutung, aes(x = Item, y = value, fill = Item)) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "plasma") +
  theme_gray(base_size = 22) +
  theme(plot.background = element_rect(fill = 'transparent'),
        panel.background = element_rect(fill = 'transparent'),
        text=element_text(color="white"),axis.text=element_text(color="white"),
        legend.position = "none") +
  labs(title = "Bedeutung der (Bundes)länder",
       subtitle = "1 = 'volle Zustimmung' bis 5 = 'Ablehnung' ",
       y = "", x = "")



df %>% wilcox_test(formula = q12 ~ Durchlauf, 
                   alternative = "less")
df %>% t_test(formula = q12 ~ Durchlauf,
              alternative = "less")


df %>% wilcox_test(formula = q16 ~ Durchlauf, 
                   alternative = "less")

df %>% group_by(Durchlauf) %>% 
  reframe(mn = mean(as.numeric(q12), na.rm = T))
ggplot(data = df, aes(y = as.numeric(q12),
                      x = Durchlauf, 
                            group = factor(Durchlauf), 
                            colour = factor(Durchlauf))) + 
  geom_boxplot()

###############################################################
## Wettbewerb
mapping.df <- data.frame(variable = paste0("q",18:22),
                         Item = c("EU-Staaten", "(Bundes)länder", "Städten", "Schulen", "Großunternehmen"))
wettbewerb <- survey.melt %>% 
  filter(., variable %in% paste0("q",18:22)) %>%
  left_join(x = ., y = mapping.df, by = "variable") %>%
  mutate(value = as.numeric(value))
wettbewerb.mn <- wettbewerb %>% group_by(Item, Durchlauf) %>% 
  reframe(mean = mean(value, na.rm = T)) %>%
  mutate(mean = round(mean, 2))

### Wettbewerb zwischen den Ländern: Steuern (q24)

df %>% group_by(Durchlauf) %>% 
  reframe(mn = mean(q24, na.rm = T)) 

df %>% wilcox_test(formula = q24 ~ Durchlauf)#, 
#                   alternative = "greater")
