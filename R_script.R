library(dplyr)  # pipe functionen
library(boot)   # bootstrap 
library(purrr)  # für purrr::map (Apply a functin to each element of a vector)
library(ggplot2) # Visualisierung
library(rstatix) # Für statistische Tests (Mann-Whithney U-Test (Wilcox))
library(ggpubr)  # Gruppiert die ggplots zu einem gemeinsamen Bild
#
setwd("red")
# 1.  Daten einlesen  ############################################################
numcol <- c(paste0("q", 3:22), paste0("q", 24:27),
            paste0("q", 29:33))
#
df <- data.frame(jsonlite::fromJSON("survey2.json")) %>%   # json-Datei einlesen
  setNames(c("ID", "Date", "Last.Page", "Language", "Seed", 
             paste0("q", 1:36))) %>%                      # Einheitliche Variablennamen
  mutate(Durchlauf = c(rep(1, 56), rep(2, (82 - 56))), .after = "ID" ) %>% # Identifikation des Durchlaufs
  mutate(Last.Page = as.numeric(Last.Page)) %>% 
  filter(., Last.Page == 14 & is.na(Last.Page) == F) %>%  # Wir entfernen alle Zeilen, die nur aus NA bestehen.
  mutate_at(numcol, as.numeric)
#
d1 <- df %>% filter(Durchlauf == 1)   # Teildatensatz (weil bequemer)
d2 <- df %>% filter(Durchlauf == 2)   # Teildatensatz (weil bequemer)
# 2. Variablenliste: hier sind die numerischen Variablen einzupflegen, die ausgewertet werden sollen.
var.list <- list("q11", "q16", "q17", "q19", "q24", "q29") 
###########################################################################
# 3. Funktionen definieren
# 3.1 bootstrap_mean generiert bootstrap replications und ermittelt für jede repl. den jeweiligen Mittelwert.
## Function: Bootstrap Mean
bootstrap_mean <- function(data, indices) {
  sample_data <- data[indices]
  return(mean(sample_data, na.rm = TRUE))
}
############################################################################
# 3.2 Bootstrap-Funktion auf d1 anwenden (Alternative: auf df %>% filter(., Durchlauf == 1))
set.seed(2305)  # seed for replication (Damit alle Bootstraps die exakt gleichen Werte ergeben)
############################################################################
df.01 <- d1 %>% select(., c(unlist(var.list))) %>%
  summarise(across(everything(), ~ list(boot(data = ., 
                                             statistic = bootstrap_mean, 
                                             R = 1000)))) %>%
  summarise(across(everything(), ~ map(., "t"))) 
# res01.df gibt für alle Variablen die jeweils 1000 Mittelwerte
res01.df <- data.frame(lapply(c(unlist(var.list)), 
                              function(col) unlist(df.01[[col]]))) %>%
  setNames(c(unlist(var.list)))
############################################################################
# 3.3 Das gleiche wie 3.2, aber auf Durchlauf 2 bezogen.
df.02 <- d2 %>% select(., c(unlist(var.list))) %>%
  summarise(across(everything(), ~ list(boot(data = ., 
                                             statistic = bootstrap_mean, 
                                             R = 1000)))) %>%
  summarise(across(everything(), ~ map(., "t"))) 
#
res02.df <- data.frame(lapply(c(unlist(var.list)), 
                              function(col) unlist(df.02[[col]]))) %>%
  setNames(c(unlist(var.list)))
############################################################################
rm(df.01, df.02, numcol)  # wird nicht mehr gebraucht. 
###############################################################
# 4. Wir werten die Bootstrap-Ergebnisse aus.
# 4.1 Wir generieren eine Auswertungsfunktion, so dass wir nicht für jede Variable alles neu aufschreiben müssen.
# Bootstrap evaluation function
quantile.fun <- function(v){
  quantile.df <- data.frame(d1.mean = mean(d1[[v]], na.rm = TRUE), # Schätzer Durchlauf 1
                            d2.mean = mean(d2[[v]], na.rm = TRUE), # Schätzer Durchlauf 2
                            d1_0.05 = quantile(res01.df[[v]], probs = 0.05, na.rm = TRUE),  
                            d1_0.95 = quantile(res01.df[[v]], probs = 0.95, na.rm = TRUE),
                            d2_0.05 = quantile(res02.df[[v]], probs = 0.05, na.rm = TRUE),
                            d2_0.95 = quantile(res02.df[[v]], probs = 0.95, na.rm = TRUE))
  return(quantile.df)
}
#################################################################################################
# 4.2 Als Alternative zum Bootstrap-Verfahren: Mann-Whitney-U-Test Function (Wilcoxon-Test)
# Wir bilden auch hier eine allgemeine Funktion
wilcoxon_test.fun <- function(df, var_list){
  mwutest <- function(variable){
    res <- df %>%
      wilcox_test(as.formula(paste(variable, "~ Durchlauf")))
    res <- res %>%
      mutate(variable = variable)
    return(res)
  }
  res2 <- map_dfr(var_list, mwutest) # wie lapply, gibt aber direkt ein df aus.
  return(res2)
}
#################################################################################################
# 4.3 Mann-Whitney U-Test results
res2 <- wilcoxon_test.fun(df, var.list)
##########################################
# 4.4 Wir wenden nun die Bootstrap-Auswertungsfunktion an.
res <- lapply(var.list,  quantile.fun) %>% do.call(rbind, .) %>%   # quantile.fun 
  setNames(c(unlist(var.list))) %>%       # Variablennamen 
  mutate(Bootstrap = c("**", "**", "*", "**", 0, 0)) %>%  ## händische Eingabe 
  mutate(MWUT.p = res2$p) %>%   # p-Werte des MW-U-Tests
  mutate(MWUT.Sign. = c("**", "**", "*", "**", 0, "*"))  # Dazugehörige Signifikanzniveaus
###################################################################################
## Cross-check (zur Sicherheit, wenn statt wilcox_test aus dem rstatix für
#   jede Variable ein eigener MW-U-Test mit wilcox.test durchgeführt wird.)
#wilcox.test(d1$q11, d2$q11)
#wilcox.test(d1$q16, d2$q16)
#wilcox.test(d1$q17, d2$q17)
#wilcox.test(d1$q19, d2$q19)
#wilcox.test(d1$q24, d2$q24)
#wilcox.test(d1$q29, d2$q29)
###################################################################################
## 5. Graphische Illustrationen
plots <- lapply(var.list, function(var) {
  ggplot(data = res01.df %>% 
           filter(., .data[[var]] < quantile(res01.df[[var]], probs = 0.95, na.rm = TRUE)) %>%
           filter(., .data[[var]] > quantile(res01.df[[var]], probs = 0.05, na.rm = TRUE))) +
    geom_density(aes(x = get(var), fill = "'violet'"), alpha = 0.5) +
    geom_density(data = res02.df %>% 
                   filter(., .data[[var]] > quantile(res02.df[[var]], probs = 0.05, na.rm = TRUE)) %>%
                   filter(., .data[[var]] < quantile(res02.df[[var]], probs = 0.95, na.rm = TRUE)),
                 aes(x = get(var)), fill = "orange", alpha = 0.5) +
    geom_vline(xintercept = mean(res01.df[[var]])) +
    geom_vline(xintercept = mean(res02.df[[var]])) +
    theme_bw(base_size = 22) +  # Set base size for text elements
     theme(plot.background = element_rect(fill = 'transparent'),
          panel.background = element_rect(fill = 'transparent'),
          text=element_text(color="white"),axis.text=element_text(color="white"),
          legend.position = "none") +
   # theme_bw(base_size = 22) +# theme(legend.position = "none") +
    labs(title = var, x = " ")
#    labs(title = paste("Bootstrap mean distribution of", var), x = "Mean values")
})

allplots <- ggarrange(plotlist = plots, ncol = 2, nrow = 3)
allplots
ggsave("Bootpic.pdf", width = 16, height = 8)
################################################################################
# 6. Einige Variablen sind nicht numerisch, deshalb kategorisieren wir sie.
gebiet.df <- data.frame(q2 = unique(df$q2),
                        Q2 = c(3,4,2,1, NA),
                        Land = c(0,0,1,0, NA), # Land = 1, 0 else
                        LKvsBE = c(0,0,1,1, NA))   # Kommune, Land = 0, Bund, EU = 1
bedf <- df %>% left_join(x = ., y = gebiet.df, by = "q2") %>%
  mutate(q23 = na_if(q23, "N/A")) %>%
  mutate(q28 = na_if(q28, "N/A")) %>%
  mutate(q34 = na_if(q34, "N/A")) %>%
  mutate(q35 = na_if(q35, "N/A")) %>%
  mutate(q36 = na_if(q36, "N/A"))

# logreg01: logistic regression for q36 --> significant effect of Durchlauf
logreg01 <- glm(formula = as.factor(q36) ~ Durchlauf, 
                family = binomial, 
                data = bedf) # Model 01
summary(logreg01)
q36.df <- bedf %>% 
  mutate(q36neu = ifelse(q36 == "Ja", "B", ifelse(q36 == "Nein", "A", NA)))
logreg01neu=glm(formula = as.factor(q36neu) ~ Durchlauf, family=binomial, data = q36.df)
summary(logreg01neu)
broom::tidy(logreg01neu, exponentiate = T, conf.int = T)

broom::tidy(logreg01, exponentiate = T, conf.int = T)
# logreg02: log reg for LKvsBE (Land oder Kommune versus Bund oder EU) --> Effekt auf dem 10% Signi.niveau
logreg02 <- glm(formula = LKvsBE ~ Durchlauf, 
                family = binomial, 
                data = bedf) # Model 02

summary(logreg02)
broom::tidy(logreg02, exponentiate = T, conf.int = T)

logreg03 <- glm(formula = as.factor(q28) ~ Durchlauf, family = binomial, data =bedf)
broom::tidy(logreg03, exponentiate = T, conf.int = T)

### -->Ergebnisse in tex:
library(texreg)
texreg(l = list(logreg01, logreg02), stars = c(0.01, 0.05, 0.1), booktabs = TRUE)
## Interpretationshilfe: Coeff bei logreg03 : 0.85. Wahrscheinlichkeit für eine Nennung Land oder Kommune ist exp(0.85) = 2.33 mal höher
## nach einer Beschäftigung mit dem Föderalismus als vorher. 
pr01 = plotreg(l = list(logreg01neu), signif.light = "red", 
               theme = theme_bw(base_size = 32))  # file = ...,
plot2 = pr01 + theme(plot.background = element_rect(fill = 'transparent'),
                     panel.background = element_rect(fill = 'transparent'),
                     text=element_text(color="red"),axis.text=element_text(color="red"),
                     legend.position = "none")# +
#  theme_gray( base_size = 22)
ggsave("logpic.pdf", width = 16, height = 8, plot = plot2)

##########################################################
