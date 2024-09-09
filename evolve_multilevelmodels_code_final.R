
library('haven')
library('tidyr')
library('dplyr')
library('DataCombine')
library('psych')
library('lme4')
library('lmerTest')
install.packages('stargazer')
library('stargazer')
library('effects')
library('ggplot2')
install.packages('GGally')
library('GGally')
install.packages('mediation')
library('mediation')
library('reshape2')
library('sjstats')
install.packages('MuMIn')
library('MuMIn')
install.packages('lm.beta')
library('lm.beta')
install.packages('arsenal')
library('arsenal')

library(haven) #for reading files
library(ggplot2) #for plotting
library(tidyverse) #for data management and manipulation
library(dplyr)
library(zoo) #rolling averages
library(readxl) #reading in excel files
library(lme4) #mlm
library(lmerTest) #mlm to pull p values
library(emmeans) #plot interaction using model-implied estimated marginal means
library(lubridate)
library(visdat) 
library(janitor) #nice cleaning functions
library(psych) #good describe/summary functions
library(skimr) #useful skimming functions for big datasets
library(ggdist)
library(ggforce)
library(sjPlot)

setwd("/Users/allystumper/Desktop/evolve")
evolve <- read_xlsx ("/Users/allystumper/Desktop/evolve/data_ally.xlsx")


evolve[] <- lapply(evolve, function(x) replace(x, x == 999, NA))
evolve[] <- lapply(evolve, function(x) replace(x, x == 888, NA)) 

#raw outcomes
outcomelist <- evolve %>%
  select(id, depressed,lonely, hopeless, anxious, mood_swings, rej_sen, irritability, anhedonia, concentration_diff,
         conflicts, overwhelmed, sum, e1g_crtn, pdg_crtn) %>% 
  colnames() %>% 
  noquote()

outcomelist.d <- c("depressed.d",
                   "lonely.d",
                   "hopeless.d",
                   "anxious.d",
                   "mood_swings.d",
                   "rej_sen.d",
                   "irritability.d",
                   "ahedonia.d",
                   "concentration_diff.d",
                   "conflicts.d",
                   "overwhelmed.d",
                   "sum.d",
                   "e1g_crtn.d",
                   "pdg_crtn.d") %>% noquote()


#functions to create person means and deviations
create.person.mean <- function(df, var, ...) {
  df %>%
    group_by(...) %>%
    mutate("{{var}}.m" := mean({{var}}, na.rm=T))
}
#function to create person deviations (note, must have person means already made)
create.deviation <- function(df, var, var.m) {
  df <- df %>%
    rowwise() %>%
    mutate("{{var}}.d" := {{var}} - {{var.m}})
}

#MEANS

#execute for loop: run create.person.mean() on everything in "outcomelist"
for (i in outcomelist) {
  evolve <- create.person.mean(evolve, !!sym({{i}}), id)
}
#PERSON-DEVIATIONS

#for raw deviations
#execute for loop: run create.deviation() on everything in list
for (i in outcomelist) {
  evolve <- create.deviation(evolve, !!sym({{i}}), !!sym(paste0({{i}}, ".m")))
}


view(evolve)


#prep and scaling of hormones
describe(evolve$pdg_crtn)
describe(evolve$e1g_crtn)
evolve %>% select(id, date_index, e1g_crtn, pdg_crtn) %>% View()

evolve <- evolve %>%
  mutate(e1g_crtn.d_10 = e1g_crtn.d/10)
evolve <- evolve %>%
  mutate(pdg_crtn.d_1000 = pdg_crtn.d/1000)

#hormone models 
#by ov status

evolve_ov <- filter(evolve, os==1)
evolve_anov2 <- filter(evolve,os!=1)

#ovulatory group only 

#depressed
dep_o <- lmer(depressed~ e1g_crtn.d_10 + pdg_crtn.d_1000 +(1 | id),data = evolve_ov)
summary(dep_o) 
dep_int <- lmer(depressed~ e1g_crtn.d_10 + pdg_crtn.d_1000 + e1g_crtn.d_10:pdg_crtn.d_1000 +(1 | id),data = evolve_ov)
summary(dep_int)

#lonely
lonely_o <- lmer(lonely~ e1g_crtn.d_10 + pdg_crtn.d_1000 +(1 | id),data = evolve_ov)
summary(lonely_o) 
lonely_int <- lmer(lonely~ e1g_crtn.d_10 + pdg_crtn.d_1000 + e1g_crtn.d_10:pdg_crtn.d_1000 +(1 | id),data = evolve_ov)
summary(lonely_int)

#hopeless
hopeless_o <- lmer(hopeless~ e1g_crtn.d_10 + pdg_crtn.d_1000 +(1 | id),data = evolve_ov)
summary(hopeless_o)
hopeless_int <- lmer(hopeless~ e1g_crtn.d_10 + pdg_crtn.d_1000 + e1g_crtn.d_10:pdg_crtn.d_1000 +(1 | id),data = evolve_ov)
summary(hopeless_int)

#anxious
anxious_o <- lmer(anxious~ e1g_crtn.d_10 + pdg_crtn.d_1000 +(1 | id),data = evolve_ov)
summary(anxious_o)
anxious_int <- lmer(anxious~ e1g_crtn.d_10 + pdg_crtn.d_1000 + e1g_crtn.d_10:pdg_crtn.d_1000 +(1 | id),data = evolve_ov)
summary(anxious_int)

#mood swings
ms_o <- lmer(mood_swings~ e1g_crtn.d_10 + pdg_crtn.d_1000 +(1 | id),data = evolve_ov)
summary(ms_o)
ms_int <- lmer(mood_swings~ e1g_crtn.d_10 + pdg_crtn.d_1000 + e1g_crtn.d_10:pdg_crtn.d_1000 +(1 | id),data = evolve_ov)
summary(ms_int)

#rejection sensitivity
rs_o <- lmer(rej_sen ~ e1g_crtn.d_10 + pdg_crtn.d_1000 +(1 | id),data = evolve_ov)
summary(rs_o)
rs_int <- lmer(rej_sen~ e1g_crtn.d_10 + pdg_crtn.d_1000 + e1g_crtn.d_10:pdg_crtn.d_1000 +(1 | id),data = evolve_ov)
summary(rs_int)

#irritability
irritability_o <- lmer(irritability ~ e1g_crtn.d_10 + pdg_crtn.d_1000 +(1 | id),data = evolve_ov)
summary(irritability_o)
irritability_int <- lmer(irritability~ e1g_crtn.d_10 + pdg_crtn.d_1000 + e1g_crtn.d_10:pdg_crtn.d_1000 +(1 | id),data = evolve_ov)
summary(irritability_int)

#anhedonia
anhedonia_o <- lmer(anhedonia ~ e1g_crtn.d_10 + pdg_crtn.d_1000 +(1 | id),data = evolve_ov)
summary(anhedonia_o)
anhedonia_int <- lmer(anhedonia~ e1g_crtn.d_10 + pdg_crtn.d_1000 + e1g_crtn.d_10:pdg_crtn.d_1000 +(1 | id),data = evolve_ov)
summary(anhedonia_int)

#concentration difficulty
conc_o<- lmer(concentration_diff ~ e1g_crtn.d_10 + pdg_crtn.d_1000 +(1 | id),data = evolve_ov)
summary(conc_o)
conc_int <- lmer(concentration_diff~ e1g_crtn.d_10 + pdg_crtn.d_1000 + e1g_crtn.d_10:pdg_crtn.d_1000 +(1 | id),data = evolve_ov)
summary(conc_int)

#conflicts
conflict_o <-lmer(conflicts ~ e1g_crtn.d_10 + pdg_crtn.d_1000 +(1 | id),data = evolve_ov)
summary(conflict_o)
conflicts_int <- lmer(conflicts~ e1g_crtn.d_10 + pdg_crtn.d_1000 + e1g_crtn.d_10:pdg_crtn.d_1000 +(1 | id),data = evolve_ov)
summary(conflicts_int)

#overwhelm
overwhelm_o <-lmer(overwhelmed ~ e1g_crtn.d_10 + pdg_crtn.d_1000 +(1 | id),data = evolve_ov)
summary(overwhelm_o)
overwhelm_int <-lmer(overwhelmed~ e1g_crtn.d_10 + pdg_crtn.d_1000 + e1g_crtn.d_10:pdg_crtn.d_1000 +(1 | id),data = evolve_ov)
summary(overwhelm_int)

tab_model(dep_o, lonely_o, hopeless_o, anxious_o, ms_o, rs_o, irritability_o, anhedonia_o, conc_o, conflict_o, overwhelm_o)


#anovulatory group only

#depressed
dep_ano2 <- lmer(depressed~ e1g_crtn.d_10 + pdg_crtn.d_1000 +(1 | id),data = evolve_anov2)
summary(dep_ano2) 
dep_int_ano2 <-lmer(depressed~ e1g_crtn.d_10 + pdg_crtn.d_1000 + e1g_crtn.d_10:pdg_crtn.d_1000 +(1 | id),data = evolve_anov2)
summary(dep_int_ano2)

#lonely
lonely_ano2 <- lmer(lonely~ e1g_crtn.d_10 + pdg_crtn.d_1000 +(1 | id),data = evolve_anov2)
summary(lonely_ano2) 
lonely_int_ano2 <-lmer(lonely~ e1g_crtn.d_10 + pdg_crtn.d_1000 + e1g_crtn.d_10:pdg_crtn.d_1000 +(1 | id),data = evolve_anov2)
summary(lonely_int_ano2)

#hopeless
hopeless_ano2 <- lmer(hopeless~ e1g_crtn.d_10 + pdg_crtn.d_1000 +(1 | id),data = evolve_anov2)
summary(hopeless_ano2)
hopeless_int_ano2 <-lmer(hopeless~ e1g_crtn.d_10 + pdg_crtn.d_1000 + e1g_crtn.d_10:pdg_crtn.d_1000 +(1 | id),data = evolve_anov2)
summary(hopeless_int_ano2)

#anxious
anxious_ano2 <- lmer(anxious~ e1g_crtn.d_10 + pdg_crtn.d_1000 +(1 | id),data = evolve_anov2)
summary(anxious_ano2)
anxious_int_ano2 <-lmer(anxious~ e1g_crtn.d_10 + pdg_crtn.d_1000 + e1g_crtn.d_10:pdg_crtn.d_1000 +(1 | id),data = evolve_anov2)
summary(anxious_int_ano2)

#mood swings
ms_ano2 <- lmer(mood_swings~ e1g_crtn.d_10 + pdg_crtn.d_1000 +(1 | id),data = evolve_anov2)
summary(ms_ano2)
ms_int_ano2 <-lmer(mood_swings~ e1g_crtn.d_10 + pdg_crtn.d_1000 + e1g_crtn.d_10:pdg_crtn.d_1000 +(1 | id),data = evolve_anov2)
summary(ms_int_ano2)

#rejection sensitivity
rs_ano2 <- lmer(rej_sen ~ e1g_crtn.d_10 + pdg_crtn.d_1000 +(1 | id),data = evolve_anov2)
summary(rs_ano2)
rs_int_ano2 <-lmer(rej_sen~ e1g_crtn.d_10 + pdg_crtn.d_1000 + e1g_crtn.d_10:pdg_crtn.d_1000 +(1 | id),data = evolve_anov2)
summary(rj_int_ano2)

#irritability
irritability_ano2 <- lmer(irritability ~ e1g_crtn.d_10 + pdg_crtn.d_1000 +(1 | id),data = evolve_anov2)
summary(irritability_ano2)
irritability_int_ano2 <-lmer(irritability~ e1g_crtn.d_10 + pdg_crtn.d_1000 + e1g_crtn.d_10:pdg_crtn.d_1000 +(1 | id),data = evolve_anov2)
summary(irritability_int_ano2)

#anhedonia
anhedonia_ano2 <- lmer(anhedonia ~ e1g_crtn.d_10 + pdg_crtn.d_1000 +(1 | id),data = evolve_anov2)
summary(anhedonia_ano2)
anhedonia_int_ano2 <-lmer(anhedonia~ e1g_crtn.d_10 + pdg_crtn.d_1000 + e1g_crtn.d_10:pdg_crtn.d_1000 +(1 | id),data = evolve_anov2)
summary(anhedonia_int_ano2)

#concentration difficulty
conc_ano2<- lmer(concentration_diff ~ e1g_crtn.d_10 + pdg_crtn.d_1000 +(1 | id),data = evolve_anov2)
summary(conc_ano2)
conc_int_ano2 <-lmer(concentration_diff~ e1g_crtn.d_10 + pdg_crtn.d_1000 + e1g_crtn.d_10:pdg_crtn.d_1000 +(1 | id),data = evolve_anov2)
summary(conc_int_ano2)

#conflicts
conflict_ano2 <-lmer(conflicts ~ e1g_crtn.d_10 + pdg_crtn.d_1000 +(1 | id),data = evolve_anov2)
summary(conflict_ano2)
conflict_int_ano2 <-lmer(conflict~ e1g_crtn.d_10 + pdg_crtn.d_1000 + e1g_crtn.d_10:pdg_crtn.d_1000 +(1 | id),data = evolve_anov2)
summary(conflict_int_ano2)

#overwhelm
overwhelm_ano2 <-lmer(overwhelmed ~ e1g_crtn.d_10 + pdg_crtn.d_1000 +(1 | id),data = evolve_anov2)
summary(overwhelm_ano2)
overwhelm_int_ano2 <-lmer(overwhelm~ e1g_crtn.d_10 + pdg_crtn.d_1000 + e1g_crtn.d_10:pdg_crtn.d_1000 +(1 | id),data = evolve_anov2)
summary(overwhelm_int_ano2)

tab_model(dep_ano2, lonely_ano2, hopeless_ano2, anxious_ano2, ms_ano2, rs_ano2, irritability_ano2, anhedonia_ano2, conc_ano2, conflict_ano2, overwhelm_ano2)


#plots
effects_hope_o <- effects::effect(term= "e1g_crtn.d_10", mod= hopeless_o)
summary(effects_hope_o)
x_hope_o <- as.data.frame(effects_hope_o)

effects_rs_o <- effects::effect(term= "e1g_crtn.d_10", mod= rs_o)
summary(effects_rs_o)
x_rs_o <- as.data.frame(effects_rs_o)

effects_irritability_o <- effects::effect(term= "e1g_crtn.d_10", mod= irritability_o)
summary(effects_irritability_o)
x_irritability_o <- as.data.frame(effects_irritability_o)

effects_conc_o <- effects::effect(term= "e1g_crtn.d_10", mod= conc_o)
summary(effects_conc_o)
x_conc_o <- as.data.frame(effects_conc_o)

effects_ms_o <- effects::effect(term= "pdg_crtn.d_1000", mod= ms_o)
summary(effects_ms_o)
x_ms_o <- as.data.frame(effects_ms_o)

hope_plot_o <- ggplot() + 
  geom_point(data=x_hope_o, aes(x=e1g_crtn.d_10, y=fit), color="black") +
  #4
  geom_line(data=x_hope_o, aes(x=e1g_crtn.d_10, y=fit), color="black") +
  #5
  geom_ribbon(data= x_hope_o, aes(x=e1g_crtn.d_10, ymin=lower, ymax=upper), alpha= 0.3, fill="black") +
  #6
  labs(x="Person-Mean-Centered E1G", y="Hopelessness")

hope_plot_o

#rs
rs_plot_o <- ggplot() + 
  geom_point(data=x_rs_o, aes(x=e1g_crtn.d_10, y=fit), color="black") +
  #4
  geom_line(data=x_rs_o, aes(x=e1g_crtn.d_10, y=fit), color="black") +
  #5
  geom_ribbon(data= x_rs_o, aes(x=e1g_crtn.d_10, ymin=lower, ymax=upper), alpha= 0.3, fill="black") +
  #6
  labs(x="Person-Mean-Centered E1G", y="Rejection Sensitivity")

rs_plot_o

#irritability
irritability_plot_o <- ggplot() + 
  geom_point(data=x_irritability_o, aes(x=e1g_crtn.d_10, y=fit), color="black") +
  #4
  geom_line(data=x_irritability_o, aes(x=e1g_crtn.d_10, y=fit), color="black") +
  #5
  geom_ribbon(data= x_irritability_o, aes(x=e1g_crtn.d_10, ymin=lower, ymax=upper), alpha= 0.3, fill="black") +
  #6
  labs(x="Person-Mean-Centered E1G", y="Irritability")

irritability_plot_o

#concentration difficulty
conc_plot_o <- ggplot() + 
  geom_point(data=x_conc_o, aes(x=e1g_crtn.d_10, y=fit), color="black") +
  #4
  geom_line(data=x_conc_o, aes(x=e1g_crtn.d_10, y=fit), color="black") +
  #5
  geom_ribbon(data= x_conc_o, aes(x=e1g_crtn.d_10, ymin=lower, ymax=upper), alpha= 0.3, fill="black") +
  #6
  labs(x="Person-Mean-Centered E1G", y="Concentration Difficulty")
conc_plot_o

#mood swings (w/pdg)
ms_plot_o <- ggplot() + 
  geom_point(data=x_ms_o, aes(x=pdg_crtn.d_1000, y=fit), color="black") +
  
  geom_line(data=x_ms_o, aes(x=pdg_crtn.d_1000, y=fit), color="black") +
  #5
  geom_ribbon(data= x_ms_o, aes(x=pdg_crtn.d_1000, ymin=lower, ymax=upper), alpha= 0.3, fill="black") +
  #6
  labs(x="Person-Mean-Centered PdG", y="Mood Swings")
ms_plot_o
