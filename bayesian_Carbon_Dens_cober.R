

library (tidyverse) # Data handling

library (readxl) # read excel data

library (sjPlot) # wiew GLM plots
library (sjmisc) # wiew GLM plots

Sys.setenv(USE_CXX14 = 1)
library("rstan") # observe startup messages

library (brms)
library(bayesplot)
library(ggplot2)


library(magrittr)
library(dplyr)
library(forcats)
library(tidyr)
library(modelr)
library(tidybayes)
library(ggstance)
library(ggridges)
library(cowplot)
library(rstan)
library(ggrepel)



path <- "C:/Users/diego.lizcano/Box Sync/CodigoR/Carbon-Biodiv/Data"
path2 <- "D:/BoxFiles/Box Sync/CodigoR/Carbon-Biodiv/Data"

beetle_data <- read_excel(paste(path,"/TNC_Escarabajo_Carbono_June_2018.xlsx", sep=""), 
                          sheet = "Base_Escarabajos")

beetle_data <- beetle_data %>% group_by(Punto) %>% 
  mutate (Richness = n_distinct(Especie))  # New column richness

beetle_data$LongDD <- beetle_data$LonDD * (-1)

ants_data <- read_excel(paste(path,"/TNC_Escarabajo_Carbono_June_2018.xlsx", sep=""), 
                        sheet = "Base_Hormigas")

ants_data <- ants_data %>% group_by(Punto) %>% 
  mutate (Richness = n_distinct(Especie))

ants_data$LongDD <- ants_data$LonDD * (-1)

#### Check maximum
print("max")
max(unique(beetle_data$Punto))

#### Check minumum
print("min")
min(unique(beetle_data$Punto))


carbono_data <- read_excel(paste(path,"/CARBONO-08-10-2018.xlsx", sep=""), 
                           sheet = "Densidad-Carbono")

### select < =15
carbono_data2 <- subset(carbono_data, Carbono <= 15)
### make carbon proportion
carbono_data2$carbono_prop<-carbono_data2$Carbono/100



# New column richness
# richness... shirink by punto
rich_beetle_data <- beetle_data %>% 
  group_by (Punto) %>% 
  # select ("Lat", "Lon", "Cobertura")
  summarise (Richness = n_distinct(Especie),
             Abundance = sum (Abundancia),
             Lat = unique (LatDD), 
             Lon = unique (LonDD))#,
             #Cobertura= unique (Cobertura)) 

# average carbono and compactation data 
mean_carbono_data <- carbono_data2 %>% 
  group_by (Punto) %>% 
  summarise (Carbono = mean(Carbono),
             SoilCompactation= mean(Densidad)) 

# merge both data sets
Full_data <- merge(rich_beetle_data ,
                   carbono_data2,# mean_carbono_data,
                   by="Punto")


rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# Sys.setenv(USE_CXX14 = 1)


carbon_fit1 <- brm (carbono_prop ~  Densidad + (1|Cobertura), 
                    data = carbono_data2, family=Beta() )#,
                    
                    # iter=2500, warmup=1000, chains=4, 
                    # thin = 15,
                    # seed=483892929, refresh=1200)

carbon_fit2 <- brm (carbono_prop ~  Densidad + (1|Cobertura), 
                    data = carbono_data2, family=Gamma() )



carbon_fit3 <- brm (carbono_prop ~  Densidad + (1|Cobertura) + (1|Profundidad), 
                    data = carbono_data2, family=Beta(), 
                    control = list(adapt_delta = 0.85))#, max_treedepth = 15) )

carbon_fit4 <- brm (carbono_prop ~  Densidad * Cobertura + (1|Profundidad), 
                    data = carbono_data2, family=Beta())
                    
carbon_fit5 <- brm (carbono_prop ~  Densidad * Profundidad  + (1|Cobertura), 
                    data = carbono_data2, family=Gamma() )

carbon_fit6 <- brm (carbono_prop ~  Densidad * Profundidad  + (1|Cobertura), 
                    data = carbono_data2, family=Beta() )


carbon_fit4_long <- brm (carbono_prop ~  Densidad * Cobertura + (1|Profundidad), 
                    data = carbono_data2, family=Beta(), 
                    control = list(adapt_delta = 0.89), iter=55000, warmup=1000, chains=4 
                    )

carbon_fit7 <- brm (bf(carbono_prop ~  Densidad + 
                       (Densidad| mm(Cobertura, Profundidad))), family=Beta(),
                    data = carbono_data2)


carbon_fit8 <- brm (bf(carbono_prop ~  Densidad + (1| mm(Cobertura, Profundidad)),
                                    Densidad ~ 1 + (1| mm(Cobertura, Profundidad))),             
                                    data = carbono_data2, family=Beta() )

loo(carbon_fit1, 
    carbon_fit2, 
    carbon_fit3,
    carbon_fit4,
    #carbon_fit5,
    carbon_fit6) # smaller LOOIC values indicate better fit, we see that the model accounting for overdispersion fits substantially better

plot(carbon_fit7)
pp_check(carbon_fit4)
print(carbon_fit4)

cond<-data.frame(Cobertura = c(unique(carbono_data2$Cobertura)))  
                         unique(carbono_data2$Profundidad)))
rownames(cond) <- c(unique(carbono_data2$Cobertura))#, unique(carbono_data2$Profundidad))

plot(marginal_effects(carbon_fit7, conditions = cond, re_formula = NULL, method= "predict")) #, points=TRUE)
plot(marginal_effects(carbon_fit7, effects = "Cobertura"), points=TRUE)




carbono_data2 %>%
  group_by(Cobertura) %>%
  data_grid(Densidad = seq_range(Densidad, n = 101), 
            Profundidad = rep("10", 101)) %>%
  add_fitted_draws(carbon_fit4, n = 100) %>%
  ggplot(aes(x = Densidad, y = carbono_prop, color = ordered(Cobertura))) +
  geom_line(aes(y = .value, group = paste(Cobertura, .draw)), alpha = .1) +
  geom_point(data = carbono_data2) +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(. ~ Cobertura)


carbono_data2 %>%
  group_by(Cobertura) %>%
  data_grid(Densidad = seq_range(Densidad, n = 101), 
            Profundidad = rep(c(10,20,30), 101)) %>%
  add_predicted_draws(carbon_fit4) %>%
  ggplot(aes(x = Densidad, y = carbono_prop)) +
  stat_lineribbon(aes(y = .prediction), .width = c(.99, .95, .8, .5)) +
  geom_point(data = carbono_data2) +
  scale_fill_brewer() +
  facet_grid(. ~ Cobertura)

# launch_shinystan(carbon_fit3)

# LOOIC values indicate better fit, we see that the model accounting for overdispersion fits substantially better


## Biodiversity
## Biodiversity
## Biodiversity

bio_fit4 <- brm (Richness ~  Densidad * Cobertura, #+ (1|Punto), 
                    data = Full_data, family=poisson())

plot(bio_fit4)
pp_check(bio_fit4)
print(bio_fit4)

plot(marginal_effects(bio_fit4)) #, points=TRUE)
plot(marginal_effects(bio_fit4, effects = "Densidad:Cobertura"))#, points=TRUE)


Full_data %>%
  group_by(Cobertura) %>%
  data_grid(Densidad = seq_range(Densidad, n = 101), 
            Profundidad = rep("10", 101)) %>%
  add_fitted_draws(carbon_fit4, n = 100) %>%
  ggplot(aes(x = Densidad, y = Richness, color = ordered(Cobertura))) +
  geom_line(aes(y = .value, group = paste(Cobertura, .draw)), alpha = .1) +
  geom_point(data = Full_data) +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(. ~ Cobertura)



Full_data %>%
  group_by(Cobertura) %>%
  data_grid(Densidad = seq_range(Densidad, n = 101), 
            Profundidad = rep(c(10,20,30), 101)) %>%
  add_predicted_draws(bio_fit4) %>%
  ggplot(aes(x = Densidad, y = Richness)) +
  stat_lineribbon(aes(y = .prediction), .width = c(.99, .95, .8, .5)) +
  geom_point(data = Full_data) +
  scale_fill_brewer() +
  facet_grid(. ~ Cobertura)

