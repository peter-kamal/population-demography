#________ POPULATION DEMOGRAPHY FINAL PROJECT__________#
#______ REPLICATION OF BALLADARES ET AL. (2020)________#
#______________ PETER KAMAL, JIA WANG _________________#

# Remark: We use the ggplot2 package for plots. That package requires long-form
# data, that's why there is always a fair bit of data manipulation before the plots.
# It gives us great flexibility in designing the plots. If you have any questions,
# feel free to approach us.


setwd("C:/Users/Kamal/OneDrive/TSE/M2 EE/PD_LHT/project")

# load necessary packages

library(popbio)
library(tidyverse)
library(ggrepel)

# define transition matrices

transition_managed <- matrix(c(0,0,0,21,56.9,0.3,0.248,0,0,0,0,0.082,0.726,
                               0,0,0,0,0.214,0.97,0,0,0,0,0.081,0.81),
                             nrow = 5, byrow = T)

transition_natural <- matrix(c(0,0,0,21,56.9,0.08,0.248,0,0,0,0,0.082,0.726,
                               0,0,0,0,0.214,0.97,0,0,0,0,0.081,0.81),
                             nrow = 5, byrow = T)


# get growth rates and stable age proportions

lambda_managed <- lambda(transition_managed)

lambda_natural <- lambda(transition_natural)

proportion_managed <- stable.stage(transition_managed)

proportion_natural <- stable.stage(transition_natural)


# model population dynamics

# start with one hatchling individual and track population over 50 years

N0 <- matrix(c(1, 0, 0,0,0), ncol = 1)
years <- 50

# compute the projection - managed population

N.projected_managed <- matrix(0, nrow = 5, ncol = years+1)
N.projected_managed[, 1] <- N0

for (i in 1:years) {
  N.projected_managed[, i + 1] <- transition_managed %*% N.projected_managed[,i]
}

# compute the projection - natural population

N.projected_natural <- matrix(0, nrow = 5, ncol = years+1)
N.projected_natural[, 1] <- N0

for (i in 1:years) {
  N.projected_natural[, i + 1] <- transition_natural %*% N.projected_natural[,i]
}


# manipulate data for plotting

data_managed <- as.data.frame(N.projected_managed, 
                              row.names = c("Hatchlings", "Small Juveniles", 
                                            "Large Juveniles", "Sub-Adults", "Adults"))
colnames(data_managed) <- as.character(1:(years + 1))

data_managed <- data_managed %>% 
                  rownames_to_column(., var = "Age") %>% 
                  mutate(label = proportion_managed, .after = "Age") %>% 
                  pivot_longer(3:(years + 3), names_to = "Period", values_to = "pop")


data_natural <- as.data.frame(N.projected_natural, 
                              row.names = c("Hatchlings", "Small Juveniles", 
                                            "Large Juveniles", "Sub-Adults", "Adults"))
colnames(data_natural) <- as.character(1:(years + 1))

data_natural <- data_natural %>% 
                  rownames_to_column(., var = "Age") %>% 
                  mutate(label = proportion_natural, .after = Age) %>% 
                  pivot_longer(.,3:(years + 3), names_to = "Period", values_to = "pop")


data <- left_join(data_managed, data_natural, by = c("Age","Period"))

data <- data %>% 
  relocate("label.x", .before = "pop.x") %>% 
  relocate("label.y", .before = "pop.x") 


colnames(data)[3:6] <- c("label_managed", "label_natural", "pop_managed", "pop_natural")

data <- data %>% 
  mutate(.,log_pop_managed = log(pop_managed)) %>% 
  mutate(.,log_pop_natural = log(pop_natural)) %>% 
  mutate(., Period = as.integer(Period)) %>% 
  pivot_longer(.,5:8,names_to = "type", values_to = "N") %>% 
  # this last bit is only necessary for the labels
  mutate(label_managed = if_else(Period == max(Period), label_managed, NA_real_)) %>% 
  mutate(label_natural = if_else(Period == max(Period), label_natural, NA_real_))


# plot - managed - level
data %>% 
  filter(., type == "pop_managed") %>% 
  ggplot(aes(x = Period, y = N, group = Age, color = Age)) +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = seq(0,50,10)) + 
  # ggtitle("Dynamics of the managed population") +
  annotate("rect", xmin = 5, xmax = 15, ymin = 2800, ymax = 3200, fill = "goldenrod2") +
  annotate("text", x = 10, y = 3000, label = expr(paste(lambda," = ",!! lambda_managed)))

ggsave("pop_man_level.png", height = 8, width = 12)

# plot - natural - level

data %>% 
  filter(., type == "pop_natural") %>% 
  ggplot(aes(x = Period, y = N, group = Age, color = Age)) +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = seq(0,50,10)) + 
  # ggtitle("Dynamics of natural population") +
  annotate("rect", xmin = 5, xmax = 15, ymin = 9.5, ymax = 10.5, fill = "goldenrod2") +
  annotate("text", x = 10, y = 10, label = expr(paste(lambda," = ",!! lambda_natural)))
  
ggsave("pop_nat_level.png", height = 8, width = 12)


# plot - managed - log

data %>% 
  filter(., type == "log_pop_managed") %>% 
  ggplot(aes(x = Period, y = N, group = Age, color = Age)) +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = seq(0,50,10)) +
  ylab("log(N)") + 
  # ggtitle("Dynamics of the managed population in logs (stable age proportions labelled)") +
  geom_label_repel(aes(label = if_else(is.na(label_managed), NA_character_, 
                                       paste("u = ", round(label_managed, 3)))),
                   nudge_x = 1,
                   na.rm = TRUE,
                   show.legend = F)

ggsave("pop_man_log.png", height = 8, width = 12)


# plot - natural - log

data %>% 
  filter(., type == "log_pop_managed") %>% 
  ggplot(aes(x = Period, y = N, group = Age, color = Age)) +
    geom_line() +
    scale_color_viridis_d() +
    scale_x_continuous(breaks = seq(0,50,10)) +
    ylab("log(N)") + 
    # ggtitle("Dynamics of the natural population in logs (stable age proportions labelled)") +
    geom_label_repel(aes(label = if_else(is.na(label_natural), NA_character_, 
                                         paste("u = ", round(label_natural, 3)))),
                   nudge_x = 1,
                   na.rm = TRUE,
                   show.legend = F)
  
ggsave("pop_nat_log.png", height = 8, width = 12)


# Sensitivities

sens_managed <- sensitivity(transition_managed)

sens_natural <- sensitivity(transition_natural)

# Elasticity helper matrices

helper_managed <- (1/lambda_managed) * transition_managed

helper_natural <- (1/lambda_natural) * transition_natural

# Elasticities

elast_managed <- matrix(0, nrow = 5, ncol = 5)

for(i in 1:nrow(elast_managed)){
  for(j in 1:ncol(elast_managed)){
    elast_managed[i,j] <- sens_managed[i,j]*helper_managed[i,j]
  }
}

elast_natural <- matrix(0, nrow = 5, ncol = 5)

for(i in 1:nrow(elast_natural)){
  for(j in 1:ncol(elast_natural)){
    elast_natural[i,j] <- sens_natural[i,j]*helper_natural[i,j]
  }
}

# prepare data for plotting

Ages <- 1:5

fecundity_managed <- elast_managed[1,]
same_managed <- c(elast_managed[2,1], elast_managed[3,2], elast_managed[4,3], elast_managed[5,4], NA)
next_managed <- c(elast_managed[1,1], elast_managed[2,2],elast_managed[3,3],elast_managed[4,4],elast_managed[5,5])

data_elast_managed <- as.data.frame(cbind(Ages, fecundity_managed, same_managed, next_managed))


fecundity_natural <- elast_natural[1,]
same_natural <- c(elast_natural[2,1], elast_natural[3,2], elast_natural[4,3], elast_natural[5,4], NA)
next_natural <- c(elast_natural[1,1], elast_natural[2,2],elast_natural[3,3],elast_natural[4,4],elast_natural[5,5])

data_elast_natural <- as.data.frame(cbind(Ages, fecundity_natural, same_natural, next_natural))


data_elast <- full_join(data_elast_managed, data_elast_natural, by = "Ages")

data_elast <- data_elast %>% 
  pivot_longer(.,2:7, names_to = c("Vital Rate", "Population"), names_sep = "_", values_to = "Elasticity")

# plot

ggplot(data_elast, aes(x= Ages, y= Elasticity, group = `Vital Rate`, color = `Vital Rate` )) +
  geom_line() +
  scale_color_viridis_d(labels = c("Fecundity", "Survival (next stage)", "Survival (same stage)")) +
  # ggtitle("Elasticities for the two populations") +
  facet_wrap(vars(Population))

ggsave("elasticities.png", height = 4, width = 12)
