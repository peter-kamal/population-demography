#________ POPULATION DEMOGRAPHY FINAL PROJECT__________#
#______ REPLICATION OF BALLADARES ET AL. (2020)________#
#______________ PETER KAMAL, JIA WANG _________________#

# load necessary packAges

library(popbio)
library(tidyverse)

# define transition matrices

transition_managed <- matrix(c(0,0,0,21,56.9,0.3,0.248,0,0,0,0,0.082,0.726,
                               0,0,0,0,0.214,0.97,0,0,0,0,0.081,0.81),
                             nrow = 5, byrow = T)

transition_natural <- matrix(c(0,0,0,21,56.9,0.08,0.248,0,0,0,0,0.082,0.726,
                               0,0,0,0,0.214,0.97,0,0,0,0,0.081,0.81),
                             nrow = 5, byrow = T)


# get growth rates and age proportions

lambda_managed <- lambda(transition_managed)

lambda_natural <- lambda(transition_natural)

proportion_managed <- stable.stage(transition_managed)

proportion_natural <- stable.stage(transition_natural)

# Population dynamics

# start with one hatchling individual and track population over 100 years
N0 <- matrix(c(1, 0, 0,0,0), ncol = 1)
years <- 50

# Compute the projection - managed population

N.projected_managed <- matrix(0, nrow = 5, ncol = years+1)
N.projected_managed[, 1] <- N0

for (i in 1:years) {
  N.projected_managed[, i + 1] <- transition_managed %*% N.projected_managed[,i]
}

# Compute the projection - natural population

N.projected_natural <- matrix(0, nrow = 5, ncol = years+1)
N.projected_natural[, 1] <- N0

for (i in 1:years) {
  N.projected_natural[, i + 1] <- transition_natural %*% N.projected_natural[,i]
}


# Manipulate data for plotting

data_managed <- as.data.frame(N.projected_managed, 
                              row.names = c("Hatchlings", "Small Juveniles", 
                                            "Large Juveniles", "Sub-Adults", "Adults"))
colnames(data_managed) <- as.character(1:(years + 1))

data_managed <- data_managed %>% 
                  rownames_to_column(., var = "Age") %>% 
                  mutate(label = proportion_managed, .after = Age) +
                  pivot_longer(.,3:(years + 3), names_to = "Period", values_to = "pop")


data_natural <- as.data.frame(N.projected_natural, 
                              row.names = c("Hatchlings", "Small Juveniles", 
                                            "Large Juveniles", "Sub-Adults", "Adults"))
colnames(data_natural) <- as.character(1:(years + 1))

data_natural <- data_natural %>% 
                  rownames_to_column(., var = "Age") %>% 
                  mutate(label = proportion_natural, .after = Age) +
                  pivot_longer(.,3:(years + 3), names_to = "Period", values_to = "pop")


data <- left_join(data_managed, data_natural, by = c("Age", "Period"))
colnames(data)[4:5] <- c("pop_managed", "pop_natural")

data <- data %>% 
  mutate(.,log_pop_managed = log(pop_managed)) %>% 
  mutate(.,log_pop_natural = log(pop_natural)) %>% 
  mutate(., Period = as.integer(Period)) %>% 
  pivot_longer(.,4:7,names_to = "type", values_to = "N")

# Plot
data %>% 
  filter(., type == "pop_managed") %>% 
  ggplot(aes(x = Period, y = N, group = Age, color = Age)) +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = seq(0,50,10)) + 
  ggtitle("Dynamics of the managed population") +
  annotate("rect", xmin = 5, xmax = 15, ymin = 2800, ymax = 3200, fill = "goldenrod2") +
  annotate("text", x = 10, y = 3000, label = expr(paste(lambda," = ",!! lambda_managed)))



data %>% 
  filter(., type == "pop_natural") %>% 
  ggplot(aes(x = Period, y = N, group = Age, color = Age)) +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = seq(0,50,10)) + 
  ggtitle("Dynamics of natural population") +
  annotate("rect", xmin = 5, xmax = 15, ymin = 9.5, ymax = 10.5, fill = "goldenrod2") +
  annotate("text", x = 10, y = 10, label = expr(paste(lambda," = ",!! lambda_natural)))
  

data %>% 
  filter(., type == "log_pop_managed") %>% 
  ggplot(aes(x = Period, y = N, group = Age, color = Age)) +
  geom_line() +
  scale_color_viridis_d() +
  scale_x_continuous(breaks = seq(0,50,10)) +
  ylab("log(N)") + 
  ggtitle("Dynamics of the managed population in logs")


data %>% 
  filter(., type == "log_pop_managed") %>% 
  ggplot(aes(x = Period, y = N, group = Age, color = Age)) +
    geom_line() +
    scale_color_viridis_d() +
    scale_x_continuous(breaks = seq(0,50,10)) +
    ylab("log(N)") + 
    ggtitle("Dynamics of the natural population in logs")
    
expression(alpha)

