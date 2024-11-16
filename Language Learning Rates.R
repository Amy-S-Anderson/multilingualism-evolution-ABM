
library(cowplot)
library(tidyverse)


### Random Baseline
# Number of agents = 200
# Marriage = Random pairings
# The only possible parameters to sweep are:
- Rate of learn_languages_by_listening()
- Rate of learn_languages_by_speaking()



params <- data.frame(d = 18, a = 0.2, r0 = 9)

df <- data.frame(ages = seq(from = 0, to = 51, by = 1))
df$age_factor <-  params$r0 * (1 - (1 / (1 + exp(-params$a * (df$ages - params$d))))) + 0.5
# d = 10 - 25
# a = 0.05 - 0.35
# r0 = 4 - 14

d_vec = seq(from = 10, to =26, by = 4)
a_vec = seq(from = 0.05, to = 0.35, by = 0.05)
r0_vec = seq(from = 4, to = 14, by = 2)

for(d in d_vec){
  d_name <- paste("d=", d, sep = "")
  df[d_name]  <-  params$r0 * (1 - (1 / (1 + exp(-params$a * (df$ages - d))))) + 0.5
}


d_test <- df %>%
  select(ages, `d=10`:`d=25`) %>%
  pivot_longer(cols = "d=10":"d=25", names_to = "d_value", values_to = "age_rate")


d_plot <- ggplot(d_test, aes(x = ages, y = age_rate, color = as.factor(d_value))) + 
  geom_line() + 
  labs(color = "d value") +
  theme_bw()

d_test <- d_test %>%
  group_by(d_value) %>%
  mutate(language_level = if_else(cumsum(age_rate) <= 100, cumsum(age_rate), 100))


d_langskill_plot <- ggplot(d_test, aes(x = ages, y = language_level, color = as.factor(d_value))) + 
  geom_line() + 
  labs(color = "d value") +
  theme_bw()

#######
for(a in a_vec){
  a_name <- paste("a=", a, sep = "")
  df[a_name]  <-  params$r0 * (1 - (1 / (1 + exp(-a * (df$ages - params$d))))) + 0.5
}


a_test <- df %>%
  select(ages, `a=0.05`:`a=0.35`) %>%
  pivot_longer(cols = "a=0.05":"a=0.35", names_to = "a_value", values_to = "age_rate")


a_plot <- ggplot(a_test, aes(x = ages, y = age_rate, color = as.factor(a_value))) + 
  geom_line() + 
  labs(color = "a value") +
  theme_bw()


a_test <- a_test %>%
  group_by(a_value) %>%
  mutate(language_level = if_else(cumsum(age_rate) <= 100, cumsum(age_rate), 100))


a_langskill_plot <- ggplot(a_test, aes(x = ages, y = language_level, color = as.factor(a_value))) + 
  geom_line() + 
  labs(color = "a value") +
  theme_bw()

#######

for(r0 in r0_vec){
  r0_name <- paste("r0=", r0, sep = "")
  df[r0_name]  <-  r0 * (1 - (1 / (1 + exp(-params$a * (df$ages - params$d))))) + 0.5
}


r0_test <- df %>%
  select(ages, `r0=4`:`r0=14`) %>%
  pivot_longer(cols = "r0=4":"r0=14", names_to = "r0_value", values_to = "age_rate")

r0_plot <- ggplot(r0_test, aes(x = ages, y = age_rate, color = as.factor(r0_value))) + 
  geom_line() + 
  labs(color = "r0 value") +
  theme_bw()


save_plot(filename = "./Figures/age_rate_sweep.png", plot_grid(d_plot, a_plot, r0_plot, ncol = 1), 
          base_height = 5)
  


# Look at how changing these parameters affects cumulative language skills
d_vec = seq(from = 10, to =26, by = 4)
a_vec = seq(from = 0.05, to = 0.35, by = 0.1)
r0_vec = seq(from = 4, to = 16, by = 4)


# Generate all combinations using the expand.grid() command
param_combinations <- expand.grid(d = d_vec, a = a_vec, r0 = r0_vec)


param_combinations_list <- split(param_combinations, seq(nrow(param_combinations)))

langskills <- expand.grid(ages =  seq(0,50,1), d = d_vec, a = a_vec, r0 = r0_vec)

param_names <- apply(param_combinations, 1, function(row) {
  paste("d=", row["d"], ", a=", row["a"], ", r0=", row["r0"], sep = "")
})

param_combinations$param_names <- param_names


  #param_name <- paste("d=", langskills$d[params], "a=", langskills$a[params], "r0=", langskills$r0[params], sep = " ")
langskills$age_rate <-  langskills$r0 * (1 - (1 / (1 + exp(-langskills$a * (langskills$ages - langskills$d))))) + 5 # 5 = % of language learned each year as an adult in a full immersive context
langskills$param_combo <- paste("d=", langskills$d, "a=", langskills$a, "r0=", langskills$r0, sep = " ")
langskills <- langskills %>% group_by(param_combo) %>%
  mutate(cumulative = if_else(cumsum(age_rate) <= 100, cumsum(age_rate), 100),
         age_threshold = min(ages[cumulative == 100], na.rm = TRUE))


param_sweep_plot <- ggplot(langskills, aes(x = ages, y = age_rate, color = param_combo)) +
  geom_line() +
  theme_bw() +
  theme(legend.position = "none")
save_plot(filename = "./Figures/all_possible_learning_rates.png", param_sweep_plot)







d_langskill_plot <- ggplot(langskills, aes(x = ages, y = cumulative)) + 
  geom_line(aes(group = param_combo, color = param_combo)) + 
 # scale_x_continuous(limits = c(0,25)) +
  labs(color = "d value", y = "Language skills") +
  theme_bw() +
  theme(legend.position = "none")



rates <- ggplot(langskills[which(langskills$age_threshold %in% c(10,11,12)),], aes(x = ages, y = age_rate)) + 
  geom_line(aes(group = param_combo, color = param_combo)) + 
  # scale_x_continuous(limits = c(0,25)) +
  labs(color = "d value", x = "Ages", y = "Language Learning Rate") +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("")

cumsums <- ggplot(langskills[which(langskills$age_threshold %in% c(10,11,12)),], aes(x = ages, y = cumulative)) + 
  geom_line(aes(group = param_combo, color = param_combo)) + 
  # scale_x_continuous(limits = c(0,25)) +
  labs(color = "d value", x = "Ages", y = "Language Skills") +
  theme_bw() +
  theme(legend.position = "none")

save_plot(filename = "./Figures/param_combos_with_reasonable_age_of_saturation.png", plot_grid(rates, cumsums)


### If I raise the baseline learning rate to 10% annually, then everyone learns their first language fully by age 7, no matter what the other parameters are. 

save_plot(filename = "./Figures/cumulative_learning_curves_all_possible_parameters_10perc_baseline.png", d_langskill_plot)

function(param, paramset){
  for(i in paramset){
    plot_data <- langskills %>%
      filter(langskills[param] == paramset[i])
    
    plotggplot(plot_data)
  }




plots <- list()
for(i in d_vec){
  
  i = 1
  plots[[i]] <- langskills_long %>%
    filter(d == d_vec[i]) %>%
  ggplot(langskills_long[which(langskills_long$d == d_vec[i]),], aes(x = ages, y = language_level, color = as.factor(param_combo))) + 
    geom_line() + 
    labs(color = "d value") +
    theme_bw() 
}




age_of_saturation <- langskills %>%
  filter(cumulative == 100) %>%
  group_by(param_combo) %>%
  summarise(age_threshold = min(ages))
age_of_saturation$param_names <- age_of_saturation$param_combo

tmp <- age_of_saturation %>%
  filter(age_threshold == 12)
ggplot(langskills[which ], aes(x = ages, y = cumulative)) +
  geom_line(aes(color = param_combo))

param_combinations <- left_join(param_combinations, age_of_saturation, by = "param_names")

for(a)





sweep_1d <- 

sweep_3d <- function(base_parameters, 
                     parameters_to_override = c(override1, override2, override3), 
                     overriding_values= c(param1, param2, param3), 
                     num_runs,
                     root_output_directory = "./Simulated Data/sweeps"){
  for(p1 in param1){
    for(p2 in param2){
      for(p3 in param3){
        param_set = load_default_params()
        param_set[parameters_to_override[1]] = p1
        param_set[parameters_to_override[2]] = p2
        param_set[parameters_to_override[3]] = p3
        param_set["output_directory"] = paste0(root_output_directory, "/", parameters_to_verride[1], "=", p1, "_",
                                               parameters_to_verride[2], "=", p2, "_",
                                               parameters_to_verride[3], "=", p3)
        run_model(param_set)
        }
    }
  }
}



df <- data.frame(ages = seq(from = 0, to = 51, by = 1),
  age_factor <-  params$r0 * (1 - (1 / (1 + exp(-params$a * (d$ages - params$d))))) + 0.5)

params <- data.frame(d = 18, a = 0.2, r0 = 9)
for(d in d_vec){
  d_name <- paste("age_factor_d=", d, sep = "")
  df[d_name]  <-  params$r0 * (1 - (1 / (1 + exp(-params$a * (df$ages - d))))) + 0.5
}


test <- df %>%
  pivot_longer(cols = "age_factor....params.r0....1....1..1...exp..params.a....d.ages...":"age_factor_d=25", names_to = "d_value", values_to = "age_rate")

  
ggplot(test, aes(x = ages, y = age_rate, color = as.factor(d_value))) + 
  geom_line()


# function for 2D sweep, 1D sweep, 3D sweep