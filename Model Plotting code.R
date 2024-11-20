
####### PLOTTING CODE #######


# This script contains the code to produce desired plots from the output of a series of simulation experiments about the effect of ethnolinguistic marriage rules on the generational trends in multilingualism. 


count_speakers_year <- function(output, efficacy_threshold){
  speaker_count_table <- output %>%
    group_by(year, generation) %>%
    summarise(A = round((sum(`Speaks A` >= efficacy_threshold, na.rm = T) / n() * 100), 2),
              B = round((sum(`Speaks B` >= efficacy_threshold, na.rm = T) / n() * 100), 2),
              C = round((sum(`Speaks C` >= efficacy_threshold, na.rm = T) / n() * 100), 2),
              D = round((sum(`Speaks D` >= efficacy_threshold, na.rm = T) / n() * 100), 2),
              E = round((sum(`Speaks E` >= efficacy_threshold, na.rm = T) / n() * 100), 2))
  return(speaker_count_table)
}

count_speakers_age <- function(output, efficacy_threshold){
  speaker_count_table <- output %>%
    group_by(age, generation) %>%
    summarise(A = round((sum(`Speaks A` >= efficacy_threshold, na.rm = T) / n() * 100), 2),
              B = round((sum(`Speaks B` >= efficacy_threshold, na.rm = T) / n() * 100), 2),
              C = round((sum(`Speaks C` >= efficacy_threshold, na.rm = T) / n() * 100), 2),
              D = round((sum(`Speaks D` >= efficacy_threshold, na.rm = T) / n() * 100), 2),
              E = round((sum(`Speaks E` >= efficacy_threshold, na.rm = T) / n() * 100), 2))
  return(speaker_count_table)
}



plots <- vector("list", length(sweeps))
for(i in 1:length(sweeps)){
  scenario_output <- sweeps[[i]]$output
  household <- paste("household prob = ",round(sweeps[[i]]$household_interaction_prob, 3))
  parents_choice <- paste("parents choose = ", sweeps[[i]]$parent_language_choice)
  others_choice <- paste("others choose = ", sweeps[[i]]$others_language_choice)
                     
  scenario <- paste(household, parents_choice, others_choice, sep = "
                    ")
  
  speaker_count <- count_speakers_age(scenario_output, efficacy_threshold = 100)
  speaker_freq100 <- speaker_count %>%
    pivot_longer(cols = c("A", "B", "C", "D", "E"), names_to = "Language", values_to = "Speakers")
  
  plots[[i]] <- ggplot(speaker_freq100, aes(x = age, y = Speakers)) +
    geom_line(aes(color = as.factor(Language))) +
    facet_wrap(~as.factor(generation), ncol = 1) +
    theme_linedraw() +
    labs(y = "% of population", color = "Language", caption = paste(scenario)) +
    theme(legend.position = "none")
    
    
    ggplot(speaker_freq100, aes(x = age, y = Speakers)) +
    geom_line(aes(color = as.factor(generation))) +
    facet_wrap(~Language, ncol = 1) +
    theme_linedraw() +
    labs(y = "% of population", color = "Generation", caption = paste(scenario)) +
    theme(legend.position = "none")
}





#### Plot the % of agents who have complete speaking efficacy in each language ####



speakers100 <- data.frame()
for(i in 1:length(sweeps)){
  new_output <- data.frame()
  
  for(lang in names(select(sweeps[[i]]$output, starts_with("Speaks")))){
    new_lang_output <- sweeps[[i]]$output[which(sweeps[[i]]$output$age == 49),] %>%
     # filter(lang == 100) %>%
      mutate(Language = lang)

    new_output <- rbind(new_output, new_lang_output[which(new_lang_output[lang] == 100),])
  }
  household <- paste("household prob = ", round(sweeps[[i]]$household_interaction_prob, 3))
  parents_choice <- paste("parents choose = ", sweeps[[i]]$parent_language_choice)
  others_choice <- paste("others choose = ", sweeps[[i]]$others_language_choice)
  
  new_output$scenario <- paste(household, parents_choice, others_choice, sep = "
                    ")

  speakers100 <- rbind(speakers100, new_output)
}


speakers100_plot <- speakers100  %>%
  group_by(scenario, generation, Language) %>%
  mutate(count = n()) %>%
ggplot(aes(x = generation, y = count)) +
  geom_line(aes(color = Language, group = Language)) +
  scale_x_continuous(breaks = c(0,1,3,5,7,9)) +
  facet_wrap(~ scenario) +
  theme_bw()

save_plot(speakers100_plot, filename = "./Figures/Language_Choice_percent-of-fluent-speakers.png", base_height = 8, base_width = 13)
  






#### Plot the % of agents who have complete understanding of each language ####


listeners100 <- data.frame()
for(i in 1:length(sweeps)){
  new_output <- data.frame()
  
  for(lang in names(select(sweeps[[i]]$output, starts_with("Understands")))){
    new_lang_output <- sweeps[[i]]$output[which(sweeps[[i]]$output$age == 49),] %>%
      # filter(lang == 100) %>%
      mutate(Language = lang)
    
    new_output <- rbind(new_output, new_lang_output[which(new_lang_output[lang] == 100),])
  }
  household <- paste("household prob = ", round(sweeps[[i]]$household_interaction_prob, 3))
  parents_choice <- paste("parents choose = ", sweeps[[i]]$parent_language_choice)
  others_choice <- paste("others choose = ", sweeps[[i]]$others_language_choice)
  
  new_output$scenario <- paste(household, parents_choice, others_choice, sep = "
                    ")
  
  listeners100 <- rbind(listeners100, new_output)
}


listeners100_plot <- listeners100  %>%
  group_by(scenario, generation, Language) %>%
  mutate(count = n()) %>%
  ggplot(aes(x = generation, y = count)) +
  geom_line(aes(color = Language, group = Language)) +
  scale_x_continuous(breaks = c(0,1,3,5,7,9)) +
  facet_wrap(~ scenario) +
  theme_bw()

save_plot(listeners100_plot, filename = "./Figures/Language_Choice_percent-of-fluent-understanders.png", base_height = 8, base_width = 13)
















  
no_assortment_plots <- plot_grid(plots[[1]], plots[[5]], plots[[9]], plots[[13]], ncol = 4)
low_assortment_plots <- plot_grid(plots[[2]], plots[[6]], plots[[10]], plots[[14]], ncol = 4)
mid_assortment_plots <- plot_grid(plots[[3]], plots[[7]], plots[[11]], plots[[15]], ncol = 4)
high_assortment_plots <- plot_grid(plots[[4]], plots[[8]], plots[[12]], plots[[16]], ncol = 4)

 





# OK. The starting proportions of language speakers need to be identical in each scenario. Seems there's some stochasticity there. 


speaker_count_table20 <- count_speakers_year(output, efficacy_threshold = 20)
speaker_count_table100 <- count_speakers_year(output1, efficacy_threshold = 100)


# transform speaker frequency table into long data for plotting
speaker_freq20 <- speaker_count_table20 %>%
  pivot_longer(cols = c("A", "B", "C", "D", "E"), names_to = "Language", values_to = "Speakers")
speaker_freq100 <- speaker_count_table100 %>%
  pivot_longer(cols = c("A", "B", "C", "D", "E"), names_to = "Language", values_to = "Speakers")

#### Plot: % of Population in each generation with speaking ability in each language, by year of simulation ####
year_plot <- plot_grid(
  ggplot(speaker_freq20, aes(x = year, y = Speakers)) +
    geom_line(aes(color = as.factor(generation))) +
    facet_wrap(~Language, ncol = length(unique(speaker_freq20$Language))) +
    theme_linedraw() +
    labs(y = "% of population") +
    ggtitle("% with any ability to speak") +
    theme(legend.position = "none"),
  ggplot(speaker_freq100, aes(x = year, y = Speakers)) +
    geom_line(aes(color = as.factor(generation))) +
    facet_wrap(~Language, ncol = length(unique(speaker_freq100$Language))) +
    theme_linedraw() +
    labs(y = "% of population") +
    ggtitle("% with 100% speaking efficacy"),
  #theme(legend.position = "none"),
  ncol = 1)

save_plot(filename = "./Figures/assortment_parentL1_Random_generational_trajectories_over_years.png", year_plot)




ggplot(speaker_freq100, aes(x = year, y = Speakers)) +
  geom_line(aes(color = as.factor(generation))) +
  facet_wrap(~Language, ncol = 1) +
  theme_linedraw() +
  labs(y = "% of population") +
  ggtitle("% with 100% speaking efficacy")



speaker_count_table20 <- count_speakers_age(output, efficacy_threshold = 20)
speaker_count_table100 <- count_speakers_age(output1, efficacy_threshold = 100)

# transform speaker frequency table into long data for plotting
speaker_freq20 <- speaker_count_table20 %>%
  pivot_longer(cols = c("A", "B", "C", "D", "E"), names_to = "Language", values_to = "Speakers")
speaker_freq100 <- speaker_count_table100 %>%
  pivot_longer(cols = c("A", "B", "C", "D", "E"), names_to = "Language", values_to = "Speakers")

#### Plot: % of Pop in each generation with speaking ability in each language, by agent age ####
age_plot <- plot_grid(
  ggplot(speaker_freq20, aes(x = age, y = Speakers)) +
    geom_line(aes(color = as.factor(generation))) +
    facet_wrap(~Language, ncol = length(unique(speaker_freq20$Language))) +
    theme_linedraw() +
    labs(y = "% of population") +
    ggtitle("% with any ability to speak") +
    theme(legend.position = "none"),
  ggplot(speaker_freq100, aes(x = age, y = Speakers)) +
    geom_line(aes(color = as.factor(generation))) +
    facet_wrap(~Language, ncol = length(unique(speaker_freq20$Language))) +
    theme_linedraw() +
    labs(y = "% of population") +
    ggtitle("% with 100% speaking efficacy"),
  #theme(legend.position = "none"),
  ncol = 1)



ggplot(speaker_freq100, aes(x = age, y = Speakers)) +
  geom_line(aes(color = as.factor(generation))) +
  facet_wrap(~Language, ncol = 1) +
  theme_linedraw() +
  labs(y = "% of population") +
  ggtitle("% with 100% speaking efficacy")

save_plot(filename = "./Figures/assortment_parentL1_Random_generational_age_trajectories.png", age_plot)



# make data long data for plotting.
longdata_speaks <- output %>%
  pivot_longer(cols = starts_with("Speaks"), names_to = "Language", values_to = "Efficacy")
longdata_understands <- output %>%
  pivot_longer(cols = starts_with("Understands"), names_to = "Language", values_to = "Efficacy")

plot_proficiency_trajectories <- function(longdata){
  # subset <- round(seq(from = 1, to = max(longdata$year), by = (max(longdata$year) / 4)))
  
  ggplot(longdata, aes(x = year, y = Efficacy)) +
    geom_line(aes(group = agent_id, color = as.factor(generation)), alpha = 0.25) +
    facet_wrap(~Language, ncol = 1) +
    labs(color = "Generation") +
    theme_bw()
}

#### Plot: Speaking/Understanding Trajectories over time of model run. ####
plot_grid(
  plot_proficiency_trajectories(longdata_speaks),
  plot_proficiency_trajectories(longdata_understands),
  ncol = 2
)




plot_bio_samples <- function(longdata_speaks, longdata_understands){
  sub.num <- round(seq(from = 1, to = length(unique(longdata_speaks$agent_id)), by = (length(unique(longdata_speaks$agent_id)) / 12)))
  sub <- paste("id_", sub.num, sep = "")
  
  plotdata_speaks <- longdata_speaks[which(longdata_speaks$agent_id %in% sub),] %>%
    mutate(agent_id = factor(agent_id, levels = sub))
  plotdata_understands <- longdata_understands[which(longdata_understands$agent_id %in% sub),] %>%
    mutate(agent_id = factor(agent_id, levels = sub))
  
  
  ggplot(plotdata_speaks, aes(x = age, y = Efficacy)) +
    geom_line(aes(color = Language)) +
    geom_line(data = plotdata_understands, aes(x = age, y = Efficacy, color = Language), linetype = "dashed") +
    facet_wrap(~ agent_id) +
    theme_bw()
}  


plot_bio_samples(longdata_speaks, longdata_understands)
# right now this looks a mess because the acquisition rate for understanding is the same as the acquisition rate for speaking. 





#### Experiments to Run ####

# Parameter Sweep on weighted probability of within-household interactions
# Parameter Sweep on adherence to Marriage Rules. 
# Marriage Rules: Right now they marry at random. 
# How do differences in mutual intelligibility affect outcomes?



#### Things to do ####

# Make a plot function for plotting child language trajectories grouped by parent speaking choices. 
# Make function: choose to speak language of highest speaking efficacy
# Make function: Marry based on language rules.
# Setup: Mutual Intelligibility Parameter. 
# Setup: Faster language acquisition for L3+ in children.
# Adjust Language Acqusition Rate for both speaking and understanding
# Add a Forgetting Option

# Make a 3-generation version of the model?



#### I think that's enough for today. 


# replace agent IDs with agent languages spoken.  <--- you already have code for this.
# tally up agent languages heard. <--- you already have code for this.
# tally up the languages spoken by each agent. because speaking only improves by speaking. 
# calculate agent improvements in language speaking
# calculate agent improvements in language understanding


