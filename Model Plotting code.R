
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



speaker_count_table20 <- count_speakers_year(output, efficacy_threshold = 20)
speaker_count_table100 <- count_speakers_year(output, efficacy_threshold = 100)


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
    facet_wrap(~Language, ncol = length(unique(speaker_freq20$Language))) +
    theme_linedraw() +
    labs(y = "% of population") +
    ggtitle("% with 100% speaking efficacy"),
  #theme(legend.position = "none"),
  ncol = 1)

save_plot(filename = "./Figures/assortment_parentL1_Random_generational_trajectories_over_years.png", year_plot)




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

speaker_count_table20 <- count_speakers_age(output, efficacy_threshold = 20)
speaker_count_table100 <- count_speakers_age(output, efficacy_threshold = 100)

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


