
####### PLOTTING CODE #######


# This script contains the code to produce desired plots from the output of a series of simulation experiments about the effect different language choice rules for dyadic conversations on the generational trends in multilingualism. 
# Agents have rules specific to:
# A child speaking to their parent
# A parent speaking to their child
# All other interactions (all of which will be outside the nuclear family/household) 



##### What do we want to show here? ####
# -	For scenarios in which parents are required to speak L1 to their children but children are not required to speak L1 to their parents, how many parents in each generation are being forced to speak to their children in a language that they do not know well?
#   o	That is not their best known language
# o	In which they have a speaking ability value <50/100?
#   -	
#   
#   
#   
#   Output to track:
#   At the end of each generation, for all agents age 49 (the oldest age)
# -	Number of proficient speakers of each language
# -	Number of 50% speakers of each language?
#   -	Number of languages spoken by each person 
# 
# Across generations within a household
# -	Parent natal language and child speaking values in each of the three languages (at age 24? (the age right before children become parents)). 

for(scenario in)

# Read in the output of the model sweep. 
sweep_results = read_model_sweep(root_output_directory = "./Model 5.0/model_output/all_random/", target_param_values = c(0.25, 0.5, 0.75))


sweep_results = read_model_sweep(root_output_directory = "./Model 5.0/model_output/unbalanced_speakers/parentL1_random/", target_param_values = c(0.25, 0.5, 0.75))



#  This function returns a table summarizing the number of speakers of each language in each generation. You must specify the age of the speakers you want to count, and the threshold of speaking ability that you will count as 'speaking' the language. Defaults to 100 (mastery)
count_age_specific_speakers <- function(output, efficacy_threshold = 100, count_age){
  speaker_count_table <- output %>%
    filter(age == count_age) %>%
    select(agent_id, starts_with("Speaks"),) %>%
    mutate(generation = (agent_id - 1) %/% 100 + 1) %>%
    pivot_longer(cols = starts_with("Speaks"), names_to = "language", values_to = "speaking_ability") %>%
    mutate(language = substr(language, 8,8)) %>% # grab just the 8th (last) character (the letter designation of the language -- A, B, etc.)
    group_by(generation, language) %>%
    summarise(speakers_percent = round(sum(speaking_ability >= efficacy_threshold, na.rm = T)), .groups = "keep")

  return(speaker_count_table)
}




### Create a list of # of speakers of each language, aged 49, for each run of the model across each target_param value of the sweep
speaker_counts_25 <- list() # Initialize the list to store results
# fill the list
 for(param_set_name in names(sweep_results)){
   ### Count the number of speakers of each language at the end of each generation
  count_speakers <- lapply(seq_along(sweep_results[[param_set_name]]), function(rep_index) {
    # Access the data frame for the current repetition
    rep_data <- sweep_results[[param_set_name]][[rep_index]]
    
    # Count speakers
    speakers <- count_age_specific_speakers(rep_data, count_age = 25)
  
    # Assign the repetition number
    speakers$rep = rep_index
    
    return(speakers)
  })
  
  speaker_counts_25[[param_set_name]] <- do.call(rbind, count_speakers)
  speaker_counts_25[[param_set_name]] <- speaker_counts_25[[param_set_name]] %>%
    clean_names(case = "big_camel") # changes column names from snake_case to UpperCamel (makes axis labeling of plots prettier since single-word variable names will all be capitalized)
}




speaker_counts_49 <- list() # Initialize the list to store results
# fill the list
for(param_set_name in names(sweep_results)){
  ### Count the number of speakers of each language at the end of each generation
  count_speakers <- lapply(seq_along(sweep_results[[param_set_name]]), function(rep_index) {
    # Access the data frame for the current repetition
    rep_data <- sweep_results[[param_set_name]][[rep_index]]
    
    # Count speakers
    speakers <- count_age_specific_speakers(rep_data, count_age = 49)
    
    # Assign the repetition number
    speakers$rep = rep_index
    
    return(speakers)
  })
  
  speaker_counts_49[[param_set_name]] <- do.call(rbind, count_speakers)
  speaker_counts_49[[param_set_name]] <- speaker_counts_49[[param_set_name]] %>%
    clean_names(case = "big_camel") # changes column names from snake_case to UpperCamel (makes axis labeling of plots prettier since single-word variable names will all be capitalized)
}




# - Number of languages spoken by each person
# - Plot/count by languages known
# Plot, % of population:
# monolingual in A, B, C
# bilingual in AB, AC, BC
# trilingual in ABC

# speaking values > 50 in all languages?





# plot_data = a list containing one summary data frame per model scenario. Multiple reps of the same scenario should be stored in the same data frame and labeled with their rep number. (The functions above will do this)

# first, make a function that forces axis labels to be integers
int_breaks <- function(x, n = 5) {
  l <- pretty(x, n)
  l[abs(l %% 1) < .Machine$double.eps ^ 0.5]
}
  # ^ This isn't necessary if you re-structure the 'Generation' variable in the functions above so that it's already read as an integer. But if you want to use geom_smooth(), it needs a continuous x variable.



### Function to create a line plot from model output - one plot for each scenario, saved in a list of ggplot objects

# plot_data = a list of data frames, each of which contains summary data from model output, ready for plotting
# x = name of the variable to plot on the x axis
# y = name of the variable to plot on the y axis
# split_by = name of the variable to map onto the color and fill arguments for ggplot (probably Language)
# labs_x, labs_y = character strings, specifying the axis labels for the plots. Defaults to x and y, but if these variable names aren't ideal for pretty plots, specify the preferred text here. 
# group = a character string, specifying the grouping variable. Defaults to 'Rep', the variable name that designates which model run the summary data belong to. This should have the effect of plotting a separate line for each language for each run of the model. 

lineplot_model_output <- function(plot_data, x, y, 
                              split_by, 
                              labs_x = x, 
                              labs_y = y,
                              group = "Rep",
                              title = "Proportion of conversations within own household") {
  # Initialize a list to store the plots
  plots <- vector("list", length(plot_data) + 1)
  
  for (scenario in seq_along(plot_data)) {
    # Extract scenario title
    parts <- strsplit(names(plot_data)[[scenario]], "=")[[1]]
    plot_title <- paste0(title, " = ", parts[[2]])
    data <- plot_data[[scenario]]  # Extract data for this scenario
    
    # Save the legend in the first iteration
    if (scenario == 1) {
      legend_plot <- ggplot(data, aes(
        x = .data[[x]], 
        y = .data[[y]], 
        color = as.factor(.data[[split_by]]), 
        fill = as.factor(.data[[split_by]]),
        group = interaction(.data[[group]], .data[[split_by]])
      )) +
        geom_line() +
        theme_linedraw() +
        labs(x = labs_x, y = labs_y, title = plot_title, color = split_by) +
        scale_color_viridis_d(guide = "legend") +
        scale_fill_viridis_d(guide = "legend") +
        theme(legend.position = "bottom") 
      
      # Add legend to the last position in the plots list
      plots[[length(plots)]] <- get_legend(legend_plot)
      names(plots)[[length(plots)]] <- "legend"
    }
    
    # Create the plot for the current scenario (without the legend)
    plots[[scenario]] <- ggplot(data, aes(
      x = .data[[x]], 
      y = .data[[y]], 
      color = as.factor(.data[[split_by]]), 
      fill = as.factor(.data[[split_by]]),
      group = interaction(.data[[group]], .data[[split_by]])
    )) +
      geom_line() +
      theme_linedraw() +
      scale_x_continuous(expand = c(0.01, 0.01), breaks = int_breaks) +
      labs(x = labs_x, y = labs_y, title = plot_title, color = split_by) +
      scale_color_viridis_d(guide = "legend") + 
      scale_fill_viridis_d(guide = "legend") +
      theme(legend.position = "none")
  }
  
  return(plots)
}



speaker_count_25_plots = lineplot_model_output(speaker_counts_25, 
                                        x = "Generation", 
                                        y = "SpeakersPercent",
                                        split_by = "Language",
                                        labs_y = "% of population")


speaker_count_49_plots = lineplot_model_output(speaker_counts_49, 
                                               x = "Generation", 
                                               y = "SpeakersPercent",
                                               split_by = "Language",
                                               labs_y = "% of population")


# Make compound plot: Number of speakers of each language in each generation at age 25, across parameter sweep values
plot_25 = plot_grid(plotlist = speaker_count_25_plots,
          ncol = 1, rel_heights = c(rep(1, length(speaker_count_plots) - 1),0.25))

# Compound plot: # of speakers of each language in each generation at age 49, across parameter sweep values
plot_49 = plot_grid(plotlist = speaker_count_49_plots,
          ncol = 1, rel_heights = c(rep(1, length(speaker_count_plots) - 1),0.25))


save_plot(plot_25, filename = )
























# -	It looks like ‘random consistent’ and ‘L1’ don’t actually matter because each of the languages has the same probability of being chosen as a parental language under these two scenarios. Except that under the random choice scenario, children seem to learn the prestige language slightly faster. Not sure why. Maybe this is stochastic variation b/c right now these are all single runs. 


speakers100_table <- speakers100 %>%
  group_by(scenario, generation, Language) %>%
  summarise(speakers = n()) %>% arrange(scenario, Language)








listeners-all <- data.frame()
for(i in 1:length(sweeps)){
  new_output <- data.frame()
  
  for(lang in names(select(sweeps[[i]]$output, starts_with("Understands")))){
    new_lang_output <- sweeps[[i]]$output[which(sweeps[[i]]$output$age == 49),] %>%
      mutate(Language = lang)
    
    new_output <- rbind(new_output, new_lang_output)
  }
  household <- paste("household prob = ", round(sweeps[[i]]$household_interaction_prob, 3))
  parents_choice <- paste("parents choose = ", sweeps[[i]]$parent_language_choice)
  others_choice <- paste("others choose = ", sweeps[[i]]$others_language_choice)
  
  new_output$scenario <- paste(household, parents_choice, others_choice, sep = "
                    ")
  
  listeners_all <- rbind(listeners100, new_output)
}

listeners_long <- pivot_longer(listeners_all, cols = starts_with("Understands"), values_to = "Understanding", names_to = "Languages") %>% 
  select(agent_id, Understanding, Languages, generation) %>%
  unique()
#### Haven't cracked this plot code yet - Distribution of values for speaking/understanding ability #### ******
understands <- names(sweeps[[1]]$output %>% select(starts_with("Understands")))


##### * -	Distribution of speaking values for each language in each generation at age 49.
-	Distribution of understanding values for each language in each generation at age 49.
-	How many generations to run the prestige scenarios before the other languages are dead? <------------
  
  
  
  
listeners_all_plot <- listeners_long  %>%
 # pivot_longer(cols = understands, names_to = "Understands", values_to = "Understanding value") %>%
  ggplot(aes(x = Understanding)) +
  geom_histogram(aes(fill = Languages), position = "dodge") +
  facet_wrap(~as.factor(generation))



  geom_bar(aes(group = as.factor(generation), fill = as.factor(generation)), position = "dodge") +
  facet_wrap(~ as.factor(generation))
  geom_histogram(aes(fill = as.factor(generation)), alpha = 0.5) +
  geom_histogram(data = listeners_all, aes(x = `Understands C`, color = as.factor(generation)), alpha = 0.3) +
  geom_jitter(data = listeners_all, aes(x = as.factor(generation), y = `Understands D`), color = "purple", alpha = 0.3) +
  geom_jitter(data = listeners_all, aes(x = as.factor(generation), y = `Understands E`), color = "green", alpha = 0.3) +
  geom_jitter(data = listeners_all, aes(x = as.factor(generation), y = `Understands A`)) 
  scale_x_continuous(breaks = c(0,1,3,5,7,9)) +
  facet_wrap(~ scenario) +
  theme_bw()


  
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


