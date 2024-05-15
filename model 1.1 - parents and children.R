### Describe what this script does. 
### Assign some agents to have starting age 25. Monolingual parents to newborns.
### Every round, each agent has a minimum of 10 conversations. Conversational partners are sampled with replacement, so some agents may end up having more than ten conversations because they show up multiple times in other agents' list of conversants. 


##### Next items on the To-Do List ####
# - separate the model function call into separate, specific functions that can be called in order.
# - generate agent IDs as a sequence 'id_#' to fix the problem of repeating random alphanumeric IDs
# - correct the call to filtered_mat so that it can handle matrices in which the agent is sampled in alter rows that occur before the agent's ego row. 
# - change agent language banks from a series of list/vectors to a matrix containing all language values
## implement a mortality function
## implement a fertility function
## make a function to generate the population age structure that the chosen vital rates produce
### Run a multi-generational model in which all agents start out multilingual, but there are 3 languages in the population. 


### Load libraries
library(tidyverse)
library(cowplot)


id_#
# Function to generate a random alphanumeric identifier
generate_alphanumeric_id <- function(length) {
  # Define the pool of characters (letters and numbers)
  chars <- c(0:9, letters)
  
  # Sample characters randomly
  random_chars <- sample(chars, length, replace = TRUE)
  
  # Combine sampled characters into a single string
  id <- paste0(random_chars, collapse = "")
  
  return(id)
}


# Generate a unique alphanumeric identifier for every parent
parents <- NULL
for(i in seq(200)){
  parents[i] <- generate_alphanumeric_id(3)
}
# match up parent couples
couples <- matrix(parents, ncol = 2, byrow = TRUE)
couples <- as.data.frame(couples)
colnames(couples) <- c("father", "mother")
# Generate identifiers for the child generation
agent <- NULL
for(i in seq(100)){
  agent[i] <- generate_alphanumeric_id(3)
}

# create family groups
families <- cbind(couples, agent) %>% mutate(family = seq(1:length(agent)))
all_IDs <- families %>% pivot_longer(cols = c("mother", "father", "agent"), values_to = "pid", names_to = "role")
#all_IDs$age <- if_else(all_IDs$roles == "agents", 0, 25) ### This is now a group of 25-year-olds who have all just become parents to newborns

# Designate languages in play. 
languages <- c("A", "B", "C", "D")
# languages <- "A"


# Initialize data list: Each agent is either a 25-year-old monolingual parent, or a newborn.
establish_population <- function(agents, languages){ 
  data <- vector(mode = "list", length = nrow(agents))
  names(data) <- agents$pid
  agents <- agents
  # agents$parent_language <- NA
  
  
  # Vector of NAs the length of tmax
  
  for(i in seq(agents$pid))
  {
    
    parent_tongue <- sample(languages, size = 1, replace = TRUE)
    # Create an empty language bank for each agent
    agent_languages <- lapply(languages, function(lang) vector(mode = "list", length = 1))
    names(agent_languages) <- languages
    
    
    agent_languages$role <- agents$role[i]
    if(agents$role[i] %in% c("mother", "father")){
      agent_languages$age[1] <- 25
      for (j in 1:length(languages)) {
        if (parent_tongue == names(agent_languages[j])) {
          agent_languages[[j]][1] <- 100
          agents$parent_language[i] <- languages[j]
        }
        else(agent_languages[[j]][1] <- 0)
      }
      
    } 
    if(agents$role[i] == "agent"){
      (for (j in seq(agent_languages)) {
        agent_languages[[j]][1] <- 0
      })
      agent_languages$age[1] <- 0
    }
    data[[i]] <- agent_languages
    
    
  }
  
  pop <- list(data, agents)
  names(pop) <- c("pop", "agents")
  return(pop)
}

# starting population of 200 new monolingual parents and their new singleton births, in a society with four languages spoken. 
pop <- establish_population(all_IDs, languages)
agents <- pop$agents
pop <- pop$pop

family_status <- agents %>%
  pivot_wider(id_cols = c("family"), names_from = "role", values_from = "parent_language") %>%
  group_by(family) %>%
  mutate(family_language = if_else(mother == father, "monolingual", "bilingual")) %>%
  select(family, family_language)

all_IDs <- left_join(all_IDs, family_status, by = "family") # now family language status (monolingual vs bilingual) is labelled.


### functions that power the model in separate files from the model function, also separate from analysis of model. 


#### THE MODEL FUNCTION ####
update_language_bank <- function(pop, families, all_IDs, family_weight = 1, tmax) {
  
  
  language_counts <- matrix(0, nrow = tmax, ncol = length(languages), 
                            dimnames = list(seq(tmax), languages))
  
  conversant_counts <- matrix(0, nrow = tmax, ncol = length(all_IDs$pid), 
                              dimnames = list(seq(tmax), all_IDs$pid))
  
  conversation_counts <- matrix(0, nrow = tmax, ncol = length(all_IDs$pid), 
                                dimnames = list(seq(tmax), all_IDs$pid))
  
  
  for (t in seq(tmax)) { # for each new year in model time
    
    
    # matrix of agent interactions in this round. 
    interactions <- matrix(NA, nrow(all_IDs), 11)
    
    # fill out the interaction matrix
    #   n_runs <- 100
    for (i in seq(pop)){ #seq(n_runs)){ 
      agent_id <- names(pop)[i]
      family <- families %>% filter(mother == agent_id | father == agent_id | agent == agent_id) # pid anywhere in row
      
      # set probability weighting for interaction, based on relatedness. Agents are 400x more likely to interact with a family member than any given other person.
      alters <- all_IDs[which(all_IDs$pid != agent_id),]
      family_weighting <- if_else(alters$family == family$family, family_weight, 1) # can change this to be a function of agent's age
      # each agent is exposed to 10 speech interactions, possibly repeats with the same person. 
      # each agent gets to be the ego for 10 interactions, but they will likely show up as an alter in another agent's ego row. As a result, agents experience a minimum of 10 conversations, but may experience many more. 
      conversants <- sample(alters$pid, size = 10, replace = TRUE, prob = family_weighting)
      interactions[i,] <- c(agent_id, conversants)
      
    }
    
    
    
    ### Now that the interaction matrix for this round is generated, need to count up the actual interactions each agent has had. 
    
    # Initialize a data frame to count up the number of times each agent is exposed to each language.
    language_exposure <- data.frame(matrix(NA, 1, length(languages) + 1))
    colnames(language_exposure) <- c("agent_id", languages)
    
    relative_exposures <- data.frame(matrix(NA, 1, length(languages) + 1))
    colnames(relative_exposures) <- c("agent_id", languages)
    
    for(i in seq(pop)){
      agent_id <- names(pop)[i]
      # subset interaction matrix: only the rows that contain agent i. 
      filtered_mat <- interactions[apply(interactions, 1, function(row) any(row == agent_id)), ]
      
      # Extract all alter agent IDs from the rows where the agent's ID appears in the first column (agent i's ego row)
      interactions_from_ego_rows <- interactions[interactions[, 1] == agent_id, -1]
      
      # Extract all agent IDs from the first column, excluding the agent's own ID (agent i's appearance as an alter in other agents' ego rows)
      interactions_from_alter_rows <- filtered_mat[filtered_mat[ ,1]!= agent_id, 1] ### agent ID row is NOT ALWAYS ONE
      
      # Combine the two sets of interacted agents
      all_conversant_IDs <- c(interactions_from_ego_rows, interactions_from_alter_rows)
      spoken <- NA
      
      conversant_counts[t,i] <- length(unique(all_conversant_IDs))
      conversation_counts[t,i] <- length(all_conversant_IDs)
      
      
      for(speech in all_conversant_IDs){
        
        # Identify the languages currently known by each agent who has spoken to agent i.
        ### Proficiency threshold for speech is set at 20 because this is the proficiency value achieved by a two-year-old in a monolingual exposure.
        speaker_languages <- names(purrr::keep(pop[[speech]][1:length(languages)], function(x) all(x > 20)))
        
        # Each speaker picks a language to speak —- sampled at random from each individual's language bank.
        spoken[speech] <- if(length(speaker_languages > 0)) {
          sample(speaker_languages, 1) 
        } else(NA)
        spoken <- spoken[which(!is.na(spoken))]
        
        # count the number of times agent i experienced being spoken to in each language.
        exposure_count <- NA
        for(lang in seq(languages)){
          exposure_count[lang] <- length(spoken[which(spoken == languages[lang])])
        }
        language_exposure[i,] <- c(agent_id, exposure_count)
        relative_exposures[i, -1] <- exposure_count / max(exposure_count)
        relative_exposures[i, 1] <- agent_id
      }
    }
    
    #### Look into break points #### 
    #### separate the agent state and the output ####
    
    # The amount of proficiency gained is a function of the listener's age.
    params <- data.frame(d = 18, a = 0.5, r0 = 9, tc = 0) # parameter values chosen by simulating a monolingual cohort that achieves total proficiency in birth language by age 10. 
    
    
    ### Learning by Listening ###
    for(i in seq(pop)){
      agent_id <- names(pop)[i]
      age_factor <- params$r0 * (1 - (1 / (1 + exp(-params$a * (pop[[agent_id]]$age[t] - params$tc - params$d))))) + 0.5
      
      for(lang in seq(languages)){
        if(pop[[agent_id]][[lang]][t] < 100){
          pop[[agent_id]][[lang]][t + 1] <- modify_depth(pop[[agent_id]][[lang]][t], 1, ~ .x + (age_factor * relative_exposures[i, lang + 1]))
        }else(pop[[agent_id]][[lang]][t + 1] <- 100)
        if(pop[[agent_id]][[lang]][t + 1] > 100){
          pop[[agent_id]][[lang]][t + 1] <- 100
        }
        
        # Update language speaker counts for this time step
        if (pop[[agent_id]][[lang]][t] > 0) {
          language_counts[t, lang] <- language_counts[t, lang] + 1
        }
      }
      
      
      pop[[agent_id]]$age[t + 1] <- pop[[agent_id]]$age[t] + 1
    }
  }
  
  output <- list(pop, conversant_counts, conversation_counts)
  names(output) <- c("pop", "conversant_counts", "conversation_counts")
  return(output)
}



### Trial run of model function


test_random_interactions <- update_language_bank(pop, families, all_IDs, tmax = 40) # test run: 40 years of language learning


test_family_weighted <- update_language_bank(pop, families, all_IDs, tmax = 50, family_weight = 400)

### Every couple of runs, the line above gives me an error code: Error in filtered_mat[, 1] : incorrect number of dimensions. 
# Why?

test_mono <- update_language_bank(pop, families, all_IDs, tmax = 40, family_weights = 1)





### Uniform age distribution, run with vital rates, burn-in time, should produce the proper age structure after a few generations


kids <- test_random_interactions$pop[names(test_random_interactions$pop) %in% all_IDs[which(all_IDs$role == "agent"),]$pid]

bilingual_kids <- kids[names(kids) %in% all_IDs[which(all_IDs$family_language == "bilingual"),]$pid]

adults <- test_random_interactions$pop[names(test_random_interactions$pop) %in% all_IDs[which(all_IDs$role %in% c("mother", "father")),]$pid]

bilingual_partnerships <- adults[names(adults) %in% all_IDs[which(all_IDs$family_language == "bilingual"),]$pid]



weighted_kids <- test_family_weighted$pop[names(test_family_weighted$pop) %in% all_IDs[which(all_IDs$role == "agent"),]$pid]
weighted_adults <- test_family_weighted$pop[names(test_family_weighted$pop) %in% all_IDs[which(all_IDs$role %in% c("mother", "father")),]$pid]



# Define a function to summarize language bank at each time point: e.g., Number of speakers of language A at time t
generate_speaker_summaries <- function(bank, languages, tmax, proficiency_threshold) {
  language_counts <- matrix(0, nrow = tmax, ncol = length(languages),
                            dimnames = list(seq(tmax), languages))
  
  # gen1 <- language_counts
  # gen2 <- language_counts
  # gen1_size <- 0
  # gen2_size <- 0
  #speaker_counts <- data.frame(matrix(NA, 1, length(languages) + 1))
  #colnames(speaker_counts) <- c("Time", languages)
  #data.frame(c(Time = seq(tmax),))
  
  for (t in seq(tmax)) {
    for (i in seq(bank)) {
      agent_id <- names(bank)[i]
      # if(bank[[agent_id]]$role == "0"){
      #   gen2_size <- gen2_size + 1
      # }   
      #   else{
      #     gen1_size <- gen1_size + 1}
      
      for (lang in languages) {
        if (bank[[agent_id]][[lang]][[t]] > proficiency_threshold) {
          language_counts[t, lang] <- language_counts[t, lang] + 1
        }
        # if(bank[[agent_id]][[lang]][[t]] > proficiency_threshold & bank[[agent_id]]$role %in% c("mother", "father")){
        #   gen1[t, lang] <- gen1[t, lang] + 1
        # }
        # if(bank[[agent_id]][[lang]][[t]] > proficiency_threshold & bank[[agent_id]]$role == "0"){
        #   gen2[t, lang] <- gen2[t, lang] + 1
        # }
      }
    }
  }
  
  speaker_percents <- language_counts / length(unique(names(bank))) * 100
  speaker_percents <- cbind(time = seq(tmax), speaker_percents)
  # gen1_percents <- gen1[,2:length(languages)] / gen1_size * 100
  # gen1_percents <- cbind(time = seq(tmax), gen1_percents)
  # gen2_percents <- gen2[,2:length(languages)] / gen1_size * 100
  # gen2_percents <- cbind(time = seq(tmax), gen2_percents)
  # 
  # tables <- list(speaker_percents, gen1_percents, gen2_percents)
  # names(tables) <- c("speaker_percents", "gen1_percents", "gen2_percents")
  return(speaker_percents)
}




# Call the function to generate the language counts table

random_kid_tables <- generate_speaker_summaries(kids, languages, tmax = 20, proficiency_threshold = 0)
random_adult_tables <-   generate_speaker_summaries(adults, languages, tmax = 20, proficiency_threshold = 0)

weighted_kid_tables <- generate_speaker_summaries(kids_weighted, languages, tmax = 20, proficiency_threshold = 0)
weighted_adult_tables <-   generate_speaker_summaries(adults_weighted, languages, tmax = 20, proficiency_threshold = 0)






#### Reshape the language bank data for plotting
# Create a data frame to store the language bank information
generate_long_data <- function(language_bank, languages){
  df <- data.frame()
  for(i in seq(language_bank)){
    language_bio <- as_tibble(language_bank[[i]]) %>%
      mutate(agent_ID = names(language_bank)[i], .before = "A",
             role = language_bank[[i]]$role) %>%
      # add_column(age = 1:nrow(.), .before = "A") %>%
      pivot_longer(cols = languages, names_to = "languages", values_to = "proficiency")
    df <- rbind(df, language_bio)
  }
  df$proficiency <- unlist(df$proficiency)
  return(df)
}

### Random interactions baseline
random_kids_long <- generate_long_data(kids, languages)
random_adults_long <- generate_long_data(adults, languages)




weighted_kids_long <- generate_long_data(weighted_kids, languages)
weighted_adults_long <- generate_long_data(weighted_adults, languages)


mono_long <- generate_long_data(test_mono, languages = "A")


plot_bio_samples <- function(df, category){
  subset <- all_IDs %>%
    filter(role %in% category)
  subset <- subset[1:12,]$pid
  ggplot(df[which(df$agent_ID %in% subset),], aes(x = age, y = proficiency)) +
    geom_line(aes(color = languages)) +
    facet_wrap(~ agent_ID) +
    theme_bw()
}  


plot_bio_samples(random_kids_long, category = "agent")
plot_bio_samples(random_adults_long, category = "mother")

plot_bio_samples(weighted_kids_long, category = "agent")
plot_bio_samples(weighted_adults_long, category = "mother")

plot_bio_samples(mono_long, category = "agent")
#### Plot the output!


random_kids_long$bilingual_household <- random_kids_long$agent_ID %in% all_IDs[which(all_IDs$family_language == "bilingual"),]$pid
random_adults_long$bilingual_household <- random_adults_long$agent_ID %in% all_IDs[which(all_IDs$family_language == "bilingual"),]$pid



weighted_kids_long$bilingual_household <- weighted_kids_long$agent_ID %in% all_IDs[which(all_IDs$family_language == "bilingual"),]$pid
weighted_adults_long$bilingual_household <- weighted_adults_long$agent_ID %in% all_IDs[which(all_IDs$family_language == "bilingual"),]$pid




kids_bilingual_random <- ggplot(random_kids_long, aes(x = age, y = proficiency)) +
  geom_line(aes(color = bilingual_household, group = agent_ID)) +
  facet_wrap(~ languages) +
  theme_bw()

adults_bilingual_random <- ggplot(random_adults_long, aes(x = age, y = proficiency)) +
  geom_line(aes(color = bilingual_household, group = agent_ID)) +
  facet_wrap(~ languages) +
  theme_bw()


wkids_summary <- weighted_kids_long %>%
  group_by(bilingual_household, languages, age) %>%
  summarise(proficiency = mean(proficiency))

ggplot(wkids_summary, aes(x = age, y = proficiency)) +
  geom_line(aes(color = bilingual_household)) +
  facet_wrap(~ languages) +
  theme_bw()

ggplot(weighted_kids_long, aes(x = age, y = proficiency)) +
  geom_line(aes(color = bilingual_household, group = agent_ID), alpha = 0.6) +
  facet_wrap(~ languages) +
  theme_bw()


ggplot(weighted_adults_long, aes(x = age, y = proficiency)) +
  geom_line(aes(color = bilingual_household, group = agent_ID), alpha = 0.6) +
  facet_wrap(~ languages) +
  theme_bw()

##### Now, show the effect of exposure richness on multilingualism
interlocutor_richness <- as.data.frame(cbind(time = seq(40), test_random_interactions$conversant_counts)) %>%
  pivot_longer(cols = -1, names_to = "agent_ID", values_to = "n_interlocutors")

ggplot(interlocutor_richness, aes(x = time, y = n_interlocutors)) +
  geom_line(aes(group = agent_ID, color = agent_ID), alpha = 0.3) +
  theme(legend.position = "none")

#... well, because number of conversation partners is independent across time, there's not much to be teased out here at the level of the individual. 

### But you could change the baseline number of conversations that each agent has and see what that does. 

df <- weighted_long

subset <- all_IDs %>%
  filter(role != "agent")
subset <- subset[1:10,]$pid

p2 <- ggplot(df[which(df$agent_ID %in% subset),], aes(x = age, y = proficiency)) +
  geom_line(aes(color = languages)) +
  facet_wrap(~ agent_ID) +
  theme_bw() 

#plot_grid(p, p2, ncol = 1)

plots <- list(p, p2)
names(plots) <- c('children', 'parents')



subset <- all_IDs %>%
  filter(role == "agent")
ggplot(df[which(df$agent_ID %in% subset$pid),], aes(x = age, y = proficiency)) +
  geom_line(aes(group = agent_ID, color = agent_ID), alpha = 0.5) +
  facet_wrap(~ languages) +
  theme_bw() +
  theme(legend.position = "none")



