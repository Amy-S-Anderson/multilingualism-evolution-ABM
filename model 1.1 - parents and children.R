

### Describe what this script does. 
### Assign some agents to have starting age 25. Monolingual parents to newborns.
### Every round, each agent has a minimum of 10 conversations. Conversational partners are sampled with replacement, so some agents may end up having more than ten conversations because they show up multiple times in other agents' list of conversants. 



### Load libraries
library(tidyverse)
library(cowplot)



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
establish_population <- function(agents, languages, family_weights){ 
  data <- vector(mode = "list", length = nrow(agents))
  names(data) <- agents$pid
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
  return(data)
}


# starting population of 200 new monolingual parents and their new singleton births, in a society with four languages spoken. 
pop <- establish_population(all_IDs, languages)







update_language_bank <- function(pop, families, all_IDs, family_weight = 1, tmax) {
  
  language_counts <- matrix(0, nrow = tmax, ncol = length(languages), 
                            dimnames = list(seq(tmax), languages))
  
  
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
      conversants <- sample(alters$pid, 10, replace = TRUE, prob = family_weighting)
      interactions[i,] <- c(agent_id, conversants)
    #  interactions[i,] <- c(names(pop)[i], conversants)
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
        interactions_from_ego_rows <- as.vector(filtered_mat[filtered_mat[, 1] == agent_id, -1])
        
        # Extract all agent IDs from the first column, excluding the agent's own ID (agent i's appearance as an alter in other agents' ego rows)
        interactions_from_alter_rows <- filtered_mat[,1 ]
        interactions_from_alter_rows <- interactions_from_alter_rows[interactions_from_alter_rows != agent_id]
        
        # Combine the two sets of interacted agents
        all_conversant_IDs <- c(interactions_from_ego_rows, interactions_from_alter_rows)
        spoken <- NA
        
        
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

    
    # The amount of proficiency gained is a function of the listener's age.
    params <- data.frame(d = 18, a = 0.5, r0 = 9, tc = 0) # parameter values chosen by simulating a monolingual cohort that achieves total proficiency in birth language by age 10. 
    
      
    ### Learning by Listening ###
      for(i in seq(pop)){
        agent_id <- names(pop)[i]
        age_factor <- params$r0 * (1 - (1 / (1 + exp(-params$a * (pop[[agent_id]]$age[t] - params$tc - params$d))))) + 0.5
        
        for(lang in seq(languages)){
          if(pop[[agent_id]][[lang]][t] < 100){
          pop[[agent_id]][[lang]][t + 1] <- modify_depth(pop[[agent_id]][[lang]][t], 1, ~ .x + (age_factor * relative_exposures[i, lang + 1]))
          if(pop[[agent_id]][[lang]][t + 1] > 100){
            pop[[agent_id]][[lang]][t + 1] <- 100
           }
          }
            else(pop[[agent_id]][[lang]][t + 1] <- 100)
          
          # Update language speaker counts for this time step
          if (pop[[agent_id]][[lang]][t] > 0) {
            language_counts[t, lang] <- language_counts[t, lang] + 1
          }
        }
        
        
        pop[[agent_id]]$age[t + 1] <- pop[[agent_id]]$age[t] + 1
      }
  }
  return(pop)
}



### Trial run of model function
test_random_interactions <- update_language_bank(pop, families, all_IDs, tmax = 40) # test run: 40 years of language learning
test_family_weighted <- update_language_bank(pop, families, all_IDs, tmax = 50, family_weight = 400)

### Every couple of runs, the line above gives me an error code: Error in filtered_mat[, 1] : incorrect number of dimensions. 
# Why?

test_mono <- update_language_bank(pop, families, all_IDs, tmax = 40, family_weights = 1)






  
  
# Define a function to summarize language bank at each time point: e.g., Number of speakers of language A at time t
  generate_speaker_summaries <- function(bank, languages, tmax, proficiency_threshold) {
    language_counts <- matrix(0, nrow = tmax, ncol = length(languages),
                              dimnames = list(seq(tmax), languages))
    language_counts <- cbind(time = seq(tmax), language_counts)
    gen1 <- language_counts
    gen2 <- language_counts
    gen1_size <- 0
    gen2_size <- 0
    #speaker_counts <- data.frame(matrix(NA, 1, length(languages) + 1))
    #colnames(speaker_counts) <- c("Time", languages)
    #data.frame(c(Time = seq(tmax),))
    
    for (t in seq(tmax)) {
      for (i in seq(bank)) {
        agent_id <- names(bank)[i]
        if(bank[[agent_id]]$role == "0"){
          gen2_size <- gen2_size + 1
        }   
          else{
            gen1_size <- gen1_size + 1}

        for (lang in languages) {
          if (bank[[agent_id]][[lang]][[t]] > proficiency_threshold) {
            language_counts[t, lang] <- language_counts[t, lang] + 1
          }
          if(bank[[agent_id]][[lang]][[t]] > proficiency_threshold & bank[[agent_id]]$role %in% c("mother", "father")){
            gen1[t, lang] <- gen1[t, lang] + 1
          }
          if(bank[[agent_id]][[lang]][[t]] > proficiency_threshold & bank[[agent_id]]$role == "0"){
            gen2[t, lang] <- gen2[t, lang] + 1
          }
        }
      }
    }
    
    speaker_percents <- language_counts[,2:length(languages)] / length(unique(names(bank))) * 100
    speaker_percents <- cbind(time = seq(tmax), language_percents)
    gen1_percents <- gen1[,2:length(languages)] / gen1_size * 100
    gen1_percents <- cbind(time = seq(tmax), gen1_percents)
    gen2_percents <- gen2[,2:length(languages)] / gen1_size * 100
    gen2_percents <- cbind(time = seq(tmax), gen2_percents)
    
    tables <- list(speaker_percents, gen1_percents, gen2_percents)
    names(tables) <- c("speaker_percents", "gen1_percents", "gen2_percents")
    return(tables)
  }

# Call the function to generate the language counts table
random_tables <- generate_speaker_summaries(test_random_interactions, languages, tmax = 20, proficiency_threshold = 20)
  
random_tables$language_percents
random_tables$language_counts

weighted_tables <- generate_speaker_summaries(test_family_weighted, languages, tmax = 20, proficiency_threshold = 20)
weighted_tables$speaker_percents
weighted_tables$language_counts


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


weighted_long <- generate_long_data(test_family_weighted, languages)
random_long <- generate_long_data(test_random_interactions, languages)
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


plot_bio_samples(weighted_long, category = "agent")
plot_bio_samples(weighted_long, category = "mother")
plot_bio_samples(mono_long, category = "agent")
#### Plot the output!

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



