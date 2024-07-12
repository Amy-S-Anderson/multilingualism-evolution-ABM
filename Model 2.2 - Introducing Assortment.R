












#### SET UP MODEL SPACE #### 

####  - Designate languages in play. ####
languages <- choose_local_languages(3)

#### - Designate the mortality hazard for the population ####
# choose the Siler model parameter values for the mortality regime your agents will experience.
CDW15 <- data.frame(a1 = 0.175, b1 = 1.4, a2 = 0.00368, a3 = 0.000075, b3 = 0.0917)

####  - Generate a starting set of agents, and their age structure ####
initial_ages <- generate_age_structure(n = 10000, mortality = CDW15, years = 300) # this line will take a few minutes to run.
# Assign two populations of identical size and age structure, each with their own place_ID that will determine non-random assortment.

pop1 <- make_basic_population(n_agents = 1000, age_distribution = initial_ages) %>%
  mutate(place_id = 1)
# Pop1 has an even number of monolingual speakers of A and B. Language proficiency is determined by agent age.
pop1 <- assign_starting_proficiency(pop1, languages[c(1,2)])

# Pop2 has an even number of monolingual speakers of B and C
pop2 <- make_basic_population(n_agents = 1000, age_distribution = initial_ages, id_start = 1000) %>%
  mutate(place_id = 2)
pop2 <- assign_starting_proficiency(pop2, languages[c(2,3)])
pop2$agent_id <- pop2$agent_i

agent_census <- bind_rows(pop1,pop2)


### Assign min proficiency threshold for being able to speak a language
min_speaking_proficiency <- 20

### Initialize output table
output <- as.data.frame(matrix(0, nrow = 0, ncol = ncol(agent_census)))
names(output) <- names(agent_census)

#### Set years of model run time. ####
tmax = 100


start.time <- Sys.time()
for(i in seq(tmax)){
  print(i) # Loop Counter will appear in the console to let you know how the model run is progressing. 
  
  #### DEMOGRAPHY ####
  # Calculate number of deaths there will be this year based on age structure of population.
  agent_census <- agent_census[which(is.na(agent_census$death_recorded)),]
  deaths <- reap(agent_census, mortality_regime = CDW15)
  turnover <- deaths$pop_turnover
  # Record this year's deaths
  agent_census <- deaths$agent_census
  
  # - People who lived through this round turn 1 year older
  agent_census$age <- case_when(is.na(agent_census$death_recorded) ~ agent_census$age + 1,
                                TRUE ~ agent_census$age)
  #  Pair up males/females for reproductive partnerships:
  agent_census <- select_marriage_partners(agent_census[which(is.na(agent_census$death_recorded)),], 
                                           calculate_dyad_score = calc_dyad_age_and_place)
  
  # - Generate new births in existing partnerships. Assign traits to newborn agents. 
  new_parents <- sow_stationary(n_births = turnover, agent_census) # this function only applies to living women. 
  agent_census <- birth_new_agents(agent_census, new_parents)


