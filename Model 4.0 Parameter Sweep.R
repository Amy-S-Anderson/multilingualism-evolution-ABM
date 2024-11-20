


params <- expand.grid(household_interaction_prob = c(3/199, 0.25, 0.5, 0.75),
                         parent_language_choice = c("random consistent", "L1"),
                         others_language_choice = c("random", "prestige_A"),
                      stringsAsFactors = FALSE)


sweeps <- vector("list", nrow(params)) 
for(i in 1:nrow(params)){
  print(paste(params[,i]))
sweeps[[i]]$output <- run_ABM(generation_size = 100, 
                           generations_n = 2, 
                           household_interaction_prob = params$household_interaction_prob[i],
                           parent_language_choice = params$parent_language_choice[i],
                           others_language_choice = params$others_language_choice[i])

sweeps[[i]]$household_interaction_prob <- params$household_interaction_prob[i]
sweeps[[i]]$parent_language_choice <- params$parent_language_choice[i]
sweeps[[i]]$others_language_choice <- params$others_language_choice[i]
}







# ggplot(test[which(test$generation > 0),], aes(x = age, y = `Speaks B`)) +
#   geom_line(aes(group = agent_id), color = "red", alpha = 0.7) +
#   geom_line(data = test[which(test$generation > 0),], aes(x = age, y = `Speaks C`, group = agent_id), color = "blue", alpha = 0.7) +
#   geom_line(data = test[which(test$generation > 0),], aes(x = age, y = `Speaks D`, group = agent_id), color = "purple", alpha = 0.7) +
#   geom_line(data = test[which(test$generation > 0),], aes(x = age, y = `Speaks E`, group = agent_id), color = "green", alpha = 0.7) +
#   geom_line(data = test[which(test$generation > 0),], aes(x = age, y = `Speaks A`, group = agent_id), alpha = 0.7)
#   
  
