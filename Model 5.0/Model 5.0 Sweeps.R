



#### Each set of runs below sweeps over a range of values (0.25, 0.5, 0.75) for the average proportion of an agent's conversations that are held with members of their own household, rather than members of other households. 

get_default_params = function() {
  params = list()
  params["generation_size"] = 100
  params["generations_n"] = 11
  params["languages_n"] = 3
  params["speaker_freqs"] = c(1/3, 1/3, 1/3)
  params["prop_of_intra_household_interactions"] = 0.5
  params["parent_language_choice"] = "random"
  params["child_language_choice"] = "random"
  params["others_language_choice"] = "random"
  
  return(params)
}


#### Parameter Sweeps ####

params = get_default_params()

# All Agents choose language of conversation at random
run_model_sweep(base_params = params, 
                root_output_directory = "./Model 5.0/model_output/all_random", 
                target_param = "prop_of_intra_household_interactions",
                target_param_values = c(0.25, 0.5, 0.75), numreps = 5)




# Parents always speak L1 to their children (others choose at random)
params["parent_language_choice"] = "L1" ####

run_model_sweep(base_params = params, 
                root_output_directory = "./Model 5.0/model_output/parentL1_random", 
                target_param = "prop_of_intra_household_interactions",
                target_param_values = c(0.25, 0.5, 0.75), numreps = 5)




# Parents and children speak L1 to each other (otherwise choose at random)
params["child_language_choice"] = "L1" ####

run_model_sweep(base_params = params, 
                root_output_directory = "./Model 5.0/model_output/parentL1_childL1", 
                target_param = "prop_of_intra_household_interactions",
                target_param_values = c(0.25, 0.5, 0.75), numreps = 5)




# Parents speak L1 to their children; children speak to parents in child's best known language; speak to non-household agents in randomly chosen known language
params["child_language_choice"] = "best_known" ####

run_model_sweep(base_params = params, 
                root_output_directory = "./Model 5.0/model_output/parentL1_childbest", 
                target_param = "prop_of_intra_household_interactions",
                target_param_values = c(0.25, 0.5, 0.75), numreps = 5)




# Parents speak L1 to their children; all other agents speak in their best known language at the time of conversation. 
params["others_language_choice"] = "best_known" ####
  
run_model_sweep(base_params = params, 
                root_output_directory = "./Model 5.0/model_output/L1_best_best", 
                target_param = "prop_of_intra_household_interactions",
                target_param_values = c(0.25, 0.5, 0.75), numreps = 5)




# Parents speak L1 to their children; children speak to parents in the child's best known language; languages outside the household are conducted in the prestige language (language A) if possible; if an agent cannot speak the prestige variant, they speak the language that they know best. 
params["others_language_choice"] = "prestige_A" ####

run_model_sweep(base_params = params, 
                root_output_directory = "./Model 5.0/model_output/L1_best_prestige", 
                target_param = "prop_of_intra_household_interactions",
                target_param_values = c(0.25, 0.5, 0.75), numreps = 5)





