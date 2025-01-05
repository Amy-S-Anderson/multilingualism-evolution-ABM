library(jsonlite)


read_all_output = function(directory) {
  output = read.csv(file.path(directory, "output.csv"))
  return(output)
}




# creates a list object of params used in this specific model run. 
read_params_used = function(directory) {
  #output = fromJSON(read.csv(file.path(directory, "params_used.json")))
  output = fromJSON(file.path(directory, "params_used.json"))
  
  return(output)
}



extract_from_repeats = function(root_directory, extractor_func) {
  output_list = list()
  iRep = 1
  while (dir.exists(file.path(root_directory, paste0("rep=", iRep)))) {
    rep_directory = file.path(root_directory, paste0("rep=", iRep))
    output_list[[iRep]] = extractor_func(rep_directory)
  }
  
  return(output_list)
}



rep_outputs = extract_from_repeats(directory, read_params_used) # I don't understand why this function isn't working



directory = "./Model 5.0/model_output/all_random/prop_of_intra_household_interactions=0.5/rep=1"
root_directory = "./Model 5.0/model_output/all_random/prop_of_intra_household_interactions=0.5"
