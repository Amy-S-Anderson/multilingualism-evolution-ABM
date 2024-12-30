library(jsonlite)


read_all_output = function(directory) {
  output = read.csv(file.dir(directory, "output.csv"))
  return(output)
}

read_params_used = function(directory) {
  output = fromJSON(read.csv(file.dir(directory, "params_used.json")))
  return(output)
}



extract_from_repeats = function(root_directory, extractor_func) {
  output_list = list()
  iRep = 1
  while (dir.exists(file.path(root_directory, paste0("rep=", iRep)))) {
    rep_directory = file.path(root_directory, paste0("rep=", iRep))
    output_list[[irep]] = extractor_func(rep_directory)
  }
  
  return(output_list)
}



rep_outputs = extract_from_repeats("model_output/test", read_params_used)



