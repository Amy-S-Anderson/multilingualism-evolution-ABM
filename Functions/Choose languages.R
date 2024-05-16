


#### Function to choose languages in the simulation space ####


# This function needs to:
# 1. Generate capital letters that stand in for language names of abstract languages. 
# 2. Generate a user-specified number of language names, returning a vector of character values ("A", "B"...)


# Future versions of this function may need to include information about
##. - the relationship of the languages to each other
##. - How complex/difficult a language is to learn
##. - Any number of other traits that can be assigned to each language and may affect its chances of agent-agent transmission.

########################################################################################




#### Function: Generate Language Names

# number_of_languages = an integer between 1 and 9. 
choose_languages <- function(number_of_languages){
  languages <- chartr("123456789", "ABCDEFGHI", seq(number_of_languages)) 
  return(languages)
}




########################################################################################



#### Example use ####

# Generate names for 3 abstract languages
languages <- choose_languages(3)

