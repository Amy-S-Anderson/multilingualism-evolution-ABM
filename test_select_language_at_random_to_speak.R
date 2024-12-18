
#### Write mini tests of all language choice functions. 


select_language_at_random_to_speak2 = function(agents_to_speak, pop) {
  #get column names for degree of language understood
  understands <- names(pop)[which(startsWith(names(pop), "Understands"))]
  
  #extract degree of language understanding for each agent interacting with focal agent
  language_data <- pop[match(agents_to_speak, pop$agent_id), c("agent_id", understands)]
  
  #calculate sampling weights for each agent choosing the language (this will be 0 or 1)
  language_weights = matrix(0, nrow=nrow(language_data), ncol=length(understands))
  language_weights[language_data[,understands]>15] = 1
  
  has_speech = rowSums(language_weights) != 0 #vector of true/false, corresponding to agents_to_speak (and to rows in language_weights)
  
  #Sample spoken language, or use "none" if speechless
  spoken = rep("none", length(agents_to_speak)) #Replace "none" with whatever you want speechless agents to use
  if (any(has_speech)) { #only sample languages if at least one interacting agent has speech
    spoken[has_speech] = apply(language_weights[has_speech,,drop=FALSE], 1, function(probs) {paste0("Speaks ", 
                                                                                                   sample(str_sub(understands, -1, -1), 1, 
                                                                                                          prob=probs))})
  }
  return(spoken)
}




agents_to_speak = c(2, 5, 2, 6)
#agents_to_speak = c(5, 6)
pop = data.frame(agent_id=seq(1, 6),
                  "Understands A"=c(0, 0, 0, 0, 0, 5),
                  "Understands B"=c(30, 30, 30, 0, 0, 0),
                  "Understands C"=c(0, 0, 100, 0, 0, 0)
                  )
names(pop) = c("agent_id", "Understands A", "Understands B", "Understands C")

spoken = select_language_at_random_to_speak2(agents_to_speak, pop)
print(spoken)


