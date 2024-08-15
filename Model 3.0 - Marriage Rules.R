

### Marriage Rules and Multilingualism ###


  # Generate first parent cohort, all age 25, all monolingual in one of five languages (A-E)
  agents <- start_cohort(n = 100, age = 25, n_languages = 5) %>%
  # Marry off the first cohort to each other, at random. 
  marry_random() %>%
  # birth new cohort (children of parent cohort)
  birth_new_cohort()
  
  
  # Ok, now things really begin.
  
  ### How do I write a function to assign parent-child-specific language choices?
  

  
  # Generate interactions for each agent
  interactions <- generate_interactions(agents, n_interactions = 10, own_household_prob = 0.5)
  
  # Now, tally up the full list of conversants/conversations from this matrix. 
  