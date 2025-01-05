# multilingualism-evolution-ABM
An agent-based model of the evolutionary dynamics of multilingualism. This model was developed as part of the MULTILING-HIST Project (ERC Grant #.....). 

In this model, agents learn languages through dyadic conversations with other agents. 

The model focuses on the impact of  
- the initial prevalence of each language in the population
- the ratio of conversations inside/outside an agent's own household
- the logical rules that determine which language an agent chooses to speak in a conversation with another agent.
  **Four options for the rule to pick which language to speak:**
  1.	Pick at random.
  2.	Speak your natal language, if you can. Otherwise, speak the language you know best. 
  3.	Speak the language you know best.
  4.	Speak the prestige language, if you can. Otherwise, speak the language you know best. 




# Simplifying Assumptions of the model #
- Languages are discrete, unchanging entities. They are currently modeled as categorical variables, rather than as continuous variables with varying degrees of similarity/difference to each other.
- The population is demographically stationary, with a parent cohort that reproduces at replacement rate (one child per parent). All parents reproduce at age 25, generating a child cohort that will become the next parent cohort after 25 years in the simulation.
- Each household consists of only two agents: one parent, and one child. 
- Only two generations are ever alive at the same time. 
- Agents do not have to coordinate on their choice of language within a conversation. As a result, many dyadic conversations are conducted bilingually.
  An agent's choice of language in a given conversation depends on 
      - the agent's relationship to their conversation partner (are they a member of the agent's own household, or not?), and on
      - the agent's own language skills at the time of the conversation.
-   Agents' language choices do not depend on the language skills of their conversation partner. As a result, there are no costs or benefits to 'successful' or 'unsuccessful' communication.
-   Once learned, a language is not forgotten. Language skills do not decay, even if the language is not heard or spoken for many years. 
-   The likelihood that an agent interacts with a member of their own household, or an agent who is outside their own household, remains constant across the life course.
-   An agent's rate of language learning remains constant across the life course. An agent's gain in their skills of a specific language depends only on the extent of their exposure to that language in the current year of model time.

The model operates in discrete, one-year time intervals. The output is a data frame that catalogs each agent's language skills at each age of their life from birth to death. It also records each agent's household ID, which remains stable across an agent's lifetime. Each agent inherits their household ID from their parent. 

**Note: Each of these simplifying assumptions points to a variable that could become the focus of its own set of experiments with a few adjustments to the existing model** 



# Arguments to specify
The `run_ABM()` function requires the following arguments be specified in order to run a simulation: 
- `generations_n`: an integer. The number of generations to run the model.
- `languages_n`: an integer between 1 and 9. The total number of imaginary languages spoken by the agents in the first parent generation (individual agents in this generation are monolingual).
- `speaker_freqs`: A vector of length = `languages_n`. This vector must sum to 1. It specifies the relative frequencies of monolingual speakers belonging to each language in the first parent generation.
- `prop_of_intra_household_interactions`: a number between 0 and 1. The proportion of an agent's annual conversations, on average, that are with the member(s) of the agent's own household, rather than agents whose household ID differs from their own.
- `parent_language_choice`: a character string. Either "random", "L1" (natal/heritage language), "best_known", or "prestige_A". Determines the rule followed by an agent when speaking to their own child.
- `child_language_choice`: See above. Determines the rule followed by an agent when speaking to their own parent.
- `others_language_choice`: See above. Determines the rule followed by an agent when speaking to an agent whose household ID differs from their own (i.e., someone who is neither their parent nor their child)




# Model Process Overview and Scheduling 
An initial parent generation is generated, appearing at age 25 and immediately giving birth to the first cohort of agents who will grow up inside the environment of the model. This first parent generation is monolingual, with agents assigned a language specified by the number and relative frequency of languages (`languages_n` and `speaker_freqs`) in the model call. 

In each year of model time, agents learn languages by interacting with each other, performing the following actions, in the following order:
  #### 1. Agents are matched with other agents for dyadic conversations. ####
  a.	For each agent, the pool of all other agents is sampled with replacement 10 times.  
        Each agent thus has a minimum of 10 conversations (mean) in each year of model time, but is much more likely to have, say 20 conversations in a simulation with 200 agents alive at a time.  
  b.	Conversant sampling can be weighted according to whether two agents share the same household ID.  
      For example, if household assortment has a probability weighting of 0.5, then on average, 50% of an agent’s interactions will be with members of their own household. Every household is a nuclear family comprised           of two parents and two children.
     
 #### 2.	Agents choose which language to speak in each of their conversations. ####
  Both agents in a dyadic conversation choose which language they will speak independently of their conversation partner’s choice. As a result, many conversations may be conducted bilingually.  
  a.	If the agent is speaking to their own child, then they may choose one of several rules, depending on the current model scenario:  
  - The parent agent speaks in L1, the language in which they first attained a speaking value > 0 during their own childhood.
  - The parent agent chooses from among their known languages at random and remains consistent with this choice in future interactions with this child.  
      
   b.	If the agent is speaking to any agent other than their own child, then they may choose one of several rules, again depending on the current model scenario:  
  - Choose L1, the language that you learned first (earliest speaking value > 0).
  - Choose the language that is designated as the ‘prestige’ language variant.  
          
####  3.	Agents update their cumulative language knowledge as a function of their exposure to each language in this year, and their current age. ####
   a.	Understanding ability increases as a result of listening: An agent’s level of understanding in a language increases by hearing other agents speak this language.  
   
   b.	Speaking ability increases as a result of speaking: An agent’s level of speaking efficacy in a language increases only by speaking this language themselves.  
  - An agent can only choose to speak a language once their level of understanding reaches the skill level equivalent to roughly the value of a two-year-old in a monolingual context.
    
   c.	When an agent with speaking abilities interacts with an infant who has not yet developed speaking ability in any language, the speaking agent will not increase their level of language understanding from this                interaction, but they will increase their own level of speaking efficacy in the language that they choose to speak to the infant, assuming their efficacy in this language has not already reached the ceiling of              possible values (100).  
   

Only two generations are alive at any given time; every 25 years, the parent generation (aged 49) dies, and the child generation (now aged 25) reproduces, becoming the new parent generation.



# How to Run the Model
All the files needed to run the Model are inside the `Model 5.0` folder. Other files are historical. 

### Step 1  ###
In your own workspace, from inside the `Model 5.0` folder, Open and run `Make_File.R`  

  This will load  
    1. All the necessary libraries  
    2. All the bespoke functions in the `Functions.R` file that are called inside the model.  
    3. The model function itself, which is in the file `Model 5.0 - How to Survive a Killer Language.R`  
    4. The `workflow.R` file, which contains functions to facilitate running and saving model sweeps in a systematically organized way with a consistent file and naming structure.  
    
### Step 2 ###  
Open `Model 5.0 Sweeps.R` and either run it, or use it as a template from which to set up the model scenarios that you are interested in running.
  **Note: If you run this file as is, it will take at least a full day on a laptop. I recommend running it on a desktop, at least.**  

### Step 3 ### 
Open the files with `Model Plotting` in their names. These contain functions and code designed to read in the simulated data from a model sweep, and functions to produce multipanel plots of the output from a sweep.   


# Beyond the current architecture: Easy adjustments to the existing model  
These adjustments will need to be made inside the code for the model, since they aren't currently listed as arguments to be specified in the model call.  

1. If you want to change the length of a generation
    - change the number assigned to `generation_time`, which specifies the number of years of interaction and language learning to run before the next demographic update (births and deaths) to the population.
      
2. If you want to change the age at which agents become parents
   - change the numeric value assigned to `parent_age` in the `birth_new_cohort()` function. This is used in the initial setup of the model and at the end of each generational `for` loop.
   - Careful, you probably want to make sure that the age at which agents are immaculately conceived (the first parent generation) using the `start_cohort()` function is old enough for them to immediately become parents.

3. If you want to have three-generation households and allow three generations to be alive at a time
   - You could change the timing of agent death so that it depends on agent age, not on the number of years passed. Logic along the lines of 'while max(agent_age) < 75, run the learning loop.' Inside this loop, agents who are age == parent_age become parents. It's a slightly more involved reshaping of the model. Let me know if you'd like it.  
  
4. If you want languages to have some degree of mutual intelligibility
   - Currently, languages are assigned into the simulation inside the `start_cohort()` function. You'd likely want to pull the languages outside of that and generate a set of languages with specified relationships to each other before assigning languages to your starting cohort.
  
5. To implement forgetting
   - In the section of the model code with the chapter title 'agents learn languages from their interactions,' implement an if statement that updates language skills using a decay function IF the language was not spoken/heard that year. Agents should lose their speaking skills faster than their understanding skills.

6. To change the rate of language learning
   - Adjust the terms of the functions `learn_languages_by_speaking()` and `learn_languages_by_listening()` in the file `./Functions/All Functions - How To Survive a Killer Language.R`

  



