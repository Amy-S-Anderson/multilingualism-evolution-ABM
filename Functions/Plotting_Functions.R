



############ PLOT MODEL OUTPUT ##############


#### Check population size over time
pop_size <- function(output){
  pop_size_table <- output %>%
  group_by(year) %>%
  summarise(pop_count = n())
pop_size_table

ggplot(pop_size_table, aes(x = year, y = pop_count)) +
  geom_line()
}





