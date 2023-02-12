dotplot_fn <- function(x){
  
co2 <- x %>% 
  group_by(food_category) %>% 
  mutate(co2_per_kg = co2_emmission/consumption) %>% 
  summarise(co2_per_kg = round(mean(co2_per_kg, na.rm = TRUE), 2),# Calculate co2 per kg eaten by country
            total_eaten = sum(consumption)) %>% # sum total kg eaten
  mutate(total_co2 = co2_per_kg * total_eaten) %>% # calculate total co2 by country
  arrange(total_co2) %>% 
  #Create factor levels dividing food groups into plant or animal origin
  mutate(food_category = as.factor(food_category),
        food_group = c(0,0,1,0,1,1,0,1,1,1,1),
        food_group = factor(food_group, labels = c('Plant Protein', 'Animal Protein')))
  
  my_cols <- c("#D55E00", "#0072B2") # set colours from palate to be used in graph
  
  dotchart(co2$total_co2, labels = co2$food_category, # create base R dotchart
           xlab = 'Total Carbon footprint (Kg/CO2/person)',
           groups = co2$food_group,
           color = my_cols[co2$food_group], # use colours from Palate 
           main =  'Total Carbon footprint by food type',
           cex = 1.1,
           pch = 15,
           cex.main = 1.2)
  
}
    