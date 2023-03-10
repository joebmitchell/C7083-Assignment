dotplot_fn <- function(x){
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
co2 <- x %>% 
  group_by(food_category) %>% 
  group_by(food_category) %>% 
  mutate(co2_per_kg = co2_emmission/consumption) %>% 
  summarise(co2_per_kg = round(mean(co2_per_kg, na.rm = TRUE), 2)) %>% # Calculate co2 per kg eaten by country
  arrange(co2_per_kg) %>% 
  #Create factor levels dividing food groups into plant or animal origin
  mutate(food_category = as.factor(food_category),
         food_group = c(0,0,1,1,0,1,1,0,1,1,1),
         food_group = factor(food_group, labels = c('Plant Protein', 'Animal Protein')))
  
my_cols <- cbbPalette[c(2,6)] # set colours from palate to be used in graph

dotchart(co2$co2_per_kg, labels = co2$food_category, # create base R dotchart
         xlab = 'CO2 emissions per KG',
         
         color = my_cols[co2$food_group], # use colours from Palate 
         main =  'Carbon footprint per KG consumed',
         cex = 1.1,
         pch = 15,
         cex.main = 1.2)
legend(20, 5, legend=c("Plant Protein", "Animal Protein"),
       col=my_cols, pch = 15, cex=1.1)
  
}
    