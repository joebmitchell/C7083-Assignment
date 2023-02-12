scatter_dat <- function(x) {
  # function to process data for the scatter graph
  x %>% 
  group_by(country) %>% 
  mutate(food_category = as.factor(food_category),
         food_group = c(1,1,1,1,1,1,1,0,0,0,0),
         food_group = factor(food_group, labels = c('Plant', 'Animal'))) %>% 
  rename("NAME_LONG" = "country") %>% # change name of country so it matches other data frames
  group_by(NAME_LONG, food_group) %>% 
    # calculate total co2 and consumption by group
  summarise(total_co2_gp = sum(co2_emmission),
            total_con_gp = sum(consumption),) %>%
    # pivot table to allow total c02 and consumption in same row
  pivot_wider(names_from = food_group, values_from = c(total_co2_gp, total_con_gp)) %>% 
    # calculate total co2 by adding two groups together
  mutate(total_co2 = total_co2_gp_Animal + total_co2_gp_Plant)
}
