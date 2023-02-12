future_co2 <- function(x) {
  
  co2_dat <- x %>% 
    group_by(country) %>% 
    # Create factor levels dividing food groups into plant or animal origin
    mutate(food_category = as.factor(food_category),
           food_group = c(1,1,1,1,1,1,1,0,0,0,0),
           food_group = factor(food_group, labels = c('Plant', 'Animal'))) %>% 
    ungroup() %>% 
    group_by(food_group) %>%
    # Create variables to calculate co2 emissions and consumption by food group
    mutate(sum_co2 = sum(co2_emmission),
           sum_kg = sum(consumption),
           co2_per_kg = (sum_co2/sum_kg)) %>% 
    # summarise df to include only results for each food group
    summarise(co2_per_kg = min(co2_per_kg),
              sum_co2 = min(sum_co2),
              sum_kg = min(sum_kg)) %>% 
    # calculate amount of co2 if kg of animal protein eaten had average emissions of plant consumed
    mutate(animal_as_plant =(sum_kg * lag(co2_per_kg)))
  
  # set x and y coordinates for graph
  x <- c(1,1,2)
  y<- c(1,3,2)
  # extract relevant information for graph from above calculations
  #co2 emissions from current consumption levels and c02 total if everything plant based
  z<- as.list(c(co2_dat[[3]],co2_dat[2,5]))
  
  # merge location and size data into 1 dataframe
  df <- data.frame(x,y) %>% 
    mutate(value = round(as.numeric(z)))
  
  # Create plot object
  
  ggplot(df, aes(x =x, y = y))+
    geom_point(aes(size = value), colour = c('#D55E00', '#0072B2', '#D55E00'))+ # set colours using palatte
    scale_size_area(max_size = 120) + # increase size of points
    theme_void(base_size = 20)+ # remove all graoh junk and increase text size
    theme(legend.position = "none")+ # remove legend
    xlim(0,2.5)+ # set plot limits to centre dots
    ylim(0,5)+
    geom_text(aes(label = paste0( scales::comma(value)," KG\n of CO2")), # add formatted labels to dots
              colour = 'white')+
    # add labels and arrows of current situation
    geom_text(aes(x = 0.2, y = 0.8), 
              label = 'The current situation',
              colour = '#E69F00',
              size = 6)+
    geom_curve(aes(x = 0.2, y = 0.9, xend = 0.85, yend = 1.1), 
               colour = "#E69F00", 
               size=1, 
               curvature = -0.2,
               arrow = arrow(length = unit(0.03, "npc")))+
    geom_curve(aes(x = 0.2, y = 0.9, xend = 0.5, yend = 3), 
               colour = "#E69F00", 
               size=1, 
               curvature = -0.6,
               arrow = arrow(length = unit(0.03, "npc")))+
    # Add labels and arrows of future situation
    geom_text(aes(x = 2, y = 4), 
              label = 'If everyone ate\n only plant protein',
              colour = '#009E73',
              size = 6)+
    geom_curve(aes(x = 2, y = 3.7, xend = 2, yend = 2.7), 
               colour = "#009E73", 
               size=1, 
               curvature = -0.2,
               arrow = arrow(length = unit(0.03, "npc")))+
    # add titles colour coded to show consumption type
    labs(title = "C02 Emissions of the world: Current and Potential <br>
       <span style='color:#D55E00'> Plant </span>and <span style='color:#0072B2'>Animal  </span>CO2 emissions",
         x = NULL,
         y = ' Consumption per capita (Kg)') +
    theme(plot.title = element_markdown(hjust = 0.5),
    )
}