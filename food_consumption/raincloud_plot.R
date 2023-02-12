raincloud_plot <- function(x){

  amounts <- x %>% 
    # Create ordered factors for cood category by median consumption
    mutate(food_category = fct_reorder(food_category, consumption, .fun='median')) 
  
  
  ggplot(amounts, aes(x = food_category, y = consumption))+
    # Add the raincloud part of plot
    ggdist::stat_halfeye( fill = '#CC79A7',
                          adjust = 0.5, 
                          width = .6, 
                          .width = 0, 
                          justification = -.2, 
                          point_colour = NA,
                          normalize = 'groups', 
    )+# add boxplot to show distribution
    geom_boxplot( width = 0.15,
                  outlier.colour = NA,
                  alpha = 0.5)+
    # add 'rain' 
    stat_dots(alpha = 1,
              fill = 'black',
              dotsize = 1,
              side = 'left')+
    # flip so that rain falls down
    coord_flip()+
    theme_minimal(base_size = 15)+
    labs(title = 'Consumption of food categories by country',
         subtitle = "<span style='color:#D55E00'>Maximum amount (KG)</span>",
         x = NULL,
         y = ' Consumption per capita (KG)') +
    theme(legend.position = 'none', # remove legend
          plot.title = element_text(hjust =.4),
          plot.subtitle = element_markdown(hjust =.4), # set as markdown to allow colour change
          axis.text.y = element_text(vjust = -1, size = 15),
          panel.grid.major.y = element_blank(), # remove grid lines
          panel.grid.minor.x = element_blank())+ 
    scale_y_continuous(breaks = seq(0, 450, by = 50))+ # set y axis breaks
    # add label and arrow for Highest value: Finland
    geom_curve(aes(x = 7.5, y = 355, xend = 9.8, yend = 421), 
               colour = "red", 
               size=1,
               curvature = 1,
               arrow = arrow(length = unit(0.03, "npc")))+
    geom_label(aes(x = 8, y = 400,
                   label = 'Finland consumes\n
                 the most dairy\n
                 products per capita'),
               hjust = 1,
               vjust = 1,
               lineheight = 0.8,
               label.size = NA,
               colour = '#0072B2') +
    # label all the highest figures by category
    geom_label_repel(data = amounts %>%
                       group_by(food_category) %>%
                       summarise(max = max(consumption)),
                     aes(x = food_category, y = max,
                         label = max),
                     colour = '#D55E00', label.size = NA,
                     min.segment.length = 0)
}
