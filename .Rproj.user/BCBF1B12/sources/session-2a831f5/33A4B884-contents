## Packages ####

if(!require("janitor")) install.packages("janitor")
if(!require("tidyverse")) install.packages("tidyverse")
library(tmap)
library(ggdist)
library(ggtext)


## Import Data ####
food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

glimpse(food_consumption)
summary(food_consumption)

dupes<- get_dupes(food_consumption, country, food_category) # check for duplicates

sum(is.na(food_consumption)) # =0 No NA values

plot(food_consumption$consumption, food_consumption$co2_emmission)

# food_consumption %>% 
#   filter(food_category == 'Beef') %>% 
#   arrange(consumption) %>% 
# ggplot() +
#   geom_bar(aes(country,consumption), stat = 'identity')+
#   scale_x_discrete(limits = food_consumption$consumption)
  
?facet_grid()
## data_dictionary ####

# variable	class	description
# country	character	Country Name
# food_category	character	Food Category
# consumption	double	Consumption (kg/person/year)
# co2_emission	double	Co2 Emission (Kg CO2/person/year)

dat <- food_consumption

co2 <- dat %>% 
  group_by(food_category) %>% 
  mutate(co2_per_kg = co2_emmission/consumption) %>% 
  summarise(co2_per_kg = round(mean(co2_per_kg, na.rm = TRUE), 2),
            total_eaten = sum(consumption)) %>% 
  mutate( total_co2 = co2_per_kg * total_eaten) %>% 
  arrange(total_co2) %>% 
  mutate(food_category = as.factor(food_category),
         food_group = c(0,0,1,0,1,1,0,1,1,1,1),
         food_group = factor(food_group, labels = c('Plant Protein', 'Animal Protein')))
          


           

plot(co2$total, co2$c02_per_kg, 
     xlab = ' Worldwide Consumption (Kg)',
     ylab = ' CO2 per Kg consumed',
     col = 'goldenrod',
     pch = 16,
     main = 'CO2 emissions per Kg consumed vs Worldwide consumption in Kg')
abline(lm(co2$c02_per_kg ~co2$total))


my_cols <- c("darkgreen", "blue")
dotchart(co2$total_co2, labels = co2$food_category, 
         xlab = 'Total Carbon footprint (Kg/co2/person)',
         groups = co2$food_group,
         color = my_cols[co2$food_group],
         main =  'Total Carbon footprint by food type',
         cex = 1.1,
         pch = 15)

barplot(co2$total_co2, names.arg= co2$food_category, 
        xlab = 'Total Carbon footprint by food type (Kg/co2/person)',
        horiz = TRUE)

## Mapping total c02 by country ####

library(rnaturalearth)
library(sf)
#selecting data 
co2 <- dat %>% 
  group_by(country) %>% 
  mutate(co2_per_kg = co2_emmission/consumption) %>% 
  summarise(co2_per_kg = round(mean(co2_per_kg, na.rm = TRUE), 2),
            total_eaten = sum(consumption)) %>% 
  mutate(total_co2 = co2_per_kg * total_eaten) %>% 
  select(country, total_co2)
  


world_rnatural = rnaturalearth::ne_download(returnclass = "sf")

# Select key variables and assign them to a new object
world_iso = world_rnatural %>% 
  select(NAME_LONG, ISO_A3_EH, POP_EST, GDP_MD, CONTINENT)

# Transform the map
world_projected = world_iso %>% 
  st_transform("+proj=moll")

# Plot transformed map
plot(st_geometry(world_projected))

# find unmatched records


# merge datasets
combined_df = left_join(world_projected,
                        co2,  # Dataset 2
                        by = c("NAME_LONG" = "country"))  # Variables
  
  
plot(combined_df['total_co2'])

unmatch <- anti_join(co2,
                     combined_df,
)
unmatch

# 16 unmatched countries - need to correct names to join
# barbados not in world_projected
# Bermuda  not present
# 3 Congo    'Democratic Republic of the Congo'
# 4 French Polynesia         not present.
# 5 Gambia                   1403.
# 6 Grenada                 not present.
# 7 Hong Kong SAR. China     not present
# 8 Macedonia                2282.
# 9 Maldives                 not present.
# 10 Malta                    not present.
# 11 Mauritius                not present.
# 12 Russia                   2902.
# 13 South Korea              2031.
# 14 Swaziland                1067.
# 15 Taiwan. ROC              1942.
# 16 USA    

correct_names <- co2 %>% 
  mutate(country = replace(country, country == 'Congo', 'Democratic Republic of the Congo'),
         country = replace(country, country == 'Gambia', 'The Gambia'),
         country = replace(country, country == 'Macedonia', 'North Macedonia'),
         country = replace(country, country == 'Russia', 'Russian Federation'),
         country = replace(country, country == 'South Korea', 'Republic of Korea'),
         country = replace(country, country == 'Swaziland', 'Kingdom of eSwatini'),
         country = replace(country, country == 'Swaziland', 'Kingdom of eSwatini'),
         country = replace(country, country == 'Taiwan. ROC', 'Taiwan'),
         country = replace(country, country == 'USA', 'United States'),)
         
combined_df = left_join(world_projected,
                        correct_names,  # Dataset 2
                        by = c("NAME_LONG" = "country"))  # Variables


plot(combined_df['total_co2'])

unmatch <- anti_join(correct_names,
                     combined_df)
unmatch

big_emitters <- combined_df %>% 
  filter(total_co2 > 4000 |  total_co2 < 400)

ggplot()+
  geom_sf(data = combined_df,aes(fill = total_co2))+
  #geom_sf_label(data = big_emitters,aes(label = NAME_LONG)) +
  scale_fill_viridis_b(name = 'CO2 Emissions\n per person') +
  theme_void()+
  theme(legend.position = 'bottom',
        legend.key.width = unit(2, 'cm'))

ggplot() +
  geom_sf(data = combined_df, aes(fill = log(GDP_MD/POP_EST)))+
  theme_void()+
  scale_fill_viridis_b()+
  theme(legend.position = 'bottom',
        legend.key.width = unit(2, 'cm'))

?geom_sf_text


# The tmap package enables publication-quality maps to be created with concise and relatively commands:
# may have to run code twice!
# Select map type and what to plot


combined_df <- combined_df %>% 
  mutate(wealth = ntile((log(GDP_MD/POP_EST)), 100))

tmap_mode("view")

c <- tm_shape(combined_df) +
  tm_polygons(c("total_co2"), 
              palette = 'viridis',
              title = c("CO2 Emissions\n per person (Kg)")) +
  tm_layout(main.title = " Mapping of per capita CO2 Emissions and Wealth",
            frame = FALSE,
            )
            
  
w <- tm_shape(combined_df) +
  tm_polygons(c("wealth"), 
              palette = 'viridis',
              title = c( 'Percentile of GDP\nper capita'),
              n =5) +
  tm_layout(,
            frame = FALSE)
  
sf::sf_use_s2(FALSE)
tmap_arrange(c, w, nrow = 2, sync = TRUE
             )

## RainCloud Plot ####

# AMount eaten by category
# All Countries
library(ggdist)

amounts <- food_consumption %>% 
  mutate(food_category = fct_reorder(food_category, consumption, .fun='median')) #%>% 
  filter(food_category == c('Milk - inc. cheese'))

ggplot(amounts, aes(x = food_category, y = consumption))+
  ggdist::stat_halfeye( fill = 'black',
    adjust = 0.5, 
    width = .6, 
    .width = 0, 
    justification = -.2, 
    point_colour = NA,
    normalize = 'groups'
  )+
  geom_boxplot( width = 0.15,
                outlier.colour = NA,
                alpha = 0.5)+
  stat_dots(alpha = .3,
            fill = 'black',
            dotsize = 0.5,
            side = 'left')+
  coord_flip()+
  theme_minimal()+
  labs(title = 'Consumption of food categories by country',
       subtitle = 'Maximum amount (Kg) in Blue',
       x = NULL,
       y = ' Consumption per capita (Kg)') +
  theme(legend.position = 'none',
        plot.title = element_text(hjust =.4),
        plot.subtitle = element_text(hjust =.4),
        axis.text.y = element_text(vjust = -1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank())+ 
  scale_y_continuous(breaks = seq(0, 450, by = 50))+
  geom_curve(aes(x = 8, y = 400, xend = 10.7, yend = 431), 
                    colour = "red", 
                    size=1,
             curvature = .2,
             arrow = arrow(length = unit(0.03, "npc")))+
  geom_label(aes(x = 8, y = 400,
                 label = 'Finland consumes\n
                 the most milk\n
                 per capita'),
             hjust = 1,
             vjust = 1,
             lineheight = 0.8,
             label.size = NA,
             colour = 'red') +
  geom_label(data = amounts %>% 
               group_by(food_category) %>% 
               summarise(max = max(consumption)), 
                 aes(x = food_category, y = max, 
                     label = max,
                     fill = NA),
             nudge_y = 2,
             colour = 'goldenrod', label.size = NA)
       

ggplot(amounts, aes(x = food_category, y = consumption))+
  geom_boxplot()+
  coord_flip()

## by country co2 ####

by_country <- dat %>% 
  group_by(country) %>% 
  filter(country == 'Argentina') %>% 
  arrange(-co2_emmission)  %>% 
  mutate(food_category = as.factor(food_category),
         food_group = c(0,0,1,0,1,1,0,1,1,1,1),
         food_group = factor(food_group, labels = c('Plant Protein', 'Animal Protein'))) %>% 
  pivot_longer(cols = c('consumption', 'co2_emmission'),
               names_to = ('type'),
               values_to = 'kg') 
  
ggplot(by_country, aes(x = food_category, y = kg, fill = type))+
  geom_bar(stat = 'identity',
           position = 'dodge') +
  theme_minimal()+
  labs(title = 'C02 Emmissions and Consumption of {input$country}',
       subtitle = 'Maximum amount (Kg) in Blue',
       x = NULL,
       y = ' Consumption per capita (Kg)') +
  theme(legend.position = 'inset',
        plot.title = element_text(hjust =.4),
        plot.subtitle = element_text(hjust =.4),
        axis.text.y = element_text(vjust = -1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank())+
  scale_fill_discrete()

by_country <- dat %>% 
  group_by(country) %>% 
  rename(CO2 = co2_emmission,
         Consumption = consumption) %>% 
  mutate(food_category = fct_reorder(food_category, CO2)) %>% 
  pivot_longer(cols = c('Consumption', 'CO2'),
               names_to = ('type'),
               values_to = 'kg') 

## CO2 by KG food ####

co2_kg <- dat %>% 
  group_by(food_category) %>% 
  mutate(co2_per_kg = co2_emmission/consumption) %>% 
  summarise(co2_per_kg = round(mean(co2_per_kg, na.rm = TRUE), 2),
            eaten = sum(consumption)) %>% 
  mutate(food_category = as.factor(food_category),
         food_group = c(0,0,1,0,1,1,0,1,1,1,1),
         food_group = factor(food_group, labels = c('Plant Protein', 'Animal Protein'))) %>% 
  mutate(food_category = fct_reorder2(food_category, food_group, co2_per_kg )) %>% 
mutate( total_co2 = co2_per_kg * eaten) 

ggplot(co2_kg, aes (x = eaten, y = co2_per_kg))+
  geom_point(aes(size = total_co2))+
  geom_text_repel(aes(label = food_category),
                  force_pull = 0.05,
                  box.padding = 0.5,
                  point.padding = 0.1)+
  theme_minimal()

ggplot(co2_kg, aes (x = food_category, y = co2_per_kg, size = eaten))+
  geom_point()

## proportion of category by country ####

world_rnatural = rnaturalearth::ne_download(returnclass = "sf")

# Select key variables and assign them to a new object
world_iso = world_rnatural %>% 
  select(NAME_LONG, ISO_A3_EH, POP_EST, GDP_MD, CONTINENT)

# Transform the map
world_projected = world_iso %>% 
  st_transform("+proj=moll")



# find unmatched records
# Filter and calculate proportion

category <- 'Beef'

by_cat <- dat %>% 
  group_by(country) %>% 
  mutate(total = sum(co2_emmission),
         proportion = co2_emmission/total) %>% 
  filter(food_category == category)

# merge datasets

correct_names <- by_cat %>% 
  mutate(country = replace(country, country == 'Congo', 'Democratic Republic of the Congo'),
         country = replace(country, country == 'Gambia', 'The Gambia'),
         country = replace(country, country == 'Macedonia', 'North Macedonia'),
         country = replace(country, country == 'Russia', 'Russian Federation'),
         country = replace(country, country == 'South Korea', 'Republic of Korea'),
         country = replace(country, country == 'Swaziland', 'Kingdom of eSwatini'),
         country = replace(country, country == 'Swaziland', 'Kingdom of eSwatini'),
         country = replace(country, country == 'Taiwan. ROC', 'Taiwan'),
         country = replace(country, country == 'USA', 'United States'),)

combined_df = left_join(world_projected,
                        correct_names,  # Dataset 2
                        by = c("NAME_LONG" = "country"))  # Variables

tm_shape(combined_df) +
  tm_polygons(c("proportion"), 
              palette = 'viridis',
              title = paste0("Proportion of emmissions from ", category )) +
  tm_layout(main.title = paste0(" Mapping of Proportion of emmissions from ", category),
            frame = FALSE,
  )
##consumption of plant vs animal####

dat <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv') %>% 
  group_by(country) %>% 
  mutate(total = sum(co2_emmission),
         proportion = co2_emmission/total) %>%
  mutate(food_category = as.factor(food_category),
         food_group = c(1,1,1,1,1,1,1,0,0,0,0),
         food_group = factor(food_group, labels = c('Plant', 'Animal'))) %>% 
  mutate(country = replace(country, country == 'Congo', 'Democratic Republic of the Congo'),
         country = replace(country, country == 'Gambia', 'The Gambia'),
         country = replace(country, country == 'Macedonia', 'North Macedonia'),
         country = replace(country, country == 'Russia', 'Russian Federation'),
         country = replace(country, country == 'South Korea', 'Republic of Korea'),
         country = replace(country, country == 'Swaziland', 'Kingdom of eSwatini'),
         country = replace(country, country == 'Swaziland', 'Kingdom of eSwatini'),
         country = replace(country, country == 'Taiwan. ROC', 'Taiwan'),
         country = replace(country, country == 'USA', 'United States'),) %>% 
  rename("NAME_LONG" = "country") %>% 
  group_by(NAME_LONG, food_group) %>% 
  summarise(total_co2_gp = sum(co2_emmission),
            total_con_gp = sum(consumption),) %>%
  # mutate(total_co2_prop_animal = total_co2_gp / (total_co2_gp +lag(total_co2_gp)),
  #        total_co2_prop_plant =  1 - lead(total_co2_prop_animal)) %>% 
  
  pivot_wider(names_from = food_group, values_from = c(total_co2_gp, total_con_gp)) %>% 
  mutate(prop_plant = 1-(total_co2_gp_Animal / (total_co2_gp_Animal + total_co2_gp_Plant)),
         total_co2 = total_co2_gp_Animal + total_co2_gp_Plant)



library(ggrepel)
library(ggiraph)
library(plotly)

top <- dat %>% 
  arrange(-total_co2) %>% 
  head(10)

ggplot <- ggplot(dat, aes(x = total_co2, y = prop_plant ))+
  geom_point(aes(size = total_co2, alpha = 0.6), colour ='red')+
  geom_point(aes(size = total_co2_gp_Plant, alpha = 0.6), colour = 'yellow')+
  # geom_text_repel(data = top, aes(label = NAME_LONG),
  #                 box.padding = 0.5)+
  scale_fill_viridis_b()

ggplot

plotly::ggplotly(ggplot)

plot_ly(dat, x = ~total_co2, y = ~prop_plant) %>% 
  add_markers(
    sizes=c(1,100),
    hoverinfo="total_co2",
    size=~total_co2) %>% 
  add_markers(
    sizes = c(1,100),
    size = ~total_co2_gp_Plant)
  )

## Bubble plot of c02 ####

co2_dat <- dat %>% 
  group_by(country) %>% 
  mutate(food_category = as.factor(food_category),
         food_group = c(1,1,1,1,1,1,1,0,0,0,0),
         food_group = factor(food_group, labels = c('Plant', 'Animal'))) %>% 
  ungroup() %>% 
  group_by(food_group) %>% 
  mutate(sum_co2 = sum(co2_emmission),
         sum_kg = sum(consumption),
         co2_per_kg = (sum_co2/sum_kg)) %>% 
  summarise(co2_per_kg = min(co2_per_kg),
            sum_co2 = min(sum_co2),
            sum_kg = min(sum_kg)) %>% 
  mutate(animal_as_plant =(sum_kg * lag(co2_per_kg)))

x <- c(1,1,2)
y<- c(1,3,2)
z<- as.list(c(co2_dat[[3]],co2_dat[2,5]))

df <- data.frame(x,y) %>% 
  mutate(value = round(as.numeric(z)))

pos <- position_jitter(width = 0, height = 0.001, seed = 2)

ggplot(new_df, aes( x = 1, y = 5))+
  geom_point(aes(size = co2_dat.sum_co2), position = pos)+
  geom_label_repel(aes(label= groups), position = pos)+

  scale_size_area(max_size = 30) +
  facet_wrap(~groups)

ggplot(new_df, aes( x = 1, y = 5))+
  geom_point(aes(size = co2_dat.sum_co2), alpha = c(1, 0.5,1), colour = c('darkgreen', 'blue', 'darkgreen'))+
  geom_text_repel(aes(label= round(co2_dat.sum_co2)))+
  scale_size_area(max_size = 100) +
  facet_wrap(~groups)+
  theme_void()+
  theme(legend.position = "none") 
 
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
ggplot(df, aes(x =x, y = y))+
  geom_point(aes(size = value), colour = c('#D55E00', '#0072B2', '#D55E00'))+
  scale_size_area(max_size = 120) +
  theme_void(base_size = 20)+
  theme(legend.position = "none")+
  xlim(0,2.5)+
  ylim(0,5)+
  geom_text(aes(label = paste0( scales::comma(value)," KG\n of CO2")),
            colour = 'white')+
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
  # geom_text(aes(x = 1, y = 1.5), 
  #           label = '+',
  #           colour = '#E69F00',
  #           size = 20)+
geom_text(aes(x = 2, y = 4), 
          label = 'If everyone ate\n only plant protein',
          colour = '#009E73',
          size = 6)+
  geom_curve(aes(x = 2, y = 3.7, xend = 2, yend = 2.7), 
             colour = "#009E73", 
             size=1, 
             curvature = -0.2,
             arrow = arrow(length = unit(0.03, "npc")))+
  
  labs(title = "C02 Emissions of the world: Current and Potential <br>
       <span style='color:#D55E00'> Plant </span>and <span style='color:#0072B2'>Animal  </span>CO2 emissions",
       x = NULL,
       y = ' Consumption per capita (Kg)') +
  theme(plot.title = element_markdown(hjust = 0.5),
        )
  

?position_jitter()
  
