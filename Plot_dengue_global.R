## Libraries ----

library(cleanepi)
library(geodata)
library(RColorBrewer)
library(rio)
library(sf)
library(tidyverse)

## Data ----

global = import("dengue_global.xlsx")
global = global %>% 
  mutate(date = as.Date(date), year = year(date))

cases = global %>% 
  group_by(country, year) %>% 
  summarise(cases = mean(cases)) %>% 
  filter(year == 2024)

world_map = st_as_sf(world(resolution = 1, path = tempdir()), crs = 4326)

cases = st_as_sf(left_join(cases, world_map,
                           by = c("country" = "NAME_0")), crs = 4326)

classifier = Vectorize(function(x){
  if (is.na(x)){
    return(NA)
  } else if (x == 0){
    return("0")
  } else if (x > 0 & x < 500){
    return("1-499")
  } else if (x >= 500 & x < 5000){
    return("500-4900")
  } else if (x >= 5000 & x < 49000){
    return("5000 - 49000")
  } else if (x >= 50000){
    return("50000+")
  } else {
    return(NA)
  }
})

cases$type = classifier(cases$cases)

## Plot ----

custom_colors = brewer.pal(n = 9, name = "Greens")[3:9]

ggplot()+
  geom_sf(data = world_map, aes(fill = NA))+
  geom_sf(data = cases, aes(fill = type))+
  scale_fill_manual(values = custom_colors, na.value = NA)+
  labs(subtitle = "World Map of Dengue cases (2024)" , fill = "Cases")+
  theme_minimal(base_size = 18)+
  theme(
    plot.subtitle = element_text(face = "bold"),
    axis.ticks = element_blank(),
    axis.text = element_blank()
    )

