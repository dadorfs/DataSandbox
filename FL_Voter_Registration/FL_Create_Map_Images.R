## -------------------------------------------------
##
## Script name: FL Create Registered Voter Maps
##
## Purpose of script: Create a county map showing the
## proportions of registered democrats and republicans
## from 1995-2022
##
## Author: Daniel Dorfsman
##
## Date Created: 2023-08-30
##
## -------------------------------------------------
##
## Notes:
##   
##
## -------------------------------------------------


## Load packages

library(tidyverse)
library(readxl)
library(tigris)
library(glue)
library(showtext)
library(ggtext)

## -------------------------------------------------
## Load data

## Paths
fl_reg_1995_to_2016_paths <-
  list.files("Records_clean/", pattern = "*.csv", full.names = TRUE)

fl_reg_2017_to_2022_paths <- 
  list.files("Records_raw/", pattern = ".xlsx", full.names = TRUE)

## Read
fl_reg_1995_to_2016_lst <-
  map(fl_reg_1995_to_2016_paths, ~ read_csv(.x, col_names = TRUE))

fl_reg_2017_to_2022_lst <- 
  map(fl_reg_2017_to_2022_paths, ~ read_xlsx(.x, sheet = "December", skip = 3))

names(fl_reg_2017_to_2022_lst) <- 2017:2022
  
## -------------------------------------------------
## Process Data

# 1995-2016
fl_reg_1995_to_2016_lst_slct <- fl_reg_1995_to_2016_lst %>%
  map( ~ select(.x, YEAR, COUNTY, REP, DEM))

fl_reg_1995_to_2016_df <- fl_reg_1995_to_2016_lst_slct %>% 
  bind_rows() %>% 
  filter(COUNTY != "TOTALS")

# A few instances of 'DADE' instead of 'MIAMI-DADE'
table(fl_reg_1995_to_2016_df$COUNTY)

fl_reg_1995_to_2016_df$COUNTY <- str_replace(fl_reg_1995_to_2016_df$COUNTY,
                                             "^DADE$", "MIAMI-DADE")

# 2017-2022
fl_reg_2017_to_2022_lst_slct <- fl_reg_2017_to_2022_lst %>% 
  map( ~ rename_with(.x, toupper, everything()) %>% 
         select(COUNTY, 
                REP = `REPUBLICAN PARTY OF FLORIDA`,
                DEM = `FLORIDA DEMOCRATIC PARTY`))
               
fl_reg_2017_to_2022_df <- fl_reg_2017_to_2022_lst_slct %>% 
  bind_rows(.id = "YEAR") %>% 
  mutate(YEAR = as.numeric(YEAR)) %>% 
  filter(COUNTY != "TOTALS")

# Combine
fl_reg_1995_to_2022 <- bind_rows(fl_reg_1995_to_2016_df, fl_reg_2017_to_2022_df)

# Convert county name to title
fl_reg_1995_to_2022 <- fl_reg_1995_to_2022 %>% 
  mutate(COUNTY = str_to_title(COUNTY)) %>% 
  mutate(COUNTY = str_replace(COUNTY, "Desoto", "DeSoto")) # map data uses DeSoto, not Desoto

# Add proportion republican
fl_reg_1995_to_2022 <- fl_reg_1995_to_2022 %>% 
  rowwise() %>% 
  mutate(PROP_REP = REP / (REP + DEM), 
         DEM_REP_RATIO = DEM / REP)


## -------------------------------------------------
## Create Maps

fl_state <- filter(states(cb = TRUE, class = "sf"), NAME %in% "Florida")
fl_counties <- counties(state = "Florida", cb = TRUE, class = "sf")

fl_counties_w_reg_voters <- fl_counties %>% 
  left_join(fl_reg_1995_to_2022, by = c("NAME" = "COUNTY"))

for(year in 1995:2022){

temp <- filter(fl_counties_w_reg_voters, YEAR == year)

legend_text <- 
  glue("Percent Registered Republican in **<span style='font-size:13pt;'>{year}</span>**<br>(Out of Total<span style = 'color:#FD2611;'> Republican</span> and <span style = 'color:#2532FE;'>Democratic</span> Registered Voters)</p>")

p <- 
  ggplot(temp) +
  geom_sf(aes(fill = PROP_REP), color = "black") +
  scale_fill_gradientn(
    name = legend_text,
    colors = c("blue", "purple2", "red"),
    values = c(0, 0.5, 1),
    limits = c(0, 1),
    breaks = c(0, 0.5, 1),
    labels = scales::label_percent(scale = 100)
  ) +
  labs(title = "<strong>Once Upon a Swing State</strong>") +
  guides(fill = guide_colorbar(
    title.position = 'top',
    title.hjust = 0,
    barwidth = unit(20, 'lines'),
    barheight = unit(0.5, 'lines')
  )) +
  theme(
    plot.background = element_rect(fill = "white", color = "white"),
    plot.title = element_markdown(size = 26, hjust = 0.5),
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'top',
    legend.title = element_markdown()
  )

ggsave(glue("FL_maps/{year}_map.png"), p, width = 6, height = 5)

}
