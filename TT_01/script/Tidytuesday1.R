### Pet Cats UK ###

### Load Library ###
library(tidyverse)
library(here)
library(dplyr)
library(kableExtra)
library(maps)
library(mapdata)
library(mapproj)
library(janitor)

### Read Data Manually ###
cats_uk <- read_csv("https://www.datarepository.movebank.org/bitstream/handle/10255/move.883/Pet%20Cats%20United%20Kingdom.csv?sequence=3") |> 
  clean_names() |> 
  # Standardize things and reorder columns.
  select(
    tag_id = tag_local_identifier,
    event_id:location_lat,
    ground_speed,
    height_above_ellipsoid,
    algorithm_marked_outlier,
    manually_marked_outlier,
    study_name
  ) |> 
  # Explicitly encode FALSE in the outlier columns.
  tidyr::replace_na(
    list(
      algorithm_marked_outlier = FALSE,
      manually_marked_outlier = FALSE
    )
  )

cats_uk_reference <- read_csv("https://www.datarepository.movebank.org/bitstream/handle/10255/move.884/Pet%20Cats%20United%20Kingdom-reference-data.csv?sequence=1") |>
  clean_names() |> 
  mutate(
    # animal_life_stage is ALMOST just age in years.
    age_years = case_when(
      str_detect(animal_life_stage, fixed("<")) ~ 0L,
      str_detect(animal_life_stage, "year") ~ str_extract(
        animal_life_stage, "\\d+"
      ) |> 
        as.integer(),
      TRUE ~ NA_integer_
    )
  ) |> 
  # There are only a handful of unique values for the comments, extract those.
  separate_wider_delim(
    animal_comments,
    "; ",
    names = c("hunt", "prey_p_month")
  ) |> 
  mutate(
    hunt = case_when(
      str_detect(hunt, "Yes") ~ TRUE,
      str_detect(hunt, "No") ~ FALSE,
      TRUE ~ NA
    ),
    prey_p_month = as.numeric(
      str_remove(prey_p_month, "prey_p_month: ")
    )
  ) |> 
  # manipulation_comments similarly has a pattern.
  separate_wider_delim(
    manipulation_comments,
    "; ",
    names = c("hrs_indoors", "n_cats", "food")
  ) |> 
  mutate(
    hrs_indoors = as.numeric(
      str_remove(hrs_indoors, "hrs_indoors: ")
    ),
    n_cats = as.integer(
      str_remove(n_cats, "n_cats: ")
    )
  ) |> 
  separate_wider_delim(
    food,
    ",",
    names = c("food_dry", "food_wet", "food_other")
  ) |> 
  mutate(
    food_dry = case_when(
      str_detect(food_dry, "Yes") ~ TRUE,
      str_detect(food_dry, "No") ~ FALSE,
      TRUE ~ NA
    ),
    food_wet = case_when(
      str_detect(food_wet, "Yes") ~ TRUE,
      str_detect(food_wet, "No") ~ FALSE,
      TRUE ~ NA
    ),
    food_other = case_when(
      str_detect(food_other, "Yes") ~ TRUE,
      str_detect(food_other, "No") ~ FALSE,
      TRUE ~ NA
    )
  ) |>
  # Drop uninteresting fields.
  select(
    -animal_life_stage,
    -attachment_type,
    -data_processing_software,
    -deployment_end_type,
    -duty_cycle,
    -deployment_id,
    -manipulation_type,
    -tag_manufacturer_name,
    -tag_mass,
    -tag_model,
    -tag_readout_method
  )


### Glimpse Data ###

glimpse(cats_uk) 
glimpse(cats_uk_reference)

UK <- map_data("world", region = "UK")

ggplot()+
  geom_polygon(data = UK,
               aes(x =long,
                   y= lat,
                   group=group),
               color="pink")+
  coord_map()+
  geom_point(data = cats_uk,
             aes(x = location_long,
                 y= location_lat,
                 color="where the cats were found"))+
  scale_x_continuous(limits = c(-6,-3)) + 
  scale_y_continuous(limits = c(50,51)) 
  ggsave(here("TT_01","output","Tidytuesday_01_plot.pdf"))
