#### Prep Data

#### Data
cnty <- read_sf(
  "C:/Users/samdutton/Desktop/Local Shiny Apps/Library-Map/data/counties/Counties.shp"
) %>%
  as.data.frame() %>%
  select(COUNTYNBR, county_name = NAME) %>%
  mutate(county_name = str_to_title(county_name))

poverty_df1 <- read.csv(
  "C:/Users/samdutton/Downloads/ACSST5Y2024.S1701-2026-05-12T191644.csv"
) %>%
  head(1)
outlets <- readRDS(
  "C:/Users/samdutton/Desktop/Local Shiny Apps/Library-Map/data/processed/outlet_ut_app.RDS"
)
municipalities <- sf::read_sf(
  "C:/Users/samdutton/Desktop/Local Shiny Apps/Library-Map/data/municipalities/Municipalities.shp"
)

#### Tidy Poverty DF

poverty_df2 <- poverty_df1 %>%
  mutate(across(everything(), ~ as.character(.))) %>%
  pivot_longer(
    !Label..Grouping.,
    names_to = "place",
    values_to = "value"
  ) %>%
  select(place, value) %>%
  mutate(place = gsub("St\\.\\.", "St\\.", place))

poverty_df2 %<>%
  separate_wider_delim(
    place,
    "..",
    names = c("place_name", "state", "method", "estimate"),
    too_many = "debug"
  ) %>%
  mutate(value = gsub(",|%", "", value))

poverty_df3 <- poverty_df2 %>%
  select(place_name, method, value) %>%
  mutate(value = as.numeric(value)) %>%
  pivot_wider(
    names_from = "method",
    values_from = "value"
  )

poverty_df <- poverty_df3 %>%
  mutate(
    place_name = gsub("\\.city|\\.town|\\.township|\\.metro", "", place_name),
    place_name = gsub("\\.", " ", place_name)
  )

#### Join Municipalities and Poverty DF

setdiff(municipalities$NAME, poverty_df$place_name)

poverty_df %<>%
  mutate(
    place_name = case_when(
      place_name == "St George" ~ "St. George",
      place_name == "Marriott Slaterville" ~ "Marriott-Slaterville",
      place_name == "Heber" ~ "Heber City",
      place_name == "Magna" ~ "Magna City",
      .default = place_name
    )
  )

setdiff(poverty_df$place_name, municipalities$NAME)

poverty_df_munic <- left_join(
  municipalities,
  poverty_df,
  by = c("NAME" = "place_name")
)


poverty_df_munic %<>% left_join(cnty, by = "COUNTYNBR")


##### Get Municipality Centroid
poverty_df_munic_centroid <- sf::st_centroid(poverty_df_munic)

##### Tidy Outlet File
outlets_sf <- st_as_sf(
  outlets,
  coords = c("LONG", "LAT"),
  crs = 4326
)
