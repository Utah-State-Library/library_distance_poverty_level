library(tidyverse)
library(magrittr)
library(sf)
library(ggplot2)
library(leaflet)
library(shiny)
library(shinyWidgets)
library(bslib)
library(highcharter)
library(reactable)
library(crosstalk)

source("prep_data.R", local = TRUE)$value

counties <- unique(poverty_df_munic_centroid$county_name) %>% sort()


ui <- page_sidebar(
  title = "Distance to the Nearest Library",
  fillable = FALSE,
  sidebar = sidebar(
    pickerInput(
      "county",
      choices = counties,
      selected = counties,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = paste0(
          "count > ",
          length(counties) - 1
        ),
        `count-selected-text` = "All Counties"
      )
    ),
    input_switch("hide_bookmobile_counties", "Hide Bookmobile Counties?")
  ),
  layout_columns(
    col_widths = c(12, 12),
    layout_columns(
      col_widths = c(6, 6),
      card(
        leafletOutput("map")
      ),
      card(reactableOutput("distance_table"))
    ),
    card(highchartOutput("hc_scatter"))
  )
)

server <- function(input, output, session) {
  centroid_reactive <- reactive({
    df_out <- poverty_df_munic_centroid %>%
      filter(county_name %in% input$county)

    if (input$hide_bookmobile_counties) {
      df_out %<>%
        filter(
          !county_name %in%
            c("Utah", "Iron", "Wayne", "Piute", "Garfield", "Sanpete", "Kane")
        )
    }

    validate(
      need(nrow(df_out) > 0, "Please select at least one county")
    )

    df_out
  })

  outlet_reactive <- reactive({
    df <- outlets_sf %>%
      filter(CNTY %in% input$county)

    if (input$hide_bookmobile_counties) {
      df %<>%
        filter(
          !CNTY %in%
            c("Utah", "Iron", "Wayne", "Piute", "Garfield", "Sanpete", "Kane")
        )
    }

    validate(
      need(nrow(df) > 0, "Please select at least one county")
    )
    df
  })

  output$map <- renderLeaflet({
    ##### Find Distance from Centroids to Outlet Points

    centroid_react <- st_transform(
      centroid_reactive(),
      4326
    )

    outlets_react <- outlet_reactive()

    nearest_points <- st_nearest_points(centroid_react, st_union(outlets_react))
    nearest_id <- st_nearest_feature(centroid_react, outlets_react)

    centroid_map <- centroid_react
    centroid_map$nearest_lib <- outlets_react$CURRENT_LIBNAME_OUTLET[nearest_id]
    centroid_map$distance <- st_distance(
      centroid_react,
      outlets_react[nearest_id, ],
      by_element = TRUE
    ) %>%
      units::set_units(mi)

    centroid_map_wgs84 <- st_transform(centroid_map, crs = 4326)
    centroid_map$lon <- st_coordinates(centroid_map_wgs84)[, 1]
    centroid_map$lat <- st_coordinates(centroid_map_wgs84)[, 2]

    outlets_wgs84 <- st_transform(outlets_react, crs = 4326)
    outlets_react$lon <- st_coordinates(outlets_wgs84)[, 1]
    outlets_react$lat <- st_coordinates(outlets_wgs84)[, 2]

    nearest_points_wgs84 <- st_transform(nearest_points, crs = 4326)
    nearest_points$lon <- st_coordinates(nearest_points_wgs84)[, 1]
    nearest_points$lat <- st_coordinates(nearest_points_wgs84)[, 2]

    pal <- colorFactor(
      palette = c("#d7d7d7", "#f8fe50", "#ffc507", "#ff0000"),
      domain = centroid_map$Percent.below.poverty.level
    )

    leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(
        data = centroid_map,
        lat = centroid_map$lat,
        lng = centroid_map$lon,
        radius = 3,
        opacity = .8,
        color = ~ pal(centroid_map$Percent.below.poverty.level),
        label = ~ lapply(
          paste0(
            #popup
            "<b>",
            centroid_map$NAME,
            "</b>",
            "<br>",
            "Percent Below Poverty Level: ",
            centroid_map$Percent.below.poverty.level,
            "%<br>",
            "Distance to nearest library: ",
            round(centroid_map$distance, 2),
            " miles"
          ),
          HTML
        )
      ) %>%
      addCircleMarkers(
        data = outlets_react,
        lat = outlets_react$lat,
        lng = outlets_react$lon,
        radius = 2,
        opacity = .8,
        color = "#413ac8",
        label = ~ lapply(
          paste0("<b>", outlets_react$CURRENT_LIBNAME_OUTLET, "</b>"),
          HTML
        )
      ) %>%
      addPolylines(
        data = st_nearest_points(centroid_map, st_union(outlets_react)), #centroid_map
        weight = 1
      )
  })

  ##### Scatter Plot #####

  output$hc_scatter <- renderHighchart({
    centroid_react <- st_transform(
      centroid_reactive(),
      4326
    )

    outlets_react <- outlet_reactive()

    nearest_points <- st_nearest_points(centroid_react, st_union(outlets_react))
    nearest_id <- st_nearest_feature(centroid_react, outlets_react)

    centroid_map <- centroid_react
    centroid_map$nearest_lib <- outlets_react$CURRENT_LIBNAME_OUTLET[nearest_id]
    centroid_map$distance <- st_distance(
      centroid_react,
      outlets_react[nearest_id, ],
      by_element = TRUE
    ) %>%
      units::set_units(mi)

    # centroid_map_wgs84 <- st_transform(centroid_map, crs = 4326)
    # centroid_map$lon <- st_coordinates(centroid_map_wgs84)[, 1]
    # centroid_map$lat <- st_coordinates(centroid_map_wgs84)[, 2]

    hchart(
      centroid_map,
      "scatter",
      hcaes(Percent.below.poverty.level, distance)
    ) %>%
      hc_tooltip(
        pointFormat = paste0(
          "<b>{point.NAME}</b><br>",
          "Nearest Library: {point.nearest_lib}<br>",
          "Distance to Library: {point.y:,.2f} Miles<br>",
          "Percent Below Poverty Level: {point.x:,.2f}%"
        ),
        headerFormat = ""
      ) %>%
      hc_title(
        text = "Poverty vs Distance to Nearest Library"
      ) %>%
      hc_yAxis(
        title = list(text = "Distance (Miles)"),
        labels = list(
          style = list(fontSize = "15px")
        )
      ) %>%
      hc_xAxis(
        allowDecimals = FALSE,
        title = list(text = "Percent Below Poverty Level"),
        labels = list(
          style = list(fontSize = "15px")
        )
      ) %>%
      hc_exporting(
        enabled = TRUE,
        filename = paste0(
          "percentbelowpoverty_libdistance"
        )
      )
  })

  output$distance_table <- renderReactable({
    centroid_react <- st_transform(
      centroid_reactive(),
      4326
    )

    outlets_react <- outlet_reactive()

    nearest_points <- st_nearest_points(centroid_react, st_union(outlets_react))
    nearest_id <- st_nearest_feature(centroid_react, outlets_react)

    centroid_map <- centroid_react
    centroid_map$nearest_lib <- outlets_react$CURRENT_LIBNAME_OUTLET[nearest_id]
    centroid_map$distance <- st_distance(
      centroid_react,
      outlets_react[nearest_id, ],
      by_element = TRUE
    ) %>%
      units::set_units(mi)

    centroid_map %>% #centroid_map
      as.data.frame() %>%
      select(NAME, Percent.below.poverty.level, nearest_lib, distance) %>%
      mutate(
        Percent.below.poverty.level = round(Percent.below.poverty.level, 2),
        distance = round(distance, 2)
      ) %>%
      reactable(
        resizable = TRUE,
        pagination = FALSE,
        sortable = TRUE,
        highlight = TRUE,
        virtual = TRUE,
        height = 650,
        defaultExpanded = TRUE,
        compact = TRUE,
        theme = reactableTheme(
          headerStyle = list(
            background = "#ecf0f1",
            borderColor = "#555"
          )
        ),
        defaultColDef = colDef(align = "left"),
        columns = list(
          NAME = colDef(name = "Municipality"),
          Percent.below.poverty.level = colDef(
            name = "Percent Below Poverty Level"
          ),
          nearest_lib = colDef(name = "Nearest Library"),
          distance = colDef(name = "Distance to Nearest Library (Miles)")
        )
      )
  })
}

#### Run App ####
shinyApp(
  ui = ui,
  server = server
)
