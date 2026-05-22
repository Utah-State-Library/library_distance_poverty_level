library(tidyverse)
library(magrittr)
library(sf)
library(leaflet)
library(shiny)
library(shinyWidgets)
library(bslib)
library(highcharter)
library(reactable)
library(crosstalk)
library(units)

source("prep_data.R", local = TRUE)$value

##### Counties #####

counties <- unique(
  poverty_df_munic_centroid$county_name
) %>%
  sort()

bookmobile_counties <- c(
  "Utah",
  "Iron",
  "Wayne",
  "Piute",
  "Garfield",
  "Sanpete",
  "Kane"
)

##### UI #####

ui <- page_sidebar(
  title = "Distance to the Nearest Library",
  fillable = FALSE,

  tags$head(
    tags$style(HTML(
      "

    /* Default state */
    .leaflet-marker-icon {
      opacity: 0.25 !important;
      transition: opacity 0.2s ease;
    }

    /* Selected markers */
    .leaflet-marker-icon.crosstalk-selection {
      opacity: 1 !important;
      transform: scale(1.4);
      z-index: 1000 !important;
    }

    "
    ))
  ),

  sidebar = sidebar(
    pickerInput(
      inputId = "county",
      label = "Select Counties",

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

    input_switch(
      "hide_bookmobile_counties",
      label = "Hide Bookmobile Counties?"
    )
  ),

  layout_columns(
    col_widths = c(12, 12),
    card(
      layout_columns(
        col_widths = c(6, 6),
        value_box(
          "Average Percent Below Poverty Level",
          uiOutput("avg_poverty_vb")
        ),
        value_box(
          "Average Distance to Nearest Library",
          uiOutput("avg_distance_vb")
        )
      )
    ),

    layout_columns(
      col_widths = c(6, 6),

      card(
        full_screen = TRUE,
        leafletOutput("map", height = 650)
      ),

      card(
        full_screen = TRUE,
        reactableOutput("distance_table")
      )
    ),

    card(
      full_screen = TRUE,
      highchartOutput("hc_scatter", height = 500)
    )
  )
)

##### SERVER #####

server <- function(input, output, session) {
  ##### Main Spatial Dataset #####

  centroid_map <- reactive({
    centroid_sf <- st_transform(
      poverty_df_munic_centroid, #centroid_reactive(),
      4326
    )

    outlets_sf_react <- st_transform(
      outlets_sf, #outlet_reactive(),
      4326
    )

    ##### Nearest Library #####

    nearest_id <- st_nearest_feature(
      centroid_sf,
      outlets_sf_react
    )

    centroid_sf$nearest_lib <-
      outlets_sf_react$CURRENT_LIBNAME_OUTLET[
        nearest_id
      ]

    centroid_sf$outlet_key <- centroid_sf$nearest_lib

    ##### Distance #####

    centroid_sf$distance <- st_distance(
      centroid_sf,
      outlets_sf_react[nearest_id, ],
      by_element = TRUE
    ) %>%
      set_units(mi) %>%
      drop_units()

    ##### Coordinates #####

    coords <- st_coordinates(
      centroid_sf
    )

    centroid_sf$lon <- coords[, 1]
    centroid_sf$lat <- coords[, 2]

    centroid_sf$crosstalk_id <- paste0(
      centroid_sf$county_name,
      "_",
      centroid_sf$NAME
    )

    centroid_sf
  })

  ##### Municipality Centroids #####

  centroid_reactive <- reactive({
    df_out <- centroid_map() %>% #poverty_df_munic_centroid %>%
      filter(
        county_name %in% input$county
      )

    if (isTRUE(input$hide_bookmobile_counties)) {
      df_out <- df_out %>%
        filter(
          !county_name %in% bookmobile_counties
        )
    }

    validate(
      need(
        nrow(df_out) > 0,
        "Please select at least one county"
      )
    )

    df_out
  })

  ##### Library Outlets #####

  outlet_reactive <- reactive({
    df_out <- outlets_sf %>%
      filter(
        CNTY %in% input$county
      )

    if (isTRUE(input$hide_bookmobile_counties)) {
      df_out <- df_out %>%
        filter(
          !CNTY %in% bookmobile_counties
        )
    }

    validate(
      need(
        nrow(df_out) > 0,
        "Please select at least one county"
      )
    )

    df_out
  })

  # ##### SharedData for Leaflet #####

  # shared_map <- reactive({
  #   SharedData$new(
  #     centroid_reactive(),
  #     key = ~crosstalk_id,
  #     group = "centroids"
  #   )
  # })

  # ##### SharedData for Reactable #####

  # shared_table <- reactive({
  #   centroid_reactive() %>%

  #     st_drop_geometry() %>%

  #     select(
  #       crosstalk_id,
  #       NAME,
  #       Percent.below.poverty.level,
  #       nearest_lib,
  #       distance
  #     ) %>%

  #     mutate(
  #       Percent.below.poverty.level = round(
  #         Percent.below.poverty.level,
  #         2
  #       ),

  #       distance = round(distance, 2)
  #     ) %>%

  #     SharedData$new(
  #       key = ~crosstalk_id,
  #       group = "centroids"
  #     )
  # })

  ##### ValueBoxes #####
  output$avg_distance_vb <- renderUI({
    val <- centroid_reactive() %>%
      as.data.frame() %>%
      summarise(avg_dist = round(mean(distance), 2)) %>%
      pull()

    paste0(val, " Miles")
  })

  output$avg_poverty_vb <- renderUI({
    val <- centroid_reactive() %>%
      as.data.frame() %>%
      summarise(avg_poverty = round(mean(Percent.below.poverty.level), 2)) %>%
      pull()

    paste0(val, "%")
  })

  ##### Leaflet Map #####

  output$map <- renderLeaflet({
    centroid_sf <- centroid_reactive()

    outlets_sf_react <- st_transform(
      outlet_reactive(),
      4326
    )

    ##### Nearest Lines #####

    nearest_lines <- st_nearest_points(
      centroid_sf,
      st_union(outlets_sf_react)
    )

    ##### Color Palette #####

    pal <- colorFactor(
      palette = c(
        "#d7d7d7",
        "#f8fe50",
        "#ffc507",
        "#ff0000"
      ),

      domain = centroid_sf$Percent.below.poverty.level
    )

    ##### Leaflet #####

    #leaflet(shared_map()) %>%
    leaflet(centroid_reactive()) %>%

      addProviderTiles(
        providers$CartoDB.Positron
      ) %>%

      ##### Municipality Points #####

      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,

        radius = 5,

        stroke = FALSE,

        fillOpacity = 0.85,

        color = ~ pal(
          Percent.below.poverty.level
        ),

        label = ~ lapply(
          paste0(
            "<b>",
            NAME,
            "</b><br>",

            "Percent Below Poverty Level: ",
            round(
              Percent.below.poverty.level,
              2
            ),
            "%<br>",

            "Nearest Library: ",
            nearest_lib,
            "<br>",

            "Distance to Library: ",
            round(distance, 2),
            " miles"
          ),
          HTML
        )
      ) %>%

      ##### Library Outlets #####

      addCircleMarkers(
        data = outlets_sf_react,

        lng = ~ st_coordinates(geometry)[, 1],
        lat = ~ st_coordinates(geometry)[, 2],

        radius = 4,

        color = "#413ac8",

        stroke = TRUE,
        weight = 1,

        fillOpacity = 1,

        label = ~ lapply(paste0("<b>", CURRENT_LIBNAME_OUTLET, "</b>"), HTML)
      ) %>%

      ##### Connection Lines #####

      addPolylines(
        data = nearest_lines,
        weight = 1,
        opacity = 0.5
      )
  })

  ##### Scatter Plot #####

  output$hc_scatter <- renderHighchart({
    scatter_df <- centroid_reactive() %>%
      st_drop_geometry()

    hchart(
      scatter_df,

      type = "scatter",

      hcaes(
        x = Percent.below.poverty.level,
        y = distance
      )
    ) %>%

      hc_title(
        text = "Poverty vs Distance to Nearest Library"
      ) %>%

      hc_xAxis(
        title = list(
          text = "Percent Below Poverty Level"
        )
      ) %>%

      hc_yAxis(
        title = list(
          text = "Distance to Library (Miles)"
        )
      ) %>%

      hc_tooltip(
        headerFormat = "",

        pointFormat = paste0(
          "<b>{point.NAME}</b><br>",
          "Percent Below Poverty: ",
          "{point.x:.2f}%",
          "Nearest Library: ",
          "{point.nearest_lib}<br>",
          "Distance: ",
          "{point.y:.2f} Miles<br>"
        )
      )
  })

  ##### Reactable #####

  output$distance_table <- renderReactable({
    reactable(
      centroid_reactive() %>%
        as.data.frame() %>%
        select(NAME, Percent.below.poverty.level, nearest_lib, distance) %>%
        mutate(
          distance = round(distance, 2),
          Percent.below.poverty.level = round(Percent.below.poverty.level, 2)
        ), #shared_table(),
      searchable = FALSE,
      sortable = TRUE,
      resizable = TRUE,
      highlight = TRUE,
      bordered = FALSE,
      striped = FALSE,
      compact = TRUE,
      pagination = FALSE,
      #selection = "multiple",
      #onClick = "select",
      # rowStyle = list(
      #   cursor = "pointer"
      # ),
      height = 650,
      defaultColDef = colDef(
        align = "left"
      ),
      columns = list(
        #crosstalk_id = colDef(show = FALSE),
        NAME = colDef(
          name = "Municipality"
        ),
        Percent.below.poverty.level = colDef(
          name = "Percent Below Poverty Level"
        ),
        nearest_lib = colDef(
          name = "Nearest Library"
        ),
        distance = colDef(
          name = "Distance to Nearest Library (Miles)"
        )
      ),
      theme = reactableTheme(
        headerStyle = list(
          background = "#ecf0f1",
          borderColor = "#dfe6e9",
          fontWeight = "bold"
        )
      )
    )
  })
}

##### RUN APP #####

shinyApp(
  ui = ui,
  server = server
)
