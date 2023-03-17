library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(shinythemes)
library(markdown)
library(bslib)
library(DT)
library(shinyWidgets)
library(plotly)
library(shinyBS)
sf::sf_use_s2(FALSE)

colors = data.frame(
  Assessment_Category = c("Impaired", "Good", "Not Assessed", "No State Data"),
  col = c("#DC851E", "#059FA4", "#A1A522", "#C2CAD7"),
  Priority = c(1, 2, 3, 4))

boundary_lines <- readRDS('data/nps_boundary_lines.RDS') 
inside <- readRDS('data/nps_inside.RDS')
outside <- readRDS('data/nps_outside.RDS') 

ui <- fluidPage(
  
  titlePanel("National Park Service Water Quality"),
  
  fluidRow(
    column(3, 
           h6("This is a draft version of a data viewer to accompany 
                NPCA's evaluation of state water quality assessments and 
                the National Park Service System. The underlying data used
                to develop this map comes from the EPA's ATTAINS geospatial
                database. Currently, the pie chart plots status by catchment 
                counts, not by catchment area."),
           fluidRow(pickerInput(
             inputId = "park", 
             label = "Select National Park:",
             choices = sort(boundary_lines$Park),
             selected = "Gauley River National Recreation Area",
             options = list('actions-box' = TRUE),
             multiple = FALSE)),
           fluidRow(plotOutput("plot2"))
    ),
    column(9,
           h6(""),
           h6(""),
           br(),
           leafletOutput("map2"), height = "100%")
  )
)

server <- function(input, output, session) {
  
  outsider <- reactive({
    
    outside <- filter(outside, Park == input$park)
    
    outside
    
  })
  
  insider <- reactive({
    
    inside <- filter(inside, Park == input$park)
    
    inside
    
  })
  
  boundary_liner <- reactive({
    
    boundary_lines <- filter(boundary_lines, Park == input$park)
    
    boundary_lines
    
  })
  
  output$plot2 <- renderPlot({
    validate (need(nrow(insider()) > 0, message="No assessment data currently available at this park unit."))
    pie <- insider() %>%
      st_drop_geometry() %>%
      distinct(Assessment_Unit, .keep_all = TRUE) %>%
      dplyr::group_by(Assessment_Category, col) %>%
      dplyr::summarize(count=n()) %>%
      ungroup()
    pie <- pie %>%
      mutate(Assessment_Category = factor(x = Assessment_Category, levels = Assessment_Category)) %>% 
      mutate(prop = count/sum(pie$count)) %>%  
      mutate(ypos = cumsum(prop)- 0.5*prop) %>%
      mutate(legend = paste0(Assessment_Category, " (", scales::percent(prop), ")"))
    
    ggplot(data=pie, aes(x="", y=count, fill=legend)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0) +
      scale_fill_manual(values=pie$col) +
      guides(fill=guide_legend(title="Status by Catchment")) +
      theme_void() + # remove background, grid, numeric label
      theme(text = element_text(size = 20)) 
  })
  
  #  generarte the map object 
  output$map2 <- leaflet::renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 4)) %>%
      setView(lng = -105.07592352501446, 
              lat = 40.59085658003177, 
              zoom = 6) %>%
      addProviderTiles("CartoDB.Positron", group = "CartoDB.Positron") %>%
      addLayersControl(
        # baseGroups = c("OpenStreetMap", "Light"),
        overlayGroups = c(
          "Catchments"
        ),
        position = "topleft",
        options = layersControlOptions(collapsed = TRUE)
      )
    # addPolylines(
    #   data = boundary_liner(),
    #   fillColor = "",
    #   fillOpacity = 0,
    #   color = "black",
    #   weight = 8) %>%
    # addPolygons(
    #   data = insider(),
    #   fillColor = insider()$col,
    #   fillOpacity = 1,
    #   color = "black",
    #   weight = 1,
    #   popup = paste(insider()$Assessment_Category,
    #                 "<br>",
    #                 insider()$Assessment_Unit,
    #                 "<br>",
    #                 ifelse(is.na(insider()$Impairments), "", insider()$Impairments))) %>%
    # addPolygons(
    #   data = outsider(),
    #   fillColor = outsider()$col,
    #   fillOpacity = 0.15,
    #   color = "black",
    #   weight = 1,
    #   popup = paste0(outsider()$Assessment_Category,
    #                 "<br>",
    #                 outsider()$Assessment_Unit,
    #                 "<br>",
    #                 ifelse(is.na(outsider()$Impairments), "", outsider()$Impairments)))
  })
  
  # this makes it so the proxy map is rendered in the background, otherwise the map is empty when you first navigate to this page
  outputOptions(output, "map2", suspendWhenHidden=FALSE)
  
  #set location to park
  observe({
    c1 <- reactive({
      boundary_liner() %>%
        #filter(Park  == input$park) %>%
        st_set_agr("constant") %>% # attributes constant over geometries (suppresses warning message)
        st_bbox() %>%
        unname()
      #sf::st_centroid() %>%
      #st_coordinates()
    })
    #  
    leafletProxy('map2') %>% 
      clearMarkers() %>%
      clearShapes() %>%
      fitBounds(lng1 =  c1()[1], lat1 = c1()[2], lng2 =  c1()[3], lat2 = c1()[4]) %>%
      addPolylines(
        data = boundary_liner(),
        fillColor = "",
        fillOpacity = 0,
        color = "black",
        weight = 8) %>%
      addPolygons(
        data = insider(),
        group = "Catchments",
        fillColor = insider()$col,
        fillOpacity = 1,
        color = "black",
        weight = 1,
        popup = paste0("Status: ", insider()$Assessment_Category,
                       "<br>",
                       "State ID(s): ", insider()$Assessment_Unit,
                       "<br>",
                       "Impairments: ", ifelse(is.na(insider()$Impairments), "None", insider()$Impairments))) %>%
      addPolygons(
        data = outsider(),
        fillColor = outsider()$col,
        group = "Catchments",
        fillOpacity = 0.15,
        color = "black",
        weight = 1,
        popup = paste0("Status: ", outsider()$Assessment_Category,
                       "<br>",
                       "State ID(s): ", outsider()$Assessment_Unit,
                       "<br>",
                       "Impairments: ", ifelse(is.na(outsider()$Impairments), "None", outsider()$Impairments)))
    
  })
  
  
}

## Run the app. 
shinyApp(ui = ui, server = server)
