## NOTES
# MAP WHO REPORT
# LAYER SDS PEPFAR COP
# GET INTERN

## COVID-2019 interactive mapping tool
## Edward Parker, London School of Hygiene & Tropical Medicine (edward.parker@lshtm.ac.uk), last updated April 2020

## includes code adapted from the following sources:
# https://github.com/rstudio/shiny-examples/blob/master/087-crandash/
# https://rviews.rstudio.com/2019/10/09/building-interactive-world-maps-in-shiny/
# https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example

# update data with automated script
#source("jhu_data_full.R") # run locally to update numbers, but not live on Rstudio server (to avoid possible errors on auto-updates)
#source("ny_data_us.R") # run locally to update numbers, but not live on Rstudio server (to avoid possible errors on auto-updates)

# load required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")

# set mapping colour for each outbreak
covid_col = "#cc4c02"




# import data
data.who.kp     <-  read.csv("input_data/who_data.csv")
countries       <-  read.csv("input_data/countries_codes_and_coordinates.csv")
varnames        <- read.csv("input_data/raw_data/varnames.csv")
worldcountry    <- geojson_read("input_data/50m.geojson", what = "sp")


### DATA PROCESSING: COVID-19 ###
working.data <- data.who.kp %>%
  left_join(countries, by = "alpha3")

pop.names <- unique(working.data$pop)

indicator.names         <- varnames$name 
names(indicator.names)  <- varnames$label
  
cv_pal <- colorFactor("Oranges", domain = c(0,1), levels = c(0,1))
plot_map <- worldcountry[worldcountry$ADM0_A3 %in% data.who.kp$alpha3, ]

# create base map 
basemap = leaflet(plot_map) %>% 
  addTiles() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>%
  fitBounds(~25,25,~-29,-29) %>%
  addLegend("bottomright", pal = cv_pal, values = ~working.data$condoms,
            title = "<small>Included in NSP?</small>") 










### SHINY UI ###
ui <- bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             "WHO NSP Tracker", id="nav",
             
             tabPanel("Men who have Sex with Men",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("mymap.msm", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, left = 55, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        span(tags$i(h6("Indicator")), style="color:#045a8d"),
                                        selectInput("input.indicator.msm", label = "Indicator", width = 250, choices = names(indicator.names))

                          )
                      )
             ),             
             tabPanel("Sex Workers",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("mymap.sw", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, left = 55, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        span(tags$i(h6("Indicator")), style="color:#045a8d"),
                                        selectInput("input.indicator.sw", label = "Indicator", width = 250, choices = names(indicator.names))
                                        
                          )
                      )
             ),             
             tabPanel("Transgender People",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("mymap.tg", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, left = 55, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        span(tags$i(h6("Indicator")), style="color:#045a8d"),
                                        selectInput("input.indicator.tg", label = "Indicator", width = 250, choices = names(indicator.names))
                                        
                          )
                      )
             ),
             
             tabPanel("People who inject Drugs",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("mymap.pwid", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, left = 55, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        span(tags$i(h6("Indicator")), style="color:#045a8d"),
                                        selectInput("input.indicator.pwid", label = "Indicator", width = 250, choices = names(indicator.names))
                                        
                          )
                      )
             ),
             
             
             tabPanel("People in Prisons",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("mymap.pris", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, left = 55, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        span(tags$i(h6("Indicator")), style="color:#045a8d"),
                                        selectInput("input.indicator.pris", label = "Indicator", width = 250, choices = names(indicator.names))
                                        
                          )
                      )
             ),
             tabPanel("About this site",
                      tags$div(
                        tags$h4("Background [Under Construction]"), 
                        "This site is built around the results of a WHO report on Key Populations and AIDS National Strategic Plans in Africa:",tags$br(),
                        tags$a(href="https://www.who.int/hiv/pub/toolkits/key-population-report-afro/en/", "Focus on key populatins in national HIV strategic plans in the WHO African Region"),tags$br(),
                        tags$br(),tags$br(),tags$h4("Site Author"),
                        "Keletso Makofane, PhD Candidate in Social Network Epidemiology - Harvard University",tags$br(),
                        tags$br(),tags$br(),tags$h4("Contact"),
                        "kmakofane@g.harvard.edu",tags$br(),tags$br()
                      )
             )
             
  )          
)





### SHINY SERVER ###

server = function(input, output, session) {
  
  # ALL TABS

  reactive.working.data.msm = reactive({ working.data[working.data$pop == "msm", c(indicator.names[input$input.indicator.msm], "latitude", "longitude", "alpha3")] %>% rename(value = 1) 
  })
  
  reactive.working.data.sw = reactive({ working.data[working.data$pop ==  "sw", c(indicator.names[input$input.indicator.sw], "latitude", "longitude", "alpha3")] %>% rename(value = 1) 
  })
  
  reactive.working.data.pris = reactive({ working.data[working.data$pop == "pris", c(indicator.names[input$input.indicator.pris], "latitude", "longitude", "alpha3")] %>% rename(value = 1) 
  })
  
  reactive.working.data.pwid = reactive({ working.data[working.data$pop == "pwid", c(indicator.names[input$input.indicator.pwid], "latitude", "longitude", "alpha3")] %>% rename(value = 1) 
  })
  
  reactive.working.data.tg = reactive({ working.data[working.data$pop == "tg", c(indicator.names[input$input.indicator.tg], "latitude", "longitude", "alpha3")] %>% rename(value = 1) 
  })
  
  
  reactive_polygons = reactive({
    worldcountry[worldcountry$ADM0_A3 %in% reactive.working.data.msm()$alpha3, ]
  })
  
  
  
  
  # MSM
  output$mymap.msm <- renderLeaflet({ 
    basemap
  })
  
  observeEvent(reactive.working.data.msm(), {
    leafletProxy("mymap.msm") %>% 
      clearMarkers() %>%
      clearShapes() %>%
      addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal(reactive.working.data.msm()$value ))
 
  })
  
  # SW
  output$mymap.sw <- renderLeaflet({ 
    basemap
  })
  
  observeEvent(reactive.working.data.sw(), {
    leafletProxy("mymap.sw") %>% 
      clearMarkers() %>%
      clearShapes() %>%
      addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal(reactive.working.data.sw()$value ))
    
  })
  
  # tg
  output$mymap.tg <- renderLeaflet({ 
    basemap
  })
  
  observeEvent(reactive.working.data.tg(), {
    leafletProxy("mymap.tg") %>% 
      clearMarkers() %>%
      clearShapes() %>%
      addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal(reactive.working.data.tg()$value ))
    
  })
  
  # pris
  output$mymap.pris <- renderLeaflet({ 
    basemap
  })
  
  observeEvent(reactive.working.data.pris(), {
    leafletProxy("mymap.pris") %>% 
      clearMarkers() %>%
      clearShapes() %>%
      addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal(reactive.working.data.pris()$value ))
    
  })
  
  # pwid
  output$mymap.pwid <- renderLeaflet({ 
    basemap
  })
  
  observeEvent(reactive.working.data.pwid(), {
    leafletProxy("mymap.pwid") %>% 
      clearMarkers() %>%
      clearShapes() %>%
      addPolygons(data = reactive_polygons(), stroke = FALSE, smoothFactor = 0.1, fillOpacity = 0.15, fillColor = ~cv_pal(reactive.working.data.pwid()$value ))
    
  })
  
  
 
  
  
  }

#runApp(shinyApp(ui, server), launch.browser = TRUE)
shinyApp(ui, server)
#library(rsconnect)
#deployApp(account="vac-lshtm")
