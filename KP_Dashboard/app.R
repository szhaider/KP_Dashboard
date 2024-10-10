#Loading the required libraries
library(shiny)
library(dplyr)
library(stringr)
library(glue)
library(ggplot2)
library(leaflet)
library(leaflet.minicharts, warn.conflicts = FALSE)
library(sf)
library(janitor)
library(shinycssloaders)
library(shinythemes)
library(shinyscreenshot)
library(leafsync)
library(zip)
library(forcats)
library(scales)
library(tidyr)
library(purrr)
library(broom)
library(DT)
# remotes::install_github("EBukin/devPTIpack")
library(devPTIpack)

theme_set(theme_light())

###############
#Reading in PTI DATA

# Shapes files should be loaded with readRDS()

pti_shps <-  readRDS("data/pak_geometries.rds") 

pti_mtdt <-  rio::import_list("data/kp_mtdt_full_orig_data.xlsx")

###########
#Reading in combined data of accessibility and natural hazards
data <- readRDS("data/data.RDS") %>% 
  as_tibble() %>% 
  mutate(indicator_v2 = str_replace_all(indicator_v2, "_", " "),
         component = str_to_title(component),
         polygon = str_to_title(polygon),
         unit = str_to_title(unit),
         source = str_to_title(source),
         description = str_to_title(description)) 

#Reading in combined shape file of tehsils and districts
pak_shp_comb <- readRDS("data/pak_shp_comb.RDS")

#User Interface
ui <- navbarPage(tags$strong(tags$em("Khyber Pakhtunkhwa")),
                 tabPanel("INTERACTIVE MAPS",     
                          bootstrapPage(theme = shinytheme("flatly")),
                          
                          tags$style(type = 'text/css', '#map {height: calc(90vh - 80px) !important;}', style= 'padding-top:0px;'),
                          leafletOutput("map") %>% 
                            withSpinner(),
                          br(),
                          tags$head(tags$style("#source{color:black; font-size:12px; font-style:italic; max-height: 110px; background: #ffe6cc; }")),
                          verbatimTextOutput("source"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default", fixed= TRUE,
                                        draggable = TRUE, bottom = "auto", right = "auto", left = 70, top = 95,
                                        width = 260, height = "auto",
                                        style = "background-color: white;
                                                   opacity: 0.85;
                                                   padding: 20px 20px 20px 20px;
                                                   margin: auto;
                                                   border-radius: 5pt;
                                                   box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                                   padding-bottom: 2mm;
                                                   padding-top: 1mm;",
                                        # br(),
                                        selectInput("polygon",
                                                    "Choose Spatial Level",
                                                    choices = unique(data$polygon),
                                                    selected = "District"),
                                        selectInput("component",
                                                    "Choose Component",
                                                    choices = unique(data$component),
                                                    selected = "Accessibility"),
                                        
                                        selectInput("stat_map",
                                                    "Choose Indicator",
                                                    choices = unique(data$indicator_v2),
                                                    selected = unique(data$indicator_v2)[3]),
                                        h6(tags$b(tags$em("Use the buttons below to download the data underlying the current on-screen map and screenshot"))),
                                        downloadButton("mapdata", "Get Data", class = "btn-sm"),
                                        actionButton("screenshot", "Screeenshot", class= "btn-sm", icon = icon("camera")),
                                        br()
                          )
                 ),
                 ####PTI UI
                 tabPanel("PTI",  
                          devPTIpack::mod_ptipage_twocol_ui(
                            id = "pti_mod", 
                            map_height = "calc(90vh)", 
                            side_width = "350px", 
                            wt_style = "zoom:1;", 
                            show_waiter = FALSE
                          )
                 ),
                 tabPanel("COMPARISON MAPS",
                          mainPanel(
                            width = 12,
                            fluidRow(
                              column(width = 6,
                                     offset = 0,
                                     style = 
                                       'padding-bottom:0px; 
                                       padding-left:0px; 
                                       padding-right:0px; 
                                       margin-left:-10px; 
                                       position: relative;',
                                     tags$style(type = 'text/css', '#double_map_1 {height: calc(98vh - 50px) !important;}'),
                                     leafletOutput("double_map_1", width = "100%", height = "400px"),
                                     absolutePanel(id = "controls", class = "panel panel-default", fixed= FALSE,
                                                   draggable = TRUE, bottom = "auto", right = "auto", left = 40, top = 50,
                                                   width = 280, height = "auto",
                                                   style = "background-color: white;
                                                   opacity: 0.85;
                                                   padding: 20px 20px 20px 20px;
                                                   margin: auto;
                                                   border-radius: 5pt;
                                                   box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                                   padding-bottom: 2mm;
                                                   padding-top: 1mm;",
                                                   br(),
                                                   selectInput("polygon1", 
                                                               "Choose Spatial Level",
                                                               choices = unique(data$polygon),
                                                               selected = "District"),
                                                   selectInput("component_c1",
                                                               "Select component for Map 1",
                                                               choices = unique(data$component)),
                                                   selectInput("stat_c1",
                                                               "Indicator for Map 1",
                                                               choices = unique(data$indicator_v2),
                                                               selected = unique(data$indicator_v2)[3])
                                     )
                              ),
                              column(width = 6,
                                     offset = 0,
                                     style = 
                                       'padding-bottom:0px; 
                                       padding-left:2px; 
                                       padding-right:2px; 
                                       margin-left:0px;
                                       margin-right:5px;
                                       position: relative;',
                                     tags$style(type = 'text/css', '#double_map_2 {height: calc(98vh - 50px) !important;}'),
                                     leafletOutput("double_map_2", width = "100%", height = "400px"),
                                     absolutePanel(id = "controls", class = "panel panel-default", fixed= FALSE,
                                                   draggable = TRUE, bottom = "auto", right = "auto", left = 120, top = 50,
                                                   width = 280, height = "auto",
                                                   style = "background-color: white;
                                                   opacity: 0.85;
                                                   padding: 20px 20px 20px 20px;
                                                   margin: auto;
                                                   border-radius: 5pt;
                                                   box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                                   padding-bottom: 2mm;
                                                   padding-top: 1mm;",
                                                   br(),
                                                   selectInput("polygon2", 
                                                               "Choose Spatial Level",
                                                               choices = unique(data$polygon),
                                                               selected = "District"),
                                                   selectInput("component_c2",
                                                               "Select component for Map 2",
                                                               choices = unique(data$component)),
                                                   selectInput("stat_c2",
                                                               "Indicator for Map 2",
                                                               choices = unique(data$indicator_v2),
                                                               selected = unique(data$indicator_v2)[3]),
                                     )
                              )
                            )
                          )
                 ),
                 tabPanel("GRAPHS",
                          tabsetPanel(
                            tabPanel("BAR CHARTS",
                                     sidebarLayout(
                                       sidebarPanel(width= 3,
                                                    style = "background-color: white;
                                opacity: 0.85;
                                padding: 20px 20px 20px 20px;
                                margin: auto;
                                border-radius: 5pt;
                                box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                padding-bottom: 2mm;
                                padding-top: 1mm;",
                                                    selectInput("polygon_bar", 
                                                                "Choose Spatial Level",
                                                                choices = unique(data$polygon),
                                                                selected = "District"),
                                                    conditionalPanel(condition = "input.polygon_bar == 'Tehsil'",
                                                                     selectInput("division_bar",
                                                                                 "Choose from Divisions",
                                                                                 choices = unique(data$division))),
                                                    selectInput("domain_bar",
                                                                "Select Component",
                                                                choices = unique(data$component)),
                                                    selectInput("stat_bar",
                                                                "Indicator",
                                                                choices = unique(data$indicator_v2)),
                                                    br(),
                                                    actionButton("screenshot_bar", "Download", class= "btn-sm", icon = icon("download")),
                                                    br()
                                       ),
                                       
                                       mainPanel(
                                         plotOutput("plot_bar",height = 390, width = 870),
                                         # br(),
                                         verbatimTextOutput("source_bar"),
                                         tags$head(tags$style("#source_bar{color:black; font-size:12px; font-style:italic; max-height: 120px; background: #ffe6cc;;}"))
                                       )
                                     )
                            ),
                            tabPanel("SCATTERPLOTS",
                                     sidebarLayout(
                                       sidebarPanel(width=3,
                                                    style = "background-color: white;
                                               opacity: 0.85;
                                               padding: 20px 20px 20px 20px;
                                               margin: auto;
                                               border-radius: 5pt;
                                               box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                               padding-bottom: 2mm;
                                               padding-top: 1mm;",
                                                    selectInput("polygon_sc",
                                                                "Spatial Level",
                                                                choices = unique(data$polygon)),
                                                    selectInput("domain_scat1",
                                                                "Select Component",
                                                                choices = unique(data$component)),
                                                    selectInput("stat_scat1",
                                                                "Indicator 1",
                                                                choices = unique(data$indicator_v2)),
                                                    selectInput("domain_scat2",
                                                                "Select Component",
                                                                choices = unique(data$component)),
                                                    selectInput("stat_scat2",
                                                                "Indicator 2",
                                                                choices = unique(data$indicator_v2)),
                                                    downloadButton("downloadscatter", "Download Plot", type= "default", class="btn-sm")  #, class = "btn-success"
                                                    
                                       ),
                                       mainPanel(
                                         plotOutput("plot_scat", width =860 , height = 400),
                                         # br(),
                                         verbatimTextOutput("source_scat1"),
                                         tags$head(tags$style("#source_scat1{color:black; font-size:12px; font-style:italic; max-height: 120px; background: #ffe6cc;;}")),
                                         
                                         verbatimTextOutput("source_scat2"),
                                         tags$head(tags$style("#source_scat2{color:black; font-size:12px; font-style:italic; max-height: 120px; background: #ffe6cc;;}")),
                                         br(),
                                         # h4(tags$strong(tags$em("Correlation Summary")), align= "center"),
                                         tags$head(
                                           tags$style(HTML("pre {
        color: white;
        background-color: white;
      }
      /* this will affect only the pre elements under the class myclass */
      .myclass pre {
        color: black;
        background-color: white;
        font-weight: bolder;
      }"))),
                                         div(class = "myclass",
                                             verbatimTextOutput("correlation"),
                                             verbatimTextOutput("hypotheses"),
                                         )
                                         # br(),
                                         
                                       )
                                     )
                            )
                          )),
                 tabPanel("TABLES",
                          sidebarLayout(
                            sidebarPanel(width =3,
                                         style = "background-color: white;
                                               opacity: 0.85;  
                                               padding: 20px 20px 20px 20px;
                                               margin: auto;
                                               border-radius: 5pt;
                                               box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                               padding-bottom: 2mm;
                                               padding-top: 1mm;",
                                         selectInput("polygon_tab", 
                                                     "Choose Spatial Level",
                                                     choices = unique(data$polygon),
                                                     selected = "District"),
                                         selectInput("component_tab",
                                                     "Choose Component",
                                                     choices = unique(data$component)),
                                         
                                         selectInput("stat_tab",
                                                     "Choose Indicator",
                                                     choices = unique(data$indicator_v2),
                                                     selected = unique(data$indicator_v2)[3]),
                                         verbatimTextOutput("source_table"),
                                         tags$head(tags$style("#source_table{color:black; font-size:12px; font-style:italic; 
               overflow-y:scroll; max-height: 120px; background: white;}")),
                                         br(),
                                         downloadButton("downloadtable",
                                                        "Download Table",
                                                        type= "default", 
                                                        class="btn-sm")  #, class = "btn-success"
                                         
                            ),
                            mainPanel(width= 9,
                                      dataTableOutput("table"))
                          )       
                 ),
                 tabPanel("DOCUMENTATION",
                          mainPanel(
                            br(),
                            h4(strong("Collaboration between Poverty and Equity GP, Transport GP and the Social Development and Inclusion GP")),
                            br(),
                            h4(strong("This Application covers District and Tehsil level indicators for spatial work in Khyber Pakhtunkhwa, Pakistan")),
                            br(),
                            h4(strong("The Application may be used to assess the spatial variations in various Districts and Tehsils of Khyber Pakhtunkhwa")),
                            tags$hr(),
                            fluidRow(downloadButton("bulkdownload", "Download Complete Dataset",
                                                    type= "default", class="btn-sm"),  # class = "btn-success"
                                     downloadButton("shapefile_district", "Download KP District Shapefiles",
                                                    type= "default", class="btn-sm"),# class = "btn-success"
                                     downloadButton("shapefile_tehsil", "Download KP Tehsil Shapefiles",
                                                    type= "default", class="btn-sm"), # class= "btn-success"
                                     hr(),
                                     h4(strong("Data Repository")),
                                     br(),
                                     tags$a(href= "https://github.com/haider-zeeshan51214/KP_Dashboards", "Repo Link", target="_blank"), br(),
                                     div(tags$em(span("Source code will be avaialble on Github repo")), style="color:red"),
                            )
                          )),
                 
                 tabPanel("ABOUT",
                          mainPanel(
                            br(),
                            tags$hr(),
                            tags$h4(strong("Contributors/Developers")),
                            "Moritz Meyer, Senior Economist, Poverty & Equity GP - World Bank",
                            tags$br(),
                            tags$br(),
                            "Sina Johanna Smid, Consultant, Poverty & Equity GP - World Bank",
                            tags$br(),
                            tags$br(),
                            "Lander Bosch, Young Professional, Poverty & Equity GP - World Bank",
                            tags$br(),
                            tags$br(),
                            "Robert Steven Banick, Consultant, Poverty & Equity GP - World Bank",
                            tags$br(),
                            tags$br(),
                            "Zeeshan Haider, Consultant, Poverty & Equity GP - World Bank",
                            tags$br(),
                            hr(),
                            br()
                            
                          )
                 )
)

#Server
server <- function(input, output, session) {
  
  ##
  ### HERE THE PTI IS ADDED
  ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ###
  ### ### ### ### ### ### ### ###
  mod_ptipage_newsrv(
    id = "pti_mod",
    inp_dta = reactive(pti_mtdt),
    shp_dta = reactive(pti_shps), 
    show_waiter = FALSE
  )
  
  #Natural Hazards Map
  #data filtering for the main map
  
  d_m1 <-  reactive({
    data %>% 
      # mutate(component = str_to_title(component)) %>% 
      filter(input$polygon == polygon)
  })
  
  
  d_mm <- reactive({
    data %>%
      mutate(polygon = str_to_title(polygon)) %>%
      filter(input$polygon_tab == polygon) %>%
      select(polygon) %>%
      distinct(polygon) %>%
      pull(polygon)
  })
  
  observeEvent(d_m1(),{
    if(d_mm() == "Tehsil"){
      choices_dm <- d_m1() %>% 
        distinct(component) %>%
        as_tibble() %>% 
        filter(component != c("Household Welfare"))
    }else{
      choices_dm <- unique(d_m1()$component)
    }
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "component",
      choices = choices_dm)
  })
  
  d_m2 <-  reactive({
    d_m1() %>% 
      filter(input$component == component)
  })
  
  #Updating indicators based on component
  #Updating natural hazards component based on tehsil or ditrict spatial level
  #If district selected, Tehsil population and area vanishes and vice versa
  
  observeEvent(d_m2(),{
    choices <- unique(d_m2()$indicator_v2) 
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "stat_map",
      choices = choices)
  })
  
  
  d1 <- reactive({
    d_m2() %>%
      filter(input$stat_map == indicator_v2)
  })
  
  
  
  d2 <- reactive({
    pak_shp_comb[[input$polygon]]
  })
  
  
  #Value based map   
  #Lealfet
  output$map <- renderLeaflet({
    # message("rendering map")
    leaflet(options = leafletOptions(zoomSnap = 0.20, zoomDelta = 0.20)) %>%
      addProviderTiles(providers$CartoDB, group = "CARTO") %>%
      setView(lng=71, lat = 34.2, zoom = 6)
  })
  
  observe({
    #Labelling for the Map       
    labels <- reactive({
      paste0(glue::glue("<b>{ str_to_title(unique(d1()$polygon)) }</b>: { d2()$dis_teh } </br>"), glue::glue("<b> { d1()$indicator_v2 }: </b>"), " ", glue::glue("{ round(d1()$value, 2) }</br>"),"<b>Unit: </b>", str_to_title(glue("{ unique(d1()$unit)  }")), sep = "") %>%
        lapply(htmltools::HTML)
    })
    
    pal <- reactive({
      colorBin(palette = c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6'), 
               bins=7,
               na.color = "grey",  
               domain= d1()$value, reverse=T)
    })
    
    leafletProxy("map", 
                 data= st_transform(d2(), crs=4326)) %>% 
      addPolygons(label= labels(),
                  labelOptions = labelOptions(
                    style = list("font-weight"= "normal",   
                                 padding= "3px 8px",
                                 "color"= "#cc4c02"), 
                    textsize= "15px",
                    direction = "auto"
                  ),
                  fillColor =  ~pal()(d1()$value),
                  fillOpacity = 0.9,
                  stroke = TRUE,
                  color= "white",
                  weight = 1,
                  opacity = 0.7,
                  fill = TRUE,
                  dashArray = NULL,
                  smoothFactor = 0.5,
                  highlightOptions = highlightOptions(weight= 5,
                                                      fillOpacity = 1,
                                                      opacity= 1,
                                                      bringToFront = TRUE), 
                  group = "Polygons") %>%
            addMeasure() %>% 
      addScaleBar("bottomright")
    
    leafletProxy("map", data= subset(st_transform(d2(), crs=4326))
    ) %>% 
      clearControls() %>%
      addLegend("bottomright",
                pal= pal(),
                values= ~d1()$value,
                title = str_to_title(glue("{ unique(d1()$unit)  }")),
                opacity= 1,
                labFormat = labelFormat(
                  between = "  :  ",
                  digits = 2))
  })
  
 
  #Source:
  output$source <- renderText({
    paste(" Source: ", unique(d1()$source), "\n", "Description:", unique(d1()$description))
  })
  
  #Main Map screenshot
  observeEvent(input$screenshot,{
    screenshot(filename = glue("{ input$stat_map }", " ", "map_screenshot"), selector = "#map", scale = 0.8, timer = 1)
    
  })
  
  #Main map current on-screen data download
  output$mapdata <- downloadHandler(
    filename = function(){
      paste(glue("{ input$stat_map }"), ".csv")
    },
    content = function(file){
      write.csv(d1(), file)
    }
  )
  
  
  #Double/comparison Map
  
  #Map1
  
  d_d1 <- reactive({
    data %>%
      # mutate(component = str_to_title(component)) %>% 
      filter(polygon == input$polygon1)
  })
  
  d_mmc1 <- reactive({
    data %>%
      mutate(polygon = str_to_title(polygon)) %>%
      filter(input$polygon1 == polygon) %>%
      select(polygon) %>%
      distinct(polygon) %>%
      pull(polygon)
  })
  
  
  observeEvent(d_d1(),{
    if(d_mmc1() == "Tehsil"){
      choices_dmc1 <- d_d1() %>% 
        distinct(component) %>%
        # pull(component) %>% 
        as_tibble() %>% 
        filter(component != c("Household Welfare")) %>% 
        pull(component)
    }else{
      choices_dmc1 <- unique(d_d1()$component)
    }
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "component_c1",
      choices = choices_dmc1)
  })
  
  #
  d_d2 <- reactive({
    d_d1() %>%
      # mutate(component = str_to_title(component)) %>% 
      filter(component == input$component_c1)
  })
  
  #Updating indicators based on component
  
  # observeEvent(d_mm1(),{
  observeEvent(d_d2(),{
    choices_c1 <- unique(d_d2()$indicator_v2) 
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "stat_c1",
      choices = choices_c1)
  })
  
  
  
  #Selected indicator for the first map
  ##
  d_c1 <- reactive({
    d_d2() %>%
      filter(input$stat_c1 == indicator_v2)
  })
  
  d_c2 <- reactive({
    pak_shp_comb[[input$polygon1]]
  })
  #
  # #Labelling for the Map  1
  
  #Lealfet
  output$double_map_1 <- renderLeaflet({
    leaflet(options = leafletOptions(zoomSnap = 0.20, zoomDelta = 0.20)) %>%
      addProviderTiles(providers$CartoDB, group = "CARTO") %>%
      setView(lng=72.7, lat = 34, zoom = 6)  %>% 
      syncWith("combined_map")
  })
  
  #To render leafelt map before proxy observer updates
  outputOptions(output, "double_map_1", suspendWhenHidden = FALSE)
  
  labels_c1 <- reactive({
    paste0(glue::glue("<b>{ str_to_title(unique(d_c2()$polygon)) }</b>: { d_c2()$dis_teh } </br>"), glue::glue("<b> Indicator 1: </b>"), " ", glue::glue("{ round(d_c1()$value, 2) }</br>"), sep = "") %>%
      lapply(htmltools::HTML)
  })
  #
  
  pal_c1 <- reactive({
    colorBin(palette = c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6'), bins=7, na.color = "grey",  domain= d_c1()$value, reverse=T)
  })

  observe({
    req(input$stat_c1)
    leafletProxy("double_map_1", 
                 data = st_transform(d_c2(), crs=4326)) %>% 
      addPolygons(label= labels_c1(),
                  labelOptions = labelOptions(
                    style = list("font-weight"= "normal",
                                 padding= "3px 8px",
                                 "color"= "#cc4c02"),
                    textsize= "15px",
                    direction = "auto"
                  ),
                  fillColor =  ~pal_c1()(d_c1()$value),
                  fillOpacity = 0.9,
                  stroke = TRUE,
                  color= "white",
                  weight = 1,
                  opacity = 0.5,
                  fill = TRUE,
                  dashArray = NULL,
                  smoothFactor = 0.5,
                  highlightOptions = highlightOptions(weight= 5,
                                                      fillOpacity = 1,
                                                      opacity= 1,
                                                      bringToFront = TRUE),
                  group = "Polygons")
    
    leafletProxy("double_map_1", 
                 data= st_transform(d_c2(), 
                                    crs=4326)) %>%
      clearControls() %>%
      addLegend("bottomright",
                pal= pal_c1(),
                values= ~d_c1()$value,
                title = "Legend",
                opacity= 1,
                labFormat = labelFormat(
                  between = "  :  ",
                  digits = 2))

  })
  
  #Source map1
  output$source_c1 <- renderText({
    paste(" Source: ",  unique(d_c1()$source),"\n"  ,"Unit: ", unique(d_c1()$unit),
          "\n", "Description:", unique(d_c1()$description))
    
  })
  
  #Comparison Maps
  # #Map2
  #
  # #Selected domain for the second map
  
  d_d3 <- reactive({
    data %>%
      # mutate(component = str_to_title(component)) %>% 
      filter(polygon == input$polygon2)
  })
  
  d_mmc2 <- reactive({
    data %>%
      mutate(polygon = str_to_title(polygon)) %>%
      filter(input$polygon2 == polygon) %>%
      select(polygon) %>%
      distinct(polygon) %>%
      pull(polygon)
  })
  
  observeEvent(d_d3(),{
    if(d_mmc2() == "Tehsil"){
      choices_dmc2 <- d_d3() %>% 
        distinct(component) %>%
        as_tibble() %>% 
        filter(component != c("Household Welfare")) %>% 
        pull(component)
    }else{
      choices_dmc2 <- unique(d_d3()$component)
    }
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "component_c2",
      choices = choices_dmc2)
  })
  
  d_d4 <- reactive({
    d_d3() %>%
      filter(component == input$component_c2)
  })
  
  # #Selected indicator for the first map
  observeEvent(d_d4(),{
    choices_c2 <- unique(d_d4()$indicator_v2)
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "stat_c2",
      choices = choices_c2)
  })
  
  d_c3 <- reactive({
    d_d4() %>%
      filter(input$stat_c2 == indicator_v2)
  })
  
  d_c4 <- reactive({
    pak_shp_comb[[input$polygon2]]
  })
  # #Labelling for the Map  2
  
  labels_c2 <- reactive({
    paste0(glue::glue("<b>{ str_to_title(unique(d_c2()$polygon)) }</b>: { d_c2()$dis_teh } </br>"), glue::glue("<b> Indicator 2: </b>"), " ", glue::glue("{ round(d_c3()$value, 2) }</br>") ,sep = "") %>%
      lapply(htmltools::HTML)
  })

  #Lealfet
  output$double_map_2 <- renderLeaflet({
    leaflet(options = leafletOptions(zoomSnap = 0.20, zoomDelta = 0.20)) %>%
      addProviderTiles(providers$CartoDB, group = "CARTO") %>%
      setView(lng=72.7, lat = 34, zoom = 6)  %>% 
      syncWith("combined_map")
  })
  
  #To render leafelt map before proxy observer updates
  outputOptions(output, "double_map_2", suspendWhenHidden = FALSE)
  
  pal_c2 <- reactive({
    colorBin(palette = c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6'), 
             bins=7, na.color = "grey",  
             domain= d_c3()$value, reverse=T)
  })

  
  observe({
    req(input$stat_c2) 
    leafletProxy("double_map_2", 
                 data = st_transform(d_c4(), 
                                     crs=4326)) %>% 
      clearShapes() %>% 
      addPolygons(label= labels_c2(),
                  labelOptions = labelOptions(
                    style = list("font-weight"= "normal",
                                 padding= "3px 8px",
                                 "color"= "#cc4c02"),
                    textsize= "15px",
                    direction = "auto"
                  ),
                  fillColor =  ~pal_c2()(d_c3()$value),
                  fillOpacity = 0.9,
                  stroke = TRUE,
                  color= "white",
                  weight = 1,
                  opacity = 0.5,
                  fill = TRUE,
                  dashArray = NULL,
                  smoothFactor = 0.5,
                  highlightOptions = highlightOptions(weight= 5,
                                                      fillOpacity = 1,
                                                      opacity= 1,
                                                      bringToFront = TRUE),
                  group = "Polygons") 
    
    leafletProxy("double_map_2", 
                 data= st_transform(d_c4(), 
                                    crs=4326)) %>%
      clearControls() %>%
      addLegend("bottomright",
                pal= pal_c2(),
                values= ~d_c3()$value,
                title = "Legend 2",
                opacity= 1,
                labFormat = labelFormat(
                  between = "  :  ",
                  digits = 2))
  })
  
  #Source map1
  output$source_c2 <- renderText({
    paste(" Source: ",  unique(d_c3()$source),"\n"  ,"Unit: ", unique(d_c3()$unit),
          "\n", "Description: ", unique(d_c3()$description))
  })
  
  #
  # #Rendering synced maps in comparison MAPS
  output$double_map <- renderUI({
    sync(map_c1(), map_c2(), ncol=2, no.initial.sync = T)
  })
  
  #Combined dataset on both maps
  combined_comp_data <- reactive({
    d_c1() %>%
      bind_rows(d_c3())
  })
  #
  # #Download data underlying the both maps
  output$mapdata_c <- downloadHandler(
    filename = function(){
      paste(glue("{input$stat_c1}"), "_", glue("{input$stat_c2}"), ".csv")
    },
    content = function(file){
      write.csv(combined_comp_data(), file)
    }
  )
  
  #
  # #Double Map screen shot
  observeEvent(input$screenshot_c,{
    screenshot(filename = glue("{ input$stat_c1 }", "{ input$stat_c2 }", " ", "map_screenshot"), selector = "#double_map", timer = 1)
  })
  ####
  # 
  # #Graphs
  # 
  # #Bar charts
  # 
  # #Select Domain
  gr_0 <- reactive({
    data %>%
      filter(polygon == input$polygon_bar)
  })
  
  d_mmbar <- reactive({
    data %>%
      mutate(polygon = str_to_title(polygon)) %>%
      filter(input$polygon_bar == polygon) %>%
      select(polygon) %>%
      distinct(polygon) %>%
      pull(polygon)
  })
  
  observeEvent(gr_0(),{
    if(d_mmbar() == "Tehsil"){
      choices_dmbar <- gr_0() %>% 
        distinct(component) %>%
        as_tibble() %>% 
        filter(component != c("Household Welfare")) %>% 
        pull(component)
    }else{
      choices_dmbar <- unique(gr_0()$component) 
    }
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "domain_bar",
      choices = choices_dmbar) 
  })
  
  
  gr_00 <- reactive({
    gr_0() %>%
      filter(component == input$domain_bar)
  })
  
  # observeEvent(gr_0(),{
  observeEvent(gr_00(),{
    choices_bar <- unique(gr_00()$indicator_v2) 
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "stat_bar",
      choices = choices_bar)
  })
  
  
  gr_d <- reactive({
    gr_00() %>%
      filter(indicator_v2 == input$stat_bar, 
             polygon == "District") %>%
      filter(!is.na(value)) %>%
      # mutate(.data[[str_to_lower(input$polygon_bar)]] = fct_reorder(.data[[str_to_lower(input$polygon_bar)]], value)) %>%
      mutate(district = fct_reorder(district, value)) %>%
      ggplot(aes(district, value)) +
      geom_col(na.rm = T, color="white") +
      geom_text(aes(label = round(value,1)),
                alpha= 0.9, size=3.2,
                vjust=0.5, hjust = 1,
                nudge_x= 0.1,
                color="white",
                check_overlap = T)+
      expand_limits(y=0)+
      labs(x="",
           y= input$stat_bar)+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank())+
      scale_y_continuous(labels = label_comma())+
      coord_flip()
    
  })
  
  
  gr_t <- reactive({
    gr_00() %>%
      filter(indicator_v2 == input$stat_bar,
             division == input$division_bar) %>%
      filter(!is.na(value),
             !is.na(tehsil)) %>%
      mutate(tehsil = fct_reorder(tehsil, value)) %>%
      ggplot(aes(tehsil, value)) +
      geom_col(na.rm = T, color="white") +
      geom_text(aes(label = round(value,1)),
                alpha= 0.9, size=3.2,
                vjust=0.5, hjust = 1,
                nudge_x= 0.09,
                color="white",
                check_overlap = T)+
      expand_limits(y=0)+
      labs(x="",
           y= input$stat_bar)+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank())+
      scale_y_continuous(labels = label_comma())+
      coord_flip()
    
  })
  # 
  output$plot_bar <- renderPlot({
    if(input$polygon_bar == "District"){
      gr_d()
    }else{
      gr_t()
    }
  })
  # 
  #Source barcharts
  output$source_bar <- renderText({
    if(input$polygon_bar == "District"){
      s_bard <- reactive({
        gr_00() %>% 
          # filter(input$stat_bar == indicator_v2)       
          
          #     data %>%
          filter(indicator_v2 == input$stat_bar, 
                 polygon == "District")
      })
      paste(" Source: ",  unique(s_bard()$source),"\n"  ,
            "Unit: ", unique(s_bard()$unit), "\n", 
            "Description: ", unique(s_bard()$description))
    }else{
      s_bart <- reactive({
        #     data %>%
        gr_00() %>% 
          filter(indicator_v2 == input$stat_bar,
                 division == input$division_bar,
                 polygon == "Tehsil")
      })
      paste("Source: ",  unique(s_bart()$source),"\n"  ,"Unit: ", unique(s_bart()$unit),
            "\n", "Description: ", unique(s_bart()$description))
      
    }
  })
  # #Download bar charts
  observeEvent(input$screenshot_bar,{
    screenshot(filename = glue("{ input$stat_bar }", " ", ".png"), selector = "#plot_bar", scale = 0.8, timer = 1)
    
  })
  
  sc_0 <- reactive({
    data %>%
      filter(polygon == input$polygon_sc)
  })
  
  d_mmsc <- reactive({
    data %>%
      mutate(polygon = str_to_title(polygon)) %>%
      filter(input$polygon_sc == polygon) %>%
      select(polygon) %>%
      distinct(polygon) %>%
      pull(polygon)
  })
  
  observeEvent(sc_0(),{
    if(d_mmsc() == "Tehsil"){
      choices_dmsc <- sc_0() %>% 
        distinct(component) %>%
        as_tibble() %>% 
        filter(component != c("Household Welfare")) %>% 
        pull(component)
    }else{
      choices_dmsc <- unique(sc_0()$component) 
    }
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "domain_scat1",
      choices = choices_dmsc) 
  })
  
  # #Select domain 1 
  sc_1 <- reactive({
    sc_0() %>%
      filter(component == input$domain_scat1)
  })
 
  # Updating Indicators
  
  observeEvent(sc_1(),{
    req(input$polygon_sc)
    # if(d_mmc() == "District"){
    choices_scat1 <- unique(sc_1()$indicator_v2) 
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "stat_scat1",
      choices = choices_scat1)
  })
  

  observeEvent(sc_0(),{
    if(d_mmsc() == "Tehsil"){
      choices_dmsc <- sc_0() %>% 
        distinct(component) %>%
        as_tibble() %>% 
        filter(component != c("Household Welfare")) %>% 
        pull(component)
    }else{
      choices_dmsc <- unique(sc_0()$component) 
    }
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "domain_scat2",
      choices = choices_dmsc) 
  })
  
  # #Select Domain 2
  sc_2 <- reactive({
    sc_0() %>%
      filter(component == input$domain_scat2)
  })
  
  # observeEvent(sc_2(),{
  observeEvent(sc_2(),{
    req(input$polygon_sc)
    choices_scat2 <- unique(sc_2()$indicator_v2) 
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "stat_scat2",
      choices = choices_scat2)
  })
  gr_2 <- reactive({
    req(input$polygon_sc)
    data %>%
      filter(!is.na(value)) %>%
      filter(polygon == input$polygon_sc) %>% 
      filter(indicator_v2 %in% c(input$stat_scat1, input$stat_scat2)) %>%
      pivot_wider(names_from =  'indicator_v2', values_from = 'value', 
                  id_cols =  str_to_lower(input$polygon_sc)) %>%
      ggplot(aes(.data[[input$stat_scat1]],.data[[input$stat_scat2]])) +
      geom_point(na.rm = TRUE, size = 2.5) +
      geom_smooth(na.rm = TRUE, formula = 'y~x' , method = "lm") +
      geom_text(na.rm = TRUE,
                aes(label = .data[[str_to_lower(input$polygon_sc)]]), check_overlap = T, hjust=0.3, vjust= -0.5) +
      labs(x=input$stat_scat1,
           y= input$stat_scat2
      ) +
      expand_limits(y=0, x=0)+
      theme(panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_blank())
  })
  
  
  output$plot_scat <- renderPlot({
    gr_2()
  })
  # 
  # #Download scatterplot
  output$downloadscatter <- downloadHandler(
    filename = function(){
      paste0("plot_", glue("{ input$stat_scat1 }", "_", "{ input$stat_scat2 }"), ".png")
    },
    content = function(file){
      device <- function(..., width, height) grDevices::png(..., width = width, height = height, res = 300, units = "in")
      ggsave(file, gr_2(), device = device)
    }
    
  )

  output$source_scat1 <- renderText({
    s_scat1 <- reactive ({
      data %>%
        filter(
          polygon == input$polygon_sc,
          component == input$domain_scat1,
          indicator_v2 == input$stat_scat1) %>%
        distinct(source, unit, description)
    })
    
    paste("Source 1: ", s_scat1()$source , "\n",
          "Units: ", s_scat1()$unit,"\n",
          "Description: ", s_scat1()$description)
    
  })
  
  output$source_scat2 <- renderText({
    s_scat2 <- reactive({
      data %>% 
        filter(
          polygon == input$polygon_sc,
          component == input$domain_scat2,
          indicator_v2 == input$stat_scat2) %>% 
        distinct(source, unit, description)
    })
    paste("Source 2: ", s_scat2()$source, "\n",
          "Units: ", s_scat2()$unit,"\n", 
          "Description: ", s_scat2()$description)
  })
  

    #   
  # Tables server side
  # #Select Domain
  
  
  tb_0 <- reactive({
    data %>%
      filter(polygon == input$polygon_tab)
  })
  
  d_mmt <- reactive({
    data %>%
      mutate(polygon = str_to_title(polygon)) %>%
      filter(input$polygon_tab == polygon) %>%
      select(polygon) %>%
      distinct(polygon) %>%
      pull(polygon)
  })
  
  observeEvent(tb_0(),{
    if(d_mmt() == "Tehsil"){
      choices_dmtab <- tb_0() %>% 
        distinct(component) %>%
        as_tibble() %>% 
        filter(component != c("Household Welfare"))%>% 
        pull(component)
    }else{
      choices_dmtab <- unique(tb_0()$component)
    }
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "component_tab",
      choices = choices_dmtab)
  })
  
  tb_00 <- reactive({
    req(tb_0())
    tb_0() %>%
      filter(component == input$component_tab)
  })
  
  
  observeEvent(tb_00(),{
    choices_tab <- unique(tb_00()$indicator_v2)
    updateSelectInput(
      session = getDefaultReactiveDomain(),
      "stat_tab",
      choices = choices_tab)
  })
  
  tab <- reactive({
    if(d_mmt() == "District"){
      tb_00() %>% 
        filter(component == input$component_tab) %>%    
        filter(indicator_v2 %in% input$stat_tab) %>% 
        mutate(value = round(value, 2)) %>%  
        select(District = district,
               Indicator = indicator_v2,
               Value = value,
               Units = unit,
               -source,
               -division,
               -variable,
               -description,
               -component,
               -tehsil,
               -polygon)
    }
    else{
      tb_00() %>%
        filter(component == input$component_tab) %>%    
        filter(indicator_v2 %in% input$stat_tab) %>%
        mutate(value = round(value, 2)) %>%
        select(Tehsil = tehsil,
               Indicator = indicator_v2,
               Value = value,
               Units = unit,
               -source,
               -division,
               -variable,
               -description,
               -component,
               -polygon,
               -district)
    }
  })
  
  output$table <- renderDataTable({
    DT::datatable(tab(),
                  extensions = "Buttons",
                  options= list(pageLength=30,
                                lengthChange = FALSE,
                                dom = "Blfrtip",
                                buttons = c("copy", "csv", "excel", "pdf"))
    )
  })
  
  #Source table
  src_tab <- reactive({
    req(input$stat_tab)
    tb_0() %>% 
      filter(indicator_v2 == input$stat_tab) %>% 
      select(source)
  })   
  
  output$source_table <- renderText({
    paste0("Source : ",unique(src_tab()$source))
  })
  
  # Download table
  #Download national level table  
  output$downloadtable <- downloadHandler(
    filename = function(){
      paste0("table_", glue("{ input$polygon_tab }", "_", "{ input$stat_tab }"), ".csv")
    },
    content = function(file){
      write.csv(tab(), file)
    }
    
  )
  
  # #Bulk download natural hazards data
  output$bulkdownload <- downloadHandler(
    filename = function(){
      paste0("KP_dataset", ".csv")
    },
    content = function(file){
      write.csv(data, file)
    }
  )
  
  #Download shapefile in zip
  shp_dist <- reactive({
    pak_shp_comb[["District"]]
  })
  
  shp_teh <- reactive({
    pak_shp_comb[["Tehsil"]]
  })
  
  output$shapefile_district <- downloadHandler(
    filename = "kpdistrict.zip",
    content = function(file){
      if(length(Sys.glob("kp_district.*"))>0){
        file.remove(Sys.glob("kp_district.*"))
      }
      st_write(shp_dist(), dsn = "kp_district.shp", layer= "kp_district" ,driver= "ESRI Shapefile", overwrite_layer = T)
      zip(zipfile = 'kpdistrict.zip', files= Sys.glob("kp_district.*"))
      file.copy("kpdistrict.zip", file)
      if(length(Sys.glob("kp_district.*"))>0){
        file.remove(Sys.glob("kp_district.*"))
      }
    }
  )
  
  output$shapefile_tehsil <- downloadHandler(
    filename = "kptehsil.zip",
    content = function(file){
      if(length(Sys.glob("kp_tehsil.*"))>0){
        file.remove(Sys.glob("kp_tehsil.*"))
      }
      st_write(shp_teh(), dsn = "kp_tehsil.shp", layer= "kp_tehsil" ,driver= "ESRI Shapefile", overwrite_layer = T)

      zip(zipfile = 'kptehsil.zip', files= Sys.glob("kp_tehsil.*"))
      file.copy("kptehsil.zip", file)
      if(length(Sys.glob("kp_tehsil.*"))>0){
        file.remove(Sys.glob("kp_tehsil.*"))
      }
    }
  )
  
  
  
  
}

#Run App
shinyApp(ui, server)