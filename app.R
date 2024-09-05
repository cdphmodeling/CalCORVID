## ***************************
##
## Script name: app.R
##
## Purpose of script: Contains UI and server functions to run RShiny dashboard.
##
## Author: Phoebe Lu, MPH
##
## Date Created: 2024-01-25
##
## Copyright (c) California Department of Public Health (CDPH)
## Email: Phoebe.Lu@cdph.ca.gov
##
## ***************************

#Need to source global.R file prior to running dashboard
source("global.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
  shinyjs::useShinyjs(),
  # Sidebar with a slider input for number of bins
  #Changed from tabsetPanel to navbarPage
  navbarPage(id="Daily",
             #Set theme of dashboard
             theme = shinythemes::shinytheme("cosmo"),
             inverse=T,
             title="CalCORVID",
             tabPanel(title="Home",
                      align="center",
                      HTML("<h3><b>Welcome to the California Clustering for Operational Real-time Visualization of Infectious Diseases Dashboard!</b></h3>"),
                      HTML("<h4 style='background-color:#FFFACD;'><i>Note: This dashboard displays sample results from running the prospective space-time analysis on California vaccination data.</i></h4>"),
                      HTML("<h4>Select an option below.</h4>"),
                      hr(),
                      fluidRow(column(6, 
                                      div(style='padding:15px;',
                                          img(id="results_image",src="results.PNG", height="300px", style="cursor:pointer;", align="center")),
                                      h4(strong("Results", align="center")),
                                      "Click image above for results."),
                               column(6,
                                      div(style='padding:15px;',
                                          img(id="technotes_image", src="technotes.PNG", height="300px", style="cursor:pointer;", align="center")),
                                      h4(strong("Technical Notes", align="center")),
                                      "Click image above for technical notes."),
                      ),
                      hr(),
                      HTML('<center><h5>Version 1.0|Released September 3, 2024|Compatible with SaTScan v10.2.1
                           </h5></center>'),
                      HTML('<center><h5>Contact: <a href="mailto:modeling@cdph.ca.gov">modeling@cdph.ca.gov</a>
                           </h5></center>'),
                      HTML('<center><p>Icons provided by thenounproject.com: <a href=https://thenounproject.com/icon/map-pack-4750908/>magnifier</a>|<a href=https://thenounproject.com/icon/notes-5219147/>notes</a>
                           </p></center>')
             ),
             tabPanel(title="Results",
                      h4(strong(paste0("Cluster Results for ", model, " Analysis in ", state, " State"))),
                      HTML("<p><i>Dashboard instructions: 
                      <br>
                      </i>Click on a row in the table to highlight the corresponding cluster. Clicking a cluster will display a pop-up containing average Social Vulnerability Index
                        calculations for each of the four themes and overall. To toggle on county boundaries, click on the 'County' checkbox on the map.</p>"),
                      fluidRow(align="center", 
                               column(3, offset=2, 
                                      selectInput(inputId = "date_slider", label = HTML("<b>Date</b><br>Note: Reflects latest date model was run for."), 
                                                  choices = unique(all_datasets_clean$END_DATE), 
                                                  selected = max(unique(all_datasets_clean$END_DATE)))),
                               # column(4, offset=1,
                               #        sliderInput(inputId="cluster_filter",
                               #                    label=HTML("<b>Filter clusters by recurrence interval (>= days)</b>"),
                               #                    min=1,
                               #                    max=1825, 
                               #                    value=1, 
                               #                    step=5, 
                               #                    ticks=T))
                               column(4, offset=1,
                                      sliderTextInput(inputId="cluster_filter",
                                                      label=HTML("<b>Filter clusters by >= recurrence interval (days)</b>"),
                                                      choices=days,
                                                      selected=365,
                                                      grid=T
                                      ))
                               # column(4, offset=1, 
                               #        checkboxInput(inputId="cluster_filter", label= HTML("<b>Show statistically significant clusters only (p-value <= 0.05)</b>"),
                               #                      value = F))
                      ),
                      fluidRow(align="center", splitLayout(cellWidths = c("38%", "58%"),
                                                           leafletOutput("state_map", width="auto", height="600px"),
                                                           div(DTOutput("state_table"), style = "font-size:100%; font-family: Calibri; margin-left:50px"))),
                      hr()),
             tabPanel(title="Technical Notes",
                      HTML("<h4><b>Methodology</b></h4>"),
                      tabsetPanel(type="tabs",
                                  tabPanel("CalCORVID Dashboard", align="left",
                                           HTML("<b>Description</b>"),
                                           HTML("<p>This dashboard facilitates the visualization of spatial clusters and is designed for usage in an applied public health setting. 
                                                Additional documentation for adapting this dashboard for your jurisdiction's needs is available in the README file.</p>"),
                                           HTML("	CalCORVID is written in the R programming language and is released under the <a href='https://opensource.org/license/mit/'>MIT Open Source License</a>. 
                                           The current distribution includes CalCORVID source code, tutorials on using the software, and detailed descriptions of the configuration parameters. The documentation also describes the overall structure of the dashboard and 
                                           basic RShiny functionalities for individuals who may be interested in customizing CalCORVID for their own use. The primary output of this repository is open source dashboard code for users to adapt for their spatiotemporal 
                                           cluster outputs from SaTScan software. This dashboard can be used for epidemiologists conducting routine surveillance or to share with other stakeholders who may be interested in disease clustering. 
                                           The documentation describes the input files needed to display correctly on the dashboard and the parameters to change for successful adaptation to other jurisdictions’ results. CalCORVID is distributed with sample data from 
                                           the rsatscan package to demonstrate data preprocessing and dashboard display. An additional simulated dataset is provided and detailed in the README file for users to adapt and become more familiar with the structure of the 
                                           dashboard before generating their own data.")),
                                  tabPanel("SaTScan", align="left",
                                           HTML("<b>Background</b>"),
                                           HTML("<p>SaTScan is a free statistical software that analyzes data using spatial, temporal, or spatiotemporal scan statistics. 
                                                It is a <a href='https://www.satscan.org/'>free download</a> and the source documentation is available
                                                <a href='https://www.satscan.org/cgi-bin/satscan/register.pl/SaTScan_Users_Guide.pdf?todo=process_userguide_download'>here</a>.
                                                SaTScan has been applied to many different fields to identify low and/or high clusters of events across space and time. In applied
                                                public health settings, SaTScan is often used to identify abnormal clusters of disease and is typically used to complement traditional
                                                disease surveillance methods.</p>"),
                                           HTML("<b>Methodology Overview</b>"),
                                           HTML("<p>Clusters in this spatiotemporal context can be thought of as 3-dimensional cylinders, with the base and height corresponding to the spatial and
                                                time units, respectively. The user specifies a geographic unit to scan over, such as census tracts in the state of California, and the
                                                temporal window of interest to detect clusters, such as 14-21 days. The spatiotemporal scan statistic then creates permutations of all potential
                                                combinations of the spatial and time units, and each of these permutations is considered a potential cluster. An observed and expected case count
                                                is calculated for each potential cluster, and the scan statistic scans over all the clusters to identify those that have a greater expected case count
                                                than observed case count (the abnormal clusters.) These abnormal clusters are then reported by the SaTScan software to the user.</p>
                                                <p>There are several different analyses and probability models that can be used. The input files and parameters will differ based on the chosen 
                                                analysis and/or probability model. For example, SaTScan requires a case file and population file if using the space-time analysis with the discrete Poisson
                                                probability model, and only requires the case file for the space-time permutation. The space-time analysis calculates the expected case count by
                                                standardizing the case counts for each geographic unit (e.g., cases in census tract x) to the population counts (e.g., census population in census tract x.)
                                                On the other hand, the space-time permutation does not require a population file and calculates the expected case count only using the case file.</p>")),
                                  tabPanel("Social Vulnerability Index (SVI)", align="left",
                                           HTML("<b>Background</b>"),
                                           HTML("<p>The Centers for Disease Control/Agency for Toxic Substances and Disease Registry (CDC/ATSDR) provides the Social Vulnerability Index (SVI)
                                                as a population-level metric for comparing social vulnerability across geographies. Social vulnerability is based on the idea that potential negative
                                                effects on communities caused by external stresses on human health exist, and by lowering vulnerability will also lower human suffering and economic loss.
                                                SVI was originally designed to help public health officials and emergency response planners identify and map the communities that will most likely need support
                                                before, during, and after a hazardous event, but has also been used as a proxy for describing the population-level socioeconomic environment.</p>"),
                                           HTML("<p>SVI data is available from the <a href='https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html'> CDC website </a> at the
                                                census tract and county levels. Zip-code level SVI requires the use of a crosswalk as outlined in their 
                                                <a href='https://www.atsdr.cdc.gov/placeandhealth/svi/faq_svi.html'>FAQ</a>, but by using the <i>findSVI</i> package, we are able to
                                                easily integrate zip code-level SVI into the CalCORVID dashboard. Otherwise, SVI is given at the county and census tract levels.</p>"),
                                           HTML("<p>SVI is calculated using 16 Census variables and is composed of 4 themes.
                                                The CDC/ATSDR SVI ranking variables for the four themes are: 
                                                <li>RPL_THEME1 for the Socioeconomic Status theme,</li>
                                                <li>RPL_THEME2 for the Household Characteristics theme, </li>
                                                <li>RPL_THEME3 for the Racial & Ethnic Minority Status theme, </li>
                                                <li>RPL_THEME4 for the Housing Type & Transportation theme </li>
                                                <li>RPL_THEMES for overall vulnerability</li>
                                                <br>
                                                The breakdown for each theme is shown below, with variables obtained from American Community Survey (ACS), 2016-2020 (5-year) data:
                                                <br>
                                                <img src='svi.png', height=500px, width=600px></img>
                                                <br>
                                                <small><a href='https://www.atsdr.cdc.gov/placeandhealth/svi/documentation/SVI_documentation_2020.html'>Source</a></small>
                                                <br>
                                                <br>"), 
                                           HTML("<b>Interpretation</b>"),
                                           HTML("<p>A percentile ranking represents the proportion of tracts (or zip codes/counties) that are equal to or lower than a tract (or county) of interest in terms of social vulnerability. 
                                                For example, a CDC/ATSDR SVI ranking of 0.85 signifies that 85% of tracts (or zip codes/counties) in the state or nation are less vulnerable than the tract (or county) of interest and 
                                                that 15% of tracts (or counties) in the state or nation are more vulnerable.</p>
                                                <br>"),
                                           HTML("<b>Interpretation Example</b>
                                                <br>"),
                                           HTML("<img src='svi_popup_ex.PNG', height=200px, width=300px></img>
                                                <br>This particular cluster contains 9 zip codes, 1 of which is missing SVI information so is not included in the mean calculations. SVI can be missing
                                                for a variety of reasons, including tracts that have zero estimates for total population or any of the variables needed to calculate SVI was missing.
                                                We then calculate the average percentile ranking for each of the 8 remaining zip codes in the detected cluster, with percentiles of 0.85, 0.62,
                                                0.94, and 0.83 for the Socioeconomic Status, Housing Characteristics, Racial & Ethnic Minority Status, and Housing Type & Transportation themes.
                                                An average percentile of 0.85 for the Socioeconomic Status theme indicates that 85% of zip codes in New York state are less vulnerable than the
                                                zip codes in the cluster, and 15% of zip codes in New York state are more vulnerable than the zip codes in the cluster. In other words,
                                                the higher the percentile the more vulnerable the zip codes are. We also calculate a percentile for overall vulnerability across all four SVI ranking themes. 
                                                The overall vulnerability percentile describes the overall vulnerability of the 8 zip codes in the detected cluster. 
                                                <hr>")),
                                  tabPanel("References", align="left",
                                           HTML("<b>SaTScan:</b>"),
                                           HTML("<li><a href='https://www.tandfonline.com/doi/abs/10.1080/03610929708831995'>Kulldorff, 1997:</a> A spatial scan statistic</li>
                                                <li><a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC548793/'>Kulldorff, 2005:</a> A Space–Time Permutation Scan Statistic for Disease Outbreak Detection</li>
                                                <li><a href='https://www.satscan.org/cgi-bin/satscan/register.pl/SaTScan_Users_Guide.pdf?todo=process_userguide_download'>SaTScan User Guide Download</a></li>
                                                <li><a href='https://preprints.jmir.org/preprint/50653'><i>Preprint; Levin-Rector, 2023</i>:</a> Prospective Spatiotemporal Cluster Detection using SaTScan: A Tutorial for Designing and Finetuning a System to Detect Reportable Communicable Disease Outbreaks</li>"),
                                           br(),
                                           HTML("<b>Social Vulnerability Index</b>"),
                                           HTML("<li><a href='https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html'> (CDC/ASTDR) SVI Documentation Download</a></li>
                                                <li><a href='https://pubmed.ncbi.nlm.nih.gov/33090977/'>Dasgupta, 2020</a>: Association Between Social Vulnerability and a County's Risk for Becoming a COVID-19 Hotspot - United States, June 1-July 25, 2020</li>
                                                <li><a href='https://svi.cdc.gov/A%20Social%20Vulnerability%20Index%20for%20Disaster%20Management.pdf'>Flanagan, 2011</a>: A Social Vulnerability Index for Disaster Management</li>
                                                <li><a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7179070/'>Flanagan, 2018</a>: Measuring Community Vulnerability to Natural and Anthropogenic Hazards: The Centers for Disease Control and Prevention’s Social Vulnerability Index</li>
                                                <li><a href='https://pubmed.ncbi.nlm.nih.gov/34388054/'>Troppy, 2021</a>: Geographic Associations Between Social Factors and SARS-CoV-2 Testing Early in the COVID-19 Pandemic, February-June 2020, Massachusetts</li>"),
                                           br(),
                                           HTML("<b>Applied Public Health Examples:</b>"),
                                           HTML("<li><a href='https://wwwnc.cdc.gov/eid/article/28/3/21-1147_article'>Gleason, 2022</a>: Development and Evaluation of Statewide Prospective Spatiotemporal Legionellosis Cluster Surveillance, New Jersey, USA</li>
                                                <li><a href='https://wwwnc.cdc.gov/eid/article/27/5/20-3583_article'>Greene, 2021</a>: Detecting COVID-19 Clusters at High Spatiotemporal Resolution, New York City, New York, USA, June–July 2020</li>
                                                <li><a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1525257/'>Jones, 2006</a>: Use of a Prospective Space-Time Scan Statistic to Prioritize Shigellosis Case Investigations in an Urban Jurisdiction</li>
                                                <li><a href='https://www.cdc.gov/mmwr/volumes/69/wr/mm6926a2.htm'>Latash, 2020</a>: Salmonellosis Outbreak Detected by Automated Spatiotemporal Analysis — New York City, May–June 2019</li>
                                                <li><a href='https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3000152/'>Mostashari, 2003</a>: Dead Bird Clusters as an Early Warning System for West Nile Virus Activity</li>"))
                      )),
             tags$head(tags$style(".leaflet-top {z-index:999!important;}")) #overlays drop-down date selection over leaflet map
  ))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  #Welcome page click to tab
  shinyjs::onclick(id="results_image", updateTabsetPanel(session, inputId="Daily", selected="Results"))
  shinyjs::onclick(id="technotes_image", updateTabsetPanel(session, inputId="Daily", selected="Technical Notes"))
  
  #Dynamic labeling depending on specified geographic aggregation
  label_reactive <- reactive({
    ifelse(level=="zcta", "# of ZIP Codes",
           ifelse(level=="county", "# of Counties", 
                  ifelse(level=="tract", "# of Census Tracts", NA)))
  })
  
  #Render leaflet map
  if(zoom_level=="state"){
    output$state_map <-  renderLeaflet({
      leaflet() %>%
        setView(lng=state_coords$Longitude, lat=state_coords$Latitude, zoom=state_coords$Zoom) %>%
        addProviderTiles("CartoDB.Positron", group="CartoDB")
    })
  } else if (zoom_level=="cluster"){
    output$state_map <-  renderLeaflet({
      leaflet() %>%
        setView(lng=cluster_coords$Longitude, lat=cluster_coords$Latitude, zoom=cluster_coords$Zoom) %>%
        addProviderTiles("CartoDB.Positron", group="CartoDB")
    })
  } else{
    output$state_map <-  renderLeaflet({
      leaflet() %>%
        setView(lng=state_coords$Longitude, lat=state_coords$Latitude, zoom=state_coords$Zoom) %>%
        addProviderTiles("CartoDB.Positron", group="CartoDB")
    })
  }
  
  #Filter based on run date and region to create underlying dataset for map and tables. This is the dataset
  #underlying the location geographies on the leaflet map.
  state_reactive <- reactive({
    req(input$date_slider)
    req(input$cluster_filter)
    val <- input$cluster_filter
    all_datasets_clean <- all_datasets_clean %>%
      dplyr::filter(END_DATE == input$date_slider &
                      RECURR_INT >= val)
    return(all_datasets_clean)
    # req(!is.null(input$cluster_filter))
    # if(input$cluster_filter==T){
    #   all_datasets_clean <- all_datasets_clean %>% dplyr::filter(END_DATE==input$date_slider,
    #                                                              P_VALUE <= 0.05)
    # } else if(input$cluster_filter==F){
    #   all_datasets_clean <- all_datasets_clean %>% dplyr::filter(END_DATE==input$date_slider)
    # }
    # return(all_datasets_clean)
    # all_datasets_clean %>% dplyr::filter(END_DATE == input$date_slider)
  })
  
  #Create filtered version of state_reactive() to only display one cluster or row for display in table and layer
  state_reactive_filter <- reactive({
    state_reactive() %>%
      dplyr::group_by(CLUSTER, END_DATE) %>%
      dplyr::filter(row_number()==1)
  })
  
  #Reactive filter for table (builds off state_reactive(), but re-formats table and changes # of Locations to reflect level of geographic aggregation)
  if(level=="zcta"){
    state_reactive_table <- reactive({
      state_reactive_filter() %>% data.frame() %>% 
        dplyr::select(CLUSTER, OBSERVED, EXPECTED,
                      START_DATE,END_DATE, NUMBER_LOC, P_VALUE_DISPLAY, RECURR_INT_DISPLAY) %>% 
        arrange(CLUSTER) %>%
        rename(c("SaTScan Cluster ID"=CLUSTER,
                 "Start Date"=START_DATE, "End Date"=END_DATE,
                 "# of ZIP Codes"=NUMBER_LOC, "p-value"=P_VALUE_DISPLAY,
                 "Recurrence Interval"=RECURR_INT_DISPLAY, "Observed"=OBSERVED,
                 "Expected"=EXPECTED)) 
    })
  } else if(level=="county"){
    state_reactive_table <- reactive({
      state_reactive_filter() %>% data.frame() %>% 
        dplyr::select(CLUSTER, OBSERVED, EXPECTED,
                      START_DATE,END_DATE, NUMBER_LOC, P_VALUE_DISPLAY, RECURR_INT_DISPLAY) %>% 
        arrange(CLUSTER) %>%
        rename(c("SaTScan Cluster ID"=CLUSTER,
                 "Start Date"=START_DATE, "End Date"=END_DATE,
                 "# of Counties"=NUMBER_LOC, "p-value"=P_VALUE_DISPLAY,
                 "Recurrence Interval"=RECURR_INT_DISPLAY, "Observed"=OBSERVED,
                 "Expected"=EXPECTED)) 
    })
  } else if(level=="tract"){
    state_reactive_table <- reactive({
      state_reactive() %>% data.frame() %>% 
        dplyr::select(CLUSTER, OBSERVED, EXPECTED,
                      START_DATE,END_DATE, NUMBER_LOC, P_VALUE_DISPLAY, RECURR_INT_DISPLAY) %>% 
        arrange(CLUSTER) %>%
        rename(c("SaTScan Cluster ID"=CLUSTER,
                 "Start Date"=START_DATE, "End Date"=END_DATE,
                 "# of Census Tracts"=NUMBER_LOC, "p-value"=P_VALUE_DISPLAY,
                 "Recurrence Interval"=RECURR_INT_DISPLAY, "Observed"=OBSERVED,
                 "Expected"=EXPECTED)) 
    })
  } else{
    state_reactive_table <- reactive({
      state_reactive_filter() %>% data.frame() %>% 
        dplyr::select(CLUSTER, OBSERVED, EXPECTED,
                      START_DATE,END_DATE, NUMBER_LOC, P_VALUE_DISPLAY, RECURR_INT_DISPLAY) %>% 
        arrange(P_VALUE_DISPLAY) %>%
        rename(c("SaTScan Cluster ID"=CLUSTER,
                 "Start Date"=START_DATE, "End Date"=END_DATE,
                 "# of Locations"=NUMBER_LOC, "p-value"=P_VALUE_DISPLAY,
                 "Recurrence Interval"=RECURR_INT_DISPLAY, "Observed"=OBSERVED,
                 "Expected"=EXPECTED)) 
    })
  }
  
  #Render all_state_reactive_table() as datatable
  output$state_table <- renderDT(
    datatable(state_reactive_table(), 
              rownames=FALSE, 
              selection="single", 
              class="display",
              options=list(stateSave=TRUE,
                           dom = 'Btsp',
                           columnDefs = list(list(className = 'dt-center', targets = "_all")))))
  
  
  #Interactive markers on map
  highlight_icon_1b = makeAwesomeIcon(icon='map-marker', library="fa", markerColor='green', iconColor='white')
  observeEvent(input$state_table_rows_selected, ignoreNULL=F, {
    row_selected = state_reactive_filter()[input$state_table_rows_selected, ]
    proxy_state <- leafletProxy('state_map')
    proxy_state %>%
      clearMarkers() %>%
      addAwesomeMarkers(layerId = as.character(row_selected$END_DATE),
                        lng = row_selected$LONGITUDE,
                        lat = row_selected$LATITUDE,
                        icon = highlight_icon_1b)
  })
  
  
  ## OBSERVE FUNCTION FOR DYNAMIC MAPS
  #Reactive function for county boundary layer
  county_boundaries_reactive <- reactive({
    county_boundaries
  })
  
  #Render leaflet map
  observe({
    if(!"LOC_ID_col" %in% colnames(state_reactive())) { #If circular scan not run, then don't output Cluster layer
      input$Daily
      leafletProxy(mapId = "state_map", data = state_reactive()) %>%
        clearShapes() %>%
        addPolygons(data=state_reactive(),
                    stroke=T, color="black", weight=1.0,
                    fillColor=~neon_pal_map(CLUSTER),
                    popup = paste0("<b>", label_reactive(),  ": </b>", state_reactive()$NUMBER_LOC, "<br/>",
                                   "<b>", label_reactive(), " Missing SVI: </b>", state_reactive()$NUM_SVI_NA, "<br/>",
                                   "<b> 1) Socioeconomic Status: </b>", state_reactive()$MEAN_RPL_THEME1, "<br/>",
                                   "<b> 2) Household Characteristics: </b>", state_reactive()$MEAN_RPL_THEME2, "<br/>",
                                   "<b> 3) Racial & Ethnic Minority Status: </b>", state_reactive()$MEAN_RPL_THEME3, "<br/>",
                                   "<b> 4) Housing Type & Transportation: </b>", state_reactive()$MEAN_RPL_THEME4, "<br/>",
                                   "<b> Overall: </b>", state_reactive()$MEAN_RPL_THEMES, "<br/>")) %>%
        addPolygons(data=county_boundaries_reactive(),
                    stroke=T, color="black", weight=2.0, fillOpacity=0,
                    options = pathOptions(clickable = FALSE), 
                    group="County") %>%
        addLabelOnlyMarkers(data=county_boundaries_reactive(),
                            lng=~long, lat=~lat,
                            label=~NAME,
                            labelOptions = labelOptions(noHide=T,
                                                        textOnly=T,
                                                        direction="left",
                                                        offset=c(0,10),
                                                        style=list('background-color'='white',
                                                                   'color'="black",
                                                                   'fontSize'="11px",
                                                                   'fontWeight'="bold")),
                            group="County") %>%
        addLayersControl(overlayGroups=c("County"),
                         options=layersControlOptions(collapsed=F)) %>%
        hideGroup(c("County"))
    } else if ("LOC_ID_col" %in% colnames(state_reactive())){ #If circular scan is run, output Cluster layer
      input$Daily
      leafletProxy(mapId = "state_map", data = state_reactive()) %>%
        clearShapes() %>%
        addPolygons(data=state_reactive(),
                    stroke=T, color="black", 
                    fillColor=~neon_pal_map(CLUSTER), weight=1.0, 
                    opacity=~ifelse(state_reactive()$RECURR_INT >= 100, 1.0, 0.5),
                    fillOpacity=~ifelse(state_reactive()$RECURR_INT >= 100, 1.0, 0.5),
                    popup = paste0("<b>", label_reactive(),  ": </b>", state_reactive()$NUMBER_LOC, "<br/>",
                                   "<b>", label_reactive(), " Missing SVI: </b>", state_reactive()$NUM_SVI_NA, "<br/>",
                                   "<b> 1) Socioeconomic Status: </b>", state_reactive()$MEAN_RPL_THEME1, "<br/>",
                                   "<b> 2) Household Characteristics: </b>", state_reactive()$MEAN_RPL_THEME2, "<br/>",
                                   "<b> 3) Racial & Ethnic Minority Status: </b>", state_reactive()$MEAN_RPL_THEME3, "<br/>",
                                   "<b> 4) Housing Type & Transportation: </b>", state_reactive()$MEAN_RPL_THEME4, "<br/>",
                                   "<b> Overall: </b>", state_reactive()$MEAN_RPL_THEMES, "<br/>")) %>%
        addPolygons(data=county_boundaries_reactive(),
                    stroke=T, color="black", weight=2.0, fillOpacity=0,
                    options = pathOptions(clickable = FALSE), 
                    group="County") %>%
        addCircles(data = state_reactive_filter(), lng=state_reactive_filter()$LONGITUDE, lat=state_reactive_filter()$LATITUDE, radius=state_reactive_filter()$RADIUS_M,
                   color =  ~ifelse(state_reactive_filter()$RECURR_INT >= 100, "red", "darkorange"), opacity=1, weight=1,
                   label=paste0(state_reactive_filter()$CLUSTER),
                   labelOptions = labelOptions(noHide=T,
                                               textOnly=T,
                                               direction = "center",
                                               style = list(
                                                 "color"="black",
                                                 "font-weight" = "bold")),
                   group="Cluster") %>%
        addLabelOnlyMarkers(data=county_boundaries_reactive(),
                            lng=~long, lat=~lat,
                            label=~NAME,
                            labelOptions = labelOptions(noHide=T,
                                                        textOnly=T,
                                                        direction="left",
                                                        offset=c(0,10),
                                                        style=list('background-color'='white',
                                                                   'color'="black",
                                                                   'fontSize'="11px",
                                                                   'fontWeight'="bold")),
                            group="County") %>%
        addLayersControl(overlayGroups=c("County", "Cluster"),
                         options=layersControlOptions(collapsed=F)) %>%
        hideGroup(c("County", "Cluster"))
    }
    
  })
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)
