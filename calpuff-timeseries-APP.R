library(shiny); library(leaflet); 
library(shinycssloaders)
library(readr); library(openair); library(rgdal); library(sp); library(tidyr)

    # Increase File Size Limit
    options(shiny.maxRequestSize = 500*1024^2)
    
    # Define UI 
    ui <- fluidPage(
      
      # Application title
      titlePanel("CALPUFF Time Series Analysis"),
      
      # Top panel with county name
      fluidRow(
        
        column(4,
          fileInput("file", "Select CALPUFF format timeseries DAT file",
                    accept = c(
                      "text/plain",
                      "TSERIES*",
                      "*.DAT")
                    )
        ),
        
        column(2, 
        selectInput(
          inputId = "select_plot",
          label = "Select Plot Type",
          choices = c(
            "timePlot" = "timePlot",
            "calendarPlot" = "calendarPlot",
            "trendLevel" = "trendLevel",
            "timeVariation" = "timeVariation"
          ), 
          selected = "All",
          multiple = FALSE
        )),
        
        column(4, textOutput("receptor")),
        
        # the map itself
        column(8,
          leafletOutput("map")
        ),
        
        plotOutput(outputId = "plot") 
        # %>% withSpinner(color="#0dc5c1")
      )
      
    )

    # Define server logic 
    server <- function(input, output) {
      
      observe({
  
        # Import CALPUFF Time Series Data
        # file.loc <- "C:/Users/putlands/Desktop/Modelling/TSERIES_PM10_1HR_CONC_C.DAT"
        # file.loc <- "L:/Secure/Modeling2/605x/60553814/AIR/Revised_Modelling_2021/Domain6/scenarios/B2G_Domain6_CALSUM_Scen2_MAX/_batchCALPOST_MAX/NOX/TSERIES_NOX_1HR_CONC_C.DAT"
        
        inFile <- input$file
        
        if (is.null(inFile))
          return(NULL)
        
        calpost <- read_table2(inFile$datapath,
                               skip = 14,
                               col_names = FALSE)
        
        coords <- read_table2(inFile$datapath,
                              skip = 7,
                              col_names = FALSE)
        
        columns <- coords[1,2:ncol(coords)]
        coords <- coords[3:4,2:ncol(coords)]
        coords <- t(coords)
        x <- as.numeric(coords[,1]) * 1000
        y <- as.numeric(coords[,2]) * 1000
        
        xy <- data.frame(ID = paste0("r",1:length(x)), X = x, Y = y)
        
        #Format and red date
        date <- paste(calpost$X1,calpost$X2,calpost$X3, sep = "-")
        calpost_v <- calpost[,c(4:ncol(calpost))]
        calpost <- cbind(date, calpost_v)
        calpost$date <- as.POSIXct(strptime(calpost$date, format = "%Y-%j-%H"))
        colnames(calpost) <- c("date", paste0("r",1:length(x)))
        
        for(i in 1:length(calpost_v)){
          xy$z[i] <- tibble(calpost_v[,i])
        }
        
        coordinates(xy) <- c("X", "Y")
        proj4string(xy) <- CRS("+init=epsg:32756")
        
        #Re-project Data to 'longlat'
        geo_proj = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
        xy_trnsfrmd = spTransform(xy,geo_proj)
        # v <- reactiveValues(msg = "")
        
        output$map <- renderLeaflet({
          
          inFile <- input$file
          
          if (is.null(inFile))
            return(NULL)
          
          leaflet() %>% 
            addProviderTiles("Esri.WorldImagery") %>% 
            addCircleMarkers(data = xy_trnsfrmd, layerId = ~ID, radius = 2, color = "yellow", stroke = TRUE, fillOpacity = 0)
          
        })
        
        observeEvent(input$map_marker_click, {
          p <- input$map_marker_click
          select_plot <- input$select_plot
          
          output$receptor <- renderPrint({
            p$id
          })
          
          output$plot <- renderPlot({
            
            if(select_plot == "timePlot")
            timePlot(calpost, pollutant = p$id, auto.text = FALSE)
            
            if(select_plot == "calendarPlot")
            calendarPlot(calpost, pollutant = p$id)
            
            if(select_plot == "timeVariation")
            timeVariation(calpost, pollutant = p$id)
            
            if(select_plot == "trendLevel")
            trendLevel(calpost, pollutant = p$id)
            
          })
          
        })
      })
    
    }

# Run the application 
shinyApp(ui = ui, server = server)
