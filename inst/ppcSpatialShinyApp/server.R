shinyServer(
  function(input, output, session) {
    observe({
      filterDivision <- dplyr::filter(PakPC2017Tehsil, Province %in% input$Province)
      levelDivision <- levels(as.factor(filterDivision$Division))
      updateSelectInput(
        session = session
        , inputId = "Division"
        , choices = c("select division"="", levelDivision)
      )
      reactive(input$Division)
    })

    observe({
      filterDistrict <- dplyr::filter(PakPC2017Tehsil, Division %in% input$Division)
      levelDistrict <- levels(as.factor(filterDistrict$District))
      updateSelectInput(
        session = session
        , inputId = "District"
        , choices = c("select district"="",levelDistrict)
      )
      reactive(input$District)
    })

    observe({
      filterTehsil <- dplyr::filter(PakPC2017Tehsil, District %in% input$District)
      levelTehsil <- levels(as.factor(filterTehsil$Tehsil))
      updateSelectInput(
        session = session
        , inputId = "Tehsil"
        , choices = c("select tehsil"="", levelTehsil)
      )
    })

    adminUnit <-
      reactive(
        {
          tableOut(as.character(input$AdminUnit))
        }
      )

    plotUnit <-
      reactive(
        {
          if(input$PlotUnit=="Provinces"){
            plotProvinces()
          }
          else if(input$PlotUnit=="Divisions"){
            plotDivisions(input$Division)
          }
          else if(input$PlotUnit=="Districts"){
            plotDistricts(input$District)
          }
          else if(input$PlotUnit=="Tehsils"){
            plotTehsils(input$Tehsil)
          }
          else{
            plotCity()
          }
        }
      )
    Population <- readRDS('FSDCity.RDS')
    mapCity<-tm_shape(Population)+
      tm_fill("Popultion", id = "Circle")
    cityMap<-tmap_leaflet(mapCity)
    cityMap2<-cityMap %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addPolygons(data = Population,label = as.character(Population$Circle), color = "#007196", weight = 1, smoothFactor = 0.5,
                  opacity = 0.7, fillOpacity = 0,
                  popup=paste0("<b>Circle Code = </b>",Population$Circle,"<p>Population = ",Population$Popultion,"<br> Households = ",Population$HH," </p>",sep = ""),
                  highlightOptions = highlightOptions(color = "red", weight = 2,
                                                      bringToFront = TRUE))%>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true,watch: true}); }")))

    Population2 <- readRDS('PakDist.RDS')
    mapDist<-tm_shape(Population2)+
      tm_polygons("Pop2017", id = "NAME_EN", title = "Population 2017", palette = "YlOrRd", contrast = c(0.21, 0.71),style = "kmeans",n = 7, auto.palette.mapping = FALSE)
    distMap1<-tmap_leaflet(mapDist)
    distMap<-distMap1 %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addPolygons(data = Population2, label = Population2$NAME_EN, color = "#007196", weight = 1, smoothFactor = 0.5,
                  opacity = 0.7, fillOpacity = 0,
                  popup=paste0("<b>District = </b>",Population2$NAME_EN,"<p>Population = ",Population2$Pop2017,"<br> Households = ",Population2$HH," </p>",sep = ""),
                  highlightOptions = highlightOptions(color = "red", weight = 2,
                                                      bringToFront = TRUE))%>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(1); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true,watch: true}); }")))


    output$geoMap = renderLeaflet(cityMap2)
    output$geoDist = renderLeaflet(distMap)


    output$myTable <- renderDataTable({adminUnit()})

    output$myMap <-
      renderPlot(
        {
          plotUnit()
        }
      )
    output$download_plot.pdf <- downloadHandler(
      filename = function() {
        paste("Figure_ggplotGUI_", Sys.time(), ".pdf", sep = "")
      },
      content <- function(file) {
        ggsave(file, plot = last_plot())
      },
      contentType = "application/pdf" # MIME type of the image
    )
  }
)
