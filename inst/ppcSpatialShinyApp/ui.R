shinyUI(
  fluidPage(
    headerPanel(
      title = "Spatial Analysis of Pakistan Population Census 2017"
    )
    , sidebarPanel(
      width = 3
      , conditionalPanel(
        condition = "input.tabs == 'data'"
        ,  h4("Population by")
        , selectInput(
          inputId = "AdminUnit"
          , label   = "Please Select Admin Unit"
          , choices = c(
            "Provinces"
            , "Divisions"
            , "Districts"
            , "Tehsils"
            , "City"
          )
          , selected ="Provinces"
        )
      )
      , conditionalPanel(
        condition = "input.tabs == 'Plot'"
        ,  h4("Chart by Administrative Unit")
        ,  selectInput(
          inputId = "PlotUnit"
          , label   = "Select Admin Unit"
          , choices = c(
            "Provinces"
            , "Divisions"
            , "Districts"
            , "Tehsils"
            , "Cities"
          )
          , "Provinces"
        )
        , conditionalPanel(
          condition = "input.PlotUnit == 'Divisions'||
          input.PlotUnit == 'Districts'||
          input.PlotUnit == 'Tehsils'"
          ,  selectInput(
            inputId  = "Province"
            , label    = "Select one or more Provinces first"
            , choices  = c("Select Province"="",levels(as.factor(PakPC2017Tehsil$Province)))
            , multiple = TRUE
          )
        )
        , conditionalPanel(
          condition = "input.PlotUnit == 'Divisions'||
          input.PlotUnit == 'Districts'||
          input.PlotUnit == 'Tehsils'"
          , selectInput(
            inputId  = "Division"
            , label    = "Select one or more Divisions"
            , choices  = ""
            , multiple = TRUE
          )
        )
        , conditionalPanel(
          condition = "input.PlotUnit == 'Districts'||
          input.PlotUnit == 'Tehsils'"
          , selectInput(
            inputId  = "District"
            , label    = "Select one or more Districts"
            , choices  = ""
            , multiple = TRUE
          )
        )
        , conditionalPanel(
          condition = "input.PlotUnit == 'Tehsils'"
          ,  selectInput(
            inputId  = "Tehsil"
            , label    = "Select one or more Tehsils"
            , choices  = ""
            , multiple = TRUE
          )
        )
      )


)
, mainPanel(
  width = 9
  , tabsetPanel(
    type = "tabs"
    , tabPanel(
      title = "data"
      , dataTableOutput("myTable")
    )
    , tabPanel(
      title = "Plot"
      , mainPanel(
        width = 12
        , downloadButton(
          outputId = "download_plot.pdf"
          , label    = "Download pdf of figure"
        )
        , plotOutput("myMap")
      )
    )
    ,tabPanel("Geographical Districts", leafletOutput("geoDist"))
    ,tabPanel("Geographical Faisalabad", leafletOutput("geoMap"))
    , id = "tabs"
  )
)
  )

)
