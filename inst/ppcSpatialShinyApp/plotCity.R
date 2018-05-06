#' @title Visualization of all top cities.
#' @name plotCity
#' @description Visualization of all top cities from shiny app.
#' @keywords internal
#' @return Return ggplot of all top cities.

if(getRversion() >= "2.15.1"){
  utils::globalVariables(
    c(
      "Census"
      , "City"
      , "District"
      , "Division"
      , "Pop1998"
      , "Pop2017"
      , "Population"
      , "Province"
      , "Tehsil"
    )
  )
}

plotCity <- function() {
  plotUnit <-
    PakPC2017::PakPC2017City10 %>%
    arrange(desc(Pop2017)) %>%
    tidyr::gather(
      key   = "Census"
      , value = "Population"
      , -City
    )


  plotOut<-ggplot(
    data = plotUnit
    , mapping = aes(x = reorder(City, -Population), y = Population, fill = Census)) +
    geom_bar(stat = "identity", position = position_dodge(width = .9))+
    scale_y_continuous(labels= scales::comma,expand =  c(0,0)) +
    scale_x_discrete(expand = c(0.03,0))+
    geom_text(aes(label=scales::comma(Population)),
              position=position_dodge(width = 0.9), vjust = 0.8) +
    labs(x = "Cities") +
    theme(legend.position = "top")
return(plotOut)
}
