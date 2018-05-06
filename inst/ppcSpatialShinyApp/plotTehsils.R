#' @title Visualization of selected tehsils.
#' @name plotTehsils
#' @description Visualization of selected tehsils from shiny app.
#' @param filterTehsil The selected tehsils for which plot to be build.
#' @keywords internal
#' @return Return ggplot of selected tehsils.

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

plotTehsils <- function(filterTehsil) {
  plotUnit <-
    PakPC2017::PakPC2017Tehsil %>%
    tidyr::gather(
      key   = "Census"
      , value = "Population"
      , -Province, - Division, -District, -Tehsil
    )  %>%
    dplyr::filter(Tehsil %in% filterTehsil)

  ggplot(
    data = plotUnit
    , mapping = aes(x = reorder(Tehsil, -Population), y = Population, fill = Census)) +
    geom_bar(stat = "identity", position = position_dodge(width = .9))+
    scale_y_continuous(labels= scales::comma, expand =  c(0, 0)) +
    scale_x_discrete(expand = c(0.03,0))+
    geom_text(aes(label=scales::comma(Population)),
              position=position_dodge(width = 0.9), vjust = 0.8) +
    facet_grid(Division ~ Province, scales = "free_x") +
    labs(x = "Tehsils") +
    theme(legend.position = "top")
}
