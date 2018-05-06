#' @title Visualization of all provinces.
#' @name plotProvinces
#' @description Visualization of all provinces from shiny app.
#' @keywords internal
#' @return Return ggplot of all provinces.

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

plotProvinces <- function() {
    plotUnit <-
      PakPC2017::PakPC2017Tehsil %>%
      dplyr::group_by(Province) %>%
      dplyr::summarize(
          Pop2017 = sum(Pop2017, na.rm = TRUE)
        , Pop1998 = sum(Pop1998, na.rm = TRUE)
      ) %>%
      tidyr::gather(
        key   = "Census"
        , value = "Population"
        , -Province
      )

    ggplot(
      data = plotUnit
      , mapping = aes(x = reorder(Province, -Population), y = Population, fill = Census)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.9))+
      scale_y_continuous(labels= scales::comma, expand =  c(0, 0)) +
      scale_x_discrete(expand = c(0.03,0))+
      geom_text(aes(label=scales::comma(Population)),
                position=position_dodge(width = 0.9), vjust = 0.8) +
      labs(x = "Provinces") +
      theme(legend.position = "top")


  }
