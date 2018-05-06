#' @title Visualization of selected districts.
#' @name plotDistricts
#' @description Visualization of selected districts from shiny app.
#' @param filterTehsil The selected districts for which plot to be build.
#' @keywords internal
#' @return Return ggplot of selected districts.


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

plotDistricts <- function(filterDistrict) {
  plotUnit <-
    PakPC2017::PakPC2017Tehsil %>%
    dplyr::group_by(Province, Division, District) %>%
    dplyr::summarize(
        Pop2017 = sum(Pop2017, na.rm = TRUE)
      , Pop1998 = sum(Pop1998, na.rm = TRUE)
    ) %>%
    dplyr::filter(District %in% filterDistrict) %>%
    tidyr::gather(
      key   = "Census"
      , value = "Population"
      , -Province, - Division, -District
    )


  ggplot(
    data = plotUnit
    , mapping = aes(x = reorder(District, -Population), y = Population, fill = Census)) +
    geom_bar(stat = "identity", position = position_dodge(width = .9))+
    scale_y_continuous(labels= scales::comma, expand =  c(0, 0)) +
    scale_x_discrete(expand = c(0.03,0))+
    geom_text(aes(label=scales::comma(Population)),
              position=position_dodge(width = 0.9), vjust = 0.8) +
    facet_wrap(~ Province, scales = "free_x") +
    labs(x = "Districts") +
    theme(legend.position = "top")
}
