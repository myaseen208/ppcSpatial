#' @title Visualization of selected divisions.
#' @name plotDivisions
#' @description Visualization of selected divisions from shiny app.
#' @param filterTehsil The selected divisions for which plot to be build.
#' @keywords internal
#' @return Return ggplot of selected divisions.


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

plotDivisions <- function(filterDivision) {
plotUnit <-
  PakPC2017::PakPC2017Tehsil %>%
  dplyr::group_by(Province, Division) %>%
  dplyr::summarize(
    Pop2017 = sum(Pop2017, na.rm = TRUE)
    , Pop1998 = sum(Pop1998, na.rm = TRUE)
  ) %>%
  dplyr::filter(Division %in% filterDivision) %>%
  tidyr::gather(
    key   = "Census"
    , value = "Population"
    , -Province, - Division
  )

ggplot(
  data = plotUnit
  , mapping = aes(x = reorder(Division, -Population), y = Population, fill = Census)) +
  geom_bar(stat = "identity", position = position_dodge(width = .9))+
  scale_y_continuous(labels= scales::comma, expand =  c(0, 0)) +
  scale_x_discrete(expand = c(0.03,0))+
  geom_text(aes(label=scales::comma(Population)),
            position=position_dodge(width = 0.9), vjust = 0.8) +
  facet_wrap(~Province, scales = "free_x") +
  labs(x = "Divisions") +
  theme(legend.position = "top")
}
