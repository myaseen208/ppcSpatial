tableOut <- function(type) {
  if(type == "Provinces"){
    adminUnit <-
      PakPC2017Tehsil %>%
      dplyr::group_by(Province) %>%
      dplyr::summarize(
          Pop2017 = sum(Pop2017, na.rm = TRUE)
        , Pop1998 = sum(Pop1998, na.rm = TRUE)
      )
    adminUnit
  }
  else if(type == "Divisions"){
    adminUnit <-
      PakPC2017Tehsil %>%
      dplyr::group_by(Province, Division) %>%
      dplyr::summarize(
        Pop2017 = sum(Pop2017, na.rm = TRUE)
        , Pop1998 = sum(Pop1998, na.rm = TRUE)
      )
    adminUnit
  }
  else if(type == "Districts"){
    adminUnit <-
      PakPC2017Tehsil %>%
      dplyr::group_by(Province, Division, District) %>%
      dplyr::summarize(
        Pop2017 = sum(Pop2017, na.rm = TRUE)
        , Pop1998 = sum(Pop1998, na.rm = TRUE)
      )
    adminUnit
  }
  else if(type == "Tehsils"){
    adminUnit <- PakPC2017Tehsil
    adminUnit
  }
  else{
    adminUnit <- PakPC2017City10
    adminUnit
  }
}
