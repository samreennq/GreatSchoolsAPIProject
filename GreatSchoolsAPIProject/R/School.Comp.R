#' Combine datasets with School Search data and School Census data.
#'
#' Combines the two desparate datasets where census data is created by
#' using the entries from School Search data into a single dataset.
#'
#' @param Browse.DF Name of a dataframe with school search results.
#' @param Census.DF Name of a dataframe with school census data.
#'
#' @return Use School Search data and School Census data from different dataset and then
#' create a single dataset with comprehensive information.
#'
#' School.Comp(Browse.Schools.NY, Census.Schools.NY)
#'
#' @export
#'

School.Comp<-function(Browse.DF, Census.DF) {
  Browse.DF$latitude<-Browse.DF$lat
  Browse.DF$lat<-NULL
  Browse.DF$longitude<-Browse.DF$lon
  Browse.DF$lon<-NULL
  Browse.DF$schoolName<-Browse.DF$name
  Browse.DF$name<-NULL
  School_Comp<-suppressMessages(dplyr::left_join(Census.DF, Browse.DF))
  School_Comp
}
