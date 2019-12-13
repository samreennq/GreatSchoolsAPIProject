#' Extract School Search data from GreatSchools API.
#'
#' Includes school names, parent and GreatSchools assigned ratings, location (langitude, latitude, address)
#' and contact information of the school. There are also links to school website, GreatSchools
#' demographic and review pages. Schools are indexed by gsId.
#'
#' @param state A string.
#' @param city A string.
#' @param AppKey API Key.
#' @return School Search data in a clean and formatted dataframe.
#'
#' Browse.Schools("NY", "Albany", Sys.getenv("GreatSchoolsAPI"))
#' Browse.Schools("CA", "Oakland",Sys.getenv("GreatSchoolsAPI"))
#'
#' @export

Browse.Schools<- function (state, city, AppKey) {
  endpoint<-"https://api.greatschools.org/schools/"
  State<-as.character(state)
  City<-as.character(city)
  Loc<-paste(State,City,sep="/")
  url<-paste(endpoint,Loc,"?key=",AppKey, sep="")
  Browse.Response<-httr::GET(url)
  Data<-httr::content(Browse.Response)
  DF1<-Data[[1]]$school
  Browse.Schools.DF<-purrr::map_df(DF1, ~ purrr::map_df(.x,
                                          ~ as.character(.x)))
  Browse.Schools.DF
}
