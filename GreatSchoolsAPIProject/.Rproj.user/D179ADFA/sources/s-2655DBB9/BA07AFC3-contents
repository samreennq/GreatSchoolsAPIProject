#' Extract Top Ranking schools from School Search data
#'
#' Goes through the School Search dataset created by Browse.Schools to find top ranking
#' schools by school type as well as rating types.
#'
#' @param Browse.DF Name of a dataframe with school search results.
#' @param school.type A string. This could be "private", "public" or "charter"
#' @param rating.type A string. This could be "parent" or "gs"
#'
#' @return Use School Search data from a dataset compiled using Browse.Schools to return a
#' dataframe of top 5 ranking schools by school type and rating type.
#'
#' Top.Rating(Browse.Schools.NY, "charter", "gs")
#' Top.Rating(Browse.Schools.NY, "private", "parent")
#'
#' @export
#'




Top.Rating<-function(Browse.DF,school.type, rating.type){
  vars<-c('parentRating','gsRating')
  if (rating.type=="parent") {
    rating.type.new <-  vars[1]
  } else {
    rating.type.new <-  vars[2]
  }
  Browse.DF[[rating.type.new]]<-as.numeric(as.character(Browse.DF[[rating.type.new]]))
  Type<-as.character(school.type)
  DF<-dplyr::arrange(Browse.DF,dplyr::desc(get(rating.type.new)),name)
  DF<-dplyr::filter(DF,type==Type)
  DF<- utils::head(DF,5)
  DF
}
