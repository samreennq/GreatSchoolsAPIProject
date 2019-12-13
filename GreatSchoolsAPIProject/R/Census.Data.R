#' Extract School Census data from Great Schools API
#'
#' Goes through an already extracted list of School Search results to compile a dataframe of school
#' and relavant census data. Includes school names, location, contact information, name of school
#' officials as well as enrollment, racial/ethnic breakdown and other demographic information.
#' Schools are indexed by gsId.
#'
#' @param df Name of a dataframe with school search results.
#' @param AppKey API Key.
#'
#' @return Use School Search data from a dataset compiled using Browse.Schools to create a
#' new dataset with school census data.
#'
#' Census.Data(Browse.Schools.NY,Sys.getenv("GreatSchoolsAPI"))
#'
#' @export


Census.Data<-function(df, AppKey=Sys.getenv("GreatSchoolsAPI")){
  CD<-data.frame(gsId=as.character(),
                 stringsAsFactors = FALSE)
  endpoint<-"https://api.greatschools.org/school/census/"
  vars<-as.list(df[,1])
  vars<-vars[[1]]
  for(i in 1:nrow(df)){
    gsId<-vars[i]
    gsId<-as.character(gsId)
    state<-df$state[i]
    State<-paste(state, gsId, sep="/")
    AppKeyNew<-paste("?key=",AppKey, sep="")
    url<-paste(endpoint, State, AppKeyNew, sep="")
    census.response<-httr::GET(url)
    if (census.response$status_code!=200) {
      next
    }
    census.data<-suppressMessages(httr::content(census.response, as ="text"))
    census.data.xml<-xml2::read_xml(census.data)
    data <- XML::xmlParse(census.data.xml)
    data<-XML::xmlToList(data)
    data1<-as.data.frame(rlist::list.remove(data, "ethnicities"))
    data2<-data[["ethnicities"]]
    if (!is.null(data2)){
      data3<-suppressWarnings(purrr::map_df(data2,data.frame))
      data3<-t(data3)
      my.names<-data3[1,]
      colnames(data3) <- my.names
      data3<-as.data.frame(data3)
      data3<-data3[-1,]
      data3<-data3[-2,]
      gsId<-as.data.frame(gsId)
      data1<-cbind(gsId,data1,data3)
    }
    data1
    if(colnames(data1)[ncol(data1)] == "Native Hawaiian or Other Pacific Islander") {
      data1$'Native Hawaiian or Other Pacific Islander' == data1$'Pacific Islander'
    }
    CD<-suppressWarnings(dplyr::bind_rows(CD,data1))
  }
  CD
}
