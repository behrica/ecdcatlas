#' Downloads measures as data frame from altas API at
#' http://atlas.ecdc.europa.eu/public/AtlasService/rest/GetMeasuresResultsExportFile
#' @importFrom magrittr %>%
#' @param healthTopicId The healthTopicId to query data for
#' @export
GetMeasuresResultsExportFile <- function(healthTopicId) {
    readr::read_csv(paste0("http://atlas.ecdc.europa.eu/public/AtlasService/rest/GetMeasuresResultsExportFile?healthTopicId=",healthTopicId,
                    "&datasetId=27&measurePopulation=&measureIds=&measureTypes=I,Q&timeCodes=&geoCodes=&geoLevel=2&timeUnit=Year")) %>%
      dplyr::mutate(NumValue = ifelse(NumValue == "-",NA,NumValue),
             NumValue = as.numeric(NumValue))
}

#' Requests all helath topics from API.
#' Calls endpoint http://atlas.ecdc.europa.eu/public/AtlasService/rest/GetHealthTopics
#' @export
GetHealthTopics = function() {
  httr::GET("http://atlas.ecdc.europa.eu/public/AtlasService/rest/GetHealthTopics") %>%
    httr::content(as = "text") %>%
  tidyjson::as.tbl_json() %>%
    tidyjson::enter_object("HealthTopics") %>%
    tidyjson::gather_array() %>%
    tidyjson::spread_values(Label = tidyjson::jstring("Label"),
                  Code = tidyjson::jstring("Code"),
                  Id = tidyjson::jstring("Id")) %>%
    dplyr::select(Id,Label,Code)


}
