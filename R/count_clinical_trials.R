

#' Find the No. of Trials in CT.gov website for a query
#'
#' @param query : string
#'
#' @return found_studies : integer, No. of Studies available in CT.gov database against a query.
#' @importFrom utils URLencode
#' @export
#'
#' @examples
#' count_clinical_trials(query = "nsclc")
#'
#' count_clinical_trials(query = "covid19 AND (new york OR italy)")
#'
count_clinical_trials <- function(query = NULL){

  if (is.null(query)){
    warning('Search query is empty.\nReturning total trials available.')
    query = 'ALL'
  } else if (gsub(' ','',query) == '')
    {
    warning('Search query is empty.\nReturning total trials available.')
  }

  if (class(query) != 'character')stop('Please enter only character fields.')

  # TODO: Make Fields function agnostic!!!
  BASE_URL <- "https://clinicaltrials.gov/api/query/study_fields?expr="
  search_query <- toupper(gsub(" ", "+", query, fixed = T)) # Refer https://stackoverflow.com/questions/35655485/replace-with-space-using-gsub-in-r

  # TODO: Dynamically update this list
  fields = list('NCTId', 'BriefTitle', 'Condition',
                'OverallStatus','LeadSponsorClass','LeadSponsorName',
                'InterventionName','InterventionType','Keyword')

  fields = paste0(fields,collapse = '%2C')

  fields = paste0('&fields=', fields)

  end_url <- glue::glue("&min_rnk=1&max_rnk=1","&fmt=json") # Retrieve only 1 result

  full_url <- paste0(BASE_URL,search_query, fields,end_url)
  full_url <- URLencode(full_url)

  ## GET Results

  result <- httr::GET(full_url)

  result_content <- httr::content(result, as = 'text')

  content_json <- jsonlite::fromJSON(result_content)

  glimpse_result <- dplyr::as_tibble(content_json) # TODO: Improve this!

  found_studies <- as.integer(glimpse_result$StudyFieldsResponse$NStudiesFound)
  return(found_studies)

}
