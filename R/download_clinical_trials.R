
#' Download CT.gov trials as a tibble
#'
#' @param search_query : character, query to be fetched using CT.gov api
#'
#' @return df : tibble
#'
#' @export
#'
#' @examples
#'
#' df_cancer <- download_clinical_trials(search_query = "cancer")
#' df_heart_attack <- download_clinical_trials(search_query = "heart attack")
#' df_complex_query <- download_clinical_trials(search_query = "covid19 AND (new york OR italy)")
download_clinical_trials <- function(search_query){

  if (is.null(search_query)){
    warning('Search query is empty.\nReturning total trials available.')
    search_query = 'ALL'
  } else if (gsub(' ','',search_query) == '')
  {
    warning('Search query is empty.\nReturning total trials available.')
  }

  if (class(search_query) != 'character')stop('Please enter only character fields.')


  df <- dplyr::tibble() # Empty Data frame


  to_old <- count_clinical_trials(search_query)

  if (to_old>=1000){
    warning('\nLarge Data Files!\nDownload will take time.')
  }

  if (to_old >= 10000){
    raise_warning <- glue::glue("\nAvailable trials :", to_old, '\n\nFirst 10000 trials will be loaded!')
    warning(raise_warning)
    to_old <- 10000
  } else {
    to_old <- to_old
  }


  # URL Generator
  generate_url <- function(min, max){

    # TODO: Make 'fields' params agnostic!!!

    min <- as.character(min)
    max <- as.character(max)
    BASE_URL <- "https://clinicaltrials.gov/api/query/study_fields?expr="
    search_query <- toupper(gsub(" ", "+", search_query, fixed = T)) # Refer https://stackoverflow.com/questions/35655485/replace-with-space-using-gsub-in-r

    # TODO: Update this list
    fields = list('NCTId', 'BriefTitle', 'Condition',
                  'OverallStatus','LeadSponsorClass','LeadSponsorName',
                  'InterventionName','InterventionType','Keyword')

    fields = paste0(fields,collapse = '%2C')

    fields = paste0('&fields=', fields)

    end_url <- glue::glue("&min_rnk=",{min},"&max_rnk=",{max},  "&fmt=json")

    full_url <- paste0(BASE_URL,search_query, fields,end_url)
    full_url <- URLencode(full_url)

    return(full_url)
  }

  # URL List
  get_urls <- function(query, to=to_old){


    rank_matrix <- function(to=to){
      min_rank = as.vector(seq(1, to, by = 1000))
      max_rank = sapply(min_rank, function(t) t+999)
      max_rank[which(max_rank == max(max_rank))] = to # Replace last max_rank with available Trials number

      ranks_matrix = rbind(min_rank, max_rank)

      return(ranks_matrix)
    }

    min_max_matrix = rank_matrix(to)

    full_urls = c() # Create an empty vector

    for (col in 1:ncol(min_max_matrix)){
      url <- generate_url(
        min = min_max_matrix[,col][1],
        max = min_max_matrix[,col][2])

      full_urls <- c(full_urls,url)
    }

    return(full_urls)
  }

  urls <- get_urls(query = search_query, to = to_old)
  #urls <- trials_unique_urls(query = search_query)



  if (to_old>= 1000)
    warning('Large no of Files.\nDownloading Data will take time!')

  get_url_data <- function(url){
    Sys.sleep(6) # Refer Crawl-delay parameter in http://www.clinicaltrials.gov/robots.txt
    request <- httr::GET(url)
    request_content <- httr::content(request, as = 'text')
    content_json <- jsonlite::fromJSON(request_content)
    df1 <- dplyr::as_tibble(content_json$StudyFieldsResponse$StudyFields)

    return(df1)

  }

  # Save Data
  for (url in urls){
    Sys.sleep(1)
    df2 <- get_url_data(url = url)

    df <- dplyr::bind_rows(df,df2)
  }

  return(df)
}
