init_rAPI <- function(baseUrl) {

  require("httr")
  require("jsonlite")

  # rAPI Base
  rAPI <- list()
  rAPI$base <- baseUrl

  # rAPI Shared
  rAPI$shared <- list()

  rAPI$shared$getCallingFunctionsName <- function(level = 1) {
    return (gsub(".*\\.", "", sys.calls()[[sys.nframe()-level]]))
  }

  rAPI$shared$getCallingFunctionsParameters <- function(level = 1) {
    parameters <- as.list(parent.frame(n = level))
    return (parameters)
  }

  rAPI$shared$getUrl <- function(urlEndpointPart, parameters = NULL) {
    urlParametersPart <- rAPI$shared$getParametersUrlPart(parameters)
    return (paste(rAPI$base, urlEndpointPart, urlParametersPart, sep=""))
  }

  rAPI$shared$getParametersUrlPart <- function(parameters = NULL) {
    if (is.null(parameters) || length(parameters) == 0) {
      return ("")
    }
    parametersUrlPart <- "?"
    parameterNames <- names(parameters)
    for (i in 1:length(parameterNames)) {
      sep <- ""
      if (i > 1) {
        sep <- "&"
      }
      parameterName = parameterNames[i]
      parameterValue = get(parameterName, parameters)
      parametersUrlPart <- paste(parametersUrlPart, sep, parameterName, "=", parameterValue, sep="")
    }
    return (parametersUrlPart)
  }

  # rAPI API Internals
  rAPI$api <- list()

  rAPI$api$generic <- function(urlEndpointPart, getUrl=NULL) {
    parameters <- rAPI$shared$getCallingFunctionsParameters(level = 2)
    if (is.null(getUrl)) {
      url <- rAPI$shared$getUrl(urlEndpointPart = urlEndpointPart, parameters = parameters)
    } else {
      url <- getUrl(urlEndpointPart = urlEndpointPart, parameters = parameters)
    }
    return (rAPI$api$request(url = url))
  }

  rAPI$api$request <- function(url) {
    #cat(paste("GET", url))
    message(paste("GET", url))
    response <- list()
    response$raw <- GET(url)
    retrySleepTime <- 1
    while (retrySleepTime < 10) {
      if (response$raw$status_code != 200) {
        Sys.sleep(retrySleepTime)
        response$raw <- GET(url)
        retrySleepTime <- retrySleepTime + 1
      } else {
        break
      }
    }
    response$content <- list()
    response$content$raw <- content(response$raw, "text")
    response$content$parsed <- fromJSON(response$content$raw, flatten = TRUE)
    return(response)
  }

  return (rAPI)
}
