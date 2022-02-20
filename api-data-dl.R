library(httr)
library(jsonlite) ## NAEP API returns data in json format
library(stringr)
library(purrr)
library(here)

source(here("api-helpers.R"))

## API documentation at https://www.nationsreportcard.gov/api_documentation.aspx

## Use expand.grid() to build a df of query sets ,
## rather than one large query. The NAEP API will time-out even
## on queries that are not all that large.

qparms <-
    expand.grid(
        list(
            jur = c("CT", "RI"), ## could also do, e.g.,  jur = juris.by.type(type="State")
            yr = c("2017", "2019"),
            grd = c("4", "8")    ## ranges over c(4,8,12)
        ))

## Pass the df of query sets to helper function my.query()
## to get a list of query strings.
query0 <- pmap_chr(qparms, my.query)

## The base url for the NAEP API
url0 <- "https://www.nationsreportcard.gov/DataService/GetAdhocData.aspx"

## Pass the base url and the list of query strings to modify_url()
## to build set of API calls, each for a portion of the desired query.
api.calls <- map2_chr(.x=url0, .y=query0, .f=~modify_url(url=.x, query=.y))

## Make the API calls and get back a list of return values
resp <- map(api.calls, GET)

## Check the statuses of the individual API calls
map_dfr(resp, http_status)

## Pull the data from the API return values and combine it into
## a single data.frame
D0 <- map_dfr(resp, ~fromJSON(content(., as="text"),
                        simplifyVector = TRUE)$result)
