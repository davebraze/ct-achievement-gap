library(httr)
library(jsonlite) ## NAEP API returns data in json format
library(stringr)
library(purrr)
library(here)
library(lubridate)

source(here("naep-helpers.R"))

## API documentation at https://www.nationsreportcard.gov/api_documentation.aspx

## Use expand.grid() to build a df of query sets ,
## rather than one large query. The NAEP API will time-out even
## on queries that are not all that large.

qparms <-
    expand.grid(
        list(
            jur = c(juris.by.type("State"), "DS"),
            yr = c(2017, 2019),
            grd = c("4", "8"),
            var = c("SDRACE", "C051701")
        ))

## Pass the df of query sets to helper function my.query()
## to get a list of query strings.
query0 <- pmap_chr(qparms, my.query)

## The base url for the NAEP API
endpoint <- "https://www.nationsreportcard.gov/DataService/GetAdhocData.aspx"

## Pass the base url and the list of query strings to modify_url()
## to build set of API calls, each for a portion of the desired query.
api.calls <- map2_chr(.x=endpoint, .y=query0, .f=~modify_url(url=.x, query=.y))

## Make the API calls and get back a list of return values. May take a while to complete.
resp <- map(api.calls, GET)

## Check that all API calls return status "Success"
all(str_detect(map_dfr(resp, http_status)$category, "Success"))

## "Success" status is not sufficient.
## So, use resp.content.enc() to identify responses with useful content.

## Pull the data from the list of API return values and combine it into
## a single data.frame, but first check to be sure each response
## in the list contains useful values.
D0 <-
    resp[!is.na(map_chr(resp, resp.content.enc))] %>%
    map_dfr(~fromJSON(content(., as="text"),   ## value in doing rawToChar(resp) %>% fromJSON(...)?
                      simplifyVector = TRUE)$result)

outfile <- here("00-data-cache", paste0("D0_", today(), ".rds"))
saveRDS(D0, file=outfile)

