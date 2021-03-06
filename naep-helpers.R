library(dplyr)

naep.read.cutscores <- function() {
    ## Return cut scores for each reading achievement level in each grade.
    ## Values from: https://www.nationsreportcard.gov/reading/nation/scores/

    retval <- expand.grid(level=c("basic", "proficient", "advanced"),
                          grade=c(4,8,12))

    dplyr::tibble(retval,
           cut=c(208L, 238L, 268L, 243L, 281L, 323L, 265L, 302L, 346L))

}

juris <- function() {
    ## Return a data.frame of jurisdictions recognized by the NAEP API,
    ## categorized by one of c("National", "State", "Metro" "DoDEA").
    ## 'code' and 'name' columns are scraped from NAEP API docs at:
    ## https://www.nationsreportcard.gov/api_documentation.aspx .
    ## 'type' is assigned by FDB.
    juris0 <-
        c("NT",  "National",
          "NP",  "National public",
          "NR",  "National private",
          "NL",  "Large city",
          "AL",  "Alabama",
          "AK",  "Alaska",
          "AZ",  "Arizona",
          "AR",  "Arkansas",
          "CA",  "California",
          "CO",  "Colorado",
          "CT",  "Connecticut",
          "DE",  "Delaware",
          "DC",  "District of Columbia",
          "DS",  "DoDEA",
          "FL",  "Florida",
          "GA",  "Georgia",
          "HI",  "Hawaii",
          "ID",  "Idaho",
          "IL",  "Illinois",
          "IN",  "Indiana",
          "IA",  "Iowa",
          "KS",  "Kansas",
          "KY",  "Kentucky",
          "LA",  "Louisiana",
          "ME",  "Maine",
          "MD",  "Maryland",
          "MA",  "Massachusetts",
          "MI",  "Michigan",
          "MN",  "Minnesota",
          "MS",  "Mississippi",
          "MO",  "Missouri",
          "MT",  "Montana",
          "NE",  "Nebraska",
          "NV",  "Nevada",
          "NH",  "New Hampshire",
          "NJ",  "New Jersey",
          "NM",  "New Mexico",
          "NY",  "New York",
          "NC",  "North Carolina",
          "ND",  "North Dakota",
          "OH",  "Ohio",
          "OK",  "Oklahoma",
          "OR",  "Oregon",
          "PA",  "Pennsylvania",
          "RI",  "Rhode Island",
          "SC",  "South Carolina",
          "SD",  "South Dakota",
          "TN",  "Tennessee",
          "TX",  "Texas",
          "UT",  "Utah",
          "VT",  "Vermont",
          "VA",  "Virginia",
          "WA",  "Washington",
          "WV",  "West Virginia",
          "WI",  "Wisconsin",
          "WY",  "Wyoming",
          "DD",  "DoDEA/DDESS (domestic)",
          "DO",  "DoDEA/DoDDS (overseas)",
          "XQ",  "Albuquerque",
          "XA",  "Atlanta",
          "XU",  "Austin",
          "XM",  "Baltimore City",
          "XB",  "Boston",
          "XT",  "Charlotte",
          "XC",  "Chicago",
          "XX",  "Clark County (NV)",
          "XV",  "Cleveland",
          "XS",  "Dallas",
          "XY",  "Denver",
          "XR",  "Detroit",
          "XW",  "District of Columbia (DCPS)",
          "XE",  "Duval County (FL)",
          "XZ",  "Fort Worth (TX)",
          "XF",  "Fresno",
          "XG",  "Guilford County (NC)",
          "XO",  "Hillsborough County (FL)",
          "XH",  "Houston",
          "XJ",  "Jefferson County (KY)",
          "XL",  "Los Angeles",
          "XI",  "Miami-Dade",
          "XK",  "Milwaukee",
          "XN",  "New York City",
          "XP",  "Philadelphia",
          "XD",  "San Diego",
          "YA",  "Shelby County (TN)",
          "AS",  "American Samoa",
          "GU",  "Guam",
          "PR",  "Puerto Rico",
          "VI",  "Virgin Islands")

    retval <-
        matrix(juris0, ncol=2, byrow = TRUE,
               dimnames=list(NULL, c("code", "name"))) %>%
        as_tibble() %>%
        mutate(group = case_when(
                   str_detect(code, "^(X|Y|NL)") ~ "Metro",
                   code %in% c("NT", "NP", "NR") ~ "National",
                   code %in% c("AS", "GU", "PR", "VI") ~ "Territory",
                   code %in% c("DS", "DD", "DO") ~ "DoDEA",
                   TRUE ~ "State"
               ))
    retval

}

juris.by.type <- function(type=c("State", "National", "Metro", "Territory", "DoDEA")){
    ## Make it easy to pull jurisdictions by type.
    pull(filter(juris(), group %in% type), code)
}

my.query <- function(subj="reading",
                     subscale="RRPCM",
                     stat="MN:MN",
                     grd="4",
                     yr="current",
                     var="SDRACE",
                     jur="CT") {
    ## Build the 'query' portion of API call.

    ## See NAEP API docs for usable values, although their list not complete:
    ## https://www.nationsreportcard.gov/api_documentation.aspx
    retval <- paste("type=data",
                    paste0("subject=", subj),
                    paste0("subscale=", subscale), ## RRPCM = composite scale
                    paste0("stattype=", stat),     ## return jurisdiction-wise mean values for 'subscale'
                    paste0("grade=", grd),         ## 4,8,12
                    paste0("year=", yr),           ## 'current' == most recent year; can also give numeric YYYY
                    paste0("variable=", var),      ## C051701 == Title 1 status; SDRACE == race/ethn for trends
                    paste0("jurisdiction=", jur),
                    sep="&")
    retval
}

resp.content.enc <- function(ll) {
    ## This is hackish, but works for use cases that I've seen so far.
    ## When no useful content is returned for a query, the content encoding
    ## header will be missing. If useful content is present, then encoding is
    ## "gzip".
    if(is.null(ll$headers$"content-encoding")) NA_character_
    else ll$headers$"content-encoding"
}

