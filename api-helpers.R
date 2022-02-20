
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
        mutate(type = case_when(
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
    pull(filter(juris(), type %in% type), code)
}

my.query <- function(jur="CT", yr="current", grd="4") {
    ## Build the 'query' portion of API call.
    ## For now, only parameterized over jurisdiction, year, and grade.
    ## See NAEP API docs for details:
    ## https://www.nationsreportcard.gov/api_documentation.aspx
    retval <- paste("type=data",
                    "subject=reading",
                    "subscale=RRPCM",      ## RRPCM = composite scale
                    "stattype=MN:MN",      ## return jurisdiction-wise mean values for 'subscale'
                    paste0("grade=", grd), ## 4,8,12
                    paste0("year=", yr),   ## 'current' == most recent year; can also give numeric YYYY
                    "variable=SDRACE",
                    paste0("jurisdiction=", jur),
                    sep="&")
    retval

}
