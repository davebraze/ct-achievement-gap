## API documentation at https://www.nationsreportcard.gov/api_documentation.aspx

library(httr)
library(jsonlite) ## NAEP API returns data in json format

url <- "https://www.nationsreportcard.gov/DataService/GetAdhocData.aspx"

tmp <- GET(url)

query <- paste("type=data",
               "subject=reading",
               "subscale=RRPCM",  ## composite scale
               "stattype=MN:MN",     ## return mean values
               "grade=4",         ## 4,8,12
               "year=Current",    ## most recent year; can also give numeric YYYY
               "variable=SDRACE",
               "jurisdiction=NL",
               sep="&")

resp <- GET(modify_url(url = url, query = query))

http_status(resp)
http_type(resp)

D <- fromJSON(content(resp, as = "text"),
              simplifyVector = TRUE)


juris <-
    c( "NT",  "National",
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
      "DD",  "DoDEA/DDESS",
      "DO",  "DoDEA/DoDDS",
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

matrix(juris, ncol=2, byrow = TRUE)
