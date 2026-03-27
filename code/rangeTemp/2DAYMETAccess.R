# Created 03/18/2026 by Anneliese Pinnell

# The purpose of this file is to retrieve DAYMET data from AppEEARS

# Important information
# This document involves sending requests (GET and POST) to a server using a JSON
# format. A GET request retrieves data (such as clicking on a link to open a webpage
# - a server sends back the HTML website and no modifications are made)
# A POST request sends data to create or update a resource (such as logging-in with
# a username and password)

# The tricky part is formatting coordinates into a list format to add to a JSON
# (what is used to submit the GET/POST request - there are other formats but
# they can be a bit finicky) and is what most of this file is
# Needed format [x,y], [x2,y2],...[xn,yn] <- important to not end with a comma

# I highly, highly suggest doing a test polygon over ~1-2 days to ensure it is
# working the way you want it to

# DAYMET only covers North America

# You must have a login to Earth Data Login to get this to work

library(httr)
library(jsonlite)
library(tmap)
library(geojsonio)
library(purrr)
library(striprtf)

#Get login info
# Username is first line in a txt file, password is 2nd line
txt <- readLines("Secret.txt")

#Read in data from aforementioned txt file
secret <- base64_enc(paste(txt[1], txt[3], sep = ":"))
#Sends a POST request to login to AppEEARS
response <- POST("https://appeears.earthdatacloud.nasa.gov/api/login", 
                 add_headers("Authorization" = paste("Basic", gsub("\n", "", secret)),
                             "Content-Type" = "application/x-www-form-urlencoded;charset=UTF-8"), 
                 body = "grant_type=client_credentials")

#This token tells AppEEARs that you are a valid entity requesting data
#Set to expire in 48hrs (AppEEARS sets this time limit)
token_response <- prettify(toJSON(content(response), auto_unbox = TRUE))
token_response

# Reads in america shp file, simplifies, and ensures validity
americas <- st_cast(st_read("data/temperature/america.shp"), "POLYGON") |>
  st_simplify() |>
  st_make_valid()

# Holder for all polygons in americas
holdCoordFormattedPolygons <- vector("character", nrow(americas))

#
for (i in seq_len(nrow(americas))) {
  poly <- americas[i, ]
  coords <- st_coordinates(poly) |>
    unique()
  #Super important: Adds first coordinates to close ring (prevents errors!!!)
  # Without a closed ring, will not be considered a "valid polygon"
  coords <- rbind(coords, coords[1, ])
  # Formats coordinates into what is needed for JSON formatting [x,y],
  formattedCoords <- apply(coords, 1, function(r) sprintf("[%f,%f]", r[1], r[2]))
  # Adds commas between all coordinates
  holdCoordFormattedPolygons[i] <- paste(formattedCoords, collapse = ",")
}

# This is the format the coordinates need to be passed in. The coordinates
# go inbetween the two parts of the c() (between [[ ]]). One of these is 
# needed for each polygon (can't put all polygon coordinates in one of these)
neededFormat <- c('{
  "type": "Feature",
  "geometry": {
    "type": "Polygon",
    "coordinates": [
      [',
                  ']
    ]
  },
  "properties": {"id": "Area1"}
},')

# getJsonFormat holds all of the above formatting for all polygons
getJsonFormat <- c()
for (j in seq_len(nrow(americas))){
#for (j in seq_len(1)){
  poly <- holdCoordFormattedPolygons[j]
  getJsonFormat <- paste(c(getJsonFormat, neededFormat[1], poly, neededFormat [2]), collapse = "")
  
}
#Removes last comma (prevents errors!! It will think there is more after and 
# error if this is not deleted!!!) 
getJsonFormat <- substr(getJsonFormat, 1, nchar(getJsonFormat) - 1)

# This is the main JSON format that is passed as a request
# All of the above information that is formatted is thrown in features
# Important info:
# Data is recurring if you want the same dates over the span of years
# If data is recurring, set recurring = true <- do not use uppercase for JSON t/f
# passed in "yearRange"
# endDate: date to end (year not needed if recurring)
# startDate: date to start (year not needed if recurring)
# layers: DAYMET has various different layers and throw in what you need
# type: other things can be put here - check API documentation!
# Task-name: name of the task that will be stored on the AppEEARs website
# task_type: area or point (check API!) - use area for polygons
task2 <- c('{
"params": {
  "geo": {
    "type": "FeatureCollection",
    "features": [
    ', getJsonFormat ,'
    ]
  },
  "dates": [{
    "endDate": "01-02-2020",
    "startDate": "01-01-2020",
    "recurring": false,
    "yearRange": [2020,2020]
  }],
  "layers": [{
    "layer": "tmin",
    "product": "DAYMET.004"
  }],
  "output": {
    "format": {
      "type": "geotiff",
      "filename_date": "calendar"
    },
    "projection": "native"
  }
},
"task_name": "Test North America DayMet ALL #1",
"task_type": "area"
}')

#Collapses the task to have no spaces
theRequest <- paste(task2, collapse = "")

#Formats appropriatley into JSON
task <- fromJSON(theRequest)
task <- toJSON(task, auto_unbox=TRUE)

# Submit the task request
token <- paste("Bearer", fromJSON(token_response)$token)
response <- POST("https://appeears.earthdatacloud.nasa.gov/api/task", body = task, encode = "json", 
                 add_headers(Authorization = token, "Content-Type" = "application/json"))

# Outputs into console information regarding whether the task submission was sucsessful
# Should get an email to your AppEEARs login that the task was sucsessfully submitted
# Else, check task_response to ensure it says "pending" and no errors were thrown
# Since accessing data is on the AppEEARs server, you can do other stuff in R while it
# collects your data! Yay!!
task_response <- prettify(toJSON(content(response), auto_unbox = TRUE))
task_response

# If you need to see the output json for any reason (troubleshooting), uncomment
#write_json(task, "outputJson.json", auto_unbox = TRUE, pretty = TRUE)

