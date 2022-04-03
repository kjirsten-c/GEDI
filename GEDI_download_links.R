#### START ####


# Import Packages
#install.packages("httr")
library(httr)

# Define Function to Query Common Metadata Repository (CMR)
gedi_finder <- function(product, bbox) {
  
  # Define the base CMR granule search url, including LPDAAC provider name and max page size (2000 is the max allowed)
  cmr <- "https://cmr.earthdata.nasa.gov/search/granules.json?pretty=true&provider=LPDAAC_ECS&page_size=2000&concept_id="
  
  # Set up dictionary where key is GEDI shortname + version and value is CMR Concept ID
  concept_ids <- list('GEDI01_B.002'='C1908344278-LPDAAC_ECS', 
                      'GEDI02_A.002'='C1908348134-LPDAAC_ECS', 
                      'GEDI02_B.002'='C1908350066-LPDAAC_ECS')
  
  # CMR uses pagination for queries with more features returned than the page size
  page <- 1
  bbox <- sub(' ', '', bbox)  # Remove any white spaces
  granules <- list()          # Set up a list to store and append granule links to
  
  # Send GET request to CMR granule search endpoint w/ product concept ID, bbox & page number
  cmr_response <- GET(sprintf("%s%s&bounding_box=%s&pageNum=%s", cmr, concept_ids[[product]],bbox,page))
  
  # Verify the request submission was successful
  if (cmr_response$status_code==200){
    
    # Send GET request to CMR granule search endpoint w/ product concept ID, bbox & page number, format return as a list
    cmr_response <- content(GET(sprintf("%s%s&bounding_box=%s&pageNum=%s", cmr, concept_ids[[product]],bbox,page)))$feed$entry
    
    # If 2000 features are returned, move to the next page and submit another request, and append to the response
    while(length(cmr_response) %% 2000 == 0){
      page <- page + 1
      cmr_response <- c(cmr_response, content(GET(sprintf("%s%s&bounding_box=%s&pageNum=%s", cmr, concept_ids[[product]],bbox,page)))$feed$entry)
    }
    
    # CMR returns more info than just the Data Pool links, below use for loop to go through each feature, grab DP link, and add to list
    for (i in 1:length(cmr_response)) {
      granules[[i]] <- cmr_response[[i]]$links[[1]]$href
    }
    
    # Return the list of links
    return(granules)
  } else {
    
    # If the request did not complete successfully, print out the response from CMR
    print(content(GET(sprintf("%s%s&bounding_box=%s&pageNum=%s", cmr, concept_ids[[product]],bbox,page)))$errors)
  }
}

# User-provided inputs (UPDATE FOR YOUR DESIRED PRODUCT AND BOUNDING BOX REGION OF INTEREST)
product <- 'GEDI01_B.002'           # Options include 'GEDI01_B.002', 'GEDI02_A.002', 'GEDI02_B.002'
bbox <- '129.152287,35.504724,129.415958,35.597460'  # bounding box coordinates in LL Longitude, LL Latitude, UR Longitude, UR Latitude format


# Call the gedi_finder function using the user-provided inputs
granules <- gedi_finder(product, bbox)
print(sprintf("%s %s Version 2 granules found.", length(granules), product))

# Export Results
# Set up output textfile name using the current datetime
outName <- sprintf("%s_GranuleList_%s.txt", sub('.002', '_002', product), format(Sys.time(), "%Y%m%d%H%M%S"))

# Save to text file in current working directory
write.table(granules, outName, row.names = FALSE, col.names = FALSE, quote = FALSE, sep='\n')
print(sprintf("File containing links to intersecting %s Version 2 data has been saved to: %s/%s", product, getwd(), outName))



#### Access the LP DAAC Data Pool with R ####


# Load necessary packages into R
#install.packages("sys")
library(sys)
#install.packages("getPass")
library(getPass)
library(httr)


# ---------------------------------SET UP ENVIRONMENT--------------------------------------------- #
# IMPORTANT: Update the line below if you want to download to a different directory (ex: "c:/data/")
dl_dir <- Sys.getenv("HOME")                                 # Set dir to download files to
setwd(dl_dir)                                                # Set the working dir to the dl_dir
usr <- file.path(Sys.getenv("USERPROFILE"))                  # Retrieve home dir (for netrc file)
if (usr == "") {usr = Sys.getenv("HOME")}                    # If no user profile exists, use home
netrc <- file.path(usr,'.netrc', fsep = .Platform$file.sep)  # Path to netrc file


# ------------------------------------CREATE .NETRC FILE------------------------------------------ #

if (file.exists(netrc) == FALSE || grepl("urs.earthdata.nasa.gov", readLines(netrc)) == FALSE) {
  netrc_conn <- file(netrc)
  
  # User will be prompted for NASA Earthdata Login Username and Password below
  writeLines(c("machine urs.earthdata.nasa.gov",
               sprintf("login %s", getPass(msg = "Enter NASA Earthdata Login Username \n (or create an account at urs.earthdata.nasa.gov) :")),
               sprintf("password %s", getPass(msg = "Enter NASA Earthdata Login Password:"))), netrc_conn)
  close(netrc_conn)
}

#### DOWNLOAD ####

# Textfile containing links 
files <- readLines("C:/Users/Kjirs/GEDI01_B_002_GranuleList_20220403163151.txt", warn = FALSE)

# Loop through all files
for (i in 1:length(files)) {
  filename <-  tail(strsplit(files[i], '/')[[1]], n = 1) # Keep original filename
  
  # Write file to disk (authenticating with netrc) using the current directory/filename
  response <- GET(files[i], write_disk(filename, overwrite = TRUE), progress(),
                  config(netrc = TRUE, netrc_file = netrc), set_cookies("LC" = "cookies"))
  
  # Check to see if file downloaded correctly
  if (response$status_code == 200) {
    print(sprintf("%s downloaded at %s", filename, dl_dir))
  } else {
    print(sprintf("%s not downloaded. Verify that your username and password are correct in %s", filename, netrc))
  }
}


#### END ####

# References:

# Cole Krehbiel
# GEDI FINDER Tutorial: https://git.earthdata.nasa.gov/projects/LPDUR/repos/gedi-finder-tutorial-r/browse/GEDI_Finder.R
# DAAC DOWNLOADER in R: https://git.earthdata.nasa.gov/projects/LPDUR/repos/daac_data_download_r/browse/DAACDataDownload.R
# Last Updated: 11/14/2019

#BBOX FINDER: http://bboxfinder.com/#35.542794,129.271282,35.561589,129.312481