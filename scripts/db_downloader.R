library(curl)
library(zip)
library(here)

#I'm not using rdrop2 because fun fact rdrop2 is in need of a maintainer and may be archived soon

#dropbox link to  data (TO DO get exact filepath)
dropbox_link <- "https://www.dropbox.com/sh/35zcd3v72hg2c6q/AAAjBb8LfcbsdhugwxpUDEHqa?dl=1"
destination_dropbox <- file.path(here("data_input"), "dropbox_data.zip")
#download dropbox folder as a zip file
curl::multi_download(url = dropbox_link, destfile = destination_dropbox)
#unzip the file
zip::unzip(zipfile = destination_dropbox, exdir = here("data_input", "mc_dropbox_data", "unzipped"))

