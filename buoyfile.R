library(tidyverse)
# library(stringr)


### make URLs

url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=mlrf1h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"

years <- c(1987:2016)

urls <- str_c(url1, years, url2, sep = "")

filenames <- str_c("mr", years, sep = "")

###  Read the data from the website

N <- length(urls)

for (i in 1:N){
  suppressMessages(  ###  This stops the annoying messages on your screen.  Do this last.
    assign(filenames[i], read_table(urls[i], col_names = TRUE))
  )
  
  file <- get(filenames[i])
  
  colnames(file)[1] <-"YYYY"
  
  # put '19' in front of 2 digit years
  # check that all columns are included
  # filter down to only the 1 daily observation that you want
  # etc etc etc
  
  if(i == 1){
    MR <- file
  }
  
  else{
    MR <- rbind.data.frame(MR, file)
  }
  
  
  
}
