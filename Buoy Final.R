library(tidyverse)
library(ggplot2)
library(rstanarm)
library(ggpubr)
library(dplyr)
library(tidyr)
# library(stringr)


### make URLs

url1 <- "https://www.ndbc.noaa.gov/view_text_file.php?filename=44013h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"

years <- c(1998:2018)

urls <- str_c(url1, years, url2, sep = "")

filenames <- str_c("buoy", years, sep = "")

###  Read the data from the website

N <- length(urls)

for (i in 1:N){
  if (i <= 7) {
    suppressMessages(  ###  This stops the annoying messages on your screen.  Do this last.
      assign(filenames[i], read_table(urls[i], col_names = c("YYYY","MM","DD","hh",
                                                             "WDIR","WSPD", "GST","WVHT","DPD","APD",
                                                             "MWD","PRES","ATMP","WTMP","DEWP","VIS","TIDE"), skip = 1)
  ))}
  else {
  suppressMessages(  ###  This stops the annoying messages on your screen.  Do this last.
    assign(filenames[i], read_table(urls[i], col_names = c("YYYY","MM","DD","hh","mm",
                                    "WDIR","WSPD", "GST","WVHT","DPD","APD",
                                    "MWD","PRES","ATMP","WTMP","DEWP","VIS","TIDE"), skip = 2))
  
    )}
    
  file <- get(filenames[i])
  colnames(file)[1] <-"YYYY"
  
  # put '19' in front of 98
  if (i == 1) {
    file[i] = file[i] + 1900
    
  }
  
  if (i <= 3) {
    file[,'TIDE'] = 0
  }
  
  if (i <= 7) {
    file[,'mm'] = 0
  }
  
  if(i == 1){
    MR <- file
  }
  
  else{
    MR <- rbind.data.frame(MR, file)
  
  }
}
MR <- filter(MR, MR$ATMP < 50)
MR$MM <- as.numeric(MR$MM)
two_pm <- MR[MR$hh %in% c("14"), ]

for (i in 1998:2018){
  year <- two_pm[two_pm$YYYY %in% c(i),]
  for (j in 1:12) {
    month <- year[year$MM %in% c(j),]
    if (j == 1){
      ave = mean(month$ATMP)
    } else {
      ave <- rbind(ave, mean(month$ATMP))
    }
  }
  if (i == 1998) {
    data_frame <- rbind.data.frame("year" = i, "ave_temp" <- ave)
  } else {
    data_frame <- cbind.data.frame(data_frame, rbind.data.frame("year" = i, "ave_temp" <- ave))
  }
}
final.df <- as.data.frame(t(data_frame))

colnames(final.df) = c("year","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

# do the plot from here

jj<-ggplot(data=final.df, aes(x=year, y=Jan)) + 
  geom_point() +
  labs(x="Year", y="ATMP", title= "") + geom_smooth(method = lm)
ff<- ggplot(data=final.df, aes(x=year, y=Feb)) +
  geom_point()+
  labs(x="Year", y="ATMP", title= "") +geom_smooth(method = lm)
mm <-ggplot(data=final.df, aes(x=year, y=Mar)) + 
  geom_point() +
  labs(x="Year", y="ATMP", title= "") + geom_smooth(method = lm)
aa<-ggplot(data=final.df, aes(x=year, y=Apr)) + 
  geom_point() +
  labs(x="Year", y="ATMP", title= "") +geom_smooth(method = lm)
mayy<-ggplot(data=final.df, aes(x=year, y=May)) + 
  geom_point() +
  labs(x="Year", y="ATMP", title= "") +geom_smooth(method = lm)
junn<-ggplot(data=final.df, aes(x=year, y=Jun)) + 
  geom_point() +
  labs(x="Year", y="ATMP", title= "")+geom_smooth(method = lm)
jull<-ggplot(data=final.df, aes(x=year, y=Jul)) + 
  geom_point() +
  labs(x="Year", y="ATMP", title= "") +geom_smooth(method = lm)
augg<-ggplot(data=final.df, aes(x=year, y=Aug)) + 
  geom_point() +
  labs(x="Year", y="ATMP", title= "") +geom_smooth(method = lm)
ss<-ggplot(data=final.df, aes(x=year, y=Sep)) + 
  geom_point() +
  labs(x="Year", y="ATMP", title= "") +geom_smooth(method = lm)
oo<-ggplot(data=final.df, aes(x=year, y=Oct)) + 
  geom_point() +
  labs(x="Year", y="ATMP", title= "") +geom_smooth(method = lm)
novv<-ggplot(data=final.df, aes(x=year, y=Nov)) + 
  geom_point() +
  labs(x="Year", y="ATMP", title= "") +geom_smooth(method = lm)
dd<-ggplot(data=final.df, aes(x=year, y=Dec)) + 
  geom_point() +
  labs(x="Year", y="ATMP", title= "") +geom_smooth(method = lm)
figure <- ggarrange(jj, ff, mm, aa, mayy, junn, jull, augg, ss, oo, novv, dd + rremove("x.text"), 
          labels = c("JAN", "FEB", "MAR", "APR", "MAY","JUNE","JULY","AUG","SEP","OCT","NOV","DEC"),
          ncol = 4, nrow = 3)
annotate_figure(figure,
                top = text_grob("Buoy Project", color = "red", face = "bold", size = 14))
