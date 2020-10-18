---
output: html_document
runtime: shiny
---

library(rmarkdown)
library(ggplot2)
library(knitr)
library(tidyverse)
library(magrittr)
library(kableExtra)
library(shiny)

opts_chunk$set(message = FALSE)


##  Acquire and read the data

#These data were collected from the USDA database selector: <a href="https://quickstats.nass.usda.gov">https://quickstats.nass.usda.gov</a>

#The data were <a href="https://quickstats.nass.usda.gov/results/D416E96E-3D5C-324C-9334-1D38DF88FFF1">stored online</a> and then downloaded as a CSV file.




## read the data

ag_data <- read_csv("berries.csv", col_names = TRUE)

## look at number of unique values in each column
ag_data %>% summarize_all(n_distinct) -> aa


## make a list of the columns with only one unique value
bb <- which(aa[1,]==1)

## list the 1-unique value column names 
cn <- colnames(ag_data)[bb]


## remove the 1-unique columns from the dataset
ag_data %<>% select(-all_of(bb))

aa %<>% select(-all_of(bb)) 


## State name and the State ANSI code are (sort of) redundant
## Just keep the name
ag_data %<>% select(-4)
aa %<>% select(-4) 


kable(head(ag_data)) %>% kable_styling(font_size=12)


berry <- unique(ag_data$Commodity)
nberry <- length(berry)


#This table contains informaton about `r nberry` berries: blueberries, raspberries, and strawberries.

#When the data have been cleaned and organized, the three kinds of berries will be separated into tables with the same structure so that they can be compared.  
#So, working with Blueberries along demonstrates how the data will be cleaned and organized for all three kinds of berries. Only the "YEAR" time period will be considered.

## Separate blueberries, rasberries, and strawberries

bberry <- ag_data %>% filter((Commodity=="BLUEBERRIES") & (Period=="YEAR"))
bberry %<>% select(-c(Period, Commodity)) 

rberry <- ag_data %>% filter((Commodity=="RASPBERRIES") & (Period=="YEAR"))
rberry %<>% select(-c(Period, Commodity)) 

sberry <- ag_data %>% filter((Commodity=="STRAWBERRIES") & (Period=="YEAR"))
sberry %<>% select(-c(Period, Commodity)) 


#### Does every Data Item begin with "
sum(str_detect(bberry$`Data Item`, "^BLUEBERRIES, ")) == length(bberry$`Data Item`)


di <- bberry$`Data Item`
di_m <- str_split(di, ",", simplify=TRUE)
dim(di_m)

unique(di_m[,1])
di_m <- di_m[,2:4]

bberry %<>% separate(`Data Item`, c("B","type", "meas", "what"), sep = ",") 
bberry %<>% select(-B)

head(bberry$type, n=20)
ty <- str_split(bberry$type, " ", simplify=TRUE)
head(ty, n=20)

bberry %<>% separate(type,c("b1", "type", "b2", "lab1", "lab2"), " ")

bberry %<>% select(-c(b1,b2)) 

bberry[is.na(bberry)] <- " "  ## OK now Data Item has been split into parts


## onto Domain

bberry$Domain %>% unique()

bberry[is.na(bberry)] <- " "

kable(head(bberry, n=10)) %>% kable_styling(font_size=12)



#Repeat the same steps for Raspberries
sum(str_detect(sberry$`Data Item`, "^RASPBERRIES, ")) == length(sberry$`Data Item`)


di_r <- rberry$`Data Item`
di_m_r <- str_split(di_r, ",", simplify=TRUE)
dim(di_m_r)

unique(di_m_r[,1])
di_m_r <- di_m_r[,2:4]

rberry %<>% separate(`Data Item`, c("B","type", "meas", "what"), sep = ",") 
rberry %<>% select(-B)

head(rberry$type, n=20)
ty <- str_split(rberry$type, " ", simplify=TRUE)
head(ty, n=20)

rberry %<>% separate(type,c("b1", "type", "b2", "lab1", "lab2"), " ")

rberry %<>% select(-c(b1,b2)) 

rberry[is.na(rberry)] <- " "  ## OK now Data Item has been split into parts

## onto Domain


rberry$Domain %>% unique()

kable(head(rberry, n=10)) %>% kable_styling(font_size=12)


#Repeat the same steps for Strawberries

sum(str_detect(sberry$`Data Item`, "^STRAWBERRIES, ")) == length(sberry$`Data Item`)


di_s <- sberry$`Data Item`
di_m_s <- str_split(di_s, ",", simplify=TRUE)
dim(di_m_s)

unique(di_m_s[,1])
di_m_s <- di_m_s[,2:4]

sberry %<>% separate(`Data Item`, c("B","type", "meas", "what"), sep = ",") 
sberry %<>% select(-B)

head(sberry$type, n=20)
ty <- str_split(sberry$type, " ", simplify=TRUE)
head(ty, n=20)

sberry %<>% separate(type,c("b1", "type", "b2", "lab1", "lab2"), " ")

sberry %<>% select(-c(b1,b2)) 

sberry[is.na(sberry)] <- " "  ## OK now Data Item has been split into parts

## onto Domain
sberry$Domain %>% unique()

kable(head(sberry, n=10)) %>% kable_styling(font_size=12)


# Make a data frame for Blueberries
data_bb <- as.data.frame(bberry)
data_bb <- data.frame(bberry[1], bberry[2], bberry[8], bberry[9])
data_bb <- data_bb[,which(unlist(lapply(data_bb, function(x) !all(is.na(x)))))]


# Make a data frame for Raspberries
data_rr <- as.data.frame(rberry)
data_rr <- data.frame(rberry[1], rberry[2], rberry[8], rberry[9])
data_rr <- data_rr[,which(unlist(lapply(data_rr, function(x) !all(is.na(x)))))]


#Make a data frame for Strawberries
data_ss <- as.data.frame(sberry)
data_ss <- data.frame(sberry[1], sberry[2], sberry[8], sberry[9])
data_ss <- data_ss[,which(unlist(lapply(data_ss, function(x) !all(is.na(x)))))]


# Make a bar plot of Domain for Blueberries
png(width=200, height =100)
plot <- ggplot(data=data_bb, aes(x=Domain), color="blue") + geom_bar()
dev.off()

# Make a bar plot of Domain for Raspberries
png(width=200, height =100)
plot2 <- ggplot(data=data_rr, aes(x=Domain), color="blue") + geom_bar()
dev.off()

# Make a bar plot of Domain for Strawberries
png(width=200, height =100)
plot3 <- ggplot(data=data_ss, aes(x=Domain), color="blue") + geom_bar()
dev.off()


# Shiny App ui
# Make three tab panels for each Bluberries, Raspberries, and Strawberries
ui <- fluidPage(
  
  title = "Examples of Data Tables",
  sidebarLayout(
    tabsetPanel(
      conditionalPanel(
        'input.dataset === "data_bb"'),
      
      conditionalPanel(
        'input.dataset === "data_rr"')
    ),
    mainPanel(
      
      tabsetPanel(
        id = 'dataset',
        tabPanel("Blueberries Table",
                 plotOutput("plot"),
                 # Create a new Row in the UI for selectInputs
                 fluidRow(
                   column(4,
                          selectInput("State",
                                      "State:",
                                      c("All",
                                        unique(as.character(data_bb$State))))
                   ),
                   column(4,
                          selectInput("Year",
                                      "Year:",
                                      c("All",
                                        unique(data_bb$Year)))
                   )
                 ),
                 # Create a new row for the table.
                 DT::dataTableOutput("table1")),
        
        tabPanel("Raspberries Table",
                 plotOutput("plot2"),
                 # Create a new Row in the UI for selectInputs
                 fluidRow(
                   column(4,
                          selectInput("State",
                                      "State:",
                                      c("All",
                                        unique(as.character(data_rr$State))))
                   ),
                   column(4,
                          selectInput("Year",
                                      "Year:",
                                      c("All",
                                        unique(data_rr$Year)))
                   )
                 ),
                 # Create a new row for the table.
               
                 DT::dataTableOutput("table2")),
        
        tabPanel("Strawberries Table",
                 plotOutput("plot3"),
                 # Create a new Row in the UI for selectInputs
                 fluidRow(
                   column(4,
                          selectInput("State",
                                      "State:",
                                      c("All",
                                        unique(as.character(data_ss$State))))
                   ),
                   column(4,
                          selectInput("Year",
                                      "Year:",
                                      c("All",
                                        unique(data_ss$Year)))
                   )
                 ),
                 # Create a new row for the table.
                 
                 DT::dataTableOutput("table3")
        
        
        
      )
    )
  )
))


# Server
server <- function(input, output) {
  
  # Filter data based on selections 
  # Make a table and a bar plot
  #Blueberries
  output$table1 <- DT::renderDataTable(DT::datatable({
    data <- data_bb
    
    if (input$State != "All") {
      data <- data[data$Period == input$State,]
    }
    if (input$Year != "All") {
      data <- data[data$Year == input$Year,]
    }
    
    data
    
  }))

  output$plot <- renderPlot({
    p<- ggplot(data=data_bb, aes(x=Domain)) + geom_bar()
    print(p)
  })
  
  # Raspberries
  output$table2 <- DT::renderDataTable(DT::datatable({
    data2 <- data_rr
    if (input$State != "All") {
      data2 <- data2[data2$Period == input$State,]
    }
    if (input$Year != "All") {
      data2 <- data2[data2$Year == input$Year,]
    }
    
    data2
  }))
 
  output$plot2 <- renderPlot({
    p2<-ggplot(data=data_rr, aes(x=Domain)) + geom_bar()
    print(p2)
  })

  # Strawberries
  output$table3 <- DT::renderDataTable(DT::datatable({
    data3 <- data_ss
    if (input$State != "All") {
      data3 <- data3[data3$Period == input$State,]
    }
    if (input$Year != "All") {
      data3 <- data3[data3$Year == input$Year,]
    }
    
    data3
  }))
  
  output$plot3 <- renderPlot({
    p3<-ggplot(data=data_ss, aes(x=Domain)) + geom_bar()
    print(p3)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
