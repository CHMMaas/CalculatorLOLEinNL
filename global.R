#####
##### Tool to display Loss of Life Exepctancy (LOLE) estimates
##### Author: C. Maas
#####
library(shiny)
library(shinyjs) # update after clicking calculate
library(shinydashboard)
library(dashboardthemes)
library(DT) # for tables
# icons shiny: https://fontawesome.com/v5.15/icons?d=gallery&p=2
# colors shiny: https://rstudio.github.io/shinydashboard/appearance.html

#####
##### Obtain LOLE estimates for all cancer types
#####
# library(haven)
# results.LOLE <- read_dta('G:/IKNL/Registratie en Onderzoek/Onderzoek/projecten lopend/LOLE/Hoog-over/Data/file_for_tool.dta')
# save(results.LOLE, file='LOLE_estimates.Rdata')
load(file='LOLE_estimates.Rdata')
results.LOLE[is.na(results.LOLE)] <- 999

#####
##### Create library to obtain abbreviations from
#####
index.names <- function(){
  library <- data.frame(full = c("Bladder, urinary tract",
                      "Cervix",
                      "Central nervous system",
                      "Colorectum",
                      "Esophagus, cardia, stomach",
                      "Endometria",
                      "Female breast",
                      "Head, neck",
                      "Hepato, pancreato, biliary",
                      "Kidney",
                      "Lung",
                      "Skin melanoma",
                      "Ovary, fallopian tube",
                      "Prostate",
                      "Squamous cell carcinoma",
                      "Testicle",
                      "Thyroid",
                      "Overall"),
                   abbr = c("BLAD", "CERV", "CNS", "CRC", "ECS", "ENDO", "FBRE", 
                            "HN", "HPB", "KIDN", "LUNG", "MEL", "OFT", "PROST", 
                            "SCC", "TEST", "THY", "ALL"))
  return(library)
}

#####
##### Convert strings to numeric values
#####
string.to.num <- function(input){
  # string to numeric
  if (input$gender == "Male"){
    gender.num <- 1
  }
  else if (input$gender == "Female"){
    gender.num <- 2
  }
  if (input$stage == "Localized" | input$stage == "Not applicable"){
    stage.num <- 1
  }
  else if (input$stage == "Regional"){
    stage.num <- 2
  }
  else if (input$stage == "Distant"){
    stage.num <- 3
  }
  type.abbr <- subset(index.names(), full == input$type)$abbr
  
  return(list(gender=gender.num, stage=stage.num, type=type.abbr))
}