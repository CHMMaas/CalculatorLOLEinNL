shiny::shinyServer(
  function(input, output, session){
    observe({
      num.val <- string.to.num(input)
      
      if (num.val$type == "SCC"){
        shiny::updateSelectInput(session, "stage", choices = c("Not applicable"))
      }
      if (num.val$type != "SCC"){
        shiny::updateSelectInput(session, "stage", choices = c("Localized", "Regional", "Distant"))
      }
      
      if (num.val$type == "PROST" | num.val$type == "TEST"){
        shiny::updateSelectInput(session, "gender", choices = c("Male"))
      }
      else if (num.val$type == "CERV" | num.val$type == "OFT" |
               num.val$type == "ENDO" | num.val$type == "FBRE"){
        shiny::updateSelectInput(session, "gender", choices = c("Female"))
      }
      else if (num.val$type == "BLAD" | num.val$type == "CNS" |
               num.val$type == "CRC" | num.val$type == "ECS" |
               num.val$type == "HN" | num.val$type == "HPB"|
               num.val$type == "KIDN" | num.val$type == "LUNG" |
               num.val$type == "MEL" | num.val$type == "SCC" |
               num.val$type == "THY" | num.val$type == "ALL") {
        shiny::updateSelectInput(session, "gender", choices = c("Male", "Female"))
      }
    })
    
    output$survexp_box <- shinydashboard::renderInfoBox({
      num.val <- string.to.num(input)
      if (num.val$type == "SCC"){
        stage.dummy <- 999
      }
      else{
        stage.dummy <- 1
      }
      
      survexp <- subset(results.LOLE, type == num.val$type & geslacht == num.val$gender & 
                          age == input$age & jaar == input$year & 
                          eval(as.name(paste("stage", num.val$stage, sep=""))) == stage.dummy)[[paste("survexp_", input$survived, sep="")]]
      shinydashboard::infoBox("The life expectancy of the Dutch population is", 
              paste(round(survexp, 1), "years"), 
              icon = icon("heart"), color = "aqua"
      )
    })
    
    output$survobs_box <- shinydashboard::renderInfoBox({
      num.val <- string.to.num(input)
      if (num.val$type == "SCC"){
        stage.dummy <- 999
      }
      else{
        stage.dummy <- 1
      }
      
      survobs <- subset(results.LOLE, type == num.val$type & geslacht == num.val$gender &
                          age == input$age & jaar == input$year &
                          eval(as.name(paste("stage", num.val$stage, sep=""))) == stage.dummy)[[paste("survobs_", input$survived, sep="")]]
      survobs_lci <- subset(results.LOLE, type == num.val$type & geslacht == num.val$gender &
                              age == input$age & jaar == input$year &
                              eval(as.name(paste("stage", num.val$stage, sep=""))) == stage.dummy)[[paste("survobs_lci_", input$survived, sep="")]]
      survobs_uci <- subset(results.LOLE, type == num.val$type & geslacht == num.val$gender &
                              age == input$age & jaar == input$year &
                              eval(as.name(paste("stage", num.val$stage, sep=""))) == stage.dummy)[[paste("survobs_uci_", input$survived, sep="")]]
      
      shinydashboard::infoBox("The life expectancy of the cancer patient is",
              paste(round(survobs, 1), "years"), 
              paste0("This estimation may vary between ", round(survobs_lci, 1), " and ", round(survobs_uci, 1), " years."),
              icon = icon("heartbeat"), color = "aqua"
      )
    })
    
    output$LOLE_box <- shinydashboard::renderInfoBox({
      num.val <- string.to.num(input)
      if (num.val$type == "SCC"){
        stage.dummy <- 999
      }
      else{
        stage.dummy <- 1
      }
      
      LOLE <- subset(results.LOLE, type == num.val$type & geslacht == num.val$gender &
                       age == input$age & jaar == input$year &
                       eval(as.name(paste("stage", num.val$stage, sep=""))) == stage.dummy)[[paste("ll_", input$survived, sep="")]]
      LOLE_lci <- subset(results.LOLE, type == num.val$type & geslacht == num.val$gender &
                           age == input$age & jaar == input$year &
                           eval(as.name(paste("stage", num.val$stage, sep=""))) == stage.dummy)[[paste("ll_lci_", input$survived, sep="")]]
      LOLE_uci <- subset(results.LOLE, type == num.val$type & geslacht == num.val$gender &
                           age == input$age & jaar == input$year &
                           eval(as.name(paste("stage", num.val$stage, sep=""))) == stage.dummy)[[paste("ll_uci_", input$survived, sep="")]]
      
      shinydashboard::infoBox("The number of years lost after cancer diagnosis are", 
              paste(round(LOLE, 1), "years"),
              paste0("This estimation may vary between ", round(LOLE_lci, 1), " and ", round(LOLE_uci, 1), " years."),
              icon = icon("chart-line"), color = "aqua")
    })
    
    output$PLOLE_box <- shinydashboard::renderInfoBox({
      num.val <- string.to.num(input)
      if (num.val$type == "SCC"){
        stage.dummy <- 999
      }
      else{
        stage.dummy <- 1
      }
      
      survexp <- subset(results.LOLE, type == num.val$type & geslacht == num.val$gender & 
                          age == input$age & jaar == input$year & 
                          eval(as.name(paste("stage", num.val$stage, sep=""))) == stage.dummy)[[paste("survexp_", input$survived, sep="")]]
      PLOLE <- subset(results.LOLE, type == num.val$type & geslacht == num.val$gender &
                        age == input$age & jaar == input$year &
                        eval(as.name(paste("stage", num.val$stage, sep=""))) == stage.dummy)[[paste("ll_", input$survived, sep="")]]/survexp
      PLOLE_lci <- subset(results.LOLE, type == num.val$type & geslacht == num.val$gender &
                            age == input$age & jaar == input$year &
                            eval(as.name(paste("stage", num.val$stage, sep=""))) == stage.dummy)[[paste("ll_lci_", input$survived, sep="")]]/survexp
      PLOLE_uci <- subset(results.LOLE, type == num.val$type & geslacht == num.val$gender &
                            age == input$age & jaar == input$year &
                            eval(as.name(paste("stage", num.val$stage, sep=""))) == stage.dummy)[[paste("ll_uci_", input$survived, sep="")]]/survexp
      
      shinydashboard::infoBox("The proportion of life lost after cancer diagnosis is", 
              paste(round(PLOLE*100, 1), "%", sep=""),
              paste0("This estimation may vary between ", round(PLOLE_lci*100, 1), "% and ", round(PLOLE_uci*100, 1), "%."),
              icon = icon("percentage"), color = "aqua")
    })
    
    output$LOLE_bar <- shiny::renderPlot({
      num.val <- string.to.num(input)
      if (num.val$type == "SCC"){
        stage.dummy <- 999
      }
      else{
        stage.dummy <- 1
      }
      
      survexp <- subset(results.LOLE, type == num.val$type & geslacht == num.val$gender & 
                          age == input$age & jaar == input$year & 
                          eval(as.name(paste("stage", num.val$stage, sep=""))) == stage.dummy)[[paste("survexp_", input$survived, sep="")]]
      survobs <- subset(results.LOLE, type == num.val$type & geslacht == num.val$gender &
                          age == input$age & jaar == input$year &
                          eval(as.name(paste("stage", num.val$stage, sep=""))) == stage.dummy)[[paste("survobs_", input$survived, sep="")]]
      survobs_lci <- subset(results.LOLE, type == num.val$type & geslacht == num.val$gender &
                              age == input$age & jaar == input$year &
                              eval(as.name(paste("stage", num.val$stage, sep=""))) == stage.dummy)[[paste("survobs_lci_", input$survived, sep="")]]
      survobs_uci <- subset(results.LOLE, type == num.val$type & geslacht == num.val$gender &
                              age == input$age & jaar == input$year &
                              eval(as.name(paste("stage", num.val$stage, sep=""))) == stage.dummy)[[paste("survobs_uci_", input$survived, sep="")]]
      LOLE <- subset(results.LOLE, type == num.val$type & geslacht == num.val$gender &
                       age == input$age & jaar == input$year &
                       eval(as.name(paste("stage", num.val$stage, sep=""))) == stage.dummy)[[paste("ll_", input$survived, sep="")]]
      LOLE_lci <- subset(results.LOLE, type == num.val$type & geslacht == num.val$gender &
                           age == input$age & jaar == input$year &
                           eval(as.name(paste("stage", num.val$stage, sep=""))) == stage.dummy)[[paste("ll_lci_", input$survived, sep="")]]
      LOLE_uci <- subset(results.LOLE, type == num.val$type & geslacht == num.val$gender &
                           age == input$age & jaar == input$year &
                           eval(as.name(paste("stage", num.val$stage, sep=""))) == stage.dummy)[[paste("ll_uci_", input$survived, sep="")]]
      
      plot.new()
      title(main="Life expectancy", 
            sub=paste("Life expectancy Dutch population is", 
                      sprintf("%.1f", survexp), "years. \n",
                      "Life expectancy of cancer patient is",
                      sprintf("%.1f", survobs), "years. \n Thus,",
                      sprintf("%.1f", LOLE), "years lost after cancer diagnosis"))
      graphics::polygon(c(0, 0, 0.49, 0.49), c(0, 1, 1, 0), col="#3c8cbc", border="#3c8cbc")
      graphics::polygon(c(0.51, 0.51, 1, 1), c(0, survobs/survexp-0.01, survobs/survexp-0.01, 0), col="#08C4EC", border="#08C4EC")
      graphics::polygon(c(0.51, 0.51, 1, 1), c(survobs/survexp+0.01, 1, 1, survobs/survexp+0.01), col="lightgrey", border="lightgrey")
      graphics::text(0.49/2, 0.5, paste(sprintf("%.1f", survexp), "years"), col="white")
      graphics::text(0.5+0.49/2, survobs/survexp/2, paste(sprintf("%.1f", survobs), "years"))
      graphics::text(0.5+0.49/2, survobs/survexp+0.01+(1-survobs/survexp-0.01)/2, paste(sprintf("%.1f", LOLE), "years"))
    }, bg="transparent")
    
    output$PLOLE_pie <- shiny::renderPlot({
      num.val <- string.to.num(input)
      if (num.val$type == "SCC"){
        stage.dummy <- 999
      }
      else{
        stage.dummy <- 1
      }
      
      survexp <- subset(results.LOLE, type == num.val$type & geslacht == num.val$gender & 
                          age == input$age & jaar == input$year & 
                          eval(as.name(paste("stage", num.val$stage, sep=""))) == stage.dummy)[[paste("survexp_", input$survived, sep="")]]
      PLOLE <- subset(results.LOLE, type == num.val$type & geslacht == num.val$gender &
                        age == input$age & jaar == input$year &
                        eval(as.name(paste("stage", num.val$stage, sep=""))) == stage.dummy)[[paste("ll_", input$survived, sep="")]]/survexp
      PLOLE_lci <- subset(results.LOLE, type == num.val$type & geslacht == num.val$gender &
                            age == input$age & jaar == input$year &
                            eval(as.name(paste("stage", num.val$stage, sep=""))) == stage.dummy)[[paste("ll_lci_", input$survived, sep="")]]/survexp
      PLOLE_uci <- subset(results.LOLE, type == num.val$type & geslacht == num.val$gender &
                            age == input$age & jaar == input$year &
                            eval(as.name(paste("stage", num.val$stage, sep=""))) == stage.dummy)[[paste("ll_uci_", input$survived, sep="")]]/survexp
      
      graphics::pie(c(PLOLE, 1-PLOLE)*100, labels=c(paste0(sprintf("%.1f", PLOLE*100), "%"), ""),
          init.angle=90, clockwise=TRUE,
          main="Proportional life expectancy", 
          sub=paste0("The proportion lost after cancer diagnosis is ",
                     round(PLOLE*100, 1), "%. \n",
                    "This estimation may vary between ",
                     round(PLOLE_lci*100, 1), "% and ", 
                     round(PLOLE_uci*100, 1), "%."),
          col=c("#08C4EC", "white"), border="lightgrey")
    }, bg="transparent")
    
    info.data <- data.frame(cancer.type=index.names()$full, 
                            Description=c(c("Carcinoma of the renal pelvis carcinoma, Carcinoma of the ureter carcinoma, Superficial bladder carcinoma (Ta/Tis), Muscle-invasive bladder carcinoma, Urachus carcinoma, Carcinoma of the urethra, Carcinoma of the urinary system, NOS"),
                                          c("Cervix carcinoma"),
                                          c("Malign brain tumors, neuroepithelial, Malign brain tumors, unspecified, Malign spinal cord tumors, neuroepithelial, Malign spinal cord tumors, unspecified, Anaplastic meningioma, Meningiaal melanoma, Anaplastic hemangiopericytoma, Malign vestibular schwannoma, Malign tumors of the other brain nerves, Malign tumors of the spinal nerves, Pineal parenchymtumor/pineoblastoma, neuroepithelial, Pineal parenchymtumor/pineoblastoma, unspecified"),
                                          c("Colon carcinoma, Appendix carcinoma, Rectum carcinoma, Rectosigmoid carcinoma"),
                                          c("Cervical oesophageal carcinoma, Oesophageal carcinoma (excl cervical), Invasive cardia carcinoma, Invasive stomach carcinoma"),
                                          c("Endometrium carcinoma"),
                                          c("Invasive breast cancer carcinoma"),
                                          c("Invasive lip carcinoma, Tongue carcinoma, Gum carcinoma, Floor of mouth carcinoma, Palate carcinoma, Oral cavity, other/NOS, Oropharynx carcinoma, Nasopharynx carcinoma, Hypopharynx carcinoma, Mouth and pharynx, other/NOS, Parotid gland carcinoma, Other salivary glands, Carcinoma of the nasal cavity, Carcinoma of the middle ear, Carcinoma of the maxillary sinus, Carcinoma of the ethmoid sinus, Carcinoma of other and unspecified paranasal sinuses, Supraglottic carcinoma, Glottic carcinoma, Subglottic carcinoma, Other / unspecified laryngeal carcinoma, Unknown primary tumor with squamous cell carcinoma in, head and neck"),
                                          c("Hepatocellular carcinoma, Intrahepatic cholangiocarcinoma, Carcinoma of the gallbladder carcinoma, Cystic duct carcinoma, Exocrine pancreatic carcinoma, Neuroendocrine pancreatic cancer carcinoma, Cholangiocarcinoma, NOS, Perihilar cholangiocarcinoma, Distal cholangiocarcinoma, Non-invasive papil carcinoma"),
                                          c("Kidney carcinoma"),
                                          c("Non small-cell lung carcinoma, Small-cell lung carcinoma , Carcinoid of the lung, Other / unspecified lung cancer, Pleuropulmonal blastoma"),
                                          c("Melanoma of the skin and lip, Melanoma of the vulva, Melanoma of the penis, Melanoma of the scrotum, Melanoma of a primary unknown site"),
                                          c("Epithelial ovarian carcinoma, Extra-ovarian carcinoma, Tuba carcinoma"),
                                          c("Prostate carcinoma"),
                                          c("Squamous cell carcinoma of the eye lid, Squamous cell carcinoma other skin sites, Squamous cell carcinoma head and nek"),
                                          c("Seminoma, Non-seminoma"),
                                          c("Papillary and follicular carcinoma of the thyroid gland, Medullary carcinoma of the thyroid gland, Anaplastic carcinoma of the thyroid gland, Squamous cell carcinoma of the thyroid gland, Other/unspecified carcinoma of the thyroid gland"), 
                                          c("All together")))
    output$infotable <- DT::renderDT(datatable(info.data,
                                           rownames=FALSE, colnames=c("Cancer type", "Description")))
    
    shiny::observeEvent(input$calculateButton, {
      shinyjs::show("results.panel")
    })
    
    shiny::observeEvent({input$type
      input$gender
      input$age
      input$year
      input$survived
      input$stage
    }, {
      shinyjs::hide("results.panel")
    })
  }
)