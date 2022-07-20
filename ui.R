shiny::shinyUI(
  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "Calculator"),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Dashboard", tabName = "dashboard", icon = icon("calculator")),
        shinydashboard::menuItem("Specification", tabName="specification", icon=icon("info-circle")),
        shinydashboard::menuItem("About", tabName="abstract", icon=icon("info-circle")),
        shinydashboard::menuItem("Supplier", tabName = "supplier", icon=icon("tools"))
      )
    ),
    shinydashboard::dashboardBody(
        shinydashboard::tabItems(
          shinydashboard::tabItem(tabName = "dashboard", 
                                  dashboardthemes::shinyDashboardThemes(theme = "grey_light"),
          shiny::fluidPage(
            shinyjs::useShinyjs(),
            shinydashboard::box(width = 12,
                title = "This web application is under development", status="warning", 
                solidHeader=FALSE),
            shinydashboard::box(width = 6,
                title = "Adjust the patient characteristics",
                solidHeader = FALSE, status = "primary", 
                shiny::selectInput("type", "Select cancer type", 
                            label = tags$span(
                              "Select cancer type", 
                              tags$i(class = "glyphicon glyphicon-info-sign", 
                                     style = "color:#0072B2;",
                                     title = "Have a look at the disclaimer to see if your cancer type is included.")),
                            choices = index.names()$full),
                shiny::selectInput("gender", "Select gender", 
                            choices = c("Male", "Female")),
                shiny::sliderInput("age", "Enter the age at which the patient was diagnosed", 18, 99, 68),
                shiny::sliderInput("year", "Enter the calendar year in which the patient was diagnosed", 1989, 2018, 2010, sep = ""),
                shiny::sliderInput("survived", "Enter the number of years survived after diagnosis", 0, 10, 0, sep = ""),
                shiny::selectInput("stage", "Select the cancer stage at diagnosis",
                            choices = c("Localized",
                                        "Regional",
                                        "Distant")),
                shiny::actionButton(inputId = "calculateButton", label = "Calculate", icon=icon("calculator"),
                             style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
            ),
            shinyjs::hidden(
              div(
                id="results.panel",
                shiny::conditionalPanel(condition="calculateButton",
                                 box(width = 6,
                                     title = "Results",
                                     solidHeader = FALSE, status = "primary", 
                                     shinydashboard::infoBoxOutput("survexp_box", width = 12), 
                                     shinydashboard::infoBoxOutput("survobs_box", width = 12),
                                     shinydashboard::infoBoxOutput("LOLE_box", width = 12),
                                     shinydashboard::infoBoxOutput("PLOLE_box", width = 12))),
                shiny::conditionalPanel(condition="calculateButton",
                                 box(width=6,
                                     title = "Disclaimer",
                                     solidHeader = FALSE, status = "warning",
                                     includeHTML("html/disclaimer.html"))),
                shiny::conditionalPanel(condition="calculateButton",
                                 box(width=12,
                                     title = "Visualization",
                                     solidHeader = FALSE, status = "primary",
                                     shiny::column(6, shiny::plotOutput("LOLE_bar", width="300px", height="300px")),
                                     shiny::column(6, shiny::plotOutput("PLOLE_pie", width="350px", height="300px"))))
              )
            )
          )
        ),
        shinydashboard::tabItem(tabName = "specification",
                h2("Detailed specification of cancer types"),
                DT::DTOutput("infotable")),
        shinydashboard::tabItem(tabName = "abstract",
                shiny::includeHTML("html/abstract.html")),
        shinydashboard::tabItem(tabName = "supplier",
                shiny::includeHTML("html/supplier.html"))
      )
    )
  )
)