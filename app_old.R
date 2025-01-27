library(shiny)
library(plotly)
library(shinyjs)
library(tidyverse)
library(shinyWidgets)

#setwd("C:/Users/zhang/Desktop/NISS/NISS_Visualization/")
dta <- read.csv("data.csv")

dta <- dta %>% mutate(
  Gender = as.factor(Gender),
  Education = as.factor(Education),
  Constant = as.factor(Constant)
) %>% mutate(EduNGender = paste(as.character(Gender), as.character(Education))) %>% mutate(EduNGender = as.factor(EduNGender))


ui <- navbarPage(
  "NISS",
  id = "Controller",
  tabPanel(
    title =p("Home", class ="MainFont"),
    value="firstPage",
    class="navbarBlock",
    id = "mainPageOn",
    position = "static-top",
    useShinyjs(),
    includeCSS("niss_css.css"),
    headerPanel(
      div(
        tags$span("Gender", class = "themeColor"),
        tags$span(" & "),
        tags$span("Educational Attainment",class ="themeColor"), 
        tags$span("Affect Your Annual "),
        tags$span("Earnings",class="themeColor")
      )),
    column(width=12, class = "bottomLine"),
    
    textOutput("warningMessage"),
    img(id="reference",class = "infoIcon", src="Information-Icon.png", height = 28, width=28),
    
    column(
      class = "inputColumn",
      width = 2,
      sliderTextInput(
        inputId = "range", 
        label = NULL, 
        grid = TRUE,
        choices = c(1990,1995,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019),
        selected = c(1990,2019),
        width = 300
        
      ),
      radioButtons(
        "rb",
        NULL,
        c("Constant Dollar (2019)" = "Constant",
          "Current Dollar" = "Current"),
      ),
      tags$h4(
        id = "text1",
        p(tags$span("Does",class="DoesMatterTheme") ,tags$span("Gender", class = "themeColor"), tags$span("Matters?",class="DoesMatterTheme")),
        class = "compareGroup"
      ),
      div(
        id = "GenderCompareBlock",
        class = "div",
        
        hidden(div(
          id = "sliderInputBlock",
          class = "div",
          setSliderColor(c("#F6893D", "#F6893D", "#F6893D", "#F6893D"), c(1, 2, 3, 4)),
          sliderTextInput(
            inputId = "percentile", 
            label = NULL, 
            grid = TRUE,
            choices = c("Lowest 6 Grps","Lowest 12 Grps","All Groups"),
            selected = "All Groups",
            width = 300
          )
        )),
        
        radioButtons(
          "educationSub",
          "Please choose subgroup:",
          c(
            "Overall" = "total",
            "All Educational Attainment" = "all",
            "Elementary, Secondary or Lower" = "element",
            "College or Higher" = "college"
          )
        )
      ),
      hidden(tags$h4(id = "text2", p(tags$span("Does",class="DoesMatterTheme") ,tags$span("Educational Attainment", class = "themeColor"), tags$span("Matters?",class="DoesMatterTheme")), class = "compareGroup")),
      hidden(div(
        id = "degreeCompareBlock",
        class = "div",
        radioButtons(
          "twoGraphRadio",
          NULL,
          c(
            "Yes, regardless of gender, the educational attainment matters" = "all",
            "Yes, people with bachelor or higher degree was consistantly higher median annual earning" = "college"
          ),
        )
      )),
      column(2,class="textOutputBox",
             p(class ="paragraph",
               tags$span(class="maintext1changable",textOutput("firstPageResult"))
               
             )),
      tags$h4(id = "text3", p(tags$span("Does",class="DoesMatterTheme") ,tags$span("Educational Attainment", class = "themeColor"), tags$span("Matters?",class="DoesMatterTheme")), class = "compareGroup"),
      
    ),
    
    mainPanel(width = 10, plotlyOutput("distPlot")),
    includeCSS("charts.css"),
    tags$div(id = "legendPage1",
             tags$ul(class = "charts-css legend_2 legend-line",
                     tags$li(tags$h4("Male")),
                     tags$li(tags$h4("Female"))
             )
    )
    
  ),
   tabPanel(title =p("Compare By Yourself", class ="MainFont"),
           value="secondPage",
           class="navbarBlock",
           id = "secondPageOn",
     div(
       id = "secondPage",
       class = "secondPage", 
       fixedRow(
         ###fix this class
         
         column(
           class = "inputColumn_2",
           width = 2, 
           tags$h3(tags$strong("Compare By Yourself")),
           sliderTextInput(
             inputId = "tgp_range", 
             label = NULL, 
             grid = TRUE,
             choices = c(1990,1995,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019),
             selected = c(1990,2019),
             width = 300
             
           ), 
           checkboxInput("sndP_errorBar", tags$span(class="errorBarCheckInputText", "Standard Error (Uncertainty)"), FALSE),
           tags$p(class="groupTitle_1" ,"First Subgroup"), 
           div(class="slInput", id="gender", selectInput("gender", tags$p(class="selectOptionTitle", "Gender"), choices = c("Males", "Females"))),
           
           hidden(
             div( 
               id="Educational Attainment",
               class ="dropBars",
               selectInput("educationalLevel", tags$p(class="selectOptionTitle", "Educational Attainment"), choices=c("Overall"="total", 
                                                                            "Elementary/secondary"="elementary",
                                                                            "College"="college"))
             )
           ),
           
           hidden(
             div(
               id="ElementaryLevels",
               selectInput("elementaryLevels", tags$p(class="selectOptionTitle", "Choose Elementary Level"), 
                           choices = c("Less than 9th grade" = "lessThan9", 
                                       "Some high school, no completion" = "someHighSchool", 
                                       "High School Completion" = "highSchoolComplete"))
             )
           ),
           
           hidden(
             div( 
               id="CollegeLevels",
               selectInput("collegeLevels", tags$p(class="selectOptionTitle", "College completion level"),
                           choices=c("Some college, no degree" = "someCollege", 
                                     "Associate's degree" = "associateDegree", 
                                     "Bachelor's or higher degree" = "bachelorOrHigher"))
             )
           ),
           hidden(
             div( 
               id="BachelorOrHigherLevels",
               selectInput("bachelorOrHigherLevels", tags$p(class="selectOptionTitle", "Bachelor degree or higher"), choices = c(
                 "Bachelor or higher" = "bachelorHigherTotal",
                 "Bachelor's degree" = "bachelor",
                 "Master's degree" = "master",
                 "Professional degree" = "professionDegree",
                 "Doctor's degree" = "doctor"
               ))
             )
           ),
           # fluidRow(
           #   width=1.5,
           #   actionButton("add2compare", "Add another group to compare")
           # ),
           
           # another set of select input to compare
           
           tags$p(class="groupTitle_2", "Second Subgroup"),
           div(class="slInput", id="snd_gender", selectInput("snd_gender", tags$p(class="selectOptionTitle", "Gender"), choices = c("Males", "Females"))), 
           
           div( 
             id="snd_education_level",
             selectInput("snd_educationalLevel", tags$p(class="selectOptionTitle", "Educational Attainment"), choices=c("Overall"="total", 
                                                                              "Elementary/secondary"="elementary", 
                                                                              "College"="college"))),
           
           hidden(
             div(
               id="snd_ElementaryLevels",
               selectInput("snd_elementaryLevels", tags$p(class="selectOptionTitle", "Choose Elementary Level"), 
                           choices = c("Less than 9th grade" = "lessThan9", 
                                       "Some high school, no completion" = "someHighSchool", 
                                       "High school completion" = "highSchoolComplete"))
             )
           ),
           
           hidden(
             div( 
               id="Snd_collegeLevels",
               selectInput("snd_collegeLevels", tags$p(class="selectOptionTitle", "College completion level"),
                           choices=c("Some college, no degree" = "someCollege", 
                                     "Associate's degree" = "associateDegree", 
                                     "Bachelor's or higher degree" = "bachelorOrHigher"))
             )
           ),
           hidden(
             div( 
               id="snd_BachelorOrHigherLevels",
               selectInput("snd_bachelorOrHigherLevels", tags$p(class="selectOptionTitle", "Bachelor degree or higher"), choices = c(
                 "Bachelor or higher" = "bachelorHigherTotal",
                 "Bachelor's degree" = "bachelor",
                 "Master's degree" = "master",
                 "Professional degree" = "professionDegree",
                 "Doctor's degree" = "doctor"
               ))
             )
           )
           # hidden(
           #   fluidRow(
           #     width=1.5,
           #     id="remove2ndGroup",
           #     actionButton("remove2ndGroup", "Remove the Second Subgroup")
           #   )
           # )
           # set finished 
         ),
         column(
           class="sndLevelMiddleColumn",
           width = 7,
            plotlyOutput("scatterPlot"),  
           div(
             class="thinTextArea",
             textOutput("warningMessage_2"),
             tags$p(
               textOutput(
                 "thinBar"
               ))
           ),
            plotlyOutput("hist2") # percent comparison ,
         ),
         column( # ploting legions and another textBar 
           width=2,
           div(
             class="iconPlace",
             img(id="reference_2", src="Information-Icon.png", height = 28, width=28),
           ),
           div(
             class="linePlotLegend",
             tags$ul(
               class="charts-css legend legend-line",
               tags$li(id="line1Legend", tags$h4(textOutput("group1Cos"))),
               tags$li(id="line2Legend", tags$h4(textOutput("group1Curr"))),
               tags$li(id="line3Legend",tags$h4(textOutput("group2Cos"))),
               tags$li(id="line4Legend",tags$h4(textOutput("group2Curr"))),
             )
           ), 
           div(
             class="sqrTextArea",
             verbatimTextOutput(
               "sqrBar"
             )
           ),
           div(
             class="histPlotLegend",
             tags$ul( 
               class="charts-css legend_2 legend-circle",
               tags$li(id="bar1Legend", tags$h4(class="additional_item_1", textOutput("group1"))),
               tags$li(id="bar2Legend", tags$h4(class="additional_item_1", textOutput("group2")))
             )
           )
           
         )
       )
     )
  ),
 tabPanel(
           title =p("Your Earnings Really Grown?", class ="MainFont"),
           value="thirdPage",
           class="navbarBlock",
           id = "thirdPageOn",
     div(
       id = "snd_secondPage", 
       fluidRow(
         column(
           class = "inputColumn_2",
           width = 2,  
           tags$h3(tags$strong("Did Your Earnings Really Go Up?")) ,
           
           sliderTextInput(
             inputId = "snd_range", 
             label = NULL, 
             grid = TRUE,
             choices = c(1990,1995,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019),
             selected = c(1990,2019),
             width = 300
             
           ), 
           checkboxInput("snd_sndP_errorBar", tags$span(class="errorBarCheckInputText", "Standard Error (Uncertainty)"), FALSE),
           
           tags$p(class="groupTitle_3", "Subgroup selection") ,
           div(class="slInput", id="gender", selectInput("s_gender_sndlv2Page", "Gender", choices = c("Male", "Female"))),
           hidden(
             div( 
               id="s_Educational Attainment_sndlv2Page",
               class ="dropBars",
               selectInput("s_educationalLevel_sndlv2Page", "Educational Attainment", choices=c("Overall"="total", 
                                                                                         "Elementary/secondary"="elementary", 
                                                                                         "College"="college"))
             )
           ),
           
           hidden(
             div(
               id="s_ElementaryLevels_sndlv2Page",
               selectInput("s_elementaryLevels_sndlv2Page", tags$p(class="selectOptionTitle", "Choose Elementary Level"), 
                           choices = c("Less than 9th grade" = "lessThan9", 
                                       "Some high school, no completion" = "someHighSchool", 
                                       "High school completion" = "highSchoolComplete"))
             )
           ),
           
           hidden(
             div( 
               id="S_collegeLevels_sndlv2Page",
               selectInput("s_collegeLevels_sndlv2Page", tags$p(class="selectOptionTitle", "College completion level"),
                           choices=c("Some college, no degree" = "someCollege", 
                                     "Associate's degree" = "associateDegree", 
                                     "Bachelor's or higher degree" = "bachelorOrHigher"))
             )
           ),
           hidden(
             div( 
               id="s_BachelorOrHigherLevels_sndlv2Page",
               selectInput("s_bachelorOrHigherLevels_sndlv2Page", tags$p(class="selectOptionTitle", "Bachelor degree or higher"), choices = c(
                 "Bachelor or higher" = "bachelorHigherTotal",
                 "Bachelor's degree" = "bachelor",
                 "Master's degree" = "master",
                 "Professional degree" = "professionDegree",
                 "Doctor's degree" = "doctor"
               ))
             )
           )
         ),
         column(
           class="sndLevelMiddleColumn",
           width = 7,
           plotlyOutput("scatterPlot_sndlv2Page"),
           div(
             class="thinTextArea",
             textOutput("warningMessage_3"),
             tags$p(
               textOutput(
                 "thinBar_2"
               ))
           ),
           plotlyOutput("sgGroupHist")
        ),
        
        column( # ploting legions and another textBar width=2,
          div(
            class="iconPlace",
            img(id="reference_3", src="Information-Icon.png", height = 28, width=28),
          ),
          width=2,
          div(
            class="linePlotLegend",
            tags$ul(
              class="charts-css legend_2 legend-line",
              tags$li(tags$h4(textOutput("snd_group1Cos"))),
              tags$li(tags$h4(textOutput("snd_group1Curr"))), 
            )
          ),
          
          div(
            class="sqrTextArea",
            verbatimTextOutput(
              "sqrBar_2"
            )
          ),
          div(
            class="histPlotLegend",
            tags$ul( 
              class="charts-css legend_2 legend-circle",
              tags$li(tags$h4(class="additional_item_1", textOutput("snd_group1"))), 
            )
          )
        )
        
       )
     )
  )
)






server <- function(input, output, session) {
  
  #######################
  currentLevel <- reactiveVal(1)
  observeEvent(input$Controller, {
    if (input$Controller == "firstPage") {
      currentLevel(1)
    } else if (input$Controller == "secondPage") {
      currentLevel(2)
      showSdError <- reactiveVal(FALSE)
      gender <- reactiveVal("Male")
      data_type <- reactiveVal("Overall")
      show_snd_type <- reactiveVal(TRUE)
      snd_gender <- reactiveVal("Female")
      snd_data_type <- reactiveVal("Overall")
      
      # automatically toggle buttons based on default setting
      updateSelectInput(session, "gender", choices = c("Male", "Female"), selected=gender())
      updateSelectInput(session, "snd_gender", choices = c("Male", "Female"), selected=snd_gender())
      
      
    } else if (input$Controller == "thirdPage") {
      currentLevel(3)
    }
  })
  
  FemaleColor = "#F6893D"
  MaleColor = "#27647B"
  VerticalColor = "#57575F"
  ####################
  
  ############################## Overall and 1st page variables ######################################
  addClass("text1", "underline")
  addClass("")
  #ratio = 16:9
  plotWidth <- 1400
  plotHeight <- 750
  plotLineSize <- 0.2
  plotLineSizeThick <- 0.4
  
  dashlineAlpha = 0.8
  
  dotSize <- 0.5
  HighlightedDotSize <- 1.5
  
  ConstantDta <- dta %>% filter(Constant == TRUE)
  CurrentDta <- dta %>% filter(Constant == FALSE)
  ############### END ########### Overall and 1st page variables #########
  
  ######################## Starting Variables for Second Page ########################## 
  showSdError <- reactiveVal(FALSE)
  gender <- reactiveVal("Male")
  data_type <- reactiveVal("Overall")
  show_snd_type <- reactiveVal(TRUE)
  snd_gender <- reactiveVal("Male")
  snd_data_type <- reactiveVal("Overall")
  histBarPlotWidth <- reactiveVal(1000)
  widthSet <- reactiveVal(0.7)
  group_1_cos_color <- reactiveVal("#DA5526")
  group_1_curr_color <- reactiveVal("#FEBC38")
  group_2_cos_color <- reactiveVal("#27647B")
  group_2_curr_color <- reactiveVal("#D8C6B4")
  group_1_hist_color <- reactiveVal("#27647B")
  group_2_hist_color <- reactiveVal("#F6893D")
  linePlotHeight <- reactiveVal(370)
  histPlotHeight <- reactiveVal(370)
  ########## END ########### Starting Variables for Second Page Ends #####################
  
  
  ########################## Starting Variable for Second Second page #####################
  snd_showSdError <- reactiveVal(FALSE)
  sndP_gender <- reactiveVal("Male")
  sndP_data_type <- reactiveVal("Overall")
  sndP_show_snd_type <- reactiveVal(FALSE)
  sndP_snd_gender <- reactiveVal("Male")
  sndP_snd_data_type <- reactiveVal("Overall")
  widthSet_3rd <- reactiveVal(0.35)
  histBarPlotWidth_3rd <- reactiveVal(850)
  #currentLevel <- reactiveVal(3) 
  ########################## Starting Variable for second second page END #################
  
  ############################ Functions For 1st Page ####################################################
  
  minorAdjustmentOnLocation <- function(dta) {
    adjustment = 1000
    dta <-
      dta %>% mutate(yPos = ifelse(
        Gender == "Female" &
          Education == "Doctor",
        yPos - adjustment,
        yPos
      ))
    dta <-
      dta %>% mutate(yPos = ifelse(
        Gender == "Female" &
          Education == "Professional",
        yPos + 1.5 * adjustment,
        yPos
      ))
    dta <-
      dta %>% mutate(yPos = ifelse(
        Gender == "Male" &
          Education == "Professional",
        yPos + 1.5 * adjustment,
        yPos
      ))
    dta <-
      dta %>% mutate(yPos = ifelse(
        Gender == "Female" &
          Education == "College no degree",
        yPos - adjustment,
        yPos
      ))
    dta <-
      dta %>% mutate(yearPos = ifelse(
        Gender == "Female" &
          Education == "College no degree",
        yearPos + 2,
        yearPos
      ))
    
    dta <-
      dta %>% mutate(yPos = ifelse(
        Gender == "Male" &
          Education == "College no degree",
        yPos - adjustment,
        yPos
      ))
    dta <-
      dta %>% mutate(yearPos = ifelse(
        Gender == "Male" &
          Education == "College no degree",
        yearPos + 2,
        yearPos
      ))
    
    dta <-
      dta %>% mutate(yPos = ifelse(
        Gender == "Female" &
          Education == "AssociateDegree",
        yPos + adjustment,
        yPos
      ))
    dta <-
      dta %>% mutate(yPos = ifelse(
        Gender == "Female" &
          Education == "AssociateDegree",
        yPos + adjustment,
        yPos
      ))
    dta <-
      dta %>% mutate(yearPos = ifelse(
        Gender == "Male" &
          Education == "AssociateDegree",
        yearPos + 2,
        yearPos
      ))
    dta <-
      dta %>% mutate(yPos = ifelse(
        Gender == "Male" &
          Education == "Highschool completed",
        yPos - 2.5* adjustment,
        yPos
      ))
    dta <-
      dta %>% mutate(yearPos = ifelse(
        Gender == "Male" &
          Education == "Highschool completed",
        yearPos +1.5,
        yearPos
      ))
    dta <-
      dta %>% mutate(yPos = ifelse(
        Gender == "Female" &
          Education == "Highschool completed",
        yPos - 2.5 * adjustment,
        yPos
      ))
    dta <-
      dta %>% mutate(yearPos = ifelse(
        Gender == "Female" &
          Education == "Highschool completed",
        yearPos +2.5,
        yearPos
      ))
    
    dta <-
      dta %>% mutate(yPos = ifelse(
        Gender == "Male" &
          Education == "Highschool not completed",
        yPos + 1.5 * adjustment,
        yPos
      ))
    dta <-
      dta %>% mutate(yearPos = ifelse(
        Gender == "Male" &
          Education == "Highschool not completed",
        yearPos +2.5,
        yearPos
      ))
    dta <-
      dta %>% mutate(yPos = ifelse(
        Gender == "Female" &
          Education == "Highschool not completed",
        yPos + 2 * adjustment,
        yPos
      ))
    dta <-
      dta %>% mutate(yearPos = ifelse(
        Gender == "Female" &
          Education == "Highschool not completed",
        yearPos + 1.5,
        yearPos
      ))
    dta <-
      dta %>% mutate(yPos = ifelse(
        Gender == "Female" &
          Education == "LessThan9thGrade",
        yPos - 2.0 * adjustment,
        yPos
      ))
    dta <-
      dta %>% mutate(yearPos = ifelse(
        Gender == "Female" &
          Education == "LessThan9thGrade",
        yearPos + 2,
        yearPos
      ))
    dta <-
      dta %>% mutate(yPos = ifelse(
        Gender == "Male" &
          Education == "LessThan9thGrade",
        yPos - 2.0 * adjustment,
        yPos
      ))
    dta <-
      dta %>% mutate(yPos = ifelse(
        Gender == "Female" &
          Education == "BachelorOrHigher",
        yPos + 1.5* adjustment,
        yPos
      ))
    dta <-
      dta %>% mutate(yPos = ifelse(
        Gender == "Male" &
          Education == "BachelorOrHigher",
        yPos + adjustment,
        yPos
      ))
    
    dta
  }
  
  minorAdjustmentOnLocation2 <- function(dta) {
    adjustment = 1000
    dta <-
      dta %>% mutate(yPos = ifelse(
        Gender == "Female" &
          Education == "Doctor",
        yPos - adjustment,
        yPos
      ))
    dta <-
      dta %>% mutate(yPos = ifelse(
        Gender == "Female" &
          Education == "Professional",
        yPos + adjustment,
        yPos
      ))
    dta <-
      dta %>% mutate(yPos = ifelse(
        Gender == "Female" &
          Education == "SomeCollege",
        yPos - adjustment,
        yPos
      ))
    dta <-
      dta %>% mutate(yPos = ifelse(
        Gender == "Female" &
          Education == "AssociateDegree",
        yPos + adjustment,
        yPos
      ))
    dta <-
      dta %>% mutate(yPos = ifelse(
        Gender == "Female" &
          Education == "SomeHighSchool",
        yPos + adjustment,
        yPos
      ))
    dta <-
      dta %>% mutate(yPos = ifelse(
        Gender == "Female" &
          Education == "LessThan9thGrade",
        yPos - adjustment,
        yPos
      ))
    dta
  }
  
  minorAdjustmentOnLocation3 <- function(dta){
    adjustment = 1000
    dta <-
      dta %>% mutate(yearPos = ifelse(
        Gender == "Male" &
          Education == "Professional",
        yearPos + 5.001,
        yearPos
      ))
    dta <-
      dta %>% mutate(yearPos = ifelse(
        Gender == "Male" &
          Education == "Doctor",
        yearPos + 5.001,
        yearPos
      ))
    dta <-
      dta %>% mutate(yearPos = ifelse(
        Gender == "Male" &
          Education == "Master",
        yearPos + 5.001,
        yearPos
      ))
    dta <-
      dta %>% mutate(yearPos = ifelse(
        Gender == "Male" &
          Education == "AssociateDegree",
        yearPos + 5.001,
        yearPos
      ))
    
    dta <-
      dta %>% mutate(yPos = ifelse(
        Gender == "Male" &
          Education == "AssociateDegree",
        yPos + 2.6 * adjustment,
        yPos
      ))
    dta
  }
  
  minorAdjustmentOnLocation4 <- function(dta){
    adjustment  = 1000
    dta <-
      dta %>% mutate(yPos = ifelse(
        Gender == "Male" &
          Education == "AssociateDegree",
        yPos + 0.7 * adjustment,
        yPos
      ))
  }
  
  
  textFunction <- function(dta) {
    map(
      paste(
        ' <b>Year:</b>',
        dta$Year,
        '<br>',
        '<b>Median Income:</b>',
        sprintf("$%d", dta$Income),
        '<b>(</b>',
        sprintf("$%.2f", dta$Income_SD),
        '<b>)</b>',
        '<br>',
        '<b>Full time Number:</b>',
        dta$NumberOfPeople,
        '<b>(</b>',
        dta$NumberOfPeople_SD,
        '<b>)</b>',
        '<br>',
        '<b>Full time percentage:</b>',
        dta$Percentage,
        '<b>(</b>',
        dta$Percentage_SD,
        '<b>)</b>',
        '<br>',
        '<b>[Standard errors appear in parentheses]</b>'
      ),
      HTML
    )
  }
  
  textFunction_hist<- function(dta) { 
    map(
      paste(
        ' <b>Year:</b>',
        dta$year,
        '<br>', 
        '<b>', ifelse(dta$valueType=="Number of People with Earnings Who Work Full-time, Year Round (in Thousands)", "Number of people in 1000", "Percentage of people"),':</b>',
        dta$value, ifelse(dta$valueType=="Number of People with Earnings Who Work Full-time, Year Round (in Thousands)", "", "%"),
        '<b>(</b>',
        dta$sd,
        '<b>)</b>',
        '<br>',  
        '<b>[Standard errors appear in parentheses]</b>'
      ),
      HTML
    )
  }
  
  #filter the elementary group and college group
  elementFilter <-
    c("LessThan9thGrade", "Highschool not completed", "Highschool completed")
  collegeFilter <- c("SomeCollege",
                     "AssociateDegree",
                     "Bachelor",
                     "Master",
                     "Professional",
                     "Doctor")
  
  
  resultVal <- reactiveVal(0)
  
  filterGroupByConsIncome_2 <- function(inputData, ranks) {
    grouped_data <-
      inputData %>% filter(Constant == TRUE, Year == 2018)
    grouped_data <- grouped_data %>% arrange(Income)
    grouped_data_selected <- grouped_data[ranks, ]
    
    finalData <- NULL
    for (i in 1:NROW(grouped_data_selected)) {
      curData <-
        inputData %>% filter(
          Education == grouped_data_selected$Education[i],
          Gender == grouped_data_selected$Gender[i]
        )
      #print(curData)
      finalData <- rbind(finalData, curData)
    }
    finalData
  }
  
  
  scaleFUN <- function(x)
    sprintf("%.0fk", x / 1000)
  
  
  
  
  
   drawPlot <- reactive({
    totalDta <- dta %>% filter(Education == "Overall")
    thisdta <- dta %>% filter(Education != "Overall")
    thisdta <-
      thisdta %>% filter(Education != "BachelorOrHigher")
    
    truncatedDta <-
      switch(
        input$percentile,
        "Lowest 6 Grps" =filterGroupByConsIncome_2(thisdta, 1:6),
        "Lowest 12 Grps" =filterGroupByConsIncome_2(thisdta, 1:12),
        "All Groups" = thisdta
      )
    
    targetDtaFlag <-
      switch (input$rb, "Constant" = TRUE, "Current" = FALSE)
    
    truncatedDta <-
      truncatedDta %>% filter(Constant == targetDtaFlag)
    
    totalDta <- totalDta %>% filter(Constant == targetDtaFlag)
    
    targetDta <- switch(
      input$educationSub,
      "total" = totalDta,
      "all" = truncatedDta,
      "element" = truncatedDta  %>% filter(Education %in% elementFilter),
      "college" = truncatedDta %>% filter(Education %in% collegeFilter)
    )
    
    alpha2 <- 1
    alpha1 <- 0.5
    size2 <- dotSize
    linesize2 <- plotLineSize
    linesize1 <- plotLineSizeThick
    
    ##deal with size with totla
    if (input$educationSub == "total") {
      alpha1 = alpha2
      size2 = dotSize + 1
      linesize1 = plotLineSizeThick + 0.5
      linesize2 = plotLineSizeThick + 0.5
    } else if(input$educationSub == "element"){
      size2 <-dotSize +0.8
      linesize1 = plotLineSize + 0.5
      linesize2 = plotLineSizeThick + 0.5
    }
    
    minYear = input$range[1]
    maxYear = input$range[2]
    
    targetDta <-
      targetDta %>% filter(Year >= minYear, Year <= maxYear)
    
    
    textData_male <-
      targetDta %>% filter(Gender == "Male") %>% group_by(Gender, Education) %>%
      summarise(yearPos = first(Year) - 2, yPos = first(na.omit(Income)))
    
    textData_female <-
      targetDta %>% filter(Gender == "Female") %>% group_by(Gender, Education) %>%
      summarise(yearPos = last(Year) + 2, yPos = last(na.omit(Income)))
    textData <- rbind(textData_male, textData_female)
    
    textData <- minorAdjustmentOnLocation2(textData)
    if(minYear== 1990){
      textData <- minorAdjustmentOnLocation3(textData)
    } else {
      textData <- minorAdjustmentOnLocation4(textData)
    }
    #textData<- formatEducation(textData)
    
    
    thePlot <- ggplot() +
      geom_line(
        data = targetDta,
        aes(
          x = Year,
          y = Income,
          size = Gender,
          group = Education,
          color = Gender,
          alpha = Gender
        )
      ) +
      geom_point(
        data = targetDta,
        size = size2,
        aes(
          x = Year,
          y = Income,
          group = Education,
          color = Gender,
          alpha = Gender,
          text = textFunction(targetDta),
        )
      ) +
      geom_text(data = textData,
                aes(
                  x = yearPos,
                  y = yPos,
                  color = Gender,
                  label = Education,
                )) +
      #cust_whline(targetDta) +
      scale_y_continuous(name = 'Median Annual Earning ($K)', labels = scaleFUN) +
      scale_color_manual(values = c(FemaleColor, MaleColor)) +
      scale_alpha_manual(values = c(alpha2, alpha1)) +
      scale_size_manual(values = c(linesize1, linesize2)) +
      xlim(minYear - 3, maxYear + 3) + theme_minimal()+ theme(
        axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size =
                                    14, face = "bold")
      ) 
    if (input$educationSub == "total" && minYear == 1990 && maxYear == 2019) {
      years = c(1990, 2005, 2019)
      
      dtaYear <- totalDta %>% filter(Year %in% years)
      yInd <- (dtaYear$Income[1] + dtaYear$Income[4]) / 2
      xInx <-  dtaYear$Year[1] - 1
      thePlot <-
        thePlot + geom_segment(
          aes(
            x = dtaYear$Year[1],
            y = dtaYear$Income[1],
            xend = dtaYear$Year[1],
            yend = dtaYear$Income[4]
          ),
          linetype = "dashed"
        ) +
        geom_text(color= "red", aes(
          x = dtaYear$Year[1] - 1 + 0.01,
          y = (dtaYear$Income[1] + dtaYear$Income[4]) / 2,
          label = paste("$", abs(dtaYear$Income[1] - dtaYear$Income[4]))
        )) +
        geom_segment(
          aes(
            x = dtaYear$Year[2],
            y = dtaYear$Income[2],
            xend = dtaYear$Year[2],
            yend = dtaYear$Income[5]
          ),
          color = VerticalColor,
          linetype = "dashed"
        ) +
        geom_text(color= "red",aes(
          x = dtaYear$Year[2] - 1 + 0.01,
          y = (dtaYear$Income[2] + dtaYear$Income[5]) / 2,
          label = paste("$", abs(dtaYear$Income[2] - dtaYear$Income[5]))
        )) +
        geom_segment(
          aes(
            x = dtaYear$Year[3],
            y = dtaYear$Income[3],
            xend = dtaYear$Year[3],
            yend = dtaYear$Income[6]
          ),
          color = VerticalColor,
          linetype = "dashed"
        ) +
        geom_text(color= "red", aes(
          x = dtaYear$Year[3] - 1 + 0.01,
          y = (dtaYear$Income[3] + dtaYear$Income[6]) / 2,
          label = paste("$", abs(dtaYear$Income[3] - dtaYear$Income[6]))
        ))
    }
    g <-
      ggplotly(thePlot,
               tooltip = "text",
               width = plotWidth,
               height = plotHeight) %>% event_register("plotly_unhover")%>% event_register("plotly_click")
    hide_legend(g)
  })
  
  
  
  drawHighlightedPlot <- reactive({
    totalDta <- dta %>% filter(Education == "Overall")
    thisdta <- dta %>% filter(Education != "Overall")
    thisdta <-
      thisdta %>% filter(Education != "BachelorOrHigher")
    
    truncatedDta <-
      switch(
        input$percentile,
        "Lowest 6 Grps" =filterGroupByConsIncome_2(thisdta, 1:6),
        "Lowest 12 Grps" =filterGroupByConsIncome_2(thisdta, 1:12),
        "All Groups" = thisdta
      )
    
    targetDtaFlag <-
      switch (input$rb, "Constant" = TRUE, "Current" = FALSE)
    
    truncatedDta <-
      truncatedDta %>% filter(Constant == targetDtaFlag)
    totalDta <- totalDta %>% filter(Constant == targetDtaFlag)
    
    targetDta <- switch(
      input$educationSub,
      "total" = totalDta,
      "all" = truncatedDta,
      "element" = truncatedDta  %>% filter(Education %in% elementFilter),
      "college" = truncatedDta %>% filter(Education %in% collegeFilter)
    )
    
    alpha2 <- 1
    alpha1 <- 0.15
    linesize = 0.2
    sizeHigh <- dotSize
    ##deal with size with totla
    if (input$educationSub == "total") {
      linesize <- 1
      alpha1 <- 1
      sizeHigh <- dotSize + 1
    }
    
    targetDta$isHighlighed <- targetDta$Education == groupName()
    
    minYear = input$range[1]
    maxYear = input$range[2]
    
    
    targetDta <-
      targetDta %>% filter(Year >= minYear, Year <= maxYear)
    
    targetDta <- targetDta %>% filter(!is.na(Income))
    
    textData_male <-
      targetDta %>% filter(Gender == "Male") %>% group_by(Gender, Education) %>%
      summarise(yearPos = first(Year) - 2, yPos = first(na.omit(Income)))
    textData_male <- textData_male %>% mutate(yearPos = 1988)
    
    textData_female <-
      targetDta %>% filter(Gender == "Female") %>% group_by(Gender, Education) %>%
      summarise(yearPos = last(Year) + 2, yPos = last(na.omit(Income)))
    
    textData <-
      rbind(textData_male, textData_female) %>% filter(Education == groupName())
    
    
    thePlot <- ggplot() +
      geom_line(
        data = targetDta,
        size = linesize,
        aes(
          x = Year,
          y = Income,
          group = Education,
          color = Gender,
          alpha = isHighlighed
        )
      ) +
      geom_point(
        data = targetDta,
        aes(
          x = Year,
          y = Income,
          size = isHighlighed,
          group = Education,
          color = Gender,
          alpha = isHighlighed,
          text = textFunction(targetDta)
        )
      ) +
      geom_text(data = textData,
                aes(
                  x = yearPos,
                  y = yPos,
                  color = Gender,
                  label = Education
                ))  +
      scale_y_continuous(name = 'Median Annual Earning ($K)', labels = scaleFUN) +
      scale_color_manual(values = c(FemaleColor, MaleColor)) +
      scale_size_manual(values = c(sizeHigh, HighlightedDotSize)) +
      scale_alpha_manual(values = c(alpha1, alpha2)) +
      xlim(minYear - 3, maxYear + 3) + theme_minimal()+ theme(
        axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold")
      )
    
      eventd <-event_data("plotly_click")
    
    if (!is.null(eventd)) {
      if (eventd$x %% 1 == 0) {
        temp <-
          dta %>% filter(Income == eventd$y,
                         Year == eventd$x,
                         Constant == targetDtaFlag)
        maleD <-
          dta %>% filter(
            Education == temp$Education,
            Gender == "Male",
            Year == eventd$x,
            Constant == targetDtaFlag
          )
        femaleD <-
          dta %>% filter(
            Education == temp$Education,
            Gender == "Female",
            Year == eventd$x,
            Constant == targetDtaFlag
          )
        
        thePlot <-
          thePlot + geom_hline(
            yintercept = maleD$Income[1],
            color = MaleColor,
            linetype = "dashed",
            size = plotLineSize,
            alpha = dashlineAlpha
          ) +
          geom_hline(
            yintercept =  femaleD$Income[1],
            color = FemaleColor,
            linetype = "dashed",
            size = plotLineSize,
            alpha = dashlineAlpha
          )
        
        yInd <- (maleD$Income[1] + femaleD$Income[1]) / 2
        xInx <-  maleD$Year[1] - 1
        thePlot <-
          thePlot + geom_segment(
            aes(
              x = maleD$Year[1],
              y = femaleD$Income[1],
              xend = maleD$Year[1],
              yend = maleD$Income[1]
            ),
            color = VerticalColor,
            linetype = "dashed"
          ) +
          geom_text(color = "red", aes(
            x = xInx + 0.01,
            y = yInd,
            label = paste("$", abs(maleD$Income[1] - femaleD$Income[1])),
          ))
      }
      
    }
    
    
    
    g <- ggplotly(thePlot,
                  tooltip = "text",
                  width = plotWidth,
                  height = plotHeight) %>% event_register("plotly_unhover")%>% event_register("plotly_click")
    hide_legend(g)
  })
  
  
  drawGenderComparePlots <- reactive({
    minYear = input$range[1]
    maxYear = input$range[2]
    
    targetDta <-
      switch(input$rb, "Constant" = ConstantDta, "Current" = CurrentDta)
    targetDta <-
      targetDta %>% filter(Year >= minYear, Year <= maxYear)
    
    DtaWithTotal <- targetDta %>% filter(Education != "Overall")
    DtaWithTotal <-
      DtaWithTotal %>% filter(Education != "BachelorOrHigher")
    
    totalDta <-
      targetDta %>% filter(Education %in% c("Overall", "BachelorOrHigher"))
    
    compareDta <-
      switch(input$twoGraphRadio,
             "all" = DtaWithTotal,
             "college" = totalDta)
    
    lineSize = 0.2
    if(input$twoGraphRadio=="college"){
      lineSize = 1.0
    }
    
    textData_male <-
      compareDta %>% filter(Gender == "Male") %>% group_by(Gender, Education) %>%
      summarise(yearPos = last(Year) + 2, yPos = last(na.omit(Income)))
    
    textData_female <-
      compareDta %>% filter(Gender == "Female") %>% group_by(Gender, Education) %>%
      summarise(yearPos = last(Year) + 2, yPos = last(na.omit(Income)))
    
    textData <- rbind(textData_male, textData_female)
    textData <- minorAdjustmentOnLocation(textData)
    
    upperBound <- maxYear + 8
    # if(maxYear>=2015){
    #   upperBound <- maxYear + 8
    # } else if(maxYear>=2010){
    #   upperBound <- maxYear + 7
    # } else if(maxYear>=2005) {
    #   upperBound <- maxYear + 6 
    # }
    # upperBound <- maxYear + 8
    
    if (input$twoGraphRadio == "all") {
      thePlot <- ggplot() +  geom_line(
        data = compareDta,
        size = lineSize,
        aes(
          x = Year,
          y = Income,
          group = Education,
          color = Gender
        )
      ) +
        geom_point(
          data = compareDta,
          size = dotSize,
          aes(
            x = Year,
            y = Income,
            group = Education,
            color = Gender,
            text = textFunction(compareDta)
          )
        ) +
        geom_text(data = textData,
                  aes(
                    x = yearPos,
                    y = yPos,
                    color = Gender,
                    label = Education
                  )) +
        
        facet_wrap( ~ Gender) +
        scale_y_continuous(name = 'Median Annual Earning ($K)', labels = scaleFUN) +
        scale_color_manual(values = c(FemaleColor, MaleColor)) +
        xlim(minYear, upperBound) + theme_minimal() +
        theme(
          axis.text = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 14, face = "bold"),
          strip.text = element_text(size= 18,face = "bold")
        )
    } else {
      
      
      thePlot <- ggplot() + geom_line(
        data = compareDta,
        size = lineSize,
        aes(
          x = Year,
          y = Income,
          shape = Constant,
          group = Education,
          color = Gender
          
        )
      ) +
        geom_point(
          data = compareDta,
          size = dotSize + 1,
          aes(
            x = Year,
            y = Income,
            shape = Constant,
            group = Education,
            color = Gender,
            text = textFunction(compareDta)
          )
        ) +
        geom_text(data = textData,
                  aes(
                    x = yearPos,
                    y = yPos,
                    color = Gender,
                    label = Education
                  )) +
        facet_wrap(~ Gender)  +
        scale_y_continuous(name = 'Median Annual Earning ($K)', labels = scaleFUN) +
        scale_color_manual(values = c(FemaleColor, MaleColor)) + xlim(minYear, upperBound) + 
        theme_minimal() +
        theme(
          axis.text = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 14, face = "bold"),
          strip.text = element_text(size= 18,face = "bloder")
        )
    }
    
    g <-
      ggplotly(thePlot,
               tooltip = "text",
               width = plotWidth,
               height = plotHeight) %>% event_register("plotly_unhover")%>% event_register("plotly_click")
    hide_legend(g)
  })
  
  
  drawHighlightedComparePlot <- reactive({
    minYear = input$range[1]
    maxYear = input$range[2]
    
    
    targetDtaFlag <-
      switch (input$rb, "Constant" = TRUE, "Current" = FALSE)
    
    targetDta <-
      switch(input$rb, "Constant" = ConstantDta, "Current" = CurrentDta)
    targetDta <-
      targetDta %>% filter(Year >= minYear, Year <= maxYear)
    
    DtaWithTotal <- targetDta %>% filter(Education != "Total")
    DtaWithTotal <-
      DtaWithTotal %>% filter(Education != "BachelorOrHigher")
    
    totalDta <-
      targetDta %>% filter(Education %in% c("Overall", "BachelorOrHigher"))
    
    compareDta <-
      switch(input$twoGraphRadio,
             "all" = DtaWithTotal,
             "college" = totalDta)
    
    
    compareDta$isHighlighed <- compareDta$Education == groupName()
    
    textData_male <-
      compareDta %>% filter(Gender == "Male") %>% group_by(Gender, Education) %>%
      summarise(yearPos = last(Year) + 2.5, yPos = last(na.omit(Income)))
    
    textData_female <-
      compareDta %>% filter(Gender == "Female") %>% group_by(Gender, Education) %>%
      summarise(yearPos = last(Year) + 2.5, yPos = last(na.omit(Income)))
    
    textData <-
      rbind(textData_male, textData_female) %>% filter(Education == groupName())
    #TextDta %>% filter
    
    
    upperBound <- maxYear + 8
    # if(maxYear==2019){
    #   upperBound <- maxYear + 8
    # } else if(maxYear>=2010){
    #   upperBound <- maxYear + 6
    # } else if(maxYear>=2005) {
    #   upperBound <- maxYear + 4 
    # }
    if (input$twoGraphRadio == "all") {
      thePlot <- ggplot() +  geom_line(
        data = compareDta,
        aes(
          x = Year,
          y = Income,
          group = Education,
          color = Gender,
          alpha = isHighlighed
        )
      ) +
        geom_point(
          data = compareDta,
          aes(
            x = Year,
            y = Income,
            size = isHighlighed,
            group = Education,
            color = Gender,
            alpha = isHighlighed,
            text = textFunction(compareDta)
          )
        )
    } else {
      thePlot <- ggplot() + geom_line(
        data = compareDta,
        aes(
          x = Year,
          y = Income,
          shape = Constant,
          group = Education,
          color = Gender,
          alpha = isHighlighed
        )
      ) +
        geom_point(
          data = compareDta,
          aes(
            x = Year,
            y = Income,
            size = isHighlighed,
            shape = Constant,
            group = Education,
            color = Gender,
            alpha = isHighlighed,
            text = textFunction(compareDta)
          )
        )
    }
    thePlot = thePlot +
      geom_text(data = textData,
                aes(
                  x = yearPos,
                  y = yPos,
                  color = Gender,
                  label = Education
                )) +
      facet_wrap( ~ Gender)  +
      scale_y_continuous(name = 'Median Annual Earning ($K)', labels = scaleFUN) +
      scale_color_manual(values = c(FemaleColor, MaleColor)) +
      scale_alpha_manual(values = c(0.15, 1)) +
      scale_size_manual(values = c(dotSize, HighlightedDotSize)) +
      xlim(minYear, upperBound) + theme_minimal() + theme(
        axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(size=14, face = "blod")
      )
    
    eventd <- event_data("plotly_click")
    
    if (!is.null(eventd) && eventd$x%%1==0) {
      temp <-
        dta %>% filter(Income == eventd$y,
                       Year == eventd$x,
                       Constant == targetDtaFlag)
      
      maleD <-
        dta %>% filter(
          Education == temp$Education,
          Gender == "Male",
          Year == eventd$x,
          Constant == targetDtaFlag
        )
      femaleD <-
        dta %>% filter(
          Education == temp$Education,
          Gender == "Female",
          Year == eventd$x,
          Constant == targetDtaFlag
        )
      
      thePlot <-
        thePlot + geom_hline(
          yintercept = maleD$Income[1],
          color = MaleColor,
          linetype = "dashed",
          size = plotLineSize,
          alpha = dashlineAlpha
        ) +
        geom_hline(
          yintercept =  femaleD$Income[1],
          color =  FemaleColor,
          linetype = "dashed",
          size = plotLineSize,
          alpha = dashlineAlpha
        )
    }
    
    g <-
      ggplotly(thePlot,
               tooltip = "text",
               width = plotWidth,
               height = plotHeight) %>% event_register("plotly_unhover") %>% event_register("plotly_click")
    hide_legend(g)
  })
  
  ############## END ############# Functions For 1st Page ####################################################
  groupName <- reactive({
    d <- event_data("plotly_click")

    result <- "not defined"
    if (!is.null(d)) {
      selectedYear = d["x"]
      selectedYear = as.integer(selectedYear)
      selectedIncome = d["y"]
      selectedIncome = as.numeric(selectedIncome)
      possibleRow <-
        dta %>% filter(Year == selectedYear, Income == selectedIncome)
      result <- possibleRow$Education[1]
    }
    result
  })
  
  
  
  ################################ Functions For 2nd Page ####################################################
  
  
   drawSctLinePlot <- reactive({
      d = event_data("plotly_hover")
      # filtering time
      minYear = input$tgp_range[1]
      maxYear = input$tgp_range[2]
      selectedData <- minYear
      targetData = dta %>% filter(Year >= minYear, Year <= maxYear)
      
      ##### This should not have any effect currently
      firstGender <- gsub("s", "", gender())
      secondGender <- gsub("s", "", snd_gender())
      
      firstGroup <- data_type()
      secondGroup <- snd_data_type()
      
      targetData_1 <- targetData %>% filter(Gender == firstGender, Education == firstGroup) %>% mutate(line = ifelse(Constant=="TRUE", "line1", "line2"))
      targetData_2 <- targetData %>% filter(Gender == secondGender, Education == secondGroup) %>% mutate(line = ifelse(Constant=="TRUE", "line3", "line4"))
      targetData <- rbind(targetData_1, targetData_2)
      targetData$Group <- paste(targetData$Gender, targetData$Education, targetData$Constant)
      
      line1 = (paste(gender(), data_type(), "TRUE"))
      line2 = (paste(gender(), data_type(), "TRUE"))
      line3 = (paste(snd_gender(), snd_data_type(), "TRUE"))
      line4 = (paste(snd_gender(), snd_data_type(), "FALSE"))
      
      colorSpec <- c(
        "line1" = group_1_cos_color(),
        "line2" = group_1_curr_color(),
        "line3" = group_2_cos_color(),
        "line4" = group_2_curr_color() )
      
      #print(colorSpec)
      thePlot <- ggplot() + 
        scale_y_continuous(name = 'Median Annual Earning ($K)', labels = scaleFUN) +
        geom_point(data=targetData, aes(x=Year, y=Income, color=line, text=textFunction(targetData)))  + 
        geom_line(data=targetData, aes(x=Year, y=Income, color=line, group=Group)) + 
        xlim(c(minYear-1, maxYear+1)) + 
        scale_color_manual(values=colorSpec) + theme_minimal()+ theme(
          axis.text = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 14, face = "bold"),
          strip.text = element_text(size= 14,face = "bold")
        ) + theme(legend.position="none")
      
      if (showSdError()) {
        thePlot <- thePlot + geom_errorbar(
          data=targetData, aes(x=Year, ymin=Income-Income_SD, ymax=Income+Income_SD, color=line, fill=line)
        )
      }  
      eventClickData <- event_data("plotly_hover")
      clickedYear <- eventClickData["x"]
      if (is.null(eventClickData)) {
        return(ggplotly(thePlot,width = 1000, height = linePlotHeight(), tooltip = "text") %>% event_register(event="plotly_unhover"))
      }
      
      # remove the invalid event
      if (abs(clickedYear - as.integer(clickedYear)) > 0) {
        return(ggplotly(thePlot,width = 1000, height = linePlotHeight(), tooltip = "text") %>% event_register(event="plotly_unhover"))  
      }
      
      clickedYear <- as.integer(clickedYear)
      clickedY <- as.numeric(eventClickData["y"])
      
      if (clickedYear < 1989 | clickedYear > 2019) {
        return(ggplotly(thePlot,width = 1000, height = linePlotHeight(), tooltip = "text") %>% event_register(event="plotly_unhover"))
      }
      if (is.null(clickedY) | clickedY < 100) {
        return(ggplotly(thePlot,width = 1000, height = linePlotHeight(), tooltip = "text") %>% event_register(event="plotly_unhover"))
      }
      
      clickedIncome <- eventClickData["y"]
      clickedCurve <- eventClickData["curveNumber"]
      
      hoveredData <- dta %>% filter(as.integer(Year) == as.integer(clickedYear), as.integer(Income) == as.integer(clickedIncome))
      isConstant = hoveredData$Constant[1]
      
      if (show_snd_type()) {
        fstGroupSltdta <- dta %>% filter(Gender == gender(), Education == data_type(), Year == clickedYear, Constant == isConstant)
        sndGroupSltdta <- dta %>% filter(Gender == snd_gender(), Education == snd_data_type(), Year == clickedYear, Constant == isConstant)
        
        oneIncome <- fstGroupSltdta$Income[1]
        sndIncome <- sndGroupSltdta$Income[1]
        
        # if both lines are not NA, can draw the vertical line
        if (!is.na(oneIncome) && !is.na(sndIncome) & (oneIncome != sndIncome)) {
          
          # smartly decide which position to put differencein dollars 
          difTxtX <- clickedYear
          difTxtY <- (oneIncome+sndIncome)/2
          if (abs(oneIncome - sndIncome) < 5000) {
            difTxtY = min(oneIncome, sndIncome) - 1500
            if (isConstant) {
              difTxtY <- max(oneIncome, sndIncome) + 1500
            } else {
              difTxtY <- min(oneIncome, sndIncome) - 1500
            }
          }
          
          # tmpCln1 <- data[[constant2019DollarColumn]]
          # tmpCln2 <- data[[constant2019DollarColumn_snd]]
          # tmpCln3 <- data[[currentDollarColumn]]
          # tmpCln4 <- data[[currentDollorColumn_snd]]
          # 
          # tmpMatrix <- matrix(
          #   c(tmpCln1, tmpCln2, tmpCln3, tmpCln4), ncol=length(tmpCln1)
          # )
          
            difTxtX <- ifelse((clickedYear - minYear)/(maxYear - minYear) > 0.75, difTxtX + 0.9, difTxtX - 0.9) 
            thePlot <- thePlot + geom_segment(aes(x = clickedYear, y = oneIncome, xend = clickedYear, 
                                                yend = sndIncome),linetype="dashed") + geom_text(
                                                  aes(x=difTxtX, y=difTxtY, 
                                                      label=paste("$",abs(oneIncome-sndIncome),sep="")), color="red"
                                                )
            ggplotly(thePlot, width=1000, height = linePlotHeight(), tooltip = "text") %>% event_register("plotly_unhover")   
        } else {
          ggplotly(thePlot, width=1000, height = linePlotHeight(), tooltip = "text") %>% event_register("plotly_unhover")
        }
      }
  })

   plot2groupBar <- reactive({ 
     eventClickData <- event_data("plotly_hover")
     shouldHighlight <- TRUE
     if (!is.null(eventClickData)) {
       clickedYear <- eventClickData["x"]
       # remove the invalid event
       if (abs(clickedYear - as.integer(clickedYear)) > 0) {
         shouldHighlight = FALSE 
       }
       
       clickedYear <- as.integer(clickedYear)
       clickedY <- as.numeric(eventClickData["y"])
       
       if (clickedYear < 1989 | clickedYear > 2019) {
         shouldHighlight = FALSE 
       }
       if (clickedY < 100) {
         shouldHighlight = FALSE 
       }
       clickedIncome <- eventClickData["y"]
       clickedCurve <- eventClickData["curveNumber"]
     } else {
       shouldHighlight = FALSE
     }
       
     
    minYear = input$tgp_range[1]
    maxYear = input$tgp_range[2]
    selectedData <- minYear
    fstGroupDta <- dta %>% filter(Year >= minYear & Year <= maxYear) %>% filter(
      Gender == gender(), Education == data_type(), Constant=="TRUE"
    )
    
    sndGroupDta <- dta %>% filter(Year >= minYear & Year <= maxYear) %>% filter(
      Gender == snd_gender(), Education == snd_data_type(), Constant=="TRUE"
    )
    
    desiredData_fstG_Num <- data.frame(
      year = fstGroupDta$Year,
      value = fstGroupDta$NumberOfPeople,
      sd = fstGroupDta$NumberOfPeople_SD,
      valueType = rep("Number of People with Earnings Who Work Full-time, Year Round (in Thousands)", length(fstGroupDta$Year)),
      peopleType = rep(paste(gender(), data_type()), NROW(fstGroupDta))
    ) %>% mutate(lineNum = "bar1")
    
    desiredData_sndG_Num <- data.frame(
      year = sndGroupDta$Year,
      value = sndGroupDta$NumberOfPeople,
      sd = sndGroupDta$NumberOfPeople_SD,
      valueType = rep("Number of People with Earnings Who Work Full-time, Year Round (in Thousands)", length(sndGroupDta$Year)),
      peopleType = rep(paste(snd_gender(), snd_data_type()), NROW(sndGroupDta))
    ) %>% mutate(lineNum = "bar2")
    
    desiredData_fstG_Per <- data.frame(
      year = fstGroupDta$Year,
      value = fstGroupDta$Percentage,
      sd = fstGroupDta$Percentage_SD,
      valueType = rep("Percent of People with Earnings Who Work Full-time, Year Round", length(fstGroupDta$Year)),
      peopleType = rep(paste(gender(), data_type()), NROW(fstGroupDta))
    ) %>% mutate(lineNum = "bar1")
    
    desiredData_sndG_Per <- data.frame(
      year = sndGroupDta$Year,
      value = sndGroupDta$Percentage,
      sd = sndGroupDta$Percentage_SD,
      valueType = rep("Percent of People with Earnings Who Work Full-time, Year Round", length(sndGroupDta$Year)),
      peopleType = rep(paste(snd_gender(), snd_data_type()), NROW(sndGroupDta))
    ) %>% mutate(lineNum = "bar2")
    
    colorSpecs <- c(
      "bar1" = group_1_hist_color(),
      "bar2" = group_2_hist_color()
    )
    
    if (gender() == snd_gender() & data_type() == snd_data_type()) {
      colorSpecs <- c(
        "bar1" = group_1_hist_color(),
        "bar2" = group_1_hist_color()
      )
    }
    desiredData <- rbind(desiredData_fstG_Per, desiredData_sndG_Per, desiredData_fstG_Num, desiredData_sndG_Num)
    
    desiredData <- desiredData %>% mutate(
      valueType = as.factor(valueType),
      peopleType = as.factor(peopleType)
    )
    
    ### At this part, some bars in the graph need to be gray 
    if (shouldHighlight) {
      desiredData <- desiredData %>%  mutate(hlightingStatus = ifelse(
        as.integer(year) == as.integer(clickedYear), "Highlighted", "Hidden"
      )
      
      ) %>% mutate(hlightingStatus = as.factor(hlightingStatus)) %>% mutate(oriYear = year) %>% mutate(year = year + 0.001)
      
      g <- ggplot() + geom_bar(data=desiredData, aes(x=year, y=value, fill=lineNum, alpha=hlightingStatus, text=textFunction_hist(desiredData)), position="dodge", stat="identity", width = widthSet()) + facet_wrap(~valueType, ncol=1, scales="free_y") + 
        expand_limits(y=100) + scale_alpha_manual(values=c(0.15, 1)) + scale_fill_manual(values=colorSpecs) + 
        labs(x="Year", y="Value") + theme_minimal() + theme(legend.position = "none") +
        theme(axis.text = element_text(size = 14, face = "bold"), axis.title = element_text(size = 14, face = "bold"), strip.text = element_text(size= 14,face = "bold"))
      
      ggplotly(g, width = histBarPlotWidth(), height = histPlotHeight(), tooltip = "text", hoverinfo = "none")  %>% event_unregister("plotly_hover")
    } else {
      if (gender() == snd_gender() && data_type() == snd_data_type()) {
        desiredData <- rbind(desiredData_fstG_Per, desiredData_fstG_Num)
        widthSet(0.35)
        histBarPlotWidth(850)
      } else { 
        desiredData <- rbind(desiredData_fstG_Per, desiredData_sndG_Per, desiredData_fstG_Num, desiredData_sndG_Num)
        histBarPlotWidth(1000)
        widthSet(0.70)
      }
      
      desiredData <- desiredData %>% mutate(
        valueType = as.factor(valueType),
        peopleType = as.factor(peopleType)
      )
      
      g <- ggplot() + geom_bar(data=desiredData, aes(x=year+0.001, y=value, fill=lineNum, text=textFunction_hist(desiredData)), stat="identity", position = "dodge2", width=widthSet()) +  
        scale_fill_manual(values=colorSpecs) +
        facet_wrap(~valueType, ncol=1, scales = "free_y") + expand_limits(y=100) + 
        labs(x="Year", y="Value") + theme_minimal()+ theme(
          axis.text = element_text(size = 14, face = "bold"),
          axis.title = element_text(size = 14, face = "bold"),
          strip.text = element_text(size= 14,face = "bold")
        ) + theme(legend.position = "none")
      ggplotly(g, width = histBarPlotWidth(), height = histPlotHeight(), tooltip = "text", hoverinfo = "none") %>% event_unregister("plotly_hover")
    } 
  })
  
  ############## END ############# Functions For 2nd Page ####################################################
  
  ################################ Functions For third Page ####################################################
  #### Creating the plot with draw vertical line to the starting horizontal line from the hovered point 
   drawPlot_3rd<- function(d, inputData,yr, add_horizontal, genderType, educationType, errorBar){
    ##print(names(inputData))
    #print("Output from function")
    #print("d")
    
    minYear <- input$snd_range[1]
    maxYear <- input$snd_range[2]
    data <- inputData %>% filter(Year >= minYear, Year <= maxYear)
    data <- data[!is.na(data$Income),]
    data <- data %>% filter(Gender == genderType, Education == educationType)
    ##print(data)
    VerticallinePoint <- data[data$Year==yr,"Income"]
    HorizontalLine <- data[order(data$Year),"Income"]
    
    
    ##print(VerticallinePoint)
    ##print(HorizontalLine)
    
    thePlot <- ggplot() + 
      geom_line(data=data, aes(x=Year, y=Income, color=Constant)) + 
      geom_point(data=data, aes(x=Year, y=Income, color=Constant, text = map(
        paste(
          '<b>Year:</b>', Year, 
          '<br>','<b>Median Income: </b>', "$",Income,'<b> (</b>', "$",Income_SD,'<b>)</b>',
          '<br>','<b>Full time Number: </b>', NumberOfPeople,'<b> (</b>',NumberOfPeople_SD,'<b>)</b>',
          '<br>','<b>Full time percentage: </b>',Percentage, "%",'<b> (</b>',Percentage_SD,"%",'<b>)</b>',
          '<br>','<b>[Standard errors appear in parentheses]</b>',
          sep=""
        ), HTML))) + 
      scale_y_continuous(name = 'Median Annual Earning ($K)', labels = scaleFUN) + theme_minimal() + theme(legend.position = "none") + 
      scale_color_manual(values=c(FemaleColor,MaleColor)) + theme(
        axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(size=14, face = "blod"))
    
    ###print(paste("Whether add horizon", add_horizontal))
    if (add_horizontal==TRUE){ 
      
      thePlot <- thePlot + 
        geom_hline(yintercept = HorizontalLine[1]
                   ,color = MaleColor,linetype="dashed", alpha = 0.5)  +
        geom_hline(yintercept = HorizontalLine[2]
                   ,color = FemaleColor,linetype="dashed",alpha =0.5)
    } 
    ###print(d)
    
    # show the error bar
    if (errorBar) {
      print("Adding error bar")
      thePlot <- thePlot + geom_errorbar(
        data=data, aes(x=Year, ymin = Income - Income_SD, ymax = Income + Income_SD, color=Constant)
      ) + geom_line(data=data, aes(x=Year, y=Income, color=Constant))
    }
    
    if(is.null(d)==FALSE){
      if (d$curveNumber != 2 && d$curveNumber != 3) {
        thePlot <- thePlot 
        return(ggplotly(thePlot, tooltip = "text", width = 1000, height=histPlotHeight()))
      }
      if (abs(yr - as.integer(yr)) > 0) {
        #print("Invalid Error")
        return(ggplotly(thePlot, tooltip = "text", width = 1000, height=histPlotHeight()))
      }
      
      if(add_horizontal==TRUE){
        
        #print(d)
        index  <- if(d$curveNumber==2) 2 else 1
        #print(index)
        percChange <- (VerticallinePoint[index] - HorizontalLine[index]) / HorizontalLine[index] * 100
        
        y_preset_pos <- (VerticallinePoint[index] + HorizontalLine[index]) / 2
        if (percChange > 0 && percChange < 5) {
          y_preset_pos <- HorizontalLine[index] - 400
        } 
        else if (percChange < 0 && percChange > -5) {
          y_preset_pos <- HorizontalLine[index] + 400
        } 
        
        percChange <- paste(sprintf("%.02f", percChange), "%", sep="")
        
        offset <- ifelse((yr - minYear) / (maxYear - minYear) > 0.75, 1, -1)
        
        ##print(percChange)
        thePlot <- thePlot + 
          geom_segment(aes(x = yr, y = VerticallinePoint[index], xend = yr, 
                           yend = HorizontalLine[index])
                       ,color= if(d$curveNumber==3) MaleColor else FemaleColor,linetype="dashed") + geom_text(
                         aes(x = yr + offset, y = y_preset_pos,
                             label = percChange
                         ), color=if(d$curveNumber==3) MaleColor else FemaleColor)
        
        
      } else {
        thePlot <- thePlot + 
          geom_segment(aes(x = yr, y = VerticallinePoint[1], xend = yr, 
                           yend = VerticallinePoint[2])
                       ,color= FemaleColor,linetype="dashed")   
      }
    } 
    plts  <- ggplotly(thePlot,tooltip = "text", width = 1000, height=histPlotHeight())
    plts
  }
  
   drawHist_2nd <- reactive({
    # gonna change this 
    # number of people of two groups 
    fst_group_people <- paste(sndP_gender(), "_NumberOfPeopleWithEarningWhoWorkedInFullTimeInThousands_", sndP_data_type(), sep="")

    # percentage of prople of two groups 
    fst_group_percentage <- paste(sndP_gender(), "_PercentOfPersonsWithEarningsWhoWorkedFullTime_", sndP_data_type(), sep="")

    
    minYear = input$snd_range[1]
    maxYear = input$snd_range[2] 
    
    desiredData_unfinished <- dta %>% filter(
      Gender == sndP_gender(), Education == sndP_data_type(), Constant==TRUE
    ) %>% filter(Year >= minYear, Year <= maxYear)
    
    desiredData_perc <- desiredData_unfinished[, c("Year", "Gender", "Education", "Percentage", "Percentage_SD")]
    desiredData_perc <- desiredData_perc %>% mutate(valueType = "Percent of People with Earnings Who Work Full-time, Year Round") %>% rename(value=Percentage, sd = Percentage_SD)
    
    desiredData_num <- desiredData_unfinished[, c("Year", "Gender", "Education", "NumberOfPeople", "NumberOfPeople_SD")] 
    desiredData_num <- desiredData_num %>% mutate(valueType = "Number of People with Earnings Who Work Full-time, Year Round (in Thousands)") %>% rename(value=NumberOfPeople, sd = NumberOfPeople_SD)

    #print(names(desiredData_num))
    #print(names(desiredData_perc))
    
    desiredData <- rbind(desiredData_perc, desiredData_num) 
    
    desiredData <- desiredData %>% mutate(
      valueType = as.factor(valueType) 
    ) %>% rename(year=Year)
    
    d <- event_data("plotly_hover")
    #print(d)
    
    g <- ggplot() + geom_bar(data=desiredData, aes(x=year+0.001, y=value, text=textFunction_hist(desiredData)), 
                             stat="identity", position = "dodge2", width=widthSet_3rd(), fill=group_1_hist_color()) + 
      facet_wrap(~valueType, ncol=1, scales = "free_y") + expand_limits(y=100) + labs(x="Year", y="Value") + theme(legend.position = "none") + theme_minimal() + 
      theme(legend.position = "none") + theme(
        axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 14, face = "bold"),
        strip.text = element_text(size= 14, face = "bold"))
    
    if (is.null(d)) {
      g
    } else {
      clickedYear <- d["x"]  
      if (abs(clickedYear - as.integer(clickedYear)) > 0) {
        #print("histogram hovered activated")
        g
      }
      else if (clickedYear < minYear | clickedYear > maxYear) {
        g
      }
      else {
        desiredData <- desiredData %>%  mutate(hlightingStatus = ifelse(
          as.integer(year) == as.integer(clickedYear), "Highlighted", "Hidden"
        )) %>% mutate(hlightingStatus = as.factor(hlightingStatus)) %>% mutate(oriYear = year)
        
        g <- ggplot() + geom_bar(data=desiredData, aes(x=year+0.001, y=value, alpha=hlightingStatus, text=textFunction_hist(desiredData)), 
                                 stat="identity", position = "dodge2", width=widthSet_3rd(), fill=group_1_hist_color()) + 
          facet_wrap(~valueType, ncol=1, scales = "free_y") + expand_limits(y=100) + labs(x="Year", "values") + theme_minimal() + theme(legend.position = "none") + theme(
            axis.text = element_text(size = 14, face = "bold"),
            axis.title = element_text(size = 14, face = "bold"),
            strip.text = element_text(size=14, face = "bold"))
        
        g
      }
    }  
  })
  
  ############## END ############# Functions For third Page ####################################################
  
  ################################ Procedure For 1st page#####################################################
  calculateGroupDif <- function(group, year,targetDtaFlag){
    dta2 <- dta %>% filter(Constant == targetDtaFlag, Year==year,Education==group)
    paste("$",abs(dta2$Income[1]-dta2$Income[2]),sep="")
  }
  
page1out <- reactive({
     targetDtaFlag <-
       switch (input$rb, "Constant" = TRUE, "Current" = FALSE)
     if (currentCompare() == 1) {
       if (input$educationSub == "total") {
         totalDta <-
           dta %>% filter(Education == "Overall", Constant == targetDtaFlag)
         
         highYear <- -1
         diff <- -1
         for (year in unique(totalDta$Year)) {
           curData <- totalDta[totalDta$Year == year, ]$Income
           val <- abs(curData[1] - curData[2])
           if (val > diff) {
             diff <- val
             highYear <- year
           }
         }
         diff <- paste("$", diff, sep = "")
         count_as <-
           paste("counted as" , switch(
             input$rb,
             "Constant" = " constant 2019 dollar",
             "Current" = paste(" current dolloar in ", highYear)
           ), ".", sep = "")
         
         text <- paste(
           "From 1990 to 2019, the median annual earning of females was never higher than those of males, the maximum difference between female median annual earnings and male median annual earnings is " ,
           diff ,
           " occured in ",
           highYear,
           ", ",
           count_as,
           sep = ""
         )
         text
       } else if (input$educationSub == "all") {
         if (input$percentile == "All Groups") {
           profdif <- calculateGroupDif("Professional", 2019, targetDtaFlag)
           doctdif <- calculateGroupDif("Doctor", 2019, targetDtaFlag)
           NotComDif <-
             calculateGroupDif("Highschool not completed", 2019, targetDtaFlag)
           LessDif <-
             calculateGroupDif("LessThan9thGrade", 2019, targetDtaFlag)
           
           
           text <-
             paste(
               "Under the same educational attainment, the median annual earnings of females are consistently lower than those of males.
            Males with professional or doctor's degree are top
            two median annual earning groups, consistently from 1990 to 2019.
            Their median annual earnings are",
               profdif,
               "and",
               doctdif,
               "higher than those of similarly educated females in 2019. Females with uncompleted high school degrees or femals who has less than 9th grade are the two lowest median annual earning groups, from 1990 to 2019. Their median annual earnings are" ,
               NotComDif,
               "and",
               LessDif,
               "lower than those of similarly educated males in 2019."
             )
         } else if (input$percentile == "Lowest 12 Grps") {
           text <-
             paste(
               "Among those 12 groups with lower median annual earnings, ranked by 2018 constant money, only 5 out of 12 groups are males while 7 groups are females. "
             )
         } else {
           text <-
             paste(
               "Among those 6 groups with lower median annual earnings, ranked by 2018 constant money, only 2 out of 6 groups are males while 4 groups are females, numbers of female groups is two times of the number of males group. "
             )
         }
         
       } else if (input$educationSub == "element") {
         text <- paste(
           "Females with uncompleted high school degrees and females has less than 9th grade are the two groups with the lowest median annual earnings among those groups which only got an elementary, secondary or lower degree. The median annual earnings of females with high school completed degrees is kind of similar to the median annual earnings of those males with high school not completed since 2000. It seems that the median annual earnings of males with high school completed degrees are at another level compared to other groups shown here."
         )
         text
       } else {
         text <- paste(
           "The median annual earnings of males with associate's degree were even higher than the median annual earnings of females with bachelor's degree in most of years from 1990 to 2019." ,
           "Furthermore, the median annual earnings of females with a doctor's degree is getting closer to males with a master's degree since 2014.",
           "The median annual earnings of females with both doctor's and professional degrees are still much more lower than the median annual earnings of males with a same doctor's and professional degree.",
           sep = " "
         )
       }
     } else {
       if (input$twoGraphRadio == "all") {
         text <- paste("Regardless of Male or Female, the rank of median annual earnings for educational attainment is: Professoinal > Doctor's > Master's > Bachelor's > Associate's degree > College with no degree > High school completed > High school not completed > Less than 9th degree.",
                       "The consistent lower median annual earnings of females compared to males under the same educational attainment can be shown here as well.", sep ="\n")
       }
       else{
         text <- paste("Regardless of Male or Female, the people with bachelor's or higher educational attainment have consistently higher median annual earnings than overall.",
                       "Again, even with bachelor's or higher educational attainment, males are consistently having higher median annual earnings than females from 1990 to 2019.", sep="\n")
       }
     }
     
     
     
     
   })
   
   output$firstPageResult <-renderText({
     page1out() 
   })
  currentLine <- reactiveVal(0)
  currentCompare <- reactiveVal(1)
  output$distPlot <- renderPlotly({
    plts <- drawPlot()
    plts
  })
  
  onclick("text1", function(event) {
    removeClass("text3", "underline")
    addClass("text1", "underline")
    shinyjs::show("GenderCompareBlock")
    shinyjs::hide("degreeCompareBlock")
    shinyjs::hide("text2")
    shinyjs::show("text3")
    currentCompare(1)
    output$distPlot <- renderPlotly({
      drawPlot()
    })
  })
  
  observeEvent(input$educationSub, {
    if (input$educationSub == "all") {
      shinyjs::show("sliderInputBlock")
    }  else {
      ##showNotification("Please do not use subgroup and y range together")
      shinyjs::hide("sliderInputBlock")
      updateSliderTextInput(session, "percentile", selected = "All Groups")
      output$distPlot <- renderPlotly({
        drawPlot()
      })
    }
    
  })
  
  onclick("text3", function(event) {
    removeClass("text1", "underline")
    addClass("text2", "underline")
    addClass("text3","underline")
    shinyjs::hide("GenderCompareBlock")
    shinyjs::show("degreeCompareBlock")
    shinyjs::hide("text3")
    shinyjs::show("text2")
    currentCompare(2)
    output$distPlot <- renderPlotly({
      drawGenderComparePlots()
    })
  })
  
  
  
  
  ########### END ################ Procedure For 1st page###############################################
  
  
  ################################ Plotly Event Control Centeer##########################
  
  output$warningMessage <- renderText({
    result <- "[Click the dots in the lines for more details]"
    result
  })
  
  output$warningMessage_2 <-renderText({ 
      result <-"[Hover the dots in the lines for more details]"  
     result
  })
  
  output$warningMessage_3 <-renderText({ 
    result <-"[Hover the dots in the lines for more details]"  
    result
  })
  
  observeEvent(event_data("plotly_click"),{
    minYear = input$range[1]
    maxYear = input$range[2]
    d <- event_data("plotly_click")
    if (d$x <= maxYear && d$x >= minYear && d$x%%1==0) {
      output$distPlot <- renderPlotly({
        if (currentCompare() == 2) {
          drawHighlightedComparePlot()
        } else {
          drawHighlightedPlot()
        }
      })
    }
  })
  
  observeEvent(event_data("plotly_hover"), {
    d = event_data("plotly_hover")
    
    minYear = input$tgp_range[1]
    maxYear = input$tgp_range[2]
    
    #### Diverse the event into from page 1 page 2 and page 3 
    if (currentLevel() == 1) { # from page 1
       
    } else if (currentLevel() == 2){
       
    } else if (currentLevel() == 3) {
      #print(currentLevel())
      clickedYear_3 <- d["x"]
      #print(" Happened")
      if (abs(clickedYear_3 - as.integer(clickedYear_3)) > 0) {
        #print("Invalid Event Eended")
        return()
      }
      if (clickedYear_3 < 1990 | clickedYear_3 > 2020) {
        return()
      }
    }
  })
  
  observeEvent(event_data("plotly_unhover"), {
    d = event_data("plotly_unhover")
    if (currentLevel() == 1) {
      output$distPlot <- renderPlotly({
        if (currentCompare() == 2) {
          drawGenderComparePlots()
        }
        else {
          drawPlot()
        }
        
      })
    } else if (currentLevel() == 2) {
       
    } else if (currentLevel() == 3) {
      # Computed in created function for page 3
    }
  })
  ############## End ################# Plotly Event Control Center ##########################
  
  #################################### Procedures for second page ##########################
  
observeEvent(input$sndP_errorBar, { 
    showSdError(input$sndP_errorBar)
  })
  
  observeEvent(input$back,{
    #### Before hide the second page, make sure to reset the 
    #### drop down list added during operation at the second page  
    show_snd_type(TRUE)
    
    #### Hide Second page 
    #### and show first page overview
    hide("secondPage")
    show("firstPage")
    currentLevel(1)
  })
  
  observeEvent(input$gender, {
    show("Educational Attainment")
    gender(input$gender)
    output$scatterPlot <- renderPlotly({drawSctLinePlot()})
    changeLegend()
  }, ignoreInit = FALSE)
  
  observeEvent(input$educationalLevel, {
    if (input$educationalLevel == "total") {
      hide("ElementaryLevels")
      hide("CollegeLevels")
      hide("BachelorOrHigherLevels") 
      data_type("Overall")
    }
    
    if (input$educationalLevel == "elementary") {
      #print("Setting education to elementary")
      hide("BachelorOrHigherLevels")
      hide("CollegeLevels")
      show("ElementaryLevels")
      data_type("LessThan9thGrade")
    }
    
    if (input$educationalLevel == "college") {
      hide("ElementaryLevels")
      hide("BachelorOrHigherLevels")
      show("CollegeLevels")
      data_type("College no degree")
    }
    
    changeLegend()
    output$scatterPlot <- renderPlotly({drawSctLinePlot()})
    
  }, ignoreInit = TRUE)
  
  observeEvent(input$elementaryLevels, {
    if (input$elementaryLevels == "lessThan9") {
      data_type("LessThan9thGrade")
      hide("BachelorOrHigherLevels") 
      hide("CollegeLevels")
    }
    
    if (input$elementaryLevels == "someHighSchool") {
      data_type("Highschool not completed")
      hide("BachelorOrHigherLevels") 
      hide("CollegeLevels")
    }
    
    if (input$elementaryLevels == "highSchoolComplete") {
      data_type("Highschool completed")
      hide("BachelorOrHigherLevels") 
      hide("CollegeLevels")
    }
    changeLegend()
    output$scatterPlot <- renderPlotly({drawSctLinePlot()})
  }, ignoreInit = TRUE)
  
  observeEvent(input$collegeLevels, {
    if (input$collegeLevels == "someCollege") {
      hide("BachelorOrHigherLevels")
      data_type("College no degree")
    }
    
    if (input$collegeLevels == "associateDegree") {
      hide("BachelorOrHigherLevels")
      data_type("AssociateDegree")
    }
    
    if (input$collegeLevels == "bachelorOrHigher") {
      show("BachelorOrHigherLevels")
      #print("Error may happen")
      data_type("BachelorOrHigher")
    }
    changeLegend()
    output$scatterPlot <- renderPlotly({drawSctLinePlot()})
  }, ignoreInit = TRUE)
  
  observeEvent(input$bachelorOrHigherLevels, {
    #"Total of bachelor or highers" = "bachelorHigherTotal",
    #"Bachelor's degree" = "bachelor",
    #"Master's degree" = "master",
    #"Professional degree" = "professionDegree",
    #"Doctor's degree" = "doctor"
    if (input$bachelorOrHigherLevels == "bachelorHigherTotal"){
      
      #print("Error may happen 1")
      data_type("BachelorOrHigher")}
    if (input$bachelorOrHigherLevels == "bachelor"){
      data_type("Bachelor")}
    if (input$bachelorOrHigherLevels == "master"){
      data_type("Master")}
    if (input$bachelorOrHigherLevels == "professionDegree"){
      data_type("Professional")}
    if (input$bachelorOrHigherLevels == "doctor") {
      data_type("Doctor")}
    
    changeLegend()
    output$scatterPlot <- renderPlotly({drawSctLinePlot()})
  }, ignoreInit = TRUE)
  
  #### controller for second sets of selection input
  # observeEvent(input$add2compare, {
  #   show("snd_gender")
  #   show("remove2ndGroup")
  #   show("snd_education_level")
  #   show("comparisonPlots")
  #   show_snd_type(TRUE)
  #   output$scatterPlot <- renderPlotly({drawSctLinePlot()})
  # }, ignoreInit = T)
  # 
  observeEvent(input$snd_gender, {
    show("snd_education_level")
    snd_gender(input$snd_gender)
    output$scatterPlot <- renderPlotly({drawSctLinePlot()})
    
    changeLegend()
  }, ignoreInit = T)
  
  observeEvent(input$snd_educationalLevel, {
    if (input$snd_educationalLevel == "total") {
      hide("snd_ElementaryLevels")
      hide("Snd_collegeLevels")
      hide("snd_BachelorOrHigherLevels")
      snd_data_type("Overall")
    }
    
    if (input$snd_educationalLevel == "elementary") {
      show("snd_ElementaryLevels")
      hide("Snd_collegeLevels")
      hide("snd_BachelorOrHigherLevels")
      snd_data_type("LessThan9thGrade")
    }
    
    if (input$snd_educationalLevel == "college") {
      hide("snd_ElementaryLevels")
      hide("snd_BachelorOrHigherLevels")
      show("Snd_collegeLevels")
      snd_data_type("College no degree")
    }
    output$scatterPlot <- renderPlotly({drawSctLinePlot()})
    changeLegend()
  }, ignoreInit = T)
  
  observeEvent(input$snd_elementaryLevels, {
    if (input$snd_elementaryLevels == "lessThan9") {
      snd_data_type("LessThan9thGrade")
    }
    
    if (input$snd_elementaryLevels == "someHighSchool") {
      snd_data_type("Highschool not completed")
    }
    
    if (input$snd_elementaryLevels == "highSchoolComplete") {
      snd_data_type("Highschool completed")
    }
    output$scatterPlot <- renderPlotly({drawSctLinePlot()})
    changeLegend()
  }, ignoreInit = T)
  
  observeEvent(input$snd_collegeLevels, {
    if (input$snd_collegeLevels == "someCollege") {
      hide("snd_BachelorOrHigherLevels")
      snd_data_type("College no degree")
    }
    
    if (input$snd_collegeLevels == "associateDegree") {
      hide("snd_BachelorOrHigherLevels")
      snd_data_type("AssociateDegree")
    }
    
    if (input$snd_collegeLevels == "bachelorOrHigher") {
      show("snd_BachelorOrHigherLevels")
      snd_data_type("BachelorOrHigher")
    }
    output$scatterPlot <- renderPlotly({drawSctLinePlot()})
    changeLegend()
  }, ignoreInit = T)
  
  observeEvent(input$snd_bachelorOrHigherLevels, {
    #"Total of bachelor or highers" = "bachelorHigherTotal",
    #"Bachelor's degree" = "bachelor",
    #"Master's degree" = "master",
    #"Professional degree" = "professionDegree",
    #"Doctor's degree" = "doctor"
    if (input$snd_bachelorOrHigherLevels == "bachelorHigherTotal"){
      snd_data_type("BachelorOrHigher")}
    if (input$snd_bachelorOrHigherLevels == "bachelor"){
      snd_data_type("Bachelor")}
    if (input$snd_bachelorOrHigherLevels == "master"){
      snd_data_type("Master")}
    if (input$snd_bachelorOrHigherLevels == "professionDegree"){
      snd_data_type("Professional")}
    if (input$snd_bachelorOrHigherLevels == "doctor") {
      snd_data_type("Doctor")}
    output$scatterPlot <- renderPlotly({drawSctLinePlot()})
    changeLegend()
  }, ignoreInit = T)
  
  # 
  # observeEvent(input$remove2ndGroup, {
  #   hide("remove2ndGroup")
  #   hide("snd_BachelorOrHigherLevels")
  #   hide("snd_collegeLevels")
  #   hide("snd_ElementaryLevels")
  #   hide("snd_education_level")
  #   hide("snd_gender") 
  #   hide("comparisonPlots")
  #   show_snd_type(FALSE)
  #   output$scatterPlot <- renderPlotly({drawSctLinePlot()})
  # })
  
  #### Second set of controller finished 
  
  
  
  # render plot histogram percentage comparison
  output$hist2 <- renderPlotly({
    plot2groupBar()
  })
  
  savedPlot <- reactiveVal(NULL)
  
  output$scatterPlot <- renderPlotly({
    drawSctLinePlot()
  })
  
  extractData <- function(inputX, inputY, inputData, minX, maxX, groupCount, genders, educations) {
    resultData <- NULL
    resultData_cos <- NULL
    for (i in 1:groupCount) {
      curFilteredData <- inputData %>% filter(Gender == genders[i], Education == educations[i],
                                              Year == inputX, Constant=="FALSE")
      curFilteredData_cos <- inputData %>% filter(Gender == genders[i], Education == educations[i],
                                                  Year == inputX, Constant=="TRUE")
      ##print(curFilteredData_cos)
      ##print(curFilteredData)
      resultData <- rbind(resultData, curFilteredData)      
      resultData_cos <- rbind(resultData_cos, curFilteredData_cos)
    }
    return(list(currentData = resultData, constantData=resultData_cos))
  }
   
  
  fstGroupNameRes <- reactive({
    middlePart = ""
    if (data_type()!="Overall" ) {
      middlePart = " with "
    }
    paste(tolower(gender()), middlePart, mappingFunc(data_type()), sep="")
  })
  
  sndGroupNameRes <- reactive({
    middlePart = ""
    if (snd_data_type()!="Overall" ) {
      middlePart = " with "
    }
    paste(tolower(snd_gender()), middlePart, mappingFunc(snd_data_type()), sep="")
  }) 
  
  fstGroupNameRes_legend <- reactive({
    middlePart = ""
    if (data_type()!="Overall" ) {
      middlePart = " with "
    }
    paste(gender(), middlePart, mappingFunc(data_type()), sep="")
  })
  
  sndGroupNameRes_legend <- reactive({
    middlePart = ""
    if (snd_data_type()!="Overall" ) {
      middlePart = " with "
    }
    paste(snd_gender(), middlePart, mappingFunc(snd_data_type()), sep="")
  }) 
  
  output$group1Cos <- renderText({
    # if (gender() == snd_gender() & data_type() == snd_data_type()) {
    #   hide("line1Legend")
    # } else {
    #   shinyjs::show("line1Legend")
    # }
    paste(fstGroupNameRes_legend(), ", ",  "2019 constant dollar", sep="")
  })
  
  output$group1Curr <- renderText({
    # if (gender() == snd_gender() & data_type() == snd_data_type()) {
    #   hide("line2Legend")
    # } else {
    #   shinyjs::show("line2Legend")
    # }
    paste(fstGroupNameRes_legend(), ", ",  "current dollar", sep="")
  })
  
  output$group2Cos <- renderText({
    paste(sndGroupNameRes_legend(),", ",  "2019 constant dollar", sep="")
  })
  
  output$group2Curr <- renderText({
    paste(sndGroupNameRes_legend(), ", ",  "current dollar", sep="")
  })
  
  output$group1 <- renderText({  
    # if (gender() == snd_gender() & data_type() == snd_data_type()) {
    #   hide("bar1Legend")
    # } else {
    #   shinyjs::show("bar1Legend")
    # }
    fstGroupNameRes_legend()
  })
  
  output$group2 <- renderText({
    sndGroupNameRes_legend()
  })
  
  responsiveText <- reactive({
    d <- event_data("plotly_hover") 
    ##print("hello")
    groupCount = 2
    educations <- c(data_type(), snd_data_type())
    genders <- c(gender(), snd_gender())
    if (educations[1] == educations[2] & genders[1] == genders[2]) {
      groupCount = 1
      educations = educations[1:1]
      genders = genders[1:1]
      result <- paste("You are comparing the same group.")
      return(result)
    }
    
    endData <- extractData(2019, inputY, dta, input$tgp_range[1], input$tgp_range[2], 
                           groupCount, genders, educations)
    endDataCurr <- endData$currentData
    endDataCur_1st <- endDataCurr %>% filter(Gender == gender(), Education == data_type())
    endDataCur_2nd <- endDataCurr %>% filter(Gender == snd_gender(), Education == snd_data_type())
    higherAmount <- endDataCur_1st$Income[1] - endDataCur_2nd$Income[1]
    
    result <- paste("Median annual earning of ", fstGroupNameRes(), " is $",
                    abs(higherAmount), " ", ifelse(higherAmount >= 0, "higher", "lower"), 
                    " than the median annual earning of ", sndGroupNameRes(), " in 2019.", sep="")
    if (!is.null(d)) {
      inputX = d["x"]
      inputY = d["y"]
      if (abs(inputX - as.integer(inputX)) > 0) {
        return(result)
      }
      inputX <- as.integer(inputX)
      inputY <- as.numeric(inputY) 
      if (inputX == 2019) {
        return(result)
      }
      resultData <- extractData(inputX, inputY, dta, input$range[1], input$range[2], 
                                groupCount, genders, educations)
      cosData = resultData$constantData
      isConstant = ifelse(mean(cosData$Constant == "TRUE" & cosData$Income == inputY) > 0, TRUE, FALSE)
      firstGroupIncome <- 0
      secondGroupIncome <- 0
      curData_fstGroup <- NULL
      curData_sndGroup <- NULL
      print("NA detected")
      if (is.na(isConstant)) {return(result)}
      if (isConstant) {
        curData_fstGroup <- resultData$constantData %>% filter(Gender == gender(), Education == data_type())
        curData_sndGroup <- resultData$constantData %>% filter(Gender == snd_gender(), Education == snd_data_type())
      } else { 
        curData_fstGroup <- resultData$currentData %>% filter(Gender == gender(), Education == data_type())
        curData_sndGroup <- resultData$currentData %>% filter(Gender == snd_gender(), Education == snd_data_type())
      }
      
      firstGroupIncome <- curData_fstGroup$Income[1]
      secondGroupIncome <- curData_sndGroup$Income[1]
      
      clickedHigher <- firstGroupIncome - secondGroupIncome
      curResult <- paste("In year ", inputX, ", the median annual earning of ", fstGroupNameRes(), " is $", abs(clickedHigher) , " ", ifelse(clickedHigher >= 0, "higher", "lower"),
                         " than the median annual earning of ", sndGroupNameRes(), ", counted as ", 
                         ifelse(isConstant, "2019 constant dollar", "current dollar"), ". ", sep="")
      
      result <- paste(curResult, result, sep="")
    }  
    result
  })
  
  output$thinBar <- renderText({
    responsiveText()
  })
  
  changeLegend <- reactive({
    if (gender() == snd_gender() && data_type() == snd_data_type()) {
      hide("line1Legend")
      hide("line2Legend")
      hide("bar2Legend")
    } else {
      show("line1Legend")
      show("line2Legend")
      show("bar2Legend")
    }
  })
  
  ############## End ################# Procedure for second page end #######################
  
  #################################### Procedure for third page ############################
  myModal <- function(){
    showModal(modalDialog(
      title = p(style="size: 20px; font-weight: bold","Important Messages"),
      p(class ="modalText",
        tags$span(style ="color:red !important;font-weight:bold;","This page is designed for 1920x1080 pixels resolution, using a screen which is not 1080P pixels resolution will influence functions and typography in the website pages!"),
        br(),
        br(),
        tags$span(style ="color:blue !important;font-weight:bold;","Warning: There is high uncertainty for estimated data in 2019 because of Covid-19 pandemic. "),
        br(),
        br(),
        tags$span("1. NA means the data is not available or not applicable in this whole visualization"),
        br(),
        br(),
        tags$span("2. Some high school, no completion includes 1 to 3 years of high school for 1990."),
        br(),
        br(), 
        tags$span("3. High school completion includes 4 years of high school for 1990."),
        br(),
        br(),
        tags$span("4. Some college, no degree includes 1 to 3 years of college and associate's degrees for 1990."),
        br(),
        br(),
        tags$span("5. Bachelor's or higher degree includes 4 or more years of college for 1990."),
        br(),
        br(),
        tags$span("6. Bachelor's degree includes 4 years of college for 1990."),
        br(),
        br(),
        tags$span("7.	Data for Assiciate's degree, Master's degree, Professional degree and Doctor's degree is not reported separately for 1990."),
        br(),
        br(),
        tags$span("8. Data of percent of persons with earnings who worked full time is not available for 1990 and 1995."),
        br(),
        br(),
        tags$span("9. Constant 2019 dollars based on the Consumer Price Index, prepared by the Bureau of Labor Statistics, U.S. Department of Labor."),
        br(),
        br(),
        tags$span("NOTE: Data are based on sample surveys of the noninstitutionalized population, which excludes persons living in institutions (e.g., prisons or nursing facilities); 
                data include military personnel who live in households with civilians but exclude those who live in military barracks. Caution should be used when comparing 2019 estimates 
                to those of prior years due to the impact that the coronavirus pandemic had on interviewing and response rates. For additional information about the impact of the coronavirus 
                pandemic on the Current Population Survey data collection, please see https://www2.census.gov/programs-surveys/cps/techdocs/cpsmar20.pdf. Detail may not sum to totals because of rounding."),
        
        br(),
        br(),
        tags$span("SOURCE: U.S. Department of Commerce, Census Bureau, Current Population Reports, Series P-60, Money Income of Households, Families, and Persons in the United States and Income, Poverty, 
                and Valuation of Noncash Benefits, 1990; Series P-60, Money Income in the United States, 1995 through 2002; and Current Population Survey (CPS), Annual Social and Economic Supplement, 
                2003 through 2020. Retrieved January 29, 2021, from https://www.census.gov/data/tables/time-series/demo/income-poverty/cps-pinc/pinc-03.html. (This table was prepared January 2021.)")
      ),
      easyClose = TRUE,
      footer =  modalButton("Close")
    ))
  } 

  myModal()

  onclick("reference", function(event){
    myModal()
  })
  
  onclick("reference_2", function(event){
    myModal()
  })
  
  onclick("reference_3", function(event){
    myModal()
  })
  
  observeEvent(input$snd_sndP_errorBar, {
    # change whether set error bar
    snd_showSdError(input$snd_sndP_errorBar)
  })
  
  observeEvent(input$snd_back, {
    hide("snd_secondPage")
    show("firstPage") 
    currentLevel(1)
  })
  
  observeEvent(input$fstSecondPage, {
    hide("snd_secondPage")
    show("secondPage")
    currentLevel(2)
  })
  
  observeEvent(input$s_gender_sndlv2Page, {
    show("s_Educational Attainment_sndlv2Page")
    sndP_gender(input$s_gender_sndlv2Page)
  }, ignoreInit = FALSE)
  
  
  observeEvent(input$s_educationalLevel_sndlv2Page, {
    if (input$s_educationalLevel_sndlv2Page == "total") {
      hide("s_ElementaryLevels_sndlv2Page")
      hide("S_collegeLevels_sndlv2Page")
      hide("s_BachelorOrHigherLevels_sndlv2Page") 
      sndP_data_type("Overall")
    }
    
    if (input$s_educationalLevel_sndlv2Page == "elementary") {
      show("s_ElementaryLevels_sndlv2Page")
      hide("S_collegeLevels_sndlv2Page")
      hide("s_BachelorOrHigherLevels_sndlv2Page")
      sndP_data_type("LessThan9thGrade")
    }
    
    if (input$s_educationalLevel_sndlv2Page == "college") {
      hide("s_ElementaryLevels_sndlv2Page")
      hide("s_BachelorOrHigherLevels_sndlv2Page")
      show("S_collegeLevels_sndlv2Page")
      sndP_data_type("College no degree")
    }
    
  }, ignoreInit = TRUE)
  
  observeEvent(input$s_elementaryLevels_sndlv2Page, {
    if (input$s_elementaryLevels_sndlv2Page == "lessThan9") {
      sndP_data_type("LessThan9thGrade")
      hide("s_BachelorOrHigherLevels_sndlv2Page") 
      hide("S_collegeLevels_sndlv2Page")
    }
    
    if (input$s_elementaryLevels_sndlv2Page == "someHighSchool") {
      sndP_data_type("Highschool not completed")
      hide("s_BachelorOrHigherLevels_sndlv2Page") 
      hide("S_collegeLevels_sndlv2Page")
    }
    
    if (input$s_elementaryLevels_sndlv2Page == "highSchoolComplete") {
      sndP_data_type("Highschool completed")
      hide("s_BachelorOrHigherLevels_sndlv2Page") 
      hide("S_collegeLevels_sndlv2Page")
    }
  }, ignoreInit = T)
  
  observeEvent(input$s_collegeLevels_sndlv2Page, {
    if (input$s_collegeLevels_sndlv2Page == "someCollege") {
      hide("s_BachelorOrHigherLevels_sndlv2Page")
      sndP_data_type("College no degree")
    }
    
    if (input$s_collegeLevels_sndlv2Page == "associateDegree") {
      hide("s_BachelorOrHigherLevels_sndlv2Page")
      sndP_data_type("AssociateDegree")
    }
    
    if (input$s_collegeLevels_sndlv2Page == "bachelorOrHigher") {
      show("s_BachelorOrHigherLevels_sndlv2Page")
      #print("Setting bachelor")
      sndP_data_type("BachelorOrHigher")
    }
  }, ignoreInit = T)
  
  observeEvent(input$s_bachelorOrHigherLevels_sndlv2Page, {
    #"Total of bachelor or highers" = "bachelorHigherTotal",
    #"Bachelor's degree" = "bachelor",
    #"Master's degree" = "master",
    #"Professional degree" = "professionDegree",
    #"Doctor's degree" = "doctor"
    if (input$s_bachelorOrHigherLevels_sndlv2Page == "bachelorHigherTotal"){
      
      #print("Setting bachelor")
      sndP_data_type("BachelorOrHigher")}
    if (input$s_bachelorOrHigherLevels_sndlv2Page == "bachelor"){
      sndP_data_type("Bachelor")}
    if (input$s_bachelorOrHigherLevels_sndlv2Page == "master"){
      sndP_data_type("Master")}
    if (input$s_bachelorOrHigherLevels_sndlv2Page == "professionDegree"){
      sndP_data_type("Professional")}
    if (input$s_bachelorOrHigherLevels_sndlv2Page == "doctor") {
      sndP_data_type("Doctor")}
  }, ignoreInit = T)
  
  
  output$scatterPlot_sndlv2Page <- renderPlotly({
    d <- event_data("plotly_hover")
    #print(" this ") 
    plts <- drawPlot_3rd(d,dta,d$x,TRUE,sndP_gender(),sndP_data_type(), snd_showSdError()) # event_data, dataframe, event_x, if need add line
  })
  
  mappingFunc <- function(educationType) {
    if (educationType == "Overall") {
      return("")
    } else if (educationType == "LessThan9thGrade") {
      return("less than 9th grade degree")
    } else if (educationType == "Highschool not completed") {
      return("high school not completed")
    } else if (educationType == "College no degree") {
      return("college no degree") 
    } else if (educationType == "Highschool completed") {
      return("high school degree")
    } else if (educationType == "AssociateDegree") {
      return("associate degree")
    } else if (educationType == "BachelorOrHigher") {
      return("bachelor or higher degree")
    } else if (educationType == "Bachelor"){
      return("bachelor degree")
    } else if (educationType == "Master") {
      return("master degree")
    } else if (educationType == "Professional") {
      return("professional degree")
    } else if (educationType == "Doctor") {
      "doctor degree"
    }
  }
   
  output$sgGroupHist <- renderPlotly({
    ggplotly(drawHist_2nd(), width=histBarPlotWidth_3rd(), height = histPlotHeight(), tooltip="text")  %>% event_unregister("plotly_hover")
  })
  
  output$snd_group1Cos <- renderText({
    "2019 constant dollar"
  })
  
  output$snd_group1Curr <- renderText({
    "Current dollar"
  })
  
  sndPage_groupName <- reactive({
    
    middlePart = ""
    if (sndP_data_type() != "Overall" ) {
      middlePart = " with "
    }
    paste(tolower(sndP_gender()), middlePart, mappingFunc(sndP_data_type()), sep="")
  })
  
  sndPage_groupName_legend <- reactive({
    
    middlePart = ""
    if (sndP_data_type() != "Overall" ) {
      middlePart = " with "
    }
    paste(sndP_gender(), middlePart, mappingFunc(sndP_data_type()), sep="")
  })
  
  
  
  output$snd_group1 <- renderText({
    #print(sndPage_groupName())
    sndPage_groupName_legend()
  })
  
  
  responsiveText_2 <- reactive({
    d <- event_data("plotly_hover") 
    
    curMinYear <- input$snd_range[1]
    curMaxYear <- input$snd_range[2]
    curGender <- sndP_gender()
    curDataType <- sndP_data_type()
  
    constantData <- dta %>% filter(Gender == curGender, 
                                   Education == curDataType, Constant=="TRUE", !is.na(Income), !is.na(Year))%>% arrange(Year)
    currentData <- dta %>% filter(Gender == curGender, 
                                  Education == curDataType, Constant=="FALSE", !is.na(Income), !is.na(Year))  %>% arrange(Year)
    
    constantData <- constantData %>% filter(Year >= curMinYear, Year <= curMaxYear)
    currentData <- currentData %>% filter(Year >= curMinYear, Year <= curMaxYear)
    
    lastIndex <- NROW(constantData)
    consLastIncome <- constantData$Income[lastIndex]
    consLastYear <- constantData$Year[lastIndex]
    currentLastIncome <- currentData$Income[lastIndex]
    currentLastYear <- currentData$Year[lastIndex]
    
    consFirstIncome <- constantData$Income[1]
    consFirstYear <- constantData$Year[1]
    currentFirstIncome <- currentData$Income[1]
    currentFirstYear <- currentData$Year[1]
    
    consPercChange <- (consLastIncome - consFirstIncome) / consFirstIncome * 100
    currPercChange <- (currentLastIncome - currentFirstIncome) / currentFirstIncome * 100
    
    cosWord <- ifelse(consPercChange >= 0, "higher", "lower")
    currword <- ifelse(currPercChange >= 0, "higher", "lower")
    
    consPercChange <- sprintf("%.2f", abs(consPercChange))
    currPercChange <- sprintf("%.2f", abs(currPercChange))
    
    # constantData <- constantData %>% filter(Year >= curMinYear)
    # currentData <- currentData %>% filter(Year >= curMinYear)
    movingStartYear_cos <- constantData$Year[1]
    movingStartIncome_cos <- constantData$Income[1]
    movingStartYear_curr <- currentData$Year[1]
    movingStartIncome_curr <- currentData$Income[1]
    
    result <- paste(
      "Based on current dollar, the median annual earning of ", sndPage_groupName(), 
      " in ", currentLastYear, " is ", currPercChange, "% ", currword, " than ", currentFirstYear, ". ", 
      "Based on 2019 constant dollar, the median annual earning of ", sndPage_groupName(), 
      " in ", consLastYear, " is ", consPercChange, "% ", cosWord, " than ", consFirstYear, ".", 
      sep=""
    )
    
    if (!is.null(d)) {
      clickedYear <- d["x"]
      print(clickedYear)
      if (abs(clickedYear - as.integer(clickedYear)) > 0.001) {
        print("returning")
        return(result)
      }
      clickedYear <- as.integer(clickedYear)
      choosenY <- d["y"]
      choosenY <- as.numeric(choosenY)
      
      selectedData <- dta %>% filter(Year == clickedYear, Gender==sndP_gender(),
                                     Education == sndP_data_type(), Income == choosenY)
      if (NROW(selectedData) != 1) {
        print("returning 2")
        return(result)
      }
      
      isConstant <- selectedData$Constant[1] == "TRUE"
      theRealLast <- ifelse(isConstant, consLastYear, currentLastYear)
      if (clickedYear == theRealLast) {
        print("It is already on the screen")
        return(result)
      }
      
      dollarType <- ifelse(isConstant, "2019 costant dollar", "current dollar")
      choosenStart <- ifelse(isConstant, movingStartIncome_cos, movingStartIncome_curr) 
      movingFstYear <- ifelse(isConstant, movingStartYear_cos, movingStartYear_curr) 
      interPercChange <- (choosenY - choosenStart) / choosenStart * 100
      interWord <- ifelse(interPercChange >= 0, "higher", "lower")
      interPercChange <- sprintf("%.2f", abs(interPercChange))
      curResult <- paste(
        "Based on ", dollarType, ", the median annual earning of ", sndPage_groupName(),
        " in ", clickedYear, " is ", interPercChange, "% ",interWord, " than ", movingFstYear, ". ", sep=""
      )
      
      result <- paste(curResult, result, sep="")
    } 
    
    result
  }) 
  
  output$thinBar_2 <- renderText({
    responsiveText_2()
  })
  ############## End ################# Procedure for third page end ############################
}

# Run the application
shinyApp(ui = ui, server = server)