library(ggplot2) # data visualization
library(magrittr) # pipe operator
library(dplyr) # manipulate data
library(GGally) # ggpairs()
library(plotly) # ggpairs()

# Make dashboard
library(shiny)
library(shinydashboard)

# For LDA
library(tidyverse)
library(caret)
library(MASS)


patients = read.csv("Downloads/patients.csv")
patients$Mortality <- ifelse(patients$P8 == "A",1,0)
patients$MortalityID <- ifelse(patients$P8 == "A","Alive","Dead")
traj = read.csv("Downloads/traj.csv")
doctors = read.csv("Downloads/doctors.csv")
eval = read.csv("Downloads/eval.csv")
SOFA_Diff <- read.csv("Downloads/SOFA_Diff.csv")

patients_1 = subset(patients, patients$DocID == "doc-01")
patients_2 = subset(patients, patients$DocID == "doc-02")
patients_3 = subset(patients, patients$DocID == "doc-03")
patients_4 = subset(patients, patients$DocID == "doc-04")
patients_5 = subset(patients, patients$DocID == "doc-05")
patients_6 = subset(patients, patients$DocID == "doc-06")
patients_7 = subset(patients, patients$DocID == "doc-07")
patients_8 = subset(patients, patients$DocID == "doc-08")
patients_9 = subset(patients, patients$DocID == "doc-09")
patients_10 = subset(patients, patients$DocID == "doc-10")
patients_11 = subset(patients, patients$DocID == "doc-11")
patients_12 = subset(patients, patients$DocID == "doc-12")
patients_13 = subset(patients, patients$DocID == "doc-13")
patients_14 = subset(patients, patients$DocID == "doc-14")
patients_15 = subset(patients, patients$DocID == "doc-15")
patients_16 = subset(patients, patients$DocID == "doc-16")
patients_17 = subset(patients, patients$DocID == "doc-17")
patients_18 = subset(patients, patients$DocID == "doc-18")
patients_19 = subset(patients, patients$DocID == "doc-19")
patients_20 = subset(patients, patients$DocID == "doc-20")
patients_21 = subset(patients, patients$DocID == "doc-21")
patients_22 = subset(patients, patients$DocID == "doc-22")
patients_23 = subset(patients, patients$DocID == "doc-23")
patients_24 = subset(patients, patients$DocID == "doc-24")
patients_25 = subset(patients, patients$DocID == "doc-25")

doc_01 = subset(traj, PtID %in% patients_1$PtID)
doc_02 = subset(traj, PtID %in% patients_2$PtID)
doc_03 = subset(traj, PtID %in% patients_3$PtID)
doc_04 = subset(traj, PtID %in% patients_4$PtID)
doc_05 = subset(traj, PtID %in% patients_5$PtID)
doc_06 = subset(traj, PtID %in% patients_6$PtID)
doc_07 = subset(traj, PtID %in% patients_7$PtID)
doc_08 = subset(traj, PtID %in% patients_8$PtID)
doc_09 = subset(traj, PtID %in% patients_9$PtID)
doc_10 = subset(traj, PtID %in% patients_10$PtID)
doc_11 = subset(traj, PtID %in% patients_11$PtID)
doc_12 = subset(traj, PtID %in% patients_12$PtID)
doc_13 = subset(traj, PtID %in% patients_13$PtID)
doc_14 = subset(traj, PtID %in% patients_14$PtID)
doc_15 = subset(traj, PtID %in% patients_15$PtID)
doc_16 = subset(traj, PtID %in% patients_16$PtID)
doc_17 = subset(traj, PtID %in% patients_17$PtID)
doc_18 = subset(traj, PtID %in% patients_18$PtID)
doc_19 = subset(traj, PtID %in% patients_19$PtID)
doc_20 = subset(traj, PtID %in% patients_20$PtID)
doc_21 = subset(traj, PtID %in% patients_21$PtID)
doc_22 = subset(traj, PtID %in% patients_22$PtID)
doc_23 = subset(traj, PtID %in% patients_23$PtID)
doc_24 = subset(traj, PtID %in% patients_24$PtID)
doc_25 = subset(traj, PtID %in% patients_25$PtID)

#DASHBOARD UI-----
ui = dashboardPage( #Create a dashboard page, uses library(shinydashboard)
  
#Colour
  skin="blue",

#Header
  dashboardHeader(title = "Physician Performance"),
  
#Sidebar menu
  dashboardSidebar(
    sidebarMenu(id="menu1", #Creates a menu of tab names 
                   menuItem("Health Improvement Performance", tabName="trajectory"),
                   menuItem("Critically Ill Patient Performance", tabName="ill"),
                   menuItem("Information", tabName="inf")
    ),

#Conditional panel for menu1
    conditionalPanel(condition = "input.menu1=='trajectory'", 
                     tags$hr(), #horizontal rule, i.e. line
                     h4("Controls"),
                     p("Select a physician to display their SOFA trajectory performance data."),
                     selectInput(
                       inputId = "doc",
                       label = "Physician",
                       choices = c("doc_01","doc_02","doc_03","doc_04","doc_05","doc_06",
                                   "doc_07","doc_08","doc_09","doc_10","doc_11","doc_12",
                                   "doc_13","doc_14","doc_15","doc_16","doc_17","doc_18",
                                   "doc_19","doc_20","doc_21","doc_22",
                                   "doc_23","doc_24","doc_25")
                     )
    ),

#Conditional panel for menu2    
    conditionalPanel(condition = "input.menu1=='ill'", 
                 tags$hr(), #horizontal rule, i.e. line
                 h4("Controls"),
                 p("Select the patients displayed to compare physician performance on patients of interest."),
                 radioButtons(
                   inputId = "ill",
                   label = "Patients displayed",
                   choices = c("Critically Ill", "Moderately Ill", "Total"),
                   selected = c("Total")
                 )
    ) 
  ),

  dashboardBody(
    tabItems( #one for each page
      
#Trajectory body
      tabItem(tabName="trajectory", #first page
        h2("SOFA Trajectory Performance Data"),
        p("The current page displays patient trajectory data for each physician."),
        fluidRow(
          tabBox(width=12,
                 id = "table1", #ID lets us use input$table1 on the server to find the current tab.
                 tabPanel("SOFA Performance Table", "The SOFA difference, calculated by taking the differences between final day SOFA score and admission SOFA score for each patient and taking the average, and the patient count for each physician.", tableOutput("tab1")),
          ),
        ),
        fluidRow(
          box(width=6, plotOutput("plot1", height=400)), #plot1
          box(width=6, plotOutput("plot2", height=400))  #plot2
        ),
        p("Sean Chapman, July 13, 2022")#it put an image there but it doesn't show up, would be fun to have an image
      ),
      
#Score body      
      tabItem(tabName="ill", #second page
        h2("Physician Performance Score"),
        p("The following bar charts correspond to critical illness treatment score."),
        p("The  first figure shows the mortality rates for each physician for patients, where critical illness is defined as having a SOFA score in the highest quartile."),
        p("Severe Illness Score was calculated in case study 1."),
        fluidRow(
          box(width=6, plotOutput("plot3", height=600)), #plot3
          box(width=6, plotOutput("plot4", height=600)) #plot4
        ),
        p("Sean Chapman, July 13, 2022")
      ),

#Information body
      tabItem(tabName = "inf", #Third page
        h2("Dashboard Information"),
        tags$br(),
        p("The Dashboard contains information regarding the performance of 25 Physicians on a dataset of 2113 ICU patients."),
        p("The relevant datasets can be found at https://ssc.ca/en/case-study/developing-a-physician-performance-model-critical-care-assessing-quality-and-value."),
        tags$br(),
        p("The purpose of this dashboard is to display some of the physician performance score information that would be interesting to a performance reviewer."),
        p("This information is organized to be able to assess one physician at a time, using both charts pertaining to the individual physician,"),
        p("as well as comparisons between that physician and their colleagues."),
        tags$br(),
        h2("Key Performance Indicators"),
        tags$br(),
        p("Key Performance Indicators include SOFA trajectories for a physician's patients and their mortality rates, including the difference in mortality rate for critically ill patients."),
        tags$br(),
        p("The SOFA trajectories trendlines for the physicians show the rate at which their patients recover on average."),
        p("Some information about the SOFA trajectory information is also included, as well as a comparison of their patient trajectories with other physicians."),
        p("This comparison places the physician's SOFA difference on a min-max scale between 0 and 1, where 1 is linked to higher performance."),
        tags$br(),
        p("The mortality rates show the count of patients surviving and dying in the physician's care."),
        p("These mortality rates can be shown for a subset of patients in the highest quartile of SOFA score (Critically Ill), or lowest 3 quartiles (Moderately Ill)"),
        p("The mortality rates of each physician can also be compared to assess how a physician is performing compared to their colleagues"),
        tags$br(),
        p("Sean Chapman, July 13, 2022")
              )
    )
  )
)


#DASHBAORD SERVER-----
server = function(input, output){
  
#Output for Tab1 
#Table for time series
  output$tab1 = renderTable({
    df = cbind(SOFA_Diff$SOFA_DIFF,count(patients, DocID)$n)
    tab = t(df)
    colnames(tab) = c("doc_01","doc_02","doc_03","doc_04","doc_05","doc_06",
                      "doc_07","doc_08","doc_09","doc_10","doc_11","doc_12",
                      "doc_13","doc_14","doc_15","doc_16","doc_17","doc_18",
                      "doc_19","doc_20","doc_21","doc_22",
                      "doc_23","doc_24","doc_25")
    rownames(tab) = c("SOFA difference","Patient Count")
    tab
  }, include.rownames=T, striped=T)
  
#Time series plot
  output$plot1 = renderPlot({
    if(input$doc == "doc_01"){ df = doc_01}
    else if(input$doc == "doc_02"){df = doc_02}
    else if(input$doc == "doc_03"){df = doc_03}
    else if(input$doc == "doc_04"){df = doc_04}
    else if(input$doc == "doc_05"){df = doc_05}
    else if(input$doc == "doc_06"){df = doc_06}
    else if(input$doc == "doc_07"){df = doc_07}
    else if(input$doc == "doc_08"){df = doc_08}
    else if(input$doc == "doc_09"){df = doc_09}
    else if(input$doc == "doc_10"){df = doc_10}
    else if(input$doc == "doc_11"){df = doc_11}
    else if(input$doc == "doc_12"){df = doc_12}
    else if(input$doc == "doc_13"){df = doc_13}
    else if(input$doc == "doc_14"){df = doc_14}
    else if(input$doc == "doc_15"){df = doc_15}
    else if(input$doc == "doc_16"){df = doc_16}
    else if(input$doc == "doc_17"){df = doc_17}
    else if(input$doc == "doc_18"){df = doc_18}
    else if(input$doc == "doc_19"){df = doc_19}
    else if(input$doc == "doc_20"){df = doc_20}
    else if(input$doc == "doc_21"){df = doc_21}
    else if(input$doc == "doc_22"){df = doc_22}
    else if(input$doc == "doc_23"){df = doc_23}
    else if(input$doc == "doc_24"){df = doc_24}
    else{df = doc_25}
    ggplot(df,aes(day,SOFA)) + geom_point() + 
      geom_jitter(aes(colour = PtID)) + theme_classic() +
      theme(text = element_text(size = 12)) +
      labs(title=paste(input$doc, "Overall Patient Trajectory"), x="day", y="SOFA")
    })
  
  output$plot2 = renderPlot({
    ggplot(doc_scores, aes(y=Health.Improvement.Score, x=Doctor_ID))+
      geom_bar(stat = "identity",fill = "light blue") +
      labs(title="SOFA Trajectory Score", x="Physician", y="Index Score") +
      theme_classic() + theme(text = element_text(size = 12)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
#Output for Tab2  
#Ill plot
  output$plot3 = renderPlot({
    if(input$ill == "Critically Ill"){
      ggplot(subset(patients, patients$P6 >= 9), aes(x=DocID, fill = factor(P8))) +
        geom_bar(stat="count", position=position_dodge(), width = 0.75) +
        labs(title="Critically Ill Mortality", x="Physician", y="Count") +
        theme_classic() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        scale_fill_manual(name = "Patient Status", values=c("steel blue", "#F37B59")) 
    }
    else if(input$ill == "Moderately Ill"){
      ggplot(subset(patients, patients$P6 < 9), aes(x=DocID, fill = factor(P8))) +
        geom_bar(stat="count", position=position_dodge(), width = 0.75) +
        labs(title="Moderately Ill Mortality", x="Physician", y="Count") +
        theme_classic() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        scale_fill_manual(name = "Patient Status", values=c("steel blue", "#F37B59")) 
        
    }
    else{
      ggplot(patients, aes(x=DocID, fill = factor(P8))) +
        geom_bar(stat="count", position=position_dodge(), width = 0.75) +
        labs(title="Overall Mortality", x="Physician", y="Count") +
        theme_classic() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        scale_fill_manual(name = "Patient Status", values=c("steel blue", "#F37B59")) 
        
    }
  })
  
#Score Plot Doctors 
  output$plot4 = renderPlot({
    ggplot(doc_scores, aes(y=Severe.Illness.Survival.Score, x=Doctor_ID))+
      geom_bar(stat = "identity",fill = "light blue") +
      labs(title="Severe Illness Score", x="Physician", y="Severe Illness Rate") +
      theme_classic() + theme(text = element_text(size = 12)) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  
#Score Plot Department
}

#Run App
shinyApp(ui,server)

