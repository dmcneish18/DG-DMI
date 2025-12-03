###packages

#needed for spinner if not installed
devtools::install_github("daattali/shinycssloaders")

library(shiny)
library(lavaan)
library(dplyr)
library(MASS)
library(shinyjs)
library(rmarkdown)
library(knitr)
library(tidyr)
library(purrr)
library(GenOrd)
library(tinytex)
library(semPlot)
library(mbend)
library(shinycssloaders)

######################## Shiny Code

## Code for Interface
ui <- fluidPage(

  shinyjs::useShinyjs(),

## Without Names under the title Application title
    titlePanel(h1(HTML("Dynamic Measurement Invariance Cutoffs: Dependent Groups"), align="center", style="background-color: #0098BA; color: white; padding-top:10px;padding-bottom:10px; margin:bottom=0px;"),
               windowTitle = "Dynamic Measurement Invariance Cutoffs"
               ),

    # sidebar for data upload and option selection
      sidebarLayout(

        sidebarPanel(

          style="height:90vh; overflow-y:auto;",
          #make labels font color white
          tags$style('label {color:white;}'),

          #make help text font color white;
          tags$style('.help-block {color:white;}'),

          #change progress bar to black
          tags$head(tags$style(".progress-bar{background-color:#000000;}")),

          #create object that is horizontal line
          tags$head(tags$style(HTML("hr {border-top: 1px solid color=white;}"))),

            #instructions heading
            h3(HTML("<b>Instructions</b>"), align="center", style="color: white; margin-top:0px;"),

            #instructions;
            helpText("1. Upload a dataset in .csv format"),
            helpText("2. The first 5 rows will appear to verify the data loaded correctly."),
            helpText("3. Select the number of groups (or timepoints for longitudinal data)"),
            helpText("4. Select the number of factors in each groups"),
            helpText("5. Select the items on each factor"),
            helpText("6. Select whether the residuals for the same items should correlate across groups"),
            helpText("7. Choose the reponse scale of the items"),
            helpText("8. Choose the estimator (some combinataions of options only have one choice)"),
            helpText("9. Choose the cutoff precision and click Submit"),
            helpText("10. Results will appear in a few minutes after calculations are complete"),
            helpText(HTML("Looking for a tutorial on how to use this app? 
                            <a href='https://osf.io/z4hyj' style='color:black;'>
                            Click Here!
                            </a>")),

            #add horizontal line to separate instructions from options
            hr(),

            #label for file upload
            h5(HTML("<b>File Upload</b>"), style="color: white; margin-top:25px; margin-bottom:10px;"),

            #box to upload data
            fileInput("upload", NULL, accept = c(".csv")),

            #reduce space between upload box and ratio buttons
            div(style = "margin-top:-15px"),

            #box to input missing data indicator
            textInput("missing", "Missing Data Indicator", "NA"),

          ##Number of Factors
          numericInput("Groups", "Select the Number of Groups (Timepoints for Longitudinal Data)",
                       min=2, max=6, value=2),
          
          ##Number of Factors
            numericInput("Factors", "Select the Number of Factors in One Group",
                        min=1, max=8, value=" "),

            #Box(es) for selecting items; Number of boxes based on number of factors
            uiOutput("FactorItems1"),
            uiOutput("FactorItems2"),
            uiOutput("FactorItems3"),
            uiOutput("FactorItems4"),
            uiOutput("FactorItems5"),
            uiOutput("FactorItems6"),
          
          helpText(HTML("<i>*Put items in the same order across groups to ensure constraints are properly applied</i>")),
          div(style = "margin-top:15px"),

          ##Residual Covariances
          checkboxInput("RC", label = "Include residual covariances for the same items in different groups", value = FALSE),

          #Response Scale
          radioButtons("Scale", label="Select Response Scale",
                       choiceNames=c("Continuous (Normal)",
                                     "Likert/Ordinal as Continuous"),
                       choiceValues=c("N","L"), selected=character(0)),

          #Invariance options
          radioButtons("Inv", label="Constraints of Interests/Invariance Type",
                       choiceNames=c("Loadings Only (Metric)",
                                     "Loadings & Intercepts (Metric & Scalar)",
                                     "Loadings, Intercepts, & Residual Variances (Metric, Scalar, & Strict)"),
                       choiceValues=c(1,2,3), selected=character(0)),
          helpText(HTML("<i>More constraints requires testing more models and more computational time</i>")),

            #Estimator; options vary depending on estimator 
            radioButtons("est", label="Estimator",
                         choiceNames=c("Maximum Likelihood",
                                       "Robust Maximum Likelihood (MLR)"),
                         choiceValues=c("ML","MLR"), selected=character(0)),

          #Precision/Reps option
          radioButtons("Reps", label="Select Level of Precision",
                       choiceNames=c(
                                     "Exploratory (100 replications)",
                                     "Full (1000 Replications, mimicking Cheung & Rensvold)",
                                     "(App Developer Test Only)"),
                       choiceValues=c(100,1000,10), selected=character(0)),
          # Helptext underneath box to select items
          helpText(HTML("<i>Higher precision takes more computational time</i>")),

          #automatically download results
            div(style="display:inline-block; width:100%; text-align: center;", checkboxInput("dl", label = "Download results automatically", value = FALSE)),

            #activation button to begin calculations
            #center "submit" within column
            div(style="display:inline-block; width:100%; text-align: center;", actionButton("go", "Submit")),

            #make background blue
            style="background-color: #0098BA;"
        ),

   #print first 5 rows of data after it is uploaded
   mainPanel(

     tags$style(HTML("
           .tabbable > .nav > li[class=active]    > a {background-color: #0098BA; color:white}")),
     
     #create panels for output
     tabsetPanel(id="Tabs", type="pill",

       #Welcome/Overview
       tabPanel( title = "Overview",

                 h4("Welcome to Dynamic Measurement Invariance Cutoffs, a tool to customize fit index difference cutoffs for measurement invariance!"),

                 p(HTML("<br/>")),

                 h4(HTML("<li>Measurement invariance is commonly assessed by comparing fit of factor models with and without invarinace constraints."),style = "margin-bottom: 30px;"),

                 h4(HTML("<li>Researchers commonly rely on guidelines from Chueng & Rensvold (2002)")),
                 h4(HTML("<ul><li>General suggestion is that &#x0394RMSEA < .01, &#x0394 CFI > -.01, or &#x0394McDonald's Non-Centrality > -.02 indicate reasonable invariance"),style = "margin-bottom: 30px;"),

                 h4(HTML("<li>However, these cutoffs were derived for independent groups like demographic characteristics or language spoken")),
                 h4(HTML("<ul><li>They do not generlize to dependent groups like matched pairs or repeated measures"), style = "margin-bottom: 30px;"),

                 h4(HTML("<li><b>This software simulates custom &#x0394RMSEA, &#x0394 CFI, &#x0394McDonald's Non-Centrality  cutoff values specifically for dependent groups</b>")),
                 h4(HTML("<ul><li>The goal is to provide cutoffs that are appropriate for groups that are related")),
                 h4(HTML("<ul><li>Cutoffs for this software will be more informative than generalizing guidelines for independent groups")),
                 h4(HTML("<ul><li>Cutoffs are optimized for your specific model and data charactersitics"), style = "margin-bottom: 30px;"),
                 p(HTML("<br/>")),
     
                 h4(HTML("<b>Looking for a tutorial on how to use this app? <a href='https://osf.io/z4hyj'> Click Here! </a></b>"))
                 ),
       
       #DMI Cutoff table
       tabPanel(title="Data Preview",
                             tableOutput("head")
                ),
       tabPanel(title = "DMI Table",
                 h4("Your Model Fit Indices:"),
                 tableOutput("Fit"),
                 h4("Your Fit Index Differences:"),
                 tableOutput("Del"),
                 tags$details(tags$summary("Purpose", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                  p(HTML("These are the fit index differences for when applying models with and without invariance constraints to your data with the
                         specified factor structure."), style="width:60%")),
                 tags$details(tags$summary("Interpretation", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),(
                  p(HTML("Indices that start with '&#x0394' tell you how much worse the model fit after adding constraints across groups. Metric Invariance constrains the loadings; Scalar
                  Invariance constrains the loadings and items means; and Strict Invariance constrains the loadings, item intercepts, and residual variances.
                  <br>
                  <br>
                  When constraints are added across groups, fit indices are expected to be a little worse by chance due to sampling variability. The practical question
                  is demarcating how much of a decrease is acceptable (and suggesting invariance) versus problematic (and suggesting non-invariance).<br>
                  <br>
                  RMSEA<sub>D</sub> does not take the difference between indices from different models. Instead, it substitutes the &#x0394 Chi-Square statistic into the 
                  RMSEA formula. <br>
                  <br>
                  Fit index difference cutoffs are one piece of evidence that can help make this decision.<br>
                  <br>

                         "),style="width:60%"))),

                  h4("Dynamic Measurement Invariance  Cutoffs for the Model and Data:"),
                  tableOutput("DFI"),
                  downloadButton("download","Download DMI Report"),
                  htmlOutput("GroupPrint"),
                  htmlOutput("GroupWarn2"),

                  #downloadButton("download","Download DMI Report"),
                  p(HTML(" "),style="margin-top:12px;"),
                  tags$details(tags$summary("Purpose", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),(
                   p(HTML("Measurement invariance assessement attempts to answer whether a construct is measured the same way in different groups.
                   <br>
                   <br>
                   <b>Metric</b> Invariance constrains loadings to be equal across groups but allows item means and residual variances to be unique.
                   <br>
                   <br>
                   <b>Scalar</b> Invariance constrains loadings and items means to be equal across groups but allows item residuals variances to be unique.
                   <br>
                   <br>
                   <b>Strict</b> Invariance constraints loadings, item means, and item residual variances to be equal across groups.
                   <br>
                   <br>
                   Fit index differences try to quantify how much worse the model fits when parameters are constrained across groups.
                   A practical question is: how much can fit decrease after applying constraints while still considering fit to be acceptable?
                   Simulations studies have tried to answer this questions for <b>independent</b> groups and provided commonly used rules of thumb like &#x0394RMSEA < .01,
                   &#x0394 CFI > -.01, or &#x0394McDonald's Non-Centrality > -.02.
                   <br>
                   <br>
                   However, these cutoffs are known to generalize poorly for models with a different number of items, factor loading magnitude, distribution of responses, and missing data.
                   The traditional cutoffs are not equally effective for different types of models, and the cutoffs were  not intended to apply to dependent groups <br>
                   <br>This software calculates customized indices that are intended to apply directly to models with dependent groups."),
                          style="width:60%"))),

                 tags$details(tags$summary("Interpretation", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),(
                 p(HTML("Compare the values in the table under 'Your Fit Index Differences' to the Dynamic Measurement Invariance Cutoffs.<br>
                 <br>
                 Low values are better for RMSEA and SRMR but high values are better for CFI and McDonald's Non-Centrality. So Interpretation is different
                 for different indices.<br>
                 <br>
                 If  &#x0394RMSEA, &#x0394SRMR, or RMSEA<sub>D</sub> are <b>smaller</b> than the cutoff, that indicates  evidence for the type of variance. For example,
                 if the cutoff is 0.015 and &#x0394RMSEA in your model is 0.012, there is evidence for invariance (invariance constraints  invariance resulted
                 in a small change in fit and suggest invariance). However, if the &#x0394RMSEA in your model is 0.019, then there is not evidence for invariance (invariance constraints
                  resulted in an unacceptably large change fit and do not suggest invariance). <br>
                 <br>
                 If the &#x0394 CFI and &#x0394McDonald's Non-Centrality are <b>larger</b> than the cutoff, that indicates  evidence for the type of variance. For example,
                 if the cutoff is -0.015 and &#x0394 CFI in your model is -0.012, there is evidence for invariance (invariance constraints  invariance resulted
                 in a small change in fit and suggest invariance). Negative signs can be tricky: smaller absolute values with negative numbers are bigger.
                 However, if the &#x0394 CFI in your model is -0.019, then there is not evidence for invariance (invariance constraints
                  resulted in an unacceptably large change fit and do not suggest invariance).<br>
                 <br>
    
                 Note that there are different cutoffs associated with different magnitudes of invariance. <i>It is possible that your model may satisfy a less restrictive form of invariance (like Metric)
                 but will not satisfy the cutoff for a more restrictive form of invariance (like Strict) </i>. The conclusions are not all-or-nothing. Typically, scalar invariance
                 is desirable because it allows direct comparison of latent variable means across groups. "),
                   style="width:60%"))),
                ),
       
       #Model estimates
       tabPanel(title = "Unstandardized Model Estimates", tabsetPanel(
         tabPanel("Metric Invariance Model",
                  tags$details(tags$summary("Loadings", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                tableOutput("Loadm2")),

                tags$details(tags$summary("Intercepts", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                             tableOutput("Intm2")),

                tags$details(tags$summary("Covariances", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                tableOutput("Corrm2")),

                tags$details(tags$summary("Variances", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                tableOutput("Varm2")),
         ),
         
         tabPanel("Scalar Invariance Model",
                  tags$details(tags$summary("Loadings", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                               tableOutput("Loadm3")),

                  tags$details(tags$summary("Intercepts", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                               tableOutput("Intm3")),

                  tags$details(tags$summary("Covariances", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                               tableOutput("Corrm3")),

                  tags$details(tags$summary("Variances", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                               tableOutput("Varm3")),
         ),

         tabPanel("Strict Invariance Model",
                  tags$details(tags$summary("Loadings", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                               tableOutput("Loadm4")),

                  tags$details(tags$summary("Intercepts", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                               tableOutput("Intm4")),

                  tags$details(tags$summary("Covariances", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                               tableOutput("Corrm4")),

                  tags$details(tags$summary("Variances", style = "display: list-item;font-size:14px; font-weight:bold; ; margin-bottom:8px; width: 60%"),
                               h5("Group 1:"),
                               tableOutput("Varm4"))
         ),
       )
       ),

     

       #Path diagram
       tabPanel(title = "Path Diagram",
                p(HTML(" "),style="margin-top:15px;"),
                tags$details(tags$summary("Interpretation", style = "display: list-item;font-size:14px; ; margin-bottom:8px; font-weight:bold;"),
                             p(HTML("This is a visual depiction of the <b>configural</b> model. Circles are factors,
                             rectangles are observed item responses, single-headed arrows are regression paths,
                             and double-headed arrows are covariances."), style="width:60%;")),
                tags$details(tags$summary("Reference", style = "display: list-item;font-size:14px; ; margin-bottom:8px; font-weight:bold;"),
                            p(HTML("Epskamp, S. (2015). semPlot: Unified visualizations of structural equation models.
                              Structural Equation Modeling, 22(3), 474-483."), style="width:60%;")),
                plotOutput("PD",width="7in", height="5in")
                ),

        #FAQS
        tabPanel(title="FAQs",
                 p(HTML(" "),style="margin-top:12px;"),
                 p(HTML("<i>Have a question that is not answered here? Submit it <a href='mailto:dmcneish@asu.edu,missgord@gmail.com?subject=DFI FAQ Suggestion'>here.</a></i>")),
                 p(HTML("<b><ol><li>Does the application save or cache data that are uploaded?</b>")),
                 tags$details(tags$summary("Answer", style = "display: list-item;"),
                 p(HTML("<br>No! The data are stored in a temporary folder that is deleted once the session is terminated."), style="width:60%")),
       
                 p(HTML("<br>")),
       
                 p(HTML("<b><li>How long does it take to compute DMI cutoffs? </b>")),
                 tags$details(tags$summary("Answer", style = "display: list-item;"),
                 p(HTML("<br>Computational times are a function of model size, sample size, number of groups, the number of invariance types being tested, the estimator, and the precision.
                 As a rough guidline, models assessing all three types of invariance with 1000 people, 3 factors, continuous outcomes, maximum likelihood, 2 groups, and full precision take about 5-10 minutes. 
                 Models with many items, ordinal indicators, and robust maximum likelihood can take considerably longer (30 min or more). <br>
                 <br>
                 If you are worried about computational time or have many models, start with the 'Exploratory' precision option.
                 This uses fewer simulation replications and provides a quicker (but less precise) idea of the suitable cutoffs. Reserve the full precision option
                 precision for final models. The exploratory precision is not sufficiently accurate for final reporting, however, and cutoffs are based on the 
                 single most extreme replication. <br>
                <br>
                 You can use the ‘Download Results Automatically’ option directly above 'Submit' to save a report of the DMI output
                 automatically when computation is complete so that you do not have to wait for the app or worry about being disconnected from the server
                 and losing your results if you walk away from the computer during  computation"), style="width:60%")),
       
                 p(HTML("<br>")),
                 
                 p(HTML("<b><li>Why is a separate method needed for assessing measurement invariance with dependent groups?</b>")),
                 tags$details(tags$summary("Answer", style = "display: list-item;"),
                              p(HTML("<br>Traditional measurement invariance cutoffs come from simulation studies exploring the 
                                     multiple-group confirmatory factor analysis framework (MG-CFA). <br>
                                     <br>
                                     A limitation of MG-CFA is that the model
                                     does not permit associations across groups. However, in dependent data like dyadic or longitudinal data,
                                     responses are correlated (e.g., the response at timepoint 2 is related to the response at timepoint 1 because 
                                     both responses came from the same person). Guidelines from MG-CFA do not generalize to situations
                                     where there are correlations between groups. <br>
                                     <br>
                                     This software implements a similar idea as previous studies on MG-CFA, but does so in a different modeling framework
                                     that allows researchers to acknowledge that responses will be related"), style="width:60%")),
                 
                 p(HTML("<br>")),
       
                 p(HTML("<b><li>Are categorical items supported?</b>")),
                 tags$details(tags$summary("Answer", style = "display: list-item;"),
                      p(HTML("<br>Not yet. DMI replicates Cheung & Rensvold (2002), which uses a method from Cudeck & Browne (1992) to simulate data with certain
                      levels of misfit between groups. Misfit in this method is defined with the maximum liklihood fit function, which is appropriate for continuous.
                      outcomes but is not used for data explicitly treated as categorical. <br>
                                        <br>
                      Extensions are possible, but additional work and code development are needed.
                      We hope to be able to provide support for categorical outcomes in the near future."), style="width:60%")),
                 
                 p(HTML("<br>")),
       
                 p(HTML("<b><li>Why is the MLR estimator not available with continuous outcomes?</b>")),
                 tags$details(tags$summary("Answer", style = "display: list-item;"),
                 p(HTML("<br>The continuous option in the DMI menu simulates normal data and the MLR and ML estimators are identical when data are truly continuous. 
                 In this situation, the MLR estimator is unnecessary because the data
                 are known to be normal and the correction term drops out, making ML and MLR equivalent.<br>
                 <br>
                 Options to simulate non-normal data are a future direction and the app will be updated as these procedures are developed."), style="width:60%")),
                 p(HTML("<br>"))
                 ),

       #references
        tabPanel(style="width:70%;",title = "References",
                 h4("This Application:"),
                 p(HTML("McNeish, D. & Wolf, M. G. (2024). Dynamic Measurement Invariance Cutoffs. R Shiny application [Software], version 0.0.0. www.dynamicfit.app.")),
                 p(HTML("<br>")),
                 h4("Idea underlying this application"),
                 p(HTML("McNeish, D. (in press). Dynamic measurement invariance cutoffs for two-group fit index differences. <i>Psychological Methods</i>.")),
                 p(HTML("McNeish, D. (under review). Dynamic measurement invariance cutoffs for longitudinal and dyadic data.")),
                 p(HTML("<br>")),
                 h4("General idea behind dynamic cutoffs:"),
                 p(HTML("McNeish, D. & Wolf, M.G. (2023). Dynamic fit index cutoffs for confirmatory factor analysis models. <i>Psychological Methods, 28</i> (1), 61-88.")),
                 p(HTML("McNeish, D. (2023). Dynamic fit index cutoffs for categorical factor analysis with Likert-type, ordinal, or binary responses. <i>American Psychologist, 79</i> (9), 1061-1075.")),
                 p(HTML("McNeish, D. (in press). Dynamic fit index cutoffs for treating Likert items as continuous. Psychological Methods.")),
                 p(HTML("Millsap, R. E. (2007). Structural equation modeling made difficult. Personality and Individual Differences, 42(5), 875-881.")),
                 p(HTML("<br>")),
                 h4("Computationally, this application relies on:"),
                 p(HTML("Barbiero, A., & Ferrari, P. A. (2017). An R package for the simulation of correlated discrete variables. Communications in Statistics-Simulation and Computation, 46(7), 5123-5140.")),
                 p(HTML("Epskamp, S. (2015). semPlot: Unified visualizations of structural equation models. Structural Equation Modeling, 22(3), 474-483.")),
                 p(HTML("Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. Journal of Statistical Software, 48(2), 1-36.")),
                 p(HTML("Schneider, W. J. (2019). simstandard: Generate Standardized Data. R package version 0.3.0.")),
                 p(HTML("<br>")),
                 h4("Aesthetically, this application relies on:"),
                 p(HTML("Chang, W., Cheng, J., Allaire, J., Xie, Y., & McPherson, J. (2020). shiny: Web Application Framework for R. R package version 1.4.0.2.")),
                 p(HTML("Attali, D. (2021). shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds. R package version 2.1.0.")),
                 p(HTML("Attali, D & Sali, A. (2023). shinycssloaders: Add loading animations to a ‘shiny’ output while it’s recalculating.")),
              )
          )
       )
     )
  )

#R code to execute in the background
server <- function(input, output,session) {

  #create object for data once it is uploaded
  data <- reactive({
        req(input$upload)

        ext <- tools::file_ext(input$upload$name)
        switch(ext,
               csv = vroom::vroom(input$upload$datapath, delim = ","),
               validate("Invalid file; Please upload a .csv file")
        )
    })
  

  csv<-reactive({
    input$upload
  })
  
   observeEvent(csv(),{updateTabsetPanel(session, inputId = "Tabs", selected = 'Data Preview')})
   
   M<-function(data=NULL, m=NULL){
     data[data == m] <- NA
     return(data)
   }
   
   data1<-reactive({
     req(input$upload)
     if(input$missing!="NA"){
       x<-M(data=data(),m=input$missing)
     }
     
     if(input$missing=="NA"){
       x<-data()
     }
     return(x)
   })


  shinyjs::disable("download")
  shinyjs::disable("go")

  # Identify which response scale was selected
  N<-reactive({req(input$Scale=="N")})
  L<-reactive({req(input$Scale=="L")})
  D<-reactive({req(input$dl==TRUE)})

  #If normal, don't allow MLR
  observeEvent(N(), {updateRadioButtons(session,"est",
                                                     choiceNames=c("Maximum Likelihood"),
                                                     choiceValues=c("ML"))
                    })

  #If Likert, no restrictions
  observeEvent(L(), {updateRadioButtons(session,"est",
                                        choiceNames=c("Maximum Likelihood",
                                                      "Robust Maximum Likelihood (MLR)"),
                                        choiceValues=c("ML", "MLR"), selected="Robust Maximum Likelihood (MLR)")
})

      #adaptively update the number of boxes based number of specified factors
    output$FactorItems1<-renderUI({
      req(input$Factors)
      lapply(1:(input$Factors), function(i) {
        selectizeInput(inputId=paste0("Factor",i,"_1"),
                    label=paste0("Select Items on Group 1, Factor ", i, ":"),
                    choices=names(data1()), multiple=TRUE)
      })
    })
    
    #adaptively update the number of boxes based number of specified factors
    output$FactorItems2<-renderUI({
      req(input$Factors)
      lapply(1:(input$Factors), function(i) {
        selectizeInput(inputId=paste0("Factor",i,"_2"),
                       label=paste0("Select Items on Group 2, Factor ", i, ":"),
                       choices=names(data1()), multiple=TRUE)
      })
    })
    
    #adaptively update the number of boxes based number of specified factors
    output$FactorItems3<-renderUI({
      req(input$Groups > 2)
      req(input$Factors)
      lapply(1:(input$Factors), function(i) {
        selectizeInput(inputId=paste0("Factor",i,"_3"),
                       label=paste0("Select Items on Group 3, Factor ", i, ":"),
                       choices=names(data1()), multiple=TRUE)
      })
    })
    
    output$FactorItems4<-renderUI({
      req(input$Groups > 3)
      req(input$Factors)
      lapply(1:(input$Factors), function(i) {
        selectizeInput(inputId=paste0("Factor",i,"_4"),
                       label=paste0("Select Items on Group 4, Factor ", i, ":"),
                       choices=names(data1()), multiple=TRUE)
      })
    })
    
    output$FactorItems5<-renderUI({
      req(input$Groups > 4)
      req(input$Factors)
      lapply(1:(input$Factors), function(i) {
        selectizeInput(inputId=paste0("Factor",i,"_5"),
                       label=paste0("Select Items on Group 5, Factor ", i, ":"),
                       choices=names(data1()), multiple=TRUE)
      })
    })
    
    output$FactorItems6<-renderUI({
      req(input$Groups > 5)
      req(input$Factors)
      lapply(1:(input$Factors), function(i) {
        selectizeInput(inputId=paste0("Factor",i,"_6"),
                       label=paste0("Select Items on Group 6, Factor ", i, ":"),
                       choices=names(data1()), multiple=TRUE)
      })
    })

  #print the first five rows of the uploaded data
    output$head <- renderTable({
      req(input$upload)
      head(data(), 5)
    })

    observeEvent(c(req(input$Factors),
                   req(eval(parse(text=paste0("input$Factor",input$Factors,"_",input$Groups)))),
                   req(input$Scale),
                   req(input$est),
                   req(input$Reps)),
                   {shinyjs::enable("go")})

###########################################

    #once the submit button is clicked (go =1), write lavaan model statements and fit models to the data using the selected options
    observeEvent(input$go,{

      #left side
      lhs<-list()
      #right side
      rhs<-list()
      #intercepts
      int<-list()
      #factor variances
      fvar<-list()
      #factor MEANS
      fint<-list()
      #combined
      line<-list()
      
      ## dummy vectors to be populated by subsequent code
      rc<-vector()
      q1<-list()
      q2<-list()
      l<-list(q1,q2)
      
      for (g in 1:input$Groups){
        l[[g]]<-
          list(eval(parse(text = (paste0("input$Factor1_",g)))),
               eval(parse(text = (paste0("input$Factor2_",g)))),
               eval(parse(text = (paste0("input$Factor3_",g)))),
               eval(parse(text = (paste0("input$Factor4_",g)))),
               eval(parse(text = (paste0("input$Factor5_",g)))),
               eval(parse(text = (paste0("input$Factor6_",g)))),
               eval(parse(text = (paste0("input$Factor7_",g)))),
               eval(parse(text = (paste0("input$Factor8_",g))))
          )
        #left side
        lhs[[g]]<-list()
        #right side
        rhs[[g]]<-list()
        #combined
        line[[g]]<-list()
        #factor variances
        fvar[[g]]<-list()
        #factor MEANS
        fint[[g]]<-list()
        #observed intercepts
        int[[g]]<-list()
      }
      

      #################################################################
      ### Write Lavaan input statements for different invariance levels
      #################################################################
      
   ############################
   # CONFIGURAL #
  ############################
      
      for (g in 1:input$Groups){
        for(m in 1:input$Factors){

          #left side is factor name
          lhs[[g]][[m]]<-paste0("g",g,"f",m,"=~")
          #begin right side with first item
          rhs[[g]][[m]]<-paste0("NA*",l[[g]][[m]][1],"+","l",m,"1*",l[[g]][[m]][1])

          #loop over number of items
          for(i in 2:(length(l[[g]][[m]]))){
            #plus sign between item names
            rhs[[g]][[m]]<-paste(rhs[[g]][[m]],"+", l[[g]][[m]][i])
          }

          #combine left and right hand side
          line[[g]][[m]]<-paste(lhs[[g]][[m]],rhs[[g]][[m]])
          int[[g]][[m]]<-paste0(l[[g]][[m]][1],"~","i",m,"*1")

          if (g==1){
            fint[[g]][[m]]<-paste0("g",g,"f",m,"~ 0*1")
            fvar[[g]][[m]]<-paste0("g",g,"f",m,"~~ 1*","g",g,"f",m)
          } else{
            fint[[g]][[m]]<-paste0("g",g,"f",m,"~ 1")
          }
        }
      }


      if(input$RC==TRUE){
        for(m in 1:input$Factors){
          for (g1 in 2:(input$Groups)){
            for (g2 in 1:(g1-1)){
              for(i in 1:(length(unlist(l[[g1]][[m]])))){
                rc<-append(rc,paste0(unlist(l[[g2]][[m]])[i],"~~", unlist(l[[g1]][[m]][i])))
              }
            }
          }
        }
      }
      
      model_config<-unlist(list(line,fvar,fint,int,rc))

   ############################
   # METRIC #
   ############################
      
  ## CLEAR LISTS ##
      
      #left side
      lhs<-list()
      #right side
      rhs<-list()
      #intercepts
      int<-list()
      #factor variances
      fvar<-list()
      #factor MEANS
      fint<-list()
      #combined
      line<-list()
      
      ## dummy vectors to be populated by subsequent code
      rc<-vector()
      q1<-list()
      q2<-list()
      l<-list(q1,q2)
      
      for (g in 1:input$Groups){
        l[[g]]<-
          list(eval(parse(text = (paste0("input$Factor1_",g)))),
               eval(parse(text = (paste0("input$Factor2_",g)))),
               eval(parse(text = (paste0("input$Factor3_",g)))),
               eval(parse(text = (paste0("input$Factor4_",g)))),
               eval(parse(text = (paste0("input$Factor5_",g)))),
               eval(parse(text = (paste0("input$Factor6_",g)))),
               eval(parse(text = (paste0("input$Factor7_",g)))),
               eval(parse(text = (paste0("input$Factor8_",g))))
          )
        #left side
        lhs[[g]]<-list()
        #right side
        rhs[[g]]<-list()
        #combined
        line[[g]]<-list()
        #factor variances
        fvar[[g]]<-list()
        #factor MEANS
        fint[[g]]<-list()
        #observed intercepts
        int[[g]]<-list()
      }
      
      for (g in 1:input$Groups){
        for(m in 1:input$Factors){

          #left side is factor name
          lhs[[g]][[m]]<-paste0("g",g,"f",m, "=~")
          #begin right side with first item
          rhs[[g]][[m]]<-paste0("NA*",l[[g]][[m]][1])

          #loop over number of items
          for(i in 1:length(l[[g]][[m]])){
            #plus sign between item names
            rhs[[g]][[m]]<-paste(rhs[[g]][[m]],"+",paste0("l",m,i,"*", l[[g]][[m]][i]))
          }

          #combine left and right hand side
          line[[g]][[m]]<-paste(lhs[[g]][[m]],rhs[[g]][[m]])
          int[[g]][[m]]<-paste0(l[[g]][[m]][1],"~","i",m,"*1")


          if (g==1){
            fint[[g]][[m]]<-paste0("g",g,"f",m,"~ 0*1")
            fvar[[g]][[m]]<-paste0("g",g,"f",m,"~~ 1*","g",g,"f",m)
          } else{
            fint[[g]][[m]]<-paste0("g",g,"f",m,"~ 1")
          }
        }
      }

      if(input$RC==TRUE){
        for(m in 1:input$Factors){
          for (g1 in 2:(input$Groups)){
            for (g2 in 1:(g1-1)){
              for(i in 1:(length(unlist(l[[g1]][[m]])))){
                rc<-append(rc,paste0(unlist(l[[g2]][[m]])[i],"~~", unlist(l[[g1]][[m]][i])))
              }
            }
          }
        }
      }

      model_metric<-unlist(list(line,fvar,fint,int,rc))
      
  ############################
  # SCALAR #
  ############################
      
      ## CLEAR LISTS ##
      
      #left side
      lhs<-list()
      #right side
      rhs<-list()
      #intercepts
      int<-list()
      #factor variances
      fvar<-list()
      #factor MEANS
      fint<-list()
      #combined
      line<-list()
      
      ## dummy vectors to be populated by subsequent code
      rc<-vector()
      q1<-list()
      q2<-list()
      l<-list(q1,q2)
      
      for (g in 1:input$Groups){
        l[[g]]<-
          list(eval(parse(text = (paste0("input$Factor1_",g)))),
               eval(parse(text = (paste0("input$Factor2_",g)))),
               eval(parse(text = (paste0("input$Factor3_",g)))),
               eval(parse(text = (paste0("input$Factor4_",g)))),
               eval(parse(text = (paste0("input$Factor5_",g)))),
               eval(parse(text = (paste0("input$Factor6_",g)))),
               eval(parse(text = (paste0("input$Factor7_",g)))),
               eval(parse(text = (paste0("input$Factor8_",g))))
          )
        #left side
        lhs[[g]]<-list()
        #right side
        rhs[[g]]<-list()
        #combined
        line[[g]]<-list()
        #factor variances
        fvar[[g]]<-list()
        #factor MEANS
        fint[[g]]<-list()
        #observed intercepts
        int[[g]]<-list()
      }
      
      for (g in 1:input$Groups){
        for(m in 1: input$Factors){

          #left side is factor name
          lhs[[g]][[m]]<-paste0("g",g,"f",m, "=~")
          #begin right side with first item
          rhs[[g]][[m]]<-paste0("NA*",l[[g]][[m]][1])

          #loop over number of items
          for(i in 1:(length(l[[g]][[m]]))){
            #plus sign between item names
            rhs[[g]][[m]]<-paste(rhs[[g]][[m]],"+",paste0("l",m,i,"*", l[[g]][[m]][i]))
          }

          for(i in 1:(length(unlist(l[[g]])))){
            int[[g]][i]<-c(paste0(unlist(l[[g]])[i],"~", "i",i, "*1"))
          }

          #combine left and right hand side
          line[[g]][[m]]<-paste(lhs[[g]][[m]],rhs[[g]][[m]])

          if (g==1){
            fint[[g]][[m]]<-paste0("g",g,"f",m,"~ 0*1")
            fvar[[g]][[m]]<-paste0("g",g,"f",m,"~~ 1*","g",g,"f",m)
          } else{
            fint[[g]][[m]]<-paste0("g",g,"f",m,"~ 1")
          }
        }
      }
      if(input$RC==TRUE){
        for(m in 1: input$Factors){
          for (g1 in 2:(input$Groups)){
            for (g2 in 1:(g1-1)){
              for(i in 1:(length(unlist(l[[g1]][[m]])))){
                rc<-append(rc,paste0(unlist(l[[g2]][[m]])[i],"~~", unlist(l[[g1]][[m]][i])))
              }
            }
          }
        }
      }

      model_scalar<-unlist(list(line,int,fvar,fint,rc))
      
  ############################
  # STRICT #
  ############################
      
      ## CLEAR LISTS ##
      
      #left side
      lhs<-list()
      #right side
      rhs<-list()
      #intercepts
      int<-list()
      #variances
      var<-list()
      #factor variances
      fvar<-list()
      #factor MEANS
      fint<-list()
      #combined
      line<-list()
      
      ## dummy vectors to be populated by subsequent code
      rc<-vector()
      q1<-list()
      q2<-list()
      l<-list(q1,q2)
      
      for (g in 1:input$Groups){
        l[[g]]<-
          list(eval(parse(text = (paste0("input$Factor1_",g)))),
               eval(parse(text = (paste0("input$Factor2_",g)))),
               eval(parse(text = (paste0("input$Factor3_",g)))),
               eval(parse(text = (paste0("input$Factor4_",g)))),
               eval(parse(text = (paste0("input$Factor5_",g)))),
               eval(parse(text = (paste0("input$Factor6_",g)))),
               eval(parse(text = (paste0("input$Factor7_",g)))),
               eval(parse(text = (paste0("input$Factor8_",g))))
          )
        #left side
        lhs[[g]]<-list()
        #right side
        rhs[[g]]<-list()
        #combined
        line[[g]]<-list()
        #factor variances
        fvar[[g]]<-list()
        #factor MEANS
        fint[[g]]<-list()
        #observed intercepts
        int[[g]]<-list()
        #observed variances
        var[[g]]<-list()
      }
      
      for (g in 1:input$Groups){
        for(m in 1: input$Factors){
          
          #left side is factor name
          lhs[[g]][[m]]<-paste0("g",g,"f",m, "=~")
          #begin right side with first item
          rhs[[g]][[m]]<-paste0("NA*",l[[g]][[m]][1])
          
          #loop over number of items
          for(i in 1:(length(l[[g]][[m]]))){
            #plus sign between item names
            rhs[[g]][[m]]<-paste(rhs[[g]][[m]],"+",paste0("l",m,i,"*", l[[g]][[m]][i]))
          }
          
          for(i in 1:(length(unlist(l[[g]])))){
            int[[g]][i]<-c(paste0(unlist(l[[g]])[i],"~", "i",i, "*1"))
            var[[g]][[i]]<-paste0(unlist(l[[g]])[i],"~~", "v", i,"*",unlist(l[[g]])[i])
            
          }
          
          #combine left and right hand side
          line[[g]][[m]]<-paste(lhs[[g]][[m]],rhs[[g]][[m]])
          
          if (g==1){
            fint[[g]][[m]]<-paste0("g",g,"f",m,"~ 0*1")
            fvar[[g]][[m]]<-paste0("g",g,"f",m,"~~ 1*","g",g,"f",m)
          } else{
            fint[[g]][[m]]<-paste0("g",g,"f",m,"~ 1")
          }
        }
      }
      if(input$RC==TRUE){
        for(m in 1: input$Factors){
          for (g1 in 2:(input$Groups)){
            for (g2 in 1:(g1-1)){
              for(i in 1:(length(unlist(l[[g1]][[m]])))){
                rc<-append(rc,paste0(unlist(l[[g2]][[m]])[i],"~~", unlist(l[[g1]][[m]][i])))
              }
            }
          }
        }
      }
      
      model_strict<-unlist(list(line,int,var,fvar,fint,rc))
  
    #Fit model in lavaan
    a<-lavaan::cfa(data=data1(), model=model_strict, estimator=input$est, missing="ml")
    aa<-lavaan::cfa(data=data1(), model=model_config, estimator=input$est, missing="ml")
    
    lav0<-lavaan::partable(a)
    
    df0<-a@test$standard$df
    df0x<-aa@test$standard$df
    
    imp0<-a@implied$cov[[1]]
    n0<-a@Data@nobs[[1]]
    
    #######################################
    ######ML vs MLR for fit measures ######
    #######################################
    if(input$est=="ML"){
    a1<-lavaan::cfa(data=data1(), model=model_config, estimator=input$est, missing="ml")
    indm1<-t(as.data.frame(lavaan::fitmeasures(a1,c("rmsea","srmr","cfi","mfi","chisq","df","pvalue"))))
    colnames(indm1)<-c("RMSEA","SRMR","CFI","McDonald Non-Centrality", "Chi-Square", "df","p-value")
    rownames(indm1)<-("Configural")
    chi0<-a@test$standard$stat
    }
    
    if(input$est=="MLR"){
      a1<-lavaan::cfa(data=data1(), model=model_config, estimator=input$est, missing="ml")
      indm1<-t(as.data.frame(lavaan::fitmeasures(a1,c("rmsea.robust","srmr","cfi.robust","mfi","chisq.scaled","df.scaled","pvalue.scaled"))))
      #lavaan doesn't have scaled version of mfi, so calculate it manually
      indm1[4]<-exp(-.5*((indm1[5]-indm1[6])/(n0-1)))
      colnames(indm1)<-c("RMSEA","SRMR","CFI","McDonald Non-Centrality", "Chi-Square", "df","p-value")
      rownames(indm1)<-("Configural")
      chi0<-a@test$yuan.bentler.mplus$stat

    }

    ##########################################################################
    ##ADD ROWS TO TABLE THAT TAKE DIFFERENCE BETWEEN CURRENT AND PREVIOUS ROW#
    ##########################################################################

    if(input$Inv>0){
      fit2<-lavaan::cfa(data=data1(), model=model_metric, estimator=input$est, missing="ml")

      q<-lavaan::parameterEstimates(fit2)

      names(q)[names(q) == 'est'] <- 'Estimate'
      names(q)[names(q) == 'se'] <- 'SE'
      names(q)[names(q) == 'z'] <- 'Z'
      names(q)[names(q) == 'pvalue'] <- 'p'
      names(q)[names(q) == 'ci.lower'] <- '95% CI Lower Limit'
      names(q)[names(q) == 'ci.upper'] <- '95% CI Upper Limit'

      qload<-q %>%
        dplyr::filter(op=="=~")
      qload<-qload[,-c(2,4)]
      names(qload)[names(qload) == 'lhs'] <- 'Factor'
      names(qload)[names(qload) == 'rhs'] <- 'Item'

      qint<-q %>%
        dplyr::filter(op=="~1")
      qint<-qint[,-c(2,3,4)]
      names(qint)[names(qint) == 'lhs'] <- 'Variable'

      qvar<-q %>%
        dplyr::filter(lhs==rhs & op=="~~")
      qvar<-qvar[,-c(1,2,4)]
      names(qvar)[names(qvar) == 'rhs'] <- 'Variable'

      qcov<-q %>%
        dplyr::filter(lhs!=rhs & op=="~~")
      qcov<-qcov[,-c(2,4)]
      names(qcov)[names(qcov) == 'lhs'] <- 'Variable 1'
      names(qcov)[names(qcov) == 'rhs'] <- 'Variable 2'

       output$Loadm2<-renderTable({qload})
       output$Intm2<-renderTable({qint})
       output$Varm2<-renderTable({qvar})
       output$Corrm2<-renderTable({qcov})
      
      if(input$est=="ML"){
        indm2<-t(as.data.frame(lavaan::fitmeasures(fit2,c("rmsea","srmr","cfi","mfi","chisq","df","pvalue"))))
        colnames(indm2)<-c("RMSEA","SRMR","CFI","McDonald Non-Centrality", "Chi-Square", "df","p-value")
        SBdif1<-lavaan::anova(a1,fit2)
        
      }
      
      if(input$est=="MLR"){
        indm2<-t(as.data.frame(lavaan::fitmeasures(fit2,c("rmsea.robust","srmr","cfi.robust","mfi","chisq.scaled","df.scaled","pvalue.scaled"))))
        #lavaan doesn't have scaled version of mfi, so calculate it manually
        indm2[4]<-exp(-.5*((indm2[5]-indm2[6])/(n0-1)))
        #calcuate satorra-bentler delta X2
        SBdif1<-lavaan::anova(a1,fit2)
      }
      
      colnames(indm2)<-c("RMSEA","SRMR","CFI","McDonald Non-Centrality", "Chi-Square", "df","p-value")
      rownames(indm2)<-("Metric")
      Ind<-rbind(indm1,indm2)

      Dif<-t(as.data.frame(indm2[,1:4]-indm1[,1:4]))
      DifD1_num<-SBdif1[2,5]-(indm2[,6]-indm1[,6])
      DifD1_num<-ifelse(DifD1_num<0,0,DifD1_num)
      DifD1<-sqrt(DifD1_num/((indm2[,6]-indm1[,6])*(n0-1)))
      
      Dif<-cbind(Dif, DifD1)
      
      del='\u0394'
      colnames(Dif)<-c(paste0(del,"RMSEA"), paste0(del,"SRMR"), paste0(del,"CFI"), paste0(del,"McDonald Non-Centrality"), "RMSEA<sub>D</sub>")
      rownames(Dif)<-c("Metric")

      Dif<-t(Dif)

      output$Fit<-renderTable(Ind,rownames=TRUE, digits=3)
      output$Del<-renderTable(Dif,rownames=TRUE, digits=3, sanitize.text.function = function(x) x)
    }

    if(input$Inv>1){
      fit3<-lavaan::cfa(data=data1(), model=model_scalar, estimator=input$est, missing="ml")

       qq<-lavaan::parameterEstimates(fit3)
       
       names(qq)[names(qq) == 'est'] <- 'Estimate'
       names(qq)[names(qq) == 'se'] <- 'SE'
       names(qq)[names(qq) == 'z'] <- 'Z'
       names(qq)[names(qq) == 'pvalue'] <- 'p'
       names(qq)[names(qq) == 'ci.lower'] <- '95% CI Lower Limit'
       names(qq)[names(qq) == 'ci.upper'] <- '95% CI Upper Limit'

       qqload<-qq %>%
         dplyr::filter(op=="=~")
       qqload<-qqload[,-c(2,4)]
       names(qqload)[names(qqload) == 'lhs'] <- 'Factor'
       names(qqload)[names(qqload) == 'rhs'] <- 'Item'
       
       qqint<-qq %>%
         dplyr::filter(op=="~1")
       qqint<-qqint[,-c(2:4)]
       names(qqint)[names(qqint) == 'lhs'] <- 'Variable'
       
       qqvar<-qq %>%
         dplyr::filter(lhs==rhs & op=="~~")
       qqvar<-qqvar[,-c(1,2,4)]
       names(qqvar)[names(qqvar) == 'rhs'] <- 'Variable'
       
       qqcov<-qq %>%
         dplyr::filter(lhs!=rhs & op=="~~")
       qqcov<-qqcov[,-c(2,4)]
       names(qqcov)[names(qqcov) == 'lhs'] <- 'Variable 1'
       names(qqcov)[names(qqcov) == 'rhs'] <- 'Variable 2'
       
       output$Loadm3<-renderTable({qqload})
       output$Intm3<-renderTable({qqint})
       output$Varm3<-renderTable({qqvar})
       output$Corrm3<-renderTable({qqcov})
      
      if(input$est=="ML"){
        indm3<-t(as.data.frame(lavaan::fitmeasures(fit3,c("rmsea","srmr","cfi","mfi","chisq","df","pvalue"))))
        colnames(indm3)<-c("RMSEA","SRMR","CFI","McDonald Non-Centrality", "Chi-Square", "df","p-value")
        SBdif2<-lavaan::anova(fit2, fit3)
      }
      
      if(input$est=="MLR"){
        indm3<-t(as.data.frame(lavaan::fitmeasures(fit3,c("rmsea.robust","srmr","cfi.robust","mfi","chisq.scaled","df.scaled","pvalue.scaled"))))
        #lavaan doesn't have scaled version of mfi, so calculate it manually
        indm3[4]<-exp(-.5*((indm3[5]-indm3[6])/(n0-1)))
        SBdif2<-lavaan::anova(fit2, fit3)
      }
      colnames(indm3)<-c("RMSEA","SRMR","CFI","McDonald Non-Centrality", "Chi-Square", "df","p-value")
      rownames(indm3)<-("Scalar")
      Ind<-rbind(indm1,indm2,indm3)

      Dif1<-t(as.data.frame(indm2[,1:4]-indm1[,1:4]))
      DifD1_num<-SBdif1[2,5]-(indm2[,6]-indm1[,6])
      DifD1_num<-ifelse(DifD1_num<0,0,DifD1_num)
      DifD1<-sqrt(DifD1_num/((indm2[,6]-indm1[,6])*(n0-1)))
      Dif1<-cbind(Dif1, DifD1)
      
      Dif2<-t(as.data.frame(indm3[,1:4]-indm2[,1:4]))
      DifD2_num<-SBdif2[2,5]-(indm3[,6]-indm2[,6])
      DifD2_num<-ifelse(DifD2_num<0,0,DifD2_num)
      DifD2<-sqrt(DifD2_num/((indm3[,6]-indm2[,6])*(n0-1)))
      Dif2<-cbind(Dif2, DifD2)
      
      Dif<-rbind(Dif1,Dif2)
      del='\u0394'
      colnames(Dif)<-c(paste0(del,"RMSEA"), paste0(del,"SRMR"), paste0(del,"CFI"), paste0(del,"McDonald Non-Centrality"), "RMSEA<sub>D</sub>")
      rownames(Dif)<-c("Metric", "Scalar")

      Dif<-t(Dif)
      output$Fit<-renderTable(Ind,rownames=TRUE, digits=3)
      output$Del<-renderTable(Dif,rownames=TRUE, digits=3,sanitize.text.function = function(x) x)
    }

    if(input$Inv>2){
      fit4<-lavaan::cfa(data=data1(), model=model_strict, estimator=input$est, missing="ml")

       qqq<-lavaan::parameterEstimates(fit4)
       
       names(qqq)[names(qqq) == 'est'] <- 'Estimate'
       names(qqq)[names(qqq) == 'se'] <- 'SE'
       names(qqq)[names(qqq) == 'z'] <- 'Z'
       names(qqq)[names(qqq) == 'pvalue'] <- 'p'
       names(qqq)[names(qqq) == 'ci.lower'] <- '95% CI Lower Limit'
       names(qqq)[names(qqq) == 'ci.upper'] <- '95% CI Upper Limit'

       qqqload<-qqq %>%
         dplyr::filter(op=="=~")
       qqqload<-qqqload[,-c(2,4)]
       names(qqqload)[names(qqqload) == 'lhs'] <- 'Factor'
       names(qqqload)[names(qqqload) == 'rhs'] <- 'Item'
       
       qqqint<-qqq %>%
         dplyr::filter(op=="~1")
       qqqint<-qqqint[,-c(2:4)]
       names(qqqint)[names(qqqint) == 'lhs'] <- 'Variable'
       
       qqqvar<-qqq %>%
         dplyr::filter(lhs==rhs & op=="~~")
       qqqvar<-qqqvar[,-c(1,2,4)]
       names(qqqvar)[names(qqqvar) == 'rhs'] <- 'Variable'
       
       qqqcov<-qqq %>%
         dplyr::filter(lhs!=rhs & op=="~~")
       qqqcov<-qqqcov[,-c(2,4)]
       names(qqqcov)[names(qqqcov) == 'lhs'] <- 'Variable 1'
       names(qqqcov)[names(qqqcov) == 'rhs'] <- 'Variable 2'
       
       output$Loadm4<-renderTable({qqqload})
       output$Intm4<-renderTable({qqqint})
       output$Varm4<-renderTable({qqqvar})
       output$Corrm4<-renderTable({qqqcov})
       

      if(input$est=="ML"){
        indm4<-t(as.data.frame(lavaan::fitmeasures(fit4,c("rmsea","srmr","cfi","mfi","chisq","df","pvalue"))))
        colnames(indm2)<-c("RMSEA","SRMR","CFI","McDonald Non-Centrality", "Chi-Square", "df","p-value")
        SBdif3<-lavaan::anova(fit3, fit4)
        
      }
      
      if(input$est=="MLR"){
        indm4<-t(as.data.frame(lavaan::fitmeasures(fit4,c("rmsea.robust","srmr","cfi.robust","mfi","chisq.scaled","df.scaled","pvalue.scaled"))))
        #lavaan doesn't have scaled version of mfi, so calculate it manually
        indm4[4]<-exp(-.5*((indm4[5]-indm4[6])/(n0-1)))
        SBdif3<-lavaan::anova(fit3, fit4)
        
      }
      
      colnames(indm4)<-c("RMSEA","SRMR","CFI","McDonald Non-Centrality", "Chi-Square", "df","p-value")
      rownames(indm4)<-("Strict")
      Ind<-rbind(indm1,indm2,indm3,indm4)

      Dif1<-t(as.data.frame(indm2[,1:4]-indm1[,1:4]))
      DifD1_num<-SBdif1[2,5]-(indm2[,6]-indm1[,6])
      DifD1_num<-ifelse(DifD1_num<0,0,DifD1_num)
      DifD1<-sqrt(DifD1_num/((indm2[,6]-indm1[,6])*(n0-1)))
      Dif1<-cbind(Dif1, DifD1)
      
      Dif2<-t(as.data.frame(indm3[,1:4]-indm2[,1:4]))
      DifD2_num<-SBdif2[2,5]-(indm3[,6]-indm2[,6])
      DifD2_num<-ifelse(DifD2_num<0,0,DifD2_num)
      DifD2<-sqrt(DifD2_num/((indm3[,6]-indm2[,6])*(n0-1)))
      Dif2<-cbind(Dif2, DifD2)
      
      Dif3<-t(as.data.frame(indm4[,1:4]-indm3[,1:4]))
      DifD3_num<-SBdif3[2,5]-(indm4[,6]-indm3[,6])
      DifD3_num<-ifelse(DifD3_num<0,0,DifD3_num)
      DifD3<-sqrt(DifD3_num/((indm4[,6]-indm3[,6])*(n0-1)))
      Dif3<-cbind(Dif3, DifD3)

      Dif<-rbind(Dif1,Dif2,Dif3)
      del='\u0394'
      colnames(Dif)<-c(paste0(del,"RMSEA"), paste0(del,"SRMR"), paste0(del,"CFI"), paste0(del,"McDonald Non-Centrality"),"RMSEA<sub>D</sub>")
      rownames(Dif)<-c("Metric", "Scalar","Strict")

      Dif<-t(Dif)
      output$Fit<-renderTable(Ind,rownames=TRUE, digits=3)
      output$Del<-renderTable(Dif,rownames=TRUE, digits=3,sanitize.text.function = function(x) x)

      }

    #show spinner while calculating
    shinycssloaders::showPageSpinner(caption = "Simulating cutoffs, please wait")


    sig.star<-function(lav=NULL, dat=NULL)
    {

      ss_mod_load <- suppressMessages(lav %>%
                                        dplyr::filter(lhs != rhs) %>%
                                        dplyr::group_by(lhs,op) %>%
                                        dplyr::filter(op != "~1") %>%
                                        dplyr::filter(op != "|") %>%
                                        dplyr::filter(op != "==") %>%
                                        dplyr::select(lhs,op,rhs,est) %>%
                                        #dplyr::mutate(est=round(est,digits=8)) %>%
                                        dplyr::reframe(rhs=paste(est,"*",rhs,collapse=" + ")) %>%
                                        dplyr::arrange(desc(op)) %>%
                                        tidyr::unite("mod",lhs,op,rhs,sep="") %>%
                                        dplyr::pull(mod))

      ss_mod_res <- suppressMessages(lav %>%
                                       dplyr::filter(lhs == rhs) %>%
                                       dplyr::select(lhs,op,rhs,est) %>%
                                       #dplyr::mutate(est=round(est,digits=8)) %>%
                                       dplyr::reframe(rhs=paste(lhs,"~~",est,"*",rhs)) %>%
                                       dplyr::pull())

      #Collapse into one string because my other functions expect that
      mod <- base::paste(c(ss_mod_load,ss_mod_res), sep="", collapse="\n")

      #gdat<-dplyr::filter(dat,dat[,noquote(paste0(input$Group))]==grp)
      b<-lavaan::cfa(data=dat, model=mod)

      return(b@implied$cov[[1]])
    }

    sig.star0<-sig.star(lav0,dat=data1())


    ##helper functions for simulation

  duplication.matrix <- function (n = 1)
    {
      if ((n < 1) | (round(n) != n))
        stop("n must be a positive integer")
      d <- matrix(0, n * n, n * (n + 1)/2)
      count = 0
      for (j in 1:n) {
        d[(j - 1) * n + j, count + j] = 1
        if (j < n) {
          for (i in (j + 1):n) {
            d[(j - 1) * n + i, count + i] <- 1
            d[(i - 1) * n + j, count + i] <- 1
          }
        }
        count = count + n - j
      }
      return(d)
    }

    vech<-function (x)
    {
      if (!is.square.matrix(x))
        stop("argument x is not a square numeric matrix")
      return(t(t(x[!upper.tri(x)])))
    }

    is.square.matrix<-function (x)
    {
      if (!is.matrix(x))
        stop("argument x is not a matrix")
      return(nrow(x) == ncol(x))
    }

    matrix.trace<-function (x)
    {
      if (!is.square.matrix(x))
        stop("argument x is not a square matrix")
      return(sum(diag(x)))
    }

    #lav = parameter table, n = sample size in group G, imp= implied covariance matrix in Group G

    datgencov<-function(lav=NULL, n=NULL, imp=NULL, dat=NULL, df=NULL) {

      #preliminary definitions
      Sigma.gamma0<-imp #model-implied from fitted model
      p<-nrow(Sigma.gamma0) #number of manifest variables
      q<-lav_partable_npar(lav) #number of model parameters
      p.star <- p*(p+1)/2 #number of non-duplicated entries

      discrep<-chi0/n #desired disscrepancy is df / N
      delta<-discrep

      D.mat <- duplication.matrix(p)
      D <- t(D.mat)%*%D.mat
      Sigma.gamma0<-sig.star(lav,dat)
      W <- Sigma.gamma0
      W.inv <- solve(W)

      #matrix manipulation functions

      #setup tracking matrices
      h<- 1e-8
      Sigma.deriv<- array(NA, c(p,p,q))
      B <- matrix(NA, p.star, q)

      #loop through parameter table and perturb each estimate
      for (i in 1:q){
        lav1<-lav
        lav1[i,14]<- lav1[i,14]+h #should this be ordered or does it not matter because the parameters are fixed anyway?
        #[order(-lav$free),]
        Sigma.gamma <-sig.star(lav1,dat)
        Sigma.deriv[,,i] <- 	(Sigma.gamma-Sigma.gamma0)*(1/h)
        B[,i]<- (-1)*D%*%vech(W.inv%*%Sigma.deriv[,,i]%*%W.inv)
      }

      #randomly draw errors
      set.seed(101492)
      y <- matrix(rnorm(p.star), p.star,1)
      B.qr<- qr(B)
      e.tilt <- qr.resid(B.qr, y)

      E1 <- matrix(0, p,p)
      index<-1
      for (i2 in 1:p){
        for(i1 in i2:p){
          E1[i1, i2]<- e.tilt[index,1]
          index<-index+1
        }
      }

      E2 <- matrix(0, p,p)
      index <- 1
      for (i1 in 1:p){
        for (i2 in i1:p){
          E2[i1, i2]<- e.tilt[index, 1]
          index <- index+1
        }
      }

      E.tilt <- E1+E2-diag(diag(E1))

      #rescale errors to have magnitude that will give desired result
      G <- W.inv %*% E.tilt
      get.kappa <- function(kappa, G, I, delta){
        target<-abs(kappa*matrix.trace(G) - log(det(I+kappa*G))-delta)
        return(target)
      }

      kappa0 <- sqrt(2*delta/matrix.trace(G%*%G))
      I <- diag(p)
      res.kappa<- suppressWarnings(nlm(get.kappa, kappa0, G=G, I=I, delta=delta))
      kappa <- res.kappa$estimate
      iter<- res.kappa$iterations

      kappa<-as.numeric(kappa)
      E <- kappa*E.tilt

      #return matrix with perturbations that make chi-square = df
      Sigma.star <-Sigma.gamma0+E

      return(Sigma.star)

    }
    
     if(input$Scale=="N"){
        imp=imp0
     }
    
      if(input$Scale=="L"){
      varname<-c(unlist(a@Data@ov.names[[1]]))
      p<-list()
      for (i in 1:length(varname)){
      xx<-as.data.frame(table(data1()[varname[1]]))%>% mutate(cum_sum = cumsum(Freq))
            xx1<-xx %>% mutate(cum_prop=cum_sum/cum_sum[nrow(xx)])
            xx2<-t(xx1[,4])
            xx3<-xx2[,1:ncol(xx2)-1]
            xx4<-c(xx3)
            p[[i]]<-xx4
      }
      sd<-diag(imp0)
      R<-cov2cor(imp0)
      RR<-GenOrd::ordcont(marginal=p,Sigma=R)
      Rd<-RR$SigmaC
      imp<-cor2cov(Rd,sqrt(sd))
    }

    
  Sigma.star0<-datgencov(lav=lav0, n=n0, imp=imp, dat=data1(), df=df0)
    
    ###borrowed from 'matrixcalc' pacakge
    is.positive.definite <- function( x, tol=1e-5 )
    {
      eigenvalues <- eigen(x, only.values = TRUE)$values
      n <- nrow( x )
      for ( i in 1: n ) {
        if ( abs( eigenvalues[i] ) < tol ) {
          eigenvalues[i] <- 0
        }
      }    
      if ( any( eigenvalues <= 0 ) ) {
        return( FALSE )
      }
      return( TRUE )
    }
    
    if (is.positive.definite(round(Sigma.star0,5))){
      Sigma.star0<-round(Sigma.star0,5)
    } else {
      Sigma.star0<-mbend::bend(round(Sigma.star0,5))$bent
    }
    ###########################
    ############ simulate data 
    ###########################

    true_fit_MI <- function(model,reps, n0, Scale=input$Scale){

      #Number of reps
      r<-reps

      #Set Seed
      set.seed(8675309)

      if (Scale=="N"){
      datax<-as.data.frame(MASS::mvrnorm(n=n0*r, mu=rep(0,nrow(Sigma.star0)), Sigma.star0))
      colnames(datax)<-c(unlist(a@Data@ov.names[[1]]))
      }

       if (Scale=="L"){
         SigR<-cov2cor(Sigma.star0)
         datax<-as.data.frame(GenOrd::ordsample(n=n0*r,Sigma=SigR, marginal = p))
         colnames(datax)<-c(unlist(a@Data@ov.names[[1]]))
       }

      rep <- base::rep(1:r,n0)
      data_true<- base::cbind(datax,rep)

      #Group and list
      true_data <- data_true %>%
        dplyr::group_by(rep) %>%
        tidyr::nest() %>%
        base::as.list()

      if(input$est=="ML"){
      ind<-c("srmr", "rmsea", "cfi", "mfi","chisq","df")
      }

      if(input$est=="MLR"){
        ind<-c("srmr", "rmsea.robust", "cfi.robust", "mfi","chisq.scaled","df.scaled")
      }

      #Run 500 cfa

        ##configural Model
      m1 <- purrr::map(true_data$data,function(x) lavaan::cfa(model=model_config,estimator=input$est,
                                                              data=x,
                                                              check.gradient=FALSE,
                                                              check.post=FALSE,
                                                              check.vcov=FALSE,
                                                              control=list(rel.tol=.001)))
      m1_fit <- purrr::map_dfr(m1,~lavaan::fitMeasures(., ind)) %>%
        #`colnames<-`(c("SRMR_M1","RMSEA_M1","CFI_M1", "McD_M1","Chisq_M1"))
        `colnames<-`(c("SRMR_M1","RMSEA_M1","CFI_M1", "McD_M1","Chisq_M1","df_M1"))

      #metric models
      m2 <- purrr::map(true_data$data,function(x) lavaan::cfa(model=model_metric,estimator=input$est,
                                                              data=x,
                                                              check.gradient=FALSE,
                                                              check.post=FALSE,
                                                              check.vcov=FALSE,
                                                              control=list(rel.tol=.001)))
      m2_fit <- purrr::map_dfr(m2,~lavaan::fitMeasures(., ind)) %>%
        `colnames<-`(c("SRMR_M2","RMSEA_M2","CFI_M2", "McD_M2","Chisq_M2","df_M2"))
      dif1<-purrr::pmap_dfr(list(m1,m2),lavaan::anova)%>%dplyr::select("Chisq diff")%>%na.omit()%>%`colnames<-`(c("x2dif_M2"))
      m2_fit<-cbind(m2_fit,dif1)

      if(input$Inv >1){

      #scalar models
      m3 <- purrr::map(true_data$data,function(x) lavaan::cfa(model=model_scalar,estimator=input$est,
                                                              data=x,
                                                              check.gradient=FALSE,
                                                              check.post=FALSE,
                                                              check.vcov=FALSE,
                                                              control=list(rel.tol=.001)))
      m3_fit <- purrr::map_dfr(m3,~lavaan::fitMeasures(., ind)) %>%
        `colnames<-`(c("SRMR_M3","RMSEA_M3","CFI_M3", "McD_M3","Chisq_M3","df_M3"))
      dif2<-purrr::pmap_dfr(list(m2,m3),lavaan::anova)%>%dplyr::select("Chisq diff")%>%na.omit()%>%`colnames<-`(c("x2dif_M3"))
      m3_fit<-cbind(m3_fit,dif2)
      }

      if(input$Inv ==3) {

      #Strict models
      m4 <- purrr::map(true_data$data,function(x) lavaan::cfa(model=model_strict, estimator=input$est,
                                                              data=x,
                                                              check.gradient=FALSE,
                                                              check.post=FALSE,
                                                              check.vcov=FALSE,
                                                              control=list(rel.tol=.001)))
      m4_fit <- purrr::map_dfr(m4,~lavaan::fitMeasures(., ind)) %>%
        `colnames<-`(c("SRMR_M4","RMSEA_M4","CFI_M4", "McD_M4","Chisq_M4","df_M4"))
      dif3<-purrr::pmap_dfr(list(m3,m4),lavaan::anova)%>%dplyr::select("Chisq diff")%>%na.omit()%>%`colnames<-`(c("x2dif_M4"))
      m4_fit<-cbind(m4_fit,dif3)
      }

      if(input$Inv==1){

        m<-cbind(m1_fit, m2_fit) %>%
          mutate(n0=n0)%>%

          mutate(RMSEA_metric=RMSEA_M2-RMSEA_M1)%>%
          mutate(SRMR_metric=SRMR_M2-SRMR_M1)%>%
          mutate(CFI_metric=CFI_M2-CFI_M1)%>%
          mutate(McD_metric=McD_M2-McD_M1)%>%
          mutate(RMSEAD_metric_num=x2dif_M2-(df_M2-df_M1))%>%
          mutate(RMSEAD_metric_num=ifelse(RMSEAD_metric_num<0,0,RMSEAD_metric_num))%>%
          mutate(RMSEAD_metric=sqrt(RMSEAD_metric_num/((df_M2-df_M1)*(n0-1))))

        res<-apply(m[,c(15:18,20)],2,quantile, probs=c(.01,.99))

        #signs are different, so save group by lower-is-better and higher-is-better
        pos<-res[2,c(1:2,5)]
        neg<-res[1,3:4]
        all_res<-as.data.frame(c(pos,neg))

        #one-column per fit index
        metric<-all_res[c(1,2,4,5,3),]

        #table with more intuitive labels
        table<-cbind(metric)
        colnames(table)<-c("Metric")
        del='\u0394'
        rownames(table)<-c(paste0(del,"RMSEA"), paste0(del,"SRMR"), paste0(del,"CFI"), paste0(del,"McDonald Non-Centrality"),"RMSEA<sub>D</sub>")
      }

      if(input$Inv==2){
        ##might have to change comparison
        ##Should it be 2 vs. 4 and not 3 vs 4?
        m<-cbind(m1_fit, m2_fit, m3_fit) %>%
          mutate(no=n0)%>%

          mutate(RMSEA_metric=RMSEA_M2-RMSEA_M1)%>%
          mutate(RMSEA_Scalar=RMSEA_M3-RMSEA_M2)%>%

          mutate(SRMR_metric=SRMR_M2-SRMR_M1)%>%
          mutate(SRMR_Scalar=SRMR_M3-SRMR_M2)%>%

          mutate(CFI_metric=CFI_M2-CFI_M1)%>%
          mutate(CFI_Scalar=CFI_M3-CFI_M2)%>%

          mutate(McD_metric=McD_M2-McD_M1)%>%
          mutate(McD_Scalar=McD_M3-McD_M2)%>%

          mutate(RMSEAD_metric_num=x2dif_M2-(df_M2-df_M1))%>%
          mutate(RMSEAD_Scalar_num=x2dif_M3-(df_M3-df_M2))%>%

          mutate(RMSEAD_metric_num=ifelse(RMSEAD_metric_num<0,0,RMSEAD_metric_num))%>%
          mutate(RMSEAD_Scalar_num=ifelse(RMSEAD_Scalar_num<0,0,RMSEAD_Scalar_num))%>%

          mutate(RMSEAD_metric=sqrt(RMSEAD_metric_num/((df_M2-df_M1)*(n0-1))))%>%
          mutate(RMSEAD_Scalar=sqrt(RMSEAD_Scalar_num/((df_M3-df_M2)*(n0-1))))

        res<-apply(m[,c(22:29,32:33)],2,quantile, probs=c(.01,.99))

        #signs are different, so save group by lower-is-better and higher-is-better
        pos<-res[2,c(1:4,9:10)]
        neg<-res[1,5:8]
        all_res<-as.data.frame(c(pos,neg))

        #one-column per fit index
        metric<-all_res[c(1,3,7,9,5),]
        scalar<-all_res[c(2,4,8,10,6),]

        #table with more intuitive labels
        table<-cbind(metric, scalar)
        colnames(table)<-c("Metric", "Scalar")
        del='\u0394'
        rownames(table)<-c(paste0(del,"RMSEA"), paste0(del,"SRMR"), paste0(del,"CFI"), paste0(del,"McDonald Non-Centrality"), "RMSEA<sub>D</sub>")
      }

      if(input$Inv==3){
      ##might have to change comparison
      ##Should it be 2 vs. 4 and not 3 vs 4?
      m<-cbind(m1_fit, m2_fit, m3_fit, m4_fit) %>%
        mutate(n0=n0)%>%

        mutate(RMSEA_metric=RMSEA_M2-RMSEA_M1)%>%
        mutate(RMSEA_Scalar=RMSEA_M3-RMSEA_M2)%>%
        mutate(RMSEA_Strict=RMSEA_M4-RMSEA_M3)%>%

        mutate(SRMR_metric=SRMR_M2-SRMR_M1)%>%
        mutate(SRMR_Scalar=SRMR_M3-SRMR_M2)%>%
        mutate(SRMR_Strict=SRMR_M4-SRMR_M3)%>%

        mutate(CFI_metric=CFI_M2-CFI_M1)%>%
        mutate(CFI_Scalar=CFI_M3-CFI_M2)%>%
        mutate(CFI_Strict=CFI_M4-CFI_M3)%>%

        mutate(McD_metric=McD_M2-McD_M1)%>%
        mutate(McD_Scalar=McD_M3-McD_M2)%>%
        mutate(McD_Strict=McD_M4-McD_M3)%>%

        mutate(RMSEAD_metric_num=x2dif_M2-(df_M2-df_M1))%>%
        mutate(RMSEAD_Scalar_num=x2dif_M3-(df_M3-df_M2))%>%
        mutate(RMSEAD_Strict_num=x2dif_M4-(df_M4-df_M3))%>%

        mutate(RMSEAD_metric_num=ifelse(RMSEAD_metric_num<0,0,RMSEAD_metric_num))%>%
        mutate(RMSEAD_Scalar_num=ifelse(RMSEAD_Scalar_num<0,0,RMSEAD_Scalar_num))%>%
        mutate(RMSEAD_Strict_num=ifelse(RMSEAD_Strict_num<0,0,RMSEAD_Strict_num))%>%

        mutate(RMSEAD_metric=sqrt(RMSEAD_metric_num/((df_M2-df_M1)*(n0-1))))%>%
        mutate(RMSEAD_Scalar=sqrt(RMSEAD_Scalar_num/((df_M3-df_M2)*(n0-1))))%>%
        mutate(RMSEAD_Strict=sqrt(RMSEAD_Strict_num/((df_M4-df_M3)*(n0-1))))

          res<-apply(m[,c(29:40,44:46)],2,quantile, probs=c(.01,.99),na.rm=T)

          #signs are different, so save group by lower-is-better and higher-is-better
          pos<-res[2,c(1:6,13:15)]
          neg<-res[1,7:12]
          all_res<-as.data.frame(c(pos,neg))

          #one-column per fit index
          metric<-all_res[c(1,4,10,13,7),]
          scalar<-all_res[c(2,5,11,14,8),]
          strict<-all_res[c(3,6,12,15,9),]

      #table with more intuitive labels
      table<-cbind(metric, scalar, strict)
      colnames(table)<-c("Metric", "Scalar", "Strict")
      del='\u0394'
      rownames(table)<-c(paste0(del,"RMSEA"), paste0(del,"SRMR"), paste0(del,"CFI"), paste0(del,"McDonald Non-Centrality"),"RMSEA<sub>D</sub>")
      }

      #set.seed(NULL)

      return(round(table,4))

    }

    Results<-true_fit_MI(model=model,reps=as.numeric(input$Reps), n0=n0)

    #hide spinner when calculations are complete
    shinycssloaders::hidePageSpinner()

    updateTabsetPanel(session, inputId = "Tabs", selected = 'DMI Table')

    #output$DFI<-renderTable(Sigma.star0)
    output$DFI<-renderTable(Results, rownames=TRUE, digits=3, sanitize.text.function = function(x) x)
    output$Dec<-renderTable(Decision, rownames=TRUE)
    
    
    PD<-semPlot::semPaths(a,residuals=FALSE, intercepts=FALSE, thresholds = FALSE)
    output$PD<-renderPlot(semPlot::semPaths(a,residuals=FALSE, intercepts=FALSE, thresholds = FALSE))
    
    if(input$Inv==1){
      MI<-"Metric"
    }
    
    if(input$Inv==2){
      MI<-"Metric and Scalar"
    }
    
    if(input$Inv==3){
      MI<-"Metric, Scalar, and Strict"
    }
    
    shinyjs::enable("download")
    #  #download results
    output$download <- downloadHandler(
      filename = function() {
       # create default file name
       paste0("DMICutoffs - ",Sys.Date(), ".html")
     },
     content = function(f) {
       # Render the document using template saved in same folder
       rmarkdown::render("template-DGMI1.Rmd",
                         output_format = rmarkdown::html_document(),
                         output_file=f,
                         params=list(
                           data = input$upload,
                           name = input$upload$name,
                           Scale = input$Scale,
                           Reps = input$Reps,
                           Groups = input$Groups,
                           Factors = input$Factors,
                           missing= input$missing,
                           MI =MI,
                           PD = PD,
                           fit = Ind,
                           Del = Dif,
                           cutoffs=Results
                         ))
     })

    if(D()==TRUE) {
      shinyjs::delay(100,shinyjs::click("download"))
    }


})
}

    

###########################################

# Run the application
shinyApp(ui = ui, server = server)

