library(shiny)    # shiny
library(shinythemes)

library(ggplot2)   # plotting
library(ggpubr)
library(ggrepel)
library(plotly)

library(dplyr)   # filter, select and other functions
library(readxl)  # reading excels files
library(stringr)
library(writexl)

library(FME)    # non linear model
library(nlme)   # non linear model
library(DT)     # table display

library(lme4)

ui <- navbarPage("Navigation Bar",
                 tabPanel("Introduction",
                    
                          titlePanel("Easy rumen degradation profile "), 
                 verticalLayout(div("Here you find a easy way to get the degradation profile on a plot, 
                                    extract the coefficients from the model and apply the correction for soluble particles, 
                                    lost particle and lag time where appropriate. 
                                    In the following two folders (Method CP and Method NDF), you find a description
                                    of the models and formulas that are used for the analysis. 
                                    Is recommended to understand well the method before proceding with the analysis."),
                                br(),
                                h4("Example data"),
                                div("Here you can download an example file for CP and NDF degradation profile."),
                                br(),
                                downloadButton("demoCP", "Download example data CP"),
                                br(),
                                downloadButton("demoNDF", "Download example data NDF"),
                                br(),br(),
                                img(src = "appfront.png", width = "100%")
                                )),
                 tabPanel("Method CP",
                     titlePanel("Rumen degradation profile CP"),  
                     verticalLayout(
                     h3("The main model and coefficients calculation"),   
                     div("The formula/model used to analyzed the data:"),
                     withMathJax(helpText("$$ Amount \\: of \\: CP \\:degraded \\:at \\:time \\: \\textit{t}, \\:
                                          \\% = A + B \\times (1 - exp^{-C \\times t} ) $$")),
                     div(HTML("The amount of CP degraded at time <em>t</em> are the values obtained from the lab analysis, 
                               the time <em>t</em> is the incubation time in hours and
                               the coefficient A, B and C will be calculated by the model, where:")),
                     HTML("<ul>
                                    <li> A: Rumen soluble fraction, assumed to be degraded immediately.  </li>
                                    <li> B: Insoluble fraction, potentially degradable in the rumen.  </li>
                                    <li> C: Fractional rate of degradation of fraction B. </li>
                                    </ul>"),
                    
                     h3("Correction of the coefficients"),   
                     div("The calculated coefficient have to be corrected on the assumption that a part of the CP disappear 
                          because soluble and another fraction escaped from the bag"),
                     HTML("<ul>
                                    <li> The fraction of soluble particles (SP) should be market at time (<em>t</em>) -1, 
                                         and this correspond the fraction of CP the was soluble in the filter paper. </li>
                                    <li> The fraction of escaped particles (EP) should be market at time (<em>t</em>) 0, 
                                         and this correspond the fraction of CP that was lost was washing the bag under running water, 
                                          without incubation. </li>
                       </ul>"),
                    
                     div("The fraction of lost particle (LP) has to be corrected from soluble particle, 
                          and it is calculate as the diffence between escaped particle and soluble particles:"),
                     withMathJax(helpText("$$ LP = EP - SP $$")),
                     
                     div("Finally, coefficient can be corrected as follow and effective protein degradation (EPD) is calculated."), 
                     div("The coefficient A, corrected for lost particles:"),
                     withMathJax(helpText("$$  A_{cor} = A - LP $$")),
                     div("The coefficient B, corrected for soluble and lost particles:"),
                     withMathJax(helpText("$$ B_{cor} = B + LP \\times \\frac{B}{100 - (LP + SP)} $$")),
                     div("The EPD as percentage (\\%) is calculated as:"),
                     withMathJax(helpText("$$ EPD = A_{cor} + B_{cor} \\times \\frac{C}{C + Kp} $$")),
                     div("Where Kp is the passage rate, assumed at 0.05 for CP."),
                     h3("Analysis"),   
                     div("Now you can procede to the Analysis CP folder and analyze your data, in brief:"),
                     HTML("<ul>
                                    <li> Upload an excel (.xlsx) file in the top right panel. </li>
                                    <li> Define your columns (Time, Degradation and Feedstuffs) in the top left panel. </li>
                                    <li> You can have a look at the graph and the results and speculate about that. </li>
                                    </ul>")
                     )),
                 
                 tabPanel("Method NDF",
                          titlePanel("Rumen degradation profile NDF"),  
                          verticalLayout(
                            h3("The main model and coefficients calculation:"),   
                            div("The model is slightly difference for NDF"),
                            HTML("<ul>
                                    <li> There is no fraction A (there is no soluble NDF!). </li>
                                    <li> The model can include a lag time. </li>
                                    </ul>"),
                            div("The formula/model used to analyzed the data without lag time (L):"),
                            withMathJax(helpText("$$ Amount \\: of \\: NDF \\:degraded \\:at \\:time \\: \\textit{t}, \\:
                                                 \\% = B \\times (1 - exp^{-C \\times t} ) $$")),
                            div("The formula/model used to analyzed the data with lag time (L):"),
                            withMathJax(helpText("$$ Amount \\: of \\: NDF \\:degraded \\:at \\:time \\: \\textit{t}, \\:
                                                   \\% = B \\times (1 - exp^{-C \\times (t - L)} ) $$")),
                            div(HTML("The amount of NDF degraded at time <em>t</em> are the values obtained from the lab analysis, 
                                      the time <em>t</em> is the incubation time in hours and
                                      the coefficient B and C (and in case L) will be calculated by the model, where:")),
                            HTML("<ul>
                                    <li> B: Insoluble fraction, potentially degradable in the rumen.  </li>
                                    <li> C: Fractional rate of degradation of fraction B. </li>
                                    <li> L: lag time </li>
                                    </ul>"),
                            
                            h3("Correction of the coefficients:"),   
                            div("As stated before, there is no soluble particle from NDF, and the escaped particles are
                                already accounted by the NDF lab analysis, therefore there is not need to correct the coefficients."),
                            div("The effective degradation NDF as percentage (\\%) 
                                is calculated with two different formulas, if model does not include lagtime:"),
                            withMathJax(helpText("$$ ED = B \\times \\frac{C}{C + Kp} $$")),
                            div("If the model includes lag time:"),
                            withMathJax(helpText("$$ ED = B \\times \\frac{C}{C + Kp} \\times exp^{-Kp \\times L} $$")),
                            div("Where Kp is the passage rate, assumed at 0.02 for NDF."),
                            
                            h3("Analysis"),   
                            div("Now you can procede to the Analysis NDF and analyze your data, in brief:"),
                            HTML("<ul>
                                    <li> Upload an excel (.xlsx) file in the top right panel. </li>
                                    <li> Define your columns (Time, Degradation and Feedstuffs) in the top left panel. </li>
                                    <li> You can have a look at the graph and the results and speculate about that. </li>
                                    </ul>")
                          )),
                 
                
                 
                 tabPanel("Analysis CP",
                          sidebarLayout(
                              sidebarPanel(div("Select variables", style = "text-align:center"),
                                           div("Is important to select all three variables for time, 
                                               degradation and grouping (feed) factor."),
                                           br(),br(),
                                           uiOutput("t"),
                                           uiOutput("d"),
                                           uiOutput("g"),
                                           radioButtons("plot1button","Do you want to split the plot?",
                                                        choices = c("No", "Yes"),
                                                        selected = "No"),
                                           uiOutput("f")
                              ),

                              mainPanel(
                                  inputPanel(
                                      div("Load the data set", style = "text-align:center"), 
                                      br(),br(),br(),
                                      fileInput('file1', 'Choose xlsx file',
                                                accept = c(".xlsx"))),
                                  conditionalPanel(
                                    condition = "input.plot1button == 'No'", plotlyOutput("CPplot1")),
                                  conditionalPanel(
                                    condition = "input.plot1button == 'Yes'", plotlyOutput("CPplot2"))
                              )
                          ),

                          sidebarLayout(
                              sidebarPanel(div(HTML("Starting coefficients A, B and C. 
                                               This is required from the R code to work, 
                                                    <b>there is no need to change these</b>.")),
                                           br(),
                                           uiOutput("A"),
                                           uiOutput("B"),
                                           uiOutput("C")
                                           ),
                              
                              mainPanel(
                                  tableOutput("finaltable"),
                                  downloadButton('download',"Download final table (excel)"))
                              )),
                              
                              
                 tabPanel("Analysis NDF",
                          sidebarLayout(
                            sidebarPanel(div("Select variables", style = "text-align:center"),
                                         div("Is important to select all three variables for time, 
                                             degradation and grouping (feed) factor."),
                                         br(),br(),
                                         uiOutput("tNDF"),
                                         uiOutput("dNDF"),
                                         uiOutput("gNDF"),
                                         radioButtons("plot2button","Do you want to split the plot?",
                                                      choices = c("No", "Yes"),
                                                      selected = "No"),
                                         uiOutput("fNDF")
                            ),
                            
                            mainPanel(
                              inputPanel(
                                div("Load the data set", style = "text-align:center"), 
                                br(),br(),br(),
                                fileInput('file2', 'Choose xlsx file',
                                          accept = c(".xlsx"))),
                              conditionalPanel(
                                condition = "input.plot2button == 'No' && input.lagtime == 'No'", plotlyOutput("NDFplot1.1")),
                              conditionalPanel(
                                condition = "input.plot2button == 'No' && input.lagtime == 'Yes'", plotlyOutput("NDFplot1.2")),
                              conditionalPanel(
                                condition = "input.plot2button == 'Yes' && input.lagtime == 'No'", plotlyOutput("NDFplot2.1")),
                              conditionalPanel(
                                condition = "input.plot2button == 'Yes' && input.lagtime == 'Yes'", plotlyOutput("NDFplot2.2"))
                            )
                          ),
                          
                          sidebarLayout(
                            sidebarPanel(div(HTML("Starting coefficients B, C and lag time.
                                                   This is required from the R code to work, 
                                                    <b>there is no need to change these</b>.")),
                                         br(),
                                         uiOutput("BNDF"),
                                         uiOutput("CNDF"),
                                         uiOutput("LAG")                            ),
                            
                            mainPanel(
                              radioButtons("lagtime","Do you want use lag time in the model?",
                                           choices = c("No", "Yes"),
                                           selected = "No"),
                              conditionalPanel(
                                condition = "input.lagtime == 'No'", tableOutput("finaltableNDF1")),
                              conditionalPanel(
                                condition = "input.lagtime == 'Yes'", tableOutput("finaltableNDF2")),
                              downloadButton('downloadNDF',"Download final table (excel)"))
                          )),
		 
		  tabPanel("References",
			   sidebarLayout(
                              sidebarPanel(h3("References"),
				   mainPanel(includeHTML("rumendegradationbib.html"))))
)






server <- function(input, output, session) {
    
  # example data CP
  output$demoCP <- downloadHandler(
    filename = function(){
      paste("Dummy_data_CP","xlsx",sep=".")
    },
    content = function(con){
      file.copy("demoCP.xlsx", con)
    })
  
  # example data NDF

  output$demoNDF <- downloadHandler(
    filename = function(){
      paste("Dummy_data_NDF","xlsx",sep=".")
    },
    content = function(con){
      file.copy("demoNDF.xlsx", con)
    })
  
  
    # data
    dataset <- reactive({
        req(input$file1)
        inFile <- input$file1
        read_xlsx(inFile$datapath, sheet = 1)
    })
   
    
    
    # data visualization CP
    
    output$t <- renderUI(varSelectInput("t", label = "Select time variable:", dataset()))
    output$d <- renderUI(varSelectInput("d", label = "Select degradation (%) variable:", dataset()))
    output$g <- renderUI(varSelectInput("g", label = "Select grouping variable:", dataset()))
    output$f <- renderUI(varSelectInput("f", label = "Select faceting variable:", dataset()))
    
    
    # graph CP 1
    
    output$CPplot1 <- renderPlotly({
      req(input$file1)
      
      ggplotly(
        ggplot(dataset() %>% filter((!!rlang::sym(input$t)) >= 0), 
              aes_string(x = input$t, y = input$d, color = as.character(input$g))) + 
          geom_point(size = 3) + 
          geom_smooth(method="nls", 
                      formula=y~A1+B1*(1-exp(-C1*x)), 
                      method.args = list(start = c(A1=input$A,B1=input$B,C1=input$C)), 
                      se = FALSE) +
          labs(x = paste(input$t, ", h", sep = ""), y = paste(input$d, ", degraded %", sep = "")) +
          theme_light()
      )
    })
    # graph CP 2
    
    output$CPplot2 <- renderPlotly({
      req(input$file1)
      
      ggplotly(
        ggplot(dataset() %>% filter((!!rlang::sym(input$t)) >= 0), 
               aes_string(x = input$t, y = input$d, color = as.character(input$g))) + 
          geom_point(size = 3) + 
          geom_smooth(method="nls", 
                      formula=y~A1+B1*(1-exp(-C1*x)), 
                      method.args = list(start = c(A1=input$A,B1=input$B,C1=input$C)), 
                      se = FALSE) +
          facet_wrap(paste("~", input$f, sep = " ")) +
          labs(x = paste(input$t, ", h", sep = ""), y = paste(input$d, ", degraded %", sep = "")) +
          theme_light()
      )
    })
    
    
    output$A <- renderUI(numericInput("A", label = "Insert A:", value = 40))
    output$B <- renderUI(numericInput("B", label = "Insert B:", value = 30))
    output$C <- renderUI(numericInput("C", label = "Insert C:", value = 0.1, step = 0.1))
    
    form <- reactive({ 
      as.character(paste(input$d, "~", "A", "+", "B", "*(1-exp(-", "C", "*", input$t, "))", sep=""))
    })
    
    output$form <- renderText({ form() })

    
    mod2 <- reactive({ 
        nlsList(
            as.formula(paste(form(), input$g, sep= "|")),
            #input$d ~ profile(input$t, input$A, input$B, input$C) | input$g,
            pool=FALSE,
            data = as.data.frame(dataset() %>% filter((!!rlang::sym(input$t)) >= 0) %>% na.omit()),
            start = c(A=input$A, B=input$B, C=input$C)) 
        })
    
    feeds <- reactive({ sort(pull(unique(dataset()[,as.character(input$g)]))) })

    Acoef <- reactive({ coef(mod2())[1] })        # put the estimates for A into the variable A
    Bcoef <- reactive({ coef(mod2())[2] })        # put the estimates for A into the variable A
    Ccoef <- reactive({ coef(mod2())[3] })        # put the estimates for A into the variable A
            
    filterpaper <- reactive({ dataset() %>% dplyr::filter((!!rlang::sym(input$t)) < 0 ) %>% 
                                  group_by(!!rlang::sym(input$g)) %>%
                                  summarize(filterpapercol = mean(!!rlang::sym(input$d), na.rm = TRUE)) %>%
                                  dplyr::select(filterpapercol) %>% pull()  })
    
    particleloss <- reactive({ Particleloss <- dataset() %>% filter((!!rlang::sym(input$t)) == 0) %>%
                                                             group_by(!!rlang::sym(input$g)) %>%
                                                             summarize(particlelosscol = mean(!!rlang::sym(input$d), na.rm = TRUE))
                               Particleloss <- Particleloss %>% dplyr::select(particlelosscol) %>% pull()
                               Particleloss <- Particleloss - filterpaper()
                               Particleloss })
    
    A_cor <- reactive({ pull(Acoef()) - particleloss() })
    B_cor <- reactive({ pull(Bcoef()) + particleloss() * (pull(Bcoef()) / (100 - (particleloss() + filterpaper()))) })
    EPD <- reactive({ A_cor() + B_cor() * (pull(Ccoef()) / ( pull(Ccoef()) + 0.05) ) })

    finaltable <- reactive({tab <- data.frame(feeds(), pull(Acoef()), pull(Bcoef()), pull(Ccoef()), filterpaper(), particleloss(), A_cor(), B_cor(), EPD())
    colnames(tab) <- c(input$g, "A", "B", "C", "Filterpaper", "Particleloss", "A_cor", "B_cor", "EPD_CP, %") 
    tab})
    
    output$finaltable <- renderTable({tab <- data.frame(feeds(), pull(Acoef()), pull(Bcoef()), pull(Ccoef()), filterpaper(), particleloss(), A_cor(), B_cor(), EPD())
                                      colnames(tab) <- c(input$g, "A", "B", "C", "Filterpaper", "Particleloss", "A_cor", "B_cor", "EPD_CP, %") 
                                      tab })
    

    
    output$download <- downloadHandler(
        filename = function(){"Rumen_degradationprofile_CP.xlsx"}, 
        content = function(fname){
            write_xlsx(finaltable(), fname) })
    
    
    
    datasetNDF <- reactive({
      req(input$file2)
      inFileNDF <- input$file2
      read_xlsx(inFileNDF$datapath, sheet = 1)
    })
    
    
    # data visualization
    
    output$tNDF <- renderUI(varSelectInput("tNDF", label = "Select time variable:", datasetNDF()))
    output$dNDF <- renderUI(varSelectInput("dNDF", label = "Select degradation (%) variable:", datasetNDF()))
    output$gNDF <- renderUI(varSelectInput("gNDF", label = "Select grouping variable:", datasetNDF()))
    output$fNDF <- renderUI(varSelectInput("fNDF", label = "Select faceting variable:", datasetNDF()))
    
    # graph NDF no facet no lag
    
    output$NDFplot1.1 <- renderPlotly({
      req(input$file2)
      
      ggplotly(
        ggplot(datasetNDF() %>% filter((!!rlang::sym(input$tNDF)) >= 0), 
               aes_string(x = input$tNDF, y = input$dNDF, color = as.character(input$gNDF))) + 
          geom_point(size = 3) + 
          geom_smooth(method="nls", 
                      formula=y~B2*(1-exp(-C2*x)), 
                      method.args = list(start = c(B2=input$BNDF,C2=input$CNDF)), 
                      se = FALSE) +
          labs(x = paste(input$tNDF, ", h", sep = ""), y = paste(input$dNDF, ", degraded %", sep = "")) +
          theme_light()
      )
    })
    
    # graph NDF no facet yes lag
    
    output$NDFplot1.2 <- renderPlotly({
      req(input$file2)
      
      ggplotly(
        ggplot(datasetNDF() %>% filter((!!rlang::sym(input$tNDF)) >= 0), 
               aes_string(x = input$tNDF, y = input$dNDF, color = as.character(input$gNDF))) + 
          geom_point(size = 3) + 
          geom_smooth(method="nls", 
                      formula=y~B2*(1-exp(-C2*(x-L2))), 
                      method.args = list(start = c(B2=input$BNDF,C2=input$CNDF,L2=input$LAG)), 
                      se = FALSE) +
          labs(x = paste(input$tNDF, ", h", sep = ""), y = paste(input$dNDF, ", degraded %", sep = "")) +
          theme_light()
      )
    })
    
    
    # graph NDF facet no lag
    
    output$NDFplot2.1 <- renderPlotly({
      req(input$file2)
      
      ggplotly(
        ggplot(datasetNDF() %>% filter((!!rlang::sym(input$tNDF)) >= 0), 
               aes_string(x = input$tNDF, y = input$dNDF, color = as.character(input$gNDF))) + 
          geom_point(size = 3) + 
          geom_smooth(method="nls", 
                      formula=y~B2*(1-exp(-C2*x)), 
                      method.args = list(start = c(B2=input$BNDF,C2=input$CNDF)), 
                      se = FALSE) +
          facet_wrap(paste("~", input$fNDF, sep = " ")) +
          labs(x = paste(input$tNDF, ", h", sep = ""), y = paste(input$dNDF, ", degraded %", sep = "")) +
          theme_light()
      )
    })
    
    # graph NDF facet yes lag
    
    output$NDFplot2.2 <- renderPlotly({
      req(input$file2)
      
      ggplotly(
        ggplot(datasetNDF() %>% filter((!!rlang::sym(input$tNDF)) >= 0), 
               aes_string(x = input$tNDF, y = input$dNDF, color = as.character(input$gNDF))) + 
          geom_point(size = 3) + 
          geom_smooth(method="nls", 
                      formula=y~B2*(1-exp(-C2*(x-L2))), 
                      method.args = list(start = c(B2=input$BNDF,C2=input$CNDF, L2=input$LAG)), 
                      se = FALSE) +
          facet_wrap(paste("~", input$fNDF, sep = " ")) +
          labs(x = paste(input$tNDF, ", h", sep = ""), y = paste(input$dNDF, ", degraded %", sep = "")) +
          theme_light()
      )
    })
    
    output$BNDF <- renderUI(numericInput("BNDF", label = "Insert B:", value = 30))
    output$CNDF <- renderUI(numericInput("CNDF", label = "Insert C:", value = 0.1, step = 0.1))
    output$LAG <- renderUI(numericInput("LAG", label = "Insert lag time:", value = 1))
    
    formNDF1 <- reactive({ 
      as.character(paste(input$dNDF, "~", "BNDF", "*(1-exp(-", "CNDF", "*", input$tNDF,"))", sep=""))
    })
    
    formNDF2 <- reactive({ 
      as.character(paste(input$dNDF, "~", "BNDF", "*(1-exp(-", "CNDF", "*(", input$tNDF,"-LAG)))", sep=""))
    })
    
    output$formNDF1 <- renderText({ formNDF1() })
    output$formNDF2 <- renderText({ formNDF2() })
    
    mod2NDF1 <- reactive({ 
      nlsList(
        as.formula(paste(formNDF1(), input$gNDF, sep= "|")),
        #input$d ~ profile(input$t, input$A, input$B, input$C) | input$g,
        pool=FALSE,
        data = as.data.frame(datasetNDF() %>% filter((!!rlang::sym(input$tNDF)) >= 0) %>% na.omit()),
        start = c(BNDF=input$BNDF, CNDF=input$CNDF)) 
      
    })
    
    
    
    mod2NDF2 <- reactive({ 
      nlsList(
        as.formula(paste(formNDF2(), input$gNDF, sep= "|")),
        #input$d ~ profile(input$t, input$A, input$B, input$C) | input$g,
        pool=FALSE,
        data = as.data.frame(datasetNDF() %>% filter((!!rlang::sym(input$tNDF)) >= 0) %>% na.omit()),
        start = c(BNDF=input$BNDF, CNDF=input$CNDF, LAG = input$LAG)) 
      
    })
    
    output$tableNDF1 <- renderTable({ coef(mod2NDF1)})
    output$tableNDF2 <- renderTable({ coef(mod2NDF2)})
    
    feedsNDF <- reactive({ sort(pull(unique(datasetNDF()[,as.character(input$gNDF)]))) })
    
    BcoefNDF1 <- reactive({ coef(mod2NDF1())[1] })        # put the estimates for A into the variable A
    CcoefNDF1 <- reactive({ coef(mod2NDF1())[2] })        # put the estimates for A into the variable A
    
    BcoefNDF2 <- reactive({ coef(mod2NDF2())[1] })        # put the estimates for A into the variable A
    CcoefNDF2 <- reactive({ coef(mod2NDF2())[2] })        # put the estimates for A into the variable A
    LAG <- reactive({ coef(mod2NDF2())[3] })        # put the estimates for A into the variable A
    
    
    EPDNDF1 <- reactive({ BcoefNDF1() * (pull(CcoefNDF1()) / ( pull(CcoefNDF1()) + 0.02) ) })
    EPDNDF2 <- reactive({ BcoefNDF2() * (pull(CcoefNDF2()) / ( pull(CcoefNDF2()) + 0.02)) * exp(-0.02*pull(LAG())) })
    
    finaltableNDF1 <- reactive({ tab1 <- data.frame(feedsNDF(), pull(BcoefNDF1()), pull(CcoefNDF1()), EPDNDF1())
                                              colnames(tab1) <- c(input$gNDF, "B", "C", "EPD_NDF, %") 
                                              tab1  })
    
    finaltableNDF2 <- reactive({ tab2 <- data.frame(feedsNDF(), pull(BcoefNDF2()), pull(CcoefNDF2()), pull(LAG()),  EPDNDF2())
    colnames(tab2) <- c(input$gNDF, "B", "LAG", "C", "EPD_NDF, %") 
    tab2  })
    
    output$finaltableNDF1 <- renderTable({ tab1 <- data.frame(feedsNDF(), pull(BcoefNDF1()), pull(CcoefNDF1()), EPDNDF1())
    colnames(tab1) <- c(input$gNDF, "B", "C", "EPD_NDF, %") 
    tab1 })
    
    output$finaltableNDF2 <- renderTable({ tab2 <- data.frame(feedsNDF(), pull(BcoefNDF2()), pull(CcoefNDF2()), pull(LAG()), EPDNDF2())
    colnames(tab2) <- c(input$gNDF, "B", "C", "LAG", "EPD_NDF, %") 
    tab2 })
    
    finaltableNDF3 <- reactive({t1 <- finaltableNDF1()
                                t1$LAG <- 0 
                                t1$lagtime <- "no" 
                                t2 <- finaltableNDF2()
                                t2$lagtime <- "yes" 
                                t3 <- rbind(t1, t2)
                                t3}) 
    
    output$downloadNDF <- downloadHandler(
      filename = function(){"Rumen_degradationprofile_NDF.xlsx"}, 
      content = function(fname){
        write_xlsx(finaltableNDF3(), fname) })
}

shinyApp(ui = ui, server = server)
