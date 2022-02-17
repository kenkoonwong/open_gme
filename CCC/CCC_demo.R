## load libraries
library(tidyverse)
library(plotly)
library(DT)
library(gridExtra)
library(lubridate)
library(shiny)
library(shinydashboard)
library(workflows)
library(ranger)
library(recipes)


load("cccdb_fake.rda")
comp_variable <- c("PC1","PC2","PC3","PC4","PC5","PC6","MK1","MK2","MK3","SBP1","SBP2","SBP3","PBLI1","PBLI2","PROF1","PROF2","PROF3","PROF4","ICS1","ICS2","ICS3")



# header ------------------------------------------------------------------



## header
header <- dashboardHeader(title = "CCC dashboard")


# sidebar -----------------------------------------------------------------


## sidebar
sidebar <- dashboardSidebar(
  tags$style(HTML(".sidebar-menu li { font-size: 20px; }")),
  selectInput("lname", "Name:", ""),
  passwordInput("password", "Password:"),
  sidebarMenu(
    menuItem(text = "All Rotations (mean)", tabName = "all_rotations"),
    menuItem(text = "Evaluations (details)", tabName = "eval_details"),
    menuItem(text = "Longitudinal Clinic", tabName = "longitudinal"),
    menuItem(text = "Peer", tabName = "peer"),
    menuItem(text = "ITE", tabName = "ite"),
    menuItem(text = "Milestones Trend", tabName = "mtrend"),
    dateRangeInput('dateRange',
                   label = 'Date range: mm/dd/yyyy',
                   start = "2021-07-01", end = Sys.Date(),
                   format = "mm/dd/yyyy")
))

# body --------------------------------------------------------------------


## body
body <- dashboardBody(
  fluidRow(
    column(width = 12,
           fluidRow(
             ## Name of resident
             column(width = 6,
                    box(textOutput(outputId = "res_name"))),
             column(width = 6,
                    box(textOutput("evalnum")))
                    
  ))),
  tabItems(
    tabItem("all_rotations",
            fluidRow(
              column(width = 12,
                     fluidRow(
                       column(width = 2,
                              DT::dataTableOutput("overall")),
                       column(width = 5,
                              plotOutput("radar1", width = "100%", height = "800px"), height = "1000px"),
                       column(width = 5,
                              plotOutput("radar2", width = "100%", height = "800px"), height = "1000px")
                       )
                     )
              )
            ),
    tabItem("eval_details",
            fluidRow(
              column(width = 12,
                     fluidRow(
                       column(width = 6,
                              plotlyOutput("eval_detail_plotly", width = "100%", height = "800px"), height = "1000px"),
                       column(width = 6,
                              DT::dataTableOutput("eval_detail_comm"))
                 )
                 )
              )
            ),
    tabItem("longitudinal",
            fluidRow(
              column(width = 12,
                     fluidRow(
                       column(width = 6,
                              plotlyOutput("longitudinal_plotly", width = "100%", height = "800px"), height = "1000px"),
                       column(width = 6,
                              DT::dataTableOutput("longitudinal_comm"))
                     ))
            )),
    tabItem("peer",
            fluidRow(
              column(width = 12,
                     fluidRow(
                       tabBox(height = "100px",
                                     tabPanel("Senior",
                              plotOutput("peer_senior_chart", width = "100%", height = "800px"), height = "1000px"),
                              tabPanel("intern",
                                       plotOutput("peer_intern_chart", width = "100%", height = "800px"), height = "1000px")),
                       column(width = 6,
                              DT::dataTableOutput("peer_comm"))
                     )))
            ),
    tabItem("ite", # ite ----
            fluidRow(
              column(width = 12,
                     fluidRow(
                       tabBox(height = "100px",
                              tabPanel("ITE",
                                       plotlyOutput("ite", width = "100%", height = "800px"),DT::dataTableOutput("ite_predict"), textOutput("ite_ml_disclaimer"), height = "1000px"), # * ite_ml_disclaimer ----
                              tabPanel("ITE Percent (whole)",
                                       plotlyOutput("ite_percent_whole", width = "100%", height = "800px"), height = "1000px"),
                              tabPanel("ITE Percentile (whole)",
                                       plotlyOutput("ite_percentile_whole", width = "100%", height = "800px"), height = "1000px")))))),
       tabItem("mtrend",
            fluidRow(
              column(12, #milestones trend ----
                     plotlyOutput("mtrend1", width = "100%", height = "1000px"))
            ))))
            
  


# ui ----------------------------------------------------------------------



## ui
ui <- dashboardPage(header, sidebar, body)


# server ------------------------------------------------------------------

server <- function(input, output, session) {

  
  # creating cccdb function to include dangeRange filter ----
  cccdbDate <- function() {
    if (input$password == "") { # insert your password inside " " <------
    cccdb_no_ondemand <- cccdb %>%
      filter(lubridate::mdy(`Session Date`) >= input$dateRange[1] & lubridate::mdy(`Session Date`) <= input$dateRange[2])  
    
    # so that the name list gets updated
    namelist <- cccdb$Resident %>% unique()
    
    updateSelectInput(session = session, inputId = "lname", label = "Name:", choices = namelist, selected = namelist[1])
    
  return(cccdb)
    } else { }}
  
  res <- function() {
    name <- input$lname
    return(name)
  }
  

  ## Name of resident
  output$res_name <- renderText({
    resname2 <- cccdbDate() %>%
      filter(str_detect(Resident, res())) %>%
      select(5)
    
    unlist(unique(resname2))
    
  })
  
  ## Number of eval
  output$evalnum <- renderText({
    num_eval <- cccdbDate() %>%
      filter(str_detect(Resident, res())) %>%
      filter(`Evaluator Status` != "Peer")
    

    paste("Total number of Faculty evaluation =", nrow(num_eval))
  })
  
  ## table for overall
  output$overall <- DT::renderDataTable({
    
  db_s <- cccdbDate() %>%
    filter(str_detect(Resident,res())) %>%
    select(comp_variable) %>%
    pivot_longer(cols = comp_variable,names_to = "subcomp", values_to = "value") %>%
    group_by(subcomp) %>%
    summarize(mean = mean(value, na.rm=TRUE)) %>%
    mutate(subcomp = fct_relevel(subcomp, comp_variable)) %>%
    arrange(subcomp)
  
  datatable(db_s, rownames = FALSE, options = list(pageLength = 23, dom = "t"))
 
  })
  
   ## radar chart 1
  output$radar1 <- renderPlot({
    db_radar <- cccdbDate() %>%
      filter(str_detect(Resident,res())) %>%
      select(comp_variable) %>%
      pivot_longer(cols = comp_variable,names_to = "subcomp", values_to = "value") %>%
      group_by(subcomp) %>%
      summarize(mean = mean(value, na.rm=TRUE)) %>%
      mutate(subcomp = fct_relevel(subcomp, comp_variable)) %>%
      arrange(subcomp)
    
    radar_chart <- db_radar %>%
      ggplot(., aes(x=subcomp, y=mean)) +
      geom_col(alpha = 0.5) +
      coord_polar() +
      ylim(0,5) +
      theme_bw() +
      theme(legend.position = "none", axis.text.x = element_text(size = 15)) 
    
    radar_chart
  })
  
  
  # radar chart 2 (violin plot)
  output$radar2 <- renderPlot({
  db_radar_box <- cccdbDate() %>%
    filter(str_detect(Resident,res())) %>%
    select(comp_variable) %>%
    gather(subcomp, value) %>%
    group_by(subcomp) 
  
  db_radar_box$subcomp <- factor(db_radar_box$subcomp, levels = comp_variable)
  
  radar_chart_box <- db_radar_box %>%
    ggplot(., aes(x=subcomp, y=value)) +
    geom_violin(aes(fill = subcomp), alpha = 0.4) + 
    geom_point(position = position_jitter(width = 0.2, height = 0.01)) +
    coord_polar() +
    ylim(0,5) +
    theme_bw() +
    theme(legend.position = "none", axis.text.x = element_text(size = 15))
  
  radar_chart_box
  
})
  
  #Eval detail boxplot ----
  output$eval_detail_plotly <- renderPlotly({
    eval_db <- cccdbDate() %>%
      filter(str_detect(Resident, res())) %>%
      filter(`Evaluator Status` != "Peer" & !str_detect(`Subject Rotation`,"imca")) %>%
      select(comp_variable, `Session Date`,`Evaluator Name`, `Subject Rotation`, Comments) %>%
      gather(subcomp,value,-`Session Date`,-`Evaluator Name`,-`Subject Rotation`,-Comments)
    
    eval_db$subcomp <- factor(eval_db$subcomp, levels = comp_variable)
    

    eval_boxplot <- eval_db %>%
      ggplot(.,
             aes(x=subcomp, y=value, fill = `Subject Rotation`, text = `Evaluator Name`)) +
      geom_boxplot(alpha = 0.5) +
      geom_point(position = position_jitter(width = 0.2, height = 0.01), alpha = 0.6) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      ylim(0,5) +
      facet_wrap(. ~ `Subject Rotation`) +
      theme(legend.position='none')
    
    if (nrow(eval_db)==0) {} else {ggplotly(eval_boxplot)}
  })
  
  #Eval detail comments
  output$eval_detail_comm <- DT::renderDataTable({
    eval_db_comment <- cccdbDate() %>%
      filter(str_detect(Resident, res())) %>%
      filter(`Evaluator Status` != "Peer" & !str_detect(`Subject Rotation`,"imca")) %>% 
      select(`Session Date`,`Evaluator Name`, `Subject Rotation`, Comments) %>%
      drop_na(Comments) %>%
      mutate(`Session Date` = mdy(`Session Date`)) %>%
      arrange(desc(`Session Date`))
    
    datatable(eval_db_comment, options = list(pageLength = 20,
                                              search = list(regex = TRUE, caseInsensitive = FALSE)))
  })
  
  #longitudinal boxplot
  output$longitudinal_plotly <- renderPlotly({
    ambdb_value <- cccdbDate() %>%
      filter(str_detect(Resident,res())) %>%
      filter(str_detect(`Subject Rotation`,"imca")) %>%
      select(comp_variable, Comments, `Evaluator Name`, `Subject Rotation`) %>%
      gather(subcomp,value,-Comments, -`Evaluator Name`, -`Subject Rotation`)
    
    ambdb_value$subcomp <- factor(ambdb_value$subcomp, levels = comp_variable)
    
    ambdb_boxplot <- ambdb_value %>%
      ggplot(.,
             aes(x=subcomp, y=value, name = `Evaluator Name`)) +
      geom_boxplot(fill = "yellow", alpha = 0.5) +
      # geom_point(alpha = 0.6) +
      geom_point(position = position_jitter(width = 0.2, height = 0.01), alpha = 0.6) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      facet_wrap(. ~ `Subject Rotation`) +
      ylim(0,5)
    
    if (nrow(ambdb_value)==0) {} else {ggplotly(ambdb_boxplot)}
  })
  
  #longitudinal comm
  output$longitudinal_comm <- DT::renderDataTable({
    ambdb_comment <- cccdbDate() %>%
      filter(str_detect(Resident,res())) %>%
      filter(str_detect(`Subject Rotation`,"imca")) %>%
      select(`Session Date`,`Evaluator Name`, Comments) %>%
      dplyr::distinct(`Session Date`, `Evaluator Name`, Comments)
    
    datatable(ambdb_comment, 
              options = list(search = list(regex = TRUE, caseInsensitive = FALSE)))
  })
  
  #peer senior chart
  output$peer_senior_chart <- renderPlot({
    peer_senior_q <- c("PS1 Safet Env", "PS2 Leadership", "PS3 Patient safety","PS3 Teamwork", "PS4 Feedback", "PS5 Professionalism/Comm", "PS6 Teaching")
    
    
    
    peer <- cccdbDate() %>%
      filter(str_detect(Resident, res())) %>%
      filter(`Evaluator Status` == "Peer") %>%
      select(Resident, peer_senior_q, `PS7 Rec`) 
    

    peer[peer_senior_q] <- lapply(peer[peer_senior_q], factor, levels = c(1:5))
    peer$`PS7 Rec` <- as.factor(peer$`PS7 Rec`)
    
    peer_p <- function(x,y) {
      x1 <- enquo(x)
      peer_px <- peer  %>%
        ggplot(., 
               aes(x=!! x1, fill=!! x1)) +
        geom_bar(col="black") +
        theme_bw() +
        # scale_y_continuous(limits = c(0,10)) +
        scale_x_discrete(limits = factor(1:5), labels=c("1" = "Never", "2" = "Rarely", "3" = "Sometimes", "4" = "Often", "5" = "Role model")) +
        scale_fill_manual(values = c("1"="red","2"="red","3"="#03a1fc","4"="#03a1fc","5"="#03a1fc")) +
        labs(title = y) +
        theme_bw() +
        theme(title = element_text(size = 12,face="bold"),axis.text.x = element_text(size = 10),axis.title.x=element_blank(), legend.position = "none")
      return(peer_px)
    }
    
    
    peer_p1 <- peer_p(`PS1 Safet Env`,"Creates a SAFE learning environment")
    peer_p2 <- peer_p(`PS2 Leadership`, "Supervision/Management/Leadership skill")
    peer_p2a <- peer_p(x = `PS3 Patient safety`, "Patient Safety Skill") 
    peer_p3 <- peer_p(`PS3 Teamwork`, "Teamwork skill")
    peer_p4 <- peer_p(`PS4 Feedback`, "Gives and receives feedback effectively")
    peer_p5 <- peer_p(`PS5 Professionalism/Comm`, "Professionalism and Communication skill")
    peer_p6 <- peer_p(`PS6 Teaching`, "Teaching skill")
 
    peer_p7 <- peer  %>%
      ggplot(.,
             aes(x=`PS7 Rec`, fill =`PS7 Rec`)) +
      geom_bar(col="black") +
      # scale_y_continuous(limits = c(0,10)) +
      labs(title = "Recommendation as a team leader/educator") +
      scale_x_discrete(limits = c("No", "Yes")) +
      scale_fill_manual(limits = c("No", "Yes"), values = c("red", "#03a1fc")) +
      theme_bw() +
      theme(title = element_text(size = 12,face="bold"),axis.text.x = element_text(size = 10),axis.title.x=element_blank(),legend.position = "none")
    
    if(nrow(peer)==0) { print("no data") } else {
      p <- grid.arrange(arrangeGrob(peer_p1, peer_p2, peer_p2a, peer_p3, peer_p4, peer_p5, peer_p6, peer_p7, ncol = 2))
    }
    
    print(p)
    
    
  })
  
  #peer intern chart
  
  output$peer_intern_chart <- renderPlot({
    peer_intern_q <- c("PI1 Knowledge", "PI2 Time management", "PI3 Patient Safety", "PI4 Teamwork", "PI5 Feedback", "PI6 Professionalism/Comm", "PI7 Critical thinking skill")
    
    peer2 <- cccdbDate() %>%
      filter(str_detect(Resident, res())) %>%
      filter(`Evaluator Status` == "Peer") %>%
      select(Resident, peer_intern_q, `PI8 Rec`) 
    
    peer2[peer_intern_q] <- lapply(peer2[peer_intern_q], factor, levels = c(1:5))
    peer2$`PI8 Rec` <- as.factor(peer2$`PI8 Rec`)
    
    
    peer2_p <- function(x,y) {
      x1 <- enquo(x)
      peer2_px <- peer2  %>%
        ggplot(., 
               aes(x=!! x1, fill=!! x1)) +
        geom_bar(col = "black") +
        theme_bw() +
        # scale_y_continuous(limits = c(0,10)) +
        scale_x_discrete(limits = factor(1:5), labels=c("1" = "Never", "2" = "Rarely", "3" = "Sometimes", "4" = "Often", "5" = "Role model")) +
        scale_fill_manual(values = c("1"="red","2"="red","3"="#03a1fc","4"="#03a1fc","5"="#03a1fc")) +
        labs(title = y) +
        theme_bw() +
        theme(title = element_text(size = 12,face="bold"),axis.text.x = element_text(size = 10),axis.title.x=element_blank(), legend.position = "none")
      return(peer2_px)
    }
    
    peer2_p1 <- peer2_p(`PI1 Knowledge`, "Knowledge in Medicine and Health System")
    peer2_p2 <- peer2_p(`PI2 Time management`,  "Time management skill")
    peer2_p3 <- peer2_p(`PI3 Patient Safety`, "Patient safety skill")
    peer2_p4 <- peer2_p(`PI4 Teamwork`, "Teamwork skill")
    peer2_p5 <- peer2_p(`PI5 Feedback`, "Gives and receives feedback effectively")
    peer2_p6 <- peer2_p(`PI6 Professionalism/Comm`, "Professionalism and Communication skill")
    peer2_p7 <- peer2_p(`PI7 Critical thinking skill`, "Critical thinking skill")
    
    peer2_p8 <- peer2 %>%
      ggplot(., 
             aes(x=`PI8 Rec`, fill = `PI8 Rec`)) +
      geom_bar(col="black") +
      theme_bw() +
      scale_y_continuous(limits = c(0,10)) +
      scale_x_discrete(limits = c("No","Yes")) +
      scale_fill_manual(limits = c("No", "Yes"), values = c("red", "#03a1fc")) +
      labs(title = "Recommendation of intern to other seniors") +
      theme_bw() +
      theme(title = element_text(size = 12,face="bold"),axis.text.x = element_text(size = 10),axis.title.x=element_blank(), legend.position = "none")
    
    if(nrow(peer2)==0) {
    } else {grid.arrange(arrangeGrob(peer2_p1, peer2_p2, peer2_p3, peer2_p4, peer2_p5, peer2_p6, peer2_p7, peer2_p8, ncol = 2))}
    
  })
  
  #peer comments
  output$peer_comm <- DT::renderDataTable({
    
    
    peer_comments <- cccdbDate() %>%
      filter(str_detect(Resident, res())) %>%
      filter(`Evaluator Status` == "Peer") %>%
      select(`Session Date`,Comments) %>%
      drop_na(Comments) %>%
      mutate(`Session Date` = mdy(`Session Date`)) %>%
      arrange(desc(`Session Date`))
    
    datatable(peer_comments, options = list (search = list(regex = TRUE, caseInsensitive = FALSE)))
  })

  
  
  # ite plotly ----
  ite_tidy <- function(){
    ite_tidy <- ite %>%
    gather(key, measure, -Name, -Subject_status) %>%
    separate(key, c("ite","style")) 
    return(ite_tidy)
  }
  
  
  output$ite <- renderPlotly({
    pgy <- ite_tidy() %>%
      filter(str_detect(Name,res())) %>%
      select(Subject_status) 
    
    ite_tidy_no_res <- ite_tidy() %>%
      filter(!str_detect(Name, res())) %>%
      filter(Subject_status == paste(unique(pgy)))
    
    ite_tidy_res <- ite_tidy() %>%
      filter(str_detect(Name, res()))
    
    styler <- c("Percent Correct","National Percentile")
    names(styler) <- c("percent","tile")
    
    ite_chart <- ggplot(ite_tidy_no_res,
                        aes(x=ite, y=measure)) +
      geom_violin(aes(fill=ite), alpha = 0.4) +
      geom_jitter(aes(name = Name),alpha = 0.5) +
      geom_point(data=ite_tidy_res,
                  aes(x=ite, y=measure, name=Name), color = "red", size = 2, shape = 5) +
      facet_grid(. ~ style, labeller = labeller(style=styler)) +
      ylab("Score") +
      theme_bw() +
      theme(axis.title.x = element_blank(), legend.title = element_blank(), legend.position = "none")
    
    ggplotly(ite_chart)
  })
  
  # * ite prediction ----
  output$ite_predict <- DT::renderDataTable({


    final_model <- readRDS("final_random_forest.rds")
    
    new_resident <- ite %>%
      filter(str_detect(Name, res())) %>%
      select(PGY1_percentile=ITE1_tile, PGY1_score=ITE1_percent, PGY2_percentile=ITE2_tile, PGY2_score=ITE2_percent, PGY3_percentile=ITE3_tile, PGY3_score=ITE3_percent, Subject_status) %>%
      na.omit()
    
    if (nrow(new_resident)>0) {
    ml_table <- stats::predict(final_model, new_resident, type = "prob") %>%
      mutate(.pred_Yes = .pred_Yes * 100) %>%
      select(`Probability of passing boards (%)` = .pred_Yes) } else {
        ml_table <- tibble(Probability = "N/A") 
      }
 

    DT::datatable(ml_table, rownames = FALSE, options = list(dom = 't'))
    
  })
  

  #ite percent whole ----
  output$ite_percent_whole <- renderPlotly({
    
    ite_tidy_percent <- ite_tidy() %>%
      filter(style == "percent")
    
    ite_chart_whole_percent <- ggplot(ite_tidy_percent,
                                      aes(x=ite, y=measure)) +
      geom_violin(aes(fill=ite), alpha = 0.4) +
      geom_jitter(aes(name = Name),alpha = 0.5) +
      facet_grid(. ~ Subject_status) +
      ylab("Score") +
      theme_bw() +
      theme(axis.title.x = element_blank(), legend.title = element_blank(), legend.position = "none")
    

    ggplotly(ite_chart_whole_percent) 

    
  })
  
  #ite percentile whole
  output$ite_percentile_whole <- renderPlotly({
    ite_tidy_tile <- ite_tidy() %>%
      filter(style == "tile")
    
    ite_chart_whole_tile <- ggplot(ite_tidy_tile,
                                   aes(x=ite, y=measure)) +
      geom_violin(aes(fill=ite), alpha = 0.4) +
      geom_jitter(aes(name = Name),alpha = 0.5) +
      facet_grid(. ~ Subject_status) +
      ylab("Score") +
      theme_bw() +
      theme(axis.title.x = element_blank(), legend.title = element_blank(), legend.position = "none")
    ggplotly(ite_chart_whole_tile)
    
  })
  

  # milestones trend ----
  output$mtrend1 <- renderPlotly({
    cccdb_no_on <- cccdb %>%
      filter(`Subject Rotation` != "On-demand") %>%
      filter(str_detect(Resident,"Barrymore")) %>%
      select(`Session Date`,comp_variable, `Evaluator Name`, `Subject Rotation`) %>%
      gather(subcomp, value, -`Session Date`, -`Evaluator Name`, -`Subject Rotation`) %>%
      mutate(date = `Session Date`,
             eval = `Evaluator Name`) %>%
      select(-`Session Date`) %>%
      mutate(subcomp = fct_relevel(subcomp, comp_variable))
    
    plot <- cccdb_no_on %>%
      ggplot(.,aes(x=date,y=value)) +
      stat_smooth(geom = "line", col = "green", alpha = 0.5, se = FALSE, formula = y ~ x) +
      geom_smooth(method = "lm", se=FALSE, formula = y ~ x) +
      geom_point(aes(name = eval, rotation = `Subject Rotation`),alpha = 0.4) +
      facet_wrap(.~subcomp) +
      ylim(-1,5.5) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_hline(yintercept = 0, color = "red", alpha = 0.2) +
      scale_x_date(date_labels = "%m-%Y") + 
      ggtitle("Trend of Subcompetencies")
    
    
    plot
  })
  }


## run shiny app
shinyApp(ui, server)

