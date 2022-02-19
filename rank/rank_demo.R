library(tidyverse)
library(gridExtra)
library(plotly)
library(shiny)
library(shinydashboard)
library(DT)
library(ggpubr)


# load dataset ----
load("rank_fake.rda")

header <- dashboardHeader(title = "Ranking")


# Sidebar -----------------------------------------------------------------



sidebar <- dashboardSidebar(
  tags$style(HTML(".sidebar-menu li { font-size: 20px; }")),
  selectInput("catpre", "Position", "Categorical", selected = "Categorical"),
  selectInput("lname", "Name:", ""),
  passwordInput("pass", "Password:"),
  sidebarMenu(
    menuItem("Summary", tabName = "summary"),
    menuItem("Details (Interview)", tabName = "details"),
    menuItem("Local vs IMG", tabName = "locimg"),
    menuItem("Automated Rank", tabName = "rank_auto")

  )
)

body <- dashboardBody(
  fluidRow(
    column(width = 12,
           fluidRow(
             column(width = 3,
                   box(textOutput("nameOutput"), width = "100%",
                       tags$head(tags$style("#nameOutput{color: black;
                                 font-size: 18px;
                                 font-weight: Bold;
                                 }"
                       )
                       ))),
             column(width = 3,
                    box(textOutput("score"), width = "100%",
                        tags$head(tags$style("#score{color: black;
                                 font-size: 18px;
                                 }"
                        )
                        ))),
             column(width = 3,
                    box(textOutput("uni"), width = "100%"),
                    tags$head(tags$style("#uni{color: black;
                                 font-size: 18px;
                                 }"
                    )
                    )
             
             )
           ))),
    tabItems(
      tabItem("summary", # tabItem: summary ----
              fluidRow(
                       column(3,
                                plotlyOutput("overall", width = "100%", height = "700px"),
                       fluidRow(
                          DT::dataTableOutput("overalldt")))
                       ,
                       
                         tabBox(height = "850px",
                              tabPanel("USMLE",
                                    column(8, plotlyOutput("usmle", width = "100%", height = "750px")),
                                    column(4, plotlyOutput("usmle_cs", width = "100%", height = "750px"))),
                                    
                           tabPanel("COMLEX",
                                    column(8, plotlyOutput("comlex", width = "100%", height = "750px")),
                                    column(4, plotlyOutput("comlex_pe", width = "100%", height = "750px"))))
                         ))
          
                       ,
      tabItem("details",
              fluidRow(
                column(12,
                       column(2,
                              plotlyOutput("prof", width = "100%", height = "700px")),
                       column(2,
                              plotlyOutput("comm", width = "100%", height = "700px"))),
                       
              fluidRow(
                column(12,
                       column(2,
                              dataTableOutput("profd")),
                       column(2,
                              dataTableOutput("commd")))
              ))),
      tabItem("locimg",
              fluidRow(
                column(12,
                       column(4,
                              plotlyOutput("overall_split")),
                       column(8,
                              plotlyOutput("usmle_split"))
                       )
              ),
              fluidRow(
                column(12,
                       column(4,
                              plotlyOutput("prof_split")),
                       column(4,
                              plotlyOutput("comm_split"))
                      
              ))),
      tabItem("rank_auto",  # automated rank ----
              fluidRow(
                column(12,
                       column(3,
                              fluidRow(textInput("profr", "Professionalism:", value = "0.2")),
                              fluidRow(textInput("commr", "Communications:", value = "0.2")),
                              fluidRow(textInput("overallimpr", "Overall Impression:", value = "0.2")),
                              fluidRow(textInput("step1r", "USMLE/COMLEX 1:", value = "0.2")),
                              fluidRow(textInput("step2r", "USMLE/COMLEX 2 CK/CE:", value = "0.2")),
                              fluidRow(textOutput("total_quality")),
                              fluidRow(actionButton("reset","Reset Default"))
                              ),
                       column(9,
                              DT::dataTableOutput("rank_auto_rank")))
              ))))
    



ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
  
  #password ----
  
  password <- ""
  
  makerankeasy <- function() {
    
    rank <- new_df %>%
      separate(`USMLE(?/?/?)`, c("USMLE_1", "USMLE_2_CK", "USMLE_2_CS","USMLE_3"), sep = "/", remove = FALSE) %>%
      separate(`COMLEX(?/?/?)`, c("COMLEX_1", "COMLEX_2_CE", "COMLEX_3_PE"), sep = "/", remove = FALSE) %>%
      mutate(
        USMLE_2_CS = case_when(
          USMLE_2_CS %in% c("Pass","Fail") ~ USMLE_2_CS,
          TRUE ~ NA_character_
        ),
        COMLEX_1 = as.double(COMLEX_1),
        COMLEX_2_CE = as.double(COMLEX_2_CE),
        `Prof (mean)` = select(.,Prof1,Prof2,Prof3,Prof4) %>% rowMeans(na.rm = T),
             `Comm (mean)`= select(.,Comm1,Comm2,Comm3,Comm4) %>% rowMeans(na.rm = T),           `Overall Imp (mean)`= select(.,`Overall Imp 1`,`Overall Imp 2`,`Overall Imp 3`,`Overall Imp 4`) %>% rowMeans(na.rm = T))
    
    rank[rank == "NULL"] <- NA
    rank[rank == "NA"] <- NA
    rank[rank == "#DIV/0!"] <- NA
    rank$USMLE_1 <- as.numeric(rank$USMLE_1)
    rank$USMLE_2_CK <- as.numeric(rank$USMLE_2_CK)
    
    namelist <- rank$Name
    
    updateSelectInput(session = session, inputId = "lname", label = "Name:", choices = namelist, selected = namelist[1])
    
    return(rank)

  }
  
  rank <- reactive({
    if (password == input$pass) { 
      makerankeasy() 
      }  
    else { }
  })


  name <- reactive({
    name <- input$lname
    return(name)
  })
  
  combine_evaluators_to_dt <- function(x) {
    select_x <- enquo(x)
    
    grouped_eval <- rank() %>%
      filter(Name == name())
    
    grouped_eval_value <- grouped_eval %>%
      select(!! select_x) %>%
      gather(prof, value) %>%
      select(value)
    
    grouped_eval_name <- grouped_eval %>%
      select(PD:Chief) %>%
      gather(title, name) %>%
      select(name)
    
    grouped_eval_combined_dt <- cbind(grouped_eval_name, grouped_eval_value)
    
    datatable(grouped_eval_combined_dt,
              list(dom = "t"), rownames = FALSE)
  }
  

  
  # name output ----
  
  output$nameOutput <- renderText({
    name_aamc <- rank() %>%
      filter(Name == name()) %>%
      select(Name, `AAMC ID`)
    unlist(name_aamc)
  })
  
  output$score <- renderText({
    score <- rank() %>%
      filter(Name == name()) %>%
      select(`USMLE(?/?/?)`,`COMLEX(?/?/?)`) %>%
      gather(test, score) 
    paste("USMLE:", unlist(score$score[1]), "COMLEX:", unlist(score$score[2]))
  
  })
  
  
  output$uni <- renderText({
    uni <- rank() %>%
      filter(Name == name()) %>%
      select(Institution)
    
    gradyear <- rank() %>%
      filter(Name == name()) %>%
      select(`Grad year`)
    
    paste(unlist(uni), "(Grad date:", unlist(gradyear), ")")
    
  })
  
  # resident dinner feedback ---- 
  
  output$resident <- renderText({
    resident <- rank() %>%
      filter(Name == name()) %>%
      select(`Resident Overall`)
    
    paste0(resident)
    
    
  })
  


  # overall score ----
  
  output$overall <- renderPlotly({
    overall <- rank() %>%   
      select(Name, `Overall Imp (mean)`) %>%
      gather(overall, value, -Name) %>%
      drop_na()
    
    overall_jitter_drop_can <- rank() %>%
      filter(Name != name()) %>%
      select(Name, `Overall Imp (mean)`) %>%
      gather(overall, value, -Name) %>%
      drop_na()
    
    overall_can <- rank() %>%
      filter(Name == name()) %>%
      select(Name, `Overall Imp (mean)`) %>%
      gather(overall, value, -Name) 
    
    overallplot <- ggplot(overall,
                          aes(x=overall, y=value)) +
      geom_violin(fill = "green", alpha = 0.2) +
      geom_jitter(data=overall_jitter_drop_can, aes(name = Name), alpha = 0.6) +
      geom_point(data=overall_can,
                 aes(x=overall, y=value, name = Name), color = "red", shape = 5, size = 5) +
      theme_bw() +
      labs(title = "Overall Impression (mean)") +
      ylab("Score (out of 6)")
    
    ggplotly(overallplot)
    
  })
  
  # overall datatable ----
  output$overalldt <- renderDataTable({
    combine_evaluators_to_dt(`Overall Imp 1`:`Overall Imp 4`)
  })
  
  # usmle (summary)----
  output$usmle <- renderPlotly({
    usmle_overall <- rank() %>%
      select(Name,USMLE_1,USMLE_2_CK) %>%
      filter(USMLE_1 <= 400) %>%
      gather(test, score, -Name) %>%
      drop_na()
    
    usmle_jitter_drop_can <- rank() %>%
      filter(Name != name() & USMLE_1 <= 400) %>%
      select(Name,USMLE_1,USMLE_2_CK) %>%
      gather(test, score, -Name) %>%
      drop_na()
    
    usmle_can <- rank() %>%
      filter(Name == name()) %>%
      select(Name, USMLE_1, USMLE_2_CK) %>%
      gather(test, score, -Name)
    
    usmle_plot <- ggplot(data=usmle_overall,
                         aes(x=test,y=score)) +
      geom_violin(aes(fill = test), alpha = 0.2) +
      geom_jitter(data=usmle_jitter_drop_can,
                  aes(x=test,y=score, name = Name), alpha = 0.6) +
      geom_point(data=usmle_can,
                 aes(x=test,y=score, name = Name), color = "red", size = 5, shape =5) +
      theme_bw() +
      theme(legend.position = 'none') +
      labs(title = "USMLE")
    
    ggplotly(usmle_plot)
  })
  
  output$usmle_cs <- renderPlotly({
    usmle_cs_all <- rank() %>%
      filter(!is.na(Name)) %>%
      select(Name, USMLE_2_CS) %>%
      gather(test, score, -Name) 
    usmle_cs_all$score <- as.factor(usmle_cs_all$score)
    
    usmle_cs_drop_can <- rank() %>%
      filter(Name != name()) %>%
      select(Name, USMLE_2_CS) %>%
      gather(test, score, -Name)
      
    usmle_cs_can <- rank() %>%
      filter(Name == name()) %>%
      select(Name, USMLE_2_CS) %>%
      gather(test, score, -Name)
    
    usmle_cs_plot <- ggplot(usmle_cs_all,
                            aes(x=score)) +
      geom_bar(aes(fill=score), alpha = 0.5) +
      theme_bw() +
      geom_point(data=usmle_cs_can, aes(x=as.factor(score),y=5, name = Name), color = "red", shape = 5, size = 5) +
      theme(legend.position = 'none') +
      labs(title = "USMLE_2_CS")
      
    
    ggplotly(usmle_cs_plot)
    
    
  })

  output$comlex <- renderPlotly({
    comlex_overall <- rank() %>%
      select(Name,COMLEX_1,COMLEX_2_CE) %>%
      gather(test, score, -Name) %>%
      drop_na()
    comlex_overall$score <- as.numeric(comlex_overall$score)
    
    comlex_jitter_drop_can <- rank() %>%
      filter(Name != name()) %>%
      select(Name,COMLEX_1,COMLEX_2_CE) %>%
      gather(test, score, -Name) %>%
      drop_na()
    comlex_jitter_drop_can$score <- as.numeric(comlex_jitter_drop_can$score)
    
    comlex_can <- rank() %>%
      filter(Name == name()) %>%
      select(Name, COMLEX_1,COMLEX_2_CE) %>%
      gather(test, score, -Name)
    comlex_can$score <- as.numeric(comlex_can$score)
    
    comlex_plot <- ggplot(data=comlex_overall,
                         aes(x=test,y=score)) +
      geom_violin(aes(fill =test), alpha = 0.2) +
      geom_jitter(data=comlex_jitter_drop_can,
                  aes(x=test,y=score, name = Name), alpha = 0.5) +
      geom_point(data=comlex_can,
                 aes(x=test,y=score, name = Name), color = "red", size = 5, shape =5) +
      theme_bw() +
      theme(legend.position = 'none')
    
    ggplotly(comlex_plot)
  })

  output$comlex_pe <- renderPlotly({
    comlex_pe_all <- rank() %>%
      filter(!is.na(Name)) %>%
      select(Name, COMLEX_3_PE) %>%
      gather(test, score, -Name) 
    comlex_pe_all$score <- as.factor(comlex_pe_all$score)
    
    comlex_pe_drop_can <- rank() %>%
      filter(Name != name()) %>%
      select(Name, COMLEX_3_PE) %>%
      gather(test, score, -Name)
    
    comlex_pe_can <- rank() %>%
      filter(Name == name()) %>%
      select(Name, COMLEX_1, COMLEX_3_PE) %>%
      gather(test, score, -Name, -COMLEX_1) 
    
    comlex_pe_plot <- ggplot(comlex_pe_all,
                            aes(x=score)) +
      geom_bar(aes(fill=score), alpha = 0.5) +
      theme_bw() +
      geom_point(data=comlex_pe_can, aes(x=as.factor(score),y=10, name = Name), color = "red", shape = 5, size = 5) +
      theme(legend.position = 'none')
    
    
    ggplotly(comlex_pe_plot)
    
    
  })
  

  output$comments <- renderText({
    comments <- rank() %>%
      filter(Name == name()) %>%
      select(`Evaluators Comments`)
    
    paste("Evaluators' Comments:", comments)
  })
  
  output$coor <- renderText({
    coor <- rank() %>%
      filter(Name == name()) %>%
      select(`Coordinator Comments`)
    
    paste("Coordinators' comments:", coor)
  })
  
  
  # details plotly ----
  output$prof <- renderPlotly({
    prof_all <- rank() %>% 
      filter(Name != name()) %>%
      select(Name, `Prof (mean)`) %>%
      gather(prof, value, -Name)
    
    prof_can <- rank() %>%
      filter(Name == name()) %>%
      select(Name, `Prof (mean)`) %>%
      gather(prof, value, -Name)
    
    prof_plot <- ggplot(prof_all,
                        aes(x=prof, y=value)) +
      geom_violin(fill = "green", alpha = 0.2) +
      geom_jitter(aes(name = Name), alpha = 0.5) +
      geom_point(data=prof_can,
                 aes(x=prof, y=value, name = Name), shape = 5, size = 5, col = "red") +
      theme_bw() +
      ylim(2,6.15) +
      labs(title = "Professionalism") +
      theme(axis.title = element_blank())
    
    ggplotly(prof_plot)
  })
  
  output$comm <- renderPlotly({
    comm_all <- rank() %>%
      filter(Name != name()) %>%
      select(Name, `Comm (mean)`) %>%
      gather(comm, value, -Name)
    
    comm_can <- rank() %>%
      filter(Name == name()) %>%
      select(Name, `Comm (mean)`) %>%
      gather(comm, value, -Name)
    
    comm_plot <- ggplot(comm_all,
                        aes(x=comm, y=value)) +
      geom_violin(fill = "blue", alpha = 0.2) +
      geom_jitter(aes(name = Name), alpha = 0.5) +
      geom_point(data=comm_can,
                 aes(x=comm, y=value, name = Name), shape = 5, size = 5, col = "red") +
      theme_bw() +
      ylim(2,6.15) +
      labs(title = "Communication") +
      theme(axis.title = element_blank())
    
    ggplotly(comm_plot)
  })
  

  output$profd <- DT::renderDataTable({
  combine_evaluators_to_dt(Prof1:Prof4)
  })
  
  output$commd <- renderDataTable({
    combine_evaluators_to_dt(Comm1:Comm4)
  })
  

  # local vs img function ----
  plotall <- function(measure, color, title) {
    x <- enquo(measure)

    overall <- rank() %>%
      select(Name, !! x, school) %>%
      gather(overall, value, -Name,-school) %>%
      drop_na() 
    
    overall_jitter_drop_can <- overall %>%
      filter(Name != name())
    
    overall_can <- overall %>%
      filter(Name == name())
    
    overallplot <- ggplot(data=overall, aes(x=school, y=value)) +
      geom_violin(fill = color, alpha = 0.2) +
      geom_boxplot() +
      geom_jitter(data=overall_jitter_drop_can, aes(name = Name), alpha = 0.6) +
      geom_point(data=overall_can,
                 aes(x=school, y=value, name = Name), color = "red", shape = 5, size = 5) +
      labs(title = title) +
      theme_bw() +
      theme(axis.title.x = element_blank()) +
      ylim(2,6) +
      ggpubr::stat_compare_means(method = "t.test", label.x = 1.5, label.y = 2.2) +
      ggpubr::stat_compare_means(label.x = 1.5, label.y = 2)
    
    ggplotly(overallplot)
  }
  
  output$overall_split <- renderPlotly({
    plotall(measure = `Overall Imp (mean)`, color = "green", title = "Overall Impression (mean)")
    
  })
  
  # usmle split ----
  
  output$usmle_split <- renderPlotly({

    usmle_overall <- rank() %>%
      select(Name,USMLE_1, USMLE_2_CK, school) %>%
      gather(test, score, -Name, -school) %>%
      drop_na()
    
    usmle_jitter_drop_can <- usmle_overall %>%
      filter(Name != name())
    
    usmle_can <- usmle_overall %>%
      filter(Name == name())
    
    usmle_plot <- ggplot(data=usmle_overall,
                         aes(x=school,y=score)) +
      geom_violin(aes(fill = test), alpha = 0.2) +
      geom_boxplot(alpha = 0.2) +
      geom_jitter(data=usmle_jitter_drop_can,
                  aes(x=school,y=score, name = Name), alpha = 0.6) +
      geom_point(data=usmle_can,
                 aes(x=school,y=score, name = Name), color = "red", size = 5, shape =5) +
      theme_bw() +
      labs(title = "USMLE Step 1 and Step 2 CK") +
      theme(legend.position = 'none', axis.title.x = element_blank()) +
      facet_wrap(. ~ test) +
      ggpubr::stat_compare_means(method = "t.test", label.x = 1.5, label.y = 180) +
      ggpubr::stat_compare_means(label.x = 1.5, label.y = 175)
      
    
    ggplotly(usmle_plot)
  })
  
  output$prof_split <- renderPlotly({
    plotall(`Prof (mean)`, "green", "Profesionalism (mean)")
  })
  
  output$comm_split <- renderPlotly({
    plotall(`Comm (mean)`, "purple", "Communication (mean)")
  })
  
  
  
  #Automated Rank ----
  output$rank_auto_rank <- DT::renderDataTable({
    
  
    auto_rank <- rank() %>%
      select(Name, Institution, school, USMLE_1, USMLE_2_CK, COMLEX_1, COMLEX_2_CE, `Prof (mean)`, `Comm (mean)`,`Overall Imp (mean)`) %>%
      filter(!is.na(Name)) %>%
      mutate(Name = sapply(Name, tolower))
    
# normalizing the variable ----
    
    normalit<-function(m,min,max){
      if(is.na(m)) { return(NA) }
      else {
      (m - min)/(max-min)
      }
    }
    
    fm <- function(x,fun) { auto_rank %>% select(!!enquo(x)) %>% drop_na() %>% fun() } 
    u1min <- fm(USMLE_1,min)
    u1max <- fm(USMLE_1,max)
    u2min <- fm(USMLE_2_CK,min)
    u2max <- fm(USMLE_2_CK,max)
    com1min <- fm(COMLEX_1,min)
    com1max <- fm(COMLEX_1,max)
    com2min <- fm(COMLEX_2_CE,min)
    com2max <- fm(COMLEX_2_CE,max)
  
    
    auto_rank3 <- auto_rank %>%
    mutate(
      usmle1_percentile = map_dbl(USMLE_1, .f = ~normalit(.x,u1min,u1max)),
      usmle2_ck_percentile = map_dbl(USMLE_2_CK, .f = ~normalit(.x,u2min,u2max)),
      comlex_percentile = map_dbl(COMLEX_1, .f = ~normalit(.x,com1min,com1max)),
      comlex_ce_percentile = map_dbl(COMLEX_2_CE, .f = ~normalit(.x,com2min,com2max)),
      usmle1_comlex_adjust = case_when(
             comlex_percentile > usmle1_percentile | is.na(usmle1_percentile) ~ as.numeric(comlex_percentile),
             TRUE ~ as.numeric(usmle1_percentile)
           ),
           usmle2cs_comlexce_adjust = case_when(
             comlex_ce_percentile > usmle2_ck_percentile | is.na(usmle2_ck_percentile) ~ as.numeric(comlex_ce_percentile),
             TRUE ~ as.numeric(usmle2_ck_percentile)
           ),
           prof = `Prof (mean)`/6,
           comm = `Comm (mean)`/6,
           overallimp = `Overall Imp (mean)`/6)

      
      x1 <- as.numeric(input$profr)
      x2 <- as.numeric(input$commr)
      x7 <- as.numeric(input$overallimpr)
      x8 <- as.numeric(input$step1r)
      x9 <- as.numeric(input$step2r)
      
    
     auto_rank4 <- auto_rank3 %>%
      mutate(rank_score =
               prof * x1 +
               comm * x2 + 
               overallimp * x7 +
               usmle1_comlex_adjust * x8 +
               usmle2cs_comlexce_adjust * x9) 

  
    auto_rank_fin <- auto_rank4 %>%
                 mutate(rank = base::rank(-rank_score)) %>%
                 select(Name, rank, rank_score, Institution, school, usmle1_comlex_adjust, usmle2cs_comlexce_adjust, prof, comm, overallimp)
    
    
  DT::datatable(auto_rank_fin,
            options = list(
              pageLength = 40,
              scrollX = TRUE, fixedColumns = TRUE, scrollY = "1000px",
              order = list(2,'desc'),
              search = list(regex = TRUE)),
            rownames =FALSE)
          
  })
  
  output$total_quality <- renderText({
    total <- as.numeric(input$profr) + as.numeric(input$commr) + as.numeric(input$overallimpr) + as.numeric(input$step1r) + as.numeric(input$step2r) 
    
    total
  })
  
  observeEvent(input$reset, {
    updateTextInput(session, "profr", value = 0.2)
    updateTextInput(session, "commr", value = 0.2)
    updateTextInput(session, "overallimpr", value = 0.2)
    updateTextInput(session, "step1r", value = 0.2)
    updateTextInput(session, "step2r", value = 0.2)
    
  })
  
  

} 
  




shinyApp(ui, server)


