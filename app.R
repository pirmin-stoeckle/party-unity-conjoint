library(shiny)
library(shinyjs)
library(shinythemes)
library(data.table)
library(ggplot2)

# clear directory
rm(list=ls())

# load data
# all we need as input is linpreds.RData
load(file=paste0(getwd(),"/data/linpreds.RData"))

# function to convert linear predictors to predicted probabilities in a 1:1 competition
compute_probs_one_vs_second <- function(x) {
  prob.first <- exp(x[1])/(sum(exp(x)))
  return(c(prob.first, 1-prob.first))
}

# user interface
ui <- fluidPage(
  useShinyjs(),
  
  titlePanel("Party Vote Choice Experiment - Simulation Tool"),
  
  theme = shinytheme("flatly"),
  
  sidebarLayout(
    sidebarPanel(width = 5,
     fluidRow(
       HTML("This App is a companion to the paper 'Assessing the relative influence of party unity on vote choice: Evidence from a conjoint experiment'. <br>
             Below you can choose attributes of a ficitional electoral race between two parties. <br>"),
        column(width = 6, h3("Party 1"),
               div(
                 id = "scenario_1",
                 h4("Candidate Characteristics:"),
                 selectInput('gender_1', 'Gender', choices = levels(linpreds$gender)),
                 selectInput('age_1', 'Age', choices = levels(linpreds$age)),
                 selectInput('job_1', 'Occupation', choices = levels(linpreds$job)),
                 h4("Intra-Party Behavior"),    
                 selectInput('conference_1', 'Behavior at party conference', choices = levels(linpreds$conference)),
                 selectInput('parliament_1', 'Voting behavior in parliament', choices = levels(linpreds$parliament)),
                 selectInput('critique_1', 'Intra-party critique', choices = levels(linpreds$critique)),
                 selectInput('reform_1', 'Clarity of reform proposals', choices = levels(linpreds$reform)),
                 h4("Party Position"), 
                 selectInput('dist_1', 'Ideological distance', choices = levels(linpreds$dist)),
                 h4("Party's Characteristic"), 
                 selectInput('role_1', 'Party role', choices = levels(linpreds$role)),
                 actionButton("reset_1", "Reset")
               )
        ),
        column(width = 6, h3("Party 2"),
               div(
                 id = "scenario_2",
                 h4("Candidate Characteristics"),
                 selectInput('gender_2', 'Gender', choices = levels(linpreds$gender)),
                 selectInput('age_2', 'Age', choices = levels(linpreds$age)),
                 selectInput('job_2', 'Occupation', choices = levels(linpreds$job)),
                 h4("Intra-Party Behavior"),    
                 selectInput('conference_2', 'Behavior at party conference', choices = levels(linpreds$conference)),
                 selectInput('parliament_2', 'Voting behavior in parliament', choices = levels(linpreds$parliament)),
                 selectInput('critique_2', 'Intra-party critique', choices = levels(linpreds$critique)),
                 selectInput('reform_2', 'Clarity of reform proposals', choices = levels(linpreds$reform)),
                 h4("Party Position"), 
                 selectInput('dist_2', 'Ideological distance', choices = levels(linpreds$dist)),
                 h4("Party's Characteristic"), 
                 selectInput('role_2', 'Party role', choices = levels(linpreds$role)),
                 actionButton("reset_2", "Reset")
               )
       )
   )),

 mainPanel(width = 5,
   fluidRow(
     tabsetPanel(type = "tabs",
       tabPanel("Plot", plotOutput("plot"))
       )
   )
  )
))

# server function
server <- function(input, output) {
  
  observeEvent(input$reset_1, {
    reset("scenario_1")
  })
  
  observeEvent(input$reset_2, {
    reset("scenario_2")
  })
  
  # compute cases within reactive expression (good practive because inputs are handled lazily and cached)
  rv_cases <- reactive({
    rbind(linpreds[role == input$role_1 &
                              conference == input$conference_1 &
                              parliament == input$parliament_1 &
                              critique == input$critique_1 &
                              reform == input$reform_1 &
                              gender == input$gender_1 &
                              age == input$age_1 &
                              job == input$job_1 &
                              dist == input$dist_1
                            ,c("median", "upper", "lower")],
                   linpreds[role == input$role_2 &
                              conference == input$conference_2 &
                              parliament == input$parliament_2 &
                              critique == input$critique_2 &
                              reform == input$reform_2 &
                              gender == input$gender_2 &
                              age == input$age_2 &
                              job == input$job_2 &
                              dist == input$dist_2
                            ,c("median", "upper", "lower")]
    )
  })
  
  # define  output plot
  output$plot <- renderPlot({
    # warning
    validate(
      need(nrow(rv_cases()) == 2, 'Invalid combination of attributes.')
    )
    
    predprob <- as.data.frame(apply(rv_cases(),2,compute_probs_one_vs_second))
    colnames(predprob) <- c("prob","CI1","CI2")
    
    # plot
    par(mar=c(0,0,0,0),mar=c(0,.5,0,.5))
    plot(0,xlim=c(0,1),ylim=c(-4.5,1.2),type="n",axes=F,ann=F)
    # 1 vs. 2a
    polygon(x=c(-.002,1.002,1.002,-.002),y=c(0,0,.5,.5),border=F
            ,col=alpha("gray",alpha=.3))
    text(x=.05,y=1.1,"Party 1",cex=1,font=2)
    text(x=.95,y=1.1,"Party 2",cex=1,font=2)
    for(i in c(0,.25,.5,.75,1)){
      lines(x=c(i,i),y=c(.5,0),lwd=.7,lty=3)
      text(x=i,y=-.4,paste0(100-i*100,":",i*100),cex=1)
    }
    polygon(x=c(1-predprob$CI1[1],1-predprob$CI2[1]
                ,1-predprob$CI2[1],1-predprob$CI1[1])
            ,y=c(-.05,-.05,.55,.55),col=alpha("gray",alpha=1),border=F)
    lines(x=c(1-predprob$prob[1],1-predprob$prob[1]),y=c(.6,-.1),lwd=1)
    text(x=1-predprob$prob[1],y=.9,paste(round(predprob$prob[1],2)*100
                                         ,":",round(1-predprob$prob[1],2)*100),cex=1)
  })
  
}


shinyApp(ui, server)

