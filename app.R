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
       tabPanel("Plot 1", plotOutput("plot"), tableOutput("table")),
       tabPanel("Plot 2", plotOutput("plot2"))
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
  
  
  # define output plot
  output$plot <- renderPlot({
    # add a warning if a the combination of attributes is restricted
    validate(
      need(nrow(rv_cases()) == 2, 'Invalid combination of attributes.')
    )
    # transform linear predictions into probabilities in 1:1 competition
    predprob <- as.data.frame(apply(rv_cases(),2,compute_probs_one_vs_second))
    colnames(predprob) <- c("prob","CI1","CI2")
    
     ggplot(predprob, aes(x = factor(c(1, 2)), y = prob)) +
       geom_pointrange(aes(ymin = CI1, ymax = CI2)) +
       ylim(c(0, 1)) +
       ylab("Predicted Vote Probabilities") +
       xlab("Candidate") +
       geom_hline(yintercept = 0.5, linetype = 3, col = "blue") +
       theme_bw()
  })
  
  # define output table
  output$table <- renderTable({
    # warning
    validate(
      need(nrow(rv_cases()) == 2, 'Invalid combination of attributes.')
    )

    predprob <- as.data.frame(apply(rv_cases(),2,compute_probs_one_vs_second)*100)
    colnames(predprob) <- c("prob","CI1","CI2")
      
    data.frame(Party=factor(c(1,2)), Prob=predprob$prob
                 ,lowerCI=predprob$CI1, upperCI=predprob$CI2)
    })
  
  # define second output plot
  output$plot2 <- renderPlot({
    # warning
    validate(
      need(nrow(rv_cases()) == 2, 'Invalid combination of attributes.')
    )
    
    predprob <- as.data.frame(apply(rv_cases(),2,compute_probs_one_vs_second))
    colnames(predprob) <- c("prob","CI1","CI2")
    
    # plot
    par(mar=c(0,0,0,0))
    plot(0,xlim=c(0,1),ylim=c(-9,10),type="n",axes=F,ann=F)
    polygon(x=c(-.002,.5,.5,-.002),y=c(0,0,1,1),col=rgb(0,0,1,alpha=.3),border=F)
    polygon(x=c(.5,1.002,1.002,.5),y=c(0,0,1,1),col=rgb(0,1,0,alpha=.3),border=F)
    text(x=.1,y=2.5,"Party 1",cex=.9,font=2)
    text(x=.9,y=2.5,"Party 2",cex=.9,font=2)
    for(i in seq(0,1,by=.2)){
      lines(x=c(i,i),y=c(1,0),lwd=.5)
    }
    lines(x=c(predprob$prob[2],predprob$prob[2]),y=c(1.5,-.5),lwd=2)
    polygon(x=c(predprob$CI1[2],predprob$CI2[2]
                ,predprob$CI2[2],predprob$CI1[2])
            ,y=c(-.2,-.2,1.2,1.2),col=rgb(0,0,0,alpha=.3),border=F)
  })
  
}


shinyApp(ui, server)

