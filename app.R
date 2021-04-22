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
ui <- navbarPage("Party Unity",
                 tabPanel("Simulation Tool",
                          sidebarLayout(
                            sidebarPanel(width = 5,
                                         fluidRow(
                                           h4("Please choose attributes of a ficitional electoral race between two parties."),
                                           column(width = 6, h3("Party 1"),
                                                  div(
                                                    id = "scenario_1",
                                                    selectInput('dist_1', 'Ideological Distance', choices = levels(linpreds$dist)),
                                                    selectInput('critique_1', 'Intra-Party Critique', choices = levels(linpreds$critique)),
                                                    selectInput('parliament_1', 'Parliamentary Voting Behavior', choices = levels(linpreds$parliament)),
                                                    selectInput('conference_1', 'Behavior at Congress', choices = levels(linpreds$conference)),
                                                    selectInput('reform_1', 'Reform Clarity', choices = levels(linpreds$reform)),
                                                    selectInput('role_1', 'Party Role', choices = levels(linpreds$role)),
                                                    selectInput('gender_1', 'Gender of Candidate', choices = levels(linpreds$gender)),
                                                    selectInput('age_1', 'Age of Candidate', choices = levels(linpreds$age)),
                                                    selectInput('job_1', "Candidate's Occupation", choices = levels(linpreds$job)),
                                                    actionButton("reset_1", "Reset")
                                                  )
                                           ),
                                           column(width = 6, h3("Party 2"),
                                                  div(
                                                    id = "scenario_2",
                                                    selectInput('dist_2', 'Ideological Distance', choices = levels(linpreds$dist)),
                                                    selectInput('critique_2', 'Intra-Party Critique', choices = levels(linpreds$critique)),
                                                    selectInput('parliament_2', 'Parliamentary Voting Behavior', choices = levels(linpreds$parliament)),
                                                    selectInput('conference_2', 'Behavior at Congress', choices = levels(linpreds$conference)),
                                                    selectInput('reform_2', 'Reform Clarity', choices = levels(linpreds$reform)),
                                                    selectInput('role_2', 'Party Role', choices = levels(linpreds$role)),
                                                    selectInput('gender_2', 'Gender of Candidate', choices = levels(linpreds$gender)),
                                                    selectInput('age_2', 'Age of Candidate', choices = levels(linpreds$age)),
                                                    selectInput('job_2', "Candidate's Occupation", choices = levels(linpreds$job)),
                                                    actionButton("reset_2", "Reset")
                                                  )
                                           )
                                         )
                            ),
                            mainPanel(width = 5,
                                      h2("Predicted individual vote probabilities"),
                                      HTML("<br><br><br>"),
                                      h4(textOutput("result")),
                                      HTML("<br><br>"),
                                      plotOutput("plot"))
                          )
                 ),
                 # tabPanel("AMCE"),
                 tabPanel("About", includeMarkdown("README.md"))
)

# server function
server <- function(input, output) {
  
  observeEvent(input$reset_1, {
    reset("scenario_1")
  })
  
  observeEvent(input$reset_2, {
    reset("scenario_2")
  })
  
  # compute cases within reactive expression (good practice because inputs are handled lazily and cached)
  rv_cases <- reactive({
    rbind(linpreds[role == input$role_1 &
                   conference == input$conference_1 &
                   parliament == input$parliament_1 &
                   critique == input$critique_1 &
                   reform == input$reform_1 &
                   gender == input$gender_1 &
                   age == input$age_1 &
                   job == input$job_1 &
                   dist == input$dist_1,
                   c("median", "upper", "lower")],
          linpreds[role == input$role_2 &
                   conference == input$conference_2 &
                   parliament == input$parliament_2 &
                   critique == input$critique_2 &
                   reform == input$reform_2 &
                   gender == input$gender_2 &
                   age == input$age_2 &
                   job == input$job_2 &
                   dist == input$dist_2,
                   c("median", "upper", "lower")]
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
  
  output$result <- renderText({
    
    # warning
    validate(
      need(nrow(rv_cases()) == 2, 'Invalid combination of attributes.')
    )
    
    predprob <- as.data.frame(apply(rv_cases(),2,compute_probs_one_vs_second))
    colnames(predprob) <- c("prob","CI1","CI2")
    paste0("The plot below shows the predicted result from the simulated competition. 
           In the specified case, the predicted probability that an individual votes for party 1 is ", 
          round(predprob$prob[1] * 100, 1), "% [", 
          round(predprob$CI1[1] * 100, 1), "; ",
          round(predprob$CI2[1] * 100, 1), "].")
  })
}


shinyApp(ui, server)

