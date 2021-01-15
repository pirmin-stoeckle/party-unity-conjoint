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

# function to convert linear predictors to predicted probabilities
compute_probs_one_vs_second <- function(x) {
  prob.first <- exp(x[1])/(sum(exp(x)))
  return(c(prob.first, 1-prob.first))
}

# user interface
ui <- fluidPage(
  useShinyjs(),
  
  titlePanel("Party Unity Vote Experiment - Simulation Tool"),
  
  theme = shinytheme("flatly"),
  
  sidebarLayout(
    sidebarPanel(
      "This could be a description of how all this works"
   ),

 mainPanel(
   fluidRow(
     column(3, h2("Candidate 1"),
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
     column(3, h2("Candidate 2"),
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
     ),
     column(5, h2("Predicted Vote Probabilities"),
            plotOutput("plot"),
            tableOutput("table")
     ,offset=1)
   ),
 )
  )
)

# server function
server <- function(input, output) {
  
  observeEvent(input$reset_1, {
    reset("scenario_1")
  })
  
  observeEvent(input$reset_2, {
    reset("scenario_2")
  })
  
  output$plot <- renderPlot({
    
    cases <- rbind(linpreds[role == input$role_1 &
                        conference == input$conference_1 &
                        parliament == input$parliament_1 &
                        critique == input$critique_1 &
                        reform == input$reform_1 &
                        gender == input$gender_1 &
                        age == input$age_1 &
                        job == input$job_1 &
                        dist == input$dist_1],
                   linpreds[role == input$role_2 &
                              conference == input$conference_2 &
                              parliament == input$parliament_2 &
                              critique == input$critique_2 &
                              reform == input$reform_2 &
                              gender == input$gender_2 &
                              age == input$age_2 &
                              job == input$job_2 &
                              dist == input$dist_2]
    )
    
    cases[,prob:=compute_probs_one_vs_second(mean)]
    cases[,CI1:=compute_probs_one_vs_second(lower)]
    cases[,CI2:=compute_probs_one_vs_second(upper)]
    
    # plot
    ggplot(cases, aes(x = factor(c(1, 2)), y = prob)) +
      geom_pointrange(aes(ymin = CI1, ymax = CI2)) +
      ylim(c(0, 1)) +
      ylab("") +
      xlab("Candidate") +
      geom_hline(yintercept = 0.5, linetype = 3, col = "blue") +
      theme_bw()
    
  })
  
  output$table <- renderTable({
    cases <- rbind(linpreds[role == input$role_1 &
                            conference == input$conference_1 &
                            parliament == input$parliament_1 &
                            critique == input$critique_1 &
                            reform == input$reform_1 &
                            gender == input$gender_1 &
                            age == input$age_1 &
                            job == input$job_1 &
                            dist == input$dist_1],
                 linpreds[role == input$role_2 &
                            conference == input$conference_2 &
                            parliament == input$parliament_2 &
                            critique == input$critique_2 &
                            reform == input$reform_2 &
                            gender == input$gender_2 &
                            age == input$age_2 &
                            job == input$job_2 &
                            dist == input$dist_2]
    )
  
   cases[,prob:=compute_probs_one_vs_second(mean)]
   cases[,CI1:=compute_probs_one_vs_second(lower)]
   cases[,CI2:=compute_probs_one_vs_second(upper)]
   
   cases[, .(Candidate = factor(c(1,2)), Prob = prob*100, LowerCI = CI1*100, UpperCI = CI2*100)]
   })
  
}


shinyApp(ui, server)

