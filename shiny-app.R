library(shiny)
library(shinyjs)
library(shinythemes)

# all we need as input is linpreds

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
            h4("Candidate characteristics"),
            selectInput('gender_1', 'Gender', choices = levels(linpreds$gender)),
            selectInput('age_1', 'Age', choices = levels(linpreds$age)),
            selectInput('job_1', 'Former occupation', choices = levels(linpreds$job)),
            h4("Intra-party behavior"),    
            selectInput('conference_1', 'Behavior at party conference', choices = levels(linpreds$conference)),
            selectInput('parliament_1', 'Voting behavior in parliament', choices = levels(linpreds$parliament)),
            selectInput('critique_1', 'Intra-party critique', choices = levels(linpreds$critique)),
            selectInput('reform_1', 'Clarity of reform proposals', choices = levels(linpreds$reform)),
            h4("Party position"), 
            selectInput('dist_1', 'Ideological distance', choices = levels(linpreds$dist)),
            h4("Party's characteristic"), 
            selectInput('role_1', 'Party role', choices = levels(linpreds$role)),
            h4("Respondent's characteristic"), 
            selectInput('respondent_educ_1', "Respondent's education", choices = levels(linpreds$respondent_educ)),
            actionButton("reset_1", "Reset")
            )
     ),
     column(3, h2("Candidate 2"),
            div(
            id = "scenario_2",
            h4("Candidate characteristics"),
            selectInput('gender_2', 'Gender', choices = levels(linpreds$gender)),
            selectInput('age_2', 'Age', choices = levels(linpreds$age)),
            selectInput('job_2', 'Former occupation', choices = levels(linpreds$job)),
            h4("Intra-party behavior"),    
            selectInput('conference_2', 'Behavior at party conference', choices = levels(linpreds$conference)),
            selectInput('parliament_2', 'Voting behavior in parliament', choices = levels(linpreds$parliament)),
            selectInput('critique_2', 'Intra-party critique', choices = levels(linpreds$critique)),
            selectInput('reform_2', 'Clarity of reform proposals', choices = levels(linpreds$reform)),
            h4("Party position"), 
            selectInput('dist_2', 'Ideological distance', choices = levels(linpreds$dist)),
            h4("Party's characteristic"), 
            selectInput('role_2', 'Party role', choices = levels(linpreds$role)),
            h4("Respondent's characteristic"), 
            selectInput('respondent_educ_2', "Respondent's education", choices = levels(linpreds$respondent_educ)),
            actionButton("reset_2", "Reset")
            )
     ),
     column(5, h2("Predicted vote probabilities"),
            plotOutput("plot"),
            tableOutput("table")
     )
   ),
 )
  )
)


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
                        respondent_educ == input$respondent_educ_1 &
                        dist == input$dist_1],
                   linpreds[role == input$role_2 &
                              conference == input$conference_2 &
                              parliament == input$parliament_2 &
                              critique == input$critique_2 &
                              reform == input$reform_2 &
                              gender == input$gender_2 &
                              age == input$age_2 &
                              job == input$job_2 &
                              respondent_educ == input$respondent_educ_2 &
                              dist == input$dist_2]
    )
    
    compute_probs_one_vs_second <- function(x) {
      prob.first <- exp(x[1])/(sum(exp(x)))
      return(c(prob.first, 1-prob.first))
    }
    
    cases <- cases[,prob:=compute_probs_one_vs_second(mean)]
    cases <- cases[,CI1:=compute_probs_one_vs_second(lower)]
    cases <- cases[,CI2:=compute_probs_one_vs_second(upper)]
    cases$mean <- NULL
    cases$lower <- NULL
    cases$upper <- NULL
    
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
                            respondent_educ == input$respondent_educ_1 &
                            dist == input$dist_1],
                 linpreds[role == input$role_2 &
                            conference == input$conference_2 &
                            parliament == input$parliament_2 &
                            critique == input$critique_2 &
                            reform == input$reform_2 &
                            gender == input$gender_2 &
                            age == input$age_2 &
                            job == input$job_2 &
                            respondent_educ == input$respondent_educ_2 &
                            dist == input$dist_2]
    )
  
    compute_probs_one_vs_second <- function(x) {
      prob.first <- exp(x[1])/(sum(exp(x)))
      return(c(prob.first, 1-prob.first))
    }
  
   cases <- cases[,prob:=compute_probs_one_vs_second(mean)]
   cases <- cases[,CI1:=compute_probs_one_vs_second(lower)]
   cases <- cases[,CI2:=compute_probs_one_vs_second(upper)]
   cases$mean <- NULL
   cases$lower <- NULL
   cases$upper <- NULL
   
    cases[, .(Candidate = factor(c(1,2)), Prob = prob*100, CI1 = CI1*100, CI2 = CI2*100)]
   })
  
}


shinyApp(ui, server)