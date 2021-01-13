shinyUI(fluidPage(
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
))
