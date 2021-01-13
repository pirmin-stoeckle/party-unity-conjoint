shinyServer(function(input, output) {
  
  observeEvent(input$reset_1, {
    reset("scenario_1")
  })
  observeEvent(input$reset_2, {
    reset("scenario_2")
  })
  
  output$plot <- renderPlot({
    load(file = "./data/linpreds.RData")
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
    load(file = "./data/linpreds.RData")
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
  
})
