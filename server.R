library(truncnorm)
library(tmvtnorm)
library(parallel)

shinyServer(
  function(input, output, session) 
  {
    observe(
      {
        updateSliderInput(session,"n_pair", max = round(floor(input$n_socks/2)))
      }
    )
    
    priors = reactive(
      {
        d_total = numeric()
        if (input$total_prior == "pois")
        {
          d_total = rpois(input$n_sims, input$total_lambda)  
        } else {
          total_size = -(input$total_mean^2)/(input$total_mean - (input$total_sd^2))
          d_total = rnbinom(input$n_sims,mu = input$total_mean, size = total_size)
        }
        d_prop = numeric()
        if (input$prop_prior == "beta"){
          d_prop = rbeta(input$n_sims, input$prop_alpha, input$prop_beta)  
        } else if (input$prop_prior == "ttdist"){
            if(n_sim < 1e6){
              d_prop = rtmvt(input$n_sims, mean = input$prop_mut, 
                             sigma = input$prop_sigma, df = input$prop_dof, 
                             lower = 0, upper= 1)
              
            }
          else
            d_prop = 
            unlist(mclapply(1:10, function (x) rtmvt(input$n_sims/10, mean = input$prop_mut, 
                                                     sigma = input$prop_sigma, df = input$prop_dof, 
                                                     lower = 0, upper= 1), mc.cores = 4))
        } else if (input$prop_prior == "tgamma"){
          rtgamma<-function(n,a,b,L,U){
            l.prob<-pgamma(L,a,b)
            u.prob<-pgamma(U,a,b)
            qgamma(runif(n,l.prob,u.prob),a,b)}
          d_prop = rtgamma(input$n_sims, input$prop_shape, input$prop_rate, 0, 1)
        } else {
            if (n_sim < 1e6){
              d_prop = rtruncnorm(input$n_sims,0,1,input$prop_mu,input$prop_sigma)
            }
            else
             d_prop = unlist(mclapply(1:10, function (x) rtruncnorm(input$n_sim/10,0,1,
                                                                    input$prop_mu,input$prop_sigma), mc.cores = 4))
          
        }
        
        data.frame(total = d_total, prop = d_prop)
      }
    )
    
    sims = reactive(
      {
        gen_model = function(d_total, d_prop){
          
          n_pairs = round(floor(d_total/2)*d_prop)
          n_odds = d_total - (n_pairs * 2)
          # Simulating picking out n_picked socks
          sock <- apply(data.frame(n_pairs + n_odds),1,seq_len)
          reps <- apply(data.frame(n_pairs,n_odds),1,function(x) rep(c(2,1),c(x[1],x[2])))
          socks <- mapply(rep, sock, reps)
          picked_socks <- mapply(function(x,y) sample(x, size =  min(input$n_socks, y)), socks, d_total)
          sock_counts = lapply(picked_socks, table)
          if(input$n_sims < 1e3){
            n_picked_pairs <- t(as.data.frame(lapply(sock_counts, function(x) sum(x == 2))))
            n_picked_odds <- t(as.data.frame(lapply(sock_counts, function(x) sum(x == 1))))
          }
          else
            n_picked_pairs <- t(as.data.frame(mclapply(sock_counts, function(x) sum(x == 2), mc.cores = 4)))
            n_picked_odds <- t(as.data.frame(mclapply(sock_counts, function(x) sum(x == 1), mc.cores = 4)))
          return(data.frame(n_picked_pairs,n_picked_odds))
        }
        
        gen_model(priors()[,1],priors()[,2])
      }
    )
    
    posterior = reactive(
      {
        input_n_odds <- input$n_socks-input$n_pair*2
        priors()[sims()[,1]==input$n_pair & sims()[,2] == input_n_odds,]    
      }
    )
    output$med_total_socks <- renderText({paste("The median value of the total number of sock:",round(median(posterior()[,1])))})
    output$mean_total_socks <- renderText({paste("The mean value of the total number of sock:",round(mean(posterior()[,1])))})
    output$cred95_total_socks <- renderText({
      cred = round(quantile(posterior()[,1], probs = c(0.025, 0.975)))
      paste("The 95% credible interval for the total number of sock:",cred[1], "-",cred[2])})
    
    output$med_prop_pairs <- renderText({paste("The median value of the proportion of socks that are paired:",round(median(posterior()[,2]),2))})
    output$mean_prop_pairs <- renderText({paste("The mean value of the propotion of socks that are paired:",round(mean(posterior()[,2]),2))})
    output$cred95_prop_pairs <- renderText({
      cred = round(quantile(posterior()[,2], probs = c(0.025, 0.975)),2)
      paste("The 95% credible interval for the propotion of socks that are paired:",cred[1], "-",cred[2])})
      
    output$total_plot = renderPlot(
      {
        par(mar=c(4,4,4,0.1))
        hist(posterior()[,1], freq=FALSE, main="Posterior on total_socks", 
             breaks = 20, yaxt='n', 
             xlim = c(0,100), xlab = "Number of socks")
        lines(density( priors()$total ),col='green',lwd=3)
        legend("topleft", legend = c('prior'), col='green',lwd=3)
        if(input$summaryOn){
          total_posterior_mean = mean(posterior()[,1])
          total_posterior_median = median(posterior()[,1])
          abline(v=total_posterior_mean, col='yellow',lwd=2)
          abline(v= total_posterior_median, col='orange',lwd=2)
          legend("topleft", legend = c('prior', 'mean', 'median'), 
                 col=c('green','yellow','orange'),lwd=c(3,2,2))
        }
        if(input$trueOn){
          abline(v=45, col='red',lwd=3)
          legend("topright", legend = c('True Value'), col='red',lwd=3)
        }
      }
    )
    
    output$prop_plot = renderPlot(
      {
        par(mar=c(4,4,4,0.1))
        hist(posterior()[,2], freq=FALSE, main="Posterior on prop_pairs",
             breaks = 20, yaxt='n', 
             xlim = c(0,1), xlab = "Proportion of socks in pair")
        lines(density( priors()$prop ),col='blue',lwd=3)
        legend("topleft", legend = c('prior'), col='blue',lwd=3)
        if(input$summaryOn){
          prop_posterior_mean = mean(posterior()[,2])
          prop_posterior_median = median(posterior()[,2])
          abline(v=prop_posterior_mean, col='yellow',lwd=2)
          abline(v=prop_posterior_median, col='orange',lwd=2)
          legend("topleft", legend = c('prior', 'mean', 'median'), 
                 col=c('green','yellow','orange'),lwd=c(3,2,2))
        }
        if(input$trueOn){
          abline(v=21*2/45, col='red',lwd=3)
          legend("topright", legend = c('True Value'), col='red',lwd=3)
        }
      }
    )
  }
)
