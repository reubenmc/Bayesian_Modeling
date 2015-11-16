library(shiny)

total_priors = c("Poisson" = "pois", "Negative Binomial" = "nbinom")
prop_priors = c("Beta" = "beta", "Truncated Normal"="tnorm", 
                "Truncated t" = "ttdist", "Truncated Gamma" = "tgamma")

shinyUI(
  fluidPage(theme = "bootstrap.css",
    titlePanel(
      "Put a Sock in It"
    ),
    
    fluidRow(
      column(4,
          checkboxInput("plotOn", "Plots"),
          checkboxInput("summaryOn", "Summary"),
          checkboxInput("trueOn", "Show True Value"),
      numericInput("n_sims", h4("Simulations:"),value = 10000, min = 10000, step = 1),
      hr(),
      h4("Observations:"),
      sliderInput("n_socks", "Number of socks drawn:", min=1, max=30, value=11, step=1),
      sliderInput("n_pair", "Number of socks paired:", min=0, max=15, 
                  value=1, step=1)
      ),
   
      column(4, 
      h3("Number of Socks"),
      selectInput("total_prior", "Prior", total_priors),
      hr(),
      h4("Hyperparameters:"),
      conditionalPanel(
        condition="input.total_prior == 'pois'",
        sliderInput("total_lambda",HTML("&lambda;"), 
                    value = 20, min=1, max=25)
      ),
      conditionalPanel(
        condition="input.total_prior == 'nbinom'",
        numericInput("total_mean",HTML("mean"), value = 30, min=1, max=100),
        numericInput("total_sd",HTML("sd"), value = 15, min=0, max=30)
    
        )
      ),
      
      column(4,
      h3("Proportion of Paired Socks"),
      selectInput("prop_prior", "Prior", prop_priors),
      hr(),
      h4("Hyperparameters:"),
      conditionalPanel(
        condition="input.prop_prior == 'beta'",
        numericInput("prop_alpha",HTML("&alpha;"), 
                     value = 15 , min=0, max=NA),
        numericInput("prop_beta",HTML("&beta;"), 
                     value = 2, min=0, max=NA)
      ),
      conditionalPanel(
        condition="input.prop_prior == 'tnorm'",
        numericInput("prop_mu",HTML("&mu;"), value = 0.75, min=0, max=1),
        numericInput("prop_sigma",HTML("&sigma;"), value = 0.1, min=0, max=20)
      ),
    conditionalPanel(
      condition="input.prop_prior == 'ttdist'",
      numericInput("prop_dof",HTML("Degrees of Freedom"), 
                   value = 1, min=1, max=20),
      numericInput("prop_mut",HTML("&mu;"), 
                   value = 0.75, min=0, max=1),
      numericInput("prop_sigmat",HTML("&sigma;"), 
                   value = 0.1, min=0, max=20)
    ),
  conditionalPanel(
    condition="input.prop_prior == 'tgamma'",
    numericInput("prop_shape",HTML("Shape"), 
                 value = 3, min=1, max=20),
    numericInput("prop_rate",HTML("Rate"), value = 4, min=1, max=20)
    )
    )
  ),
    mainPanel(
      h4("Results:"),
      conditionalPanel(
        condition="input.plotOn",
        plotOutput("total_plot"),
        br(),
        plotOutput("prop_plot")
      ),
      conditionalPanel(
        condition="input.summaryOn",
        tabPanel("Summary",
                 textOutput("med_total_socks"),
                 textOutput("mean_total_socks"),
                 textOutput("cred95_total_socks"),
                 br(),
                 textOutput("med_prop_pairs"),
                 textOutput("mean_prop_pairs"),
                 textOutput("cred95_prop_pairs")
                )
      )
    )
  )
)
