#*******************************
# Multipage Power Analysis App ----
#*******************************

source("power_analysis_functions.R")

options(shiny.host = '0.0.0.0')
options(shiny.port = 5000)

# Required Libraries ----

suppressPackageStartupMessages({
  library(shiny)
  library(shiny.router)
  library(ggplot2)
  library(plotly)
  library(ggthemes)
  library(shinythemes)
})

# Read csv's ----

all_ICC_samp_calc_df <- read.csv("data/all_ICC_samp_calc_df.csv", row.names=1)

all_wp_anova_df <- read.csv("data/all_wp_anova_df.csv", row.names=1)

all_wp_correlation_df <- read.csv("data/all_wp_correlation_df.csv", row.names=1)

all_wp_t_df <- read.csv("data/all_wp_t_df.csv", row.names=1)

all_wp_t_df_in <- read.csv("data/all_wp_t_df_in.csv", row.names=1)

all_bin_class_samp_calc_df <- read.csv("data/all_bin_class_samp_calc_df.csv", row.names=1)

all_regression_samp_calc_df <- read.csv("data/all_regression_samp_calc_df.csv", row.names=1)

type_of_t_test <- "paired"
type_of_t_test_in <- "two.sample"


# Pages Layout ----

home_page <- div(
  
  fluidRow(column(width=2),
           column(width = 8,
                  br(), br(),
                  wellPanel( h2(align='center', "Power Analysis Dashboard",
                                style='font-family: Raleway;'
                                ),
                             tags$p(align="center", tags$strong("jmspwr"), "is a dashboard intended to be used to estimate the sample size, power, and/ 
                          or expected effect size per research project. 
                          Current analyses that are supported are ",
                                    tags$strong("ANOVAs,"), 
                                    tags$strong("T-tests,"),
                                    tags$strong("Correlations,"),
                                    tags$strong("ICCs,"),
                                    tags$strong("Binary Classification,"),
                                    tags$strong("Linear Regression,"),
                                    tags$strong("Linear Mixed Models"),
                                    "."),
                             tags$br(),
                             h4(align='center', "Contact Details"),
                             tags$p(align="center", "Questions and or requests can be mailed to:", tags$strong("James Twose (contact@jamestwose.com)."),
                                    ),
                             style='font-family: Raleway;
                                    align: center;'
                             )),
           column(width=2),
           tags$br(),
           tags$br()),
  
  fluidRow(br()),
  
  fluidRow(align = 'center',
           column(4, a(href = route_link("machinelearningpoweranalysis"),
                       actionButton('btn1', "Machine Learning Power Analysis",
                                    width = '85%', 
                                    style='font-size:22px;
                                          font-family: Raleway;',
                                    ))),
           column(4, a(href = route_link("glmmpoweranalysis"),
                       actionButton('btn2', "GLMM Power Analysis", 
                                    width = '85%',
                                    style='font-size:22px;
                                          font-family: Raleway;',
                                    ))),
           column(4, a(href = route_link("frequentiststatisticspoweranalysis"),
                       actionButton('btn3',
                                    "Frequentist Statistics Power Analysis", 
                                    width = '85%', 
                                    style='font-size:22px;
                                          font-family: Raleway;',
                                    ))),
           ),
  fluidRow(tags$br(),
           div())
  
)

mlpl_page <- div(
  
  br(),
  fluidRow(column(2),
           column(8, h2(align = 'center', "Machine Learning power analysis",
                        style="font-family: Raleway;"
                        ))),
  br(),
  navbarPage("Machine Learning power analysis",
             theme = shinytheme("flatly"),
             navbarMenu("Choose  option to calculate effect and sample sizes",
                        tabPanel("Binary Classification",
                                 fluidRow(column(2),
                                          column(8, h2(align = 'center',
                                                       "Binary Classification"))),
                                 #br(),
                                 fluidRow(column(width=2),
                                          column(width=8,
                                                 br(), br(),
                                                 wellPanel(h3(align='center',
                                                              "Input ranges and parameter explanation"),
                                                           tags$br(),
                                                           tags$ol(
                                                             tags$li(tags$strong("params:",
                                                            #  style=paste('color:', highlight_color)
                                                             ),
                                                                     "the number of parameters of the ML model, default setting is 1 parameter.
                                           (min = 1, max = 10)"),
                                                             tags$br(),
                                                             tags$li(tags$strong("r_squared:",
                                                            #  style=paste('color:', highlight_color)
                                                             ),
                                                                     "the maximum expected R-squared. 
                                           The default value is set to 0.3. (min = 0, max = 1)"),
                                                             tags$br(),
                                                             tags$li(tags$strong("prevalence:",
                                                            #  style=paste('color:', highlight_color)
                                                             ),
                                                                     "Prevalence is a measure
                                                       of disease that allows us to determine 
                                                       a person's likelihood of having a disease.
                                                       Therefore, the number of prevalent cases
                                                       is the total number of cases of disease
                                                       existing in a population. Usually from
                                                       0.1 to 0.5, default value is set to 0.3.
                                           (min = 0.1, max = 0.5)"),
                                                             tags$br(),
                                                             tags$li(tags$strong("Shrinkage:",
                                                            #  style=paste('color:', highlight_color)
                                                             ),
                                                                     "Shrinkage (also known as penalisation or regularisation) methods deal 
                                         with the problem of overfitting by reducing the variability in the developed
                                         models predictions such that extreme predictions (eg, predicted probabilities
                                         close to 0 or 1) are pulled back toward the overall average.
                                         Default value is 0.9.")
                                                           ),
                                                           
                                                           h5(tags$strong("References:",
                                                          #  style='color: black;'
                                                           ),
                                                              align='left'),
                                                           tags$ul(
                                                             tags$li(
                                                               "Riley, R. D., Snell, K. I., Ensor, J., Burke, D. L., Harrell Jr, F. E., Moons, K. G., & Collins, G. S. (2019). 
                           Minimum sample size for developing a multivariable prediction model: PART II‐binary and time‐to‐event outcomes. 
                           Statistics in medicine, 38(7), 1276-1296.")),
                                                           h5(tags$strong("Hold-out set:",
                                                          #  style='color: black;'
                                                           ),
                                                              "add 0.3x to whatever the final sample size is for 
                                      the hold out"))
                                          ),
                                          column(width=2),
                                          tags$br(),
                                          br()
                                 ),
                                 br(),
                                 tabsetPanel(id="mlclasstab",
                                             tabPanel("Calculate Sample Size",
                                                      sidebarLayout(
                                                        sidebarPanel(
                                                          numericInput("params", "params", value = 1, min = 1, max = 10, step = 1),
                                                          numericInput("r_squared", "r_squared", value = 0.3, min = 0, max = 1, step = 0.05),
                                                          numericInput("prevalence", "prevalence", value = 0.3, min = 0.2, max = 0.5, step = 0.05),
                                                        ),
                                                        
                                                        mainPanel(
                                                          tableOutput("binarytable"),
                                                          br(),
                                                          plotlyOutput("binaryPlot"),
                                                          br(),
                                                          fluidRow(column(12, align='center',textOutput("tef4y"))),
                                                          br(),
                                                          br()
                                                          
                                                        )) )
                                 ),
                        ),
                        
                        tabPanel("Linear Regression",
                                 fluidRow(column(2),
                                          column(8, h2(align = 'center',
                                                       "Linear Regression"))),
                                 br(),
                                 fluidRow(column(width=2),
                                          column(width = 8,
                                                 br(), br(),
                                                 wellPanel(h3(align='center',
                                                              "Input ranges and parameter explanation"),
                                                           tags$br(),
                                                           tags$ol(
                                                             tags$li(tags$strong("params:",
                                                            #  style=paste('color:', highlight_color)
                                                             ),
                                                                     "the number of parameters of the ML model, 
                                             the default setting is 1 parameter.
                                             (min = 1, max = 10)"),
                                                             tags$br(),
                                                             tags$li(tags$strong("r_squared:",
                                                            #  style=paste('color:', highlight_color)
                                                             ),
                                                                     "the maximum expected R-squared. 
                                             The default value is set to 0.3. (min = 0, max = 1)"),
                                                             tags$br(),
                                                             tags$li(tags$strong("sd:",
                                                            #  style=paste('color:', highlight_color)
                                                             ),
                                                                     "Standard deviation
                                             in the target population (assuming the data are scaled around 0).
                                             Default value is set to 0.6. (min = 0.1, max = 2.6)"),
                                                             
                                                             tags$br(),
                                                             tags$li(tags$strong("Shrinkage:",
                                                            #  style=paste('color:', highlight_color)
                                                             ),
                                                                     "Shrinkage (also known as penalisation or regularisation) methods deal 
                                           with the problem of overfitting by reducing the variability in the developed
                                           models predictions such that extreme predictions (eg, predicted probabilities
                                           close to 0 or 1) are pulled back toward the overall average.
                                           Default value is 0.9.")),
                                                           h5(tags$strong("References:",
                                                          #  style='color: black;'
                                                           ),
                                                              align='left'),
                                                           tags$ul(
                                                             tags$li(
                                                               "Riley, R. D., Snell, K. I., Ensor, J., Burke, D. L., Harrell Jr, F. E., Moons, K. G., & Collins, G. S. (2019). 
                           Minimum sample size for developing a multivariable prediction model: PART II‐binary and time‐to‐event outcomes. 
                           Statistics in medicine, 38(7), 1276-1296.")),
                                                           h5(tags$strong("Hold-out set:",
                                                          #  style='color: black;'
                                                           ),
                                                              "add 0.3x to whatever the final sample size is for 
                                        the hold out")),
                                                 column(width=2),
                                                 tags$br(),
                                                 br())
                                 ),
                                 br(),
                                 tabsetPanel(id="mlregtab",
                                             tabPanel("Calculate Sample Size",
                                                      sidebarLayout(
                                                        sidebarPanel(
                                                          numericInput("regparams", "params", value = 1, min = 1, max = 10, step = 1),
                                                          numericInput("regr_squared", "r_squared", value = 0.3, min = 0, max = 1, step = 0.05),
                                                          numericInput("regsd", "sd", value = 0.6, min = 0.1, max = 2.6, step = 0.5),
                                                        ),
                                                        
                                                        mainPanel(
                                                          tableOutput("regressiontable"),
                                                          br(),
                                                          plotlyOutput("regressionPlot"),
                                                          br(),
                                                          fluidRow(column(12, align='center',textOutput("tef5g67g56y4y"))),
                                                          br(),
                                                          br()
                                                        )) )
                                 ),
                                 
                        ),
                        
             ),
             navbarMenu("Navigate to other pages", 
                        tabPanel(a("Home Page", href = route_link("/"))),
                        tabPanel(a("Frequentist Statistics Power Analysis",
                                   href = route_link("frequentiststatisticspoweranalysis"))),
                        tabPanel(a("GLMM Power Analysis",
                                   href = route_link("glmmpoweranalysis")))
                        )
  )
  
)

glmm_page <- div(
  
  br() ,
  fluidRow(column(2),
           column(8, h2(align = 'center',
                        "Generalized Linear Mixed Models power analysis",
                        style="font-family: Raleway;"
                        ))),
  br(),
  
  navbarPage(theme = shinytheme("flatly"),
             tabPanel("Generalized Linear Mixed Models"),
             tabPanel("Generalized Linear Mixed Models",
                      br(),
                      
                      fluidRow(column(width=2),
                               column(width = 8,
                                      br(), br(),
                                      wellPanel(h4(align='center',
                                                   "Compute an approximated sample size using",
                                                   tags$a("sjstats", href="https://strengejacke.github.io/sjstats/index.html"),
                                                   "for linear mixed models (two-level-designs), based on power-calculation
                                  for standard design and adjusted for design effect for 2-level-designs."),
                                                tags$br(),
                                                tags$ul(
                                                  tags$li(tags$strong("effect_size:",
                                                  ),
                                                          "refers to Cohen's f adjusted for the design effect of two-level-designs,
                                         where:
                                          (Default value = 0.30, min = 0, max = 1)"),
                                                  tags$br(),
                                                  tags$li(tags$strong("k:",
                                                  # style=paste('color:', highlight_color)
                                                  ),
                                                          "refers to the number of clusters, e.g., if there are 50 users and
                                         each of them has multiple samples, then k=50
                                         (Default value = 50, min = 2, max = 2000)"),
                                                  tags$br(),
                                                  tags$li(tags$strong("alpha:",
                                                  # style=paste('color:', highlight_color)
                                                  ),
                                                          "The desired alpha for hypothesis testing.
                                                                        (Default value = 0.05, min = 0.01, max = 0.1)"),
                                                  tags$br(),
                                                  tags$li(tags$strong("icc:",
                                                  # style=paste('color:', highlight_color)
                                                  ),
                                                          "the ratio between the random effect variance  with
                                         the total variance given by the sum of the random effect variance and the
                                         residual variance .",
                                                          tags$ul(
                                                            tags$li("Low values of ICC denotes that the observations within clusters are no more similar than observations from different clusters."),
                                                            tags$li("It is suggested to provide multiple scenarios for different ICC as the ICC is not known a priori."),
                                                            tags$li("the higher the expected ICC, the more n sample are required. Such trend will stop after a certain value of ICC.")
                                                          )),
                                                  tags$br(),
                                                  tags$li(tags$strong("power:",
                                                  # style=paste('color:', highlight_color)
                                                  ),
                                                          "The desired power of the hypothesis test.
                                                                        (Default value = 0.8, min = 0.1, max = 0.8)")),
                                                h5(tags$strong("References:",
                                                # style='color: black;'
                                                ),
                                                   align='left'),
                                                tags$ul(
                                                  tags$li(
                                                    " Cohen, J. (1988). Statistical power analysis for the behavioral sciences
                                   (2nd ed.). Hillsdale,NJ: Lawrence Erlbaum.."),
                                                  br(),
                                                  tags$li("Snijders TAB. 2005. Power and Sample Size in Multilevel Linear Models.
                                 In: Everitt BS, Howell DC (Hrsg.). Encyclopedia of Statistics in Behavioral Science.
                                 Chichester, UK: John Wiley and Sons, Ltd.",
                                                          tags$a( "doi: 10.1002/0470013192.bsa492",
                                                                  href="https://onlinelibrary.wiley.com/doi/10.1002/0470013192.bsa492")))
                                      ),
                                      column(width=2),
                                      tags$br(),
                                      br()
                               )),
                      br(),
                      tabsetPanel(id="glmmtab",
                                  tabPanel("Calculate Sample Size",
                                           sidebarLayout(
                                             sidebarPanel(
                                               numericInput("effectsizeLMM", "effect_size", value = 0.35, min = 0, max = 1, step = 0.05),
                                               numericInput("iccLMM", "icc", value = 0.05, min = 0, max = 1, step = 0.05),
                                               numericInput("kclusterLMM", "k_cluster", value = 3, min = 2, max = 2000, step = 1),
                                               numericInput("alphaLMM", "alpha", value = 0.05, min = 0.01, max = 0.05, step = 0.05),
                                               numericInput("powerLMM", "power", value = 0.8, min = 0.1, max = 0.8, step = 0.05),
                                             ),
                                             
                                             mainPanel(
                                               tableOutput("glmmtable"),
                                               br(),
                                               plotlyOutput("glmmdistPlotki"),
                                               br(),
                                               fluidRow(column(12, align='center',textOutput("te454y"))),
                                               br(),
                                               br()
                                             )) )
                      ),
             ),
             navbarMenu("Navigate to other pages", 
                        tabPanel(a("Home Page", href = route_link("/"))),
                        tabPanel(a("Machine Learning Power Analysis", href = route_link("machinelearningpoweranalysis"))),
                        # tabPanel(a("Frequentist Statistics Cheatsheet", href = route_link("poweranalysis"))),
                        tabPanel(a("Frequentist Statistics Power Analysis",
                                   href = route_link("frequentiststatisticspoweranalysis"))))),
  
  
  
)

fspa_page <- div(
  
  br() ,
  fluidRow(column(2),
           column(8, h2(align = 'center',
                        "Frequentist statistics power analysis",
                        style="font-family: Raleway;"
                        ))),
  br(),
  navbarPage("Frequentist statistics power analysis",
             theme = shinytheme("flatly"),
             navbarMenu("Choose  option to calculate effect and sample sizes", 
                        
                        tabPanel("Intra Class Correlation",
                                 fluidRow(column(2),
                                          column(8, h2(align = 'center',
                                                       "Intra Class Correlation"))),
                                 
                                 fluidRow(column(width=2),
                                          column(width = 8,
                                                 br(), br(),
                                                 wellPanel(h3(align='center',
                                                              "Input ranges and parameter explanation"),
                                                           tags$br(),
                                                           h4(align='center',
                                                              "ICC value based on an alternative hypothesis rho", 
                                                              tags$em(
                                                                tags$li("poor: rho<0.5,"), 
                                                                tags$li("moderate: 0.5<=rho<0.75,"), 
                                                                tags$li("good: 0.75<=rho<0.9,"),
                                                                tags$li("excellent: 0.9<=rho"))),
                                                           tags$br(),
                                                           tags$ul(
                                                             tags$li(tags$strong("p:",
                                                            #  style=paste('color:', highlight_color)
                                                             ),
                                                                     "The hypothesized value of p. Hypothesized based
                                                                      on previous data, or experience. 
                                                                     (Default value = 0.75, min = 0, max = 1)"),
                                                             tags$br(),
                                                             tags$li(tags$strong("p0:",
                                                            #  style=paste('color:', highlight_color)
                                                             ),
                                                                     "The null hypothesis value of p. 
                                                                     (Default value = 0.30, min = 0, max = 1)"),
                                                             tags$br(),
                                                             tags$li(tags$strong("k:",
                                                            #  style=paste('color:', highlight_color)
                                                             ),
                                                                     "The number of ratings of each subject. 
                                                                     (Default value = 2, min = 0, max = 100)"),
                                                             tags$br(),
                                                             tags$li(tags$strong("alpha:",
                                                            #  style=paste('color:', highlight_color)
                                                             ),
                                                                     "The desired alpha for hypothesis testing.
                                                                     (Default value = 0.05, min = 0.01, max = 0.1)"),
                                                             tags$br(),
                                                             tags$li(tags$strong("tails:",
                                                            #  style=paste('color:', highlight_color)
                                                             ),
                                                                     "The number of trails for hypothesis test. 
                                                                     (Default value = 2, min = 1, max = 2)"),
                                                             tags$br(),
                                                             tags$li(tags$strong("power:",
                                                            #  style=paste('color:', highlight_color)
                                                             ),
                                                                     "The desired power of the hypothesis test. 
                                                                     (Default value = 0.8, min = 0.1, max = 0.8)")),
                                                           h5(tags$strong("References:",
                                                          #  style='color: black;'
                                                           ),
                                                              align='left'),
                                                           tags$ul(
                                                             tags$li(
                                                               "Koo, T. K., & Li, M. Y. (2016). A Guideline of
                                                              Selecting and Reporting Intraclass Correlation 
                                                              Coefficients for Reliability Research. Journal 
                                                              of chiropractic medicine, 15(2),
                                                              155\u2013163.",
                                                               tags$a("https://doi.org/10.1016/j.jcm.2016.02.012",
                                                                      href="https://doi.org/10.1016/j.jcm.2016.02.012")),
                                                             br(),
                                                             tags$li("Zou, G. Y. (2012). Sample size formulas for estimating 
                                                                     intraclass correlation coefficients with precision and assurance.
                                                                     Statistics in medicine, 31(29), 3972-3981.",
                                                                     tags$a("https://doi.org/10.1002/sim.5466",
                                                                            href="https://doi.org/10.1002/sim.5466")))
                                                 ),
                                                 column(width=2),
                                                 tags$br(),
                                                 br()
                                          )),
                                 br(),
                                 tabsetPanel(id="ICCtab",
                                             tabPanel("Calculate Sample Size",
                                                      sidebarLayout(
                                                        sidebarPanel(
                                                          numericInput("p", "rho", value = 0.75, min = 0, max = 1, step = 0.05),
                                                          numericInput("p0", "rho0", value = 0.3, min = 0, max = 1, step = 0.05),
                                                          numericInput("k", "k", value = 2, min = 0, max = 100, step = 0.05),
                                                          numericInput("alpha", "alpha", value = 0.05, min = 0.01, max = 0.05, step = 0.05),
                                                          numericInput("tails", "tails", value = 2, min = 1, max = 2, step = 0.05),
                                                          numericInput("powericc", "power", value = 0.8, min = 0.1, max = 0.8, step = 0.05)),
                                                        
                                                        mainPanel(
                                                          tableOutput("table"),
                                                          br(),
                                                          plotlyOutput("distPlotki"),
                                                          br(),
                                                          fluidRow(column(12, align='center',textOutput("icclegend"))),
                                                          br(),
                                                          br()
                                                          
                                                        )) ),
                                             tabPanel("Calculate Power",
                                                      sidebarLayout(
                                                        sidebarPanel(
                                                          numericInput("ppower", "rho", value = 0.75, min = 0, max = 1, step = 0.05),
                                                          numericInput("p0power", "rho0", value = 0.3, min = 0, max = 1, step = 0.05),
                                                          numericInput("kpower", "k", value = 2, min = 0, max = 100, step = 0.05),
                                                          numericInput("alphapower", "alpha", value = 0.05, min = 0.01, max = 0.05, step = 0.05),
                                                          numericInput("tailspower", "tails", value = 2, min = 1, max = 2, step = 0.05),
                                                          numericInput("npower", "N", value = 60, min = 2, max = 2000, step = 1),
                                                          numericInput("despower", "desiredPower", value = 0.8, min = 0.1, max = 0.8, step = 0.05)),
                                                        
                                                        mainPanel(
                                                          tableOutput("tablepowerICC"),
                                                          br(),
                                                          plotlyOutput("distPlotpowerICC"),
                                                          br(),
                                                          fluidRow(column(12, align='center',textOutput("icclegendpower"))),
                                                          br(),
                                                          br()
                                                          
                                                        )) ),
                                             tabPanel("Calculate Effect Size",
                                                      sidebarLayout(
                                                        sidebarPanel(
                                                          numericInput("pefsi", "rho", value = 0.75, min = 0, max = 1, step = 0.05),
                                                          numericInput("kefsi", "k", value = 2, min = 0, max = 100, step = 0.05),
                                                          numericInput("alphaefsi", "alpha", value = 0.05, min = 0.01, max = 0.05, step = 0.05),
                                                          numericInput("tailsefsi", "tails", value = 2, min = 1, max = 2, step = 0.05),
                                                          numericInput("nefsi", "N", value = 60, min = 2, max = 2000, step = 1),
                                                          numericInput("desiredefsi", "power", value = 0.8, min = 0.1, max = 0.8, step = 0.05)),
                                                        
                                                        mainPanel(
                                                          tableOutput("tableefsiICC"),
                                                          br(),
                                                          plotlyOutput("distPlotefsiICC"),
                                                          br(),
                                                          fluidRow(column(12, align='center',textOutput("icclegendefsi"))),
                                                          br(),
                                                          br()
                                                          
                                                        )) )
                                 ),
                        ),
                        tabPanel("One-way ANOVA",
                                 fluidRow(column(2),
                                          column(8, h2(align = 'center',
                                                       "One-way ANOVA"))),
                                 fluidRow(column(width=2),
                                          column(width = 8,
                                                 br(), br(),
                                                 wellPanel(h3(align='center',
                                                              "Input ranges and parameter explanation"),
                                                           tags$br(),
                                                           h4(align='center',
                                                              "One way ANOVA effect size based on F-value",
                                                              tags$em(tags$li("small=0.1,"),
                                                                      tags$li("medium=0.25,"),
                                                                      tags$li("large=0.4")
                                                              )),
                                                           tags$br(),
                                                           tags$ul(
                                                             tags$li(tags$strong("f-value:",
                                                            #  style=paste('color:', highlight_color)
                                                             ),
                                                                     "The f-value. (Default value = 0.75, min = 0, max = 1)"),
                                                             tags$br(),
                                                             tags$li(tags$strong("alpha:",
                                                            #  style=paste('color:', highlight_color)
                                                             ),
                                                                     "The desired alpha for hypothesis testing.
                                                                     (Default value = 0.05, min = 0.01, max = 0.1)"),
                                                             tags$br(),
                                                             tags$li(tags$strong("power:",
                                                            #  style=paste('color:', highlight_color)
                                                             ),
                                                                     "The desired power of the hypothesis test. 
                                                                     (Default value = 0.8, min = 0.1, max = 0.8)"),
                                                             tags$br(),
                                                             tags$li(tags$strong("k:",
                                                            #  style=paste('color:', highlight_color)
                                                             ),
                                                                     "Number of groups, is pre-defined to 3.")),
                                                           h5(tags$strong("References:",
                                                          #  style='color: black;'
                                                           ),
                                                              align='left'),
                                                           tags$ul(
                                                             tags$li(
                                                               "Yujiao Mai and Zhiyong Zhang.Statistical Power Analysis
                                                               for One-way ANOVA with Binary or Count Data.",
                                                               tags$a("http://psychstat.org/anova",
                                                                      href="http://psychstat.org/anova")))
                                                 )),
                                          column(width=2),
                                          tags$br(),
                                          br()
                                 ),
                                 br(),
                                 tabsetPanel(id="ANOVAtab",
                                             tabPanel("Calculate Sample Size",
                                                      sidebarLayout(
                                                        sidebarPanel(
                                                          numericInput("fvalue", "f-value", value = 0.25, min = 0, max = 1, step = 0.05),
                                                          numericInput("kclusteranova", "k", value = 2, min = 2, max = 100, step = 1),
                                                          numericInput("alphaanova", "alpha", value = 0.05, min = 0.01, max = 0.05, step = 0.05),
                                                          numericInput("poweranova", "power", value = 0.8, min = 0.1, max = 0.8, step = 0.05),
                                                        ),
                                                        
                                                        mainPanel(
                                                          tableOutput("tableanova"),
                                                          br(),
                                                          plotlyOutput("anovaPlot"),
                                                          br(),
                                                          fluidRow(column(12, align='center',textOutput("ty"))),
                                                          br(),
                                                          br()
                                                          
                                                        )) ),
                                             tabPanel("Calculate Power", 
                                                      sidebarLayout(
                                                        sidebarPanel(
                                                          numericInput("fvaluepower", "f-value", value = 0.25, min = 0, max = 1, step = 0.05),
                                                          numericInput("kclusteranovapower", "k", value = 2, min = 2, max = 100, step = 1),
                                                          numericInput("alphaanovapower", "alpha", value = 0.05, min = 0.01, max = 0.05, step = 0.05),
                                                          numericInput("nanovapower", "N", value = 160, min = 2, max = 2000, step = 1),
                                                        ),
                                                        
                                                        mainPanel(
                                                          tableOutput("tableanovapower"),
                                                          br(),
                                                          fluidRow(column(12, align='center',textOutput("typower"))),
                                                          br(),
                                                          br()
                                                          
                                                        ))),
                                             tabPanel("Calculate Effect size", 
                                                      sidebarLayout(
                                                        sidebarPanel(
                                                          numericInput("anovanefsi", "N", value = 160, min = 2, max = 2000, step = 1),
                                                          numericInput("kclusteranovaefsi", "k", value = 2, min = 2, max = 100, step = 1),
                                                          numericInput("alphaanovaefsi", "alpha", value = 0.05, min = 0.01, max = 0.05, step = 0.05),
                                                          numericInput("poweranovaefsi", "power", value = 0.8, min = 0.1, max = 0.8, step = 0.05),
                                                        ),
                                                        
                                                        mainPanel(
                                                          tableOutput("tableanovaefsi"),
                                                          br(),
                                                          # plotlyOutput("anovaPlotefsi"),
                                                          # br(),
                                                          fluidRow(column(12, align='center',textOutput("tyefsi"))),
                                                          br(),
                                                          br()
                                                          
                                                        )))
                                 )
                        ),
                        tabPanel("Correlations for the whole group",
                                 fluidRow(column(2),
                                          column(8, h2(align = 'center',
                                                       "Correlations for the whole group"))),
                                 fluidRow(column(width=2),
                                          column(width = 8,
                                                 br(), br(),
                                                 wellPanel(h3(align='center',
                                                              "Input ranges and parameter explanation"),
                                                           tags$br(),
                                                           h4(align='center',
                                                              tags$em("Correlations effect size based on r-value",
                                                                      tags$li("small=0.1,"),
                                                                      tags$li("medium=0.3,"),
                                                                      tags$li("large=0.5"))),
                                                           tags$br(),
                                                           tags$ul(
                                                             tags$li(tags$strong("r-value:",
                                                            #  style=paste('color:', highlight_color)
                                                             ),
                                                                     "The r-value. (Default value = 0.25, min = 0, max = 1)"),
                                                             tags$br(),
                                                             tags$li(tags$strong("alpha:",
                                                            #  style=paste('color:', highlight_color)
                                                             ),
                                                                     "The desired alpha for hypothesis testing.
                                                                     (Default value = 0.05, min = 0.01, max = 0.1)"),
                                                             tags$br(),
                                                             tags$li(tags$strong("power:",
                                                            #  style=paste('color:', highlight_color)
                                                             ),
                                                                     "The desired power of the hypothesis test. 
                                                                     (Default value = 0.8, min = 0.1, max = 0.8)")),
                                                           h5(tags$strong("References:",
                                                          #  style='color: black;'
                                                           ),
                                                              align='left'),
                                                           tags$ul(
                                                             tags$li(
                                                               "Zhiyong Zhang and Ke-Hai Yuan. Practical Statistical Power Analysis using WebPower and R. ",
                                                               tags$a("Practical Statistical Power Analysis using WebPower and R",
                                                                      href="https://webpower.psychstat.org/wiki/_media/grant/practical_statistica_interior_for_kindle.pdf")))
                                                 )),
                                          column(width=2),
                                          tags$br(),
                                          br()
                                 ),
                                 br(),
                                 tabsetPanel(id="corrtab",
                                             tabPanel("Calculate Sample Size",
                                                      sidebarLayout(
                                                        sidebarPanel(
                                                          numericInput("rvalue", "r-value", value = 0.25, min = 0, max = 1, step = 0.05),
                                                          numericInput("alphacorr", "alpha", value = 0.05, min = 0.01, max = 0.05, step = 0.05),
                                                          numericInput("powercorr", "power", value = 0.8, min = 0.1, max = 0.8, step = 0.05),
                                                        ),
                                                        
                                                        mainPanel(
                                                          tableOutput("tablecorrelation"),
                                                          br(),
                                                          plotlyOutput("corrPlot"),
                                                          br(),
                                                          fluidRow(column(12, align='center',textOutput("tey"))),
                                                          br(),
                                                          br()
                                                        )) ),
                                             tabPanel("Calculate Power",
                                                      sidebarLayout(
                                                        sidebarPanel(
                                                          numericInput("rvaluepower", "r-value", value = 0.25, min = 0, max = 1, step = 0.05),
                                                          numericInput("alphacorrpower", "alpha", value = 0.05, min = 0.01, max = 0.05, step = 0.05),
                                                        ),
                                                        
                                                        mainPanel(
                                                          tableOutput("tablecorrelationpower"),
                                                          br(),
                                                          fluidRow(column(12, align='center',textOutput("teypower"))),
                                                          br(),
                                                          br()
                                                        ))
                                             ),
                                             tabPanel("Calculate Effect size",
                                                      sidebarLayout(
                                                        sidebarPanel(
                                                          numericInput("alphacorrefsi", "alpha", value = 0.05, min = 0.01, max = 0.05, step = 0.05),
                                                          numericInput("powercorrefsi", "power", value = 0.8, min = 0.1, max = 0.8, step = 0.05),
                                                        ),
                                                        
                                                        mainPanel(
                                                          tableOutput("tablecorrelationefsi"),
                                                          br(),
                                                          fluidRow(column(12, align='center',textOutput("teyefsi"))),
                                                          br(),
                                                          br()
                                                        ))
                                             )
                                 ),
                        ),
                        tabPanel("t-test's",
                                 fluidRow(column(2),
                                          column(8, h2(align = 'center',
                                                       "t-test's"))),
                                 fluidRow(column(width=2),
                                          column(width = 8,
                                                 br(), br(),
                                                 wellPanel(h3(align='center',
                                                              "Input ranges and parameter explanation"),
                                                           tags$br(),
                                                           h4(align='center',
                                                              tags$em("Dependent/ Independent t-test effect size based on d-value",
                                                                      tags$li("small=0.2,"),
                                                                      tags$li("medium=0.5,"),
                                                                      tags$li("large=0.8"))),
                                                           tags$br(),
                                                           tags$ul(
                                                             tags$li(tags$strong("d-value:",
                                                            #  style=paste('color:', highlight_color)
                                                             ),
                                                                     "The d-value. (Default value = 0.25, min = 0, max = 1)"),
                                                             tags$br(),
                                                             tags$li(tags$strong("alpha:",
                                                            #  style=paste('color:', highlight_color)
                                                             ),
                                                                     "The desired alpha for hypothesis testing.
                                                                     (Default value = 0.05, min = 0.01, max = 0.1)"),
                                                             tags$br(),
                                                             tags$li(tags$strong("power:",
                                                            #  style=paste('color:', highlight_color)
                                                             ),
                                                                     "The desired power of the hypothesis test. 
                                                                     (Default value = 0.8, min = 0.1, max = 0.8)")),
                                                           h5(tags$strong("References:",
                                                          #  style='color: black;'
                                                           ),
                                                              align='left'),
                                                           tags$ul(
                                                             tags$li(
                                                               "Zhiyong Zhang and Ke-Hai Yuan. Practical Statistical Power Analysis using WebPower and R. ",
                                                               tags$a("Practical Statistical Power Analysis using WebPower and R",
                                                                      href="https://webpower.psychstat.org/wiki/_media/grant/practical_statistica_interior_for_kindle.pdf")),
                                                             tags$li("Cohen, J. (1988). Statistical power analysis for the behavioral sciences (2nd ed.). Hillsdale,NJ: Lawrence Erlbaum."))
                                                 )),
                                          column(width=2),
                                          tags$br(),
                                          br()
                                 ),
                                 br(),
                                 tabsetPanel(id="ttesttab",
                                             tabPanel("Calculate Sample Size",
                                                      sidebarLayout(
                                                        sidebarPanel(
                                                          radioButtons("ttestType", "t-test type",
                                                                       c("Dependent"="paired",
                                                                         "Independent"="two.sample")
                                                          ),
                                                          numericInput("dvalue", "d-value", value = 0.25, min = 0, max = 1, step = 0.05),
                                                          numericInput("alphattest", "alpha", value = 0.05, min = 0.01, max = 0.05, step = 0.05),
                                                          numericInput("powerttest", "power", value = 0.8, min = 0.1, max = 0.8, step = 0.05),
                                                        ),
                                                        
                                                        mainPanel(
                                                          tableOutput("tablettest"),
                                                          br(),
                                                          plotlyOutput("ttestPlot"),
                                                          br(),
                                                          fluidRow(column(12, align='center',textOutput("tddey"))),
                                                          br(),
                                                          br()
                                                        )) 
                                             ),
                                             tabPanel("Calculate Power", 
                                                      sidebarLayout(
                                                        sidebarPanel(
                                                          radioButtons("ttestType", "t-test type",
                                                                       c("Dependent"="paired",
                                                                         "Independent"="two.sample")
                                                          ),
                                                          numericInput("n1", "n1", value = 30, min = 1, max = 1000, step = 1),
                                                          numericInput("n2", "n2", value = 30, min = 1, max = 1000, step = 1),
                                                          numericInput("dvalue", "d-value", value = 0.25, min = 0, max = 1, step = 0.05),
                                                          numericInput("alphattest", "alpha", value = 0.05, min = 0.01, max = 0.05, step = 0.05),
                                                        ),
                                                        
                                                        mainPanel(
                                                          tableOutput("tablettestpower"),
                                                          br(),
                                                          fluidRow(column(12, align='center',textOutput("tddeypower"))),
                                                          br(),
                                                          br()
                                                        ))
                                                      
                                             ),
                                             tabPanel("Calculate Effect size", 
                                                      sidebarLayout(
                                                        sidebarPanel(
                                                          radioButtons("ttestType", "t-test type",
                                                                       c("Dependent"="paired",
                                                                         "Independent"="two.sample")
                                                          ),
                                                          numericInput("n1", "n1", value = 30, min = 1, max = 1000, step = 1),
                                                          numericInput("n2", "n2", value = 30, min = 1, max = 1000, step = 1),
                                                          numericInput("alphattest", "alpha", value = 0.05, min = 0.01, max = 0.05, step = 0.05),
                                                          numericInput("powerttest", "power", value = 0.8, min = 0.1, max = 0.8, step = 0.05),
                                                        ),
                                                        
                                                        mainPanel(
                                                          tableOutput("tablettestefsi"),
                                                          br(),
                                                          fluidRow(column(12, align='center',textOutput("tddeyefsi"))),
                                                          br(),
                                                          br()
                                                        ))
                                                      
                                                      
                                             )
                                 ),
                                 
                        )),
             navbarMenu("Navigate to other pages", 
                        tabPanel(a("Home Page", href = route_link("/"))),
                        tabPanel(a("Machine Learning Power Analysis", href = route_link("machinelearningpoweranalysis"))),
                        tabPanel(a("GLMM Power Analysis", href = route_link("glmmpoweranalysis"))),
                        )
  )
)

router <- make_router(
  
  route("/", home_page),
  route("machinelearningpoweranalysis", mlpl_page),
  route("glmmpoweranalysis", glmm_page),
  route("frequentiststatisticspoweranalysis", fspa_page)

)

# UI code ----
ui <- fluidPage(
  # shinythemes::themeSelector(),
  includeCSS("www/custom_v2.css"),
  includeCSS("https://fonts.googleapis.com/css2?family=Raleway:wght@400&display=swap"),
  
  fluidRow(
    # column(2, ),
    column(2, align='center', a(href='https://services.jms.rocks',
                                img(src='https://services.jms.rocks/img/logo.png',
                                    align = "center",
                                    style="display: flex;
                                                align-items: center;
                                                margin-left: auto;
                                                margin-right: auto;
                                                padding-top: 10%;
                                                width: 20%;
                                                justify-content: center"
                                                ))),
    column(8, a(href = route_link("/"),
                h1(align = 'center', "jmspwr",
                   style="font-family: Raleway;
                          padding-top: 18px;
                          justify-content: center"
                   )),
    ),
  ),
  
  fluidRow(column(12, 
  )),
  
  router$ui,
  
  tags$footer(tags$a(href="https://services.jms.rocks","James Twose",
                     style="font-size:20px;
                            font-family: Raleway;"
                            ),
              tags$p("------"),
              tags$a(href="https://github.com/jameshtwose/jmspwr","Github",
                     icon("github"),
                     style="font-size:20px;
                            font-family: Raleway;"
                            ),
              style="
                      text-align:center;
                      width:100%;
                      bottom: 0;
                      z-index: 1000;
                      padding-top: 2.5%;
                      padding-bottom: 2.5%;"
                      )
  
)

# Server code ----

server <- function(input, output, session) {
  router$server(input, output, session)
  
  # ICC code ----
  
  output$table <- renderTable({
    all_data <- ICCSampleSize(p=input$p,p0=input$p0,k=input$k,
                              alpha=input$alpha,tails=input$tails,
                              power=input$powericc)
    all_data
  })
  
  output$distPlotki <- renderPlotly({
    ggplotly(
      ggplot(all_ICC_samp_calc_df, aes(x=N,
                                       y=p,
                                       color=power,
                                       size=alpha
                                       
      )
      ) + geom_point() + scale_size(range = c(0, 2)) +
        ggtitle("Power Analysis ICC") +
        labs(tag = "arbitrary words") +
        scale_color_gradient2(midpoint=0.5, low="#fcdd14", mid="white",
                               high="#8f0fd4", space ="Lab") +
        theme(plot.title = element_text(hjust = 0.5, color="white"),
              axis.title.x = element_text(size = 14, color="white"),
              axis.title.y = element_text(size = 14, color="white"),
              axis.text.x = element_text(color="white"),
              axis.text.y = element_text(color="white"),
              plot.tag.position = c(0.15, 0.02),
              panel.background = element_rect(fill = "#333333"),
              plot.background = element_rect(fill = "#333333"),
              panel.grid.major = element_line(color = "grey30", size = 0.2),
              legend.background = element_blank(),
              legend.text = element_text(color="white"),
              legend.title = element_text(color="white"),
              ),
      height=600
      
    ) %>% layout(annotations = list(x = 0.5, y = -0.35,
                                    text = "This plot shows the sample size as the effect size increases, whilst taking into account power and alpha",
                                    showarrow = F, xref='paper', yref='paper',
                                    xanchor='center', yanchor='center', xshift=0, yshift=0,
                                    font=list(size=10, color="grey")),
                 margin = list(l = 100,
                               r = 100,
                               b = 200,
                               t = 100,
                               pad = 1))
    
  })
  
  output$tablepowerICC <- renderTable({
    all_data <- ICCPower(p=input$ppower,p0=input$p0power,k=input$kpower,
                         alpha=input$alphapower,tails=input$tailspower,
                         N=input$npower, desiredPower=input$despower)
    all_data
  })
  
  output$tableefsiICC <- renderTable({
    all_data <- ICCAchievableP0(p=input$pefsi,k=input$kefsi,
                                alpha=input$alphaefsi,tails=input$tailsefsi,
                                N=input$nefsi,
                                power=input$desiredefsi)
    all_data
  })
  
  
  # Anova code ----
  output$tableanova <- renderTable({
    all_data <- AnovaSampleSize(f_value = input$fvalue, k=input$kclusteranova, alpha=input$alphaanova,
                                power=input$poweranova)
    all_data
  })
  
  output$anovaPlot <- renderPlotly({
    ggplotly(
      ggplot(all_wp_anova_df, aes(x=n,
                                  y=f,
                                  color=power,
                                  size=alpha
      )
      ) + geom_point() + scale_size(range = c(0, 2)) +
        scale_color_gradient2(midpoint=0.5, low="#fcdd14", mid="white",
                              high="#8f0fd4", space ="Lab") +
        theme(
          # plot.title = element_text(hjust = 0.5, color="white"),
              axis.title.x = element_text(size = 14, color="white"),
              axis.title.y = element_text(size = 14, color="white"),
              axis.text.x = element_text(color="white"),
              axis.text.y = element_text(color="white"),
              plot.tag.position = c(0.15, 0.02),
              panel.background = element_rect(fill = "#333333"),
              plot.background = element_rect(fill = "#333333"),
              panel.grid.major = element_line(color = "grey30", size = 0.2),
              legend.background = element_blank(),
              legend.text = element_text(color="white"),
              legend.title = element_text(color="white"),
        ),
      height=600
    ) %>% layout(title = list(text = paste0('Power Analysis ANOVA, Sample Size Estimation',
                                            '<br>',
                                            '<sup>',
                                            'k=3',
                                            '</sup>'),
                              font=list(color="white")),
                 annotations = list(x = 0.5, y = -0.35,
                                    text = "This plot shows the sample size as the effect size increases, whilst taking into account power and alpha",
                                    showarrow = F, xref='paper', yref='paper',
                                    xanchor='center', yanchor='center', xshift=0, yshift=0,
                                    font=list(size=10, color="grey")),
                 margin = list(l = 100,
                               r = 100,
                               b = 200,
                               t = 100,
                               pad = 1))
    
  })
  
  output$tableanovapower <- renderTable({
    
    all_data <- AnovaPower(f_value = input$fvaluepower, k=input$kclusteranovapower, n=input$nanovapower,
                           alpha_value=input$alphaanovapower, power_value = NULL)
    all_data
  })
  
  output$tableanovaefsi <- renderTable({
    
    all_data <- AnovaEffectSize(f_value = NULL, k=input$kclusteranovaefsi, n=input$anovanefsi,
                                alpha_value=input$alphaanovaefsi, power_value=input$poweranovaefsi)
    all_data
  })
  
  # Correlation code ----
  output$tablecorrelation <- renderTable({
    all_data <-CorrelationSampleSize(r_value = input$rvalue,
                                     alpha_value=input$alphacorr,
                                     power_value=input$powercorr,
                                     alternative = "two.sided")
    all_data
  })
  
  output$corrPlot <- renderPlotly({
    ggplotly(
      ggplot(all_wp_correlation_df, aes(x=n,
                                        y=r,
                                        color=power,
                                        size=alpha
      )
      ) + geom_point() + scale_size(range = c(0, 2)) +
        ggtitle("Power Analysis Correlations") +
        scale_color_gradient2(midpoint=0.5, low="#fcdd14", mid="white",
                              high="#8f0fd4", space ="Lab") +
        theme(plot.title = element_text(hjust = 0.5, color="white"),
              axis.title.x = element_text(size = 14, color="white"),
              axis.title.y = element_text(size = 14, color="white"),
              axis.text.x = element_text(color="white"),
              axis.text.y = element_text(color="white"),
              plot.tag.position = c(0.15, 0.02),
              panel.background = element_rect(fill = "#333333"),
              plot.background = element_rect(fill = "#333333"),
              panel.grid.major = element_line(color = "grey30", size = 0.2),
              legend.background = element_blank(),
              legend.text = element_text(color="white"),
              legend.title = element_text(color="white"),
        ),
      height=600
    ) %>% layout(
      annotations = list(x = 0.5, y = -0.35,
                         text = "This plot shows the sample size as the effect size increases, whilst taking into account power and alpha",
                         showarrow = F, xref='paper', yref='paper',
                         xanchor='center', yanchor='center', xshift=0, yshift=0,
                         font=list(size=10, color="grey")),
      margin = list(l = 100,
                    r = 100,
                    b = 200,
                    t = 100,
                    pad = 1))
  })
  
  output$tablecorrelationpower <- renderTable({
    all_data <-CorrelationPower(n=60 , r_value = input$rvaluepower,
                                alpha_value=input$alphacorrpower,
                                power_value=NULL,
                                alternative = "two.sided")
    all_data
  })
  
  output$tablecorrelationefsi <- renderTable({
    all_data <-CorrelationEffectSize(n=60 , r_value = NULL,
                                     alpha_value=input$alphacorrefsi,
                                     power_value=input$powercorrefsi,
                                     alternative = "two.sided")
    all_data
  })
  
  # T-test code ----
  
  ttesttype <- reactive(input$ttestType)
  
  output$tablettest <- renderTable({
    
    ttesttype <- reactive(input$ttestType)
    req(input$ttestType)
    
    if (ttesttype()=='paired')
    {all_data <- DependentTTestSampleSize(d_value=input$dvalue,
                                          alpha_value=input$alphattest,
                                          power_value=input$powerttest,
                                          sort_test=type_of_t_test)
    all_data} else
    {all_data <- IndependentTTestSampleSize(d_value=input$dvalue,
                                            alpha_value=input$alphattest,
                                            power_value=input$powerttest,
                                            sort_test=type_of_t_test_in)
    all_data}
  })
  
  output$ttestPlot <- renderPlotly({
    
    ttesttype <- reactive(input$ttestType)
    req(input$ttestType)
    
    if (ttesttype()=='paired'){
      
      ggplotly(ggplot(all_wp_t_df, aes(x=n,
                                       y=d,
                                       color=power,
                                       size=alpha
      )
      ) + geom_point() + scale_size(range = c(0, 2)) +
        ggtitle(paste("Power Analysis", type_of_t_test, "t-test")) +
        scale_color_gradient2(midpoint=0.5, low="#fcdd14", mid="white",
                              high="#8f0fd4", space ="Lab") +
        theme(plot.title = element_text(hjust = 0.5, color="white"),
              axis.title.x = element_text(size = 14, color="white"),
              axis.title.y = element_text(size = 14, color="white"),
              axis.text.x = element_text(color="white"),
              axis.text.y = element_text(color="white"),
              plot.tag.position = c(0.15, 0.02),
              panel.background = element_rect(fill = "#333333"),
              plot.background = element_rect(fill = "#333333"),
              panel.grid.major = element_line(color = "grey30", size = 0.2),
              legend.background = element_blank(),
              legend.text = element_text(color="white"),
              legend.title = element_text(color="white"),
        ),
      height=600
      ) %>% layout(annotations = list(x = 0.5, y = -0.35,
                                      text = "This plot shows the sample size as the effect size increases, whilst taking into account power and alpha",
                                      showarrow = F, xref='paper', yref='paper',
                                      xanchor='center', yanchor='center', xshift=0, yshift=0,
                                      font=list(size=10, color="grey")),
                   margin = list(l = 100,
                                 r = 100,
                                 b = 200,
                                 t = 100,
                                 pad = 1))
      
    }else {ggplotly(ggplot(all_wp_t_df_in, aes(x=n,
                                               y=d,
                                               color=power,
                                               size=alpha
    )
    ) + geom_point() + scale_size(range = c(0, 2)) +
      ggtitle(paste("Power Analysis", type_of_t_test_in, "t-test")) +
      scale_color_gradient2(midpoint=0.5, low="#fcdd14", mid="white",
                            high="#8f0fd4", space ="Lab") +
      theme(plot.title = element_text(hjust = 0.5, color="white"),
            axis.title.x = element_text(size = 14, color="white"),
            axis.title.y = element_text(size = 14, color="white"),
            axis.text.x = element_text(color="white"),
            axis.text.y = element_text(color="white"),
            plot.tag.position = c(0.15, 0.02),
            panel.background = element_rect(fill = "#333333"),
            plot.background = element_rect(fill = "#333333"),
            panel.grid.major = element_line(color = "grey30", size = 0.2),
            legend.background = element_blank(),
            legend.text = element_text(color="white"),
            legend.title = element_text(color="white"),
      ),
    height=600
    ) %>% layout(annotations = list(x = 0.5, y = -0.35,
                                    text = "This plot shows the sample size as the effect size increases, whilst taking into account power and alpha",
                                    showarrow = F, xref='paper', yref='paper',
                                    xanchor='center', yanchor='center', xshift=0, yshift=0,
                                    font=list(size=10, color="grey")),
                 margin = list(l = 100,
                               r = 100,
                               b = 200,
                               t = 100,
                               pad = 1))
      
    }})
  
  output$tablettestpower <- renderTable({
    
    ttesttype <- reactive(input$ttestType)
    req(input$ttestType)
    
    if (ttesttype()=='paired')
    {all_data <- DependentTTestPower(n1=input$n1,
                                     n2=input$n2,
                                     d_value=input$dvalue,
                                     alpha_value=input$alphattest,
                                     power_value=NULL,
                                     sort_test=type_of_t_test)
    all_data} else
    {all_data <- IndependentTTestPower(n1=input$n1,
                                       n2=NULL,
                                       d_value=input$dvalue,
                                       alpha_value=input$alphattest,
                                       power_value=NULL,
                                       sort_test=type_of_t_test_in)
    all_data}
  })
  
  output$tablettestefsi <- renderTable({
    
    ttesttype <- reactive(input$ttestType)
    req(input$ttestType)
    
    if (ttesttype()=='paired')
    {all_data <- DependentTTestEffectSize(n1=input$n1,
                                          n2=input$n2,
                                          d_value=NULL,
                                          alpha_value=input$alphattest,
                                          power_value=input$powerttest,
                                          sort_test=type_of_t_test)
    all_data} else
    {all_data <- IndependentTTestEffectSize(n1=input$n1,
                                            n2=NULL,
                                            d_value=NULL,
                                            alpha_value=input$alphattest,
                                            power_value=input$powerttest,
                                            sort_test=type_of_t_test_in)
    all_data}
  })
  
  # Binary classification code ----
  output$binarytable <- renderTable({
    all_data <- BinaryClassificationSampleSize(params = input$params,
                                               r_squared = input$r_squared,
                                               prevalence = input$prevalence,
                                               shrinkage = 0.9,
                                               cstatistic = NA)
    all_data
  })
  
  
  output$binaryPlot <- renderPlotly({
    ggplotly(
      ggplot(all_bin_class_samp_calc_df, aes(x=n,
                                             y=r_squared,
                                             color=prevalence,
                                             size=params
      )
      ) + geom_point() + scale_size(range = c(0, 2)) +
        ggtitle("Power Analysis Binary Classification") +
        scale_color_gradient2(midpoint=0.35, low="#fcdd14", mid="white",
                              high="#8f0fd4", space ="Lab") +
        theme(plot.title = element_text(hjust = 0.5, color="white"),
              axis.title.x = element_text(size = 14, color="white"),
              axis.title.y = element_text(size = 14, color="white"),
              axis.text.x = element_text(color="white"),
              axis.text.y = element_text(color="white"),
              plot.tag.position = c(0.15, 0.02),
              panel.background = element_rect(fill = "#333333"),
              plot.background = element_rect(fill = "#333333"),
              panel.grid.major = element_line(color = "grey30", size = 0.2),
              legend.background = element_blank(),
              legend.text = element_text(color="white"),
              legend.title = element_text(color="white"),
        ),
      height=600
    ) %>% layout(annotations = list(x = 0.5, y = -0.35,
                                    text = "This plot shows the sample size as the effect size increases, whilst taking into account power and alpha",
                                    showarrow = F, xref='paper', yref='paper',
                                    xanchor='center', yanchor='center', xshift=0, yshift=0,
                                    font=list(size=10, color="grey")),
                 margin = list(l = 100,
                               r = 100,
                               b = 200,
                               t = 100,
                               pad = 1))
    
  })
  
  # Regression code ----
  output$regressiontable <- renderTable({
    
    all_data <- RegressionSampleSize(params = input$regparams,
                                     r_squared = input$regr_squared,
                                     shrinkage = 0.9,
                                     cstatistic = NA,
                                     intercept = 1,
                                     sd = input$regsd)
    all_data
  })
  
  output$regressionPlot <- renderPlotly({
    ggplotly(
      ggplot(all_regression_samp_calc_df, aes(x=n,
                                              y=r_squared,
                                              color=sd,
                                              size=params
      )
      ) + geom_point() + scale_size(range = c(0, 2)) +
        ggtitle("Power Analysis Linear Regression ") +
        scale_color_gradient2(midpoint=1.5, low="#fcdd14", mid="white",
                              high="#8f0fd4", space ="Lab") +
        theme(plot.title = element_text(hjust = 0.5, color="white"),
              axis.title.x = element_text(size = 14, color="white"),
              axis.title.y = element_text(size = 14, color="white"),
              axis.text.x = element_text(color="white"),
              axis.text.y = element_text(color="white"),
              plot.tag.position = c(0.15, 0.02),
              panel.background = element_rect(fill = "#333333"),
              plot.background = element_rect(fill = "#333333"),
              panel.grid.major = element_line(color = "grey30", size = 0.2),
              legend.background = element_blank(),
              legend.text = element_text(color="white"),
              legend.title = element_text(color="white"),
        ),
      height=600
    ) %>% layout(annotations = list(x = 0.5, y = -0.35,
                                    text = "This plot shows the sample size as the effect size increases, whilst taking into account power and alpha",
                                    showarrow = F, xref='paper', yref='paper',
                                    xanchor='center', yanchor='center', xshift=0, yshift=0,
                                    font=list(size=10, color="grey")),
                 margin = list(l = 100,
                               r = 100,
                               b = 200,
                               t = 100,
                               pad = 1))
    
  })
  
  # Linear mixed models code ----
  
  output$glmmtable <- renderTable({
    
    all_data <- LMMSampleSize(effect_size = input$effectsizeLMM,
                              alpha_value=input$alphaLMM,
                              power_value=input$powerLMM,
                              k_cluster=input$kclusterLMM,
                              icc=input$iccLMM)
    all_data
    
  })
  
  output$glmmdistPlotki <- renderPlotly({

    all_smpsize_lmm_df <- data.frame()
    for (alpha_value in seq(from=0.01, to=0.05, by=0.005)) {
      for (power_value in seq(from=0.1, to=0.8, by=0.05)) {
        smpsize_lmm_list <- plyr::llply(seq(from=0.1, to=0.5, by=0.05),
                                        function(r) LMMSampleSize(effect_size = r,
                                                                  alpha_value=alpha_value,
                                                                  power_value=power_value,
                                                                  k_cluster=input$kclusterLMM,
                                                                  icc=input$iccLMM))

        smpsize_lmm_df <- reshape::merge_all(smpsize_lmm_list)

        all_smpsize_lmm_df <- rbind(all_smpsize_lmm_df, smpsize_lmm_df)
      }
    }

    ggplotly(
      ggplot(all_smpsize_lmm_df, aes(x=Total.Sample.Size,
                                     y=effect_size,
                                     color=power,
                                     size=alpha
      )) +
        xlim(0, 2000) +
        geom_point() +
        scale_color_gradient2(midpoint=0.4, low="#fcdd14", mid="white",
                              high="#8f0fd4", space ="Lab") +
        theme(plot.title = element_text(hjust = 0.5, color="white"),
              axis.title.x = element_text(size = 14, color="white"),
              axis.title.y = element_text(size = 14, color="white"),
              axis.text.x = element_text(color="white"),
              axis.text.y = element_text(color="white"),
              plot.tag.position = c(0.15, 0.02),
              panel.background = element_rect(fill = "#333333"),
              plot.background = element_rect(fill = "#333333"),
              panel.grid.major = element_line(color = "grey30", size = 0.2),
              legend.background = element_blank(),
              legend.text = element_text(color="white"),
              legend.title = element_text(color="white"),
        ) +
        scale_size(range = c(0, 2)),

      height=600
    ) %>% layout(title = list(text = paste0('Power Analysis Linear Mixed Model',
                                            '<br>',
                                            '<sup>',
                                            'bounded at total sample size = 2000',
                                            '</sup>'),
                              font=list(color="white")),
                 annotations = list(x = 0.5, y = -0.35,
                                    text = "This plot shows the sample size as the effect size increases, whilst taking into account power and alpha",
                                    showarrow = F, xref='paper', yref='paper',
                                    xanchor='center', yanchor='center', xshift=0, yshift=0,
                                    font=list(size=10, color="grey")),
                 margin = list(l = 100,
                               r = 100,
                               b = 200,
                               t = 100,
                               pad = 1))
  })
  
}

# Run App ----
shinyApp(ui, server)

