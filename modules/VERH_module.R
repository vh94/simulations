VERH_UI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Verhulst",
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(
          tabPanel("Formula",uiOutput(ns("form"))),
          tabPanel("Code",codeModules::codeOutput(ns("code")))
        ),actionButton(ns("commit"),"Update Parameters", icon("computer"), style= "float:right"),
        tabsetPanel(
          tabPanel("Sliders",
                   sliderInput(ns("tmax"),"t: timesteps",min = 1,max = 1000,value = 200, ticks = FALSE),
                   sliderInput(ns("P0"),"P: Population size",min = 1,max = 100,value = 10,ticks = FALSE),
                   sliderInput(ns("r"),"r: Growth rate",min = 0.01,max = 1,value = 0.05,ticks = FALSE),
                   sliderInput(ns("K"),"K: Carrying capacity",min = 1,max = 1000,value = 800,ticks = FALSE),
                   
          ), #end tabpanel.sliders
          tabPanel("Numbers",
                   numericInput(ns("tmax_eq"), "t: timesteps",min = 1,max = 1000,value = 200),
                   numericInput(ns("N_eq"), "N: Number of particles",min = 1,max = 100,value = 10),
                   numericInput(ns("λ_eq"), "λ: Decay rate",min = 0.01,max = 1,value = 0.05),
                   numericInput(ns("K_eq"),"K: Carrying capacity",min = 1,max = 1000,value = 800),
                   
          )
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Timecourse", 
            d3Output(ns("lines"))
          ),
          tabPanel(
            "Data",
            DTOutput(ns("data"))
          )
        )
      ) # close mainpanel
    )
  )
}

VERH_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$tmax,{
        updateNumericInput(session, "tmax_eq", value = input$tmax)
      })
      observeEvent(input$tmax_eq,{
        updateSliderInput(session, "tmax", value = input$tmax_eq)
      })
      observeEvent(input$P0,{
        updateNumericInput(session, "P_eq", value = input$P0)
      })
      observeEvent(input$P0_eq,{
        updateSliderInput(session, "P0", value = input$P0_eq)
      })
      observeEvent(input$r,{
        updateNumericInput(session, "r_eq", value = input$r)
      })
      observeEvent(input$r_eq,{
        updateSliderInput(session, "r", value = input$r_eq)
      })
      observeEvent(input$K,{
        updateNumericInput(session, "K_eq", value = input$K)
      })
      observeEvent(input$K_eq,{
        updateSliderInput(session, "K", value = input$K_eq)
      })
      # Formula
      eq_succ <-reactive({ paste0("$$\\frac{dP}{dt}= ",input$r,"P(1- \\frac{P}{",input$K,"})$$")})
      pop <- reactive({paste0("$$ P_0=",input$P0,"$$")})
      
      ns <-session$ns
      output$form <- renderUI({
        tagList(
          helpText(
            eq_succ(),
            pop()
          ),
          tags$script(paste0('renderMathInElement(document.getElementById("',ns("form"),'"));'))
        )
      })
      
      # assign user input vars. to parameters
      data <- reactive({
        julia_assign("P0",input$P0)
        julia_assign("K",input$K)
        julia_assign("r",input$r)
        julia_assign("tmax",input$tmax)
        # calc
        julia_command("tspan = (0,tmax)")
        julia_command("u0 = P0")
        julia_command("f(P, p, t) = r*P*(1-(P/K))")
        #create ODE - problem
        julia_command("prob= ODEProblem(f, u0, tspan)")
        julia_command("sol = solve(prob)")
        ## Store julia results and create dataframe for plotting
        data <- data.frame(
          Population=julia_eval("sol.u",need_return = "R"),
          time=julia_eval("sol.t",need_return = "R")) %>%
          pivot_longer(cols = c("Population"),names_to = "character") %>% 
          inner_join(character_hex, by = "character")
      }) %>% bindEvent(input$commit,ignoreNULL=FALSE)
      
      output$code <- renderText({
        sep="\n"
        paste("using DifferentialEquations",
              "f(u, p, t) = r * P*(1-P/K)",
              paste0("tspan = (0,",input$tmax,")"),
              paste0("u0 = ",input$P0),
              paste0("r = ",input$r),
              paste0("K = ",input$K),
              "prob = ODEProblem(f, u0, tspan)",
              "sol = solve(prob)",sep = "\n")
      })
      output$data<-renderDataTable({
        data()
      })
      output$lines<- renderD3({
        r2d3(data = data(),
             script = "./lvline.js",
             d3_version = "5")
      }) 
    }
  )
}