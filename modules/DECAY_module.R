DECAY_UI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Radioactive decay",
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(
          tabPanel("Formula",uiOutput(ns("form"))),
          tabPanel("Code",codeModules::codeOutput(ns("code")))
        ),actionButton(ns("commit"),"Update Parameters", icon("computer"), style= "float:right"),
        tabsetPanel(
          tabPanel("Sliders",
                   sliderInput(ns("tmax"),"t: timesteps",min = 1,max = 2000,value = 2000, ticks = FALSE),
                   sliderInput(ns("N"),"N: Number of particles",min = 1,max = 1000,value = 400,ticks = FALSE),
                   sliderInput(ns("λ"),"λ: Decay rate",min = 0.1,max = 10,value = 1.2,ticks = FALSE),
          ), #end tabpanel.sliders
          tabPanel("Numbers",
                   numericInput(ns("tmax_eq"), "t: timesteps",min = 1,max = 2000,value = 2000),
                   numericInput(ns("N_eq"), "N: Number of particles",min = 1,max = 1000,value = 400),
                   numericInput(ns("λ_eq"), "λ: Decay rate",min = 0.1,max = 10,value = 1.2),
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

DECAY_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$tmax,{
        updateNumericInput(session, "tmax_eq", value = input$tmax)
      })
      observeEvent(input$tmax_eq,{
        updateSliderInput(session, "tmax", value = input$tmax_eq)
      })
      observeEvent(input$N,{
        updateNumericInput(session, "N_eq", value = input$N)
      })
      observeEvent(input$N_eq,{
        updateSliderInput(session, "N", value = input$N_eq)
      })
      observeEvent(input$λ,{
        updateNumericInput(session, "λ_eq", value = input$λ)
      })
      observeEvent(input$λ_eq,{
        updateSliderInput(session, "λ", value = input$λ_eq)
      })
      # Formula
      eq_succ <-reactive({ paste0("$$\\frac{dN}{dt}= -",input$λ,"N$$")})
      pop <- reactive({paste0("$$ N_0=",input$N,"$$")})
      
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
        julia_assign("N",input$N)
        julia_assign("λ",input$λ)
        julia_assign("tmax",input$tmax)
        # calc
        julia_command("tspan = (0,tmax)")
        julia_command("u0 = N")
        julia_command("f(u, p, t) = -λ * u")
        #create ODE - problem
        julia_command("prob= ODEProblem(f, u0, tspan)")
        julia_command("sol = solve(prob)")
        ## Store julia results and create dataframe for plotting
        data <- data.frame(
                time=julia_eval("sol.t",need_return = "R"),
                Particles=julia_eval("sol.u",need_return = "R")
                ) %>%
          pivot_longer(cols = c("Particles"),names_to = "character") %>% 
          inner_join(character_hex, by = "character")
      }) %>% bindEvent(input$commit,ignoreNULL=FALSE)
      
      output$code <- renderText({
        sep="\n"
        paste("using DifferentialEquations",
              "f(u, p, t) = -λ * u",
              paste0("tspan = (0,",input$tmax,")"),
              paste0("u0 = ",input$N),
              paste0("λ = ",input$λ),
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