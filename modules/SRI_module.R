SRI_UI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "SIR model",
    sidebarLayout(
      sidebarPanel(
        tabsetPanel(
          tabPanel("Formula",uiOutput(ns("form"))),
          tabPanel("Code",codeModules::codeOutput(ns("code")))
        ),actionButton(ns("commit"),"Update Parameters", icon("computer"), style= "float:right"),
        tabsetPanel(
          tabPanel("Sliders",
                   sliderInput(ns("tmax"),"t: timesteps",min = 1,max = 200,value = 200, ticks = FALSE),
                   sliderInput(ns("susc"),"S: Susceptible:",min = 1,max = 1000,value = 400,ticks = FALSE),
                   sliderInput(ns("inf"),"I: Infected:",min = 1,max = 50,value = 10,ticks = FALSE),
                   sliderInput(ns("β"),"β: Infection rate", min = 0.0,max = 1.0,value = 0.2,step = 0.01,round=FALSE,ticks = FALSE),
                   sliderInput(ns("γ"),"γ: Recovery rate",min = 0.0, max = 1.0,value = 0.1,step= 0.01,round=FALSE,ticks = FALSE)
          ), #end tabpanel.sliders
          tabPanel("Numbers",
                   numericInput(ns("tmax_eq"), "t: timesteps",min = 1,max = 200,value = 200),
                   numericInput(ns("susc_eq"), "S: Susceptible:",min = 1,max = 1000,value = 400),
                   numericInput(ns("inf_eq"), "I: Infected:",min = 1,max = 50,value = 10),
                   numericInput(ns("β_eq"), "β: Infection rate", min = 0.0,max = 1.0,value = 0.2,step = 0.01),
                   numericInput(ns("γ_eq"), "γ: Recovery rate",min = 0.0, max = 1.0,value = 0.1,step= 0.01)
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

SRI_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observeEvent(input$susc,{
        updateNumericInput(session, "susc_eq", value = input$susc)
      })
      observeEvent(input$susc_eq,{
        updateSliderInput(session, "susc", value = input$susc_eq)
      })
      observeEvent(input$inf,{
        updateNumericInput(session, "inf_eq", value = input$inf)
      })
      observeEvent(input$inf_eq,{
        updateSliderInput(session, "inf", value = input$inf_eq)
      })
      observeEvent(input$tmax,{
        updateNumericInput(session, "tmax_eq", value = input$tmax)
      })
      observeEvent(input$tmax_eq,{
        updateSliderInput(session, "tmax", value = input$tmax_eq)
      })
      observeEvent(input$β,{
        updateNumericInput(session, "β_eq", value = input$β)
      })
      observeEvent(input$β_eq,{
        updateSliderInput(session, "β", value = input$β_eq)
      })
      observeEvent(input$γ,{
        updateNumericInput(session, "γ_eq", value = input$γ)
      })
      observeEvent(input$γ_eq,{
        updateSliderInput(session, "γ", value = input$γ_eq)
      })
      # Formula
      eq_succ <-reactive({ paste0("$$\\frac{dS}{dt}= - \\frac{", input$β,"IS}{N}$$")})
      eq_ifn <-reactive({ paste0( "$$\\frac{dI}{dt}= \\frac{", input$β,"IS}{N} -", input$γ,"I$$")})
      eq_rec <-reactive({ paste0( "$$\\frac{dR}{dt}= ", input$γ,"I$$")})
      pop <- reactive({paste0( "$$\\ S_0 =",input$susc,"; I_0 =",input$inf,"$$")})
      
      output$form <- renderUI({
        tagList(
          helpText(
            eq_succ(),
            eq_ifn(),
            eq_rec(),
            pop()
          ),
          tags$script('renderMathInElement(document.getElementById("SRI-form"));')
        )
      })
      
      # assign user input vars. to parameters
      data <- reactive({
        julia_assign("susc",input$susc)
        julia_assign("inf",input$inf)
        julia_assign("tmax",input$tmax)
        julia_assign("β",input$β)
        julia_assign("γ",input$γ)
        # calc
        julia_command("tspan = (0,tmax)")
        julia_command("u0 = [susc; inf; 0]")
        julia_command("p = [β;γ]")
        #create ODE - problem
        julia_command("prob= ODEProblem(SIR!,u0,tspan,p)")
        julia_command("sol = solve(prob)")
        ## Store julia results and create dataframe for plotting
        data <- data.frame(
          julia_eval("sol.t",need_return = "R"),
          julia_eval("transpose(sol)",need_return = "R")
          ) %>% 
          setNames(c("time","Susceptible","Infected","Recovered")) %>% 
          pivot_longer(cols = c("Susceptible","Infected","Recovered"),names_to = "character")%>%
          inner_join(character_hex, by = "character")
      }) %>% bindEvent(input$commit,ignoreNULL=FALSE)
      
      output$code <- renderText({
        sep="\n"
        paste("using DifferentialEquations",
        "function SIR!(du,u,p,t0)
        β,γ = p
        S,I,R = u
        N = S+I+R
        du[1] = -(β*S*I)/N
        du[2] = (β*S*I)/N-γ*I
        du[3] = γ*I 
        end",
              paste0("tspan = (0,",input$tmax,")"),
              paste0("u0 = [",input$susc,";", input$inf,"; 0]"),
              paste0("p = [",input$β,";",input$γ,"]"),
              "prob= ODEProblem(SIR!,u0,tspan,p)",
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