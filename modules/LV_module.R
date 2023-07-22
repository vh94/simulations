LV_UI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Lokta Volterra",
      sidebarLayout(
        sidebarPanel(
          tabsetPanel(
            tabPanel("Formula",uiOutput(ns("form"))),
            tabPanel("Code",codeModules::codeOutput(ns("code")))
          ),actionButton(ns("commit"),"Update Parameters", icon("computer"), style= "float:right"),
          tabsetPanel(
            tabPanel("Sliders",
              sliderInput(ns("tmax"),"t: timesteps",min = 1,max = 2000,value = 500, ticks = FALSE),
              sliderInput(ns("pop1"),"x: Number of prey:",min = 1,max = 150,value = 80,ticks = FALSE),
              sliderInput(ns("pop2"),"y: Number of predator:",min = 1,max = 50,value = 20,ticks = FALSE),
              sliderInput(ns("α"),"α: Birthrate prey",min = 0.0,max = 1.0,value = 0.1,step= 0.001,round=FALSE,ticks = FALSE),
              sliderInput(ns("β"),"β: Effect of Predators on Prey", min = 0.0,max = 1.0,value = 0.002,step = 0.001,round=FALSE,ticks = FALSE),
              sliderInput(ns("δ"),"δ: Effect of Prey on Predators",min = 0.0,max = 1.0,value = 0.002,step= 0.001,round=FALSE,ticks = FALSE),
              sliderInput(ns("γ"),"γ: Deathrate predator",min = 0.0, max = 1.0,value = 0.2,step= 0.001,round=FALSE,ticks = FALSE),
                 ), #end tabpanel.sliders
            tabPanel("Numbers",
              numericInput(ns("tmax_eq"), "t: timesteps", 500, min = 1, max = 2000),
              numericInput(ns("pop1_eq"), "x: Number of prey:", 80, min = 1, max = 150),
              numericInput(ns("pop2_eq"), "y: Number of predator:", 20, min = 1, max = 50),
              numericInput(ns("α_eq"), "α: Birthrate prey",  value = 0.1,min = 0.0, max = 1, step= 0.001),
              numericInput(ns("β_eq"), "β: Effect of Predators on Prey", 0.002, min = 0.0, max = 1.0,step= 0.001),
              numericInput(ns("δ_eq"), "δ: Effect of Prey on Predators", 0.002, min = 0.0, max = 1.0,step= 0.001),
              numericInput(ns("γ_eq"), "γ: Deathrate predator", 0.2, min = 0.0, max = 1,step= 0.001),
                 ) #end tabpanel numerics
               )
             ),
        mainPanel(
          tabsetPanel(
             tabPanel(
               "Timecourse",
               d3Output(ns("lines"))
               #actionButton(ns("updatevis"),"update Visualisation",icon("refresh"))
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

LV_Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # update slider/ numeric inputs
      observeEvent(input$pop1,{
        updateNumericInput(session, "pop1_eq", value = input$pop1)
      })
      observeEvent(input$pop1_eq,{
        updateSliderInput(session, "pop1", value = input$pop1_eq)
      })
      observeEvent(input$pop2,{
        updateNumericInput(session, "pop2_eq", value = input$pop2)
      })
      observeEvent(input$pop2_eq,{
        updateSliderInput(session, "pop2", value = input$pop2_eq)
      })
      observeEvent(input$tmax,{
        updateNumericInput(session, "tmax_eq", value = input$tmax)
      })
      observeEvent(input$tmax_eq,{
        updateSliderInput(session, "tmax", value = input$tmax_eq)
      })
      observeEvent(input$α,{
        updateNumericInput(session, "α_eq", value = input$α)
      })
      observeEvent(input$α_eq,{
        updateSliderInput(session, "α", value = input$α_eq)
      })
      observeEvent(input$β,{
        updateNumericInput(session, "β_eq", value = input$β)
      })
      observeEvent(input$β_eq,{
        updateSliderInput(session, "β", value = input$β_eq)
      })
      observeEvent(input$δ,{
        updateNumericInput(session, "δ_eq", value = input$δ)
      })
      observeEvent(input$δ_eq,{
        updateSliderInput(session, "δ", value = input$δ_eq)
      })
      observeEvent(input$γ,{
        updateNumericInput(session, "γ_eq", value = input$γ)
      })
      observeEvent(input$γ_eq,{
        updateSliderInput(session, "γ", value = input$γ_eq)
      })
      ## Formula
      eq_pred <-reactive({ paste0("$$\\frac{dx}{dt}= ", input$γ,"y - ",  input$δ ,"y$$")})
      eq_prey <-reactive({ paste0( "$$\\frac{dy}{dt}= ",input$α," x - ",input$β," xy  $$")})
      pop <- reactive({paste0( "$$ x_0 =",input$pop1,"; y_0 =",input$pop2,"$$")})
      output$form <- renderUI({
        tagList(
          helpText(
            eq_prey(),
            eq_pred(),
            pop()
          ),
          tags$script('renderMathInElement(document.getElementById("LV-form"));')
        )
      })
    #  assign user input vars. to parameters
      data <- reactive({
        julia_assign("pop1",input$pop1)
        julia_assign("pop2",input$pop2)
        julia_assign("tmax",input$tmax)
        julia_assign("α",input$α)
        julia_assign("β",input$β)
        julia_assign("δ",input$δ)
        julia_assign("γ",input$γ)
        # calc
        julia_command("tspan = (0,tmax)")
        julia_command("u0 = [pop1; pop2]")
        julia_command("p = [α;β;δ;γ]")
        #create ODE - problem
        julia_command("prob= ODEProblem(Lokta_Volterra!,u0,tspan,p)")
        julia_command("sol = solve(prob)")
        ## Store julia results and create dataframe for plotting
        data <- data.frame(
            julia_eval("sol.t",need_return = "R"),
            julia_eval("transpose(sol)",need_return = "R")
            ) %>%
          setNames(c("time","prey","predator")) %>%
          pivot_longer(cols = c("prey","predator"),names_to = "character")%>%
          inner_join(character_hex, by = "character")
      }) %>% bindEvent(input$commit,ignoreNULL=FALSE)
      output$code <- renderText({
        sep="\n"
        paste("using DifferentialEquations",
              "function Lokta_Volterra!(du,u,p,t)
                α,β,δ,γ = p
                x,y = u
                du[1] = α*x - β*x*y
                du[2] =  δ*x*y - γ*y
               end",
              paste0("tspan = (0,",input$tmax,")"),
              paste0("u0 = [",input$pop1,";", input$pop2,"]"),
              paste0("p = [",input$α,";",input$β,";",input$δ,";",input$γ,"]"),
              "prob= ODEProblem(Lokta_Volterra!,u0,tspan,p)",
              "sol = solve(prob)",sep = "\n")
      })
      # render dt
      output$data<-renderDataTable({
        data()
      })
      ## D3 LinePlot #####
      output$lines<- renderD3({
        r2d3(data = data(),
             script = "lvline.js",
             d3_version = "5")
      }) #%>% bindEvent(input$updatevis)

    }
  )
}