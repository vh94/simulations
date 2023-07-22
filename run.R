library(shiny)
library(tidyverse)
library(r2d3)
library(JuliaCall)
library(DT)
#library(codeModules)

julia<-julia_setup(JULIA_HOME ="/home/vali/julia-1.8.5/bin")
julia_library("DifferentialEquations")

#julia_source("setup.jl")

julia_command('
    function Lokta_Volterra!(du,u,p,t)
          α,β,δ,γ = p
          x,y = u
          du[1] = α*x - β*x*y
          du[2] =  δ*x*y - γ*y
    end'
)

# julia_command('
#   function SRI_RE!(du,u,p,t0)
#         du[1] = 0.01*u[3]-0.0005*u[1]*u[2]  # S
#         du[2] = 0.0005*u[1]*u[2]-0.05*u[2]  # I
#         du[3] = 0.05*u[2]-0.01*u[3]         # R
#     end'
# )
# 
# 
julia_command('
  function SIR!(du,u,p,t0)
        β,γ = p
        S,I,R = u
        N = S+I+R
        du[1] = -(β*S*I)/N        # S
        du[2] = (β*S*I)/N-γ*I  # I
        du[3] = γ*I                  # R
    end'
)




# julia_command('
#   function SIR!(du,u,p,t0)
#         N = sum(u)
#         β,γ = p
#         du[1] = -(β*u[1]*u[2])/N        # S
#         du[2] = (β*u[1]*u[2])/N-γ*u[2] # I
#         du[3] = γ*u[2]              # R
#     end'
# )


character_hex = tribble(
  ~ character, ~ color,
  "Susceptible", "#76a2ca",
  "Infected", "#cd7e05",
  "Recovered", "darkgreen",
  "prey", "#76a2ca",
  "predator", "#cd7e05",
  "Particles", "darkgreen",
  "Population","#76a2ca",
)

source("LV_module.R")
source("SRI_module.R")
source("DECAY_module.R")
source("VERH_module.R")

runApp(appDir = "./")
  