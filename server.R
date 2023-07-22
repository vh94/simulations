
# Define server logic 
function(input, output, session) {
  LV_Server("LV")
  SRI_Server("SRI")
  DECAY_Server("DEC")
  VERH_Server("VER")
}


