navbarPage(
  title= "Statistic and agent-based simulations",
  
  tags$head(
    tags$script(src = 'highlight.pack.js'),
    tags$script(src = 'shiny-showcase.js'),
    tags$link(rel = "stylesheet", type = "text/css", href = "rstudio.css")
  ),
  tags$head(
   tags$link(rel="stylesheet", 
             href="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.css", 
             integrity="sha384-dbVIfZGuN1Yq7/1Ocstc1lUEm+AT+/rCkibIcC/OmWo5f0EA48Vf8CytHzGrSwbQ",
             crossorigin="anonymous"),
   HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/katex.min.js" integrity="sha384-2BKqo+exmr9su6dir+qCw08N2ZKRucY4PrGQPPWU1A7FtlCGjmEGFqXCv5nyM5Ij" crossorigin="anonymous"></script>'),
   HTML('<script defer src="https://cdn.jsdelivr.net/npm/katex@0.10.1/dist/contrib/auto-render.min.js" integrity="sha384-kWPLUVMOks5AQFrykwIup5lo0m3iMkkHrD0uJ4H5cjeGihAutqP0yW0J6dpFiVkI" crossorigin="anonymous"></script>'),
   HTML('
    <script>
      document.addEventListener("DOMContentLoaded", function(){
        renderMathInElement(document.body, {
          delimiters: [{left: "$", right: "$", display: false}]
        });
      })
    </script>')
 ),
  LV_UI("LV"),
  SRI_UI("SRI"),
  DECAY_UI("DEC"),
  VERH_UI("VER")
)
