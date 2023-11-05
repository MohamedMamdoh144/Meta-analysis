
mainUI<-tagList(div(
    class="hero min-h-screen",style="background-image:url('./bg.jpg');",
    div(class="hero-overlay bg-slate-500/70"),  
    div(class="hero-content text-center text-neutral-content flex-col",
        
        tags$h1(class="text-3xl text-base-100 font-bold",
                "Welcome to"),
        tags$h2(class="text-8xl py-6 font-bold text-accent",
                "Statistical Converter"
        ),
        tags$p(class="py-6 text-base-100 text-2xl",
               "Your ulitimate solution for statistical conversions in Meta-Analysis"
        ),
        
        div(class="flex items-center justify-center",
            actionButton("show",
                         "Get Started", class="my-6 btn btn-accent btn-wide btn-outline btn-lg"),
            actionButton("scroll",
                         "See Conversions", class="my-6 mx-6 btn btn-secondary btn-outline btn-lg")
        ),
        
        
        
    )
  ),
div(
  class="min-h-screen bg-slate-800 grid grid-cols-3 grid-rows-3 items-center  justify-items-center	content-center ",
  #Card
  card("Hello"),
  card("chello"),
  card("cry")
  
  
)
, div(id="cardd", class="flex items-center justify-center bg-slate-800/[0.9]", style="position:fixed; left:50%; top:50%; transform:translate(-50%,-50%)") 
)

tableUI<-navbarPage("Hello world",
                    tabPanel("Component 1",
                             fluidRow(column(6,
                                             actionButton("up",
                                                          "Enter Data", class="my-6 btn btn-accent btn-wide btn-outline btn-lg"),
                                             rHandsontableOutput("df")
                             ),
                             column(6,
                                    tableOutput("outtab")
                             )
                             
                             ))
                    ,
                    tabPanel("Component 2"),
                    navbarMenu("More",
                               tabPanel("Sub-Component A"),
                               tabPanel("Sub-Component B")
                    )
                    
)

ui <- div(
  use_daisyui(),useShinyjs(),
  tags$head(
    tags$link(
      rel = "stylesheet", type = "text/css",
      href = "./infile.css" 
    ),
  ),
  `data-theme`="mytheme", router_ui(
    route("/", mainUI),
    route("tables",tableUI)
  )
)


