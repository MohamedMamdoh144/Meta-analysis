library(shiny)
library(shiny.tailwind)



modal <- function(){ 
  itm<-div(class = "w-screen h-screen flex flex-col items-center justify-center",id="crcl",
           actionButton("closeM","Close",class="btn btn-accent text-white", style="position: fixed; top:5%; right:5%"),
           
           div(class="px-lg my-lg flex items-center justify-evenly", id="wt-modal",
               selectInput("prepost","Do you have pre-post data?", choices = list("","No"=FALSE, "Yes"=TRUE), selected = NULL)),
           uiOutput("out-list")
          
  )
  return(itm)
}

# styled element for more info

u_list<-function(itmList){
  out_list<-div(class="bg-white px-5 py-4 rounded-lg absolute", style="bottom:5%; transform:translatex(-50%)",
    tags$h2(class="mb-2 text-lg font-semibold text-gray-900 dark:text-white","Available Conversions"),
    tags$ul(
    class = "max-w-md space-y-1 text-gray-500 list-disc list-inside dark:text-gray-400",
    lapply(itmList, tags$li))
  )
}

card<- function(txt) {
  cardd<-tags$div(
    class = "card w-96 bg-base-100 shadow-xl",
    tags$figure(tags$img(
      src = "/images/stock/photo-1606107557195-0e29a4b5b4aa.jpg",
      alt = "Shoes"
    )),
    tags$div(
      class = "card-body",
      tags$h2(
        class = "card-title",
        "Shoes!",
        tags$div(
          class = "badge badge-secondary",
          txt
        )
      ),
      tags$p("If a dog chews shoes whose shoes does he choose?"),
      tags$div(
        class = "card-actions justify-end",
        tags$div(
          class = "badge badge-outline",
          "Fashion"
        ),
        tags$div(
          class = "badge badge-outline",
          "Products"
        )
      )
    )
  )
  return (cardd)
}






#use native inputs
select_input<- function(id,label="",choices, multiple=FALSE, class="", style="") {
  
  selectInput("vals",label, choices = choices, multiple = multiple)
}




alert<-function(){
  tags$div(
    class = "alert shadow-lg",
    tags$svg(
      xmlns = "http://www.w3.org/2000/svg",
      fill = "none",
      viewbox = "0 0 24 24",
      class = "stroke-cyan-500  shrink-0 w-6 h-6",
      tags$path(
        `stroke-linecap` = "round",
        `stroke-linejoin` = "round",
        `stroke-width` = "2",
        d = "M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
      )
    ),
    tags$div(
      tags$h3(
        class = "font-bold",
        "New message!"
      ),
      tags$div(
        class = "text-xs",
        "You have 1 unread message"
      )
    ),
    tags$button(
      class = "btn btn-sm",
      "See"
    )
  )
}

avatar_text<-function(){
  tags$div(
    class = "avatar placeholder",
    tags$div(
      class = "bg-neutral-focus text-neutral-content rounded-full w-24",
      tags$span(
        class = "text-3xl",
        "K"
      )
    )
  )
}

avatar_group<-function(){
  tags$div(
    class = "avatar-group -space-x-6",
    tags$div(
      class = "avatar",
      tags$div(
        class = "w-12",
        tags$img(src = "/images/stock/photo-1534528741775-53994a69daeb.jpg")
      )
    ),
    tags$div(
      class = "avatar",
      tags$div(
        class = "w-12",
        tags$img(src = "/images/stock/photo-1534528741775-53994a69daeb.jpg")
      )
    ),
    tags$div(
      class = "avatar",
      tags$div(
        class = "w-12",
        tags$img(src = "/images/stock/photo-1534528741775-53994a69daeb.jpg")
      )
    ),
    tags$div(
      class = "avatar placeholder",
      tags$div(
        class = "w-12 bg-neutral-focus text-neutral-content",
        tags$span("+99")
      )
    )
  )
}

button_nav<-function(){
  tags$div(
    class = "btm-nav min-w-screen",
    tags$button(
      class = "bg-pink-200 text-pink-600",
      tags$svg(
        xmlns = "http://www.w3.org/2000/svg",
        class = "h-5 w-5",
        fill = "none",
        viewbox = "0 0 24 24",
        stroke = "currentColor",
        tags$path(
          `stroke-linecap` = "round",
          `stroke-linejoin` = "round",
          `stroke-width` = "2",
          d = "M3 12l2-2m0 0l7-7 7 7M5 10v10a1 1 0 001 1h3m10-11l2 2m-2-2v10a1 1 0 01-1 1h-3m-6 0a1 1 0 001-1v-4a1 1 0 011-1h2a1 1 0 011 1v4a1 1 0 001 1m-6 0h6"
        )
      ),
      tags$span(
        class = "btm-nav-label",
        "Home"
      )
    ),
    tags$button(
      class = "active bg-blue-200 text-blue-600 border-blue-600",
      tags$svg(
        xmlns = "http://www.w3.org/2000/svg",
        class = "h-5 w-5",
        fill = "none",
        viewbox = "0 0 24 24",
        stroke = "currentColor",
        tags$path(
          `stroke-linecap` = "round",
          `stroke-linejoin` = "round",
          `stroke-width` = "2",
          d = "M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
        )
      ),
      tags$span(
        class = "btm-nav-label",
        "Warnings"
      )
    ),
    tags$button(
      class = "bg-teal-200 text-teal-600",
      tags$svg(
        xmlns = "http://www.w3.org/2000/svg",
        class = "h-5 w-5",
        fill = "none",
        viewbox = "0 0 24 24",
        stroke = "currentColor",
        tags$path(
          `stroke-linecap` = "round",
          `stroke-linejoin` = "round",
          `stroke-width` = "2",
          d = "M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z"
        )
      ),
      tags$span(
        class = "btm-nav-label",
        "Statics"
      )
    )
  )
}

btn_outline<-function(id){
  actionButton(id,
               "Get Started", class="my-6 btn btn-accent btn-wide btn-outline btn-lg")
}

btn<-function(id){
  actionButton(id,
               "Get Started", class="my-6 btn btn-primary btn-wide btn-lg")
}

checkbox<-function(){
  twCheckboxInput(
    "chk", "Check me!",
    value = TRUE,
    container_class = "w-48 m-4 p-2 border border-gray-200 rounded-md drop-shadow-md",
    label_class = "font-serif text-gray-600 mx-2",
    input_class = "checkbox checkbox-success",
    center = TRUE
  )
  
}

countdown<-function(){
  tags$span(
    class = "countdown font-mono text-6xl",
    tags$span(style = "--value:37;")
  )
}


file_input<-function(){
  twFileInput(
    inputId = "file", label = "Upload", multiple = TRUE,
    buttonLabel = "Upload", placeholder = "Nothing selected",
    container_class = "",
    label_class = "",
    select_class = "",
    button_class = paste(
      "file-input file-input-bordered file-input-secondary w-full max-w-xs",
      "hover:border-red-700 text-white hover:text-gray-50"
    ),
    progress_class = "bg-red-800"
  )
}

text_input<-function(){
  twTextInput(
    "text", "A Text",
    type = "text", placeholder = "Some Text",
    # Apply tailwind classes
    container_class = paste(
      "w-48 m-4 p-2 border border-gray-200",
      "rounded-md drop-shadow-md"
    ),
    label_class = "font-serif text-gray-600",
    input_class = paste(
      "drop-shadow-lg font-mono text-gray-600",
      "rounded-md border-amber-400"
    )
  )
}

link<-function(){
  tags$a(
    class = "link link-primary",
    "Im a simple link"
  )
}


icon_menu<-function(){
  tags$ul(
    class = "menu menu-horizontal bg-base-200 rounded-box",
    tags$li(tags$a(tags$svg(
      xmlns = "http://www.w3.org/2000/svg",
      class = "h-5 w-5",
      fill = "none",
      viewbox = "0 0 24 24",
      stroke = "currentColor",
      tags$path(
        `stroke-linecap` = "round",
        `stroke-linejoin` = "round",
        `stroke-width` = "2",
        d = "M3 12l2-2m0 0l7-7 7 7M5 10v10a1 1 0 001 1h3m10-11l2 2m-2-2v10a1 1 0 01-1 1h-3m-6 0a1 1 0 001-1v-4a1 1 0 011-1h2a1 1 0 011 1v4a1 1 0 001 1m-6 0h6"
      )
    ))),
    tags$li(tags$a(tags$svg(
      xmlns = "http://www.w3.org/2000/svg",
      class = "h-5 w-5",
      fill = "none",
      viewbox = "0 0 24 24",
      stroke = "currentColor",
      tags$path(
        `stroke-linecap` = "round",
        `stroke-linejoin` = "round",
        `stroke-width` = "2",
        d = "M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
      )
    ))),
    tags$li(tags$a(tags$svg(
      xmlns = "http://www.w3.org/2000/svg",
      class = "h-5 w-5",
      fill = "none",
      viewbox = "0 0 24 24",
      stroke = "currentColor",
      tags$path(
        `stroke-linecap` = "round",
        `stroke-linejoin` = "round",
        `stroke-width` = "2",
        d = "M9 19v-6a2 2 0 00-2-2H5a2 2 0 00-2 2v6a2 2 0 002 2h2a2 2 0 002-2zm0 0V9a2 2 0 012-2h2a2 2 0 012 2v10m-6 0a2 2 0 002 2h2a2 2 0 002-2m0 0V5a2 2 0 012-2h2a2 2 0 012 2v14a2 2 0 01-2 2h-2a2 2 0 01-2-2z"
      )
    )))
  )
}

txt_menu<-function(){
  tags$ul(
    class = "menu bg-base-200 w-56 rounded-box",
    tags$li(tags$a("Item 1")),
    tags$li(tags$a("Item 2")),
    tags$li(tags$a("Item 3"))
  )
}

steps<-function(){
  
  tags$ul(
    class = "steps text-sky-500",
    tags$li(
      class = "step step-primary",
      "Register"
    ),
    tags$li(
      class = "step step-primary",
      "Choose plan"
    ),
    tags$li(
      class = "step step-primary",
      "Purchase"
    ),
    tags$li(
      class = "step",
      "Receive Product"
    )
  )
  
  
}


stats<-function(){
  tags$div(
    class = "stats shadow",
    tags$div(
      class = "stat",
      tags$div(
        class = "stat-figure text-secondary",
        tags$svg(
          xmlns = "http://www.w3.org/2000/svg",
          fill = "none",
          viewbox = "0 0 24 24",
          class = "inline-block w-8 h-8 stroke-current",
          tags$path(
            `stroke-linecap` = "round",
            `stroke-linejoin` = "round",
            `stroke-width` = "2",
            d = "M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
          )
        )
      ),
      tags$div(
        class = "stat-title",
        "Downloads"
      ),
      tags$div(
        class = "stat-value",
        "31K"
      ),
      tags$div(
        class = "stat-desc",
        "Jan 1st - Feb 1st"
      )
    ),
    tags$div(
      class = "stat",
      tags$div(
        class = "stat-figure text-secondary",
        tags$svg(
          xmlns = "http://www.w3.org/2000/svg",
          fill = "none",
          viewbox = "0 0 24 24",
          class = "inline-block w-8 h-8 stroke-current",
          tags$path(
            `stroke-linecap` = "round",
            `stroke-linejoin` = "round",
            `stroke-width` = "2",
            d = "M12 6V4m0 2a2 2 0 100 4m0-4a2 2 0 110 4m-6 8a2 2 0 100-4m0 4a2 2 0 110-4m0 4v2m0-6V4m6 6v10m6-2a2 2 0 100-4m0 4a2 2 0 110-4m0 4v2m0-6V4"
          )
        )
      ),
      tags$div(
        class = "stat-title",
        "New Users"
      ),
      tags$div(
        class = "stat-value",
        "4,200"
      ),
      tags$div(
        class = "stat-desc",
        "âï¸ 400 (22%)"
      )
    ),
    tags$div(
      class = "stat",
      tags$div(
        class = "stat-figure text-secondary",
        tags$svg(
          xmlns = "http://www.w3.org/2000/svg",
          fill = "none",
          viewbox = "0 0 24 24",
          class = "inline-block w-8 h-8 stroke-current",
          tags$path(
            `stroke-linecap` = "round",
            `stroke-linejoin` = "round",
            `stroke-width` = "2",
            d = "M5 8h14M5 8a2 2 0 110-4h14a2 2 0 110 4M5 8v10a2 2 0 002 2h10a2 2 0 002-2V8m-9 4h4"
          )
        )
      ),
      tags$div(
        class = "stat-title",
        "New Registers"
      ),
      tags$div(
        class = "stat-value",
        "1,200"
      ),
      tags$div(
        class = "stat-desc",
        "âï¸ 90 (14%)"
      )
    )
  )
}

tooltip<-function(inside){
  tags$div(
    class = "tooltip tooltip-open tooltip-warning",
    `data-tip` = "warning",
    inside
  )
}