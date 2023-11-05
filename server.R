#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#library(charpente) for html2r

#flow -> check prepost ->check groups -> filter functions
# ->use their inputs in selects -> **filter according to mandatory
# ->use inputs on interface -> get data -> add IDs -> apply eligible functions
# ->check ones left -> mark them as unchanged ->update outputs
# -> go to **







server<-function(input, output,session){

  router_server()
  #State of walk-through Select Inputs
  modalInputs=reactiveValues(prepost=TRUE, groups=FALSE, vars=FALSE)
 
  observeEvent(input$show, {
  insertUI(ui=modal(), selector="#cardd")
    
  })
  
  
  observeEvent((input$closeM),{
    removeUI(selector ="#crcl" )
    modalInputs$groups<-FALSE
    modalInputs$vars<-FALSE
  })

  observeEvent((input$vals),{
    print(input$vals)
  })
  
  
  observeEvent(input$prepost, {
    
    if(input$prepost!="" && !isolate(modalInputs$groups)){
    insertUI(ui=selectInput("n_groups","How many groups(arms) do you have?", choices = c("",1:10)),
             selector="#wt-modal",
             where = "beforeEnd")
      modalInputs$groups<-TRUE
    }
    
    if(input$prepost==TRUE){
      Globals$prepost<-1
    }
    else{
      Globals$prepost<-0
    }
    
    if(isolate(modalInputs$groups) && isolate(modalInputs$vars)){
      choices$`Availabile Variables`<-filter_functions(mandatory_inputs, available_inputs)
      UI_names<-rename_inputs( unlist(isolate(choices$`Availabile Variables`)),
                     input_ui_names)
      updateSelectInput(inputId = "input_vars",session=session,choices = UI_names)
     
    }

  })
  
  
  observeEvent(input$n_groups, {
    #browser()
    
    if(input$n_groups!="" && unlist(isolate(modalInputs$vars))){
      Globals$Groups<-input$n_groups
    }
    
    if(isolate(modalInputs$groups) && isolate(modalInputs$vars)){
      
      choices$`Availabile Variables`<-filter_functions(mandatory_inputs, available_inputs)
      UI_names<-rename_inputs( unlist(isolate(choices$`Availabile Variables`)),
                     input_ui_names)
      updateSelectInput(inputId ="input_vars",session=session,choices = UI_names)
    }
 
    
    if(input$n_groups!="" && !isolate(modalInputs$vars)){
      
      modalInputs$vars<-TRUE
      Globals$Groups<-input$n_groups
      choices$`Availabile Variables`<-filter_functions(mandatory_inputs, available_inputs)

      UI_names<-rename_inputs( unlist(isolate(choices$`Availabile Variables`)),
                     input_ui_names)
      insertUI(ui=selectInput("input_vars","Choose the the variable you have in your data:",
                              choices = UI_names,
                              multiple=TRUE, width = '30vw'),
               selector="#wt-modal",
               where = "beforeEnd")
      
    }
 
  })
  onclick("Go",'Shiny.setInputValue("Go", true, {priority: "event"}')
  outframe<-data.frame()
  observeEvent(input$input_vars, {

    Globals$current_outputs<-filter_outputs(input$input_vars,mandatory_inputs, available_outputs )
    
    Globals$chosen_vars<-input$input_vars
    output$`out-list`<-renderUI({
   
      tagList(u_list(mandatory_inputs$Function[isolate(Globals$current_output_functions)]),
              tags$a(id="Go","Enter Data",class="btn btn-accent text-white", style="position: fixed; bottom:5%; right:5%",
                     onclick='Shiny.setInputValue("Go", true, {priority: "event"})',
                     href=route_link("tables")),
             )
    })

  })
  
  observeEvent(input$up,{
    outframe<-as.data.frame(matrix(NA,ncol = length(Globals$chosen_vars),nrow = 10))
    colnames(outframe)<-Globals$chosen_vars
    outframe<-outframe %>% mutate_at(colnames(outframe), as.numeric) %>% mutate(Study_ID="")
    output$df<- renderRHandsontable({
      rhandsontable(outframe)
      
    })
  })
  observeEvent(input$df$changes$changes,{
    changes<-as.data.frame(do.call(rbind,input$df$changes$changes)) 
    colnames(changes)<-c("row", "col", "old", "new")
    changes<-changes%>%select(row, col)%>%mutate_all(unlist)
    changeInds<-changes %>% mutate(row=row+1, col=col+1)
    
    
    changed<-hot_to_r(input$df)
    final<- Task_manager(isolate(Globals$current_output_functions), changed)
   
    output$outtab<-renderTable({final})
  })
}
