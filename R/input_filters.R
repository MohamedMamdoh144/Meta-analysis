filter_functions<-function(mandatory,available){

  pp=isolate(Globals$prepost)
  n=isolate(Globals$Groups)
  g1=ifelse(n==1,1,0)
  g2=ifelse(n==2,1,0)
  g3=ifelse(n>2,1,0)
  
  eligible_functions<-mandatory%>%filter(`prepost`== pp,`group_1` == g1, `group_2` == g2, `group_3+`== g3) %>% select(`ID`,`Function`)
  
  inds=match(eligible_functions$`Function`,available$`Function`)

  Globals$current_input_functions<-eligible_functions$ID
 
  final_df<-available[inds,]
  return(
    list(
      colnames(
        final_df%>%
          select(2:ncol(final_df)) %>%
          select(which(colSums(.)>0))
      )
    )
  )
  
}   

filter_outputs<-function(outs, mandatory ,output){
  #
  funcs<-unlist(isolate(Globals$current_input_functions))
  available_funcs<-mandatory[funcs,]%>% select(1,7:ncol(mandatory))
  available_funcs<- available_funcs %>%  rowwise() %>% 
    mutate(original=sum( c_across(2: ncol(available_funcs) ) ), current=0 )
  cols<- match(outs, colnames(available_funcs))
  
  for (i in 1:nrow(available_funcs)) {
    row<-available_funcs[i,]
    
    available_funcs$current[i]=sum(as.numeric(row[cols]) & rep(1, length(outs)))
    
  }
  
  
  final_iDs<-unlist(available_funcs %>% filter(original== current) %>% select(ID))
  
  Globals$current_output_functions<-mandatory$`ID`[final_iDs]
  out_iDs<-match(mandatory$`Function`[final_iDs],output$`Function`)
  
  
  #set availaible output calculations
  
 
  
  
  return(
    list(
      colnames(
        output[out_iDs,]%>%
          select(2:ncol(output)) %>%
          select(which(colSums(.)>0))
      )
    )
  )
  
}   

rename_inputs<-function(inputs, df_names){
  
  #browser()
  
  pp=isolate(Globals$prepost)
  n=isolate(Globals$Groups)
  g1=ifelse(n==1,1,0)
  g2=ifelse(n==2,1,0)
  g3=ifelse(n>2,1,0)
  
  neutral_names<-df_names%>%filter( is.na(`prepost`), is.na(`group_1`), is.na(`group_2`),  is.na(`group_3+`) )
  
  variable_names<-df_names%>%filter(`prepost`== pp,`group_1` == g1, `group_2` == g2, `group_3+`== g3)
  
  final_names<-rbind(variable_names, neutral_names) %>% arrange(ID)
  
  inds<-match(inputs,final_names$internal)
  return (setNames(inputs,final_names$ui[inds]))
  
  
}