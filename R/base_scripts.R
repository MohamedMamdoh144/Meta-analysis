library(tidyverse)
library(uxstats)
library(meta)

Validate_requirements<-function(funcIDs, df, mandatory){
  #Check whether each entry satisfies the mandatory inputs
  #assign the proper function ID to valid entries
  
  
  df<-df%>%mutate(invalid=0, func="")
  available_funcs<-mandatory[funcIDs,]%>% select(1,7:ncol(mandatory))
  available_funcs<- available_funcs %>%  rowwise() %>% 
    mutate(original=sum( c_across(2: ncol(available_funcs) ) ), current=0 )
  

  for (v in 1:nrow(df)){
    df_row<-df[v,] %>% select(!invalid)
    df_row<-df_row%>%select_if(function(x) x!=""  &!is.na(x))
    for (i in 1:nrow(available_funcs)) {
      row<-available_funcs[i,]
      col_inds<-match(colnames(df_row),colnames(available_funcs))
      available_funcs$current[i]=sum(rep(1, length(df_row))& as.numeric(row[col_inds]) )
      
    }
   
    eligible<-available_funcs%>%filter(current==original)
    ifelse(nrow(eligible), df$func[v]<-mandatory$`Function`[eligible$`ID`[1]], df$invalid[v]<-1)
    available_funcs<-available_funcs%>%mutate(current=0)
  }
  
  return(df)
}


Task_manager<-function(funcIDs, df ){
  #make sure output columns exist in data // pre-processing step
  current_outs<-unlist(isolate(Globals$current_outputs))
  output_placeholder_indices<-match(current_outs,colnames(df))
  current_prepost<-isolate(Globals$prepost)
  #browser()
  old_colnames<-colnames(df)

  if(sum(is.na(output_placeholder_indices))){
  for(out in 1:length(output_placeholder_indices)){
  
    if(is.na(output_placeholder_indices[out])){
      df<-cbind(
        rep(NA,nrow(df)),
        df
      )
    }
    
  }
  colnames(df)<-c(current_outs[is.na(output_placeholder_indices)], old_colnames)
  }
  
  validated_df<-Validate_requirements(funcIDs, df, mandatory_inputs)%>%rowid_to_column("ID")
  

  ready_rows<-validated_df%>%filter_at(vars(current_outs), all_vars(!is.na(.)))
  valid_rows<-validated_df%>%filter(invalid==0)
  invalid_rows<-validated_df%>%filter(invalid!=0 ,  !(ID %in% ready_rows$ID))
  for(v in 1:nrow(valid_rows)){
    valid_rows[v,]<-do.call(valid_rows$func[v], list(valid_rows[v,]))
  }
  
  output_df<-rbind(ready_rows,valid_rows,invalid_rows)%>%arrange(ID)
  
  if(current_prepost){
    output_df<-PrePost_to_MeanSD(output_df)
  }
  
  
  return(output_df)
}


Ci_N_to_SD<-function(df){

  ci<-0.95
 if("CI%" %in% colnames(df)){
   ci<-ifelse(is.na(df$`CI%`),0.95,df$`CI%`[1])
 }
  out<-df%>%
    mutate(SD=sqrt(N) * (ulci-llci) / ifelse(N>=100, 3.92, 2*tinv(1-ci,N-1) ) )
  
  return(out)
}

SE_N_to_SD<-function(df){
  out<-df%>%
    mutate(SD=SE*sqrt(N))
  
  return(out)
}

MedianIQ_to_MeanSD<-function(df){
  out<-df%>%
    mutate(Mean=(0.7+0.39/N)*0.5*(q1+q3)+(0.3-0.39/N)*Median)%>%
    mutate(SD=(q3-q1)/(2*qnorm((0.75*N-0.125)/(N+0.25),0,1)))
  
  return(out)
}

MedianRng_to_MeanSD<-function(df){
  out<-df%>%
    mutate(Mean= 2/(4+N^0.75)*(min+max) + N^0.75/(4+N^0.75)*Median)%>%
    mutate(SD=(max-min)/(2*qnorm((N-0.375)/(N+0.25),0,1)))
  
  return(out)
}

Pval_2N_to_SD<-function(df){
  out<-df%>%
    mutate(SD= (Mean/uxstats::tinv(pval,N1+N2-2))/sqrt(1/N1+1/N2))
  
  return(out)
}

Ci_2N_to_SD<-function(df){
  ci<-0.95
  if("CI%" %in% colnames(df)){
    ci<-ifelse(is.na(df$`CI%`),0.95,df$`CI%`[1])
  }
  out<-df%>%
    mutate(SD=((ulci-llci)/ifelse(N1+N2>=100,3.92,uxstats::tinv(1-ci,N1+N2-2)*2))/sqrt(1/N1+1/N2))
  
  return(out)
}

SE_2N_to_SD<-function(df){
  out<-df%>%
    mutate(SD=SE/sqrt(1/N1+1/N2))
  
  return(out)
}

prepare_prepost<-function(df){
  
  
  #change_group=0 ->Pre, change_group=1 ->Post
  pre_group<-df%>% filter(change_group==0) %>% rename(preMean=Mean, preSD=SD)
  post_group<-df%>% filter(change_group==1) %>% rename(postMean=Mean, postSD=SD)
  
  #arrange studies with groups
  
  #Raise an error if unequal number of studies
  assertthat::are_equal(nrow(pre_group),
                        nrow(post_group))
  
  return(
    cbind(pre_group,postMean=post_group$postMean, postSD=post_group$postSD)
  )
}

calc_CCoef<-function(df){
  cc_def<-0.5
  ccoef<-data.frame()
  change_sd<-data.frame()
  if("ccoef" %in% colnames(df))
    { 
      ccoef<-df%>%filter(!is.na(`ccoef`))
  }
  else if("changeSD" %in% colnames(df))
  {  
    change_sd<-df%>%filter(!is.na(changeSD),!is.na(preSD),!is.na(postSD))
  }
  if(nrow(ccoef)){
    cc_def<-mean(ccoef$CC)
    }
  else if(nrow(change_sd)){
    change_sd<-change_sd%>%mutate(CC=(preSD^2+postSD^2-changeSD^2)/(2*preSD*postSD))
    cc_def<-mean(change_sd$CC)
   }
  
  return(cc_def)
}


PrePost_to_MeanSD<-function(df){
  #change into pre, post Means and SDs
  prepared_df<-prepare_prepost(df)
  browser()
  ccoef=calc_CCoef(prepared_df)
  
  out<-prepared_df%>%mutate(changeMean=postMean-preMean,
              changeSD=sqrt(preSD^2+postSD^2 - 2*preSD*postSD*ccoef),
              CC=ccoef)
  #remove change(Mean, SD) and make a storage version of out
  return(out)
  
}

calculate_prop<-function(df){
  m.prop<-metaprop(event_group1,total_group1,studlab = studlab ,df)
  return  (cbind(df, TE=m.prop$TE,seTE=m.prop$seTE) )
  
  
}

calculate_bin<-function(df){
  m.bin<-metabin(event_group1,total_group1,
                 event_group2,total_group2,
                 df,incr = "TACC")
  
  return  (cbind(df, TE=m.bin$TE,seTE=m.bin$seTE) )
  
  
}

calculate_cont<-function(df){
  m.cont<-metacont(number_group1,mean_group1,sd_group1,
                   number_group2,mean_group2,sd_group2,
                   studlab = studlab ,df)
  
  return  (cbind(df, TE=m.cont$TE,seTE=m.cont$seTE) )
  
  
}

# convert_M_SD<- function(df){
#   df<-df%>%filter(Study_ID!="")
#   mean_studies<-df%>%filter(!is.na(Mean))
#   meadian_studies<-df%>%filter(!is.na(Median))
#   mean_no<-mean_studies%>%filter(!is.na(Mean), !is.na(SD))
#   
#   
#   
#   #single group
#   mean_ci<-mean_studies%>%filter(!is.na(ulci), !is.na(llci),  !is.na(N))%>%mutate(SD=sqrt(N1) * (ulci-llci) / ifelse(N>=100, 3.92, 2*tinv(0.05,N-1) ) )
#   
#   mean_se<-mean_studies%>%filter(!is.na(SE) , !is.na(N))%>% mutate(SD=SE*sqrt(N))
#   
#   
#   median_iq<-meadian_studies%>%filter(!is.na(q1) , !is.na(q3) , !is.na(N))%>%
#     mutate(Mean=(0.7+0.39/N)*0.5*(q1+q3)+(0.3-0.39/N)*Median)%>%
#     mutate(SD=(q3-q1)/(2*qnorm((0.75*N-0.125)/(N+0.25),0,1)))
#   
#   median_range<-meadian_studies%>%filter(!is.na(min), !is.na(max), !is.na(N))%>%
#     mutate(Mean= 2/(4+N1^0.75)*(min+max) + N1^0.75/(4+N1^0.75)*Median)%>%
#     mutate(SD=(max-min)/(2*qnorm((N1-0.375)/(N1+0.25),0,1)))
#   df2<-rbind(mean_no,mean_ci,mean_se,median_iq,median_range) %>% arrange(Study_ID,df$Study_ID)
#   df2<-df2[match(df$Study_ID,df2$Study_ID),]
#   
#   return (df2)
#   
#   
# }


