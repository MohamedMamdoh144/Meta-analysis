library(shiny)
library(shiny.tailwind)
library(shinyjs)
library(shiny.router)

library(readxl)
library(tidyverse)
library(rhandsontable)

library(uxstats)
library(meta)

Globals<-reactiveValues(
  Groups=0,
  prepost=0,
  current_input_functions=c(),
  current_output_functions=c(),
  current_outputs=c(),
  current_DF=NULL
)




choices<-reactiveValues()


available_inputs <- read_excel("www/globals.xlsx", 
                               sheet = "Input") %>% 
  mutate(
    across(everything(), ~replace_na(.x, 0))
  ) 
mandatory_inputs <- read_excel("www/globals.xlsx", 
                               sheet = "Mandatory") %>% 
  mutate(
    across(everything(), ~replace_na(.x, 0))
  ) %>% rowid_to_column(var="ID")
available_outputs <- read_excel("www/globals.xlsx", 
                                sheet = "Output")%>% 
  mutate(
    across(everything(), ~replace_na(.x, 0))
  )

input_ui_names <- read_excel("www/globals.xlsx", 
                             sheet = "Names") %>% rowid_to_column(var="ID")