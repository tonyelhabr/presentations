
rm(list = ls())
setwd("O:/_other/data_science_club/R")

library("officer")
library("dplyr")

filepath_pptx_template <- "O:/_other/other_ercot_stuff/PUBLIC_Presentation Template-aelhabr.pptx"
# filepath_pptx_template <- "O:/_other/other_ercot_stuff/16 by 9 PUBLIC PowerPoint Template.pptx"
prez_template <- read_pptx(filepath_pptx_template)

layout_summary(prez_template)
length(prez_template)
prez_template %>% 
  remove_slide(index = 1) %>% 
  remove_slide(index = 1)
length(prez_template)

prez_template %>% 
  layout_properties(layout = "Title and Content", master = "Office Theme")
prez_template %>% 
  layout_properties(layout = "1_Custom Layout", master = "Office Theme")

prez <-
  prez_template %>% 
  # layout_properties(layout = "Title and Content", master = "Office Theme") %>% 
  add_slide(layout = "1_Custom Layout", master = "Office Theme") %>% 
  ph_with_text(type = "body", index = 1, str = "hello world")
length(prez)

pptx_summary(prez)
print(prez, target = "prez.pptx") %>% 
  invisible()


prez_2 <-
  read_pptx() %>% 
  add_slide(layout = "Two Content", master = "Office Theme") %>%
  ph_with_text(type = "body", str = "A first text", index = 1) %>%
  ph_with_text(type = "body", str = "A second text", index = 2) %>%
  ph_with_text(type = "title", str = "A title") %>%
  ph_with_text(type = "ftr", str = "Slide footer") %>%
  ph_with_text(type = "dt", str = format(Sys.Date()))

layout_summary(prez_2)
prez_2 %>% 
  layout_properties(layout = "Two Content", master = "Office Theme")
length(prez_2)
