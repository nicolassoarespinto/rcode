show_classes<- function(df){

  lapply(df[,, with =F][,], class) %>% data.frame %>%
    tidyr::gather()


}

compare_classes<- function(df.x, df.y){

  class.x<- show_classes(df.x) %>% dplyr::rename(value.x = value)
  class.y<- show_classes(df.y) %>% dplyr::rename(value.y = value)

  classes<- merge(class.x, class.y)

  return(classes)



}


