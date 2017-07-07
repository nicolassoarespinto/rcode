## dplyr way to summarize a data.frame with mean and confidence intervals
## usefull to create error bars in ggplot
## inspired on ggplot::mean_se


mean_se.df<- function(data, group, vars, mult = 1.96){

  dots <- lapply(group, as.symbol)

  se <- function(x){ sd(x, na.rm = TRUE)/sqrt(sum(!is.na(x)))}
  ymin<- function(x){mean(x, na.rm = TRUE) - se(x)}
  ymax<- function(x){mean(x, na.rm = TRUE) + se(x)}
  mean2<- function(x){mean(x, na.rm = TRUE)}


  data %>% group_by_(.dots = dots) %>%
    summarise_each_(funs = funs(mean = mean2, se, ymin, ymax, length), vars = vars)

}
