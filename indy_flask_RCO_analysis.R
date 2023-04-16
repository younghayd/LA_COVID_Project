require(readxl)
require(lubridate)
require(dplyr)
require(ggplot2)
require(bfsl)
require(grob)

setwd("H:/LA_flask_observations/Final_outputs/data/Indy_flask_data/")

#Reading in and organising data
flask_data <- read_excel("towers_enhancements_bkgdT1_20230412.xlsx", skip = 14) %>%
  select(LOC, YR, MO, DY, HR, MN, DATE, CO2FFXS, CO2FFUNC, CO2C14FLAG, COXS, CO, COFLAG, ID)%>%
  filter(CO2FFXS > -999, CO > -999)

#Need to york fit for all flasks 
plot(flask_data$CO2FFXS, flask_data$COXS)

df <- flask_data
xdata <- flask_data$CO2FFXS
ydata <- flask_data$COXS
xerror <- flask_data$CO2FFUNC 
yerror <- flask_data$CO2FFUNC*10
xlabel <- "a"
ylabel <- "b"
title <- "a"

flask_data$CO_unc <- 12 #Uncertainty from the standard deviation of hourly avg picarro data from tower 2 in Indy
 
flask_data$COxs_unc <- flask_data$CO_unc*2/sqrt(2)

flask_data <- subset(flask_data, ID != '3058-09')
flask_data <- subset(flask_data, ID != '3151-05')
flask_data <- subset(flask_data, ID != '3057-01')
flask_data <- subset(flask_data, ID != '3503-03')
flask_data <- subset(flask_data, ID != '3033-03')
flask_data <- subset(flask_data, ID != '3154-01')

graphing_function <- function(df,xdata,ydata,title,xlabel,ylabel,xerror = NULL, yerror = NULL) 
{
  
  #### Setting up tooltip, York fit, r^2 ####
  
  # #Tooltip
  tooltipinfo <- c(paste0('ID = ', df$ID))
  #York Fit
  bfsl_fit <- bfsl(xdata,ydata,xerror,yerror)
  print((bfsl_fit))
  df$fit <- xdata*bfsl_fit$coefficients[2]+bfsl_fit$coefficients[1]
  df$lowfit <- xdata*(bfsl_fit$coefficients[2]-bfsl_fit$coefficients[4])+(bfsl_fit$coefficients[1]-bfsl_fit$coefficients[3])
  df$highfit <- xdata*(bfsl_fit$coefficients[2]+bfsl_fit$coefficients[4])+(bfsl_fit$coefficients[1]+bfsl_fit$coefficients[3])
  
  #Calculating R^2
  
  r_numerator <- sum((df$fit-ydata)^2)
  ymean <- mean(ydata)
  r_denominator <- sum((ydata - ymean)^2)
  rsq <- 1-r_numerator/r_denominator
  
  #### Sorting out annotations and point colours and plotting ####
  
  #Adding annotation with gradient and r^2 value to plot
  
  text_grad <-round(bfsl_fit$coefficients[2],1)
  text_grad_unc <- round(bfsl_fit$coefficients[4],1)
  text_int <-round(bfsl_fit$coefficients[1],1)
  text_rsq <- round(rsq,2)
  
  results_tbl <- c(text_grad, text_grad_unc, text_int, text_rsq, rsq)
  
  return(results_tbl)
  # 
  # if(text_int >= 0){
  #   grob_text1 <- bquote('y = '*.(text_grad)*'x + '*.(text_int))
  # } else{
  #   grob_text1 <- bquote('y = '*.(text_grad)*'x - '*.(abs(text_int)))
  # }
  # grob_text2 <- bquote(r^2*'='*.(text_rsq))
  # grob_text3 <- bquote('Emission ratio = '*.(text_grad)*''%+-%''*.(text_grad_unc))
  # 
  # eq_label1 <- grobTree(textGrob(grob_text1,
  #                                x=0.05,  y=0.90, hjust=0,gp=gpar(col="black", fontsize=11)))
  # eq_label2 <- grobTree(textGrob(grob_text2,
  #                                x=0.05,  y=0.84, hjust=0,gp=gpar(col="black", fontsize=11)))
  # eq_label3 <- grobTree(textGrob(grob_text3,
  #                                x=0.05,  y=0.76, hjust=0,gp=gpar(col="black", fontsize=11)))
  # 
  # point_col <- "black"
  # 
  # 
  # plot <- ggplot(data = df, aes(x=xdata, y=ydata)) +theme_linedraw()+
  #   geom_point_interactive(aes(x =xdata, y = ydata, tooltip = tooltipinfo))+ 
  #   labs(title = title, x = xlabel, y = ylabel)
  # 
  # error_bar_weight <- 0.15
  # 
  # 
  # 
  # 
  # plot <- plot + geom_line(aes(x = xdata, y = fit), color ='red') +
  #   annotation_custom(eq_label1) + annotation_custom(eq_label2) + annotation_custom(eq_label3)+
  #   geom_ribbon(aes(ymin=lowfit, ymax=highfit) ,fill="blue", alpha=0.2) 
  # 
  # 
  # #Plotting error bars if they were given
  # if (error_bars == TRUE){
  #   if (!is.null(yerror)){
  #     plot <- plot + geom_errorbar(ymin = ydata - yerror, ymax = ydata + yerror,width = 0.1, size = .5, alpha = error_bar_weight,show.legend=FALSE)
  #   }
  #   if (!is.null(xerror)){
  #     plot <- plot + geom_errorbar(xmin = xdata - xerror, xmax = xdata + xerror,width = 0.1, size = .5, alpha = error_bar_weight,show.legend=FALSE)
  #   }
  # }
  # 
  # print(girafe(code=print(plot),height_svg=4, width_svg = 7))
  
  
  
}

#plotting average RCO per year - looks very scattered, small upward trend

year_list <- unique(flask_data$YR)

results <- data.frame(matrix(ncol = 6, nrow = 0))




for(year in year_list){
  
  year_flasks <- filter(flask_data, YR == year)
  stats <- graphing_function(df = year_flasks, xdata = year_flasks$CO2FFXS, ydata = year_flasks$COXS, xerror = year_flasks$CO2FFUNC, yerror = year_flasks$COxs_unc)
  stats <- c(stats, year)
  results <- rbind(results, stats) #Need to convert this into a dataframe
  
  # results <- as.data.frame(results, stats)
  
  #Results look way too scattered. Try calculate each RCO for each point individually and then plot that to have more points. 
}

colnames(results) <- c("text_grad", "text_grad_unc", "text_int", "text_rsq", "rsq", "year")
plot(results$year, results$text_grad)

lm(results$text_grad~ results$year)


#Plotting individual RCO values for each flask over time - similar upward trend. Maybe because Indy's cars get worse with age, LA gets better?

flask_data$RCO <- flask_data$COXS/flask_data$CO2FFXS
flask_data <- filter(flask_data, RCO < 100, RCO > -100)
plot(flask_data$DATE, flask_data$RCO)
lm(flask_data$RCO~flask_data$DATE)






