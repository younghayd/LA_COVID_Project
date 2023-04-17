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

#Need to York fit for all flasks 
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
  
}

#plotting average RCO per year - looks very scattered, small upward trend

year_list <- unique(flask_data$YR)

results <- data.frame(matrix(ncol = 6, nrow = 0))




for(year in year_list){
  
  year_flasks <- filter(flask_data, YR == year)
  
  stats <- graphing_function(df = year_flasks, xdata = year_flasks$CO2FFXS, ydata = year_flasks$COXS, xerror = year_flasks$CO2FFUNC, yerror = year_flasks$COxs_unc)
  stats <- c(stats, year, length(year_flasks$YR))
  results <- rbind(results, stats) #Need to convert this into a dataframe
  
  # results <- as.data.frame(results, stats)
  
  #Results look way too scattered. Try calculate each RCO for each point individually and then plot that to have more points. 
}

colnames(results) <- c("text_grad", "text_grad_unc", "text_int", "text_rsq", "rsq", "year", "flask_total")

#Plotly data here

#Plot gradients

plot <- plot_ly(data = results)

plot <- add_trace(plot, x =~year, y = ~text_grad,
                  data = results, type = "scatter", mode = "markers+lines",
                  text = c(results$rsq),
                  text2 = c(results$flask_total),
                  hovertemplate = paste('Year: %{y}',
                                        '<br>RCO: %{y}',
                                        '<br>r2: %{text:.2f}',
                                        '<br>total: %{text2}'),
                  showlegend = FALSE)
                  

plot <- layout(plot,
              title = "Indy flask RCO time series",
              yaxis = list(title = "RCO (ppb/ppm)"
              ),
              xaxis = list(title = "Year"
              )
)
plot

#Plotting individual RCO values for each flask over time - similar upward trend. Maybe because Indy's cars get worse with age, LA gets better?

flask_data$RCO <- flask_data$COXS/flask_data$CO2FFXS
flask_data <- filter(flask_data, RCO < 100, RCO > -100)
plot(flask_data$DATE, flask_data$RCO)
lm(flask_data$RCO~flask_data$DATE)

