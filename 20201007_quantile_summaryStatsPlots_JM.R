# library(tidyhydat)
library(tidyverse)
library(lubridate)
library(waterData)

#station to analyze
station = '02358000'   
#get site name to use in plot titles and such
stinfo  = siteInfo(station)

#read entire time series
dis   = importDVs(staid=station,code='00060',stat='00003', sdate= "1922-07-01")

# Remove leap days
dis_noleap <- dis %>%
  filter(!(month(dates) == 2 & day(dates) == 29))

# Calculate 0%, 25%, 50%, 75% and 100% quantile for each day
# dplyr and tidyr involve here
dis_quant <- dis_noleap %>%
  mutate(md = strftime(dates, format = "%m-%d")) %>%
  group_by(md) %>%
  summarise(quan10 = quantile(val, 0.10, na.rm=TRUE),
            quan25 = quantile(val, 0.25, na.rm=TRUE),
            quan75 = quantile(val, 0.75, na.rm=TRUE),
            quan90 = quantile(val, 0.90, na.rm=TRUE),
            quan100 = quantile(val, 1, na.rm=TRUE)) %>%
  gather("quantile", "val", -md)

# Remove the "quan" and set the quantile column to factor of 5 levels
# Note that the levels are on descending order because this force ggplot
# to plot quantile 100, then overlay quantile 75 on top of it, so on and
# so forth, i.e. quantile 100 won't cover other quantiles up.
dis_quant$quantile <- str_remove(dis_quant$quantile, "quan") %>%
  factor(levels = c("100", "90", "75", "25", "10"))


# Year of interest, keep this at the current year
#yoi = 2005

quant_plot <- list() #create empty list for storing the plot
counter = 0
for(yoi in 2005:2020){#run for 2005 to 2020 can change this for any years
  print(yoi)#printing out the year as a check
  counter = counter + 1 #use a counter to save the plots in a list
  #if we don't use a counter the first plot is at list2005 instead of list1
  
# Add year to dis_quant's date for plotting purpose
dis_quant1 <- dis_quant %>%
  mutate(dates = paste(yoi, md, sep="-") %>% as.Date)

dis_yoi <- dis_noleap %>%
  filter(year(dates) == yoi)


# To create plots for quarterly reports

library(scales)

#cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2")
cbPalette <- c("mediumpurple3", "darkslategray2", "palegreen2", "sandybrown", "indianred4")
#cbPalette <- c("indianred4", "sandybrown", "palegreen2", "darkslategray2", "mediumpurple3", "black")
#need a loop that will go to line 38 and take the yoi (year of interest) and then pass that yoi down to line
#57 and create that plot, and then move to the next year.  What we want is a loop that runs for whatever
#years specified such as 2005-2020, and then we can use cowplot to arrange those years into a multipanel plot

ref_min<-5000 #this is minimum release from JWLD

#save plot within the list
quant_plot[[counter]] <- ggplot(dis_yoi, aes(x=dates, y=val)) +
  xlab("Month")+
  ylab("River Discharge (ft^3)") +
  labs(title= yoi,fill= "Quantile") +
  geom_ribbon(data = dis_quant1, aes(x=dates, ymax=val, ymin=min(val), fill=quantile)) +
  geom_line(size=1.1) +
  #geom_text()+
  scale_fill_manual(values=cbPalette, labels = c("90-100", "75-90", "25-75", "10-25", "0-10")) +
  scale_x_date(labels = date_format("%b"))+
  theme_minimal() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  geom_hline(yintercept = ref_min, color = "orange", size=1.5, linetype="dashed")+
  scale_y_continuous(limits=c(0, 100000)) #turn off this continuous scale to have the full discharge range
  #scale_y_continuous(limits=c(0, 100000),
  #                     breaks = c(5000, 10000,30000,50000, 80000,100000))

}

require(cowplot)
quantile_plot <- plot_grid(plotlist = quant_plot)
plot(quantile_plot)

ggsave("C:/Users/billpine/Documents/UF Admin/WEC/FSU Science Advisory/quantile_plot_all.tiff", width= 20, height=10, dpi=300)

#quantile_plot<-plot_grid(quant2005, quant2006,quant2007,quant2008,quant2009,quant2010,quant2011,quant2012,quant2013,quant2014,quant2015,quant2016,quant2017,quant2018,quant2019, quant2020, ncol=4, labels=c("a","b","c","d","e","f","g", "h","i","j", "k", "l", "m", "n", "o", "p")) 


require(cowplot)
quantile_plot_1<-plot_grid(quant2005, quant2006,quant2007,quant2008,quant2009,quant2010,quant2011,quant2012, ncol=4, labels=c("a","b","c","d","e","f","g", "h")) 

require(cowplot)
quantile_plot_2<-plot_grid(quant2013,quant2014,quant2015,quant2016,quant2017,quant2018,quant2019, quant2020, ncol=4, labels=c("i","j", "k", "l", "m", "n", "o", "p")) 

#ggsave("fig/quantile_plot.png", width= 20, height=10, dpi=300)