BaseData = read.csv(file = 'Base.csv', header = TRUE, sep = ,) # Base Data contains everything about campaign except hour and custom report information
require(ggplot2)
require(dplyr)
require(lubridate) #Because dplyr doesn't support dates unless they're POSIXCT format
require(Cairo) #For nicer graphs
require(Amelia) # Handles missing data
require(grid)
require(ggthemes)
# Flat

BaseData = BaseData[1:(dim(BaseData)[1]-1),] # Remove the last line containing the totals.
BaseData$Impressions = gsub(",","", BaseData$Impressions, fixed = TRUE) # Remove the comma's from the values
BaseData$Clicks = gsub(",","", BaseData$Clicks, fixed = TRUE) # Likewise for clicks
BaseData$Impressions = as.numeric(BaseData$Impressions) # Cast values into numbers
BaseData$Clicks = as.numeric(BaseData$Clicks)
BaseData$Date = mdy(BaseData$Date) #It's not clear
ToDrop = c("CTR", "Fill.Rate", "Conversions", "eCPM", "Revenue", "IAB.Tier.1.Categories")
BaseData = BaseData[, !(names(BaseData) %in% ToDrop)] # Drop variables useless for external reporting
PlotData = aggregate(cbind(BaseData$Impressions, BaseData$Clicks) ~ BaseData$Date, BaseData, FUN = sum)
colnames(PlotData) = c("Date", "Impressions", "Clicks") #Rename to more readable formats

PlotData$CTR = (PlotData$Clicks/PlotData$Impressions)
PlotData$CumlCTR = (cumsum(PlotData$Clicks))/(cumsum(PlotData$Impressions))
BasePlot = ggplot(PlotData, aes(Date, Impressions, Clicks, CTR))

ImpDatePlot = BasePlot +
  geom_path(aes(y = Impressions), size = 0.4, colour = "#009BDC") +
  labs(title = "Impressions per day", x = "Date", y = "Impressions") +
  scale_y_continuous(breaks = c(0,10000,20000,30000,40000,50000,60000,70000)) +
  theme_few() +
  scale_colour_few() +
  theme(plot.title = element_text(size = 16, family = "Ubuntu"),
        text = element_text(family = "Ubuntu Light"),
        plot.margin = unit(c(0,0,0,0),"cm"),
        panel.border = element_blank())

ImpCumSumPlot = BasePlot +
  geom_path(aes(y = cumsum(PlotData$Impressions))) +
  geom_area(aes(y=cumsum(PlotData$Impressions)), fill = "#009BDC", alpha = 0.5) +
  scale_y_continuous(breaks = round(seq(0,max(cumsum(PlotData$Impressions)), length.out = 10))) +
  labs(title = "Cumulative Impressions by date", x = "Date", y = "") +
  theme_few() +
  scale_colour_few() +
  theme(plot.title = element_text(size = 16, family = "Ubuntu"),
        text = element_text(family = "Ubuntu Light"),
        plot.margin = unit(c(0,0,0,0),"cm"),
        panel.border = element_blank())

ClickDatePlot = BasePlot +
  geom_path(aes(y = Clicks), colour = "#009BDC", size = 0.4) +
  labs(title = "Clicks per day", x= "Date", y = "Clicks") +
  scale_y_continuous(breaks = seq(0, max(PlotData$Clicks), by = 50)) +
  theme_few() +
  scale_colour_few() +
  theme(plot.title = element_text(size = 16, family = "Ubuntu"),
        text = element_text(family = "Ubuntu Light"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.border = element_blank())

ClickCumSumPlot = BasePlot +
  geom_path(aes(y = cumsum(PlotData$Clicks))) +
  geom_area(aes(y=cumsum(PlotData$Clicks)), fill = "#009BDC", alpha = 0.5) +
  scale_y_continuous(breaks = round(seq(0,max(cumsum(PlotData$Clicks)), length.out = 10))) +
  labs(title = "Cumulative Clicks by date", x = "Date", y = "Cumulative Clicks") +
  theme_few() +
  scale_colour_few() +
  theme(plot.title = element_text(size = 16, family = "Ubuntu"),
        text = element_text(family = "Ubuntu Light"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.border = element_blank())

CTRDatePlot = BasePlot +
  geom_path(aes(y = CTR), colour = "#009BDC", size = 0.4) +
  labs(title = "CTR per day", x = "Date", y = "CTR") +
  scale_y_continuous(breaks = seq(0,max(PlotData$CTR), length.out = 10), labels = scales::percent) +
  theme(plot.margin = unit(c(0,0,0,0),"cm")) +
  theme_few() +
  scale_colour_few() +
  theme(plot.title = element_text(size = 16, family = "Ubuntu"),
        text = element_text(family = "Ubuntu Light"),
        plot.margin = unit(c(0,0,0,0),"cm"),
        panel.border = element_blank())

CTRCumlPlot = BasePlot +
  geom_path(aes(y = CumlCTR)) +
  geom_area(aes(y=CumlCTR), fill = "#009BDC", alpha = 0.5) +
  scale_y_continuous(breaks = seq(0, max(PlotData$CumlCTR), length.out = 10), labels = scales::percent) +
  labs(title = "Cumulative CTR", x = "Date", y = "CTR") +
  theme_few() +
  scale_colour_few() +
  theme(plot.title = element_text(size = 16, family = "Ubuntu"),
        text = element_text(family = "Ubuntu Light"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.border = element_blank())

StrTemp = readLines("Hour.csv") # If you get an error here, put a newline character at the end of the file
HourData = read.csv(text = StrTemp, skip = 4, nrow = length(StrTemp) -6, header= TRUE, sep = ",")
HourData = subset(HourData, select = c(Hour, Impressions, Clicks))
HourData$Hour = as.numeric(HourData$Hour)
for(i in seq(1, length(HourData$Hour), by =1)){
  HourData$Hour[i] = (i + 10) %% 24
}
HourData = HourData[order(HourData$Hour),]
row.names(HourData) = NULL
HourData$CTR = HourData$Clicks/HourData$Impressions

CTRHourly = ggplot(HourData, aes(x = Hour)) +
  geom_path(aes(y = CTR)) +
  geom_area(aes(y = CTR), fill = "#009BDC", alpha = 0.5) +
  scale_x_continuous(breaks = seq(0,23, by =2)) +
  scale_y_continuous(breaks = seq(0, max(HourData$CTR), length.out = 6), labels = scales::percent) +
  labs(title = "CTR by Hour", x = "Hour (24hr format)", y = "CTR") +
  theme_few() +
  scale_colour_few() +
  theme(plot.title = element_text(size = 16, family = "Ubuntu"),
        text = element_text(family = "Ubuntu Light"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.border = element_blank())

CTRCirclePlot = ggplot(HourData, aes(x=Hour)) +
  geom_bar(aes(y = CTR, fill = Hour), width = 1, stat = "identity") +
  scale_x_continuous(breaks = seq(0,23, by =2)) +
  theme_few() +
  scale_colour_few() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0,0,0,0),"cm"),
        axis.title = element_blank(),
        plot.title = element_text(size = 16, family = "Ubuntu"),
        text = element_text(family = "Ubuntu Light"),
        panel.border = element_blank()) +
  guides(fill = FALSE) +
  labs(title = "CTR By Hour", x = NULL, y = NULL) +
  coord_polar(start = -0.1)

CreativeData = aggregate(cbind(BaseData$Impressions, BaseData$Clicks, BaseData$Banners) ~ BaseData$Date + BaseData$Banners, FUN = sum)
CreativeData$V3 = NULL
colnames(CreativeData) = c("Date", "Banner", "Impressions", "Clicks")

BaseCreativePlot =  ggplot(CreativeData, aes(Date, Impressions, Clicks))
CreativeImpressionPlot = BaseCreativePlot + geom_area(aes(y = Impressions, colour = Banner, fill = Banner), position = 'fill')
CreativeClickPlot = BaseCreativePlot + geom_area(aes(y = Clicks, colour = Banner, fill = Banner), position = 'stack') + theme(plot.margin = unit(c(0,0,0,0),"cm"))
CustomData = read.csv("Custom.csv", header = TRUE, sep = ",")
CustomData = subset(CustomData, select = c(Device.OS, Geo.Region, Device.Type, Impressions, Clicks))
colnames(CustomData) = c("OS", "State", "Type", "Impressions", "Clicks")
CustomData$OS = gsub("[\\d-.]*", "", CustomData$OS, perl = TRUE) # Remove version numbers
CustomData$OS = gsub("Android.+", "Android", CustomData$OS) # Clean 'Android....' entries
CustomData$OS = gsub("iPhone OS", "iOS", CustomData$OS) # Clean 'iPhone/iOS/etc...' entries
CustomData$OS = gsub("^(?!.*(Android|iOS)).*", "Other", CustomData$OS, ignore.case = FALSE, perl = TRUE) # Label everything that's not Android or iOS as 'Other'

# Regex code for finding Android strings: /Android[\d-.]*/g
# Regex code for finding iOS strings: /iPhone OS[\d-.]*/g

OStemp = aggregate(cbind(CustomData$Impressions, CustomData$Clicks) ~ CustomData$OS, FUN = sum) # Aggregate on OS for graphs
colnames(OStemp) = c("OS", "Impressions", "Clicks")
OStemp$ImpFrac = OStemp$Impressions/sum(OStemp$Impressions)
OStemp$ClickFrac = OStemp$Clicks/sum(OStemp$Clicks)
OStemp$ImpMax = cumsum(OStemp$ImpFrac)
OStemp$ClickMax = cumsum(OStemp$ClickFrac)
OStemp$ImpMin = c(0, head(OStemp$ImpMax, n = -1))
OStemp$ClickMin = c(0, head(OStemp$ClickMax, n = -1))

OSData = ggplot(OStemp, aes(fill = OS)) +
  geom_rect(aes(ymax = ImpMax, ymin = ImpMin, xmax = 0.6, xmin = 0.3), colour = "white") +
  geom_rect(aes(ymax = ClickMax, ymin = ClickMin, xmax = 0.3, xmin = 0), colour = "white") +
  labs(x= NULL, y = NULL) +
  geom_text(aes(label = paste(round(ImpFrac*100, 2), "%"), x = 0.45, y = ((ImpMax + ImpMin)/2), inherit.aes = TRUE, show.legend = FALSE), family = "Ubuntu Light") +
  geom_text(aes(label = paste(round(ClickFrac*100,2),"%"), x = 0.15, y = ((ClickMin + ClickMax)/2), inherit.aes = TRUE, show.legend = FALSE)) +
  coord_flip() +
  theme_few() +
  scale_colour_few() +
  xlim(-0.1,0.7) +
  ylim(-0.2,1) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        panel.border = element_blank(),
        legend.position = "bottom") +
  annotate("text", x = 0.45, y = -0.105, label = "Impressions", size = 5, family = "Ubuntu Light") +
  annotate("text", x = 0.15, y = -0.05, label = "Clicks", size = 5, family = "Ubuntu Light") + 
  annotate("text", x = 0.7, y = 0.5, label = "Platform Breakdown", size = 6, family = "Ubuntu Light")

Statetemp = aggregate(cbind(CustomData$Impressions, CustomData$Clicks) ~ CustomData$State, FUN = sum)
colnames(Statetemp) = c("State", "Impressions", "Clicks")
Statetemp = Statetemp[!(Statetemp$State == ""),]
row.names(Statetemp) = NULL
Statetemp$ImpFrac = Statetemp$Impressions/sum(Statetemp$Impressions)
Statetemp$ClickFrac = Statetemp$Clicks/sum(Statetemp$Clicks)
Statetemp$ImpMax = cumsum(Statetemp$ImpFrac)
Statetemp$ClickMax = cumsum(Statetemp$ClickFrac)
Statetemp$ImpMin = c(0, head(Statetemp$ImpMax, n = -1))
Statetemp$ClickMin = c(0, head(Statetemp$ClickMax, n = -1))

Stateplot = ggplot(Statetemp, aes(fill = State)) +
  geom_rect(aes(ymin = ImpMin, ymax = ImpMax, xmin = 0, xmax = 0.2), colour = "White") +
  geom_rect(aes(ymin = ClickMin, ymax = ClickMax, xmin = 0.2, xmax = 0.4), colour = "white") +
  xlim(c(0,0.5)) +
  annotate("text", x = 0.1, y = 1.1, label = "Proportional \n Impressions by State", size = 5, fontface = "bold") +
  annotate("text", x = 0.3, y = 1.1, label = "Proportional cicks \n by state", size = 5, fontface = "bold") +
  labs(x = "", y = "") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank(),
        plot.margin = unit(c(0,0,0,0),"cm")) +
  guides(fill = guide_legend(override.aes = list(colour = NULL)))