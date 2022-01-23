#Installing packages
install.packages("jtools")
library("dplyr")
library("tidyverse")
library("countrycode")
library(pastecs)
library(magrittr)
library(plotrix)
library(moments)
library(FSA)
library(summarytools)
library(gmodels)
library(epiDisplay)
library(flexdashboard)
library(highcharter)
library(viridis)
library(DT)
library(corrplot)
library(gtsummary)
library(RColorBrewer)


#Setting working directory & Importing csv file into R script
setwd("C:/Users/admin/Downloads")
Suicide_Data<- read.csv("Suicide_Data.csv",header=TRUE, sep=",")
options(max.print=999999)

#Cleaning data
#finding number of missing values for each column
data.frame("column_names"=c(colnames(Suicide_Data)),
           "missing_value_counts"=sapply(Suicide_Data,function(x)sum(is.na(x))),
           row.names=NULL)

#removing columns HDI.for.year and generation
Suicide_Data = subset(Suicide_Data, select=c(-HDI.for.year, -generation))

#replacing null values with 0
Suicide_Data[is.na(Suicide_Data)] <- 0

#changing column names for better understanding
colnames(Suicide_Data) <- c("Country","Year","Sex","Age","Suicides","Population","Suicides_per_100k","Country-year","GDP_for_year","GDP_per_capita")


#removing rows with 2016 data and column Country-year
Suicide_Data <-filter(Suicide_Data, Year != 2016) %>%
  select(-"Country-year")


#using gsub and ifelse to edit data
Suicide_Data$Age <- gsub(' years','',Suicide_Data$Age)
Suicide_Data$Sex <- ifelse(Suicide_Data$Sex == "male", "Male", "Female")

#converting datatypes
Suicide_Data_nominal <- c('country', 'sex', 'continent')
Suicide_Data[Suicide_Data_nominal] <- lapply(Suicide_Data[Suicide_Data_nominal], function(x){factor(x)})

#adding continent column based on country names
Suicide_Data$Continent <- countrycode(sourcevar = Suicide_Data[, "Country"],
                                      origin = "country.name",
                                      destination = "continent")

#converting to factor
Suicide_Data$Country <- as.factor(Suicide_Data$Country)
Suicide_Data$Sex <- as.factor(Suicide_Data$Sex)
Suicide_Data$Continent <- as.factor(Suicide_Data$Continent)


###Descriptive Statistics
summary(Suicide_Data)
str(Suicide_Data)
headtail(Suicide_Data,5)
table(Suicide_Data['Country'])
round(table(Suicide_Data$Year,Suicide_Data$Continent),2)
freq(Suicide_Data$Sex)
CrossTable(Suicide_Data$Continent,Suicide_Data$Age)
stat.desc(Suicide_Data)
table_Continent<-aggregate(x = Suicide_Data$Suicides,                
                 by = list(Suicide_Data$Continent),              
                 FUN=c("count","sum","mean","median","sd","se","min","max"))
table_Age<-aggregate(x = Suicide_Data$Suicides,                
          by = list(Suicide_Data$Age),              
          FUN=c("count","sum","mean","median","sd","se","min","max"))
fivenum(Suicide_Data$No_of_suicides)

#Analysis through Data Visualization
# Distribution of suicide_rate
ggplot(Suicide_Data, aes(x = Suicides_per_100k)) +
  geom_histogram(aes(fill = ..count..), binwidth = 10)+
  scale_x_continuous(name = "Suicide rate per 100k ") +
  scale_y_continuous(name = "Count") +
  ggtitle("Distribution of Suicide Rates per 100K") +
  scale_fill_gradient("Count", low = "blue", high = "red")

#Global suicides per 100k, by Country
Countries <- Suicide_Data %>%
  group_by(Country, Continent) %>%
  summarize(n = n(),
            Suicides_per_100k = (sum(as.numeric(No_of_suicides)) / sum(as.numeric(Population))) * 100000) %>%
  arrange(desc(Suicides_per_100k))

Countries$Country <- factor(Countries$Country,
                            ordered = T,
                            levels = rev(Countries$Country))
Countries <-head(Countries,50)

ggplot(Countries, aes(x = Country, y = Suicides_per_100k, fill = Continent)) +
  geom_bar(stat = "identity") +
  labs(title = "Global suicides per 100k, by Country",
       x = "Country",
       y = "Suicides per 100k",
       fill = "Continent") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 50, 2)) +
  theme(legend.position = "bottom")


#Suicide rate by Continent
Suicide_Data %>% 
  group_by(Country, Continent) %>%
  summarize(avg_suicide_rate=mean(Suicides_per_100k)) %>%
  ggplot(aes(Continent, avg_suicide_rate)) +
  geom_boxplot( fill="light blue", color="navy") +
  # coord_flip() +
  labs(x="Continent", y="Suicide reate per 100k population") +
  ggtitle("Suicide rate by Continent")

#Trend by Age
age_plot <- Suicide_Data %>% group_by(Age) %>%
  summarize(Suicides_per_100k = (sum(as.numeric(Suicides)) / sum(as.numeric(Population))) * 100000)

ggplot(age_plot, aes(x = Age, y = Suicides_per_100k, fill = Age)) +
  geom_bar(stat = "identity") +
  labs(title = "Global suicides per 100k, by Age",
       x = "Age",
       y = "Suicides per 100k") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 30, 1), minor_breaks = F)

#Suicide Rate Variability by Population Size
Suicide_Data %>% group_by(Country, Year) %>%
  summarize(pop=mean(Population), suicide_rate=sum(Suicides)*100000/sum(Population), 
            pop=sum(pop)) %>% ungroup() %>%
  group_by(Country) %>% summarize(pop=sum(pop), 
            suicide_rate=mean(suicide_rate)) %>%
  ggplot(aes(suicide_rate, pop)) +
  geom_point(fill="deepskyblue2", color="navy") +
  geom_text(data = . %>% filter(suicide_rate > 35 | pop > 400000000), 
            aes(label = Country, col=Country), position="dodge") +
  stat_smooth(method = "lm", color = "red", size = 1) +
  theme(legend.position = "none") +
  labs(x="Suicide rate", y="Population") +
  ggtitle("Suicide Rate Variability by Population Size")


## Effect of Nation Wealth on Suicide Rates
Suicide_Data %>% group_by(Country) %>%
  summarize(suicide_rate=sum(Suicides)*100000/sum(Population), 
            gdp_per_capita=mean(GDP_per_capita),
            pop=sum(as.numeric(Population))) %>% 
  arrange(desc(gdp_per_capita)) %>%
  ggplot(aes(gdp_per_capita, suicide_rate)) +
  geom_point(fill="deepskyblue2", color="navy") +
  stat_smooth(method = "lm", color = "red", size = 1) +
  geom_text(data = . %>% filter(gdp_per_capita>64000 | 
  suicide_rate>40), aes(gdp_per_capita, suicide_rate, label=Country, col=Country)) +
  labs(x="Average GDP per capita", y="Suicide rate per 100k population") +
  ggtitle("GDP per Capita vs. Suicide Rate") +
  theme(legend.position = "none") 


##Trends By Sex
Suicide_Data%>% group_by(Year, Sex) %>%
  summarize(suicide_rate=sum(Suicides)*100000/sum(Population)) %>%
  ggplot(aes(Year, suicide_rate, col=Sex)) +
  geom_line() + geom_point() +
  facet_grid(Sex ~ ., scales = "free_y") +
  scale_x_continuous(breaks = seq(1985, 2016, 2)) +
  ggtitle("Suicide Rate Trends by Sex") +
  labs(x="Year", y="Suicide rate per 100k population", col="Sex") +
  scale_color_manual(values = c("darkseagreen", "coral1"),
                     aesthetics = c("colour", "fill")) +
  theme(axis.text.x = element_text(angle = 90), legend.position = "none") 

#Percentage of Men & Women Committed Suicide
Male <-subset(Suicide_Data$Suicides,Suicide_Data$Sex=="Male")
Male<-sum(Male)
Female<-subset(Suicide_Data$Suicides,Suicide_Data$Sex=="Female")
Female<-sum(Female)
sex<-c(Male,Female)
percentage <-round(100*prop.table(sex),digits=2)


colours <-c("lightblue1","lightcoral")
pie3D(percentage,labels=paste0(percentage,"%"),col=colours,
      main= "Percentage of Men & Women Commited Suicide")
legend("topright", c("Men","Women"), cex = 1,fill = colours)

#Creating subset data
### Subset data and graphs
#Create a subset of data for line plot for suicide per 100k for each year
Suicides_Per_year <- Suicide_Data %>%
  select(Year, Suicides, Population) %>%
  group_by(Year) %>%
  summarise(suicide_per_capita = round((sum(Suicides)/sum(Population))*100000, 2))

# Graph 1: Line graphs for suicide per 100k for each year
highchart() %>%
  hc_add_series(Suicides_Per_year, hcaes(x = Year, y = suicide_per_capita, color = Year),
                type = "line") %>%
  hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "",
             pointFormat = paste("Year: <b>{point.x}</b> <br> Suicides: <b>{point.y}</b>")) %>%
  hc_title(text = "Worldwide suicides per year") %>%
  hc_subtitle(text = "1985-2015") %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Suicides per 100K"),
           allowDecimals = FALSE,
           plotLines = list(list(
             color = "Black", width = 1, dashStyle = "Dash",
             value = mean(Suicides_Per_year$suicide_per_capita),
             label = list(text = "Mean = 13.12",
                          style = list(color = "black", fontSize = 11))))) %>%
  hc_legend(enabled = FALSE)

#Create a subset of data for line plot for suicide per 100k for each year in different age groups
suicide_by_age <- Suicide_Data %>%
  select(Year, Age, Suicides, Population) %>%
  group_by(Year, Age) %>%
  summarise(suicide_per_capita = round((sum(Suicides)/sum(Population))*100000, 2))

#graph 2:line plot for suicide per 100k for each year in different age groups
highchart() %>%
  hc_add_series(suicide_by_age, hcaes(x = Year, y = suicide_per_capita, group = Age),
                type = "line", color = age_color) %>%
  hc_tooltip(crosshairs = TRUE, borderWidth = 1.5, headerFormat = "",
             pointFormat = paste("Year: <b>{point.x}</b> <br>",
                                 "Age: <b>{point.age}</b><br>", "Suicides: <b>{point.y}</b>")) %>%
  hc_title(text = "Worldwide suicides by Age") %>%
  hc_subtitle(text = "1985-2015") %>%
  hc_xAxis(title = list(text = "Year")) %>%
  hc_yAxis(title = list(text = "Suicides per 100K people"),
           allowDecimals = FALSE)

#Create a subset of data for bar graph plot for suicide per 100k for different continents and sex.
Suicides_per_Continent_sex <- Suicide_Data %>%
  select(Continent, Sex, Suicides, Population) %>%
  group_by(Continent, Sex) %>%
  summarise(suicide_per_capita = round((sum(Suicides)/sum(Population))*100000, 2))


sex_color <- c("#aeaaac", "#fb5da6")

#graph 3:bar graph plot for suicide per 100k for different continents and sex.
highchart() %>%
  hc_add_series(Suicides_per_Continent_sex, hcaes(x = Continent, y = suicide_per_capita,
                                                  group = Sex), type = "column") %>%
  hc_colors(colors = sex_color) %>%
  hc_title(text = "Suicides by continent and <b>Gender</b>",
           style = (list(fontSize = '14px'))) %>%
  hc_subtitle(text = "1985-2015") %>%
  hc_tooltip(borderWidth = 1.5, pointFormat = paste("Gender: <b> {point.sex} </b> <br>
Suicides: <b>{point.y}</b>")) %>%
  hc_xAxis(categories = c("Africa","America", "Asia", "Europe", "Oceania"),
           labels = list(style = list(fontSize = 12))) %>%
  hc_yAxis(labels = list(style = list(fontSize = 12)),
           title = list(text = "Suicides per 100K people",
                        style = list(fontSize = 12))) %>%
  hc_legend(verticalAlign = 'top', enabled = TRUE)

#Create a subset of data for bar graph plot for suicide per 100k for different continents and age
Suicides_per_Continent_age <- Suicide_Data %>%
  select(Continent,Age, Suicides, Population) %>%
  group_by(Continent,Age) %>%
  summarise(suicide_per_capita = round((sum(Suicides)/sum(Population))*100000, 2)) %>%
  arrange(Suicides_per_Continent_age$Continent)

age_color <- rev(turbo(6))

#graph 4:bar graph plot for suicide per 100k for different continents and age
highchart() %>%
  hc_add_series(Suicides_per_Continent_age, hcaes(x = Continent, y = suicide_per_capita,
                                                  group = Age), type = "column") %>%
  hc_colors(colors = age_color) %>%
  hc_title(text = "Suicides by continent and <b>Age</b>", style = (list(fontSize = '14px'))) %>%
  hc_subtitle(text = "1985-2015") %>%
  hc_tooltip(borderWidth = 1.5, pointFormat = paste("Age: <b> {point.age} </b> <br>
Suicides: <b>{point.y}</b>")) %>%
  hc_xAxis(categories = c("Africa", "America", "Asia", "Europe", "Oceania")
           , labels = list(style = list(fontSize = 12))) %>%
  hc_yAxis(labels = list(style = list(fontSize = 12)),
           title = list(text = "Suicides per 100K people",
                        style = list(fontSize = 12))) %>%
  hc_legend(verticalAlign = 'top', enabled = TRUE)

#Create a subset of data for bar graph plot for suicide by countries and age
suicide_country_age <- Suicide_Data %>%
  select(Country, Age, Suicides, Population) %>%
  group_by(Country, Age) %>%
  summarise(suicide_per_capita = round((sum(Suicides)/sum(Population))*100000, 2))

#graph 5:bar graph plot for suicide by countries and age
highchart() %>%
  hc_add_series(suicide_country_age, hcaes(x = Country, y = suicide_per_capita, group = Age), type = "bar", color = age_color) %>%
  hc_tooltip(borderWidth = 1.5, pointFormat = paste("Age: <b>{point.age} ({point.percentage:.1f}%)</b> <br> Suicides per 100K: <b>{point.y}</b>")) %>%
  hc_title(text = "Suicides by country and age") %>%
  hc_subtitle(text = "1985-2015") %>%
  hc_xAxis(categories = unique(suicide_country_age$Country),
           labels = list(step = 1),
           min = 0, max = 25,
           scrollbar = list(enabled = TRUE)) %>%
  hc_yAxis(title = list(text = "Percent of total suicides")) %>%
  hc_plotOptions(bar = list(stacking = "percent",
                            pointPadding = 0, groupPadding = 0, borderWidth = 0.5))

#descriptive statistics for subset data
summary(suicide_country_age)
summary(Suicides_per_Continent_age)
summary(Suicides_per_Continent_sex)
summary(suicide_by_age)
summary(Suicides_Per_year)

##Two sample t test based on gdp
summary(Suicide_Data$GDP_per_capita)
low_gdp<- subset(Suicide_Data,GDP_per_capita<16816)
high_gdp<- subset(Suicide_Data,GDP_per_capita>16816)
t.test(low_gdp$Suicides,high_gdp$Suicides,var.equal = TRUE)

## Effect of Nation Wealth on Suicide Rates
Suicide_Data %>% group_by(Country) %>%
  summarize(suicide_rate=sum(Suicides)*100000/sum(Population),
            gdp_per_capita=mean(GDP_per_capita),
            pop=sum(as.numeric(Population))) %>%
  arrange(desc(gdp_per_capita)) %>%
  ggplot(aes(gdp_per_capita, suicide_rate)) +
  geom_point(fill="deepskyblue2", color="navy") +
  stat_smooth(method = "lm", color = "red", size = 1) +
  geom_text(data = . %>% filter(gdp_per_capita>64000 |
                                  suicide_rate>40), aes(gdp_per_capita, suicide_rate, label=Country, col=Country)) +
  labs(x="Average GDP per capita", y="Suicide rate per 100k population") +
  ggtitle("GDP per Capita vs. Suicide Rate") +
  theme(legend.position = "none")

##subsets for one sample t-test bases on different age groups
unique(Suicide_Data$Age)
Age_5to14<- subset(Suicide_Data,Age=="5-14")
Age_15to24<- subset(Suicide_Data,Age=="15-24")
Age_25to34<- subset(Suicide_Data,Age=="25-34")
Age_35to54<- subset(Suicide_Data,Age=="35-54")
Age_55to74<- subset(Suicide_Data,Age=="55-74")
Age_above75<- subset(Suicide_Data,Age=="75+")

##one sample t test for age 5 to 14
summary(Age_5to14$Suicides)
t.test(Age_5to14$Suicides,mu=11)
sdf <- sd(Age_5to14$Suicides)
deg_freedom =length(Age_5to14$Suicides)-1
t <- (mean(Age_5to14$Suicides)-11)/(sdf/sqrt(deg_freedom))
p_value<-2*pt(-abs(t),deg_freedom)
p_value

##one sample t test for age 15 to 24
summary(Age_15to24$Suicides)
t.test(Age_15to24$Suicides,mu=175)
sdf <- sd(Age_15to24$Suicides)
deg_freedom =length(Age_15to24$Suicides)-1
t <- (mean(Age_15to24$Suicides)-175)/(sdf/sqrt(deg_freedom))
p_value<-2*pt(-abs(t),deg_freedom)
p_value

##one sample t test for age 25 to 34
summary(Age_25to34$Suicides)
t.test(Age_25to34$Suicides,mu=243)
sdf <- sd(Age_25to34$Suicides)
deg_freedom =length(Age_25to34$Suicides)-1
t <- (mean(Age_25to34$Suicides)-243)/(sdf/sqrt(deg_freedom))
p_value<-2*pt(-abs(t),deg_freedom)
p_value

##one sample t test for age 35 to 54
summary(Age_35to54$Suicides)
t.test(Age_35to54$Suicides,mu=530)
sdf <- sd(Age_35to54$Suicides)
deg_freedom =length(Age_35to54$Suicides)-1
t <- (mean(Age_35to54$Suicides)-530)/(sdf/sqrt(deg_freedom))
p_value<-2*pt(-abs(t),deg_freedom)
p_value

##one sample t test for age 55 to 74
summary(Age_55to74$Suicides)
t.test(Age_55to74$Suicides,mu=358)
sdf <- sd(Age_55to74$Suicides)
deg_freedom =length(Age_55to74$Suicides)-1
t <- (mean(Age_55to74$Suicides)-358)/(sdf/sqrt(deg_freedom))
p_value<-2*pt(-abs(t),deg_freedom)
p_value

##one sample t test for age 75+
summary(Age_above75$Suicides)
t.test(Age_above75$Suicides,mu=141)
sdf <- sd(Age_above75$Suicides)
deg_freedom =length(Age_above75$Suicides)-1
t <- (mean(Age_above75$Suicides)-141)/(sdf/sqrt(deg_freedom))
p_value<-2*pt(-abs(t),deg_freedom)
p_value

#Trend by Age
age_plot <- Suicide_Data %>% group_by(Age) %>%
  summarize(Suicides_per_100k = (sum(as.numeric(Suicides)) / sum(as.numeric(Population))) * 100000)



ggplot(age_plot, aes(x = Age, y = Suicides_per_100k, fill = Age)) +
  geom_bar(stat = "identity") +
  labs(title = "Global suicides per 100k, by Age",
       x = "Age",
       y = "Suicides per 100k") +
  theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(0, 30, 1), minor_breaks = F)

##Two sample t test based on age
summary(Suicide_Data$Age)
Below_55<- subset(Suicide_Data,Age=="15-24" | Age=="25-34"| Age=="5-14" | Age=="35-54")
Above_55<- subset(Suicide_Data,Age=="55-74" | Age=="75+")
t.test(Below_55$Suicides,Above_55$Suicides,var.equal = TRUE)

#Suicide Data of Europe
suicides_in_Europe <- filter(Suicide_Data, Continent == "Europe")
describe(suicides_in_Europe)

#Suicide cases in Europe per country
ggplot(suicides_in_Europe, aes(x= reorder(Country, Suicides), y= Suicides)) +
  geom_bar(stat="identity", fill="darkorange3") +
  labs(title="Suicide cases in Europe per country",
       x="Country",
       y="Count") +
  coord_flip() +
  theme_minimal()

#t.test for Suicides in Europe
t.test(suicides_in_Europe$Suicides, mu = 243.4)
t1 <- (mean(suicides_in_Europe$Suicides)-243.4)/
  (sd(suicides_in_Europe$Suicides)/sqrt(length(suicides_in_Europe$Suicides)))
2*pt(-abs(t1),df=length(suicides_in_Europe$Suicides)-1)


#Suicide Data of Asia
suicides_in_Asia <- filter(Suicide_Data, Continent == "Asia")
summary(suicides_in_Asia$Suicides)

#Suicide cases in Asia per country
ggplot(suicides_in_Asia, aes(x= reorder(Country, Suicides), y= Suicides)) +
  geom_bar(stat="identity", fill="darkorange3") +
  labs(title="Suicide cases in Asia per country",
       x="Country",
       y="Count") +
  coord_flip() +
  theme_minimal()

#t.test for Suicides in Asia
t.test(suicides_in_Asia$Suicides,mu=243.4)
t2 <- (mean(suicides_in_Asia$Suicides)-243.4)/
  (sd(suicides_in_Asia$Suicides)/sqrt(length(suicides_in_Asia$Suicides)))
2*pt(-abs(t2),df=length(suicides_in_Asia$Suicides)-1)

#Suicide Data of Oceania
suicides_in_Oceania <- filter(Suicide_Data, Continent == "Oceania")
summary(suicides_in_Oceania$Suicides)

#Suicide cases in Oceania per country
ggplot(suicides_in_Oceania, aes(x= reorder(Country, Suicides), y= Suicides)) +
  geom_bar(stat="identity", fill="darkred") +
  labs(title="Suicide cases in Oceania per country",
       x="Country",
       y="Count") +
  coord_flip() +
  theme_minimal()

#t.test for Suicides in Oceania
t.test(suicides_in_Oceania$Suicides,mu=243.4)
t3 <- (mean(suicides_in_Oceania$Suicides)-243.4)/
  (sd(suicides_in_Oceania$Suicides)/sqrt(length(suicides_in_Oceania$Suicides)))
2*pt(-abs(t3),df=length(suicides_in_Oceania$Suicides)-1)pt(-abs(t),deg_freedom)
p_value

#Suicide Number Trends by Sex
Male_suicides <- filter(Suicide_Data, Sex=="Male")
Female_suicides <-filter(Suicide_Data, Sex=="Female")
summary(Male_suicides$Suicides)
summary(Female_suicides$Suicides)

Suicide_Data%>% group_by(Year, Sex) %>%
  summarize(suicide_no=sum(Suicides)) %>%
  ggplot(aes(Year, suicide_no, col=Sex)) +
  geom_line() + geom_point() +
  facet_grid(Sex ~ ., scales = "free_y") +
  scale_x_continuous(breaks = seq(1985, 2016, 2)) +
  ggtitle("Suicide Number Trends by Sex") +
  labs(x="Year", y="Suicide Number", col="Sex") +
  scale_color_manual(values = c("darkred", "navy"),
                     aesthetics = c("colour", "fill")) +
  theme(axis.text.x = element_text(angle = 90), legend.position = "none")

#two sample t.test for Suicides based on Sex
t.test(Male_suicides$Suicides,Female_suicides$Suicides,var.equal = TRUE)


## t.test for Suicide rate
summary(Suicide_Data$Suicides)
t.test(Suicide_Data$Suicides,mu=243)
sdf <- sd(Suicide_Data$Suicides)
deg_freedom =length(Suicide_Data$Suicides)-1
t <- (mean(Suicide_Data$Suicides)-243)/(sdf/sqrt(deg_freedom))
p_value<-2*pt(-abs(t),deg_freedom)
p_value

# Distribution of suicide_rate
ggplot(Suicide_Data, aes(x = Suicides_per_100k)) +
  geom_histogram(aes(fill = ..count..), binwidth = 10)+
  scale_x_continuous(name = "Suicide rate per 100k ") +
  scale_y_continuous(name = "Count") +
  ggtitle("Distribution of Suicide Rates per 100K") +
  scale_fill_gradient("Count", low = "blue", high = "red")

## Correlation testing to find relation between Suicides, Population, GDP with different age and sex.
Suicide_Data$Sex<-sapply(Suicide_Data$Sex, unclass)
SD1<- Suicide_Data[,c('Suicides','Population','GDP_per_capita','Year','Sex')]
SD1[1:5] <- lapply(SD1[1:5], as.numeric)
sum <- cor(SD1)
round(sum,2)

install.packages("corrplot")
library(corrplot)
install.packages("RColorBrewer")
library(RColorBrewer)

corrplot(sum, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))
corrplot(sum, type="upper", order="hclust", method = 'number')


## linear regression analysis 
Pop <- Suicide_Data$Population - mean(Suicide_Data$Population)
mod <- lm(Pop ~ Suicide_Data$Suicides)
mod
summary(mod)
tbl_regression(mod)

ggplot(Suicide_Data, aes(x = Population, y = Suicides, color= Suicides))+
  geom_point( aes(fill = Population), size=4, alpha = 0.6) +
  ggtitle("Suicides based on Population") + xlab("Population") + ylab("Suicides")+
  geom_smooth(method = lm, fill='Blue', color='Red',se=TRUE)

## t.test for Suicide rate
reg_model<-lm(Suicides~pops+Sex,Suicide_Data)
reg_model
summary(reg_model)
tbl_regression(reg_model)



