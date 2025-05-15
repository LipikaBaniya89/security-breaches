# Listing 3-0

#get working directory
getwd()

# set working directory to data
setwd("/Users/lipikabania/Documents/ISA/Week 02")

# packages are installed
pkg <- c("ggplot2", "lattice")
new.pkg <- pkg[!(pkg %in% installed.packages())]
if (length(new.pkg)) {
  install.packages(new.pkg)  
}
	
# Listing 3-2
# link to the dataset
avURL <- 
  "http://datadrivensecurity.info/book/ch03/data/reputation.data"

# relative path for the downloaded data
avRep <- "/Users/lipikabania/Documents/ISA/Week 02/reputation.data"

#  an if{}-wrapped test is used to avoid having to re-download a 16MB file every time 
if (file.access(avRep)) {
  download.file(avURL, avRep) 
}

# Listing 3-4
# read in the IP reputation db into a data frame using # as the delimeter
# this data file has no header, so set header=FALSE
av <- read.csv(avRep,sep="#", header=FALSE)

#more readable column names are assigned 
colnames(av) <- c("IP", "Reliability", "Risk", "Type",
                  "Country", "Locale", "Coords", "x")

str(av) # an overview of the data frame 

head(av) # first few rows of data


# Listing 3-7
# descriptive statistics of Reliability and Risk
summary(av$Reliability)
summary(av$Risk)

# Listing 3-9
# table to check the frequency of Reliability and Risk
table(av$Reliability)
table(av$Risk)

# summary sorts by the counts by default
# maxsum sets how many factors to display
summary(as.factor(av$Type), maxsum=10)
##                Scanning Host               Malware Domain 
##                       234180                         9274 
##                   Malware IP               Malicious Host 
##                         6470                         3770 
##                     Spamming                          C&C 
##                         3487                          610 
## Scanning Host;Malicious Host    Malware Domain;Malware IP 
##                          215                          173 
## Malicious Host;Scanning Host                      (Other) 
##                          163                          284 

summary(as.factor(av$Country), maxsum=40)
##      CN      US      TR              DE      NL      RU      GB 
##   68583   50387   13958   10055    9953    7931    6346    6293 
##      IN      FR      TW      BR      UA      RO      KR      CA 
##    5480    5449    4399    3811    3443    3274    3101    3051 
##      AR      MX      TH      IT      HK      ES      CL      AE 
##    3046    3039    2572    2448    2361    1929    1896    1827 
##      JP      HU      PL      VE      EG      ID      RS      PK 
##    1811    1636    1610    1589    1452    1378    1323    1309 
##      VN      LV      NO      CZ      BG      SG      IR (Other) 
##    1203    1056     958     928     871     868     866   15136 

# Listing 3-11
# require object: av (3-4)
# load the ggplot2 library to make the graphs
# NOTE: Graphing the data shows there are a number of entries without
#       a corresponding country code, hence the blank entry 
library(ggplot2)

# Filter out NA or blank Country values
av_clean <- subset(av, !is.na(Country) & Country != "")

# Get the top 20 most frequent countries
country.top20 <- names(sort(table(av_clean$Country), decreasing = TRUE))[1:20]
print(country.top20)

# give ggplot a subset of our data (the top 20 countries) 
# map the x value to a sorted count of country
gg <- ggplot(data=subset(av,Country %in% country.top20), 
             aes(x=reorder(Country, Country, length)))

# tell ggplot we want a bar chart
gg <- gg + geom_bar(fill="#000099")

# ensure we have decent labels
gg <- gg + labs(title="Country Counts", x="Country", y="Count")

# rotate the chart to make this one more readable
gg <- gg + coord_flip()

# remove "chart junk"
gg <- gg + theme(panel.grid=element_blank(),
                 panel.background=element_blank())

# display the image
print(gg)


# Listing 3-12
# requires packages: ggplot2
# require object: av (3-4)
# Bar graph of counts by Risk
#gg <- ggplot(data=av, aes(x=Risk))
gg <- ggplot(data = av, aes(x = Risk)) +
  geom_bar(fill = "#1f78b4") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.3, size = 3.5) +
  labs(title = "'Risk' Counts", x = "Risk Score", y = "Count") +
  theme_minimal()


# remove "chart junk"
gg <- gg + theme(panel.grid=element_blank(),
                 panel.background=element_blank())
print(gg)
                          
# Listing 3-13
# requires packages: ggplot2
# require object: av (3-4)
# See corresponding output in Figure 3-4
# Bar graph of counts by Reliability
gg <- ggplot(data=av, aes(x=Reliability))+
      geom_bar(fill = "#1f78b4") +
      geom_text(stat = 'count', aes(label = ..count..), vjust = -0.3, size = 3.5) +
      labs(title = "'Reliability' Counts", x = "Reliability Score", y = "Count") +
      theme_minimal()

#remove "chart junk"
gg <- gg + theme(panel.grid=element_blank(),
                 panel.background=element_blank())
print(gg)


# Listing 3-17
# Count the occurrences of each country
country_count <- table(av$Country)

# Sort the country count in descending order
country_sorted <- sort(country_count, decreasing = TRUE)

# Extract the top 10 countries
country10 <- country_sorted[1:10]

# Now convert to a percentage by dividing by the total number of rows
country.perc10 <- as.numeric(country10) / sum(country_count)

# Print the result
print(country.perc10)
##         CN         US         TR                    DE         NL 
## 0.26518215 0.19482573 0.05396983 0.03887854 0.03848414 0.03066590 
##         RU         GB         IN    (Other) 
## 0.02453736 0.02433243 0.02118890 0.30793501 

# Listing 3-19
# require object: av (3-4)
# See corresponding output in Figure 3-8
# compute contingency table for Risk/Reliability factors which 
# produces a matrix of counts of rows that have attributes at
# each (x, y) location
rr.tab <- xtabs(~Risk+Reliability, data=av)
print(ftable(rr.tab)) # print table

# virtually identical output to pandas (below)

# graphical view
# need to use levelplot function from lattice package
library(lattice)
# cast the table into a data frame
rr.df = data.frame(table(av$Risk, av$Reliability))
# set the column names since table uses "Var1" and "Var2"
colnames(rr.df) <- c("Risk", "Reliability", "Freq")
# now create a level plot with readable labels
levelplot(Freq~Risk*Reliability, data=rr.df, main="Risk ~ Reliabilty", 
          ylab="Reliability", xlab = "Risk", shrink = c(0.5, 1),
          col.regions = colorRampPalette(c("#F5F5F5", "#01665E"))(20))


# Listing 3-21
# require object: av (3-4), lattice (3-19)
# See corresponding output in Figure 3-10
# generate random samples for risk & reliability and re-run xtab
# starting PRNG from reproducable point
set.seed(1492) # as it leads to discovery
# generate 260,000 random samples
rel=sample(1:7, 260000, replace=T)
rsk=sample(1:10, 260000, replace=T)
# cast table into data frame
tmp.df = data.frame(table(factor(rsk), factor(rel)))
colnames(tmp.df) <- c("Risk", "Reliability", "Freq")
levelplot(Freq~Reliability*Risk, data=tmp.df, main="Risk ~ Reliabilty", 
          ylab = "Reliability", xlab = "Risk", shrink = c(0.5, 1),
          col.regions = colorRampPalette(c("#F5F5F5", "#01665E"))(20))


# Listing 3-22
# require object: av (3-4), lattice (3-19)
# See corresponding output in Figure 3-11
# Create a new varible called "simpletype" 
# replacing mutiple categories with label of "Multiples"
av$simpletype <- as.character(av$Type)

# Group all nodes with mutiple categories into a new category
av$simpletype[grep(';', av$simpletype)] <- "Multiples"
# Turn it into a factor again
av$simpletype <- factor(av$simpletype)

rrt.df = data.frame(table(av$Risk, av$Reliability, av$simpletype))
colnames(rrt.df) <- c("Risk", "Reliability", "simpletype", "Freq")
levelplot(Freq ~ Reliability*Risk|simpletype, data =rrt.df, 
          main="Risk ~ Reliabilty | Type", ylab = "Risk",
          xlab = "Reliability", shrink = c(0.5, 1), 
          col.regions = colorRampPalette(c("#F5F5F5","#01665E"))(20))


# Listing 3-24
# from the existing rrt.df, filter out 'Scanning Host'
rrt.df <- subset(rrt.df, simpletype != "Scanning Host")
levelplot(Freq ~ Reliability*Risk|simpletype, data =rrt.df, 
          main="Risk ~ Reliabilty | Type", ylab = "Risk",
          xlab = "Reliability", shrink = c(0.5, 1), 
          col.regions = colorRampPalette(c("#F5F5F5","#01665E"))(20))

# Listing 3-26
# require object: av (3-4), lattice (3-19), rrt.df (3-24)
# See corresponding output in Figure 3-15
rrt.df = subset(rrt.df, 
                !(simpletype %in% c("Malware distribution",
                                    "Malware Domain")))
sprintf("Count: %d; Percent: %2.1f%%",
        sum(rrt.df$Freq),
        100*sum(rrt.df$Freq)/nrow(av))
## [1] Count: 15171; Percent: 5.9%

levelplot(Freq ~ Reliability*Risk|simpletype, data =rrt.df, 
          main="Risk ~ Reliabilty | Type", ylab = "Risk",
          xlab = "Reliability", shrink = c(0.5, 1), 
          col.regions = colorRampPalette(c("#F5F5F5","#01665E"))(20)) 


## END OF CHAPTER 3 R CODE


