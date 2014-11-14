# setwd("/Users/dphnrome/Documents/Git/MassElections")
setwd("C:/Documents and Settings/dhadley/Documents/GitHub/MassElections")

d <- read.csv("./rawData/votePercents.csv")


library(plyr)
library(ggplot2)
library(scales) # for changing from scientific notation
library(reshape2)
# Maping tools
require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("plyr")
require("RColorBrewer")
require("ggmap")


#### Map it with city-town GIS layer ####
# https://github.com/hadley/ggplot2/wiki/plotting-polygon-shapefiles
# http://www.kevjohnson.org/making-maps-in-r/
# http://www.kevjohnson.org/making-maps-in-r-part-2/


# First prepare the parcel df
mass <- readOGR(dsn="./shapefiles", layer="GISDATA_CENSUS2010TOWNS_POLY")
mass@data$id = rownames(mass@data)
names(mass)
mass.points <- fortify(mass, region="id")
mass.df <- join(mass.points, mass@data, by="id")


# Prepare data to match with "mass" gis layer
d$TOWN <- toupper(d$Municipality)

# Combine data
mass.df <- merge(mass.df, d, by="TOWN")

# Map
map <- get_map(location = "Grafton, Massachusetts", zoom=8, maptype="roadmap", color = "bw")
ggmap(map)


#### Automatic Maps ####
# uncomment to use #

# for(i in 27 : 46){
#   ggmap(map) +
#     geom_polygon(data=mass.df, aes(x=long, y=lat, group=group, fill=mass.df[,i]), colour=NA, alpha=0.7) +
#     scale_fill_gradientn(colours=(brewer.pal(9,"YlGnBu")),labels=percent) +
#     labs(fill="") +
#     theme_nothing(legend=TRUE) + ggtitle(paste("Percent of Votes for ",colnames(mass.df)[[i]],sep=""))
#   
#   ggsave(paste("./plots/Map",i,".png",sep=""), dpi=300, width=6, height=5)
# }


####  Visualize ####


lime_green = "#2ecc71"
soft_blue = "#3498db"
pinkish_red = "#e74c3c"
purple = "#9b59b6"
teele = "#1abc9c"
nice_blue = "#2980b9"

my.theme <- 
  theme(#plot.background = element_rect(fill="white"), # Remove background
    panel.grid.major = element_blank(), # Remove gridlines
    # panel.grid.minor = element_blank(), # Remove more gridlines
    # panel.border = element_blank(), # Remove border
    panel.background = element_blank(), # Remove more background
    axis.ticks = element_blank(), # Remove axis ticks
    axis.text=element_text(size=6), # Enlarge axis text font
    axis.title=element_text(size=8), # Enlarge axis title font
    plot.title=element_text(size=12) # Enlarge, left-align title
    ,axis.text.x = element_text(angle=60, hjust = 1) # Uncomment if X-axis unreadable 
  )

# add Mass to d
m = as.data.frame(mass)
d <- merge(m, d, by="TOWN")
# new variable
d$WhichGov <- ifelse(d$Baker > .5, "Baker",
                     ifelse(d$Coakley > .5, "Coakley", "No Majority"))


ggplot(d, aes(x=d$Baker)) + geom_histogram(binwidth=.03, colour="white", fill=pinkish_red) + 
  my.theme + ggtitle("Votes for Baker") + xlab("Percent of City/Town's Vote")+ylab("Number of Cities/Towns") + 
  scale_x_continuous(labels = percent)

ggsave("./plots/plot01.png", dpi=300, width=5, height=4)


ggplot(d, aes(x=d$Coakley)) + geom_histogram(binwidth=.03, colour="white", fill=nice_blue) + 
  my.theme + ggtitle("Votes for Coakley") + xlab("Percent of City/Town's Vote")+ylab("Number of Cities/Towns") + 
  scale_x_continuous(labels = percent)

ggsave("./plots/plot02.png", dpi=300, width=5, height=4)


mass.df.top <- mass.df[which(mass.df$POP2010 > 70000),]

ggplot(mass.df.top, aes(x=reorder(mass.df.top$TOWN, -mass.df.top$Coakley), y=mass.df.top$Coakley)) + geom_bar(colour="white", fill=nice_blue) + 
  my.theme + ggtitle("Votes for Coakley in Cities > 70k pop.") + xlab("Cities")+ylab("Percent of City/Town's Vote") + 
  scale_y_continuous(labels = percent)

ggsave("./plots/plot03.png", dpi=300, width=5, height=4)


Coakley <- as.data.frame(d$Coakley)
Baker <- as.data.frame(d$Baker)

Coakley <- rename(Coakley, c("d$Coakley"="votes"))
Baker <- rename(Baker, c("d$Baker"="votes"))

Coakley$Candidate <- "Coakley"
Baker$Candidate <- "Baker"

long <- rbind(Coakley, Baker)
ggplot(long, aes(votes, fill = Candidate)) + geom_density(alpha = 0.5, colour="white") +
  my.theme + ggtitle("Votes for Baker v Coakley") + xlab("Percent of City/Town's Vote")+ylab("Density") + 
  scale_x_continuous(labels = percent)

ggsave("./plots/plot04.png", dpi=300, width=5, height=4)


ggplot(d, aes(x=d$Coakley, weight=d$POP2010)) + geom_histogram(binwidth=.03, colour="white", fill=nice_blue) + 
  my.theme + ggtitle("Votes for Coakley") + xlab("Percent of City/Town's Vote")+ylab("Sum Population of Places in This Bin") + 
  scale_x_continuous(labels = percent) + scale_y_continuous(labels = comma)

ggsave("./plots/plot05.png", dpi=300, width=5, height=4)


CitiesForBaker <- d[which(d$POP2010 > 40000 & d$Baker > .5),]

ggplot(CitiesForBaker, aes(x=reorder(CitiesForBaker$TOWN, -CitiesForBaker$Baker), y=CitiesForBaker$Baker)) + geom_bar(colour="white", fill=pinkish_red) + 
  my.theme + ggtitle("Cities for Baker: the Most Populous Places") + xlab("Cities")+ylab("Percent of City/Town's Vote for Baker") + 
  scale_y_continuous(labels = percent)

ggsave("./plots/plot06.png", dpi=300, width=5, height=4)


ggplot(d, aes(x=POP2010, y=d$Coakley, color=WhichGov)) +
  geom_point(shape=1) + scale_x_log10() +
  scale_color_manual(values = c(pinkish_red, nice_blue, purple)) +
  my.theme + ggtitle("Vote By Population") + xlab("Log Of Population")+ylab("Percent of City/Town's Vote for Coakley")

ggsave("./plots/plot07.png", dpi=300, width=5, height=5)


Baker <- d[which(d$WhichGov == "Baker"),]

ggplot(Baker, aes(x=POP2010, y=Baker, color=WhichGov)) +
  geom_point(shape=1) + scale_x_log10() +
  scale_color_manual(values = c(pinkish_red, nice_blue, purple)) +
  my.theme + ggtitle("Vote By Population") + xlab("Log Of Population")+ylab("Percent of City/Town's Vote for Baker")

ggsave("./plots/plot08.png", dpi=300, width=5, height=5)


#### Add Raw Counts and 2012 data ####

# Raw counts
dRaw <- read.csv("./rawData/voteCounts.csv")
d <- merge(d, dRaw, by="Municipality")

d10 <- read.csv("./rawData/PD43__2010_Governor_General_Election.csv")
d10 <- d10[-352,]
d10 <- d10[ -c(2:3) ]
d10$City.Town <- gsub("E[.]", "East", d10$City.Town)
d10$City.Town <- gsub("N[.]", "North",d10$City.Town)
d10$City.Town <- gsub("W[.]", "West", d10$City.Town)
d10$City.Town <- gsub("S[.]", "South", d10$City.Town)
d10$City.Town <- gsub("Manchester-by-the-Sea", "Manchester", d10$City.Town)


d <- merge(d, d10, by.x="TOWN2", by.y="City.Town")


d$BakerAdds <- d$Baker.y - d$Baker..Tisei
d.top <- d[order(-d$BakerAdds),]
d.top <- d.top[-c(11:351),]
ggplot(d.top, aes(x=reorder(d.top$Municipality, -d.top$BakerAdds), y=d.top$BakerAdds)) + geom_bar(colour="white", fill=pinkish_red) + 
  my.theme + ggtitle("Largest Increase for Baker - Raw Votes") + xlab("Municipality")+ylab("Increase in Votes for Baker Since 2010") + 
  scale_y_continuous(labels = comma)### Automatic Maps ####### Automatic Maps ####

ggsave("./plots/plot09.png", dpi=300, width=5, height=4)


# Turn all raw counts from 2010 into a percent
d <- data.frame(d[,1:61], lapply(d[,62:67], function(X) X/d$Total.Votes.Cast))
d$BakerIncrease <- d$Baker.x - d$Baker..Tisei

d.top <- d[order(-d$BakerIncrease),]
d.top <- d.top[-c(11:351),]
ggplot(d.top, aes(x=reorder(d.top$Municipality, -d.top$BakerIncrease), y=d.top$BakerIncrease)) + geom_bar(colour="white", fill=pinkish_red) + 
  my.theme + ggtitle("Largest Increase for Baker - Percentage Points") + xlab("Municipality")+ylab("Increase in Votes for Baker Since 2010") + 
  scale_y_continuous(labels = percent)

ggsave("./plots/plot10.png", dpi=300, width=5, height=4)


d.top <- d[order(d$BakerIncrease),]
d.top <- d.top[-c(11:351),]
ggplot(d.top, aes(x=reorder(d.top$Municipality, d.top$BakerIncrease), y=d.top$BakerIncrease)) + geom_bar(colour="white", fill=nice_blue) + 
  my.theme + ggtitle("Smallest Increase for Baker - Percentage Points") + xlab("Municipality")+ylab("Increase in Votes for Baker Since 2010") + 
  scale_y_continuous(labels = percent)

ggsave("./plots/plot11.png", dpi=300, width=5, height=4)


# Need a map of this
# First a calulation of Independents for 2nd map below
d$Independents <- (abs(d$Baker.x - d$Coakley.x))*-1 + 
  (mean(d$Falchuk.x, d$Lively.x, d$McCormick.x, d$"Cahill..Loscocco, d$Stein..Purcell", na.rm=T, trim=0))

mass.df <- merge(mass.df, d, by="TOWN")

# Map
map <- get_map(location = "Grafton, Massachusetts", zoom=8, maptype="roadmap", color = "bw")
ggmap(map)

ggmap(map) +
  geom_polygon(data=mass.df, aes(x=long, y=lat, group=group, fill=mass.df$BakerIncrease), colour=NA, alpha=0.7) +
  scale_fill_gradientn(colours=(brewer.pal(9,"Reds")),labels=percent) +
  labs(fill="") +
  theme_nothing(legend=TRUE) + ggtitle("Increase for Baker - Percentage Points")

ggsave(paste("./plots/plot12.png"), dpi=300, width=6, height=5)


# One more map of independents

ggmap(map) +
  geom_polygon(data=mass.df, aes(x=long, y=lat, group=group, fill=mass.df$Independents), colour=NA, alpha=0.7) +
  scale_fill_gradientn(colours=(brewer.pal(9,"Purples")),labels=percent) +
  labs(fill="") +
  theme_nothing(legend=TRUE) + ggtitle("Measure of Independent Voters")

ggsave(paste("./plots/plot13.png"), dpi=300, width=6, height=5)

d.top <- d[order(-d$Independents),]
d.top <- d.top[-c(16:351),]
ggplot(d.top, aes(x=reorder(d.top$Municipality, -d.top$POP2010), y=d.top$POP2010)) + geom_bar(colour="white", fill=purple) + 
  my.theme + ggtitle("Places W/ the Most Independent Voters - by Population") + 
  xlab("Municipality") + ylab("Population") + 
  scale_y_continuous(labels = comma)

ggsave("./plots/plot14.png", dpi=300, width=5, height=4)


d.top <- d[order(d$Independents),]
d.top <- d.top[which(d.top$WhichGov == "Coakley"),]
d.top <- d.top[-c(16:351),]
ggplot(d.top, aes(x=reorder(d.top$Municipality, -d.top$POP2010), y=d.top$POP2010)) + geom_bar(colour="white", fill=nice_blue) + 
  my.theme + ggtitle("Places W/ the Most Partisan Voters - Democrat") + 
  xlab("Municipality") + ylab("Population") + 
  scale_y_continuous(labels = comma)

ggsave("./plots/plot15.png", dpi=300, width=5, height=4)


d.top <- d[order(d$Independents),]
d.top <- d.top[which(d.top$WhichGov == "Baker"),]
d.top <- d.top[-c(16:351),]
ggplot(d.top, aes(x=reorder(d.top$Municipality, -d.top$POP2010), y=d.top$POP2010)) + geom_bar(colour="white", fill=pinkish_red) + 
  my.theme + ggtitle("Places W/ the Most Partisan Voters - Republican") + 
  xlab("Municipality") + ylab("Population") + 
  scale_y_continuous(labels = comma)

ggsave("./plots/plot16.png", dpi=300, width=5, height=4)


#### Census  + Voting Visualization ####
# This brings in census data

sf1 <- read.csv("./rawData/CensusSF1/DEC_10_SF1_SF1DP1_with_ann.csv", header=T, skip=1, stringsAsFactors=FALSE)
sf1$Geography <- gsub(" Town city", "", sf1$Geography)
sf1$Geography <- gsub(" town", "", sf1$Geography)
sf1$Geography <- gsub(" Center", "", sf1$Geography)
sf1$Geography <- gsub(" city", "", sf1$Geography)
sf1$Geography <- sub("[,] .*", "", sf1$Geography)

sf1[sf1=="(X)"]<-NA

sf1$Municipality <- sf1$Geography

dsf <- merge(d, sf1, by="Municipality")

# Convert to numeric
dsf[,74:444] <- as.numeric(unlist(dsf[,74:444]))



summary(lm(dsf[,342] ~ dsf$Baker.x))$r.squared 
ggplot(dsf, aes(x=dsf[,342], 
                y=dsf$Baker.x, color=WhichGov)) +
  geom_point(shape=1) + #scale_x_log10() +
  geom_smooth(method=lm, color = "grey") +
  scale_color_manual(values = c(pinkish_red, nice_blue, purple)) +
  my.theme + ggtitle("Families For Baker") + xlab("Percent of City/Town With Children - R-Sq=.33")+
  ylab("Percent of City/Town's Vote for Baker") +
  scale_y_continuous(labels = percent)

ggsave("./plots/plot17.png", dpi=300, width=5, height=4)


summary(lm(dsf[,228] ~ dsf$Baker.x))$r.squared 
ggplot(dsf, aes(x=dsf[,228], 
                y=dsf$Baker.x, color=WhichGov)) +
  geom_point(shape=1) + #scale_x_log10() +
  geom_smooth(method=lm, color = "grey") +
  scale_color_manual(values = c(pinkish_red, nice_blue, purple)) +
  my.theme + ggtitle("Race/Baker") + xlab("Percent of City/Town White - R-Sq=.07")+
  ylab("Percent of City/Town's Vote for Baker") +
  scale_y_continuous(labels = percent)

ggsave("./plots/plot18.png", dpi=300, width=5, height=4)



sf4 <- read.csv("./rawData/CensusSF4/ACS_10_SF4_DP02_with_ann.csv", header=T, skip=1, stringsAsFactors=FALSE)
sf4$Geography <- gsub(" Town city", "", sf4$Geography)
sf4$Geography <- gsub(" town", "", sf4$Geography)
sf4$Geography <- gsub(" Center", "", sf4$Geography)
sf4$Geography <- gsub(" city", "", sf4$Geography)
sf4$Geography <- sub("[,] .*", "", sf4$Geography)

sf4[sf4=="(X)"]<-NA

sf4$Municipality <- sf4$Geography

dsf <- merge(d, sf4, by="Municipality")

# Convert to numeric
dsf[,74:603] <- as.numeric(unlist(dsf[,74:603]))


summary(lm(dsf[,341] ~ dsf$Baker.x))$r.squared 
ggplot(dsf, aes(x=dsf[,341], 
                y=dsf$Baker.x, color=WhichGov)) +
  geom_point(shape=1) + #scale_x_log10() +
  geom_smooth(method=lm, color = "grey") +
  scale_color_manual(values = c(pinkish_red, nice_blue, purple)) +
  my.theme + ggtitle("Education Not Correlated") + xlab("Percent of City/Town With Bachelor's or Higher")+
  ylab("Percent of City/Town's Vote for Baker") +
  scale_y_continuous(labels = percent)

ggsave("./plots/plot19.png", dpi=300, width=5, height=4)


dp3 <- read.csv("./rawData/CensusDP3/ACS_10_5YR_DP03_with_ann.csv", header=T, skip=1, stringsAsFactors=FALSE)
dp3$Geography <- gsub(" Town city", "", dp3$Geography)
dp3$Geography <- gsub(" town", "", dp3$Geography)
dp3$Geography <- gsub(" Center", "", dp3$Geography)
dp3$Geography <- gsub(" city", "", dp3$Geography)
dp3$Geography <- sub("[,] .*", "", dp3$Geography)

dp3[dp3=="(X)"]<-NA

dp3$Municipality <- dp3$Geography

dsf <- merge(d, dp3, by="Municipality")

# Convert to numeric
dsf[,74:620] <- as.numeric(unlist(dsf[,74:620]))

summary(lm(dsf[,317] ~ dsf$Baker.x))$r.squared 
ggplot(dsf, aes(x=dsf[,317], 
                y=dsf$Baker.x, color=WhichGov)) +
  geom_point(shape=1) + #scale_x_log10() +
  geom_smooth(method=lm, color = "grey") +
  scale_color_manual(values = c(pinkish_red, nice_blue, purple)) +
  my.theme + ggtitle("Income Slightly Correlated") + xlab("Median Income, 2010 - R-Squared: .18")+
  ylab("Percent of City/Town's Vote for Baker") +
  scale_y_continuous(labels = percent) + scale_x_continuous(labels = dollar)

ggsave("./plots/plot20.png", dpi=300, width=5, height=4)



summary(lm(dsf[,147] ~ dsf$YesQ1.x))$r.squared 
ggplot(dsf, aes(x=dsf[,147], 
                y=dsf$YesQ1.x, color=WhichGov)) +
  geom_point(shape=1) + #scale_x_log10() +
  geom_smooth(method=lm, color = "grey") +
  scale_color_manual(values = c(pinkish_red, nice_blue, purple)) +
  my.theme + ggtitle("Commute / Vote on Q1") + xlab("Percent Commuting by Car Alone - R-Squared: .27")+
  ylab("Percent of City/Town's Vote for Q1") +
  scale_y_continuous(labels = percent)

ggsave("./plots/plot21.png", dpi=300, width=5, height=4)


summary(lm(dsf[,147] ~ dsf$Baker.x))$r.squared 
ggplot(dsf, aes(x=dsf[,147], 
                y=dsf$Baker.x, color=WhichGov)) +
  geom_point(shape=1) + #scale_x_log10() +
  geom_smooth(method=lm, color = "grey") +
  scale_color_manual(values = c(pinkish_red, nice_blue, purple)) +
  my.theme + ggtitle("Commute / Vote For Baker") + xlab("Percent Commuting by Car Alone - R-Squared: .27")+
  ylab("Percent of City/Town's Vote for Baker") +
  scale_y_continuous(labels = percent)

ggsave("./plots/plot22.png", dpi=300, width=5, height=4)




#### Just a few more ####

d.top <- d[order(-d$Baker.y),]
d.top <- d.top[-c(16:351),]
ggplot(d.top, aes(x=reorder(d.top$Municipality, -d.top$Baker.y), y=d.top$Baker.y)) + geom_bar(colour="white", fill=pinkish_red) + 
  my.theme + ggtitle("Places W/ the Most Raw Votes for Baker") + 
  xlab("Municipality") + ylab("Votes for Baker") + 
  scale_y_continuous(labels = comma)

ggsave("./plots/plot23.png", dpi=300, width=5, height=4)


d.top <- d[order(-d$Coakley.y),]
d.top <- d.top[-c(16:351),]
ggplot(d.top, aes(x=reorder(d.top$Municipality, -d.top$Coakley.y), y=d.top$Coakley.y)) + geom_bar(colour="white", fill=nice_blue) + 
  my.theme + ggtitle("Places W/ the Most Raw Votes for Baker") + 
  xlab("Municipality") + ylab("Votes for Baker") + 
  scale_y_continuous(labels = comma)

ggsave("./plots/plot24.png", dpi=300, width=5, height=4)


# Map
map <- get_map(location = "Boston, Massachusetts", zoom=11, maptype="roadmap", color = "bw")
ggmap(map)

ggmap(map) +
  geom_polygon(data=mass.df, aes(x=long, y=lat, group=group, fill=mass.df$BakerIncrease), colour=NA, alpha=0.7) +
  scale_fill_gradientn(colours=(brewer.pal(9,"Reds")),labels=percent) +
  labs(fill="") +
  theme_nothing(legend=TRUE) + ggtitle("Increase for Baker - Percentage Points")

ggsave(paste("./plots/plot25.png"), dpi=300, width=6, height=5)































