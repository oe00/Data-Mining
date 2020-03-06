

if (!is.element("data.table", installed.packages()[,1])) 
    install.packages("data.table", repos="http://cran.r-project.org")
library(data.table)

if (!is.element("dplyr", installed.packages()[,1])) 
    install.packages("dplyr", repos="http://cran.r-project.org")
library(dplyr)

if (!is.element("ggplot2", installed.packages()[,1])) 
    install.packages("ggplot2", repos="http://cran.r-project.org")
library(ggplot2)

if (!is.element("leaflet", installed.packages()[,1]))
    install.packages("leaflet", repos="http://cran.r-project.org")
library(leaflet)

# The survey dataset is acquired from https://osmihelp.org/research
survey =  read.csv(file="OSMI Mental Health in Tech Survey 2018.csv", header = T, sep =",") #import OSMI Mental Health in Tech Survey 2018.csv
head(survey)

countries = read.csv(file="countries.csv", header = T, sep =",") # import countries.csv
head(countries)

survey = select(survey,ID,Are.you.self.employed.,What.is.your.gender.,What.is.your.age.,What.country.do.you.live.in.,How.many.employees.does.your.company.or.organization.have.,Overall..how.much.importance.does.your.employer.place.on.mental.health.)
head(survey)

setnames(survey,old=c("ID","Are.you.self.employed.","What.is.your.gender.","What.is.your.age.","What.country.do.you.live.in.","How.many.employees.does.your.company.or.organization.have.","Overall..how.much.importance.does.your.employer.place.on.mental.health."),new=c("id","is_self_employed","gender","age","country","organization_size","mental_health"))
head(survey)

head(survey, 1)
will_mutate = survey %>% filter(country=="United States of America") # find United States of America rows
rest = setdiff(survey,will_mutate) # extract them from main dataset
mutated = will_mutate %>% mutate(country = "United States") # refactor United States of America to United States
survey = rbind(rest,mutated) # merge mutated entries with the main dataset

head(countries, 1)

survey = left_join(survey,countries,by=c("country"="country_name")) # left join tables according to country column
head(survey)

# write your descriptive statistics code here.
summary(survey)

unique(survey$gender)

males = survey %>% filter(tolower(gender) == "male" | tolower(gender) == "m") # filter males & m
females =  survey %>% filter(tolower(gender) == "female" | tolower(gender) == "f") # filter females & f
others = setdiff(survey,males) # substract males from the survey, set it to new data.table
others = setdiff(others,females) # substract females from the data.table without males

males = males %>% mutate(gender=0) # refactor males
females = females %>% mutate(gender=1) # refactor females
others = others %>% mutate(gender=2) # refactor others

survey = rbind(males,females,others) # merge males, females and others, assign it to survey
survey$gender = factor(survey$gender) # convert gender to factor (ggplot requires factor to display the graph)

head(survey)

counts = survey %>% count(country) # calculate the number of survey entries per country
head(counts)

leaf_map = left_join(counts,survey) # merge counts with countries
leaf_map = unique(select(leaf_map,country,n,latitude,longitude)) # select country name, count, cordinates, apply unique so that we end up 1 entry per country
leaf_map # table having count and cordinates of the countries that is going to be used in leaflet function


# Either as circles or markers in leaflet, put the data on the map. You can show the numbers as popups or change the circle size according to the number of survey entries.
map = leaflet() %>% addTiles() %>% addMarkers(lat=leaf_map$latitude, lng=leaf_map$longitude, popup= paste(as.character(leaf_map$country),":",as.character(leaf_map$n)))
map

survey = survey %>% filter(!is.na(organization_size) & organization_size != "")
averages = survey %>% group_by(organization_size,gender) %>% summarize(mental_health_mean = mean(mental_health, na.rm = TRUE))
head(averages) # gives mental health mean for each gender factor per organization

# Plot the averages per organization size and gender.
ggplot(averages, aes(x=organization_size, y=mental_health_mean, fill=gender)) + geom_bar(stat="identity", position=position_dodge()) + xlab("Organization Size") + ylab("Mental Health Importance") +  scale_fill_discrete(name = "Gender", labels = c("Male", "Female", "Others"))
