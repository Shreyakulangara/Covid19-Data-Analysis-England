
#Load csv file
Covid19_dataset <- read.csv("covid19_analysis_dataset.csv", stringsAsFactors = FALSE)

head(Covid19_dataset)    # Inspect top 6 rows of the data
tail(Covid19_dataset) # Inspect last 6 rows of the data
str(Covid19_dataset) #Structure 
nrow(Covid19_dataset) #number of observations
ncol(Covid19_dataset) #number of variables
names(Covid19_dataset) #list names of variables


# check for missing data
apply(Covid19_dataset, MARGIN = 2, FUN = function(x) sum(is.na(x)))
library(Amelia)
missmap(Covid19_dataset, col = c("black", "grey"), legend = FALSE)
Covid19_dataset <- na.omit(Covid19_dataset) # remove any missing data

#Summary statistics
summary(Covid19_dataset)


# Measures of Central Tendency and Dispersion on Covid19 Total Death
max(Covid19_dataset$Total_Death)
min(Covid19_dataset$Total_Death)
range <- max(Covid19_dataset$Total_Death) - min(Covid19_dataset$Total_Death)
print(paste("Range:", range))
mean(Covid19_dataset$Total_Death)
median(Covid19_dataset$Total_Death)
sd (Covid19_dataset$Total_Death)
quantiles <- quantile(Covid19_dataset$Total_Death, c(0.25, 0.75), na.rm = TRUE)
iqr_value <- diff(quantiles)
print(paste("Interquartile Range (IQR):", iqr_value))


##### CHECK FOR NORMALITY OF DEPENDENT VARIABLE - Total_Death ####

#### box plot ####

library(TeachingDemos)
# Load the function
source("https://raw.githubusercontent.com/talgalili/R-code-snippets/master/boxplot.with.outlier.label.r")
boxplot.with.outlier.label(Covid19_dataset$Total_Death, 
                           main = "Box plot - Total Deaths due to COVID-19", 
                           xlab = "Total_Death",
                           ylab = "Count",
                           seq_along(Covid19_dataset$Total_Death),
                           col = "orange",
                           border = "brown",
                           notch = TRUE
)

# inspect outliers
Covid19_dataset[82,]
Covid19_dataset[263,]
Covid19_dataset[47,]
Covid19_dataset[130,]

#### q-q plot ####

qqnorm(Covid19_dataset$Total_Death, xlab = "Theoretical Quantiles: Total_Death",
       main = "Q-Q Plot for Total Deaths due to COVID-19")
qqline(Covid19_dataset$Total_Death, col = 2)

#### histogram ####

hist(Covid19_dataset$Total_Death, col = "light blue", 
     border = "dark blue", freq = T, ylim = c(0,100),
     xlab = "Total_Death", 
     main = "Histogram for Total Deaths due to COVID-19")
rug (Covid19_dataset$Total_Death)
# probability density histogram
hist(Covid19_dataset$Total_Death, main = "Probabilty density histogram of Total Deaths due to COVID-19", 
     xlab = "Total_Death", ylim = c(0,0.7),col = "skyblue", prob = TRUE, freq= F)

lines(density(Covid19_dataset$Total_Death), col = "blue", lwd = 2)
rug (Covid19_dataset$Total_Death)

xfit <- seq(from = min(Covid19_dataset$Total_Death), to = max(Covid19_dataset$Total_Death), by = 0.1)
yfit = dnorm(xfit, mean(Covid19_dataset$Total_Death), sd(Covid19_dataset$Total_Death))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)

# Plot histogram and normal approximation
library(rcompanion)
plotNormalHistogram(Covid19_dataset$Total_Death, 
                    main = "Distribution of COVID-19 Total Death numbers", 
                    xlab = "Total_Death"
                    )

#### ks test ####
#Null Hypothesis: The Total_Death is normally distributed.
ks.test(Covid19_dataset$Total_Death,"pnorm", 
        mean(Covid19_dataset$Total_Death), 
        sd(Covid19_dataset$Total_Death))
# p-value = 0.5364 >0.05 NORMAL

#### Shapiro-Wilk's test ####
shapiro.test(Covid19_dataset$Total_Death) 
# p-value: 1.85e-08 < 0.05 NOT NORMAL
#shapiro test is appropriate for small sample size

####  Cramér-von Mises test #### 
library(goftest)
cvm.test(Covid19_dataset$Total_Death, "pnorm", mean = mean(Covid19_dataset$Total_Death), 
         sd = sd(Covid19_dataset$Total_Death))
#p-value = 0.5887 >0.05 NORMAL

####  Linear Regression #### 
model0 <- lm(Covid19_dataset$Total_Death ~ Covid19_dataset$Age0to7)
summary(model0)
# add regression line to scatter plot
plot(Covid19_dataset$Age0to7, Covid19_dataset$Total_Death, main = "Scatterplot",
     xlab = "Age0to7", ylab = "Total_Death")
abline(coef(model0), col = "red")

plot(model0, which = 2)  # Normal Q-Q plot

hist(model0$residuals)
rug(model0$residuals)

ks.test(model0$residuals, "pnorm", mean(model0$residuals), sd(model0$residuals))
#p-value = 0.5018 > 0.05 normal

#z-score calculated for dependent variable- Covid19_dataset$Total_Death
zTotal_Death<-(Covid19_dataset$Total_Death - mean(Covid19_dataset$Total_Death)) / sd(Covid19_dataset$Total_Death)

ks.test(zTotal_Death,"pnorm", 
        mean(zTotal_Death), 
        sd(zTotal_Death))
cvm.test(zTotal_Death, "pnorm", mean = mean(zTotal_Death), sd = sd(zTotal_Death))

qqnorm(zTotal_Death, xlab = "Theoretical Quantiles: Total_Death",
       main = "Q-Q Plot for Total Deaths due to COVID-19")
qqline(zTotal_Death, col = 2)

#Boxplot of all independent variables
op <- par(mar = c(5, 8, 4, 2) + 0.1)
boxplot(Covid19_dataset$Age0to7, Covid19_dataset$Age8to14,
                           Covid19_dataset$Age15to17, Covid19_dataset$Age18to24,
                           Covid19_dataset$Age25to44,Covid19_dataset$Age45to64,
                           Covid19_dataset$Age65to84,Covid19_dataset$Age85andover,
                           Covid19_dataset$White,Covid19_dataset$Mixed,
                           Covid19_dataset$Asian,Covid19_dataset$Black,
                           Covid19_dataset$OtherEthinicGroup,Covid19_dataset$Verygoodhealth,
                           Covid19_dataset$Goodhealth,Covid19_dataset$Fairhealth,
                           Covid19_dataset$Badhealth,Covid19_dataset$Verybadhealth,
                           Covid19_dataset$Males,Covid19_dataset$Females,
                           main = "Box plot - Independent Variables", 
                           names = c("Age0to7","Age8to14",
                                    "Age15to17","Age18to24",
                                    "Age25to44","Age45to64",
                                    "Age65to84","Age85andover",
                                    "White","Mixed",
                                    "Asian","Black",
                                    "OtherEthinicGroup","Verygoodhealth",
                                    "Goodhealth","Fairhealth",
                                    "Badhealth","Verybadhealth",
                                    "Males","Females"),
                          xlab = "Independent Variables",
                           ylab = "Count",
                          col = "orange",
                           border = "brown",
                           notch = TRUE,
                            las = 1, cex.axis = 0.4
)
par(op)

# check normality - Age0to7
#### q-q plot ####
qqnorm(Covid19_dataset$Age0to7, xlab = "Age0to7" )
qqline(Covid19_dataset$Age0to7, col=2) 
#### histogram ####
hist(Covid19_dataset$Age0to7, col = "light blue", border = "dark blue", freq = T, ylim = c(0,200),
     xlab = "Age0to7", main = "Histogram")
rug (Covid19_dataset$Age0to7)

# probability density histogram
hist(Covid19_dataset$Age0to7, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.10),
     xlab = "Age0to7", main = "Histogram")

# Add a rug plot
rug (Covid19_dataset$Age0to7)

# Add a density curve
lines (density(sort(Covid19_dataset$Age0to7)))


xfit <- seq(from = min(Covid19_dataset$Age0to7), to = max(Covid19_dataset$Age0to7), by = 0.1)
yfit = dnorm(xfit, mean(Covid19_dataset$Age0to7), sd(Covid19_dataset$Age0to7))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
#### ks test ####
ks.test(Covid19_dataset$Age0to7,"pnorm", mean(Covid19_dataset$Age0to7), sd(Covid19_dataset$Age0to7))

#->Based on above tests Age0to7 is normally distributed

# check normality - Age8to14
#### q-q plot ####
qqnorm(Covid19_dataset$Age8to14, xlab = "Age8to14" )
qqline(Covid19_dataset$Age8to14, col=2) 
#### histogram ####
hist(Covid19_dataset$Age8to14, col = "light blue", border = "dark blue", freq = T, ylim = c(0,200),
     xlab = "Age8to14", main = "Histogram")
rug (Covid19_dataset$Age8to14)

# probability density histogram
hist(Covid19_dataset$Age8to14, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.10),
     xlab = "Age8to14", main = "Histogram")

# Add a rug plot
rug (Covid19_dataset$Age8to14)

# Add a density curve
lines (density(sort(Covid19_dataset$Age8to14)))


xfit <- seq(from = min(Covid19_dataset$Age8to14), to = max(Covid19_dataset$Age8to14), by = 0.1)
yfit = dnorm(xfit, mean(Covid19_dataset$Age8to14), sd(Covid19_dataset$Age8to14))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
#### ks test ####
ks.test(Covid19_dataset$Age8to14,"pnorm", mean(Covid19_dataset$Age8to14), sd(Covid19_dataset$Age8to14))

#->Based on above tests Age8to14 is normally distributed

# check normality - Age15to17
#### q-q plot ####
qqnorm(Covid19_dataset$Age15to17, xlab = "Age15to17" )
qqline(Covid19_dataset$Age15to17, col=2) 
#### histogram ####
hist(Covid19_dataset$Age15to17, col = "light blue", border = "dark blue", freq = T, ylim = c(0,200),
     xlab = "Age15to17", main = "Histogram")
rug (Covid19_dataset$Age15to17)

# probability density histogram
hist(Covid19_dataset$Age15to17, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.20),
     xlab = "Age15to17", main = "Histogram")

# Add a rug plot
rug (Covid19_dataset$Age15to17)

# Add a density curve
lines (density(sort(Covid19_dataset$Age15to17)))


xfit <- seq(from = min(Covid19_dataset$Age15to17), to = max(Covid19_dataset$Age15to17), by = 0.1)
yfit = dnorm(xfit, mean(Covid19_dataset$Age15to17), sd(Covid19_dataset$Age15to17))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
#### ks test ####
ks.test(Covid19_dataset$Age15to17,"pnorm", mean(Covid19_dataset$Age15to17), sd(Covid19_dataset$Age15to17))

#->Based on above tests Age15to17 is not normally distributed

#To normalise Age15to17 
lAge15to17 <-log(Covid19_dataset$Age15to17)

# check normality - Age18to24
#### q-q plot ####
qqnorm(Covid19_dataset$Age18to24, xlab = "Age18to24" )
qqline(Covid19_dataset$Age18to24, col=2) 
#### histogram ####
hist(Covid19_dataset$Age18to24, col = "light blue", border = "dark blue", freq = T, ylim = c(0,200),
     xlab = "Age18to24", main = "Histogram")
rug (Covid19_dataset$Age18to24)

# probability density histogram
hist(Covid19_dataset$Age18to24, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.10),
     xlab = "Age18to24", main = "Histogram")

# Add a rug plot
rug (Covid19_dataset$Age18to24)

# Add a density curve
lines (density(sort(Covid19_dataset$Age18to24)))


xfit <- seq(from = min(Covid19_dataset$Age18to24), to = max(Covid19_dataset$Age18to24), by = 0.1)
yfit = dnorm(xfit, mean(Covid19_dataset$Age18to24), sd(Covid19_dataset$Age18to24))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
#### ks test ####
ks.test(Covid19_dataset$Age18to24,"pnorm", mean(Covid19_dataset$Age18to24), sd(Covid19_dataset$Age18to24))

#->Based on above tests Age18to24 is not normally distributed

#To normalise Age18to24 
lAge18to24 <-log(Covid19_dataset$Age18to24)

# check normality - Age25to44
#### q-q plot ####
qqnorm(Covid19_dataset$Age25to44, xlab = "Age25to44" )
qqline(Covid19_dataset$Age25to44, col=2) 
#### histogram ####
hist(Covid19_dataset$Age25to44, col = "light blue", border = "dark blue", freq = T, ylim = c(0,200),
     xlab = "Age25to44", main = "Histogram")
rug (Covid19_dataset$Age25to44)

# probability density histogram
hist(Covid19_dataset$Age25to44, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.10),
     xlab = "Age25to44", main = "Histogram")

# Add a rug plot
rug (Covid19_dataset$Age25to44)

# Add a density curve
lines (density(sort(Covid19_dataset$Age25to44)))


xfit <- seq(from = min(Covid19_dataset$Age25to44), to = max(Covid19_dataset$Age25to44), by = 0.1)
yfit = dnorm(xfit, mean(Covid19_dataset$Age25to44), sd(Covid19_dataset$Age25to44))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
#### ks test ####
ks.test(Covid19_dataset$Age25to44,"pnorm", mean(Covid19_dataset$Age25to44), sd(Covid19_dataset$Age25to44))

#->Based on above tests Age25to44 is not normally distributed
#To normalise Age25to44 
lAge25to44 <-log(Covid19_dataset$Age25to44)

# check normality - Age45to64
#### q-q plot ####
qqnorm(Covid19_dataset$Age45to64, xlab = "Age45to64" )
qqline(Covid19_dataset$Age45to64, col=2) 
#### histogram ####
hist(Covid19_dataset$Age45to64, col = "light blue", border = "dark blue", freq = T, ylim = c(0,200),
     xlab = "Age45to64", main = "Histogram")
rug (Covid19_dataset$Age45to64)

# probability density histogram
hist(Covid19_dataset$Age45to64, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.10),
     xlab = "Age45to64", main = "Histogram")

# Add a rug plot
rug (Covid19_dataset$Age45to64)

# Add a density curve
lines (density(sort(Covid19_dataset$Age45to64)))


xfit <- seq(from = min(Covid19_dataset$Age45to64), to = max(Covid19_dataset$Age45to64), by = 0.1)
yfit = dnorm(xfit, mean(Covid19_dataset$Age45to64), sd(Covid19_dataset$Age45to64))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
#### ks test ####
ks.test(Covid19_dataset$Age45to64,"pnorm", mean(Covid19_dataset$Age45to64), sd(Covid19_dataset$Age45to64))

#->Based on above tests Age45to64 is not normally distributed
#To normalise Age45to64 
lAge45to64 <-log(Covid19_dataset$Age45to64)

# check normality - Age65to84
#### q-q plot ####
qqnorm(Covid19_dataset$Age65to84, xlab = "Age65to84" )
qqline(Covid19_dataset$Age65to84, col=2) 
#### histogram ####
hist(Covid19_dataset$Age65to84, col = "light blue", border = "dark blue", freq = T, ylim = c(0,200),
     xlab = "Age65to84", main = "Histogram")
rug (Covid19_dataset$Age65to84)

# probability density histogram
hist(Covid19_dataset$Age65to84, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.10),
     xlab = "Age65to84", main = "Histogram")

# Add a rug plot
rug (Covid19_dataset$Age65to84)

# Add a density curve
lines (density(sort(Covid19_dataset$Age65to84)))


xfit <- seq(from = min(Covid19_dataset$Age65to84), to = max(Covid19_dataset$Age65to84), by = 0.1)
yfit = dnorm(xfit, mean(Covid19_dataset$Age65to84), sd(Covid19_dataset$Age65to84))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
#### ks test ####
ks.test(Covid19_dataset$Age65to84,"pnorm", mean(Covid19_dataset$Age65to84), sd(Covid19_dataset$Age65to84))

#->Based on above tests Age65to84 is normally distributed

# check normality - Age85andover
#### q-q plot ####
qqnorm(Covid19_dataset$Age85andover, xlab = "Age85andover" )
qqline(Covid19_dataset$Age85andover, col=2) 
#### histogram ####
hist(Covid19_dataset$Age85andover, col = "light blue", border = "dark blue", freq = T, ylim = c(0,200),
     xlab = "Age85andover", main = "Histogram")
rug (Covid19_dataset$Age85andover)

# probability density histogram
hist(Covid19_dataset$Age85andover, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.10),
     xlab = "Age85andover", main = "Histogram")

# Add a rug plot
rug (Covid19_dataset$Age85andover)

# Add a density curve
lines (density(sort(Covid19_dataset$Age85andover)))


xfit <- seq(from = min(Covid19_dataset$Age85andover), to = max(Covid19_dataset$Age85andover), by = 0.1)
yfit = dnorm(xfit, mean(Covid19_dataset$Age85andover), sd(Covid19_dataset$Age85andover))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
#### ks test ####
ks.test(Covid19_dataset$Age85andover,"pnorm", mean(Covid19_dataset$Age85andover), sd(Covid19_dataset$Age85andover))

#->Based on above tests Age85andover is normally distributed

# check normality - White
#### q-q plot ####
qqnorm(Covid19_dataset$White, xlab = "White" )
qqline(Covid19_dataset$White, col=2) 
#### histogram ####
hist(Covid19_dataset$White, col = "light blue", border = "dark blue", freq = T, ylim = c(0,200),
     xlab = "White", main = "Histogram")
rug (Covid19_dataset$White)

# probability density histogram
hist(Covid19_dataset$White, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.10),
     xlab = "White", main = "Histogram")

# Add a rug plot
rug (Covid19_dataset$White)

# Add a density curve
lines (density(sort(Covid19_dataset$White)))


xfit <- seq(from = min(Covid19_dataset$White), to = max(Covid19_dataset$White), by = 0.1)
yfit = dnorm(xfit, mean(Covid19_dataset$White), sd(Covid19_dataset$White))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
#### ks test ####
ks.test(Covid19_dataset$White,"pnorm", mean(Covid19_dataset$White), sd(Covid19_dataset$White))

#->Based on above tests White is not normally distributed
#To normalise White 
lWhite <-log(Covid19_dataset$White)

# check normality - Mixed
#### q-q plot ####
qqnorm(Covid19_dataset$Mixed, xlab = "Mixed" )
qqline(Covid19_dataset$Mixed, col=2) 
#### histogram ####
hist(Covid19_dataset$Mixed, col = "light blue", border = "dark blue", freq = T, ylim = c(0,200),
     xlab = "Mixed", main = "Histogram")
rug (Covid19_dataset$Mixed)

# probability density histogram
hist(Covid19_dataset$Mixed, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.10),
     xlab = "Mixed", main = "Histogram")

# Add a rug plot
rug (Covid19_dataset$Mixed)

# Add a density curve
lines (density(sort(Covid19_dataset$Mixed)))


xfit <- seq(from = min(Covid19_dataset$Mixed), to = max(Covid19_dataset$Mixed), by = 0.1)
yfit = dnorm(xfit, mean(Covid19_dataset$Mixed), sd(Covid19_dataset$Mixed))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
#### ks test ####
ks.test(Covid19_dataset$Mixed,"pnorm", mean(Covid19_dataset$Mixed), sd(Covid19_dataset$Mixed))

#->Based on above tests Mixed is not normally distributed
#To normalise Mixed 
lMixed <-log(Covid19_dataset$Mixed)

# check normality - Asian
#### q-q plot ####
qqnorm(Covid19_dataset$Asian, xlab = "Asian" )
qqline(Covid19_dataset$Asian, col=2) 
#### histogram ####
hist(Covid19_dataset$Asian, col = "light blue", border = "dark blue", freq = T, ylim = c(0,300),
     xlab = "Asian", main = "Histogram")
rug (Covid19_dataset$Asian)

# probability density histogram
hist(Covid19_dataset$Asian, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.05),
     xlab = "Asian", main = "Histogram")

# Add a rug plot
rug (Covid19_dataset$Asian)

# Add a density curve
lines (density(sort(Covid19_dataset$Asian)))


xfit <- seq(from = min(Covid19_dataset$Asian), to = max(Covid19_dataset$Asian), by = 0.1)
yfit = dnorm(xfit, mean(Covid19_dataset$Asian), sd(Covid19_dataset$Asian))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
#### ks test ####
ks.test(Covid19_dataset$Asian,"pnorm", mean(Covid19_dataset$Asian), sd(Covid19_dataset$Asian))

#->Based on above tests Asian is not normally distributed
#To normalise Asian 
lAsian <-log(Covid19_dataset$Asian)

# check normality - Black
#### q-q plot ####
qqnorm(Covid19_dataset$Black, xlab = "Black" )
qqline(Covid19_dataset$Black, col=2) 
#### histogram ####
hist(Covid19_dataset$Black, col = "light blue", border = "dark blue", freq = T, ylim = c(0,300),
     xlab = "Black", main = "Histogram")
rug (Covid19_dataset$Black)

# probability density histogram
hist(Covid19_dataset$Black, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.05),
     xlab = "Black", main = "Histogram")

# Add a rug plot
rug (Covid19_dataset$Black)

# Add a density curve
lines (density(sort(Covid19_dataset$Black)))


xfit <- seq(from = min(Covid19_dataset$Black), to = max(Covid19_dataset$Black), by = 0.1)
yfit = dnorm(xfit, mean(Covid19_dataset$Black), sd(Covid19_dataset$Black))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
#### ks test ####
ks.test(Covid19_dataset$Black,"pnorm", mean(Covid19_dataset$Black), sd(Covid19_dataset$Black))

#->Based on above tests Black is not normally distributed
#To normalise Black 
lBlack <-log(Covid19_dataset$Black)

# check normality - OtherEthinicGroup
#### q-q plot ####
qqnorm(Covid19_dataset$OtherEthinicGroup, xlab = "OtherEthinicGroup" )
qqline(Covid19_dataset$OtherEthinicGroup, col=2) 
#### histogram ####
hist(Covid19_dataset$OtherEthinicGroup, col = "light blue", border = "dark blue", freq = T, ylim = c(0,300),
     xlab = "OtherEthinicGroup", main = "Histogram")
rug (Covid19_dataset$OtherEthinicGroup)

# probability density histogram
hist(Covid19_dataset$OtherEthinicGroup, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.10),
     xlab = "OtherEthinicGroup", main = "Histogram")

# Add a rug plot
rug (Covid19_dataset$OtherEthinicGroup)

# Add a density curve
lines (density(sort(Covid19_dataset$OtherEthinicGroup)))


xfit <- seq(from = min(Covid19_dataset$OtherEthinicGroup), to = max(Covid19_dataset$OtherEthinicGroup), by = 0.1)
yfit = dnorm(xfit, mean(Covid19_dataset$OtherEthinicGroup), sd(Covid19_dataset$OtherEthinicGroup))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
#### ks test ####
ks.test(Covid19_dataset$OtherEthinicGroup,"pnorm", mean(Covid19_dataset$OtherEthinicGroup), sd(Covid19_dataset$OtherEthinicGroup))

#->Based on above tests OtherEthinicGroup is not normally distributed
#To normalise OtherEthinicGroup 
lOtherEthinicGroup <-log(Covid19_dataset$OtherEthinicGroup)

# check normality - Verygoodhealth
#### q-q plot ####
qqnorm(Covid19_dataset$Verygoodhealth, xlab = "Verygoodhealth" )
qqline(Covid19_dataset$Verygoodhealth, col=2) 
#### histogram ####
hist(Covid19_dataset$Verygoodhealth, col = "light blue", border = "dark blue", freq = T, ylim = c(0,200),
     xlab = "Verygoodhealth", main = "Histogram")
rug (Covid19_dataset$Verygoodhealth)

# probability density histogram
hist(Covid19_dataset$Verygoodhealth, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.10),
     xlab = "Verygoodhealth", main = "Histogram")

# Add a rug plot
rug (Covid19_dataset$Verygoodhealth)

# Add a density curve
lines (density(sort(Covid19_dataset$Verygoodhealth)))

xfit <- seq(from = min(Covid19_dataset$Verygoodhealth), to = max(Covid19_dataset$Verygoodhealth), by = 0.1)
yfit = dnorm(xfit, mean(Covid19_dataset$Verygoodhealth), sd(Covid19_dataset$Verygoodhealth))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
#### ks test ####
ks.test(Covid19_dataset$Verygoodhealth,"pnorm", mean(Covid19_dataset$Verygoodhealth), sd(Covid19_dataset$Verygoodhealth))

#->Based on above tests Verygoodhealth is normally distributed

# check normality - Goodhealth
#### q-q plot ####
qqnorm(Covid19_dataset$Goodhealth, xlab = "Goodhealth" )
qqline(Covid19_dataset$Goodhealth, col=2) 
#### histogram ####
hist(Covid19_dataset$Goodhealth, col = "light blue", border = "dark blue", freq = T, ylim = c(0,200),
     xlab = "Goodhealth", main = "Histogram")
rug (Covid19_dataset$Goodhealth)

# probability density histogram
hist(Covid19_dataset$Goodhealth, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.10),
     xlab = "Goodhealth", main = "Histogram")

# Add a rug plot
rug (Covid19_dataset$Goodhealth)

# Add a density curve
lines (density(sort(Covid19_dataset$Goodhealth)))


xfit <- seq(from = min(Covid19_dataset$Goodhealth), to = max(Covid19_dataset$Goodhealth), by = 0.1)
yfit = dnorm(xfit, mean(Covid19_dataset$Goodhealth), sd(Covid19_dataset$Goodhealth))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
#### ks test ####
ks.test(Covid19_dataset$Goodhealth,"pnorm", mean(Covid19_dataset$Goodhealth), sd(Covid19_dataset$Goodhealth))

#->Based on above tests Goodhealth is normally distributed

# check normality - Fairhealth
#### q-q plot ####
qqnorm(Covid19_dataset$Fairhealth, xlab = "Fairhealth" )
qqline(Covid19_dataset$Fairhealth, col=2) 
#### histogram ####
hist(Covid19_dataset$Fairhealth, col = "light blue", border = "dark blue", freq = T, ylim = c(0,200),
     xlab = "Fairhealth", main = "Histogram")
rug (Covid19_dataset$Fairhealth)

# probability density histogram
hist(Covid19_dataset$Fairhealth, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.10),
     xlab = "Fairhealth", main = "Histogram")

# Add a rug plot
rug (Covid19_dataset$Fairhealth)

# Add a density curve
lines (density(sort(Covid19_dataset$Fairhealth)))


xfit <- seq(from = min(Covid19_dataset$Fairhealth), to = max(Covid19_dataset$Fairhealth), by = 0.1)
yfit = dnorm(xfit, mean(Covid19_dataset$Fairhealth), sd(Covid19_dataset$Fairhealth))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
#### ks test ####
ks.test(Covid19_dataset$Fairhealth,"pnorm", mean(Covid19_dataset$Fairhealth), sd(Covid19_dataset$Fairhealth))

#->Based on above tests Fairhealth is normally distributed

# check normality - Badhealth
#### q-q plot ####
qqnorm(Covid19_dataset$Badhealth, xlab = "Badhealth" )
qqline(Covid19_dataset$Badhealth, col=2) 
#### histogram ####
hist(Covid19_dataset$Badhealth, col = "light blue", border = "dark blue", freq = T, ylim = c(0,200),
     xlab = "Badhealth", main = "Histogram")
rug (Covid19_dataset$Badhealth)

# probability density histogram
hist(Covid19_dataset$Badhealth, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.10),
     xlab = "Badhealth", main = "Histogram")

# Add a rug plot
rug (Covid19_dataset$Badhealth)

# Add a density curve
lines (density(sort(Covid19_dataset$Badhealth)))


xfit <- seq(from = min(Covid19_dataset$Badhealth), to = max(Covid19_dataset$Badhealth), by = 0.1)
yfit = dnorm(xfit, mean(Covid19_dataset$Badhealth), sd(Covid19_dataset$Badhealth))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
#### ks test ####
ks.test(Covid19_dataset$Badhealth,"pnorm", mean(Covid19_dataset$Badhealth), sd(Covid19_dataset$Badhealth))

#->Based on above tests Badhealth is normally distributed

# check normality - Verybadhealth
#### q-q plot ####
qqnorm(Covid19_dataset$Verybadhealth, xlab = "Verybadhealth" )
qqline(Covid19_dataset$Verybadhealth, col=2) 
#### histogram ####
hist(Covid19_dataset$Verybadhealth, col = "light blue", border = "dark blue", freq = T, ylim = c(0,200),
     xlab = "Verybadhealth", main = "Histogram")
rug (Covid19_dataset$Verybadhealth)

# probability density histogram
hist(Covid19_dataset$Verybadhealth, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.5),
     xlab = "Verybadhealth", main = "Histogram")

# Add a rug plot
rug (Covid19_dataset$Verybadhealth)

# Add a density curve
lines (density(sort(Covid19_dataset$Verybadhealth)))


xfit <- seq(from = min(Covid19_dataset$Verybadhealth), to = max(Covid19_dataset$Verybadhealth), by = 0.1)
yfit = dnorm(xfit, mean(Covid19_dataset$Verybadhealth), sd(Covid19_dataset$Verybadhealth))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
#### ks test ####
ks.test(Covid19_dataset$Verybadhealth,"pnorm", mean(Covid19_dataset$Verybadhealth), sd(Covid19_dataset$Verybadhealth))

#->Based on above tests Verybadhealth is normally distributed

# check normality - Males
#### q-q plot ####
qqnorm(Covid19_dataset$Males, xlab = "Males" )
qqline(Covid19_dataset$Males, col=2) 
#### histogram ####
hist(Covid19_dataset$Males, col = "light blue", border = "dark blue", freq = T, ylim = c(0,200),
     xlab = "Males", main = "Histogram")
rug (Covid19_dataset$Males)

# probability density histogram
hist(Covid19_dataset$Males, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.10),
     xlab = "Males", main = "Histogram")

# Add a rug plot
rug (Covid19_dataset$Males)

# Add a density curve
lines (density(sort(Covid19_dataset$Males)))


xfit <- seq(from = min(Covid19_dataset$Males), to = max(Covid19_dataset$Males), by = 0.1)
yfit = dnorm(xfit, mean(Covid19_dataset$Males), sd(Covid19_dataset$Males))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
#### ks test ####
ks.test(Covid19_dataset$Males,"pnorm", mean(Covid19_dataset$Males), sd(Covid19_dataset$Males))

#->Based on above tests Males is not normally distributed
#To normalise Males 
lMales <-log(Covid19_dataset$Males)

# check normality - Females
#### q-q plot ####
qqnorm(Covid19_dataset$Females, xlab = "Females" )
qqline(Covid19_dataset$Females, col=2) 
#### histogram ####
hist(Covid19_dataset$Females, col = "light blue", border = "dark blue", freq = T, ylim = c(0,200),
     xlab = "Females", main = "Histogram")
rug (Covid19_dataset$Females)

# probability density histogram
hist(Covid19_dataset$Females, col = "light blue", border = "dark blue", freq = F, ylim = c(0,0.10),
     xlab = "Females", main = "Histogram")

# Add a rug plot
rug (Covid19_dataset$Females)

# Add a density curve
lines (density(sort(Covid19_dataset$Females)))


xfit <- seq(from = min(Covid19_dataset$Females), to = max(Covid19_dataset$Females), by = 0.1)
yfit = dnorm(xfit, mean(Covid19_dataset$FeFemales), sd(Covid19_dataset$Females))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)

# Add a legend
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)
#### ks test ####
ks.test(Covid19_dataset$Females,"pnorm", mean(Covid19_dataset$Females), sd(Covid19_dataset$Females))

#->Based on above tests Females is not normally distributed
#To normalise Females 
lFemales <-log(Covid19_dataset$Females)


Covid_dataset_independent_var<-data.frame(Covid19_dataset$Age0to7,Covid19_dataset$Age8to14,
                         lAge15to17, lAge18to24,
                         lAge25to44, lAge45to64,
                         Covid19_dataset$Age65to84, Covid19_dataset$Age85andover,
                         lWhite, lMixed,
                         lAsian,lBlack,
                         lOtherEthinicGroup,Covid19_dataset$Verygoodhealth,
                         Covid19_dataset$Goodhealth,Covid19_dataset$Fairhealth,
                         Covid19_dataset$Badhealth,Covid19_dataset$Verybadhealth,
                         lMales,lFemales)
new_col_names<-c("Age0to7","Age8to14",
                 "Age15to17","Age18to24",
                 "Age25to44","Age45to64",
                 "Age65to84","Age85andover",
                 "White","Mixed",
                 "Asian","Black",
                 "OtherEthinicGroup","Verygoodhealth",
                 "Goodhealth","Fairhealth",
                 "Badhealth","Verybadhealth",
                 "Males","Females")
names(Covid_dataset_independent_var) <- new_col_names
Covid_dataset_dependent_var<-data.frame(zTotal_Death)
new_col_names_dependent<-c("Total_death")
names(Covid_dataset_dependent_var) <- new_col_names_dependent

Covid_df <- cbind(Covid_dataset_dependent_var, Covid_dataset_independent_var)
names(Covid_df)
head(Covid_df)
library(psych)

KMO(cor(Covid_df[,-1]))

library(corrgram)
corrgram(Covid_df, order=FALSE, cor.method = "pearson", lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt, main="Covid 19 ",addrect = 2)

library(psych)
pairs.panels(Covid_df, method = "spearman", hist.col = "grey", col = "blue", main = "Covid 19")

library(PerformanceAnalytics)
chart.Correlation(Covid_df)

cor.matrix <- cor(Covid_df, use = "pairwise.complete.obs", method = "spearman")
cor.df <- as.data.frame(round(cor.matrix, 2))
View(cor.df)
library(corrplot)
corrplot(cor.matrix, type = "upper", tl.col = "black", tl.srt = 45)

cor.test(Covid_df$Total_death, Covid_df$Age0to7, method = "pearson")
#p 0.01697 cor  0.1352954
#Total_death and Age0to7 are significantly correlated with correlation coeff of 0.1352

cor.test(Covid_df$Total_death, Covid_df$Age8to14, method = "pearson")
#p-value = 3.309e-07  cor 0.2846117 

cor.test(Covid_df$Total_death, Covid_df$Age15to17, method = "spearman")
#p-value = 2.439e-07 rho 0.2876775 

cor.test(Covid_df$Total_death, Covid_df$Age18to24, method = "spearman")
# p-value = 0.1167 rho 0.08912824 

cor.test(Covid_df$Total_death, Covid_df$Age25to44, method = "spearman")
# p-value = 0.4901  rho -0.03927826 

cor.test(Covid_df$Total_death, Covid_df$Age45to64, method = "spearman")
#p-value = 0.5516 rho -0.03388547 

cor.test(Covid_df$Total_death, Covid_df$Age65to84, method = "pearson")
#p-value = 0.5233 cor 0.03632983 

cor.test(Covid_df$Total_death, Covid_df$Age85andover, method = "pearson")
#p-value = 0.5623 cor -0.03298244

cor.test(Covid_df$Total_death, Covid_df$White, method = "spearman")
# p-value = 0.09135  rho -0.09590071

cor.test(Covid_df$Total_death, Covid_df$Mixed, method = "pearson")
#p-value = 0.911  cor -0.006363232 

cor.test(Covid_df$Total_death, Covid_df$Asian, method = "pearson")
# p-value = 0.004581 cor 0.1603676

cor.test(Covid_df$Total_death, Covid_df$Black, method = "pearson")
#p-value = 0.1875 cor 0.07492919

cor.test(Covid_df$Total_death, Covid_df$OtherEthinicGroup, method = "spearman")
# p-value = 0.645 rho 0.02622844 

cor.test(Covid_df$Total_death, Covid_df$Verygoodhealth, method = "pearson")
# p-value = 1.923e-09 cor -0.3320931

cor.test(Covid_df$Total_death, Covid_df$Goodhealth, method = "pearson")
#p-value = 0.0981 cor 0.0939688

cor.test(Covid_df$Total_death, Covid_df$Fairhealth, method = "pearson")
#p-value = 5.02e-09  cor 0.3238525

cor.test(Covid_df$Total_death, Covid_df$Badhealth, method = "pearson")
# p-value = 1.831e-10 cor 0.3513146

cor.test(Covid_df$Total_death, Covid_df$Verybadhealth, method = "pearson")
#p-value = 1.988e-09 cor 0.3318104 

cor.test(Covid_df$Total_death, Covid_df$Males, method = "spearman")
#p-value = 0.005668 rho -0.1565293 

cor.test(Covid_df$Total_death, Covid_df$Females, method = "spearman")
#p-value = 0.005668 rho 0.1565293 


Covid_df2<-data.frame(Covid_df$Total_death,
                      Covid_df$Age0to7,
                      Covid_df$Age8to14,
                      Covid_df$Age15to17, 
                      Covid_df$Asian, 
                      Covid_df$Verygoodhealth,
                      Covid_df$Fairhealth,
                      Covid_df$Badhealth,
                      Covid_df$Verybadhealth, 
                      Covid_df$Males, 
                      Covid_df$Females
)
new_col_names<-c("Total_death",
                "Age0to7",
                 "Age8to14",
                 "Age15to17",
                 "Asian",
                 "Verygoodhealth",
                 "Fairhealth",
                 "Badhealth",
                 "Verybadhealth",
                 "Males",
                 "Females")
names(Covid_df2) <- new_col_names
head(Covid_df2)

KMO(cor(Covid_df2[,-1]))

library(corrgram)
corrgram(Covid_df2[,-1], order=FALSE, cor.method = "pearson", lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt, main="Covid 19 data ",addrect = 2)

library(psych)
pairs.panels(Covid_df2[,-1], method = "spearman", hist.col = "grey", col = "blue", main = "Covid 19")

library(PerformanceAnalytics)
chart.Correlation(Covid_df2[,-1])

cor.matrix <- cor(Covid_df2[,-1], use = "pairwise.complete.obs", method = "spearman")
cor.df <- as.data.frame(round(cor.matrix, 2))
View(cor.df)
library(corrplot)
corrplot(cor.matrix, type = "upper", tl.col = "black", tl.srt = 45)


##################
#now do the partial correlation between variables that cor value greater than 0.5

library(ppcor)

#1
pcor.test(Covid_df2$Total_death, Covid_df2$Age0to7, Covid_df2$Age8to14) #0.6487131
pcor.test(Covid_df2$Total_death, Covid_df2$Age8to14, Covid_df2$Age0to7) #5.955677e-06
# retain age 0 to 7
#remove Age8to14

Covid_df3<-data.frame(Covid_df$Total_death,
                      Covid_df$Age0to7,
                      Covid_df$Age15to17, 
                      Covid_df$Asian, 
                      Covid_df$Verygoodhealth,
                      Covid_df$Fairhealth,
                      Covid_df$Badhealth,
                      Covid_df$Verybadhealth, 
                      Covid_df$Males, 
                      Covid_df$Females
)
new_col_names<-c("Total_death",
                 "Age0to7",
                 "Age15to17",
                 "Asian",
                 "Verygoodhealth",
                 "Fairhealth",
                 "Badhealth",
                 "Verybadhealth",
                 "Males",
                 "Females")
names(Covid_df3) <- new_col_names
KMO(cor(Covid_df3[,-1]))

corrgram(Covid_df3[,-1], order=FALSE, cor.method = "pearson", lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt, main="Covid 19")


#2
pcor.test(Covid_df3$Total_death, Covid_df3$Age0to7, Covid_df3$Asian) #0.6163729
pcor.test(Covid_df3$Total_death, Covid_df3$Asian, Covid_df3$Age0to7)# 0.1080992
#retain age 0 to 7
#remove asian


Covid_df4<-data.frame(Covid_df$Total_death,
                      Covid_df$Age0to7,
                      Covid_df$Age15to17, 
                      Covid_df$Verygoodhealth,
                      Covid_df$Fairhealth,
                      Covid_df$Badhealth,
                      Covid_df$Verybadhealth, 
                      Covid_df$Males, 
                      Covid_df$Females
)
new_col_names<-c("Total_death",
                 "Age0to7",
                 "Age15to17",
                 "Verygoodhealth",
                 "Fairhealth",
                 "Badhealth",
                 "Verybadhealth",
                 "Males",
                 "Females")
names(Covid_df4) <- new_col_names

KMO(cor(Covid_df4[,-1]))

corrgram(Covid_df4[,-1], order=FALSE, cor.method = "pearson", lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt, main="Covid 19")


#4
pcor.test(Covid_df4$Total_death, Covid_df4$Verygoodhealth, Covid_df4$Fairhealth) #0.1694069
pcor.test(Covid_df4$Total_death, Covid_df4$Fairhealth, Covid_df4$Verygoodhealth) #0.8727495
#retain fair health
#remove very good health

Covid_df5<-data.frame(Covid_df$Total_death,
                      Covid_df$Age0to7,
                      Covid_df$Age15to17, 
                      Covid_df$Fairhealth,
                      Covid_df$Badhealth,
                      Covid_df$Verybadhealth, 
                      Covid_df$Males, 
                      Covid_df$Females
)
new_col_names<-c("Total_death",
                 "Age0to7",
                 "Age15to17",
                 "Fairhealth",
                 "Badhealth",
                 "Verybadhealth",
                 "Males",
                 "Females")
names(Covid_df5) <- new_col_names
KMO(cor(Covid_df5[,-1]))
corrgram(Covid_df5[,-1], order=FALSE, cor.method = "pearson", lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt, main="Covid 19")

#7
pcor.test(Covid_df5$Total_death, Covid_df5$Fairhealth, Covid_df5$Badhealth) #0.2744468
pcor.test(Covid_df5$Total_death, Covid_df5$Badhealth, Covid_df5$Fairhealth) #0.005737773
#retain fair health
#remove bad health
Covid_df6<-data.frame(Covid_df$Total_death,
                      Covid_df$Age0to7,
                      Covid_df$Age15to17, 
                      Covid_df$Fairhealth,
                      Covid_df$Verybadhealth, 
                      Covid_df$Males, 
                      Covid_df$Females
)
new_col_names<-c("Total_death",
                 "Age0to7",
                 "Age15to17",
                 "Fairhealth",
                 "Verybadhealth",
                 "Males",
                 "Females")
names(Covid_df6) <- new_col_names
KMO(cor(Covid_df6[,-1]))

corrgram(Covid_df6[,-1], order=FALSE, cor.method = "pearson", lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt, main="Covid 19")


#8
pcor.test(Covid_df6$Total_death, Covid_df6$Fairhealth, Covid_df6$Verybadhealth) #0.02659745
pcor.test(Covid_df6$Total_death, Covid_df6$Verybadhealth, Covid_df6$Fairhealth) #0.009563563
#retain fair health 
#remove very bad health
Covid_df7<-data.frame(Covid_df$Total_death,
                      Covid_df$Age0to7,
                      Covid_df$Age15to17, 
                      Covid_df$Fairhealth,
                      Covid_df$Males, 
                      Covid_df$Females
)
new_col_names<-c("Total_death",
                 "Age0to7",
                 "Age15to17",
                 "Fairhealth",
                 "Males",
                 "Females")

names(Covid_df7) <- new_col_names
KMO(cor(Covid_df7[,-1]))

corrgram(Covid_df7[,-1], order=FALSE, cor.method = "pearson", lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt, main="Covid 19")

pcor.test(Covid_df7$Total_death, Covid_df7$Males, Covid_df7$Females) #0.4706626
pcor.test(Covid_df7$Total_death, Covid_df7$Females, Covid_df7$Males) #0.5777628
#retain females
#remove males

Covid_df8<-data.frame(Covid_df$Total_death,
                      Covid_df$Age0to7,
                      Covid_df$Age15to17, 
                      Covid_df$Fairhealth,
                      Covid_df$Females
)
new_col_names<-c("Total_death",
                 "Age0to7",
                 "Age15to17",
                 "Fairhealth",
                 "Females")

names(Covid_df8) <- new_col_names
corrgram(Covid_df8[,-1], order=FALSE, cor.method = "pearson", lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt, main="Covid 19")
KMO(cor(Covid_df8[,-1]))
#A KMO value of 0.47 suggests that the interrelationships among your variables are not very strong
#Factor analysis works best when there are moderate to high correlations among variables.

#### hypothesis testing ####

#NULL HYPOTHESIS
#There is no significant linear relationship between the Total_death and the combination of Age0to7 and Fairhealth
#The correlation coefficient is equal to zero in the population.
fit4 <- lm(Total_death ~ Age0to7 + Fairhealth , data = Covid_df8)
anova(fit4)


#NULL HYPOTHESIS
#There is no significant linear relationship between the Total_death and the combination of Age15to17 and Females
#The correlation coefficient is equal to zero in the population.
fit4 <- lm(Total_death ~ Age15to17 + Females , data = Covid_df8)
anova(fit4)

#NULL HYPOTHESIS
#There is no significant linear relationship between the Total_death and the combination of Fairhealth and Females
#The correlation coefficient is equal to zero in the population.
fit4 <- lm(Total_death ~ Fairhealth + Females , data = Covid_df8)
anova(fit4)

#### Factor analysis ####
# Determine Number of Factors to Extract
library(nFactors)

# get eigenvalues: eigen() uses a correlation matrix
ev <- eigen(cor(Covid_df8[,-1]))
ev$values
# plot a scree plot of eigenvalues
plot(ev$values, type="b", col="blue", xlab="variables")

# calculate cumulative proportion of eigenvalue and plot
ev.sum<-0
for(i in 1:length(ev$value)){
  ev.sum<-ev.sum+ev$value[i]
}
ev.list1<-1:length(ev$value)
for(i in 1:length(ev$value)){
  ev.list1[i]=ev$value[i]/ev.sum
}
ev.list2<-1:length(ev$value)
ev.list2[1]<-ev.list1[1]
for(i in 2:length(ev$value)){
  ev.list2[i]=ev.list2[i-1]+ev.list1[i]
}
plot (ev.list2, type="b", col="red", xlab="number of components", ylab ="cumulative proportion")
library(GPArotation)

fit <- principal(Covid_df8[,-1], nfactors = 3, rotate = "varimax")
fit

data_for_clustering <- Covid_df8[,c("Age15to17", "Fairhealth", "Females")]

clustering_result <- hclust(dist(data_for_clustering), method = "complete")

clusters <- cutree(clustering_result, k = 3)
plot(clustering_result, main = "Dendrogram for Agglomerative Clustering", xlab = "Observations", sub = NULL)
rect.hclust(clustering_result, k = 3, border = 2:4)

library(relaimpo)
library(RcmdrMisc)


#### Multiple linear regression modelling ####

#multiple linear regression model by supplying all independent variables
# model with all variables
model1 <- lm(Total_death~., data = Covid_df)
summary(model1)
library(car)
vif(model1)
sqrt(vif(model1)) > 2  # if > 2 vif too high
summary(model1)$r.squared #R-squared
plot(model1, which = 1) # Residuals vs Fitted values
plot(model1, which = 2)  # Normal Q-Q plot


#model1a using stepwise approach on model 1
model1a <- lm(Total_death~., data = Covid_df, direction = "forward")
summary(model1a)
library(car)
vif(model1a)
sqrt(vif(model1a)) > 2  # if > 2 vif too high
summary(model1a)$r.squared #R-squared
plot(model1a, which = 1) # Residuals vs Fitted values
plot(model1a, which = 2)  # Normal Q-Q plot


#linear regression model after correlation test and before partial correlation.
model2 <- lm(Total_death ~., data = Covid_df2)
summary(model2)
library(car)
vif(model2)
sqrt(vif(model2)) > 2  # if > 2 vif too high
summary(model2)$r.squared #R-squared
plot(model2, which = 1) # Residuals vs Fitted values
plot(model2, which = 2)  # Normal Q-Q plot

#linear regression model before factor analysis
model3 <- lm(Total_death ~ Age0to7 + Age15to17 + Fairhealth + Females, data = Covid_df8)
summary(model3)
library(car)
vif(model3)
sqrt(vif(model3)) > 2  # if > 2 vif too high
summary(model3)$r.squared #R-squared
plot(model3, which = 1) # Residuals vs Fitted values
plot(model3, which = 2)  # Normal Q-Q plot


#linear regression model after factor analysis
model4 <- lm(Total_death ~ Age15to17 + Fairhealth + Females, data = Covid_df8)
summary(model4)
library(car)
vif(model4)
sqrt(vif(model4)) > 2  # if > 2 vif too high
summary(model4)$r.squared #R-squared
plot(model4, which = 1) # Residuals vs Fitted values
plot(model4, which = 2)  # Normal Q-Q plot


anova(model1, model2, model3, model4, test="F")


################################### END ##########################################################################
