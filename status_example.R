library(tidyverse)
library(patchwork)

source("functions.R")

# --------- Set up data ---------------------------------------

# number of Monte Carlo simulations
nsim <- 1000

# example set of observations
dfObs <- data.frame(AssessmentUnit=c("Area1","Area1","Area2","Area2","Area2"),
                     QE=c("Nutrients","Nutrients","DirectEffects","Nutrients","DirectEffects"),
                     Parameter=c("NO3-N","Chl","NO3-N","PO4-P","Chl"),
                     Value=c(23,8,57,1.5,15),
                     StdErr=c(5,6,15,0.4,2),
                     Unit=c("µM","µg/L","µM","µM","µg/L")
)

# example boundary values
dfBounds <- data.frame(Parameter=c("NO3-N","PO4-P","Chl"),
                       BndEQR1=c(0,0,0),
                       BndEQR06=c(30,0.8,9),
                       BndEQR0=c(100,5,30)
)

# BndEQR1 - the value of the observed parameter corresponding to EQR = 1.0 (the best possible state for the parameter)
# BndEQR06 - the value of the observed parameter corresponding to EQR = 0.6 (the boundary between Good/Moderate status)
# BndEQR0 - the value of the observed parameter corresponding to EQR = 0.0 (worst case)


# --------- Simple calculations of status ---------------------------------------

# join observations and boundary values
df <- dfObs %>% 
  left_join(dfBounds,by="Parameter")

# calculate EQR values for each row of the data
df$EQR <- mapply(CalcEQR,df$Value,df$BndEQR0,df$BndEQR06,df$BndEQR1)

# Calculate EQR for each quality element
dfQE <- AggregateQE(df)

# Calculate EQR overall for each assessment unit
dfOverall <- AggregateOverall(dfQE)

# Find status class for each assessment, based on EQR
dfOverall$Class <- mapply(StatusClass,dfOverall$EQR)



# --------- MC calculations of status ---------------------------------------

# Generate a data frame with nsim rows
dfSim <- data.frame(SimID=c(1:nsim))

# merge - effectively creating nsim copies of dfObs
dfMC <- dfObs %>% merge(dfSim, all=TRUE)

# keep a copy of the original observed value as 'Mean'
dfMC<-dfMC %>% 
  mutate(Mean=Value,n=1)

# we will now use 'Mean' value this to estimate random, normally distributed estimates of the observed value
dfMC <- dfMC %>%
  mutate(Value=rnorm(n,Mean,StdErr))

# join observations and boundary values
dfMC <- dfMC %>% 
  left_join(dfBounds,by="Parameter")

# calculate EQR values for each row of the data
dfMC$EQR <- mapply(CalcEQR,dfMC$Value,dfMC$BndEQR0,dfMC$BndEQR06,dfMC$BndEQR1)

# Calculate EQR for each quality element
dfMCQE <- AggregateQE(dfMC)

# Calculate EQR overall for each assessment unit
dfMCOverall <- AggregateOverall(dfMCQE)

# Find status class for each assessment, based on EQR
dfMCOverall$Class <- mapply(StatusClass,dfMCOverall$EQR)


# define factor levels for status class
dfMCOverall$Class <- factor(dfMCOverall$Class,levels=c("Bad","Poor","Mod","Good","High"))

# Summarise the fractions within each status class
dfMCSummary <- dfMCOverall %>%
  group_by(AssessmentUnit,Class) %>%
  summarise(freq=n())

# --------- Show distributions in EQR and Status classes   ---------------------------------------

p1 <- ggplot(dfMCOverall) +
  theme_minimal(base_size=9) +
  geom_histogram(aes(x=EQR),binwidth = 0.02) +
  ylab("frequency") +
  scale_x_continuous(limits = c(0, 1)) +
  facet_wrap(.~AssessmentUnit)

p2 <- ggplot(dfMCOverall) +
  theme_minimal(base_size=9) +
  geom_bar(aes(x=Class,y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequency") +
  facet_wrap(.~AssessmentUnit)

p <- p1 / p2

p

ggsave(p,file="fig.png",height=10,width=15,units="cm",dpi=300)
