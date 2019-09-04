## ---- include = FALSE, echo=FALSE----------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height= 5, 
  fig.width=7
)

## ----basicfunnel, warning=FALSE, error=FALSE, message=FALSE, fig.height= 5, fig.width=7----
library(ggplot2)
library(tidyr)

# Make up some data, as if it was from a regression model with observed and predicted (expected) events.
dt <- data.frame(observed = c( 15,40,72,28,50, 66, 75),
                 expected = c( 13,32,75,33,54, 60, 72),
                 unit = factor(c("A","B","c","D","E", "F", "G"))
)

# Add a ratio (SR) of observed to expected, our indicator 
dt$SR <- dt$observed / dt$expected

# Scatter plot in ggplot
a<-ggplot(dt, aes(x=expected, y= SR))+
  geom_point()

a

# Now add a central line, in a ration like this, 1 is the average/expected value.
a<- a+geom_hline(aes(yintercept=1))
a

# Add a 95% Poisson limit, by using the density function to get the quantile value for each 'expected'.
lkup<-data.frame(id=seq(1, max(dt$expected), 1))
lkup$Upper<-(qpois(0.975,lambda = lkup$id) - 0.025) / lkup$id
lkup$lower<-(qpois(0.025,lambda = lkup$id) - 0.975) / lkup$id

lkup<-gather(lkup, key, value,-id)

a+ geom_line(aes(x=id, y=value, col=key), data=lkup)


## ----install, eval=FALSE-------------------------------------------------
#  devtools::install_github("https://github.com/chrismainey/FunnelPlotR")

## ----data, warning=FALSE, message=FALSE----------------------------------
library(FunnelPlotR)
library(COUNT)
library(ggplot2)

data(medpar)
medpar$provnum<-factor(medpar$provnum)
medpar$los<-as.numeric(medpar$los)

mod<- glm(los ~ hmo + died + age80 + factor(type), family="poisson", data=medpar)
summary(mod)


## ---- prediction---------------------------------------------------------

medpar$prds<- predict(mod, type="response")


## ---- funnel1, message=FALSE, fig.align='center', fig.retina=5, collapse=TRUE----

funnel_plot(numerator=medpar$los, denominator=medpar$prds, group = medpar$provnum, 
            title = 'Length of Stay Funnel plot for `medpar` data', Poisson_limits = TRUE,
            OD_adjust = FALSE,label_outliers = 99, return_elements = "plot")

## ---- ODcheck, message=FALSE---------------------------------------------

sum(mod$weights * mod$residuals^2)/mod$df.residual


## ---- funnel2, message=FALSE, fig.align='center', fig.retina=5, collapse=TRUE----
funnel_plot(numerator=medpar$los, denominator=medpar$prds, group = medpar$provnum, 
            title = 'Length of Stay Funnel plot for `medpar` data', Poisson_limits = FALSE,
            OD_adjust = TRUE, method = "SHMI",label_outliers = 99, return_elements = "plot")

