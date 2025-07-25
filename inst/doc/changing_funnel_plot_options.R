## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.height = 5,
  fig.width = 7
)

## ----setup--------------------------------------------------------------------
library(FunnelPlotR)

## ----dtsetup------------------------------------------------------------------
library(COUNT)
data(medpar)
medpar$provnum <- factor(medpar$provnum)
medpar$los <- as.numeric(medpar$los)

# Logistic model to predict LOS, LOS is quite overdispersed
mod <- glm(los ~ hmo + died + age80 + factor(type)
           , family = "poisson"
           , data = medpar)

#Get predicted value for ratio
medpar$prds <- predict(mod, newdata = medpar, type = "response")

# Draw plot, returning just the plot object
funnel_plot(medpar, denominator = prds, numerator = los
            , group = provnum
            , limit = 99
            , label = "outlier"
            , draw_unadjusted = TRUE)


## ----highlight----------------------------------------------------------------
# Draw plot, returning just the plot object
funnel_plot(medpar, denominator = prds, numerator = los
            , group = provnum, limit = 99, label = "outlier"
            , draw_unadjusted = TRUE, highlight = "030002")


## ----plottheme1---------------------------------------------------------------
funnel_plot(medpar, denominator = prds, numerator = los
            , group = provnum, limit = 99, label = "outlier"
            , draw_unadjusted = TRUE, theme = funnel_grey())


## ----plottheme2---------------------------------------------------------------
library(ggplot2)

new_funnel_theme <-
  funnel_grey() +
  # Change plot title
  theme(plot.title =  element_text(face = "bold", colour = "red", size = 6),
        # Alter legend background colour
        legend.background = element_rect(fill = "brown"),
        #Rotate y axis label
        axis.title.y = element_text(angle = 0))


funnel_plot(medpar, denominator = prds, numerator = los
            , group = provnum, limit = 99, label = "outlier"
            , draw_unadjusted = TRUE, theme = new_funnel_theme)


## ----colours------------------------------------------------------------------
funnel_plot(medpar, denominator = prds, numerator = los
            , group = provnum, limit = 99, label = "outlier"
            , draw_unadjusted = TRUE, theme = funnel_grey(),
            plot_cols = c("#000000", "#1F77B4FF", "#9467BDFF", "#2CA02CFF"))

## ----funnelscales-------------------------------------------------------------
## Changing labels
funnel_plot(medpar, denominator = prds, numerator = los
            , group = provnum, limit = 99, label = "outlier"
            , draw_unadjusted = TRUE, x_range = c(0, 400), y_range = c(0, 2))


## ----funnellabels1------------------------------------------------------------
funnel_plot(medpar, denominator = prds, numerator = los
            , group = provnum, limit = 99, label = "outlier"
            , draw_unadjusted = TRUE, title = "Vignette funnel plot"
            , x_label = "x-axis", y_label = "y-axis")

## ----funnellabels2------------------------------------------------------------
funnel_plot(medpar, denominator = prds, numerator = los
            , group = provnum, limit = 99
            , draw_unadjusted = TRUE, title = "Vignette funnel plot"
            , x_label = "x-axis", y_label = "y-axis"
            , highlight = "030002", label = "highlight")

## ----cutoutplot---------------------------------------------------------------
# Original funnel plot object
fp <-
  funnel_plot(medpar, denominator = prds, numerator = los
              , group = provnum, limit = 99, label = "outlier"
              , draw_unadjusted = TRUE)

# Extract just the plot
my_plot <- plot(fp)

# Add an additional geom to plot
my_plot +
  geom_vline(aes(xintercept = 400), linetype = "dashed"
             , colour = "red", linewidth = 2)

