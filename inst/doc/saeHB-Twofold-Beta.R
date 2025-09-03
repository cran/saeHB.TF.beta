## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(saeHB.TF.beta)
data("dataBeta")

## ----message=FALSE, warning=FALSE, fig.show='hold', out.width='50%'-----------
dataBeta$CV <- sqrt(dataBeta$vardir)/dataBeta$y
explore(y~X1+X2, CV = "CV", data = dataBeta, normality = TRUE)

## ----results='hide', message=FALSE, warning=FALSE-----------------------------
model <- betaTF(y~X1+X2,area="codearea",weight="w",iter.mcmc = 10000, burn.in = 3000, iter.update = 5, thin = 10, data=dataBeta)

## -----------------------------------------------------------------------------
model$plot

## -----------------------------------------------------------------------------
model$Est_area

## -----------------------------------------------------------------------------
model$area_randeff

## -----------------------------------------------------------------------------
CV_area <- (model$Est_area$SD)/(model$Est_area$Mean)*100
MSE_area <- model$Est_area$SD^2
summary(cbind(CV_area,MSE_area))

## -----------------------------------------------------------------------------
model$Est_area

## -----------------------------------------------------------------------------
model$sub_randeff

## -----------------------------------------------------------------------------
CV_sub <- (model$Est_sub$SD)/(model$Est_sub$Mean)*100
MSE_sub <- model$Est_sub$SD^2
summary(cbind(CV_sub,MSE_sub))

## -----------------------------------------------------------------------------
model$coefficient

## -----------------------------------------------------------------------------
model$refVar

## ----results='hide', warning=FALSE--------------------------------------------
library(ggplot2)

## -----------------------------------------------------------------------------
df <- data.frame(
  area = seq_along(model$Est_sub$Mean),             
  direct = dataBeta$y,              
  mean_estimate = model$Est_sub$Mean
)

## -----------------------------------------------------------------------------
ggplot(df, aes(x = area)) +
  geom_point(aes(y = direct), size = 2, colour = "#388894", alpha = 0.6) +   # scatter points
  geom_point(aes(y = mean_estimate), size = 2, colour = "#2b707a") +   # scatter points
  geom_line(aes(y = direct), linewidth = 1, colour = "#388894", alpha = 0.6) +  # line connecting points
  geom_line(aes(y = mean_estimate), linewidth = 1, colour = "#2b707a") +  # line connecting points
  labs(
    title = "Scatter + Line Plot of Estimated Means",
    x = "Area Index",
    y = "Value"
  )

## ----warning=FALSE, message=FALSE---------------------------------------------
ggplot(df, aes(x = , direct, y = mean_estimate)) +
  geom_point( size = 2, colour = "#2b707a") +
   geom_abline(intercept = 0, slope = 1, color = "gray40", linetype = "dashed") +
  geom_smooth(method = "lm", color = "#2b707a", se = FALSE) +
  ylim(0, 1) +
  labs(
    title = "Scatter Plot of Direct vs Model-Based",
    x = "Direct",
    y = "Model Based"
  )

## -----------------------------------------------------------------------------
df_cv <- data.frame(
  direct = sqrt(dataBeta$vardir)/dataBeta$y*100,              
  cv_estimate = CV_sub
)
df_cv <- df_cv[order(df_cv$direct), ]
df_cv$area <- seq_along(dataBeta$y)

## ----warning=FALSE------------------------------------------------------------
ggplot(df_cv, aes(x = area)) +
  geom_point(aes(y = direct), size = 2, colour = "#388894", alpha = 0.5) +
  geom_point(aes(y = cv_estimate), size = 2, colour = "#2b707a") +
  ylim(0, 100) +
  labs(
    title = "Scatter Plot of Direct vs Model-Based CV",
    x = "Area",
    y = "CV (%)"
  )

