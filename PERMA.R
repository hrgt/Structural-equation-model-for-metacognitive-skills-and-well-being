# Structural-equation-model-for-metacognitive-skills-and-well-being

#import and load following libraries
library(lavaan)
library(ggplot2)
library(semPlot)
library(igraph)
library(tidySEM)

#dimensions of each variable

Meta =~ Factor.1 + Factor.2 + Factor.3 + Factor.4
Well =~ P.total + E.total + R.total + M.total + A.total + Happiness

#fit SEM for wellbeing (perma) and metacognition

semod <- 'Meta =~ Factor.1 + Factor.2 + Factor.3 + Factor.4
Well =~ P.total + E.total + R.total + M.total + A.total + Happiness
Meta~Well'
fit <- sem(semod, data = semod210721)
summary(fit, fit.measures = TRUE, standardized=TRUE)
semPaths(fit, "std")
graph_sem(model = fit, rows = 4)
inspect(fit, "r2")

#plot graph for SEM
lay = get_layout("", "", "Factor.1", "Factor.2", "Factor.3", "Factor.4", "", "",
                 "P.total", "E.total", "R.total", "M.Total", "A.Total", "Happiness",
                 rows = 2)
graph_sem(fit, layout = lay)

#bootstrap the derived model

fit <- sem(semod, data = semod210721, se="bootstrap", bootstrap=5000)
parameterEstimates(fit, ci=TRUE, level=0.95, boot.ci.type="perc", standardized = TRUE)
set.seed(2019)
fitmeasures(fit,c("cfi","rmsea","srmr","tli"))
