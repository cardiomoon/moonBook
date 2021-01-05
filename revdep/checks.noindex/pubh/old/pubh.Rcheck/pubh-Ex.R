pkgname <- "pubh"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('pubh')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("Bernard")
### * Bernard

flush(stderr()); flush(stdout())

### Name: Bernard
### Title: Survival of patients with sepsis.
### Aliases: Bernard
### Keywords: datasets

### ** Examples

data(Bernard)

cross_tab(fate ~ treat, data = Bernard)

contingency(fate ~ treat, data = Bernard)



cleanEx()
nameEx("Brenner")
### * Brenner

flush(stderr()); flush(stdout())

### Name: Brenner
### Title: Prevalence of Helicobacter pylori infection in preschool
###   children.
### Aliases: Brenner
### Keywords: datasets

### ** Examples

data(Brenner)

Brenner %>%
  cross_tab(infected ~ ulcer)

contingency(infected ~ ulcer, data = Brenner, method = "cross.sectional")



cleanEx()
nameEx("Fentress")
### * Fentress

flush(stderr()); flush(stdout())

### Name: Fentress
### Title: Migraine pain reduction.
### Aliases: Fentress
### Keywords: datasets

### ** Examples

data(Fentress)

Fentress %>%
  strip_error(pain ~ group) %>%
  axis_labs()



cleanEx()
nameEx("Hodgkin")
### * Hodgkin

flush(stderr()); flush(stdout())

### Name: Hodgkin
### Title: T-cell counts from Hodgkin's disease patients.
### Aliases: Hodgkin
### Keywords: datasets

### ** Examples

data(Hodgkin)
require(dplyr, quietly = TRUE)
require(sjlabelled, quietly = TRUE)

Hodgkin <- Hodgkin %>%
  mutate(
    Ratio = CD4/CD8
  ) %>%
  var_labels(
    Ratio = "CD4+ / CD8+ T-cells"
  )

estat(~ Ratio|Group, data = Hodgkin)

Hodgkin %>%
  qq_plot(~ Ratio|Group) %>%
  axis_labs()

Hodgkin$Ratio <- Hodgkin$CD4/Hodgkin$CD8
estat(~ Ratio|Group, data = Hodgkin)

qq_plot(~ Ratio|Group, data = Hodgkin) %>%
axis_labs()



cleanEx()
nameEx("Kirkwood")
### * Kirkwood

flush(stderr()); flush(stdout())

### Name: Kirkwood
### Title: Body weight and plasma volume.
### Aliases: Kirkwood
### Keywords: datasets

### ** Examples

data(Kirkwood)

Kirkwood %>%
  gf_point(volume ~ weight) %>%
  gf_lm(col = "indianred3", interval = "confidence", fill = "indianred3") %>%
  axis_labs()



cleanEx()
nameEx("Macmahon")
### * Macmahon

flush(stderr()); flush(stdout())

### Name: Macmahon
### Title: Breast cancer and age of childbirth.
### Aliases: Macmahon
### Keywords: datasets

### ** Examples

data(Macmahon)

Macmahon %>%
  cross_tab(cancer ~ age)

odds_trend(cancer ~ age, data = Macmahon)$df
odds_trend(cancer ~ age, data = Macmahon)$fig



cleanEx()
nameEx("Oncho")
### * Oncho

flush(stderr()); flush(stdout())

### Name: Oncho
### Title: Onchocerciasis in Sierra Leone.
### Aliases: Oncho
### Keywords: datasets

### ** Examples

data(Oncho)

odds_trend(mf ~ agegrp, data = Oncho)$df
odds_trend(mf ~ agegrp, data = Oncho)$fig



cleanEx()
nameEx("Roberts")
### * Roberts

flush(stderr()); flush(stdout())

### Name: Roberts
### Title: Extracorporeal membrane oxygenation in neonates.
### Aliases: Roberts
### Keywords: datasets

### ** Examples

data(Roberts)

Roberts %>%
  cross_tab(survived ~ emo)



cleanEx()
nameEx("Rothman")
### * Rothman

flush(stderr()); flush(stdout())

### Name: Rothman
### Title: Oral contraceptives and stroke.
### Aliases: Rothman
### Keywords: datasets

### ** Examples

data(Rothman)

cross_tab(stroke ~ oc + ht, data = Rothman)

mhor(stroke ~ ht/oc, data = Rothman)

## Model with standard interaction term:
model1 <- glm(stroke ~ ht*oc, data = Rothman, family = binomial)
glm_coef(model1)

## Model considering join exposure:
Rothman$join <- 0
Rothman$join[Rothman$oc == "Non-user" & Rothman$ht == "Yes"] <- 1
Rothman$join[Rothman$oc == "User" & Rothman$ht == "No"] <- 2
Rothman$join[Rothman$oc == "User" & Rothman$ht == "Yes"] <- 3
Rothman$join <- factor(Rothman$join, labels=c("Unexposed", "Hypertension", "OC user",
                       "OC and hypertension"))

require(sjlabelled, quietly = TRUE)
Rothman$join <- set_label(Rothman$join, label = "Exposure")

cross_tab(stroke ~ join, data = Rothman)

model2 <- glm(stroke ~ join, data = Rothman, family = binomial)
glm_coef(model2)
odds_trend(stroke ~ join, data = Rothman)$df
odds_trend(stroke ~ join, data = Rothman)$fig



cleanEx()
nameEx("Sandler")
### * Sandler

flush(stderr()); flush(stdout())

### Name: Sandler
### Title: Passive smoking in adulthood and cancer risk.
### Aliases: Sandler
### Keywords: datasets

### ** Examples

data(Sandler)

Sandler %>%
  cross_tab(cancer ~ passive)

cross_tab(cancer ~ passive + smoke, data = Sandler)

mhor(cancer ~ smoke/passive, data = Sandler)



cleanEx()
nameEx("Sharples")
### * Sharples

flush(stderr()); flush(stdout())

### Name: Sharples
### Title: Measured and self-reported weight in New Zealand.
### Aliases: Sharples
### Keywords: datasets

### ** Examples

Sharples %>%
  bland_altman(srweight ~ weight, transform = TRUE) %>%
  gf_labs(x = "Mean of weights (kg)", y = "Measured weight / Self-reported weight") %>%
  gf_theme(theme = sjPlot::theme_sjplot2(base_size = 9))



cleanEx()
nameEx("Thall")
### * Thall

flush(stderr()); flush(stdout())

### Name: Thall
### Title: RCT on the treatment of epilepsy.
### Aliases: Thall
### Keywords: datasets

### ** Examples

data(Thall)

c1 <- cbind(Thall[, c(1:5)], count = Thall$y1)[, c(1:4, 6)]
c2 <- cbind(Thall[, c(1:4, 6)], count = Thall$y2)[, c(1:4, 6)]
c3 <- cbind(Thall[, c(1:4, 7)], count = Thall$y3)[, c(1:4, 6)]
c4 <- cbind(Thall[, c(1:4, 8)], count = Thall$y3)[, c(1:4, 6)]
epilepsy <- rbind(c1, c2, c3, c4)

require(lme4, quietly = TRUE)
model_glmer <- glmer(count ~ treat + base + I(age - mean(age, na.rm = TRUE)) +
                 (1|id), data = epilepsy, family = poisson)
glm_coef(model_glmer, labels = c("Treatment (Prograbide/Control)",
                               "Baseline count", "Age (years)"))



cleanEx()
nameEx("Tuzson")
### * Tuzson

flush(stderr()); flush(stdout())

### Name: Tuzson
### Title: Peak knee velocity in walking at flexion and extension.
### Aliases: Tuzson
### Keywords: datasets

### ** Examples

data(Tuzson)

Tuzson %>%
  gf_point(flexion ~ extension) %>%
  axis_labs()

cor.test(~ flexion + extension, data = Tuzson)



cleanEx()
nameEx("Vanderpump")
### * Vanderpump

flush(stderr()); flush(stdout())

### Name: Vanderpump
### Title: Smoking and mortality in Whickham, England.
### Aliases: Vanderpump
### Keywords: datasets

### ** Examples

data(Vanderpump)

Vanderpump %>%
  cross_tab(vstatus ~ .)

mhor(vstatus ~ agegrp/smoker, data = Vanderpump)



cleanEx()
nameEx("axis_labs")
### * axis_labs

flush(stderr()); flush(stdout())

### Name: axis_labs
### Title: Apply labels from variables to axis-labels in plots.
### Aliases: axis_labs

### ** Examples

data(kfm, package = "ISwR")
require(sjlabelled, quietly = TRUE)
kfm = kfm %>%
  var_labels(
     dl.milk = 'Breast-milk intake (dl/day)',
     sex = 'Sex',
     weight = 'Child weight (kg)',
     ml.suppl = 'Milk substitute (ml/day)',
     mat.weight = 'Maternal weight (kg)',
     mat.height = 'Maternal height (cm)'
     )

kfm %>%
  gf_point(weight ~ dl.milk) %>%
  gf_lm(col = 2, interval = "confidence", col = 2) %>%
  axis_labs()

kfm %>%
  box_plot(dl.milk ~ sex, fill='thistle', alpha = 0.8) %>%
  axis_labs() %>%
  gf_star(x1 = 1, y1 = 10.9, x2 = 2, y2 = 11, y3 = 11.2)



cleanEx()
nameEx("bar_error")
### * bar_error

flush(stderr()); flush(stdout())

### Name: bar_error
### Title: Bar charts with error bars.
### Aliases: bar_error

### ** Examples

require(dplyr, quietly = TRUE)
require(sjlabelled, quietly = TRUE)
data(birthwt, package = "MASS")
birthwt <- birthwt %>%
  mutate(
    smoke = factor(smoke, labels = c("Non-smoker", "Smoker")),
    Race = factor(race > 1, labels = c("White", "Non-white"))
  ) %>%
  var_labels(
    bwt = 'Birth weight (g)',
    smoke = 'Smoking status'
 )

birthwt %>%
  bar_error(bwt ~ smoke, fill = 'plum3') %>%
  axis_labs()

birthwt %>%
  bar_error(bwt ~ smoke|Race, fill = 'plum3') %>%
  axis_labs()

birthwt %>%
  bar_error(bwt ~ smoke, fill = ~ Race) %>%
  axis_labs()




cleanEx()
nameEx("bland_altman")
### * bland_altman

flush(stderr()); flush(stdout())

### Name: bland_altman
### Title: Bland-Altman agreement plots.
### Aliases: bland_altman

### ** Examples

data(wright, package = "ISwR")

wright %>%
  bland_altman(mini.wright ~ std.wright, pch = 16,
               ylab = "Large-mini expiratory flow rate (l/min)",
               xlab = "Mean expiratory flow rate (l/min)") %>%
 gf_labs(y = "Large-mini expiratory flow rate (l/min)",
         x = "Mean expiratory flow rate (l/min)") %>%
 gf_theme(theme = sjPlot::theme_sjplot2(base_size = 9))

data(Sharples)

Sharples %>%
  bland_altman(srweight ~ weight, transform = TRUE) %>%
  gf_labs(x = "Mean of weights (kg)", y = "Measured weight / Self-reported weight") %>%
  gf_theme(theme = sjPlot::theme_sjplot2(base_size = 9))



cleanEx()
nameEx("box_plot")
### * box_plot

flush(stderr()); flush(stdout())

### Name: box_plot
### Title: Construct box plots.
### Aliases: box_plot

### ** Examples

data(kfm, package = "ISwR")
require(sjlabelled, quietly = TRUE)
kfm = kfm %>%
  var_labels(
     dl.milk = 'Breast-milk intake (dl/day)',
     sex = 'Sex',
     weight = 'Child weight (kg)',
     ml.suppl = 'Milk substitute (ml/day)',
     mat.weight = 'Maternal weight (kg)',
     mat.height = 'Maternal height (cm)'
     )

kfm %>%
  box_plot(dl.milk ~ sex, fill = 'thistle', alpha = 0.8) %>%
  axis_labs()

t.test(dl.milk ~ sex, data = kfm)

kfm %>%
  box_plot(dl.milk ~ sex, fill = 'thistle', alpha = 0.8) %>%
  axis_labs() %>%
  gf_star(1, 10.9, 2, 11, 11.4, legend = 'p = 0.035', size = 2.5)



cleanEx()
nameEx("bst")
### * bst

flush(stderr()); flush(stdout())

### Name: bst
### Title: Bootstrap Confidence Intervals.
### Aliases: bst

### ** Examples

data(IgM, package = "ISwR")
bst(IgM, "median")

bst(IgM, "gmean")



cleanEx()
nameEx("coef_det")
### * coef_det

flush(stderr()); flush(stdout())

### Name: coef_det
### Title: Coefficient of determination.
### Aliases: coef_det

### ** Examples

## Linear regression:
Riboflavin <- seq(0, 80, 10)
OD <- 0.0125*Riboflavin + rnorm(9, 0.6, 0.03)
titration <- data.frame(Riboflavin, OD)
model1 <- lm(OD ~ Riboflavin, data = titration)
summary(model1)
coef_det(titration$OD, fitted(model1))

## Non-linear regression:
library(nlme, quietly = TRUE)
data(Puromycin)
mm.tx <- gnls(rate ~ SSmicmen(conc, Vm, K), data = Puromycin,
  subset = state == "treated")
summary(mm.tx)
coef_det(Puromycin$rate[1:12], mm.tx$fitted)



cleanEx()
nameEx("contingency")
### * contingency

flush(stderr()); flush(stdout())

### Name: contingency
### Title: Measures of association from two by two contingency tables
###   (formula).
### Aliases: contingency

### ** Examples

## A case-control study on the effect of alcohol on oesophageal cancer.
Freq <- c(386, 29, 389, 171)
status <- gl(2, 1, 4, labels=c("Control", "Case"))
alcohol <- gl(2, 2, labels=c("0-39", "40+"))
cancer <- data.frame(Freq, status, alcohol)
cancer <- expand_df(cancer)
contingency(status ~ alcohol, data = cancer, method = "case.control")

data(Oncho)
require(moonBook, quietly = TRUE)

mytable(mf ~ area, data = Oncho, show.total = TRUE)

Oncho %>%
  contingency(mf ~ area)



cleanEx()
nameEx("contingency2")
### * contingency2

flush(stderr()); flush(stdout())

### Name: contingency2
### Title: Measures of association from two by two contingency tables
###   (direct input).
### Aliases: contingency2

### ** Examples

## A case-control study on the effect of alcohol on oesophageal cancer.
Freq <- c(386, 29, 389, 171)
status <- gl(2, 1, 4, labels=c("Control", "Case"))
alcohol <- gl(2, 2, labels=c("0-39", "40+"))
cancer <- data.frame(Freq, status, alcohol)
cancer <- expand_df(cancer)

contingency2(171, 389, 29, 386, method = "case.control")



cleanEx()
nameEx("cross_tab")
### * cross_tab

flush(stderr()); flush(stdout())

### Name: cross_tab
### Title: Cross-tabulation.
### Aliases: cross_tab

### ** Examples

data(Oncho)

## A two by two contingency table:
Oncho %>%
  cross_tab(mf ~ area)

## Reporting prevalence:
Oncho %>%
  cross_tab(area ~ mf)

## Contingency table for both sex and area of residence:
Oncho %>%
  cross_tab(mf ~ sex + area, p_val = TRUE)

## Descriptive statistics for all variables in the \code{Oncho} data set except \code{id}.
require(dplyr, quietly = TRUE)
Oncho %>%
  select(- id) %>%
  cross_tab(mf ~ .)



cleanEx()
nameEx("diag_test")
### * diag_test

flush(stderr()); flush(stdout())

### Name: diag_test
### Title: Diagnostic tests from variables.
### Aliases: diag_test

### ** Examples

## We compare the use of lung’s X-rays on the screening of TB against the gold standard test.
Freq <- c(1739, 8, 51, 22)
BCG <- gl(2, 1, 4, labels=c("Negative", "Positive"))
Xray <- gl(2, 2, labels=c("Negative", "Positive"))
tb <- data.frame(Freq, BCG, Xray)
tb <- expand_df(tb)

tb %>%
  diag_test(BCG ~ Xray)



cleanEx()
nameEx("diag_test2")
### * diag_test2

flush(stderr()); flush(stdout())

### Name: diag_test2
### Title: Diagnostic tests from direct input.
### Aliases: diag_test2

### ** Examples

## We compare the use of lung’s X-rays on the screening of TB against the gold standard test.
diag_test2(22, 51, 8, 1739)



cleanEx()
nameEx("estat")
### * estat

flush(stderr()); flush(stdout())

### Name: estat
### Title: Descriptive statistics for continuous variables.
### Aliases: estat

### ** Examples

data(kfm, package = "ISwR")
require(sjlabelled, quietly = TRUE)
kfm = kfm %>%
var_labels(
  dl.milk = 'Breast-milk intake (dl/day)',
  sex = 'Sex',
  weight = 'Child weight (kg)',
  ml.suppl = 'Milk substitute (ml/day)',
  mat.weight = 'Maternal weight (kg)',
  mat.height = 'Maternal height (cm)'
 )

kfm %>%
  estat(~ dl.milk)

estat(~ dl.milk|sex, data = kfm)

kfm %>%
  estat(~ weight|sex)



cleanEx()
nameEx("expand_df")
### * expand_df

flush(stderr()); flush(stdout())

### Name: expand_df
### Title: Expand a data frame.
### Aliases: expand_df

### ** Examples

Freq <- c(5032, 5095, 41, 204)
Mortality <- gl(2, 2, labels=c("No", "Yes"))
Calcium <- gl(2, 1, 4, labels=c("No", "Yes"))
anyca <- data.frame(Freq, Mortality, Calcium)
anyca
anyca.exp <- expand_df(anyca)
with(anyca.exp, table(Calcium, Mortality))



cleanEx()
nameEx("freq_cont")
### * freq_cont

flush(stderr()); flush(stdout())

### Name: freq_cont
### Title: Relative and Cumulative Frequency.
### Aliases: freq_cont

### ** Examples

data(IgM, package="ISwR")
Ab <- data.frame(IgM)
estat(~ IgM, data = Ab)
freq_cont(IgM, seq(0, 4.5, 0.5))



cleanEx()
nameEx("gen_bst_df")
### * gen_bst_df

flush(stderr()); flush(stdout())

### Name: gen_bst_df
### Title: Generate a data frame with estimate and bootstrap CIs.
### Aliases: gen_bst_df

### ** Examples

data(kfm, package = "ISwR")
require(sjlabelled, quietly = TRUE)
kfm = kfm %>%
  var_labels(
     dl.milk = 'Breast-milk intake (dl/day)',
     sex = 'Sex',
     weight = 'Child weight (kg)',
     ml.suppl = 'Milk substitute (ml/day)',
     mat.weight = 'Maternal weight (kg)',
     mat.height = 'Maternal height (cm)'
     )

kfm %>%
  gen_bst_df(dl.milk ~ sex)

data(birthwt, package = "MASS")
require(dplyr, quietly = TRUE)
birthwt <- mutate(birthwt,
  smoke = factor(smoke, labels = c("Non-smoker", "Smoker")),
  Race = factor(race > 1, labels = c("White", "Non-white")))

birthwt = birthwt %>%
  var_labels(
    bwt = 'Birth weight (g)',
    smoke = 'Smoking status'
  )

gen_bst_df(bwt ~ smoke|Race, data = birthwt)



cleanEx()
nameEx("geo_mean")
### * geo_mean

flush(stderr()); flush(stdout())

### Name: geo_mean
### Title: Geometric mean.
### Aliases: geo_mean

### ** Examples

data(IgM, package = "ISwR")
Ab <- data.frame(IgM)
estat(~ IgM, data = Ab)
geo_mean(IgM)



cleanEx()
nameEx("get_r2")
### * get_r2

flush(stderr()); flush(stdout())

### Name: get_r2
### Title: Estimate R2 or Pseudo-R2 from regression models
### Aliases: get_r2

### ** Examples

require(dplyr, quietly = TRUE)
require(sjlabelled, quietly = TRUE)
require(huxtable, quietly = TRUE)

data(birthwt, package = "MASS")
birthwt <- birthwt %>%
  mutate(
    smoke = factor(smoke, labels = c("Non-smoker", "Smoker")),
    race = factor(race, labels = c("White", "African American", "Other"))
  ) %>%
  var_labels(
    bwt = 'Birth weight (g)',
    smoke = 'Smoking status',
    race = 'Race'
  )

model_norm <- lm(bwt ~ smoke + race, data = birthwt)

model_norm %>%
  glm_coef(labels = model_labels(model_norm)) %>%
  as_hux() %>% set_align(everywhere, 2:3, "right") %>%
  theme_pubh(1) %>%
  add_footnote(get_r2(model_norm), font_size = 9)



cleanEx()
nameEx("gf_star")
### * gf_star

flush(stderr()); flush(stdout())

### Name: gf_star
### Title: Annotating a plot to display differences between groups.
### Aliases: gf_star

### ** Examples

data(kfm, package = "ISwR")
require(sjlabelled, quietly = TRUE)
kfm = kfm %>%
  var_labels(
     dl.milk = 'Breast-milk intake (dl/day)',
     sex = 'Sex',
     weight = 'Child weight (kg)',
     ml.suppl = 'Milk substitute (ml/day)',
     mat.weight = 'Maternal weight (kg)',
     mat.height = 'Maternal height (cm)'
     )

kfm %>%
  box_plot(dl.milk ~ sex, fill='thistle', alpha = 0.8) %>%
  axis_labs() %>%
  gf_star(x1 = 1, y1 = 10.9, x2 = 2, y2 = 11, y3 = 11.2)

kfm %>%
  box_plot(dl.milk ~ sex, fill='thistle', alpha = 0.8) %>%
  axis_labs() %>%
  gf_star(1, 10.9, 2, 11, 11.4, legend = 'p = 0.035', size = 2.5)

data(energy, package = "ISwR")
energy = energy %>%
  var_labels(
    expend = 'Energy expenditure (MJ/day)',
    stature = 'Stature'
    )

energy %>%
  strip_error(expend ~ stature, col = 'red') %>%
  axis_labs() %>%
  gf_star(1, 13, 2, 13.2, 13.4, "**")



cleanEx()
nameEx("glm_coef")
### * glm_coef

flush(stderr()); flush(stdout())

### Name: glm_coef
### Title: Table of coefficients from generalised linear models.
### Aliases: glm_coef

### ** Examples

require(dplyr, quietly = TRUE)
require(sjlabelled, quietly = TRUE)

## Continuous outcome.
data(birthwt, package = "MASS")
birthwt <- birthwt %>%
  mutate(
    smoke = factor(smoke, labels = c("Non-smoker", "Smoker")),
    race = factor(race, labels = c("White", "African American", "Other"))
  ) %>%
  var_labels(
    bwt = 'Birth weight (g)',
    smoke = 'Smoking status',
    race = 'Race'
  )

model_norm <- lm(bwt ~ smoke + race, data = birthwt)

glm_coef(model_norm, labels = model_labels(model_norm))

## Logistic regression.
data(diet, package = "Epi")
model_binom <- glm(chd ~ fibre, data = diet, family = binomial)
model_binom %>%
  glm_coef(labels = c("Constant", "Fibre intake (g/day)"))

model_binom %>%
  glm_coef(labels = c("Constant", "Fibre intake (g/day)"), type = "ext")

## For more examples, please read the Vignette on Regression.



cleanEx()
nameEx("harm_mean")
### * harm_mean

flush(stderr()); flush(stdout())

### Name: harm_mean
### Title: Harmonic mean.
### Aliases: harm_mean

### ** Examples

data(IgM, package = "ISwR")
Ab <- data.frame(IgM)
estat(~ IgM, data = Ab)
harm_mean(IgM)



cleanEx()
nameEx("hist_norm")
### * hist_norm

flush(stderr()); flush(stdout())

### Name: hist_norm
### Title: Histogram with Normal density curve.
### Aliases: hist_norm

### ** Examples

require(dplyr, quietly = TRUE)
require(sjlabelled, quietly = TRUE)
data(birthwt, package = "MASS")
birthwt <- birthwt %>%
  mutate(
    smoke = factor(smoke, labels = c("Non-smoker", "Smoker")),
    Race = factor(race > 1, labels = c("White", "Non-white"))
  ) %>%
  var_labels(
    bwt = 'Birth weight (g)',
    smoke = 'Smoking status'
 )

birthwt %>%
  hist_norm(~ bwt, alpha = 0.7, bins = 20, fill = 'cadetblue') %>%
  axis_labs()

birthwt %>%
  hist_norm(~ bwt|smoke, alpha = 0.7, bins = 20, fill = 'cadetblue') %>%
  axis_labs()



cleanEx()
nameEx("jack_knife")
### * jack_knife

flush(stderr()); flush(stdout())

### Name: jack_knife
### Title: Ranks leverage observations from Jackknife method.
### Aliases: jack_knife

### ** Examples

x <- rnorm(10, 170, 8)
x
mean(x)
jack_knife(x)

x <- rnorm(100, 170, 8)
mean(x)
head(jack_knife(x))



cleanEx()
nameEx("knife_mean")
### * knife_mean

flush(stderr()); flush(stdout())

### Name: knife_mean
### Title: Jackknife for means.
### Aliases: knife_mean

### ** Examples

x <- rnorm(10, 170, 8)
x
mean(x)
knife_mean(x)



cleanEx()
nameEx("leverage")
### * leverage

flush(stderr()); flush(stdout())

### Name: leverage
### Title: Leverage.
### Aliases: leverage

### ** Examples

x <- rnorm(10, 170, 8)
x
mean(x)
leverage(x)
rank_leverage(x)



cleanEx()
nameEx("logistic_gof")
### * logistic_gof

flush(stderr()); flush(stdout())

### Name: logistic_gof
### Title: Goodness of fit for Logistic Regression.
### Aliases: logistic_gof

### ** Examples

data(diet, package = "Epi")
model <- glm(chd ~ fibre, data = diet, family = binomial)
glm_coef(model, labels = c("Constant", "Fibre intake (g/day)"))
logistic_gof(model)



cleanEx()
nameEx("mhor")
### * mhor

flush(stderr()); flush(stdout())

### Name: mhor
### Title: Mantel-Haenszel odds ratio.
### Aliases: mhor

### ** Examples

data(oswego, package = "epitools")
require(dplyr, quietly = TRUE)
require(sjlabelled, quietly = TRUE)
oswego <- oswego %>%
  mutate(
    ill = factor(ill, labels = c("No", "Yes")),
    sex = factor(sex, labels = c("Female", "Male")),
    chocolate.ice.cream = factor(chocolate.ice.cream, labels = c("No", "Yes"))
  ) %>%
  var_labels(
    ill = "Developed illness",
    sex = "Sex",
    chocolate.ice.cream = "Consumed chocolate ice cream"
  )

require(moonBook, quietly = TRUE)
mytable(ill ~ sex + chocolate.ice.cream, data = oswego, show.total = TRUE)

oswego %>%
  mhor(ill ~ sex/chocolate.ice.cream)



cleanEx()
nameEx("model_labels")
### * model_labels

flush(stderr()); flush(stdout())

### Name: model_labels
### Title: Using labels as coefficient names in tables of coefficients.
### Aliases: model_labels

### ** Examples

require(dplyr, quietly = TRUE)
require(sjlabelled, quietly = TRUE)

data(birthwt, package = "MASS")
birthwt <- birthwt %>%
  mutate(
    smoke = factor(smoke, labels = c("Non-smoker", "Smoker")),
    race = factor(race, labels = c("White", "African American", "Other"))
  ) %>%
  var_labels(
    bwt = 'Birth weight (g)',
    smoke = 'Smoking status',
    race = 'Race'
  )

model_norm <- lm(bwt ~ smoke + race, data = birthwt)

glm_coef(model_norm, labels = model_labels(model_norm))

model_int = lm(formula = bwt ~ smoke*race, data = birthwt)

model_int %>%
  glm_coef(labels = c(
    model_labels(model_int),
    "Smoker: African American",
    "Smoker: Other"
 ))



cleanEx()
nameEx("multiple")
### * multiple

flush(stderr()); flush(stdout())

### Name: multiple
### Title: Multiple comparisons with plot.
### Aliases: multiple

### ** Examples

data(birthwt, package = "MASS")
birthwt$race <- factor(birthwt$race, labels = c("White", "African American", "Other"))

model_1 <- aov(bwt ~ race, data = birthwt)
multiple(model_1, ~ race)$df

multiple(model_1, ~ race)$fig_ci %>%
  gf_labs(y = 'Race', x = 'Difference in birth weights (g)')

multiple(model_1, ~ race)$fig_pval %>%
  gf_labs(y = 'Race')



cleanEx()
nameEx("odds_trend")
### * odds_trend

flush(stderr()); flush(stdout())

### Name: odds_trend
### Title: Function to calculate OR using Wald CI, and plot trend.
### Aliases: odds_trend

### ** Examples

## A cross-sectional study looked at the association between obesity and a biopsy resulting
## from mammography screening.

Freq <- c(3441, 34, 39137, 519, 20509, 280, 12149, 196, 11882, 199)
Biopsy <- gl(2, 1, 10, labels = c("No", "Yes"))
Weight <- gl(5, 2, 10, labels = c("Underweight", "Normal", "Over (11-24%)",
             "Over (25-39%)", "Over (> 39%)"))
breast <- data.frame(Freq, Biopsy, Weight)
breast

breast <- expand_df(breast)
require(sjlabelled, quietly = TRUE)

breast = var_labels(breast,
  Weight = 'Weight group'
  )

odds_trend(Biopsy ~ Weight, data = breast)$df

odds_trend(Biopsy ~ Weight, data = breast)$fig



cleanEx()
nameEx("predict_inv")
### * predict_inv

flush(stderr()); flush(stdout())

### Name: predict_inv
### Title: Given y solve for x in a simple linear model.
### Aliases: predict_inv

### ** Examples

## Spectrophotometry example. Titration curve for riboflavin (nmol/ml). The sample has an absorbance
## of 1.15. Aim is to estimate the concentration of riboflavin in the sample.

Riboflavin <- seq(0, 80, 10)
OD <- 0.0125 * Riboflavin + rnorm(9, 0.6, 0.03)
titration <- data.frame(Riboflavin, OD)

require(sjlabelled, quietly = TRUE)
titration <- titration %>%
  var_labels(
    Riboflavin = "Riboflavin (nmol/ml)",
    OD = "Optical density"
  )

titration %>%
  gf_point(OD ~ Riboflavin) %>%
  gf_smooth(col = 'indianred3', se = TRUE, lwd = 0.5, method = 'loess') %>%
  axis_labs()

## Model with intercept different from zero:
model <- lm(OD ~ Riboflavin, data = titration)
glm_coef(model)
predict_inv(model, 1.15)



cleanEx()
nameEx("prop_or")
### * prop_or

flush(stderr()); flush(stdout())

### Name: prop_or
### Title: Proportion, p1 from proportion p2 and OR.
### Aliases: prop_or

### ** Examples

flu <- matrix(c(20, 80, 220, 140), nrow = 2)
colnames(flu) <- c("Yes", "No")
rownames(flu) <- c("Vaccine", "Placebo")
flu

or <- (20 * 140) / (80 * 220)
p2 <- 80 / 220
prop_or(p2 = p2, or = or)
20 / 240



cleanEx()
nameEx("pseudo_r2")
### * pseudo_r2

flush(stderr()); flush(stdout())

### Name: pseudo_r2
### Title: Pseudo R2 (logistic regression) 'pseudo_r2' Calculates R2
###   analogues (pseudo R2) of logistic regression.
### Aliases: pseudo_r2

### ** Examples

data(Oncho)
model_oncho <- glm(mf ~ area, data = Oncho, binomial)
glm_coef(model_oncho, labels = c("Constant", "Area (rainforest/savannah)"))
pseudo_r2(model_oncho)



cleanEx()
nameEx("qq_plot")
### * qq_plot

flush(stderr()); flush(stdout())

### Name: qq_plot
### Title: Quantile-quantile plots against the standard Normal
###   distribution.
### Aliases: qq_plot

### ** Examples

data(kfm, package = "ISwR")
require(sjlabelled, quietly = TRUE)
kfm = kfm %>%
  var_labels(
     dl.milk = 'Breast-milk intake (dl/day)',
     sex = 'Sex',
     weight = 'Child weight (kg)',
     ml.suppl = 'Milk substitute (ml/day)',
     mat.weight = 'Maternal weight (kg)',
     mat.height = 'Maternal height (cm)'
     )

kfm %>%
  qq_plot(~ dl.milk) %>%
  axis_labs()

qq_plot(~ dl.milk|sex, data = kfm) %>%
  axis_labs()



cleanEx()
nameEx("rank_influence")
### * rank_influence

flush(stderr()); flush(stdout())

### Name: rank_influence
### Title: Ranks observations based upon influence measures on models.
### Aliases: rank_influence

### ** Examples

data(diet, package = "Epi")
model <- glm(chd ~ fibre, data = diet, family = binomial)
rank_influence(model)



cleanEx()
nameEx("rank_leverage")
### * rank_leverage

flush(stderr()); flush(stdout())

### Name: rank_leverage
### Title: Ranks observations by leverage.
### Aliases: rank_leverage

### ** Examples

x <- rnorm(10, 170, 8)
x
mean(x)
rank_leverage(x)

x <- rnorm(100, 170, 8)
mean(x)
head(rank_leverage(x))



cleanEx()
nameEx("reference_range")
### * reference_range

flush(stderr()); flush(stdout())

### Name: reference_range
### Title: Reference range (reference interval).
### Aliases: reference_range

### ** Examples

x <- rnorm(100, 170, 8)
round(mean(x), 2)
round(sd(x), 2)

round(reference_range(mean(x), sd(x)), 2)



cleanEx()
nameEx("rel_dis")
### * rel_dis

flush(stderr()); flush(stdout())

### Name: rel_dis
### Title: Relative Dispersion.
### Aliases: rel_dis

### ** Examples

height <- rnorm(100, 170, 8)
rel_dis(height)



cleanEx()
nameEx("ss_jk")
### * ss_jk

flush(stderr()); flush(stdout())

### Name: ss_jk
### Title: Sum of squares for Jackknife.
### Aliases: ss_jk

### ** Examples

x <- rnorm(10, 170, 8)
x
mean(x)
ss_jk(x, mean(x))
jack_knife(x)



cleanEx()
nameEx("strip_error")
### * strip_error

flush(stderr()); flush(stdout())

### Name: strip_error
### Title: Strip plots with error bars.
### Aliases: strip_error

### ** Examples

data(energy, package="ISwR")
require(sjlabelled, quietly = TRUE)
energy = energy %>%
  var_labels(
    expend = 'Energy expenditure (MJ/day)',
    stature = 'Stature'
  )

energy %>%
  strip_error(expend ~ stature, col = 'red') %>%
  axis_labs()

t.test(expend ~ stature, data = energy)

## Adding an horizontal line to show significant difference:
energy %>%
  strip_error(expend ~ stature, col = 'red') %>%
  axis_labs() %>%
  gf_star(1, 13, 2, 13.2, 13.4, "**")

data(birthwt, package = "MASS")
require(dplyr, quietly = TRUE)
birthwt <- birthwt %>%
  mutate(
    smoke = factor(smoke, labels = c("Non-smoker", "Smoker")),
    Race = factor(race > 1, labels = c("White", "Non-white"))
  ) %>%
  var_labels(
    bwt = 'Birth weight (g)',
    smoke = 'Smoking status'
 )

birthwt %>%
  strip_error(bwt ~ smoke|Race, col = 'darksalmon') %>%
  axis_labs()

birthwt %>%
  strip_error(bwt ~ smoke, col = ~ Race) %>%
  gf_refine(ggsci::scale_color_jama()) %>%
  axis_labs()

birthwt %>%
  strip_error(bwt ~ smoke, pch = ~ Race, col = ~ Race) %>%
  gf_refine(ggsci::scale_color_jama()) %>%
  axis_labs()

birthwt %>%
  strip_error(bwt ~ smoke|Race) %>%
  axis_labs()



cleanEx()
nameEx("theme_pubh")
### * theme_pubh

flush(stderr()); flush(stdout())

### Name: theme_pubh
### Title: A theme for huxtables This function quickly set a default style
###   for a huxtable.
### Aliases: theme_pubh

### ** Examples

require(huxtable, quietly = TRUE)
data(Oncho)

Oncho %>%
  cross_tab(area ~ mf) %>%
  theme_pubh()

data(Bernard)

t1 <- estat(~ apache|fate, data = Bernard)
t2 <- estat(~ o2del|fate, data = Bernard)
rbind(t1, t2) %>%
  as_hux() %>%
  theme_pubh(c(1, 3))



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
