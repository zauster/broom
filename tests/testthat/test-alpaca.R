context("alpaca")

skip_on_cran()

skip_if_not_installed("modeltests")
library(modeltests)

skip_if_not_installed("alpaca")

library(broom)
library(testthat)
library(alpaca)
library(devtools)
load_all()

## create sample data
df <- simGLM(n = 40, t = 10, seed = 123, model = "poisson")

res_alpaca <- feglm(formula = y ~ x1 + x2 + x3 | i + t,
                    data = df, family = poisson())
## res_alpaca

test_that("feglm tidier arguments", {
  check_arguments(tidy.feglm)
  check_arguments(glance.feglm)
  check_arguments(augment.feglm)
})

td1 <- tidy.feglm(res_alpaca)
td2 <- tidy.feglm(res_alpaca, se.type = "cluster") ## TODO expect error
td21 <- tidy.feglm(res_alpaca, se.type = "cluster", cluster = ~ i + t)

ad1 <- augment.feglm(res_alpaca)

gd1 <- glance.feglm(res_alpaca)


## fe.1 <- getFEs(res_alpaca)

## ## df[, x1x2 := x1 * res_alpaca$coefficients[1] + x2 * res_alpaca$coefficients[2]]
## ## res.off <- feglm(formula = y ~ offset(x1x2) + x3 | i + t,
## ##                  data = df, family = poisson())
## ## res.off
## ## getFEs(res.off)

## df[, ix := factor(i)]
## df[, tx := factor(t)]
## res_glm <- glm(formula = y ~-1 + x1 + x2 + x3 + ix + tx,
##              data = df, family = poisson())
## res_glm

## ## res_glm_off <- glm(formula = y ~-1 + offset(x1x2) + x3 + ix + tx,
## ##                data = df, family = poisson())
## ## res_glm_off

## fe <- tidy(res_glm)
## setDT(fe)
## fe[, p.value := NULL]

## tail(fe)

## fe.i <- fe[term %like% "ix", ]
## fe.i[, alpaca := fe.1$i + fe.1$t[1]]
## fe.i
## fe.1$i

## fe[term %like% "tx", ]
## fe.1$t - fe.1$t[1]
