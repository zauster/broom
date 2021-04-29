#' @templateVar class feglm
#' @template title_desc_tidy
#'
#' @param x A `feglm` object returned from [alpaca::feglm()].
#' @template param_confint
#' @param fe Logical indicating whether or not to include estimates of
#'   fixed effects. Defaults to `FALSE`.
#' @param se.type Character indicating the type of covariance
#'   estimate to be used for the standard errors. Possible values
#'   are `hessian`, `outer.product`, `sandwich`, `cluster`. See
#'   alpaca::summary.feglm for details. Defaults to `hessian`.
#' @template param_unused_dots
#'
#' @evalRd return_tidy(regression = TRUE)
#'
#' @examples
#'
#' library(alpaca)
#'
#' dt <- simGLM(n = 40, t = 10, seed = 123, model = "poisson")
#'
#' res_feglm <- feglm(formula = y ~ x1 + x2 + x3 | i + t,
#'                    data = dt, family = poisson())
#' res_feglm
#' tidy(res_feglm)
#' tidy(res_feglm, type = "sandwich")
#' tidy(res_feglm, type = "clustered", cluster = ~ i + t)
#' augment(res_feglm)
#' glance(res_feglm)
#'
#' @aliases feglm_tidiers alpaca_tidiers
#' @family feglm tidiers
#' @seealso [alpaca::feglm()]
#' @export
tidy.feglm <- function(x,
                       conf.int = FALSE, conf.level = 0.95,
                       fe = FALSE, se.type = "hessian",
                       ## cluster = NULL,
                       ...) {
  dots <- list(...)
  cluster <- NULL

  ## TODO cannot use 'cluster' parameter in function
  ## @param cluster a symbolic description indicating the
  ##   clustering of observations.
  if(!is.null(dots$cluster)) {
    cluster <- dots$cluster
  }

  if(conf.int == TRUE) {
    ## TODO
    warning("Both conf.int and conf.level are not implemented yet. Ignoring now.")
  }
  if(se.type != "cluster" & !is.null(cluster)) {
    warning("Specify cluster SE with se.type = 'cluster'. Ignoring the cluster expression now.")
    cluster <- NULL
  }
  if(se.type == "cluster" & is.null(cluster)) {
    warning("Specified cluster SE but no cluster expression (e.g.: cluster = ~ var1 + var2). Falling back to se.type = 'hessian'.")
    se.type = "hessian"
  }

  xs <- summary(x, type = se.type, cluster = cluster)

  ## get the coefficient matrix and add the variable names as the
  ## first column
  ret <- tibble::as_tibble(xs$cm)
  ## ret <- data.frame(xs$cm)
  colnames(ret) <- c("estimate", "std.error", "statistic", "p.value")
  ## ret <- tibble::add_column(ret, term = rownames(xs$cm), .before = 1)
  ret$term <- rownames(xs$cm)
  ret <- ret[, c("term", "estimate", "std.error",
                 "statistic", "p.value")]

  ret
}

#' @templateVar class feglm
#' @template title_desc_augment
#'
#' @inherit tidy.feglm params examples
#' @template param_data
#'
#' @evalRd return_augment()
#'
#' @export
#' @family feglm tidiers
#' @seealso [augment()], [alpaca::feglm()]
#' @importFrom stats fitted
#' @export
augment.feglm <- function(x, data = x$data, ...) {
  df <- tibble::as_tibble(x$data)
  ## df <- x$data
  fitted_vals <- as.vector(fitted(x))
  resid_vals <- as.vector(data[[1]]) - fitted_vals
  df$`.fitted` <- fitted_vals
  df$`.resid` <- resid_vals

  df
}

#' @templateVar class feglm
#' @template title_desc_glance
#'
#' @inherit tidy.feglm params examples
#'
#' @evalRd return_glance(
#'   "AIC",
#'   "BIC",
#'   "logLik",
#'   "null.deviance",
#'   "deviance",
#'   "nobs"
#' )
#'
#' @export
glance.feglm <- function(x, ...) {
  y <- x$data[[1]]
  eta <- x$eta
  mu <- x$family$linkinv(eta)
  wt <- rep(1.0, length(y))
  nobs <- x$nobs["nobs"]
  dev <- sum(x$family$dev.resids(y, mu, wt))
  ## null.dev <- sum(res_feglm1$family[["dev.resids"]](y, mean(y), wt))

  ## this function actually returns -2 * ll
  tmp <- x$family$aic(y = y, n = nobs, mu = mu,
                           wt = wt, dev = dev)
  ll <- tmp / -2

  aic <- tmp + 2 * (sum(x$lvls.k) + length(x$coefficients) - 1)
  bic <- tmp +
    (sum(x$lvls.k) + length(x$coefficients) - 1) *
    log(nobs)

  ret <- tibble::tibble(
  ## ret <- data.frame(
    AIC = aic,
    BIC = bic,
    logLik = ll,
    null.deviance = x[["null.deviance"]],
    deviance = x[["deviance"]],
    nobs = x[["nobs"]]["nobs"])
  ret
}
