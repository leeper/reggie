#' @rdname reg
#' @title Stata-style Regression Modelling Function
#' @description This is a slim wrapper around various regression modelling functions that simplifies the process of estimating regression equations with various robust variance-covariance estimation procedures. This negates the need for the two-step exercise of \code{summary(lm(...))} to view results. It also supports survey design objects through the same interface. Reversing the typical formula-data argument order to data-argument order makes the function easily usable within a data analysis \dQuote{pipeline}.
#' @param data A data frame, or \dQuote{survey design} object from the survey package.
#' @param formula A model formula.
#' @param \dots Additional arguments passed to \code{\link[stats]{glm}} or \code{\link[survey]{svyglm}}.
#' @param vcov_type A character string specifying a variance-covariance estimation procedure. See \code{\link[sandwich]{vcovHC}}.
#' @param vcov_cluster If non-\code{NULL}, a formula specifying a clustering variable to be used for clustered standard errors.
#' @param boot_iterations If \code{vcov_type = "boot"}, an integer specifying the number of bootstrap iterations.
#' @param digits An integer specifying the preferred number of digits to use when printing the output (passed to \code{\link{print.reg}}).
#' @param signif.stars A logical specifying whether to print significance stars. Passed to \code{\link{print.reg}}.
#' @details Estimation is provided by \code{\link[stats]{glm}} and \code{\link[survey]{svyglm}}, respectively.
#' @return A \dQuote{reg} object, which contains a traditional R modelling object in element \dQuote{model}.
#' @importFrom survey svydesign svyglm
#' @importFrom graphics plot
#' @import stats
#' @import lmtest
#' @import sandwich
#' @seealso \code{\link{summary.reg}}
#' @export
reg <- 
function(data, # make it possible for this to be a "survey" design object
         formula, 
         ..., 
         vcov_type = if (is.null(vcov_cluster)) "const" else "HC0",
         vcov_cluster = NULL,
         boot_iterations = 1000L,
         digits = 2L,
         signif.stars = FALSE
         ) {
    
    # estimate model
    if (inherits(data, "data.frame")) {
        mod <- stats::glm(formula = formula, data = data, ...)
    } else if (inherits(data, "survey.design")) {
        mod <- survey::svyglm(formula = formula, design = data, ...)
    }
    n <- length(mod$residuals)
    
    if (!is.null(vcov_cluster)) {
        cluster_vec <- as.integer(interaction(get_all_vars(vcov_cluster, data)))
        if (vcov_type %in% c("boot", "bootstrap")) {
            vc <- sandwich::vcovBS(x = mod, cluster = cluster_vec, R = boot_iterations)
        } else {
            vc <- sandwich::vcovCL(x = mod, cluster = cluster_vec, type = vcov_type)
        }
    } else if (vcov_type == "const") {
        vc <- stats::vcov(mod)
    } else if (vcov_type %in% c("boot", "bootstrap")) {
        once <- function() {
            if (inherits(data, "data.frame")) {
                stats::coef(stats::glm(formula = formula, data = data[sample(seq_len(nrow(data)), nrow(data), TRUE), ], ...))
            } else if (inherits(data, "survey.design")) {
                stats::coef(survey::svyglm(formula = formula, design = data[sample(seq_len(nrow(data)), nrow(data), TRUE), ], ...))
            }
        }
        b <- replicate(boot_iterations, once() )
        vc <- cov(t(b))
    } else {
        vc <- sandwich::vcovHC(x = mod, type = vcov_type)
    }
    
    out <- structure(list(
             model = mod,
             coefficients = lmtest::coeftest(x = mod, vcov. = vc),
             call = sys.call(),
             design_type = if (inherits(data, "data.frame")) "rdd" else "survey",
             design_object = if (inherits(data, "data.frame")) NULL else mod$survey.design,
             data = deparse(substitute(data)),
             digits = digits,
             signif.stars = signif.stars
           ), class = c("reg"))
    return(out)
}

#' @rdname reg-methods
#' @title Methods for objects of class \dQuote{reg}
#' @description Methods for common modelling generics
#' @param x An object returned by \code{\link{reg}}
#' @param object the same
#' @param formula the same
#' @param \dots Additional arguments passed to methods
#' @details These functions simply provide a convenience wrapper allowing for code of the form \code{predict(mod)} rather than \code{predict(mod$model)}. These functions per se do nothing directly.
#' @return An object returned by the underlying method
#' @export
print.reg <- function(x, ...) {
    cat("## Generalized Linear Model\n")
    cat("- Model:  ", deparse(x$model$terms), "\n", sep = "")
    cat("- Family: ", x$model$family$family, " (link: ", x$model$family$link, ")\n", sep = "")
    if (x$design_type == "survey") {
        cat("- Data (n=", length(x$model$residuals), "):\n", sep = "")
        print(x$design_object)
    } else {
        cat("- Data (n=", length(x$model$residuals), "): ", x$data, "\n", sep = "")
    }
    print(x$coefficients, digits = x$digits, signif.stars = x$signif.stars)
}

#' @rdname reg-methods
#' @export
summary.reg <- function(object, ...) {
    print(object, ...)
}

#' @rdname reg-methods
#' @export
predict.reg <- function(object, ...) {
    predict(object$reg, ...)
}

#' @rdname reg-methods
#' @export
plot.reg <- function(x, ...) {
    plot(x$reg, ...)
}

#' @rdname reg-methods
#' @export
vcov.reg <- function(object, ...) {
    vcov(object$reg, ...)
}

#' @rdname reg-methods
#' @export
coef.reg <- function(object, ...) {
    object$coefficients
}

#' @rdname reg-methods
#' @export
residuals.reg <- function(object, ...) {
    residuals(object$reg, ...)
}

#' @rdname reg-methods
#' @export
terms.reg <- function(x, ...) {
    terms(x$reg, ...)
}

#' @rdname reg-methods
#' @export
anova.reg <- function(object, ...) {
    anova(object$reg, ...)
}

#' @rdname reg-methods
#' @export
df.residual.reg <- function(object, ...) {
    df.residual(object$reg, ...)
}

#' @rdname reg-methods
#' @export
deviance.reg <- function(object, ...) {
    deviance(object$reg, ...)
}

#' @rdname reg-methods
#' @export
model.frame.reg <- function(formula, ...) {
    model.frame(formula$reg, ...)
}
