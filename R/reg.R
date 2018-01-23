#' @title Stata-style Regression Modelling Function
#' @description This is a slim wrapper around various regression modelling functions that simplifies the process of estimating regression equations with various robust variance-covariance estimation procedures. This negates the need for the two-step exercise of \code{summary(lm(...))} to view results. It also supports survey design objects through the same interface. Reversing the typical formula-data argument order to data-argument order makes the function easily usable within a data analysis \dQuote{pipeline}.
#' @param data A data frame, or \dQuote{survey design} object from the survey package.
#' @param formula A model formula.
#' @param \dots Additional arguments passed to \code{\link[stats]{glm}} or \code{\link[survey]{svyglm}}.
#' @param vcov_type A character string specifying a variance-covariance estimation procedure. See \code{\link[sandwich]{vcovHC}}.
#' @param vcov_cluster If non-\code{NULL}, a formula specifying a clustering variable to be used for clustered standard errors.
#' @param boot_iterations If \code{vcov_type = "boot"}, an integer specifying the number of bootstrap iterations.
#' @param digits An integer specifying the preferred number of digits to use when printing the output (passed to \code{\link{print.model}}).
#' @param signif.stars A logical specifying whether to print significance stars. Passed to \code{\link{print.model}}.
#' @return A \dQuote{model} object, which inherits from \dQuote{glm}.
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
        mod <- survey::svyglm(formula = formula, design = data)
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
            stats::coef(stats::glm(formula = formula, data = data[sample(seq_len(nrow(data)), nrow(data), TRUE), ], ...))
        }
        b <- replicate(boot_iterations, once() )
        vc <- cov(t(b))
    } else {
        vc <- sandwich::vcovHC(x = mod, type = vcov_type)
    }
    
    out <- structure(mod,
             coefficients = lmtest::coeftest(x = mod, vcov. = vc),
             call = sys.call(),
             design_type = if (inherits(data, "data.frame")) "rdd" else "survey",
             design_object = if (inherits(data, "data.frame")) NULL else mod$survey.design,
             data = deparse(substitute(data)),
             digits = digits,
             signif.stars = signif.stars,
             class = c("model", "glm", "lm")
           )
    return(out)
}

#' @rdname reg
#' @export
print.model <- function(x, ...) {
    att <- attributes(x)
    cat("## Generalized Linear Model\n")
    cat("- Model:  ", deparse(x$terms), "\n", sep = "")
    cat("- Family: ", x$family$family, " (link: ", x$family$link, ")\n", sep = "")
    if (att$design_type == "survey") {
        cat("- Data (n=", length(x$residuals), "):\n", sep = "")
        print(att$design_object)
    } else {
        cat("- Data (n=", length(x$residuals), "): ", att$data, "\n", sep = "")
    }
    print(att$coefficients, digits = att$digits, signif.stars = att$signif.stars)
}
