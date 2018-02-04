#' @title tidy-like method for crwFit object
#' 
#' @description this function mimics the approach taken by `broom::tidy`
#' to present model output parameters in a tidy, data frame structure.
#' @param fit `crwFit` object from `crawl::crwMLE`
#' @export
#' 
crw_broom <- function(fit) {
  terms <- data.frame(term = fit$nms)
  out <- as.data.frame(round(cbind(
    fit$par, fit$se, fit$ci[, 1], fit$ci[, 2]), 3)
  )
  
  colnames(out) <- c("estimate", 
                     "std.error", 
                     "conf.low", 
                     "conf.high")
  out <- cbind(terms,out)
  
  out <- rbind(out,
               data.frame(
                 term = "logLik",
                 estimate = fit$loglik,
                 std.error = NA,
                 conf.low = NA,
                 conf.high = NA
               ),
               data.frame(
                 term = "AIC",
                 estimate = fit$aic,
                 std.error = NA,
                 conf.low = NA,
                 conf.high = NA
               ))
  out
}

tidy_crwFit <- function(fit) {
  crw_broom(fit)
}
