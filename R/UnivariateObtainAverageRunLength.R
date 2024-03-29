#' @title Run Length
#' @description Get the run length
#' @inheritParams getDist.test
#' @inheritParams NS.test
#' @param replica scalar. It is used for the parallel version of the function (\code{parallel=TRUE}). Default \code{1}.
#' @param n scalar. Subroup size
#' @param m scalar. Reference sample size
#' @param mu vector. Two elements, the first one is the mean of the reference sample and the second one is the mean of the monitoring sample.
#' @param sigma vector. Two elements, the first one is the sd of the reference sample and the second one is the sd of the monitoring sample.
#' @param dist.par vector. Distribution parameters. \code{c(par.a, par.b)}. Default \code{c(0,1)}.
#' @param chart character string. Selected type of chart. Three options are available: Shewhart, CUSUM, EWMA
#' @param chart.par vector. The size depends on the selected chart:
#' \describe{
#'   \item{Shewhart scheme: }{is \code{c(k)}, where \code{k} comes from \eqn{UCL = mu + k\sigma, LCL = mu - k\sigma.}}
#'   \item{CUSUM scheme: }{is \code{c(k, h, t)} where \code{k} is the reference value and \code{h} is the control limit,
#'   and \code{t} is the type of the chart (1:positive, 2:negative, 3:two sides)}
#'   \item{EWMA scheme: }{is \code{c(lambda, L)}, where \code{lambda} is the smoothing constant
#'   and \code{L} multiplies standard deviation to get the control limit}
#' }
#' @param calibrate logical. If \code{TRUE} the RL is limit to 10 times the target ARL.
#' @param arl0 scalar. Expected value of the RL. Default \code{370}.
#' @param isFixed logical. If \code{TRUE} the reference sample does not update, otherwise the reference sample is updated whenever the batch is in control.
#' @export
#' @import stats
#' @examples
#' n <- 5 # subgroup size
#' m <- 100 # reference-sample size
#' dist <- "Normal"
#' mu <- c(0, 0) # c(reference sample mean, monitoring sample mean)
#' sigma <- c(1, 1) # c(reference sample sd, monitoring sample sd)
#'
#' #### Distribution parameters
#' dist.par <- c(0, 1, 1) # c(location, scale, shape)
#'
#' #### Other Parameters
#' replicates <- 2
#' print.RL <- TRUE
#' calibrate <- FALSE
#' progress <- TRUE
#' arl0 <- 370
#'
#' #### Control chart parameters
#' chart <- "Shewhart"
#' chart.par <- c(3)
#' shewhart <- getRL.test(1, n, m,
#'   theta = NULL, Ftheta = NULL,dist, mu, sigma, dist.par = dist.par,
#'   chart = chart, chart.par = chart.par, calibrate = calibrate, arl0 = arl0
#' )
#'
#' chart <- "CUSUM"
#' chart.par <- c(0.25, 4.4181, 3)
#' cusum <- getRL.test(1, n, m,
#'   theta = NULL, Ftheta = NULL, dist, mu, sigma, dist.par = dist.par,
#'   chart = chart, chart.par = chart.par, calibrate = calibrate, arl0 = arl0
#' )
#'
#' chart <- "EWMA"
#' chart.par <- c(0.2, 2.962)
#' shewhart <- getRL.test(1, n, m,
#'   theta = NULL, Ftheta = NULL,dist, mu, sigma, dist.par = dist.par,
#'   chart = chart, chart.par = chart.par, calibrate = calibrate, arl0 = arl0
#' )
getRL.test <- function(replica = 1, n, m, theta = NULL, Ftheta = NULL,
                  dist, mu, sigma, dist.par = c(0,1,1), scoring = "Z",
                  chart, chart.par, calibrate = FALSE, arl0 = 370,
                  alignment = "unadjusted", constant = NULL, absolute=FALSE,
                  isFixed=FALSE,Chi2corrector="None", rounding.factor = NULL,
                  tie.correction = "EstimateSD",
                  stop.times = 1, replicates = 1,
                  limit.arl0=FALSE) {
  # initilize the reference sample
  Y <- NULL
  if (m > 0) { # if there are reference sample
    # generate the reference sample
    Y <- SNS.test::getDist.test(n = m, dist = dist, mu = mu[1], sigma = sigma[1], dist.par = dist.par, rounding.factor = rounding.factor)
    if (!is.null(rounding.factor)){
      if (tie.correction != "None"){
        #tie.correction = EstimatedSD or Studentize
        y.ns = SNS.test::NS.test(X = Y)
        z.ns = y.ns$Z
        z.sd = sd(z.ns)
        if (tie.correction == "EstimateSD2"){
          z.sd = sqrt(mean(z.ns^2))
        }
        #tie.correction = Studentize
        mean.ref = mean(z.ns)
        df = length(Y) - 1
      }
    }
  }

  RL <- 0
  in.Control <- TRUE

  switch(chart,
    Shewhart = {
      k <- chart.par[1]
    },
    CUSUM = {
      #type is always the last value in vector
      type = chart.par[length(chart.par)]

      if(type == 3 && scoring == "Z-SQ"){
        kp <- chart.par[1]
        km <- chart.par[2]
        h <- chart.par[3]
      }else{
        k <- chart.par[1]
        h <- chart.par[2]

        kp <- k
        km <- k
      }

      Cplus <- 0
      Cminus <- 0
    },
    EWMA = {
      lambda <- chart.par[1]
      L <- chart.par[2]
      E <- 0
    }
  )

  while (in.Control) {
    # add one iteration to run length
    RL <- RL + 1

    # generate the subgroup to monitor
    X <- SNS.test::getDist.test(n = n, dist = dist, mu = mu[2], sigma = sigma[2], dist.par = dist.par, rounding.factor = rounding.factor)

    # get the normal scores
    ns <- SNS.test::NS.test(X = X, Y = Y, theta = theta, Ftheta = Ftheta, alignment = alignment, constant = constant, scoring = scoring, Chi2corrector=Chi2corrector)
    Z <- ns$Z




    switch(scoring,
           "Z" = {# it is a vector with a subgroup size so it is needed to average them
             Z = mean(Z)
           },
           "Z-SQ" = {# it is a vector with a subgroup size so it is needed to sum them
             Z = sum(Z)
           }
    )


    #Correction for ties
    if (!is.null(rounding.factor)){
      if(tie.correction == "Studentize"){
        t.mean = (Z - mean.ref) / (z.sd/sqrt(n))
        Z = qnorm(p = pt(q = t.mean, df = df), mean = 0, sd = 1)
      }
    }

    # if the subgroup is out of the limits
    # an alarm is detected
    switch(chart,
      Shewhart = {
        # if the subgroup is out of the limits an alarm is detected
        ucl = k
        if (scoring == "Z"){
          ucl = ucl / sqrt(n) #k / sqrt(n)
          #Correction for ties
          if (!is.null(rounding.factor)){
            if(tie.correction != "None"){
              ucl = k * z.sd/sqrt(n) #if tie.correction == "EstimateSD" || tie.correction == "EstimateSD2"
              if(tie.correction == "Studentize"){
                ucl = k
              }
            }
          }
        }
        if (abs(Z) >= ucl) in.Control <- FALSE
      },
      CUSUM = {
        if(scoring == "Z-SQ"){# for obtain variance Z/n
          Z = Z /(n * sqrt(n))
        }
        switch(type,
          "1" = {
            Cplus <- max(c(0, Cplus + Z * sqrt(n) - kp))
          },
          "2" = {
            Cminus <- min(c(0, Cminus + Z * sqrt(n) + km))
          },
          "3" = {
            Cplus <- max(c(0, Cplus + Z * sqrt(n) - kp))
            Cminus <- min(c(0, Cminus + Z * sqrt(n) + km))
          }
        )
        if (Cplus >= h || Cminus <= -h) in.Control <- FALSE
      },
      EWMA = {
        E <- lambda * Z + (1 - lambda) * E

        UCL <- L / sqrt(n) * sqrt(lambda / (2 - lambda) * (1 - (1 - lambda)^(2 * RL)))
        # LCL = - UCL

        if (abs(E) >= UCL) in.Control <- FALSE
      }
    )


    if (!is.null(rounding.factor)){
        if (limit.arl0){
          tolerance = arl0 * 1e5 / arl0
          p = 1# - dbinom(stop.times, replicates, prob = 0.5)
          if ((RL >= tolerance) || (p < 0.0001)){
            RL = tolerance
            in.Control <- FALSE
          }
        }


    }else{
      if (calibrate){
        if (RL >= arl0*1000){
          in.Control <- FALSE
        }
      }
      #if (RL >= arl0 * 1e6){
      #  in.Control <- FALSE
      #}
    }

    # update the reference sample
    if(!isFixed){#if the reference sample is updated (not fixed)
      Y <- c(Y, X)
    }
  }
  return(RL)
}

#' @title Average Run Length (ARL)
#' @description Get the ARL \code{\link{getRL.test}}
#' @inheritParams getRL.test
#' @param print.RL logical. If \code{TRUE} return the vectors of RL for each iteration.
#' @param replicates scalar. Number of replicates to get the ARL
#' @param progress logical. If \code{TRUE} it shows the progress in the console.
#' @param isParallel logical. If \code{TRUE} the code runs in parallel according to the
#' number of cores in the computer,otherwise the code runs sequentially. Default \code{TRUE}.
#' @export
#' @import parallel
#' @import stats
#' @examples
#' n <- 5 # subgroup size
#' m <- 100 # reference-sample size
#' dist <- "Normal"
#' mu <- c(0, 0) # c(reference sample mean, monitoring sample mean)
#' sigma <- c(1, 1) # c(reference sample sd, monitoring sample sd)
#'
#' #### Normal distribution parameters
#' dist.par <- c(0, 1) # c(location, scale)
#'
#' #### Other Parameters
#' replicates <- 2
#' print.RL <- TRUE
#' isParallel <- FALSE
#' calibrate <- FALSE
#' progress <- TRUE
#' arl0 <- 370
#'
#' #### Control chart parameters
#' chart <- "Shewhart"
#' chart.par <- c(3)
#' shewhart <- getARL.test(n, m,
#'   theta = NULL, Ftheta = NULL, dist, mu, sigma, dist.par = dist.par,
#'   chart = chart, chart.par = chart.par, print.RL = print.RL,
#'   replicates = replicates, isParallel = isParallel,
#'   calibrate = calibrate, arl0 = arl0
#' )
#'
#' chart <- "CUSUM"
#' chart.par <- c(0.25, 4.4181, 3)
#' cusum <- getARL.test(n, m,
#'   theta = NULL, Ftheta = NULL, dist, mu, sigma, dist.par = dist.par,
#'   chart = chart, chart.par = chart.par, print.RL = print.RL,
#'   replicates = replicates, isParallel = isParallel,
#'   calibrate = calibrate, arl0 = arl0
#' )
#'
#' chart <- "EWMA"
#' chart.par <- c(0.2, 2.962)
#' shewhart <- getARL.test(n, m,
#'   theta = NULL, Ftheta = NULL, dist, mu, sigma, dist.par = dist.par,
#'   chart = chart, chart.par = chart.par, print.RL = print.RL,
#'   replicates = replicates, isParallel = isParallel,
#'   calibrate = calibrate, arl0 = arl0
#' )
getARL.test <- function(n, m, theta = NULL, Ftheta = NULL,
                   dist, mu, sigma, dist.par = c(0, 1, 1),
                   chart, chart.par, scoring = "Z",Chi2corrector="None",
                   replicates = 10000, isParallel = TRUE,
                   print.RL = FALSE, progress = FALSE,
                   calibrate = FALSE, arl0 = 370,
                   alignment = "unadjusted", constant = NULL, absolute=FALSE,
                   isFixed=FALSE, rounding.factor = NULL,
                   tie.correction = "EstimateSD") {

  type = chart.par[length(chart.par)]
  if(type == 3 && scoring == "Z-SQ"){
    if(length(chart.par) < 4){
      stop("Missing argument in chart.par. Four arguments needed.")
    }
  }
  RLs <- NULL
  stop.times <- 0
  if (isParallel) {
    #probar con esta solucion
    # https://stackoverflow.com/questions/56985567/r-parallel-programming-with-two-loops-and-storage-results/56986582
    #iter_stop.times <- iterators::icount(replicates)
    #stop.times <- as.numeric(iterators::nextElem(iter_stop.times))
    cluster <- parallel::makeCluster(parallel::detectCores() - 1)
    #export variables
    #parallel::clusterExport(cluster, c("stop.times", "n", "m", "theta", "Ftheta", "dist", "mu", "sigma", "dist.par", "chart", "chart.par", "calibrate", "arl0", "alignment", "constant","absolute","isFixed","scoring","Chi2corrector", "rounding.factor","tie.correction"), envir = environment())
    parallel::clusterExport(cluster, c("stop.times"), envir = environment())
    parallel::clusterExport(cluster, "NS.test")
    parallel::clusterExport(cluster, "getDist.test")
    parallel::clusterExport(cluster, "getRL.test")

    #doParallel::registerDoParallel(cluster)
    RLs <- parallel::parSapply(cluster, 1:replicates, getRL.test, n = n, m = m, theta = theta, Ftheta = Ftheta, dist = dist, mu = mu, sigma = sigma, dist.par = dist.par, chart = chart, chart.par = chart.par, calibrate = calibrate, arl0 = arl0, alignment=alignment, constant=constant,absolute=absolute,isFixed=isFixed,scoring=scoring,Chi2corrector=Chi2corrector, rounding.factor = rounding.factor,tie.correction =tie.correction)
    #RLs <- foreach::foreach(r = 1:replicates, .combine=c) %do% {
    #  RL <- SNS.test::getRL.test(1, n = n, m = m, theta = theta, Ftheta = Ftheta, dist = dist, mu = mu, sigma = sigma, dist.par = dist.par, chart = chart, chart.par = chart.par, calibrate = calibrate, arl0 = arl0, alignment=alignment, constant=constant,absolute=absolute,isFixed=isFixed,scoring=scoring,Chi2corrector=Chi2corrector, rounding.factor = rounding.factor,tie.correction =tie.correction, stop.times = stop.times, replicates = r)
    #  if(RL == arl0  * 15){
        #stop.times <- as.numeric(iterators::nextElem(iter_stop.times))
    #    stop.times <- stop.times + 1
    #  }
    #  RL
    #}


    parallel::stopCluster(cluster)
    stop.times = sum(RLs == arl0 *15)
  } else {
    t0 <- Sys.time()

    for (r in 1:replicates) {
      RL <- SNS.test::getRL.test(1, n = n, m = m, theta = theta, Ftheta = Ftheta, dist = dist, mu = mu, sigma = sigma, dist.par = dist.par, chart = chart, chart.par = chart.par, calibrate = calibrate, arl0 = arl0, alignment=alignment, constant=constant,absolute=absolute,isFixed=isFixed,scoring=scoring,Chi2corrector=Chi2corrector, rounding.factor = rounding.factor,tie.correction =tie.correction, stop.times = stop.times, replicates = r)
      if(RL == arl0  * 15){
        stop.times <- stop.times + 1
      }
      RLs <- c(RLs, RL)

      # print out progress
      if (progress) { # if is TRUE
        if (r %% 10 == 0) { # every 10 replicates
          t1 <- Sys.time()
          remaining.iterations <- replicates - r
          remaining.time <- remaining.iterations * difftime(t1, t0, units = "min") / r
          cat("ARL", round(mean(RLs), digits = 1), "-- SDRL", round(sd(RLs), digits = 1), "--> Time remaining", remaining.time, "in minutes to complete", remaining.iterations, "iterations", "\n", sep = " ")
        }
      }
    }
  }

  output <- list(
    ARL = mean(RLs),
    SDRL = sd(RLs),
    MRL = median(RLs),
    QRL = quantile(x = RLs, probs = c(0.05, 0.1, 0.2, 0.25, 0.5, 0.75, 0.8, 0.9, 0.95), names = TRUE, type = 3),
    stop.times = stop.times
  )
  if (print.RL) output$RL <- RLs

  if (progress) cat("Final ARL", round(mean(RLs), digits = 1), "-- SDRL", round(sd(RLs), digits = 1), "\n", "See output variable for more.\n\n", sep = " ")

  return(output)
}
