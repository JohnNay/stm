searchK <- function(documents, vocab, K, init.type = "Spectral", 
                    N=floor(.1*length(documents)), proportion=.5, 
                    heldout.seed=NULL,
                    M=10,
                    parallel = FALSE,
                    cores = NULL,
                    ...) {
  if(parallel) {
    forloop <- foreach::`%dopar%`
    # Save one core for watching Netflix:
    if (missing(cores)) cores <- parallel::detectCores() - 1
    doParallel::registerDoParallel(cores = cores)
  } else {
    forloop <- foreach::`%do%`
  }
  
  # Make a heldout dataset:
  heldout <- make.heldout(documents,vocab, N=N, proportion=proportion, 
                          seed=heldout.seed)
  
  # Loop over each of the number of topics and estimate a model for each:
  o <- forloop(foreach::foreach(i=seq(length(K))), {
    #Set up the object to return
    g <- as.list(rep(NA, 8))
    names(g) <- c("K","heldout","residual","bound","lbound","exclus","semcoh","em.its")
    
    g$K <- K[i]

    model <- stm(documents=heldout$documents,vocab=heldout$vocab,
                 K=K[i], init.type=init.type,...)
    
    # Calculate values to return:
    if(length(model$beta$logbeta)!=1){
      warning("Exclusivity calculation only designed for models without content covariates, so skipping it.")
      g$exclus <- NA
    } else {
      g$exclus <- mean(unlist(exclusivity(model, M=M, frexw=.7)))
    }
    g$semcoh <-mean(unlist(semanticCoherence(model, heldout$documents, M)))
    g$heldout <-eval.heldout(model, heldout$missing)$expected.heldout    
    g$residual <-checkResiduals(model,heldout$documents)$dispersion
    g$bound <-max(model$convergence$bound)
    g$lbound <-max(model$convergence$bound) + lfactorial(model$settings$dim$K)
    g$em.its <-length(model$convergence$bound)    
    g
  })
  
  K <- sapply(o, function(x) x$K)
  heldout <- sapply(o, function(x) x$heldout)
  residual <- sapply(o, function(x) x$residual)
  bound <- sapply(o, function(x) x$bound)
  lbound <- sapply(o, function(x) x$lbound)
  exclus <- sapply(o, function(x) x$exclus)
  semcoh <- sapply(o, function(x) x$semcoh)
  em.its <-sapply(o, function(x) x$em.its)
  
  g <- data.frame(K,heldout,residual,bound,lbound,exclus,semcoh,em.its)
  toreturn <- list(results=g, call=match.call(expand.dots=TRUE))
  class(toreturn)<- "searchK"
  return(toreturn)
}
