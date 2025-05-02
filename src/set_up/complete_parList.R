complete_par_list <- function(par){
    if(is.null(par$drift)){
    par$drift <- rectToPolar(par$mu1, par$mu2)$dLength
    par$angle <- rectToPolar(par$mu1, par$mu2)$dAngle
    }
    if(is.null(par$mu1)){
        par$mu1 <- polarToRect(par$angle, par$drift)$x
        par$mu2 <- polarToRect(par$angle, par$drift)$y
    }
    return(par)
}

