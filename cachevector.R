# v1 <- 1:8
# v <- makeVector(v1)
# cachemean(v)

makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}


cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

cachemean1 <- function(x, newdata, ...) {
        data <- x$get()
        if (is.vector(v1) && is.vector(v2) && length(data) == length(newdata) && data == newdata) {
                m <- x$getmean()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                m <- mean(data, ...)
        } else {
                x$set(newdata)
                m <- mean(newdata, ...)
                x$setmean(m)
        }
        m
}