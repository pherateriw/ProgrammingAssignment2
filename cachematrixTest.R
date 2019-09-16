## functions and test cases for cachematrix.R

## Function for generating a random square invertible matrix
generateTest <- function(dimensions = 3, max_value = 100, retry = 0){
    m <- matrix(sample.int(max_value, size=dimensions*dimensions, replace = TRUE), 
           nrow = dimensions, ncol = dimensions)
    det_m <- det(m)
    if(det_m != 0){
        return(m)
    } else if(det_m == 0 & retry < 10){
        retry <- retry + 1
        warning('Attempt ', retry, ' to create invertible matrix failed. Retrying')
        return(generateTest(dimensions = dimensions, 
                            max_value = max_value, retry = retry))
    } else {
        warning('Not able to generate invertible matrix in 10 retries')
        return(NA)
    }
}

## Specific Test cases
A3x3 = matrix(c(3,0,2,2,0,-2,0,1,1), nrow = 3, ncol = 3)
A4x4 = matrix(c(1,0,0,1,0,2,1,2,2,1,0,1,2,0,1,4), nrow = 4, ncol = 4)

#perform matrix multiplication, round to precision, 
#and check that the result is the identity matrix
assertIsInverse <- function(m,i,prec = 0){
    ident <- m %*% i
    ident <- round(ident, prec)
    for(row in 1:nrow(ident)){
        if(ident[row,row] != 1 | sum(ident[row,]) != 1){
            warning('This matrix is not the inverse!')
            return(FALSE)
        }
    }
    return(TRUE)
}