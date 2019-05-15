#' @title Add user and producer accuracies to a confusion matrix.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Add user and produser accuracies to a confusion matrix.
#'
#' @param conmat A confusion matrix or a list.
#' @return       The input matrix with an extra row and column or a list.
#' @export
add_upacc <- function(conmat){
    if (is.matrix(conmat)) {
        stopifnot(nrow(conmat) == ncol(conmat))
        ac_mat <- asses_accuracy_simple(conmat)
        cmat <- cbind(conmat, ac_mat$user)
        cmat <- rbind(cmat, c(ac_mat$producer, NA))
        colnames(cmat)[ncol(cmat)] <- "user_acc"
        rownames(cmat)[nrow(cmat)] <- "prod_acc"
        return(cmat)
    }else if (is.list(conmat)) {
        return(lapply(conmat, add_upacc))
    }else {
        stop("Unknow type of argument")
    }
}


#' @title Asses accuracy
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Compute the overall, user, and producer accuracies.
#'
#' @param error_matrix A matrix given in sample counts. Columns represent the reference data and rows the results of the classification
#' @return             A list of numeric vectors: The overall, user and producer accuracies
#' @export
asses_accuracy_simple <- function(error_matrix){
    overall <- sum(diag(error_matrix))/sum(error_matrix)
    acc_user <- diag(error_matrix)/rowSums(error_matrix)
    acc_prod <- diag(error_matrix)/colSums(error_matrix)
    names(acc_user) <- colnames(error_matrix)
    names(acc_prod) <- rownames(error_matrix)
    return(list(overall = overall, user = acc_user, producer = acc_prod))
}


#' @title Asses accuracy and estimate area according to Olofsson
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Compute the accuracy normalized by the area. Note that, these computations don't work on clustered sampling because the equations are different.
#'
#' @param error_matrix A matrix given in sample counts. Columns represent the reference data and rows the results of the classification
#' @param class_areas  A vector of the total area of each class on the map
#' @return             A list of two lists: The confidence interval (confint95) and the accuracy
#' @export
asses_accuracy_area <- function(error_matrix, class_areas){

    stopifnot(length(colnames(error_matrix)) > 0 && length(rownames(error_matrix)) > 0)
    stopifnot(sum(colnames(error_matrix) == rownames(error_matrix)) == length(colnames(error_matrix))) # the order of columns and rows do not match
    stopifnot(all(colnames(error_matrix) %in% names(class_areas))) # do names match?
    stopifnot(all(names(class_areas) %in% colnames(error_matrix))) # do names match?
    class_areas <- class_areas[colnames(error_matrix)] # re-order elements
    stopifnot(all(colnames(error_matrix) == names(class_areas))) # do names' positions match?

    W <- class_areas/sum(class_areas)
    #W.mat <- matrix(rep(W, times = ncol(error_matrix)), ncol = ncol(error_matrix))
    n <- rowSums(error_matrix)
    n.mat <- matrix(rep(n, times = ncol(error_matrix)), ncol = ncol(error_matrix))
    p <- W * error_matrix / n.mat                                                 # estimated area proportions
    # rowSums(p) * sum(class_areas)                                               # class areas according to the map, that is, the class_areas vector
    error_adjusted_area_estimate <- colSums(p) * sum(class_areas)                 # class areas according to the reference data
    Sphat_1 <- vapply(1:ncol(error_matrix), function(i){                          # S(phat_1) - The standard error of the area estimative is given as a function area proportions and sample counts
        sqrt(sum(W^2 * error_matrix[, i]/n * (1 - error_matrix[, i]/n)/(n - 1)))
    }, numeric(1))
    #
    SAhat <- sum(class_areas) * Sphat_1                                           # S(Ahat_1) - Standard error of the area estimate
    Ahat_sup <- error_adjusted_area_estimate + 2 * SAhat                          # Ahat_1 - 95% superior confidence interval
    Ahat_inf <- error_adjusted_area_estimate - 2 * SAhat                          # Ahat_1 - 95% inferior confidence interval
    Ohat <- sum(diag(p))                                                          # Ohat   - Overall accuracy
    Uhat <- diag(p) / rowSums(p)                                                  # Uhat_i - User accuracy
    Phat <- diag(p) / colSums(p)                                                  # Phat_i - Producer accuracy
    #
    return(
        list(
            # errorArea = SAhat,
            error_matrix = error_matrix,
            class_areas = class_areas,
            confint95 = list(
                superior = Ahat_sup,
                inferior = Ahat_inf
            ),
            accuracy = list(
                overall = Ohat,
                user = Uhat,
                producer = Phat
            )
        )
    )
}

