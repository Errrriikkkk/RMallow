#' Function used to inform user of the status of the search for the modal sequences
#' @param Ls the minimum lower bound used thus far
#' @param pos the number of items in the queue
#' @param node the search node number
#' @param cluster the cluster the node is searching on
#' @param best the best found sequence
#' @param LUB the least upper bound on the final cost for the cluster
#' @return prints out the status of the search
printInfo <- function(Ls, pos, node, cluster, best, LUB) {
  if (pos > 100) {
    str1 <- paste("(", paste(best, collapse = " "), ")", sep = "")
    str2 <- "(min.cost, LUB, size, node, clust, best): ("
    str3 <- paste(round(Ls, digits = 2), round(LUB, digits = 2), 
                  pos, node, cluster, "", sep = ", ")
    str4 <- ")"
    cat(paste(str2, str3, str1, str4, sep = ""), "\n")
  }
}