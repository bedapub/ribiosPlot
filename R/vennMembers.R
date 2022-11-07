#' Extract members of each region in Venn diagrams in to a list
#' @param venn A Venn object
#' @param removeNULL Logical, whether NULL elements should be removed
#' @return A named list of members, each list item corresponding to a region in 
#'   Venn diagrams
#' @export
#' @examples
#' if(requireNamespace("Vennerable")) {
#'   myList <- list(A=LETTERS[1:5], B=LETTERS[2:7], C=LETTERS[seq(2,9,2)])
#'   myVenn <- Vennerable::Venn(myList)
#'   myVennList <- vennMembersList(myVenn)
#' }
vennMembersList <- function(venn, removeNULL=TRUE) {
    cc <- colnames(venn@IndicatorWeight)
    groups <- cc[1:(length(cc)-1)]

    members <- venn@IntersectionSets
    codes <- names(members)
    codes.split <- strsplit(codes, "")

    tbl <- c("1"="TRUE", "0"="FALSE")
    boolean.codes <- lapply(codes.split, function(x) tbl[x])
    names <- sapply(boolean.codes, function(x) paste(groups, x, sep=":", collapse="; "))
    names(members) <- names
    if(removeNULL) {
      members <- members[!sapply(members, is.null)]
    }
    return(members)
}

#' Extract members of each region in Venn diagrams in to a data.frame
#' @param venn A Venn object
#' @return A data.frame containing logical values of sets and elements
#' @export
#' @examples
#' if(requireNamespace("Vennerable")) {
#'   myList <- list(A=LETTERS[1:5], B=LETTERS[2:7], C=LETTERS[seq(2,9,2)])
#'   myVenn <- Vennerable::Venn(myList)
#'   myVennDf <- vennMembersDataframe(myVenn)
#'   print(myVennDf)
#' }
vennMembersDataframe <- function(venn) {
  cc <- colnames(venn@IndicatorWeight)
  groups <- cc[1:(length(cc)-1)]
  
  members <- venn@IntersectionSets
  codes <- names(members)
  codes.split <- strsplit(codes, "")
  
  tbl <- c("1"=TRUE, "0"=FALSE)
  boolean.codes <- lapply(codes.split, function(x) tbl[x])
  boolean.mat <- do.call(rbind, boolean.codes)
  boolean.matRepInd <- rep(1:nrow(boolean.mat), sapply(members, length))
  boolean.df <- data.frame(boolean.mat[boolean.matRepInd,])
  colnames(boolean.df) <- groups
  
  res <- cbind(boolean.df,
               element=unlist(members))
  rownames(res) <- NULL
  return(res)
}
