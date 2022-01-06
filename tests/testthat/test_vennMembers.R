library(ribiosPlot)
library(testthat)

context("test vennMembersList and vennMembersDataFrame")

# A: A B C D E
# B: B C D E F G
# C: B D F H
myList <- list(A=LETTERS[1:5], B=LETTERS[2:7], C=LETTERS[seq(2,9,2)])
myVenn <- Vennerable::Venn(myList)
myVennFullList <- vennMembersList(myVenn, removeNULL=FALSE)
myVennList <- vennMembersList(myVenn)

myVennDf <- vennMembersDataframe(myVenn)
myExpFullVennList <- list(`A:FALSE; B:FALSE; C:FALSE`=NULL,
                       `A:TRUE; B:FALSE; C:FALSE`="A",
                       `A:FALSE; B:TRUE; C:FALSE`="G",
                       `A:TRUE; B:TRUE; C:FALSE`=c("C", "E"),
                       `A:FALSE; B:FALSE; C:TRUE`="H",
                       `A:TRUE; B:FALSE; C:TRUE`=NULL,
                       `A:FALSE; B:TRUE; C:TRUE`="F",
                       `A:TRUE; B:TRUE; C:TRUE`=c("B", "D"))
myExpVennList <- list(`A:TRUE; B:FALSE; C:FALSE`="A",
                      `A:FALSE; B:TRUE; C:FALSE`="G",
                      `A:TRUE; B:TRUE; C:FALSE`=c("C", "E"),
                      `A:FALSE; B:FALSE; C:TRUE`="H",
                      `A:FALSE; B:TRUE; C:TRUE`="F",
                      `A:TRUE; B:TRUE; C:TRUE`=c("B", "D"))
myExpVennDf <- data.frame(A=c(T, F, T, T, F, F, T, T),
                          B=c(F, T, T, T, F, T, T, T),
                          C=c(F, F, F, F, T, T, T, T),
                          element=c("A", "G", "C", "E", "H", "F", "B", "D"))

test_that("vennMembersList works", {
  expect_identical(myVennFullList, myExpFullVennList)
  expect_identical(myVennList, myExpVennList)
})

test_that("vennMembersDataFrame works", {
  expect_identical(myVennDf, myExpVennDf)
})
