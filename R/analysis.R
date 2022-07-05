#' Analysis of the biological research
#'
#' @param design Design is experimental technique you used in the experiment. rcbd -->  for randomized complete block  design , rcbd2 --> for 2 factorial  rcbd , rcbd3 --> for 3 factorial rcbd, crd -- > for complete randmized design crd2 --> for 2 factorial crd, crd3 --> for 3 factorial crd, lsd --> for latin square design ,split -- > for split plot design, strip --> for strip plot design
#' @param model 0 for no test 1 for LSD 2 for Duncan 3 for HSD test
#' @param data data frame name
#' @param rep replication factor. Block for split plot, if block is not avalable put 0
#' @param factA Put Treatment name incase of LSD , RCBD and CRD
#' @param factB Second factor Name otherwise put 0 if its absent
#' @param factC third factor Name otherwise put 0 if its absent
#'
#' @return
#' @export
#'
#
analysis <- function(design,model,data,rep, factA,factB,factC){
  library(doebioresearch)
  abc=data
  abcd = colnames(abc)
  d = design
  r=rep
  p=factA
  q=factB
  s=factC
  m= model
  print('Congratulation !!! You did it.')
  print('This package is created by BIBEK BUDHATHOKI with the intention to help biological researchers(specially those who conducts field experiment/lab experiment/trials). This package aims to help in analysis in the easiest way possible to date. ')
  print('For any issue related to this package or for neg/positive feedback feel free to mail me Bibek.budhathoki3@gmail.com')
  for (x in abcd){
    print('####################### This is analysis of of the variable ---> ')
    print(x)
    print('#######################')
    if (d == "rcbd"){
      print(rcbd(abc[x],p,r,m))
    }
    else if (d == 'rcbd2') {
      print(frbd2fact(abc[x],r,p,q,m))
    }
    else if (d == 'rcbd3') {
      print(frbd3fact(abc[x],r,p,q,s,m))
    }
    else if (d == 'crd2') {
      print(fcrd2fact(abc[x],p,q,m))
    }
    else if (d == 'crd3') {
      print(fcrd3fact(abc[x],p,q,s,m))
    }
    else if (d == 'crd') {
      print(crd(abc[x],p,m))
    }
    else if (d == 'lsd') {
      print(lsd(abc[x],p,q,s,m))
    }
    else if (d == 'split') {
      print(splitplot(abc[x],r,p,q,m))
    }
    else if (d == 'strip') {
      print(stripplot(abc[x],r,p,q,m))
    }
  }
}
