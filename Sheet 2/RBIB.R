library(Biostrings)

getmat = function(){
  mat = matrix(-1, nrow = 26, ncol = 26, dimnames = list(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z"), 
                     c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")))
  
  diag(mat) = 1
  #mat = nucleotideSubstitutionMatrix(match = 1, mismatch = -1, baseOnly = FALSE, type = "DNA")
  return(mat)
}

align = function(s, t, mat, tp = "global"){ 

Align = pairwiseAlignment(s, t, type = tp, substitutionMatrix = mat, gapOpening = 0,
                           gapExtension =- 2, scoreOnly = FALSE)

print(Align)
paste("Score:", score(Align), ", Matches: ", nmatch(Align), ", Missmatch: ", nmismatch(Align))

}

#2.4
blosum = function(s, t, tp = "global"){

blos <- pairwiseAlignment(s, t, type = tp, substitutionMatrix = "BLOSUM62", gapOpening = 0, gapExtension = -2)
print(blos)
paste("Score:", score(blos), ", Matches: ", nmatch(blos), ", Missmatch: ", nmismatch(blos))

}