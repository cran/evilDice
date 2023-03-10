# calculate Expected Return for intransitive dice
  		# Die A has sides 2, 2, 4, 4, 9, 9.
		# Die B has sides 1, 1, 6, 6, 8, 8.
		# Die C has sides 3, 3, 5, 5, 7, 7.
		
# another one: 
# A:2, 2, 6, 6, 7, 7
# B: 1, 1, 5, 5, 9, 9
# C: 3, 3, 4, 4, 8, 8	

# those have same pair-win probabilities, but the two sets, if rolled all 3 at a time, 
# show different probabilities for a given die to beat both others.  

	
# Bradley Efron set of four (six-sided) dice
		# A: 4, 4, 4, 4, 0, 0
		# B: 3, 3, 3, 3, 3, 3
		# C: 6, 6, 2, 2, 2, 2
		# D: 5, 5, 5, 1, 1, 1
		
# user enters array, 1 column per die.  function returns winning percentages and expected returns
# there are (see Miwin) dice sets with a value in common, so set those as "not win" and return of   zero
#For dice w/ dif't numbers of faces, enter NA for "short" dice
		
#BUG:  not comparing all possible pairs for >3 dice.  Fix that and append after the "standard"
#  a--b--c--d--... --a  pairings. These 'extras' must be excluded from the is/isn't determiner
#  build 'extras' still in top-down order, e.g. A--C, A--D, B--D.  Fun indexing time!
evilDice <- function(thevals) {
	numdice <- ncol(thevals)
# TODo:  Make sure some absurdly large number of dice don't get dupes,....
	dnam <- rep(LETTERS, length = numdice)
	# columns: pair-win probability, pair winning returns, 
	outs <- array(dim = c(numdice,  2 ))
	colnames(outs) <- c("WinRate" , 'ExpReturn')
	#placeholder:
	rownames(outs) <- rep('no',length=numdice)
	for (jrow in 1:numdice) {
		rownames(outs)[jrow] <- paste0(dnam[jrow], 'vs', dnam[ jrow %%numdice + 1])
		# get winning pctge
		wartmp <- expand.grid(thevals[ , jrow], thevals[ , jrow%%numdice + 1])
		deltmp <-  wartmp[,1] - wartmp[,2] 
		deltmp <- deltmp[!is.na(deltmp)]
		outs[jrow,1] <- sum(deltmp > 0) /length(deltmp)  #nrow(wartmp)
		# get winning 'margin'  
		outs[jrow, 2] <-  mean(deltmp)
	}
# make it obvious to the user
	if (min(outs[,1]) <= 0.5) {
		diceType = 'Not Intransitive'
	}	else {
		diceType = 'Intransitive'
	}
# section to get other I vs J wins. "Fun With Indices" 
	if(numdice > 3) {
		for(jfrist in 1:(numdice-2)){
			for(jsec in (jfrist + 2): numdice){
#expand the size of outs matrix. 
				outs <- rbind(outs, c(0,0))
				botrow <- nrow(outs)
				rownames(outs)[botrow] <- paste0(dnam[jfrist], 'vs', dnam[ jsec])
				# get winning pctge
				wartmp <- expand.grid(thevals[,jfrist], thevals[, jsec])
				deltmp <-  wartmp[,1] - wartmp[,2] 
				deltmp <- deltmp[!is.na(deltmp)]
				outs[botrow,1] <- sum(deltmp > 0) /length(deltmp)  #nrow(wartmp)
				# get winning 'margin'  
				outs[botrow, 2] <-  mean(deltmp)
			}
		}
	}
	#now get results when rolling the bunch - roll all, win prob for each die
	bartmp <- list()
	for ( jj in 1: ncol(thevals)) bartmp[[jj]] <- thevals[,jj]	
	wartmp <- expand.grid(bartmp)  
	# remove any row with NA here, carefully
	wartmp <- wartmp[!is.na(rowSums(wartmp)),]
	# this fast code from onyambu   https://stackoverflow.com/questions/75463792  
	#subset(foo, max.col(foo, 'first') == max.col(foo, 'last'))
	# And now can just calc winners after reducing  wartmp
	wartmp <- subset(wartmp, max.col(wartmp,'first')  == max.col(wartmp,'last'))
	winners <- max.col(wartmp)
	fooh <- hist(winners, plot=F, breaks = 0:length(unique(wartmp)) +.5)$density
	# make it label-able
	fooh <- matrix(fooh,1,length(fooh))
	colnames(fooh) <- dnam
	colnames(thevals) <- dnam
	return(invisible(list(thevals = thevals, pairs = outs, groupwins = fooh, diceType=diceType)))
}
