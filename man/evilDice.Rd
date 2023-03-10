\name{evilDice}
\alias{evilDice}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to Determine If a Set of Dice is Intransitive ~~
}
\description{
Intransitive dice sets are dice which display the unintuitive property that every die has a winning probability against a die and a losing probability against another die in the set.  This function tests the input set, returning the pairwise probabilities along with the expected returns (mean magnitude of win/loss).  In addition, the win probabilities for each die when the entire set is rolled as a group are provided.
}
\usage{
evilDice(thevals)
}

\arguments{
  \item{thevals}{
An N-row by M-column array, where each column contains the (numeric) values of the N faces of a given die. Note: if dice with unequal numbers of faces are desired, enter the \code{NA} value as needed to fill the array. 
}
}
\details{
Any tie (both dice show the same number) is treated as "not a win" and the value passed to the "expected return" calculation is zero. 
Since the win percentages are valid for any set of dice, this function can be used to determine the win rate for transitive dice. 
  
}
\value{

  \item{thevals }{Echoes back the input array. }
  \item{pairs }{A 2-column array. The first column contains the win percentage of the pair identified in the row name. The second column contains the expected return, i.e. the mean magnitude of the wins.  The upper rows present the "operational" pairing, i.e.  D1 --> D2 --> D3 ... Dn -->D1 .  Following that, all other possible pairings are given, e.g.  D1 --> D3 , D2 --> D4}
  \item{groupwins }{The set of winnng probabilities for all dice when rolled as a group. }
  \item{diceType }{A brief message stating whether the input values represent intransitive dice or not. }
%% ...
}

\author{
Carl Witthoft
Maintainer:Carl Witthoft carl@witthoft.com
}

\examples{

triDice <- cbind(c(2,2,4,4,9,9), c(1,1,6,6,8,8), c(3,3,5,5,7,7))
(evilDice(triDice))

# Bradley Efron computed this set. 
brad <- cbind(c(4, 4, 4, 4, 0, 0), c(3, 3, 3, 3, 3, 3), c(6, 6, 2, 2, 2, 2),c(5, 5, 5, 1, 1, 1))
(evilDice(brad)) 
}

 \keyword{ ~math }

