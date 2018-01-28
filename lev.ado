/*------------------------------------------------------------------------lev.ado
Calculates Levenshtein distance between 2 string variables

Stuart Craig
20180128
*/

// ------------------------------------------------------------ 1. Main call
cap program drop lev
program define lev
	syntax varlist (min=2 max=2 string), gen(string)

	tokenize `varlist'
	qui gen `gen'=.
	mata: word1=st_sdata(.,"`1'",1)
	mata: word2=st_sdata(.,"`2'",1)
	mata: ld=outvar(word1,word2)
	mata: st_store(.,"`gen'",ld[.,1])

end
// ------------------------------------------------------------ 2. Mata functions
capture mata: mata drop outvar()
capture mata: mata drop lev()
mata:
// Subroutine to calculate distance for each row
real matrix outvar(string matrix var1, string matrix var2) 
{
	outv=J(rows(var1),1,.)
	for (r=1;r<=rows(var1);r++){
		outv[r,1]=lev(var1[r,1],var2[r,1])
		// var1[r,1]
		// var2[r,1]
		// outv[r,1]
	}
	return(outv)
}
// Sub-subroutine to calculate the distance
real scalar lev(string scalar s1, string scalar s2) 
{
	ls1=strlen(s1)
	ls2=strlen(s2)
	// Get rid of prefixes (and suffixes) that match exactly 
	// (this will be everything when words are identical)
	// - Cuts computation by a lot
	sls=(ls1,ls2)
	msl=min(sls)
	for (i=1;i<=msl;i++) {
		if (substr(s1,1,1)==substr(s2,1,1)) {
			s1=substr(s1,2,.)
			s2=substr(s2,2,.)
		}
		else {
			break
		}
	}
	ls1=strlen(s1) // Now we have to recalculate strlen()
	ls2=strlen(s2)
	sls=(ls1,ls2)
	msl=min(sls)
	// Suffixes
	for (i=1;i<=msl;i++) {
		if (substr(s1,-1,1)==substr(s2,-1,1)) {
			s1=substr(s1,1,ls1-1)
			s2=substr(s2,1,ls2-1)
			ls1=ls1-1
			ls2=ls2-1
		}
		else {
			break
		}
	}
	A = J(ls2+1,ls1+1,.)
	for (i=0; i<=ls1; i++) {
		A[1,i+1]=i
	}
	for (j=0; j<=ls2; j++) {
		A[j+1,1]=j
	}
	for (j=1;j<=ls2;j++) {
		for (i=1;i<=ls1;i++) {
			if (substr(s1,i,1)==substr(s2,j,1)) cost=0
			else cost=1
			celv=(1+A[j,i+1],1+A[j+1,i],A[j,i]+cost)
			A[j+1,i+1]=min(celv)
		}
	}
	ldist=A[ls2+1,ls1+1]
	return(ldist)
}
end


exit


