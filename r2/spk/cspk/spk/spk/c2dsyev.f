	subroutine C2DSYEV( C2JOBZ, C2UPLO, N, A, LDA, W, WORK, LWORK, INFO )
	integer C2JOBZ, C2UPLO, INFO, LDA, LWORK
	double precision A(LDA, *), W(*), WORK(*)
c
	character JOBZ
	character UPLO
	JOBZ = char(C2JOBZ)
	UPLO = char(C2UPLO)
	call DSYEV(JOBZ, UPLO, N, A, LDA, W, WORK, LWORK, INFO )
	return
	end

