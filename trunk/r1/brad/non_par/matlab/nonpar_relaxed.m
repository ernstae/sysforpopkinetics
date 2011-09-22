% $begin nonpar_relaxed$$ $newlinech %$$
% $spell
%       nonpar
%	eps
%	Jacobian
%	itr
%	Karush Kuhn Tucker
%	subproblem
%	mu
%	complementarity
%	Kuhn
%	Rockafellar
%	Fenchel
% $$
%
% $comment Latex macros local to this file$$
% $latex \newcommand{\R}{{\bf R}}$$
% $latex \newcommand{\T}{{\rm T}}$$
% $latex \newcommand{\D}{{\rm D}}$$
%
% $section Non-Parametric Relaxed Interior Point Problem$$
%
% $index nonpar_relaxed$$
% $index interior, point problem$$
% $index problem, interior point$$
% $index point, interior problem$$
%
% $table
% $bold Syntax$$ $cnext
% $syntax/[/lam/, /w/, /info/] = nonpar_relaxed(/Psi/, /t/, /eps/)/$$
% $tend
%
% $fend 15$$
%
% $head Primal Problem$$
% We are given a matrix $latex \Psi \in \R_+^{m \times n}$$ 
% (where $latex m \leq n$$) and a relaxation parameter $latex t$$.
% This routine solves the following convex programming problem:
% $latex \[
% \begin{array}{lll}
%  (P) & {\rm minimize} \; \phi_m ( \Psi \lambda )  + t \phi_n ( \lambda ) 
%      + m ( 1_n^T \lambda - 1 )
%      & {\rm w.r.t.} \;  \lambda \in \R_+^n
% \end{array}
% \] $$
% where $latex \phi_m : \R_+^m \rightarrow \R$$ is defined by
% $latex \[
% \phi_m ( z ) = \left\{ \begin{array}{ll}
%	- \log( z_1 ) - \log( z_2 ) -  \cdots - \log ( z_m ) 
%	& {\rm if} \; z > 0_m
% 	\\
%	+ \infty & {\rm otherwise}
% \end{array} \right.
% \] $$
% and $latex 0_m \in \R^m$$ ($latex 1_n \in \R^n$$) 
% is the vector with all its elements equal to zero (one).
% Note that at a minimizer of the primal problem
% $latex \[
%	\sum_{j=1}^n \lambda_j = 1 + t \frac{n}{m}
% \] $$ 
% (see $xref/nonpar_relaxed/Theory/Karush Kuhn Tucker Conditions/1/$$ below).
% Thus we can add this as an extra constraint with out changing the problem. 
%
% $head Dual Problem$$
% The following dual problem is derived in the 
% $xref/nonpar_relaxed/Theory/theory/$$ section below:
% $latex \[
% \begin{array}{lll}
%  (Q) & {\rm maximize} - \phi( w ) 
%      & {\rm w.r.t.} \;  w \in \R_+^m \; , \; y \in \R_+^n
% \\
%      & {\rm subject \, to} 
%      & \Psi^\T w  + y = m 1_n 
% \end{array}
% \] $$
%
% $head Psi$$
% The matrix $latex \Psi$$ such that
% $latex \Psi \in \R_+^{m \times n}$$,
% $latex m \leq n$$,
% and $latex \Psi 1_n > 0_m$$ (no row of $latex \Psi$$ is entirely zero).
%
% $head t$$
% The non-negative scalar $italic t$$ specifies the relaxation factor.
% It must be greater than or equal to zero.
%
% $head eps$$
% The scalar $italic eps$$
% must be greater than zero and specifies the
% convergence criteria in terms of the maximum value for
% $latex q$$ and $latex r$$ corresponding to the final iterate; i.e.,
% $syntax/
%	max( /q/ , /r/ ) <= /eps/ * max(max(/Psi/))
% /$$
% (see 
% $xref/nonpar_relaxed/info/q/q/1/$$ and 
% $xref/nonpar_relaxed/info/r/r/1/$$ below).
% A suggested value is $code 1e-12$$ times the maximum absolute element
% of the matrix $latex \Psi$$.
%
% $head lam$$
% The return vector $italic lam > 0$$ 
% is an approximate solution to the primal problem $latex (P)%$$.
%
% $head w$$
% The return vector $latex w > 0$$, together with
% $latex \[
%	y = m 1_n - \Psi^\T w
% \] $$
% is an approximate solution for the dual problem $latex (Q)%$$
% (we also have $latex y > 0$$).
%
% $head info$$
% The result $italic info$$ has one row for each iteration of the
% algorithm. 
%
% $subhead mu$$
% The result $syntax//info/(/k/, 1)/$$ is 
% (see $xref/nonpar_relaxed/Theory/Newton Step/Newton step/1/$$ below)
% $latex \[
%       \mu^k = info(k, 1)
% \] $$ 
%
% $subhead q$$
% The result $syntax//info/(/k/, 2)/$$ is 
% $latex \[
%       q^k = info(k, 2) =  \max_{i = 1 , \cdots , n}
%	\left| \lambda_i^k y_i^k - t \right|
% \] $$ 
% where $latex w^k$$ and $latex \lambda^k$$ are the $th k$$
% approximations for the corresponding output vectors and
% $latex y^k = m 1_n - \Psi^\T w^k$$.
%
% $subhead r$$
% The result $syntax//info/(/k/, 3)/$$ is 
% $latex \[
%	r^k = info(k, 3) = \max_{i = 1 , \cdots , m}
%	\left| [ \D ( w^k ) \Psi \lambda^k ]_i -  1 \right| 
% \] $$ 
% where $latex w^k$$ and $latex \lambda^k$$ are the $th k$$
% approximations for the corresponding output vectors.
%
% $children|
%	nonpar_relaxed_ok.m
% |$$
% $head Example$$
% The routine $code nonpar_relaxed_ok$$ 
% (in the file $xref/nonpar_relaxed_ok.m/$$
% is an example and test of $code nonpar_relaxed$$.
% It returns true for success and false for failure.
% 
% $head Theory$$
%
% $subhead Extended Primal Problem$$
% The $xref/nonpar_relaxed/Primal Problem/primal problem/$$ is equivalent
% to the problem
% $latex \[
% \begin{array}{llll}
%  (P) & {\rm minimize} 
%      & \phi_m ( z )  + t \phi_n ( \lambda ) 
%      + m ( 1_n^T \lambda - 1 )
%      & {\rm w.r.t.} \;  z \in \R_+^m \; , \; \lambda \in \R_+^n
%  \\
%      & {\rm subject \; to} 
%      & z \leq \Psi \lambda
% \end{array}
% \] $$
%
% $subhead Duality$$
% The Lagrangian corresponding to the problem above 
% $latex L : \R_+^m \times \R_+^m \times \R_+^n \rightarrow \R$$ 
% is defined by
% $latex \[
% L( w , z , \lambda ) = \left\{ \begin{array}{ll}
% w^\T ( z - \Psi \lambda ) + \phi_m ( z )  + \phi_m ( z ) 
% + t \phi_n ( \lambda ) + m ( 1_n^T \lambda - 1 )
%	& {\rm if} \; (w, z, \lambda) > 0_{m + m + n}
% \\
% + \infty & {\rm if \, for \, some \,} i \;, \; z_i = 0
% \\
% + \infty & {\rm if \, for \, some \,} j \;, \; \lambda_j = 0
% \\   
% - \infty & {\rm if \; for \; some \;} i \;, w_i = 0
% \end{array} \right.
% \] $$
% Note that the primal problem above is equivalent to minimizing
% the following function of $latex (z , \lambda)$$:
% $latex \[
% P(z , \lambda ) = \sup \; L(w , z , \lambda ) 
%	\; {\rm .w.r.t.} \; \; w \in \R_+^m
% \] $$
% The Fenchel-Rockafellar dual problem is to maximize
% $latex \[
% Q(w) = \inf \; L(w , z , \lambda ) 
%	\; {\rm .w.r.t.} \; z \in \R_+^m \; , \; \lambda \in \R_+^n
% \] $$
% we regroup the terms in the Lagrangian as
% $latex \[
% L(w , z , \lambda ) = \left\{ \begin{array}{ll}
% \phi_m (z) + z^\T w 
% + t \phi_n ( \lambda ) + \lambda^\T ( m 1_n - \Psi^\T w ) - m
%	& {\rm if} \; w > 0_m \; z > 0_m 
% \\
% + \infty & {\rm if \, for \, some \,} i \;, \; z_i = 0
% \\
% + \infty & {\rm if \, for \, some \,} j \;, \; \lambda_j = 0
% \\   
% - \infty & {\rm if \; for \; some \;} i \;, w_i = 0
% \end{array} \right.
% \] $$
% For dual feasible points $latex w$$, 
% the minimizer of the Lagrangian with respect to $latex z_i$$ is
% $latex w_i^{-1}$$ and the minimizer with respect to $latex \lambda_j$$ is
% $latex t ( m - \Psi_j^T w )^{-1}$$ (where $latex \Psi_j$$ is the $latex j$$
% column of $latex \Psi$$).
% Using this, the dual problem is
% $latex \[
% \begin{array}{lll}
% {\rm maximize}
%  & - \phi_m ( w ) 
%  + n \log ( t ) -  t \phi_n ( m 1_n - \Psi^T w )
%  + t m
%  & {\rm w.r.t.} \;  w \in \R_+^m 
% \\
% {\rm subject \; to}
%  & \Psi^\T w \leq m 1_n
% \end{array}
% \] $$
% Using $latex y \in \R_+^n$$ as the slack variables in problem above 
% can be written as
% $latex \[
% \begin{array}{lll}
%  (Q) & {\rm maximize} - \phi_m ( w )  - t \phi_n ( y ) + n \log ( t ) + t m
%      & {\rm w.r.t.} \;  w \in \R_+^m \; , \; y \in \R_+^n
% \\
%      & {\rm subject \, to} 
%      & \Psi^\T w  + y = m 1_n 
% \end{array}
% \] $$
% 
% $subhead Karush Kuhn Tucker Conditions$$
% The following function is a Lagrangian for $latex (Q)$$: 
% $latex \[
%	- \phi_m ( w )  - t \phi_n ( y ) + n \log ( t ) + t m
%	- \lambda^T (  \Psi^\T w  + y -  m 1_n )	
% \] $$
% The corresponding KKT conditions are
% $latex \[
% \begin{array}{rcl}
%	\Psi^\T w + y          & = & m 1_n \\ 
%	\D (w) \Psi \lambda    & = & 1_m   \\
%	\D ( \lambda ) y       & = & t 1_n 
% \end{array}
% \] $$
% where for a vector $latex v$$, $latex \D (v)$$ is the diagonal
% matrix with $latex v$$ along the diagonal.
% It follows from these conditions that
% $latex \[
% \begin{array}{rcl}
%	\lambda^T \Psi^\T w + \lambda^T y & = & m \lambda^T 1_n \\ 
%	1_m^T \D (w) \Psi \lambda         & = & 1_m^T 1_m       \\
%       m                   + t n         & = & m \lambda^T 1_n \\
%	\sum_{j=1}^n \lambda_j            & = & 1 + t \frac{n}{m}
% \end{array}
% \] $$
%
% $subhead Relaxed Subproblem$$
% During the calculations, we use the following change of variables:
% $latex w = w / m$$, $latex y = y / m$$, and $latex \lambda = m \lambda$$.
% Using this change of variables, and further relaxing the 
% complementarity condition using the parameter $latex \mu > 0$$,
% the relaxed Kuhn-Tucker equations become: 
% $latex \[
% F_\mu ( \lambda , w , y ) 
% = 
% \left( \begin{array}{c}
%	\Psi^\T w + y         \\ 
%	\D (w) \Psi \lambda   \\
%	\D ( \lambda ) y
% \end{array} \right)
% =
% \left( \begin{array}{c}
%	1_n   \\ 
%	1_m   \\
%	\mu 1_n 
% \end{array} \right)
% \] $$
% where the first equality is a definition of the function 
% $latex \[
%	F_\mu : \R^n \times \R^m \times \R^n 
%	\rightarrow 
%	\R^n \times \R^m \times \R^n 
% \] $$
% The Jacobian of $latex F_\mu$$ is
% $latex \[
% F_\mu^{(1)} ( \lambda , w , y ) 
% = 
% \left( \begin{array}{ccc}
%	0_{n,n}     & \Psi^\T            & \D( 1_n )      \\
%	\D (w) \Psi & \D( \Psi \lambda ) & 0_{m,n}        \\
%	\D (y)      & 0_{n,m}            & \D( \lambda ) 
% \end{array} \right)
% \] $$
% where $latex 0_{n,m} \in \R^{n \times m}$$ is the matrix
% with all its elements equal to zero.
% Note that $latex \D( 1_n )$$ is the $latex n \times n$$ 
% identity matrix.
%
% $subhead Newton Step$$
% At the $th k$$ iteration of the algorithm we are given
% $latex \mu^k \in \R$$,
% $latex \lambda^k \in \R^n$$, and
% $latex w^k \in \R^m$$
% where $latex ( \lambda^k , w^k , y^k )$$ is an approximate solution of the
% $xref/nonpar_relaxed/Theory/Relaxed Subproblem/relaxed subproblem/1/$$.
% with $latex \mu = \mu^k$$.
% The Newton Step at the $th k$$ iteration is the value of
% $latex ( \Delta \lambda^k , \Delta w^k , \Delta y^k )$$  that solves 
% the linear equation
% $latex \[
%	F_k ( \lambda^k , w^k , y^k ) + 
%	F_k ^{(1)} ( \lambda^k , w^k , y^k ) 
%	\left( \begin{array}{c}
%		\Delta \lambda^k \\
%		\Delta w^k       \\
%		\Delta y^k
%	\end{array} \right) 
%	=
%	\left( \begin{array}{c}
%		1_n   \\ 
%		1_m   \\
%		\mu^k 1_n 
% 	\end{array} \right)
% \] $$
% (Note that we have used the short hand $latex F_k$$ for 
% $latex F$$ with the subscript $latex \mu^k$$.)
% Moving $latex  F_k ( \lambda^k , w^k , y^k )$$ to the right hand side,
% using the equations above for $latex F_k$$ and $latex F_k^{(1)}$$,
% and restricting our algorithm to $latex y^k$$ that satisfy
% $latex \[
%	y^k = 1_n - \Psi^\T w^k
% \] $$
% we obtain the following linear equation for
% $latex ( \Delta \lambda^k , \Delta w^k , \Delta y^k )$$  
% $latex \[
% \left( \begin{array}{ccc}
%       0_{n,n}     & \Psi^\T            & \D( 1_n )      \\
%       \D (w) \Psi & \D( \Psi \lambda ) & 0_{m,n}        \\
%       \D (y)      & 0_{n,m}            & \D( \lambda )
% \end{array} \right)
% \left( \begin{array}{c}
%	\Delta \lambda \\
%	\Delta w       \\
%	\Delta y
% \end{array} \right) 
% =
% \left( \begin{array}{c}
%	0                           \\ 
%	1_m - \D (w) \Psi \lambda   \\
%	\mu 1_n - \D ( \lambda ) y
% \end{array} \right)
% \] $$
% where the superscript $latex k$$ has been dropped from the equation.
% Given the value of $latex \Delta w$$, 
% we use the first row above to solve for $latex \Delta y$$.
% Given the value of $latex \Delta w$$ and $latex \Delta y$$,
% we use the third row above to solve for $latex \Delta \lambda$$.
% Subtracting $latex \D( \lambda )$$ times the first row from the 
% third row, we obtain the following reduced set of equations:
% $latex \[
% \left( \begin{array}{cc}
%       \D (w) \Psi & \D( \Psi \lambda )                \\
%       \D (y)      & - \D( \lambda ) \Psi^\T
% \end{array} \right)
% \left( \begin{array}{c}
%	\Delta \lambda \\
%	\Delta w
% \end{array} \right) 
% =
% \left( \begin{array}{c}
%	1_m - \D (w) \Psi \lambda   \\
%	\mu 1_n - \D ( \lambda ) y
% \end{array} \right)
% \] $$
% Given two vectors $latex u \in \R^n$$, $latex v \in \R^n$$,
% where none of the components of $latex v$$ are zero,
% we define the vector $latex u / v \in \R^n$$ by
% $latex (u / v)_i = u_i / v_i$$.
% We also define the vector $latex v^{-1} \in \R^n$$ by
% $latex v_i^{-1} = 1 / v_i$$.
% Subtracting $latex \Psi \D (y)^{-1}$$ times the second row
% from $latex \D (w)^{-1}$$ times the first row, 
% we obtain the following reduced equation
% $latex \[
% \begin{array}{rcl}
% [ \D (w)^{-1} \D( \Psi \lambda ) + \Psi \D( \lambda / y ) \Psi^T ] 
% \Delta \lambda
% & = & 
% w^{-1} - \Psi \lambda 
% - \mu \Psi y^{-1}
% + \Psi \D ( \lambda / y ) y 
% \\
% & = & 
% w^{-1}  - \mu \Psi y^{-1} 
% \end{array}
% \] $$
% $end
% ---------------------------------------------------------------------------
function [lam, w, info] = nonpar_relaxed(Psi, t, eps)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%GENERAL DESCRIPTION OF SOLUTION METHOD%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The path-following algorithm applies Newton's method to the 
% perturbed system of nonlinear equations above; i.e.,
%
%             Psi' * w + y     =  1_n            (1)
%            w .* (Psi * lam)  =  1_m            (2)
%            lam .* y          =  mu * 1_n       (3)
%
% where mu is decreased to the value t. The Newton step
% is damped so that the iterates remain strictly positive.
% In addition, the path-following parameter mu is reduced
% at each iteration in a manner that attempts to decrease 
% its value at a rate that is approximately the same as the 
% rate of decrease in the error in the nonlinear equation 
%            w .* (Psi * lam)  =  1_m
% Specifically, we try to reduce mu and     
%      r^k = mean( abs( w .* (Psi * lam) - 1_m ) )
% at approximately the same rate.
%
% --------------------------------------------------
% Some constants
% --------------------------------------------------
% Problem dimensions are determined by the argument Psi
[m, n] = size(Psi);

% vectors of ones with length n and m
one_n  =  ones(n, 1);
one_m  =  ones(m, 1);

% maximum absolute element of Psi
Psi_max = max( max( Psi ) );

% scaled convergence criteria
eps_scaled = eps * Psi_max;

% minimum allowable value for mu
mu_min     = max(t, eps_scaled / 10);

% --------------------------------------------------
% Check input
% --------------------------------------------------
if min( min(Psi) ) < 0
   error('The matrix Psi has a negative element.')
end   
if min( Psi * one_n ) <= 1.e-13 * Psi_max 
   error('The matrix Psi has a row that is entirely near zero.')
end   
% --------------------------------------------------
% Initialization
% --------------------------------------------------

% first approximate solution for the primal problem
lam      = one_n;

% a matrix product
Psi_lam  = Psi * lam;

% initialize w so that equation (2) is satisfied; i.e.,
%            w .* (Psi * lam)  =  one_m            (2)
w        = one_m ./ Psi_lam; 

% a matrix product
PsiT_w   = Psi' * w;

% Multiply lam and divide w by factor. 
% Note that after this operation, equation (2) is still satisfied. 
factor   = 2 * max( PsiT_w );
lam      = lam * factor;
w        = w   / factor; 

% update matrix products
Psi_lam  = Psi_lam * factor;
PsiT_w   = PsiT_w  / factor;

% Note that with the current value of w, max( PsiT_w ) = 1 / 2.
% We initialze y so that equation (1) is satisfied; i.e.,
%             Psi' * w + y  =  one_n               (1)
% In fact, equation (1) will be satisfied through out the computation.
y        = one_n - PsiT_w;

% The initial value for for this residual in equaiton (2) is zero
%            w .* (Psi * lam)  =  one_m            (2)
r        = abs(one_m - w .* Psi_lam);

% initial residual corresponding to equation (3) (with mu = t)
%            lam .* y          =  mu * one_n       (3)
q        = abs( lam .* y - t );

% initial mu 
res_old  = (mean(q) + mean(r)) / 2;
mu       = max(res_old, mu_min);

% Initialize option output values
info = [ mu , max(q), max(r) ];

% -------------------------------------------------------------
% Iterate until convergence
% -------------------------------------------------------------

while ( max(q) > eps_scaled | max(r) > eps_scaled )
	mu_y  = mu ./ y;
	lam_y = lam ./ y;

	% use coef_dw to denote the coefficient matrix for dw = Delta w 
	% D(w)^{-1} * D(Psi * lam) + Psi * D( lam / y ) * Psi^T
	coef_dw = diag( Psi_lam ./ w ) +  Psi * diag( lam_y ) * Psi';

	% Compute the Cholesky factorization R so that R' * R = coef_dw 
	% and check that this computation was successful.
	[R, p] =  chol(coef_dw);
	if p 
		msg = 'equations for Delta lambda are numerically singular'
		error(['nonpar_relaxed: ', msg])
	end

	% We use rhs_dw for the right hand side of the equation for dw
	% coef_dw * dw = rhs_dw
	rhs_dw = one_m ./ w - Psi * mu_y;

	% compute the Newton step value for dw 
	dw     =  R \ (R' \ rhs_dw);

	% check the residual in evaluation of dw 
	% dw_err = max( abs( coef_dw * dw - rhs_dw ) )
	% if dw_err > eps
	%	msg = 'cannot achieve accuracy specified by eps'
	%	error(['nonpar_relaxed: ', msg])
	% end

	% compute the Newton step value for dy 
	dy     = - Psi' * dw;

	% compute the Newton step value for dlam 
	dlam   =  mu_y - lam - lam_y .* dy;

	% compute Newton step damping factors that keep all of the
	% iterates strictly positive; i.e. w > 0, lam > 0, y > 0.
	near_one = .99995;
	alp_pri  = 1;
	term = max( - dlam ./ lam );
	if term > near_one
		alp_pri = min(alp_pri , near_one / term );
	end 
	alp_dual = 1;
	term = max( - dy ./ y );
	if term > near_one
		alp_dual = min(alp_dual , near_one / term );
	end 
	term = max( - dw ./ w );
	if term > near_one
		alp_dual = min(alp_dual , near_one / term );
	end 

	% compute the primal and dual iterates
	alp = min( alp_pri, alp_dual );
	lam = lam + alp * dlam;
	w   = w   + alp * dw;

	% compute the corresponding value of y
	% (up to roundoff, y = y + dy gives same answer and is less work)
	PsiT_w  = Psi' * w;
	y       = one_n - PsiT_w;

	% compute convergence criteria at new iterates
	q       = abs( lam .* y - t );
	Psi_lam =  Psi * lam;
	r       = abs(one_m - w .* Psi_lam);

	% determine the new value for mu
	res_avg = (mean(q) + mean(r)) / 2;
	if max(q) >= eps
		factor  = (res_avg / res_old)^3; 
		factor  = min(factor, .3);
		mu      = factor * mu;
		mu      = max(mu, mu_min);
	end
	res_old = res_avg;

	% Update convergence criteria return vectors
	info = [ info ; [ mu, max(q) , max(r) ] ];

end
% convert back to original scaling
lam = lam / m;        
w   = w * m;

return
