function [lam, w, q_itr, r_itr] = npipp_of(Psi, eps)
% $begin npipp_of$$ $newlinech %$$
% $spell
%	eps
%	Jacobian
%	itr
%	Karush Kuhn Tucker
%	subproblem
%	mu
%	npipp
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
% $section Non-Parametric Interior Point Problem$$
%
% $index npipp_of$$
% $index interior, point problem$$
% $index problem, interior point$$
% $index point, interior problem$$
%
% $table
% $bold Syntax$$ $cnext
% $syntax/[/lam/, /w/, /q_itr/, /r_itr/] = npipp_of(/Psi/, /eps/)/$$
% $tend
%
% $fend 15$$
%
% $head Primal Problem$$
% We are given a matrix $latex \Psi \in \R_+^{m \times n}$$ 
% where $latex m \leq n$$.
% This routine solves the following convex programming problem:
% $latex \[
% \begin{array}{lll}
%  (P) & {\rm minimize} \phi( z ) 
%      & {\rm w.r.t.} \;  z \in \R_+^m \; , \; \lambda \in \R^n
% \\
%      & {\rm subject \, to} 
%      & z \leq \Psi \lambda \; , \; \lambda \in \Delta
% \end{array}
% \] $$
% where $latex \phi : \R_+^m \rightarrow \R$$ is defined by
% $latex \[
% \phi( z ) = \left\{ \begin{array}{ll}
%	- \log( z_1 ) - \log( z_2 ) -  \cdots - \log ( z_m ) 
%	& {\rm if} \; z > 0_m
% 	\\
%	+ \infty & {\rm otherwise}
% \end{array} \right.
% \] $$
% $latex 0_m \in \R^m$$ is the vector with all its elements equal to zero
% and $latex \Delta$$ is the standard simplex in $latex \R^n$$; i.e.,
% the set of $latex \lambda \in \R_+^n$$ such that 
% $latex 1_n^\T \lambda \leq 1$$
% (where $latex 1_n \in \R^n$$ is the vector with all its
% elements equal to one). 
%
% $head Dual Problem$$
% The following dual problem is derived the 
% $xref/npipp_of/Theory/theory/$$ section below:
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
% $head eps$$
% The scalar $italic eps$$
% must be greater than zero and specifies the
% convergence criteria in terms of the maximum value for
% $latex q$$ and $latex r$$ corresponding to the final iterate
% (see 
% $xref/npipp_of/q_itr/q_itr/$$ and 
% $xref/npipp_of/r_itr/r_itr/$$ below).
% A suggested value is $code 1e-12$$.
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
% $head q_itr$$
% The output $italic q_itr$$ is a vector and
% the element $syntax%%q_itr%(%k%)%$$ is the value of
% $latex \[
%	q^k = \frac{1}{n} \sum_{i=1}^n \lambda_i^k y_i^k
% \] $$ 
% where $latex w^k$$ and $latex \lambda^k$$ are the $th k$$
% approximations for the corresponding output vectors and
% $latex y^k = m 1_n - \Psi^\T w^k$$.
%
% $head r_itr$$
% The output $italic r_itr$$ is a vector and
% the element $syntax%%r_itr%(%k%)%$$ is the value of
% $latex \[
%	r^k = \max_{i = 1 , \cdots , m}
%	\left| [ \D ( w^k ) \Psi \lambda^k ]_i -  1 \right| 
% \] $$ 
% where $latex w^k$$ and $latex \lambda^k$$ are the $th k$$
% approximations for the corresponding output vectors.
%
% $children|
%	npipp_ok.m
% |$$
% $head Example$$
% The routine $code npipp_ok$$ (int he file $xref/npipp_ok.m/$$
% is an example and test of $code npipp_of$$.
% It returns true for success and false for failure.
% 
% $head Theory$$
%
% $subhead Duality$$
% The Lagrangian corresponding to the problem above 
% $latex L : \R_+ \times \R_+^m \times \R_+^m \times \R_+^n \rightarrow \R$$ 
% is defined by
% $latex \[
% L( \gamma , w , z , \lambda ) = \left\{ \begin{array}{ll}
% \gamma ( 1_n^\T \lambda - 1 ) + w^\T ( z - \Psi \lambda ) + \phi (z)  
%	& {\rm if} \; w > 0_m \; z > 0_m 
% \\
% + \infty & {\rm if \, for \, some \,} i \;, \; z_i = 0
% \\   
% - \infty & {\rm if \; for \; some \;} i \;, w_i = 0
% \end{array} \right.
% \] $$
% Note that the primal problem above is equivalent to minimizing
% the following function of $latex (z , \lambda)$$:
% $latex \[
% P(z , \lambda ) = \sup \; L( \gamma , w , z , \lambda ) 
%	\; {\rm .w.r.t.} \; \gamma \in \R_+ \; , \; w \in \R_+^m
% \] $$
% The Fenchel-Rockafellar dual problem is to maximize
% $latex \[
% Q(\gamma , w) = \inf \; L( \gamma , w , z , \lambda ) 
%	\; {\rm .w.r.t.} \; z \in \R_+^m \; , \; \lambda \in \R_+^n
% \] $$
% we regroup the terms in the Lagrangian as
% $latex \[
% L( \gamma , w , z , \lambda ) = \left\{ \begin{array}{ll}
% \phi(z) + z^\T w + \lambda^\T ( \gamma 1_n - \Psi^\T w ) - \gamma
%	& {\rm if} \; w > 0_m \; z > 0_m 
% \\
% + \infty & {\rm if \, for \, some \,} i \;, \; z_i = 0
% \\   
% - \infty & {\rm if \; for \; some \;} i \;, w_i = 0
% \end{array} \right.
% \] $$
% from which we can read off the dual problem
% $latex \[
% \begin{array}{lll}
%  & {\rm maximize} - \phi( w ) + m - \gamma 
%  & {\rm w.r.t.} \;  w \in \R_+^m \; , \; \gamma \in \R_+
% \\
%  & {\rm subject \, to} 
%  & \Psi^\T w \leq \gamma 1_n
% \end{array}
% \] $$
% Substituting $latex \gamma = t \gamma$$, $latex w = t w$$
% the corresponding objective for the dual problem is
% $latex \[
%	m - t \gamma + \sum_{i=1}^m \log( t w _i )
% \] $$
% Maximizing this objective
% with respect to $latex t \in \R_+$$ we determine that the optimal
% value for $latex t \gamma$$ is $latex m$$ and the dual problem becomes
% $latex \[
% \begin{array}{lll}
%  & {\rm maximize} - \phi( w ) 
%  & {\rm w.r.t.} \;  w \in \R_+^m 
% \\
%  & {\rm subject \, to} 
%  & \Psi^\T w \leq m 1_n
% \end{array}
% \] $$
% Using $latex y \in \R_+^n$$ as the slack variables in problem above 
% can be written as
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
% $subhead Karush Kuhn Tucker Conditions$$
% The KKT conditions for this convex program are 
% $latex \[
% \begin{array}{rcl}
%	\Psi^\T w + y          & = & m 1_n \\ 
%	\D (w) \Psi \lambda    & = & 1_m   \\
%	\D ( \lambda ) y       & = & 0_n 
% \end{array}
% \] $$
% where for a vector $latex v$$, $latex \D (v)$$ is the diagonal
% matrix with $latex v$$ along the diagonal.
%
% $subhead Relaxed Subproblem$$
% During the calculations, we use the following change of variables:
% $latex w = w / m$$, $latex y = y / m$$, and $latex \lambda = m \lambda$$.
% Using this change of variables, and relaxing the complementarity condition
% by $latex \mu > 0$$ the relaxed Kuhn-Tucker equations become: 
% $latex \[
% F ( \lambda , w , y ) 
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
%	F : \R^n \times \R^m \times \R^n 
%	\rightarrow 
%	\R^n \times \R^m \times \R^n 
% \] $$
% The Jacobian of $latex F$$ is
% $latex \[
% F^{(1)} ( \lambda , w , y ) 
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
% $subhead Gap$$
% The duality gap between $latex (P)$$ and $latex (Q)$$ is
% $latex \[
% \begin{array}{rcl}
%	g( \lambda , w ) & = & \phi( \Psi \lambda ) + \phi ( w )
%	\\
%	g( m \lambda , w / m ) 
%	& = & \phi( \Psi \lambda ) - m \log(m) + \phi( w ) + m \log(m)
%	\\ 
%	& = & g( \lambda , w )
% \end{array}
% \] $$
% Thus, the change of variables:
% $latex w = w / m$$, $latex y = y / m$$, and $latex \lambda = m \lambda$$
% does not affect the value of the duality gap.
% If $latex \lambda$$ is feasible for $latex (P)$$ and $latex w$$
% is feasible for $latex (Q)$$, $latex g( \lambda , w ) \geq 0$$.
% Furthermore, with the feasibility assumption,
% $latex g( \lambda , w ) = 0$$ if and only if
% $latex \lambda$$ solves $latex (P)$$ and $latex w$$ solves $latex (Q)$$.
% We note that if $latex \D ( w ) \Psi \lambda = 1_n $$,
% it follows that for $latex i = 1 , \ldots , m$$
% $latex \[
% \begin{array}{rcl}
%	0 & = & \log[ ( \Psi \lambda )_i ]  + \log ( w_i )
%	\\
%	0 & = & \sum_{i=1}^m \log[ ( \Psi \lambda )_i ]  + \log ( w_i )
%	\\
%	& = & g( \lambda , w )
% \end{array}
% \] $$
% i.e., the corresponding duality gap is zero.
%
% $subhead Newton Step$$
% At the $th k$$ iteration of the algorithm we are given
% $latex \mu^k \in \R$$,
% $latex \lambda^k \in \R^n$$, and
% $latex w^k \in \R^m$$
% where $latex ( \lambda^k , w^k , y^k )$$ is an approximate solution of the
% $xref/npipp_of/Theory/Relaxed Subproblem/relaxed subproblem/1/$$.
% with $latex \mu = \mu^k$$.
% The Newton Step at the $th k$$ iteration is the value of
% $latex ( \Delta \lambda^k , \Delta w^k , \Delta y^k )$$  that solves 
% the linear equation
% $latex \[
%	F( \lambda^k , w^k , y^k ) + 
%	F^{(1)} ( \lambda^k , w^k , y^k ) 
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
% Moving $latex  F( \lambda^k , w^k , y^k )$$ to the right hand side,
% using the equations above for $latex F$$ and $latex F^{(1)}$$,
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
% where mu is decreased to the value zero. The Newton step
% is damped so that the iterates remain strictly positive.
% In addition, the path-following parameter mu is reduced
% at each iteration in a manner that attempts to decrease 
% its value at a rate that is approximately the same as the 
% rate of decrease in the error in the nonlinear equation 
%            w .* (Psi * lam)  =  1_m
% Specifically, we try to reduce mu and     
%      max( w .* (Psi * lam) - 1_m )
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

% --------------------------------------------------
% Check input
% --------------------------------------------------
if min( min(Psi) ) < 0
   error('The matrix Psi has a negative element.')
end   
if min( Psi * one_n ) <= 1.e-12 * max( max( Psi ) )
   error('The matrix Psi has a row that is entirely zeros.')
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
r        = max( abs(one_m - w .* Psi_lam) );

% initial residaul corresponding to equation (3) (with mu = 0)
%            lam .* y          =  mu * one_n       (3)
q        =  (lam' * y) / n;

% initial reduction parameter for mu = sig * q
sig      =  0;  


% Initialize option output values
r_itr   =  r;
q_itr   =  q;

% -------------------------------------------------------------
% Iterate until convergence
% -------------------------------------------------------------

while ( q > eps | r > eps )
	mu    = sig * q;
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
		error(['npipp_of: ', msg])
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
	%	error(['npipp_of: ', msg])
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
	q       =  (lam' * y) / n; 
	Psi_lam =  Psi * lam;
	r       = max( abs(one_m - w .* Psi_lam) );

	% Update the reduction parameter sig for mu = sig * q. 
	% This done in a way that tries to ensure that 
	% mu and r are reduced at approximately the same rate.
	if q < eps & r > eps
		sig =  1;
	else
		term_pri  = (1 - alp_pri)^2;
		term_dual = (1 - alp_dual)^2;
		term_res  = (r - q) / (r + 1e2 * q);
		term      = max([ term_pri, term_dual, term_res ]);
		sig       = min( 0.3, term );
	end

	% Update convergence criteria return vectors
	q_itr =[q_itr  q]; 
	r_itr =[r_itr  r]; 

end
% convert back to original scaling
lam = lam / m;        
w   = w * m;

return
