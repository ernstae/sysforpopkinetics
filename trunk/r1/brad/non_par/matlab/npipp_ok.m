% $begin npipp_ok.m$$ $newlinech %$$
% $spell
%	npipp
%	eps
%	itr
%	roundoff
% $$
% 
% $section Example and Test of npipp_of$$
% $codep
function [ok] = npipp_ok()
ok  = true;
% ------------------------------------------------------------------------
% case where solution is lam = [2/3, 1/3]. This can be determined by 
% substituting lam(2) = 1 - lam(1) in Psi * lam and setting derivative to zero.
Psi = [ 1 , 0 , 0 ; ...
       .25, 1 , 0 ];   
[m, n]                 = size(Psi);
eps                    = 1e-12;
[lam, w, q_itr, r_itr] = npipp_of(Psi, eps);
y                      = m - Psi.' * w;
ok  = ok & abs(lam(1) - 2/3) < 1e-12;
ok  = ok & abs(lam(2) - 1/3) < 1e-12;
%
% The duality gap is phi( Psi * lam ) + phi( w )
% (It may be negative because of roundoff error.)
gap = sum( - log( Psi * lam ) ) + sum( - log( w ) );
ok  = ok & abs(gap) <= 1e-12;
%
% check for feasible
ok  = ok & all( w >= 0 );
ok  = ok & all( y >= 0 );
%
% check for descent in the maximum residual measure (at each iteration)
n_itr    = length(q_itr);
for k = [2 : n_itr]
	ok   = ok & (q_itr(k-1) > q_itr(k));
end
ok = (q_itr(n_itr) < eps);
ok = (r_itr(n_itr) < eps);
%----------------------------------------------------------------------------
% case where psi has m2 columns nearly equal to previous columns.
n     = 20;
m     = 100;
m1    = m / 2;
m2    = m - m1;
scale =	1e-6;
A1    = rand(n, m1);
E     = rand(n, m2);
Psi   = [A1, A1 + scale *E];
eps   = 1e-12;
[lam, w, q_itr, r_itr] = npipp_of(Psi, eps);
y     = m - Psi.' * w;
gap   = sum( - log( Psi * lam ) ) + sum( - log( w ) );
ok    = ok & abs(gap) <= 1e-12;
ok    = ok & all( lam >= 0 );
ok    = ok & all( w >= 0 );
ok    = ok & all( y >= 0 );
n_itr    = length(q_itr);
for k = [2 : n_itr]
	ok   = ok & (q_itr(k-1) > q_itr(k));
end
ok = (q_itr(n_itr) < eps);
ok = (r_itr(n_itr) < eps);
%----------------------------------------------------------------------------
return
% $$
% $end
