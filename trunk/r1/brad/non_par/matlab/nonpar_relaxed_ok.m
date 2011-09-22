% $begin nonpar_relaxed_ok.m$$ $newlinech %$$
% $spell
%	qk
%	Psi Psi
%	nonpar
%	eps
%	itr
%	roundoff
% $$
% 
% $section Example and Test of nonpar_relaxed$$
% $codep
function [ok] = nonpar_relaxed_ok()
ok  = true;
% ========================================================================
% case where solution is lam = [2/3, 1/3]. This can be determined by 
% substituting lam(2) = 1 - lam(1) in Psi * lam and setting derivative to zero.
Psi = [ 1 , 0 , 0 ; ...
       .25, 1 , 0 ];   
[m, n]                 = size(Psi);
t                      = 0.;
eps                    = 1e-12;
[lam, w, info] = nonpar_relaxed(Psi, t, eps);
%
% maximum absolute element in Psi
Psi_max = max( max(Psi) );
%
y                      = m - Psi.' * w;
ok  = ok & abs(lam(1) - 2/3) < eps * Psi_max;
ok  = ok & abs(lam(2) - 1/3) < eps * Psi_max;
%
% The duality gap is phi( Psi * lam ) + phi( w )
% (It may be negative because of roundoff error.)
gap = sum( - log( Psi * lam ) ) + sum( - log( w ) );
ok  = ok & abs(gap) <= eps * Psi_max;
%
% check for feasible
ok  = ok & all( w >= 0 );
ok  = ok & all( y >= 0 );
%
% check for descent in the maximum residual measure (at each iteration)
n_itr    = size(info,1);
for k = [2 : n_itr]
	qk1  = info(k-1, 2);
	qk   = info(k, 2);
	ok   = ok & (qk1 > qk);
end
ok = max( abs(lam .* y - t) ) < eps * Psi_max;
ok = max( abs( w .* (Psi * lam) - 1 ) ) < eps * Psi_max;
% ------------------------------------------------------------------------
% sub-case where t is not zero
t = 1e-1;
[lam, w, info] = nonpar_relaxed(Psi, t, eps);
y                      = m - Psi.' * w;
ok  = ok & all( w >= 0 );
ok  = ok & all( y >= 0 );
n_itr    = size(info,1);
for k = [2 : n_itr]
	qk1  = info(k-1, 2);
	qk   = info(k, 2);
	ok   = ok & (qk1 > qk);
end
ok = max( abs(lam .* y - t) ) < eps * Psi_max;
ok = max( abs( w .* (Psi * lam) - 1 ) ) < eps * Psi_max;
% ========================================================================
% case where psi has m2 columns nearly equal to previous columns.
n     = 20;
m     = 100;
m1    = m / 2;
m2    = m - m1;
scale =	1e-6;
A1    = rand(n, m1);
E     = rand(n, m2);
Psi   = [A1, A1 + scale *E];
t     = 0.;
eps   = 1e-12;
[lam, w, info] = nonpar_relaxed(Psi, t, eps);
%
% maximum absolute element in Psi
Psi_max = max( max(Psi) );
%
y     = m - Psi.' * w;
gap   = sum( - log( Psi * lam ) ) + sum( - log( w ) );
ok    = ok & abs(gap) <= eps * Psi_max;
ok    = ok & all( lam >= 0 );
ok    = ok & all( w >= 0 );
ok    = ok & all( y >= 0 );
n_itr    = size(info,1);
for k = [2 : n_itr]
	qk1  = info(k-1, 2);
	qk   = info(k, 2);
	ok   = ok & (qk1 > qk);
end
ok = max( abs(lam .* y - t) ) < eps * Psi_max;
ok = max( abs( w .* (Psi * lam) - 1 ) ) < eps * Psi_max;
% ------------------------------------------------------------------------
% sub-case where t is not zero
t = 1e-1;
[lam, w, info] = nonpar_relaxed(Psi, t, eps);
y                      = m - Psi.' * w;
ok  = ok & all( w >= 0 );
ok  = ok & all( y >= 0 );
n_itr    = size(info,1);
for k = [2 : n_itr]
	qk1  = info(k-1, 2);
	qk   = info(k, 2);
	ok   = ok & (qk1 > qk);
end
ok = max( abs(lam .* y - t) ) < eps * Psi_max;
ok = max( abs( w .* (Psi * lam) - 1 ) ) < eps * Psi_max;
%----------------------------------------------------------------------------
return
% $$
% $end
