function [ok, lambdaStep, wStep] = NewtonStep(t, lambda, w, Psi)
% See NewtonStep.tex for documentation
[M, N]             = size(Psi);
PsiT               = Psi';
e_M                = ones(M, 1);
e_N                = ones(N, 1);
y                  = e_N - PsiT * w;
yinv               = e_N ./ y;
winv               = e_M ./ w;
LamYinvPsiT        = diag( lambda .* yinv ) * PsiT;
LinEquation        = diag( winv .* ( Psi * lambda ) ) + Psi * LamYinvPsiT;
rightHandSide      = winv - t * Psi * yinv;
[Factor, singular] = chol(LinEquation);
ok                 = logical(singular == 0);
if ok
    wStep          = LinEquation \ rightHandSide;
    lambdaStep     = t * yinv - lambda + LamYinvPsiT * wStep;
else
    wStep          = zeros(M, 1);
    lambdaStep     = zeros(N, 1);
end
