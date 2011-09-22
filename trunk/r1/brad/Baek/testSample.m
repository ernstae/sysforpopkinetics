% Inialize the state of the random number generator to reproduce results
state = 137;
rand('state', state);
%
M = 10;
hatM = 15;
Psi = rand(M,hatM);
t = 1.e-3;

[lam,y,w]=mainp2(Psi,t)
