function [lam,y,w]=mainp2(Psi,t)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%PROBLEM DEFINITION%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%This program uses a primal-dual interior point path following
%%method to solve the following convex program:
%%   
%%  P: minimize phi(Psi*lam) + t*phi(lam/t)-row-col*t+row*<erow,lam> 
%%
%%
%%where
%%     m      = the number of row of Psi and t is given
%%and
%%     erow is a vector whose elemnts are one.
%%and
%%     phi(z) = -log(z(1)*z(2)*...*z(col)), for z>0,
%%and
%%     phi(z) = +infinity, otherwise,
%%
%%     Psi in R^{row x col}  with  row <= col, 
%%and
%%     Delta subset R^{col}
%%
%%is the standard simplex in R^{col}.
%%
%%The Fenchel-Rockafellar dual to this convex program is
%%the convex program
%%
%%   D: minimize phi(w) + t*phi(y)
%%
%%
%%The KKT conditions for this convex program are
%%
%% 0 <= w, 0<= y, and 0<= lam ,
%%
%%             (Psi)'w+y  =  row*ecol,
%%
%%            w.*Psi*lam  =  erow
%%and
%%                lam.*y  =  t*erow,
%%where erow and ecol are the vectors of all ones in the 
%%row and column space of Psi. Note that these conditions
%%combine to imply that lam is in Delta.
%%
%%In our analysis we make the following change of variables
%%
%%			w   =  w/row
%%			y   =  y/row
%%			lam =  lam*row  .
%%
%%This change of variables has the effect changing the
%%first equation in the Kuhn-Tucker equations to 
%%
%%		   (Psi)'w+y  =  ecol
%%
%%while the other two remain the same.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%GENERAL DESCRIPTION OF SOLUTION METHOD%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%The path-following algorithm applies Newton's method to the 
%%perturbed system of nonlinear equations
%%
%%             (Psi)'w+y  =  ecol,
%%
%%            w.*Psi*lam  =  erow
%%and
%%                lam.*y  =  mu * ecol,
%%
%%where mu is decreased to the value t. The Newton step
%%is damped so that the iterates remain strictly positive.
%%In addition, the path-following parameter mu is reduced
%%at each iteration in a manner that attempts to decrease 
%%its value at a rate that is approximately the same as the 
%%rate of decrease in the error in the nonlinear equation 
%% w.*Psi*lam = erow. Specifically, we try to reduce
%%
%%    |mu - t|    and     norm(w.*Psi*lam - erow,infinity)
%%
%%at approximately the same rate.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%INPUT REQUIREMENTS%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%The only input required for the algorithm is t and a matrix Psi
%%in R^{row x col} with row <= col, and Psi should only have
%%non-negative entries. In addition, we require that 
%%Psi*ecol >0.
%%
%%We assume that this information comes from an m-file titled
%%data.m. We also assume that Psi is large and sparse, so we
%%will be using sparse matrix operations.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%STOPPING CRITERIA%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%The procedure terminates when 
%%
%%  max([100*|mu-t| , norm(w.*Psi*lam - erow,infinity), gap]) <= eps
%%
%%where eps = 1.e-12, and gap is a measure of the duality gap 
%%associated with the current primal-dual feasible solutions 
%%for the problem P:
%%
%% gap=|sum(log(w))+t*sum(log(y))+sum(log(Psi*lam))+t*sum(log(lam/t))-row-col*t+row*sum(lam)|/(1+|sum(log(Psi*lam))+t*sum(log(lam/t))-row-col*t+row*sum(lam)|)
%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%CHECK INPUT SPECIFICATIONS%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

[row,col]=size(Psi);

%Check that row <= col.
if row > col
   error('The matrix Psi has row > col.')
end

%Check that Psi has only non-negative entries.
if min(min(Psi)) < 0
   error('The matrix Psi has a negative element.')
end   

%Check that Psi*ecol > 0.
ecol		=  ones(col,1);
Plam        =  Psi*ecol;
% if min(Plam) <= 1.e-15
%   error('The vector Psi*e has a non-positive entry.')
% end   

%Check that t > 1.e-12.
if t < 1.e-12
    error('Error tolerance on the value of t is violated')
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%STOPPING TOLERANCE%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

eps		=  1.e-12;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%INITIALIZATION%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%We now compute the initial estimate of (lam,y,w) 
%%and related parameters.
%%
%%This initialization tries to determine values for
%%(lam,y,w) that satisfy the Kuhn-Tucker conditions as 
%%closely as possible. In particular, we chose w and y 
%%so that the affine equation Psi'w+y=ecol is satisfied 
%%on each iteration. We begin by attempting
%%to set lam = ecol and then making the necessary 
%%adjustments so that the affine equation is satisfied.

%%Set the reduction parameter for the path-following parameter mu.

sig		=  0;  

%%Key vectors and parameters for the theoretical algorithm.

%ecol	=  ones(col,1); %ecol is defined in the 
                                %specification check above.
erow	=  ones(row,1);
lam		=  ecol; 
%Plam	=  Psi*lam;     %Plam  is defined in the 
                                %specification check above.
w		=  1./Plam; 
Ptw		=  Psi'*w; 
shrink	=  2*max(Ptw);
lam		=  lam*shrink; 
Plam    =  Plam*shrink; 
w		=  w/shrink; 
Ptw		=  Ptw/shrink;

y		=  ecol-Ptw;      %Observe that due to the rescaling by shrink
                 		  %this choice of y has 0.5*ecol <= y since
                              % 0.5*ecol <= ecol - (Psi'w)/(2*max(Psi'w).  
                           
R		=  erow-w.*Plam;  %This is the error in the nonlinear 
                                  %equation whose norm we try to decrease
                                  %at the same rate as mu.
normR	=  norm(R,inf);


gap		=  abs(sum(log(w*row))+t*sum(log(y*row))+sum(log(Psi*lam/row))+t*sum(log(lam/(t*row)))-row-col*t+row*sum(lam/row))/(1+norm(sum(log(Psi*lam/row))+t*sum(log(lam/(t*row)))+row*sum(lam/row),inf));
mu      =  (lam'*y)/col;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%PROTO-TYPE REPORTING STRUCTURES%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%We now define two data arrays for tracking the 
%progress of the proto-type code.

normNiter =  [normR];
muiter    =  [mu];

%We now set up the reporting structures for the proto-type.

shifttonew = 1;
iter=0;
fprintf('iter    sig     mu      normR     gap ');
fprintf('    derr      rcondH   alfpri  alfdual  tonloop\n');
fprintf('%2d   %2.2e %2.2e %2.2e %2.2e\n',iter,sig,mu,normR,gap);

%In the remainder of the file we terminate a line with %&% if either it
%is part of the proto-type or it is a line for which it might be
%possible to implement it in more efficient fashion.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%COMPUTATION OF THE NEWTON STEP%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%The damped Newton iteration is contained in the following while loop.
%As stated the stopping criteria is based on sufficient reduction in
%mu, the residual in the nonlinear equation, and the normalized 
%duality gap.
%
%We give a brief desciption of how the Newton step is computed. The
%underlying nonlinear function is
%                       | Psi'*w + y |
%         F(lam,w,y) =  | W*Psi*lam  |
%                       | Lam*Y*ecol | .
%
%Recall that Lam = diag(lam), Y = diag(y), and W = diag(w).
%The Jacobian of F is
%                      |  0         Psi'        I  |
%   grad F(lam,w,y) =  | W*Psi   diag(Psi*lam)  0  |
%                      |  Y          0         Lam | .
%
%The equation we want to solve is
%
%        F(lam,w,y) =  [ecol',erow',t*ecol]'.
%
%The Newton step (dlam,dw,dy) for this equation at a point (lam,w,y)
%is obtained by solving the linear equation
%
%  F(lam,w,y) + grad F(lam,w,y) (dlam,dw,dy) = [ecol',erow',t*ecol]'.
%
%Using standard reduction techniques we find that
%
%[diag((Psi*lam)./w) + Psi*diag(lam./y)Psi']*dw 
%
%    = diag(erow./w)*[erow - w.*Psi*lam]
%      - Psi*diag(ecol./y)*[t*ecol - y.*lam]
%      + Psi*diag(lam./y)*[ecol - Psi'*w - y]
%
%   (= erow./w - t*Psi*(ecol./y)   if ecol = y + Psi'*w),
%
% dy = ecol - y - Psi'w - Psi*dw  (= - Psi*dw if ecol=y+Psi'w), and
%
% dlam = t*(ecol./y) - lam - (lam./y).*dy .
%
%Thus, once we solve for dw, we can easily compute both 
%dy and dlam. This is the method employed below in the 
%computation of the Newton step.
%
%The positive definite symmetric matrix used in obtaining 
%dw above is denoted by the letter H:
%
%          H = [diag((Psi*lam)./w) + Psi*diag(lam./y)Psi']
%            = [diag(Plam./w) + Psi*diag(lam./y)Psi']

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%THE NEWTON ITERATION%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%In the computations that follow, we assume that
%         ecol = y + Psi'*w
%on all iterations.

SIG=[];

while 100*abs(mu-t)>eps| normR>eps | gap>eps,
 

tstart1 =  cputime;       %This is just for the proto-type. It is
tend    =  0;             %used to find out where the computational
                          %overhead is so that we can make the code
                          %more efficient where it is slow.
SIG=[SIG sig];
iter=iter+1;

%%Rescale mu for the Newton step.

smu      =  t +sig*(mu);

%%Computation of the Newton step (dlam,dy,dw).

tnewton	=  cputime;       %The variable tnewton is used to determine
                          %the cpu time used in computing the 
                          %the Newton step. This is another proto-type
                          %variable. %&%

% We now form the matrix H as a sparse array.

inner  =  lam./y;
H      =  Psi*sparse(1:col,1:col,inner,col,col,col)*Psi'...
          +sparse(1:row,1:row,Plam./w,row,row,row);

%We now compute the Cholesky factorization of H and check that
%this computation was successful.
H=(H+H')/2;
[upH,p] =  chol(H);
if p > 0.5
   error('Could not compute the Cholesky factorization of H.')
end

%We now form the right hand side in the evaluation of dw.

smuyinv  =  smu*(ecol./y);
rhsdw    =  erow./w - (Psi*smuyinv);    
 


%Compute dw for the Newton step.

dw       =  upH\(upH'\rhsdw);


%We now check the residual in evaluation of dw to see how
%accurate the dw step is.

derr     = norm(H*dw - rhsdw,inf); 
rcondH   = condest(H); %&% Just checking how bad H gets.

%Compute dlam for the Newton step.  

% dlam     =  smuyinv - lam - inner.*dy;

[ok, dlam, dw ] = NewtonStep(t, lam, w, Psi);
dy              = - Psi'*dw;

tnewton  =  cputime-tnewton;

%Damped Newton Step
%We now compute the rescalings necessary to keep all of the
%iterates strictly positive (that is, keep the iterates in
%the interior of the positive orthant).

tdamp	 =  cputime; %&%
alfpri   =  -1/min([dlam./lam;-.5]);
alfdual  =  -1/min([dy./y;-.5]);
alfdual  =  min(alfdual,-1/min([dw./w;-.5]));
alfpri   =  min(1,0.99995*alfpri);
alfdual  =  min(1,0.99995*alfdual);
tdamp    =  cputime-tdamp; %&%

%Newton Update
%We now update to the new point.

if alfpri==1,
  lam	=  lam+dlam;
else
  lam   =  lam+alfpri*dlam;
end;
if alfdual==1,
  w     =  w+dw;
  y     =  y+dy;
else
  w     =  w+alfdual*dw;
  y     =  y+alfdual*dy;
end;

%Update mu and intermediary vectors and 
%stopping criteria values.

mu		=  (lam'*y)/col; 
Plam    =  Psi*lam;
R       =  erow-w.*Plam;
Ptw     =  Ptw - alfdual*dy;
normR   =  norm(R,inf);
gap     =  abs((sum(log(w*row))+t*sum(log(y*row))+sum(log(Psi*lam/row))+t*sum(log(lam/(t*row)))-row-col*t+row*sum(lam/row)))...
                /(1+norm(sum(log(Psi*lam/row))+t*sum(log(lam/(t*row)))+row*sum(lam/row),inf));

%Update sig the reduction parameter for mu. 
%This done in a way that tries to ensure that 
%mu and normR are reduced at approximately the same rate.

if 100*abs(mu-t)<eps & normR>eps
  sig		=  0;
else
   c1		=  2; 
   c2		=  1.e+2;
   sig	=  min([0.3,max([(1-alfpri)^c1,...
                (1-alfdual)^c1,...
                (normR-100*abs(mu-t))/(normR+c2*100*abs(mu-t))])]);
end;
  
%Time for one Newton iteration.
tonloop =  cputime-tstart1; 

supp=0;for i=1:col;if lam(i)> 1.e-8;supp=supp+1;end;end
supp=supp

%Update reporting information for the proto-type. 
muiter=[muiter mu]; 
normNiter=[normNiter normR]; 
fprintf('%2d   %2.2e %2.2e %2.2e %2.2e %2.2e %2.2e %2.2e %2.2e %2.2e\n',...
        iter,sig,mu,normR,gap,derr,rcondH,alfpri,alfdual,tonloop);
end;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%FINAL REPORTING OF OUTPUT%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tend=cputime-tstart1;

lam  			=  lam/row;         
simplexerror	=  abs(1+col/row*t-lam'*ecol)
obj             =  log(prod(w))
y               =  y*row;
w               =  w*row;


%This is the value of lam returned as the solution to the problem.
%%
lam				=  lam/norm(lam,1)*(1+col/row*t);
SIG
%%
disptext			=  'The primal-dual predictor method';
disp=('The primal-dual predictor method terminated successfully.')
