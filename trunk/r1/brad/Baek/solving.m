function solving

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%       Generate Data               %%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


m       =   6;      %  the dimension of y_i 
n       =   2;      %  the the dimension of the parameter, x = (V,K)
M       =   20;     %  the number of data : dataY = (y_1 , y_2, ... y_20)

V       =   .2*randn(M,1) + 2;

K1      =   sqrt(.05)/4*randn(M/2,1)+.5;   %3
K2      =   sqrt(.15)/4*randn(M/2,1)+1.5;  %2
K       =   [K1;K2];

tj      =   [1:m]'/m;

dataY   =   [];
for i   =   1:M
    ej      =   .2*randn(m,1);
    mi      =   20*exp(-K(i)*tj)/V(i);
    dataY   =   [dataY mi.*(1+ej)];
end







%%%%    bound for K and V %%%%%%%%%%%

boundK      =   [0.1 2.5];  % bound for K
boundV      =   [0.5 3.5];  % bound for V
bound       =   [boundK;boundV]; % coombined bound

lowb        =   bound(:,1); %lower bound for (K,V)                                                    
upb         =   bound(:,2); %upper bound for (K,V)   


hatM        =   2*M;      % the number of solution (x_1, x_2 , .... x_{hatM})

%% Find an starting point  
x0          =   findInitial(dataY,bound,hatM,n);


%% Find vectorized x
x           =   reshape(x0,n*hatM,1);

%% Vectorize the bound of K and V with the same dimension of vectorized x
ub          =   repmat(upb,hatM,1);  %% vectorized upper bound for (K,V)
lb          =   repmat(lowb,hatM,1); %% vectorized lower bound for (K,V)



k           =   1;
maxk        =   9;
Result      =   [];
options = optimset('GradObj','on','Hessian','on');

while k <=  maxk 
t       =   10^(-k);
[x, fval, exitflag,output, lambda,grad,hessian,] = fmincon(@myfun,x,[],[],[],[],lb,ub,[],options,dataY,t,n);
%[x, fval, exitflag,output, grad,hessian,] = fminunc(@myfun,x,options,dataY,t,n);

%% Find the Optimal condition number
P           =   Psi(dataY,x,n);
t0          =   1.e-10;
[lam,y,w]   =   mainp2(P,t0);
[x1,fval1,exitflag1,output1,grad1] = optimalCon(w,dataY,n,lowb,upb);
conditionN       =   -fval1 - M;

Result       =   [Result ;[t norm(grad) conditionN]];
k=k+1;
end

format short e
disp('****************************************')
disp('****************************************')
disp(' t   ::norm of gradient ::Optimal condition number ')
Result 
disp('****************************************')
disp('****************************************')




function [ww, df, D]=myfun(x,dataY,t,n)

P=Psi(dataY,x,n);
[M,hatM] = size(P)
[lam,y,w]=mainp2(P,t);
Plam=P*lam;
%w=-sum(log(Plam)) -t*log(prod(lam)) + M*sum(lam);
ww=-sum(log(Plam)) +t*sum(log(y))-hatM*t*log(t) + M + hatM*t;

if nargout > 1

df          =   [];
for j= 1:hatM
    %% find dl(x)
    xj      =   x(1+n*(j-1):n*j);
    dlxj    =   dlx(dataY,xj);   
    
    %% find df(x)
    Q       =   lam(j)*dlxj;
    dfj     =   -w'*Q;
    df      =   [df dfj];
end


if nargout > 2

[dlambda,dw]    =   dlam(dataY,x,t,n);
In              =   eye(n);

DA              =   [];
DB              =   [];
DC              =   [];


for i = 1:hatM
    xi      =   x(n*(i-1)+1:n*i);
    dli     =   dlx(dataY,xi);
    lami    =   lam(i);
    li      =   lx(dataY,xi);
    Hi      =   ddlx(dataY,xi);
    dlami   =   dlambda(i,:);
    
    ei      =   zeros(hatM,1);
    ei(i)   =   1;
          
    DAi     =   -lami*kron(In,w')*kron(ei',Hi);
    DBi     =   -lami*(dli')*dw;
    DCi     =   -kron(dli'*w,dlami);
    
    DA      =   [DA;DAi];
    DB      =   [DB;DBi];
    DC      =   [DC;DCi];
end

D       =   DA + DB + DC;

end

end





function [x,fval,exitflag,output,grad] = optimalCon(w,dataY,n,lowb,upb)


options = optimset('GradObj','on','Hessian','on');

x = [0.5;1.5];

[x, fval, exitflag,output, lambda,grad,hessian,] = fmincon(@myGfun,x,[],[],[],[],lowb,upb,[],options,w,dataY,n);
%[x, fval, exitflag,output, grad,hessian,] = fminunc(@myGfun,x,options,w,dataY,n);






function [g , dg, ddg] = myGfun(x,w,dataY,n)

 L=lx(dataY,x);
 g = -w'*L;
 
 if nargout > 1
 dL=dlx(dataY,x);
 dg = -w'*dL;
 
 if nargout > 2
  ddL = ddlx(dataY,x);
  In = eye(n);
  ddg  = -kron(In,w')*ddL;
end
end






function [dlambda,dw] = dlam(dataY,x,t,n)
%% Output
%% dlambda : hatM by n*hatM matrix
%% dw : M by n*hatM matrix

P           =   Psi(dataY,x,n);

[M,hatM]    =   size(P);
[lam,y,w]   =   mainp2(P,t);
z           =   1./w;
t1          =   1/t;

y2          =   y.*y;
w2          =   w.*w;
z2          =   z.*z;
lam2        =   lam.*lam;

Y           =   diag(y);
Y2          =   diag(y2);
W           =   diag(w);
W2          =   diag(w2);
Z           =   diag(z);
Z2          =   diag(z2);
Lam         =   diag(lam);
Lam2        =   diag(lam2);

A           =   t1*Y2 + P'*W2*P;
C           =   Z2 + t1*P*Lam2*P';

[upA,pA] =  chol(A);
[upC,pC] =  chol(C);

if pA > 0.5
   error('Could not compute the Cholesky factorization of A in dlam.')
end

if pC > 0.5
   error('Could not compute the Cholesky factorization of C in dlam.')
end


nablaL      =   [];
nablaW      =   [];

for i = 1:hatM
    xi      =   x(n*(i-1)+1:n*i);
    dli     =   dlx(dataY,xi);
    lami    =   lam(i);
    li      =   lx(dataY,xi);
    ei      =   zeros(hatM,1);
    ei(i)   =   1;
    wdli    =   w'*dli;
    
    nablaLi =   -kron(ei,wdli) + lami*P'*W2*dli;
    nablaWi =   t1*lami*lami*kron(wdli,li) + lami*dli;    
    
    nablaL  =   [nablaL nablaLi];
    nablaW  =   [nablaW nablaWi];
end

    
dlambda     =   -upA\(upA'\nablaL);
dw          =   -upC\(upC'\nablaW);






function z = findInitial(dataY,bound,hatM,n)


lowb    =   bound(:,1);                                                     
upb     =   bound(:,2);   
[m,M]   =   size(dataY);
x       =   (lowb + upb)/2;
z       =   [];
options = optimset('GradObj','on','Hessian','on');
for i   = 1:M
    [xi, fval, exitflag,output, lambda,grad,hessian,] = fmincon(@mylx_i,x,[],[],[],[],lowb,upb,[],options,dataY,hatM,n,i);
    %[xi, fval, exitflag,output, grad,hessian,] = fminunc(@mylx_i,x,options,dataY,hatM,n,i);
    z = [z xi];    
end


K   =   hatM - M;
if K > 0
    
    X       =   repmat(x,1,K);
    z       =   [z X];
end


function [Lxi , dLxi, ddLxi] = mylx_i(x,dataY,hatM,n,i)

[m ,M]  =   size(dataY);
L       =   -lx(dataY,x);
Lxi     =   L(i);
if nargout > 1
    dL     =   -dlx(dataY,x);
    dLxi   =   dL(i,:);
    if nargout > 2
        ddL     =   -ddlx(dataY,x);
        ddLxi   =   [];
        for k   =   1:n
            ddLi    =   ddL(M*(k-1)+i,:);
            ddLxi   =   [ddLxi;ddLi];
        end
    end
end