function W=dlx(dataY,x)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Input1 :  dataY,      rY     by   cY   matrix
%% Input3 :  x,              2     by     1    matrix
%%%------------------------------------------------------%%%
%% Output :  dlx,           cY   by     2   matrix
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%W is M by 2 matrix 

[m,M]=size(dataY);
c = -(25)/2;
k = x(1);
v = x(2);
t = (1/m)*(1:m)';
d = (32*pi)^(-m/2)*(v^m);
HI = (v/20)*exp(k*t);

Wk = zeros(M,1);
Wv = zeros(M,1);


for i = 1:M
y = dataY(:,i);
s = y.*HI;
s1 = s-1;
s21 = 2*s-1;
st = s.*t;
t2 =t.*t;
st2 =s.*t2;

alpha0 = k*(m+1)/2 + c*(s1'*s1);
alpha1 = m/(v) + (2*c/v)* (s1'*s);
alpha2 = (m+1)/2 + 2*c*(s1'*st); 
beta1 = 2*c/(v^2)*(s21'*s);
beta2 = 2*c/(v)*(s21'*st);
beta3 = 2*c*(s21'*st2);

L = d*exp(alpha0);

Lk = alpha2*L;
Lv = alpha1*L;

  
Wk(i) = Lk;
Wv(i) = Lv;

end

W = [Wk Wv];
