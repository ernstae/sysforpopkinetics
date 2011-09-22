function W=ddlx(dataY,x)

%W is M*2 by 2 matrix 

[m,M]=size(dataY);
c = -(25)/2;
k = x(1);
v = x(2);
t = (1/m)*(1:m)';
d = (32*pi)^(-m/2)*(v^m);
HI = (v/20)*exp(k*t);

Wkk = zeros(M,1);
Wvv = zeros(M,1);
Wvk = zeros(M,1);

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

Lkk = (beta3+alpha2^2)*L;
Lvv = (-alpha1/v + beta1+alpha1^2)*L;
Lvk = (beta2+alpha1*alpha2)*L;
  
Wkk(i) = Lkk;
Wvv(i) = Lvv;
Wvk(i) = Lvk;
end

W1 = [Wkk Wvk];
W2 = [Wvk Wvv];
W = [W1;W2];

