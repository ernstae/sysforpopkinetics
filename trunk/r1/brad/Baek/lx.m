  function W=lx(dataY,x)


% x \in R^2

%W is M*2 by 2 matrix 

[m,M]=size(dataY);

c = -(25)/2;
k = x(1);
v = x(2);
t = (1/m)*(1:m)';
d = (32*pi)^(-m/2)*(v^m);
HI = (v/20)*exp(k*t);




W =zeros(M,1);
for i = 1:M
y = dataY(:,i);
s = y.*HI;
s1 = s-1;
alpha0 = k*(m+1)/2 + c*(s1'*s1);


L = d*exp(alpha0);

W(i) = L;

end
