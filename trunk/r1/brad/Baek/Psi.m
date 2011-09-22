function w=Psi(dataY,x,n)

%% x n by hatM matrix.
%% But x is vectorized, so x should be a n*matM vector


hatM=length(x)/n;
w=[];
for i=1:hatM
    p=lx(dataY,x(1+n*(i-1):n*i));
    w=[w p];  
end
