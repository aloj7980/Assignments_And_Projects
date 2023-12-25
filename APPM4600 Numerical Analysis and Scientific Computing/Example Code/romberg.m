% Romberg integration....


a = 0;
b = pi;


f = @(x) sin(x);
Actual = 2;

nrefine = 5;

ntest = 2.^[0:1:nrefine];

nn = length(ntest);

Rom = zeros(nn,nn);

for j = 1:nn
    n = ntest(j);
    h = (b-a)/n;
    qnode = a+[0:n]*h;
    
    
    % trapezoidal rule
    
    Itrap = h/2*(f(a)+f(b)+2*sum(f(qnode(2:end-1))));
    
    Rom(j,1) = Itrap;
    
end

for j = 2:nn
     for i = j:nn
      Rom(i,j) = Rom(i,j-1) + (Rom(i,j-1)-Rom(i-1,j-1))/(4^(j-1)-1);  
     end
end

err = abs(2-diag(Rom));

semilogy(ntest,err)
keyboard