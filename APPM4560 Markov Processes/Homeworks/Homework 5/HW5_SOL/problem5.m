close all;
clear all;
beta = 0.001;
b = 0.001*exp(1.0);
alpha = 0.4;
a = 0.2;

N = 10;
T = 10000000;

n = 8;

for t = 1:T
     
   pn = beta*exp(alpha*n);
   qn = b*exp(a*(n-1));

   ntemp = n;

   r = rand();
   if(ntemp>1 && ntemp < N)
       if(r < pn)
          n = n+1;
       elseif(r < pn + qn)
          n = n-1;
       end
   end
   if(ntemp == 1)
       if(r<pn)
           n = 2;
       end
   end
   if(ntemp == N)
       if(r<qn)
           n = n-1;
       end
   end
       
   en(t) = n;

end

is = 1:10;
pie = exp((is-1).*(log(beta/b)+0.5*(alpha-a).*is));
C = sum(pie);
pie = pie/C;

pn = beta*exp(alpha*is);
qn = b*exp(a.*(is-1));

p = [1-pn(1) pn(1) 0 0 0 0 0 0 0 0;...
    qn(2) 1-pn(2)-qn(2) pn(2) 0 0 0 0 0 0 0;...
    0 qn(3) 1-pn(3)-qn(3) pn(3) 0 0 0 0 0 0;...
    0 0 qn(4) 1-pn(4)-qn(4) pn(4) 0 0 0 0 0;...
    0 0 0 qn(5) 1-pn(5)-qn(5) pn(5) 0 0 0 0;...
    0 0 0 0 qn(6) 1-pn(6)-qn(6) pn(6) 0 0 0;...
    0 0 0 0 0 qn(7) 1-pn(7)-qn(7) pn(7) 0 0;...
    0 0 0 0 0 0 qn(8) 1-pn(8)-qn(8) pn(8) 0;...
    0 0 0 0 0 0 0 qn(9) 1-pn(9)-qn(9) pn(9);...
    0 0 0 0 0 0 0 0  qn(10) 1-qn(10)];

[ve,lambda] = eigs(p',1)
ve = ve/sum(ve);

plot(en)
set(gca,'FontSize',16,'FontName','Helvetica');
grid on;
box on;
ylim([0 N])

figure;
histogram(en,'Normalization','PDF')
set(gca,'FontSize',16,'FontName','Helvetica');
xlim([0 11])
hold on
plot(is,pie,'-','LineWidth',2);
plot(is,ve,'bx','MarkerSize',10,'LineWidth',4);
