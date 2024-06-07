%problem 5

clear all;
close all;

N = 10^6;   %number of samples

us = rand(1,N); % uniform R.V.'s to simulate X
ws = rand(1,N); % uniform R.V.'s to simulate Y

xs = -log(us); % exponential with parameter 1
ys = -log(ws)/2; % exponential with parameter 2

zs = ys./xs; % Z = Y/X

%plot --- don't plot the whole horizontal domain!!
histogram(zs,'Normalization','PDF','BinEdges',0:0.05:4)
hold on;
x = 0:0.01:4;
plot(x,2./(1+2.*x).^2,'-r','LineWidth',2)
set(gca,'FontSize',16,'FontName','Helvetica');
box on
grid on