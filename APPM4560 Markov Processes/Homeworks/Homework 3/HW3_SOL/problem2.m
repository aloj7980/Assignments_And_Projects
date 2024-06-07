close all
clear all

%generate exponential samples
N = 10^5;  % number of samples
samples = asin(2*rand(N,1)-1);  % create array of uniforms and transform it

%plot
histogram(samples,100,'Normalization','pdf')
hold on
x = -pi/2:0.01:pi/2;
plot(x,cos(x)/2,'r','Linewidth',3)
xlim([-pi/2, pi/2])