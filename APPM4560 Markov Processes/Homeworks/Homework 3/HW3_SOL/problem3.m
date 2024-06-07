tic 

clear all;
close all;

j = 1;
for i = 1:1000000
    
    w = rand();
    y = tan(pi*(w-0.5));
    u = rand();
    
    if(u < (1+y^2)*exp((1-y^2)/2)/2)
        sam(j) = y;
        j = j+1;
    end
end

histogram(sam,100,'Normalization','PDF')
hold on;
x = 0:0.01:4;
plot(x,exp(-x.^2/2)/sqrt(2*pi),'-r','LineWidth',2)
xlim([0,5])

toc