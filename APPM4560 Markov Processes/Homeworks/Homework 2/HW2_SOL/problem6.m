%problem 6

clear all;
close all;

N = 10^6;   %number of samples

us = rand(1,N); % uniform R.V.'s to simulate X

xs = us.^(-1/3);

mean(xs)