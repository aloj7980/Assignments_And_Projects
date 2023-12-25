function Newton_systems

% Apply Newton's method to find the roots of f(z) = z^3-1
clear all; close all;
col = 100;          % Number of Newton iterations
m = 2000;            % Plot resolution
cx = 0;             % Center of plot (real)
cy = 0;             % Center of plot (imaginary)
l = 1;              % box size

x = linspace(cx-l, cx+l, m);
y = linspace(cy-l, cy+l, m);
[X,Y] = meshgrid(x,y);

Z = complex(X,Y);

c = -0.5*complex(1,sqrt(3));
for k = 1:col      % Loop over the Newton iterations
    Z = 2/3*Z + 1/3*1./(eps + Z.^2);
end

W = abs(Z-c);
A = angle(Z);

colormap prism(256)
pcolor(W-A);
shading flat;
axis('square','equal','off');

keyboard



return