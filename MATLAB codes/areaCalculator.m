% area calculator
% 20140321
function s=areaCalculator(j)
lat=55.25-(j-1)*0.5;
s=0.5*0.5*110*110*cos(lat/180*pi);
end