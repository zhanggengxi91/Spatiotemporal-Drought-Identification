% main dro3D
clc;

SZI3 = ncread('Data/SZI_SSP245_3month_1979_2100_historical.nc', 'variable');
SZI = rot90(flipud(SZI3),3);
data=SZI;
% dro3d:
% data-->drought index: spi
% itv--> spi threshold，即干旱阈值，论文中取的是-1
% igtv-->TV1, 前后两个时间段，两个相交干旱面积的比例
% egtv-->TV2, 决定干旱开始和结束的面积阈值

[row,col,pag]=size(data);
data(isnan(data))=0;
for i=1:pag
    data(:,:,i)=waterdamShape(data(:,:,i), -1.0, 20, 2);   % s
    data(:,:,i)=waterdamValue(data(:,:,i), -1.0, 20, 1.2); % v
end

dro=dro3d(data,-1.0,0.5,80); % 05 50
latmax = 55.5 - 0.25;
lonmin = 69.5 + 0.25;
startyear = 1979;
[droVal1,droEvent1,droInfo1]=droinfoCalculator(dro,data,latmax,lonmin,startyear);
save spei_05_165_svvvv droVal droEvent droInfo  % save的储存格式
