function [droVal,droEvent,droInfo]=droinfoCalculator(index1, index, latmax, lonmin, startyear)

% index1: dro3d.m的结果
% index: 指标

[row,col,pag]=size(index);
droVal=zeros(row,col,pag);
arrArea = zeros(row, col, pag); %将数组的每个位置计算出面积，负值给arrArea;
for i= 1:row
    arrArea(i,:,:) = areaCalculator(i);
end
arrArea_i = zeros(row, col, pag); %用于存储第i次干旱的面积值
droNum1=unique(index1); % 返回唯一值
droNum=droNum1(2:end);
len=length(droNum);
droArea=zeros(len,1);
drost=zeros(len,2);
droed=zeros(len,2);
droS=zeros(len,1);
droIntensity=zeros(len,1); 
latMin=zeros(len,1);
latMax=zeros(len,1);
lonMin=zeros(len,1);
lonMax=zeros(len,1);
duration=zeros(len,1);

for i=1:len
    disp(i);
    temp1=index1==droNum(i);%将第droNum次干旱的标识号赋值给temp1,temp1为与index1同维度的数组，除了第droNum次干旱发生位置为标识号外，其他位置都为0;
    droVal(temp1)=index(temp1);
    arrArea_i(temp1)=arrArea(temp1);
    ind=find(temp1); % 查找非零元素的索引
    [rowNum,colNum,pagNum]=ind2sub([row,col,pag],ind); % 将线性索引转为下标
    rowMin=min(rowNum);
    rowMax=max(rowNum);
    colMin=min(colNum);
    colMax=max(colNum);
    pagMin=min(pagNum);
    pagMax=max(pagNum);
    
    % drought extent
    latMin(i)=latmax-(rowMax-1)*0.5;
    latMax(i)=latmax-(rowMin-1)*0.5;
    lonMin(i)=lonmin+(colMin-1)*0.5;
    lonMax(i)=lonmin+(colMax-1)*0.5;
    % drought duration
    duration(i)=pagMax-pagMin+1;
    % drought affacted area
    for r=rowMin:rowMax
        gridArea=areaCalculator(r);
        temp2=nan(col,pag);
        temp2(:,:)=temp1(r,:,:);
        gridNum=sum(nansum(temp2,2)>0);
        droArea(i)=droArea(i)+gridArea*gridNum;
    end
    
    % drought start time and end time
    if mod(pagMin,12)==0
        drost(i,1)=startyear+floor(pagMin/12)-1;
        drost(i,2)=12; % 干旱开始时间
    else
        drost(i,1)=startyear+floor(pagMin/12);
        drost(i,2)=mod(pagMin,12);
    end
    
    if mod(pagMax,12)==0
        droed(i,1)=startyear+floor(pagMax/12)-1; % floor 朝负无穷大四舍五入
        droed(i,2)=12; % 干旱结束时间
    else
        droed(i,1)=startyear+floor(pagMax/12);
        droed(i,2)=mod(pagMax,12);
    end
    
    % drought severity
    droS(i)=abs(sum(sum(sum(droVal(temp1).*arrArea_i(temp1)))));
    Intensity = droVal(temp1);
    droIntensity(i) = abs(mean(mean(mean(Intensity(Intensity~=0)))));
end
droInfo1=[droNum,latMin,latMax,lonMin,lonMax,drost,droed,duration,droArea,droS,droIntensity];
droInfo1=sortrows(droInfo1,[6,7]); %首先基于第六列中的元素进行升序排序，其次根据第七列中的元素升序
droInfo=droInfo1(:,2:end);

droEvent=zeros(row,col,pag);
for i=1:len
    temp3=index1==droInfo1(i,1);
    droEvent(temp3)=i;
end
end











