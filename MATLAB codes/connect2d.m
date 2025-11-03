%  2D connectivity
function [numberedSpace,spaceIdx]=connect2d(data,stnum,egtv)
% data: 2维 0,1 数组
% 开始给联通区域标号的数值
% 两个不相干的干旱区别
%%
data=double(data);
[row,col]=size(data);
numberedSpace=zeros(row,col);
connectSpace=bwconncomp(data,4);
spaceNum=connectSpace.NumObjects;
spaceIdx1=connectSpace.PixelIdxList;
spaceIdx=cell(0);
num=1;
for i=1:spaceNum
    % 在网格小于egtv的space停止
    if length(spaceIdx1{i})>egtv
        spaceIdx{num}=spaceIdx1{i};
        numberedSpace(spaceIdx1{i})=stnum+num-1;
        num=num+1;
    end
end
end