function output=waterdamShape(input, itv, para1, para2)

%该程序输入的是每个月的干旱图像，根据识别原则，将面积大于para1的连通
%斑块提取出来，输出值为剪裁后的月干旱图像
% para1,para2, 建议取值：
%%%para1=20;
%%%para2=2;
output=zeros(size(input));
data=input;

if itv>0
    bw=data>itv;  %得到一个逻辑值，在本案例中是对矩阵中每个值进行判断，得到0和1的矩阵
end
if itv<0
    bw=data<itv;  % 本案例进入该循环，即小于-1的为1，大于-1的为0
end

% bwareaopen是一种二分类图像，即只包括0和1两种值,0为黑色块，1为白色块
% bwareaopen(BW,P)的作用是将BW中连续像素小于P的斑块剔除，亦即
% 由1变为0。
% 在该图中，干旱斑块为1,非干旱斑块为0，用~bw剔除面积小于para1的非干旱斑块
bw2 = ~bwareaopen(~bw, para1);  % 从二值图像中删除少于P个像素的所有连通分量，生成另一个二值图像
D = -bwdist(~bw2);
mask = imextendedmin(D, para2);
D2 = imimposemin(D,mask);
Ld2 = watershed(D2);
bw3 = bw;
bw3(Ld2 == 0) = 0;
output(bw3)=input(bw3);
% figure(1);
% imagesc(bw3);
% figure(2);
% imagesc(data);
% figure(3)
% imagesc(output)
end