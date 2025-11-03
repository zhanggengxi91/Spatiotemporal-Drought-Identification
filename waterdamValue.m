function output=waterdamValue(input, itv, para1, para2)
% para1,para2, 建议取值：
%%%para1=20;
%%%para2=1.2;
output=nan(size(input));
data=input;
if itv>0
    bw=data>itv;
end
if itv<0
    bw=data<itv;
end

bw2 = ~bwareaopen(~bw, para1);
%%
temp1=bw;
temp2=temp1+bw2;
temp3=temp2==1;
%%
data(temp3)=-1.001;
D = data;
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
% figure(3);
% imagesc(output)
end