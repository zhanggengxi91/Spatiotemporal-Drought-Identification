% dro3d
function index1=dro3d(index,itv,igtv,egtv)
% index: drought index
%itv: index threshold value
%igtv: intersection grid number threshold
%egtv: 最小面积阈值

[row,col,pag]=size(index);

if itv>0
    index1=double(index>itv);
end
if itv<0
    index1=double(index<itv);
end

for i=1:pag
    disp(i);
    if i==1
        stnum=1;
    else
        stnum=length(idx_t1)+stnum;
    end

    [index1(:,:,i),idx_t2]=connect2d(index1(:,:,i),stnum,egtv);
    
    if i==1
    else
        for j=1:length(idx_t2)
            for k=1:length(idx_t1)
                interPart=intersect(idx_t2{j},idx_t1{k});
                len=length(interPart);
				len1=length(idx_t1{k});
				len2=length(idx_t2{j});
                
				interLen=max(len/len1,len/len2);
                interLen=len; % added on 20140223
                %if interLen<igtv
                if interLen<egtv
                    continue;
                else
                    temp1=index1(:,:,1:i);
                    temp2=index1(:,:,i-1);
                    tt=temp2(idx_t1{k});
                    temp3=unique(tt);
                    temp4=temp1==temp3;
                    temp5=index1(:,:,i);
                    temp1(temp4)=unique(temp5(idx_t2{j}));
                    index1(:,:,1:i)=temp1;
                end
            end
        end
    end
    idx_t1=idx_t2;
end
end
