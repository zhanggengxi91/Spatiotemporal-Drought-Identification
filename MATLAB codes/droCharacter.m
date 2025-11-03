% 2014/02/22
%drought event details

function droDetails=droCharacter(droEvent,droVal,droInfo)


droNum=length(unique(droEvent))-1;

event=droEvent;
val=droVal;
info=droInfo;

event(event==0)=NaN;
val(val==0)=NaN;

sYear=nan(droNum,1);
sMonth=nan(droNum,1);
eYear=nan(droNum,1);
eMonth=nan(droNum,1);
D=nan(droNum,1);
centxyz=nan(droNum,3);
affArea=nan(droNum,1);
volum=zeros(droNum,1);
aRatio=nan(droNum,1);
%%
for i=1:droNum
    %% duration info
    % start time
    sYear(i,1)=info(i,5);
    sMonth(i,1)=info(i,6);
    % end time
     eYear(i,1)=info(i,7);
     eMonth(i,1)=info(i,8);
    % duration
    D(i,1)=info(i,9);
    
    % centroid
    temp1=droEvent==i;
    cent=regionprops(temp1,'Centroid');
    centxyz(i,:)=cat(1,cent.Centroid);
    
    % affacted area, km^2
    affArea(i,1)=info(i,10);
    
    %volum, or severity
    % spi*area*mont, month*km^2
    temp2=bwconncomp(temp1,26);
    spaceIdx=temp2.PixelIdxList;
    gridIdx=spaceIdx{1};
    [gridx,gridy,gridz]=ind2sub(size(droVal),gridIdx);
    %val=droVal(temp1);

    for j=1:length(gridx)
        cellarea=areaCalculator(gridx(j));
        spi=droVal(gridx(j),gridy(j),gridz(j));
        volum(i)=volum(i)+cellarea*abs(spi);
    end
    
    % aspect ratio
    
    aRatio(i)=volum(i)/affArea(i);
    
end

droDetails=[sYear,sMonth,eYear,eMonth,D,centxyz,affArea,volum,aRatio];
end