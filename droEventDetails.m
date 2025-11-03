% 2014/02/24

% calculate centoid, area, volume, for each drought event, for each time step

load('droEvent_historical_SSP585_1979-2100.mat');
load('droVal_historical_SSP585_1979-2100.mat');
droInfo_his_585 = xlsread('Drought_information_historical_SSP585_1979-2100.xlsx');

[row,col,pag]=size(droEvent_his_585);
droNum=length(droInfo_his_585);
droVal = droVal_his_585;
droEvent = droEvent_his_585;

% area: ara, centroid: ctd, volum:vlm

eventDetails=cell(droNum,1);

for i=1:droNum
    disp(i);
    evenNum=droEvent==i;
    evenVal=droVal(droEvent==droNum);
    indd=find(evenNum);
    [rr,cc,pp]=ind2sub(size(evenNum),indd);
    this_t=min(pp):max(pp);
    len=length(this_t);
    this_dro=nan(len,5);
    this_dro(:,1)=this_t';
    
    for t=1:len
          p=this_t(t);
        %% centroid
        this_layer=evenNum(:,:,p);
        c1=bwconncomp(this_layer,8);
        c2=labelmatrix(c1);
        c2(c2>=1)=1;
        c3=regionprops(c2,'Centroid');
        ctd=c3.Centroid;
        this_dro(t,2)=ctd(1);
        this_dro(t,3)=ctd(2);
        %% area and volum
        ara=0;
        vlm=0;
        for r=min(rr):max(rr)
            cellArea=areaCalculator(r);
            for c=min(cc):max(cc)
                if evenNum(r,c,p)==1
                    ara=ara+cellArea;
                    vlm=vlm+cellArea*abs(droVal(r,c,p));
                end
            end
        end
        this_dro(t,4)=ara;
        this_dro(t,5)=vlm;
    end
    
    eventDetails{i,1}=this_dro;
end
event_Details_SSP585 = eventDetails;
    





