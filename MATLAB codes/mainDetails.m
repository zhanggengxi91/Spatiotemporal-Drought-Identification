load('spei_05_165_svvvv.mat');

load('droEvent_historical_SSP245_1979-2100.mat')
load('droVal_historical_SSP245_1979-2100.mat')
droInfo_his_245 = xlsread('Drought_information_historical_SSP245_1979-2100.xlsx');
droDetails=droCharacter(droEvent_his_245,droVal_his_245,droInfo_his_245);