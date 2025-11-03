CAFEC = function(P, PET, AWC=NA, start=NA, end=NA, 
                 cal_start=NA, cal_end=NA){
  # P, monthly precipitation/mm
  # PET, monthly potential evopotranspiration/mm
  # AWC, soil available water capicity/mm
  # CAFEC, monthly climaticaly appropriate precipitation/mm
  if (is.na(start)){
    start = 1
  }
  if (is.na(end)){
    end = length(P)/12
  }
  if (is.na(cal_start)){
    cal_start = 1
  }
  if (is.na(cal_end)){
    cal_end = length(P)/12
  }
  P = matrix(P, ncol = 12, byrow = T)
  PET = matrix(PET, ncol = 12, byrow = T)
  
  # if(is.na(AWC) | AWC==0){
    AWC = 152.4
    AWCs = 25.4
    AWCu = 127
  # } else {
  #   AWCs = 0.167*AWC
  #   AWCu = AWC - AWCs
  # }
  
  yr = end - start + 1
  Ss=matrix(0, nrow=yr+1,ncol=12)
  Su=matrix(0, nrow=yr+1,ncol=12)
  L=matrix(0, nrow=yr,ncol=12)
  Ls=matrix(0, nrow=yr,ncol=12)
  Lu=matrix(0, nrow=yr,ncol=12)
  RO=matrix(0, nrow=yr,ncol=12)
  R=matrix(0, nrow=yr,ncol=12)
  ET=matrix(0, nrow=yr,ncol=12)
  PLs=matrix(0, nrow=yr,ncol=12)
  PLu=matrix(0, nrow=yr,ncol=12)
  PL=matrix(0, nrow=yr,ncol=12)
  PR=matrix(0, nrow=yr,ncol=12)
  PRO=matrix(0, nrow=yr,ncol=12)
  DSs = matrix(0, nrow=yr,ncol=12)
  DSu = matrix(0, nrow=yr,ncol=12)
  
  Ss[1,1] = AWCs
  Su[1,1] = AWCu
  
  for(ty in 1:yr){
    for(tm in 1:12){
      if(PET[ty,tm] >= P[ty,tm]){
        Ls[ty,tm] = min(Ss[ty,tm], PET[ty,tm]-P[ty,tm])
        if(Ls[ty,tm]==Ss[ty,tm]){
          Lu[ty,tm]=(PET[ty,tm]-P[ty,tm]-Ls[ty,tm])*Su[ty,tm]/AWC
        }
        else{
          Lu[ty,tm]=0
        }
        L[ty,tm]=Ls[ty,tm]+Lu[ty,tm]
        if(tm!=12){
          Ss[ty,tm+1]=Ss[ty,tm]-Ls[ty,tm]
          Su[ty,tm+1]=Su[ty,tm]-Lu[ty,tm]
        }
        else{
          Ss[ty+1,1]=Ss[ty,tm]-Ls[ty,tm]
          Su[ty+1,1]=Su[ty,tm]-Lu[ty,tm]
        }
        R[ty,tm]=0
        ET[ty,tm]=P[ty,tm]+L[ty,tm]
        RO[ty,tm]=0
      } else {
        DSs[ty,tm]=min(AWCs-Ss[ty,tm],P[ty,tm]-PET[ty,tm])
        if(DSs[ty,tm]==AWCs-Ss[ty,tm]){
          DSu[ty,tm]=min(P[ty,tm]-PET[ty,tm]-DSs[ty,tm],AWCu-Su[ty,tm])
        }
        else{
          DSu[ty,tm]=0
        }
        R[ty,tm]=DSs[ty,tm]+DSu[ty,tm]
        if(tm!=12){
          Ss[ty,tm+1]=Ss[ty,tm]+DSs[ty,tm]
          Su[ty,tm+1]=Su[ty,tm]+DSu[ty,tm]
        }
        else{
          Ss[ty+1,1]=Ss[ty,tm]+DSs[ty,tm]
          Su[ty+1,1]=Su[ty,tm]+DSu[ty,tm]
        }
        L[ty,tm]=0
        ET[ty,tm]=PET[ty,tm]
        RO[ty,tm]=P[ty,tm]-PET[ty,tm]-R[ty,tm]
      }
      PLs[ty,tm]=min(PET[ty,tm],Ss[ty,tm])
      PLu[ty,tm]=(PET[ty,tm]-PLs[ty,tm])*Su[ty,tm]/AWC
      PL[ty,tm]=PLs[ty,tm]+PLu[ty,tm]
      PR[ty,tm]=AWC-(Ss[ty,tm]+Su[ty,tm])
      PRO[ty,tm]=AWC-PR[ty,tm]
    }
  }
  
  #-------- calculate weighting factors 
  cal_period = (cal_start:cal_end) - start + 1
  PET_cal = PET[cal_period, ]
  PRO_cal = PRO[cal_period, ]
  PR_cal = PR[cal_period, ]
  PL_cal = PL[cal_period, ]
  P_cal = P[cal_period, ]
  ET_cal = ET[cal_period, ]
  R_cal = R[cal_period, ]
  RO_cal = RO[cal_period, ]
  L_cal = L[cal_period, ]
  
  CET=c()
  CR=c()
  CRO=c()
  CL=c()
  
  for(tm in 1:12){
    if(mean(PET_cal[,tm])!=0){
      CET[tm] = mean(ET_cal[,tm])/mean(PET_cal[,tm])
    }
    else if(mean(ET_cal[,tm])==0){
      CET[tm]=1
    }
    else{
      CET[tm]=0
    }
    if(!is.na(sum(PR_cal[,tm])) & mean(PR_cal[,tm])!=0){
      CR[tm]=mean(R_cal[,tm])/mean(PR_cal[,tm])
    }
    else if(!is.na(sum(PR_cal[,tm])) & mean(R_cal[,tm])==0){
      CR[tm]=1
    }
    else{
      CR[tm]=0
    }
    if(!is.na(sum(PRO_cal[,tm])) & mean(PRO_cal[,tm])!=0){
      CRO[tm]=mean(RO_cal[,tm])/mean(PRO_cal[,tm])
    }
    else if(!is.na(sum(PRO_cal[,tm])) & mean(RO_cal[,tm])==0){
      CRO[tm]=1
    }
    else{
      CRO[tm]=0
    }
    if(!is.na(sum(PL_cal[,tm])) & mean(PL_cal[,tm])!=0){
      CL[tm]=mean(L_cal[,tm])/mean(PL_cal[,tm])
    }
    else if(!is.na(sum(PL_cal[,tm])) & mean(L_cal[,tm])==0){
      CL[tm]=1
    }
    else{
      CL[tm]=0
    }
  }
  #---------- calculate moisture departure 'd'
  P_CAFEC=matrix(NA, nrow = yr, ncol = 12)
  for (ty in 1:yr){
    for (tm in 1:12){
      P_CAFEC[ty,tm]=CET[tm]*PET[ty,tm]+CR[tm]*PR[ty,tm]+CRO[tm]*PRO[ty,tm]-CL[tm]*PL[ty,tm]
    }
  }
  
  P_CAFEC = matrix(t(P_CAFEC), ncol = 1)
  ET = matrix(t(ET), ncol = 1)
  R = matrix(t(R), ncol = 1)
  PR = matrix(t(PR), ncol = 1)
  RO = matrix(t(RO), ncol = 1)
  PRO = matrix(t(PRO), ncol = 1)
  L = matrix(t(L), ncol = 1)
  PL = matrix(t(PL), ncol = 1)
  
  ET[ET<0]=0
  R[R<0]=0
  PR[PR<0]=0
  RO[RO<0]=0
  PRO[PRO<0]=0
  L[L<0]=0
  PL[PL<0]=0
  
  result = list(P_CAFEC, ET, R, PR, RO, PRO, L, PL)
  names(result) = c('P_CAFEC','ET','R','PR','RO','PRO','L','PL')
  return(result)
}


