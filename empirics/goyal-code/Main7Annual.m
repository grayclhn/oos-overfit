clear;clc;pack;
DoModelSel=0;
load Data_Annual;
r=[NaN; (Index(2:end)+D12(2:end))./Index(1:end-1)-1]; % simple return
R1=log(1+r)-log(1+Rfree); % continuosly compounded
R3=filter(ones(3,1),1,R1); R5=filter(ones(5,1),1,R1);
R=cell(5,1); R{1}=R1; R{3}=R3; R{5}=R5;

load cay_a cayall; CAYALL=cayall;

adata=cell(19,1); XBEG=zeros(19,1); XEND=zeros(19,1);
i=0;
i=i+1; adata{i,1}=dp;     XBEG(i)=1872; XEND(i)=2009;
i=i+1; adata{i,1}=dy;     XBEG(i)=1872; XEND(i)=2009;
i=i+1; adata{i,1}=ep;     XBEG(i)=1872; XEND(i)=2009;
i=i+1; adata{i,1}=de;     XBEG(i)=1872;	XEND(i)=2009;
i=i+1; adata{i,1}=svar;   XBEG(i)=1885;	XEND(i)=2009;
i=i+1; adata{i,1}=btom;   XBEG(i)=1921;	XEND(i)=2009;
i=i+1; adata{i,1}=ntis;   XBEG(i)=1927;	XEND(i)=2009;
i=i+1; adata{i,1}=eqis;   XBEG(i)=1927;	XEND(i)=2009;
i=i+1; adata{i,1}=tbl;    XBEG(i)=1920;	XEND(i)=2009;
i=i+1; adata{i,1}=lty;    XBEG(i)=1919;	XEND(i)=2009;
i=i+1; adata{i,1}=ltr;    XBEG(i)=1926;	XEND(i)=2009;
i=i+1; adata{i,1}=tms;    XBEG(i)=1920;	XEND(i)=2009;
i=i+1; adata{i,1}=dfy;    XBEG(i)=1919;	XEND(i)=2009;
i=i+1; adata{i,1}=dfr;    XBEG(i)=1926;	XEND(i)=2009;
i=i+1; adata{i,1}=infl;   XBEG(i)=1919;	XEND(i)=2009;
i=i+1; adata{i,1}=ik;     XBEG(i)=1947;	XEND(i)=2009;
i=i+1; adata{i,1}=cay;    XBEG(i)=1945;	XEND(i)=2009;
i=i+1; adata{i,1}=cay;    XBEG(i)=1945;	XEND(i)=2009; % This is reall re-estimated cay
i=i+1; adata{i,1}=[dp ep svar btom ntis eqis tbl lty ltr dfr dfy infl];  XBEG(i)=1927;XEND(i)=2009;

NN=[1 3 5];
PANELS=3; K=size(adata,1);

dumm=repmat(NaN,[K+1 PANELS length(NN)]);
DBEG=dumm; DEND=dumm; FBEG=dumm;

DBEG(1:K,1:2,:)=XBEG(:,[1 1],[1 1 1]); DBEG(:,3,:)=1927; DBEG(K-3,3,:)=1947; DBEG(K-2:K-1,3,:)=1945;
DEND(1:K,1:2,:)=XEND(:,[1 1],[1 1 1]); DEND(:,3,:)=2009;
FBEG(1:K,1,:)=DBEG(1:K,1,:)+20; FBEG(1:K,2:3,:)=1965; FBEG(K-3:K-1,2:3,:)=DBEG(K-3:K-1,2:3,:)+20;
for n=1:length(NN)
    for panel=1:3
        DBEG(end,panel,n)=DBEG(end-1,panel,n);
        DEND(end,panel,n)=DEND(end-1,panel,n);
        FBEG(end,panel,n)=FBEG(end-1,panel,n);
    end
end

IDBEG=dumm; IDEND=dumm; IFBEG=dumm;
for i=1:K+1
    for panel=1:3
        for n=1:length(NN)
            IDBEG(i,panel,n)=find(yyyy==DBEG(i,panel,n));
            IDEND(i,panel,n)=find(yyyy==DEND(i,panel,n));
            IFBEG(i,panel,n)=find(yyyy==FBEG(i,panel,n));
        end
    end
end

ISMAE=dumm; ISRMSE=dumm; ISDMAE=dumm; ISDRMSE=dumm; ISAR2=dumm; ISFPVAL=dumm;
ISOOSAR2=dumm; ISOOSDMAE=dumm; ISOOSDRMSE=dumm;
OOSMean=dumm; OOSStd=dumm; OOSMAE=dumm; OOSRMSE=dumm; OOSDMAE=dumm; OOSDRMSE=dumm; 
OOSMSET=dumm; OOSMSEF=dumm; OOSENCNEW=dumm; OOSLambda=dumm;
OOSC90T=dumm; OOSC90F=dumm; OOSC90N=dumm;
OOSC95T=dumm; OOSC95F=dumm; OOSC95N=dumm;
OOSC99T=dumm; OOSC99F=dumm; OOSC99N=dumm;
OOSDRMSEopt=dumm;  OOSDRMSEoptRolling=dumm; 
OOSAR2=dumm;

for n=1:3
    N=NN(n);
    for i=1:K
        IncludeCAY=0;
        disp(i);
        if i==(K-1) 
            IncludeCAY=1;
        end
        X=adata{i,1};
        for panel=1:3
            [InSample,OutSample1,OutSample2,Eunc,Eols]=OneSet(N,R{N},X,IDBEG(i,panel,n),IDEND(i,panel,n),IFBEG(i,panel,n),IncludeCAY,CAYALL);
            
            ISMAE(i,panel,n)=InSample.MAE;  ISRMSE(i,panel,n)=InSample.RMSE; 
            ISDMAE(i,panel,n)=InSample.DMAE; ISDRMSE(i,panel,n)=InSample.DRMSE;            
            ISAR2(i,panel,n)=InSample.AdjR2;ISFPVAL(i,panel,n)=InSample.fpval;
            ISOOSDMAE(i,panel,n)=InSample.DMAEB; ISOOSDRMSE(i,panel,n)=InSample.DRMSEB;
            ISOOSAR2(i,panel,n)=InSample.AdjR2B;

            OOSMean(i,panel,n)=OutSample1.Mean; OOSStd(i,panel,n)=OutSample1.Std; 
            OOSMAE(i,panel,n)=OutSample1.MAE; OOSRMSE(i,panel,n)=OutSample1.RMSE; 
            OOSDMAE(i,panel,n)=OutSample1.DMAE; OOSDRMSE(i,panel,n)=OutSample1.DRMSE;
            OOSDRMSEopt(i,panel,n)=OutSample1.DRMSEopt;
            OOSDRMSEoptRolling(i,panel,n)=OutSample1.DRMSEoptroll;
            OOSAR2(i,panel,n)=OutSample1.AdjR2;
            
            OOSMSET(i,panel,n)=OutSample2.MSE_T; OOSMSEF(i,panel,n)=OutSample2.MSE_F;
            OOSENCNEW(i,panel,n)=OutSample2.ENC_NEW; OOSLambda(i,panel,n)=OutSample2.LAMB;

            OOSC90T(i,panel,n)=OutSample2.Crt_T(1);  OOSC90F(i,panel,n)=OutSample2.Crt_F(1);  OOSC90N(i,panel,n)=OutSample2.Crt_N(1); 
            OOSC95T(i,panel,n)=OutSample2.Crt_T(2);  OOSC95F(i,panel,n)=OutSample2.Crt_F(2);  OOSC95N(i,panel,n)=OutSample2.Crt_N(2);         
            OOSC99T(i,panel,n)=OutSample2.Crt_T(3);  OOSC99F(i,panel,n)=OutSample2.Crt_F(3);  OOSC99N(i,panel,n)=OutSample2.Crt_N(3); 
        end
    end
end

if DoModelSel==1
    EuncMS=repmat(NaN,[length(R1) 3 3]);
    EolsMS=repmat(NaN,[length(R1) 3 3]);
    
    X=adata{K,1};
    KK=size(X,2);
    M=2^KK;
    XALL=cell(M-1,1);
    for m=1:M-1
        select_vars=dec2bin(m);
        l=length(select_vars);
        if l<KK for i=1:KK-l select_vars=strcat('0',select_vars); end; end;
        XX=[];
        for i=KK:-1:1 if select_vars(i)=='1' XX=[XX X(:,KK-i+1)]; end; end;
        XALL{m}=XX;
    end
    i=K+1;
    
    for n=1:3
        N=NN(n);
        IncludeCAY=0;
        panel=1;
        [InSample,OutSample1,OutSample2,Eunc,Eols]=...
            OneSet_ModSel(N,R{N},XALL,IDBEG(K+1,panel,n),IDEND(K+1,panel,n),IFBEG(K+1,panel,n),IncludeCAY,CAYALL);
        ISMAE(i,panel,n)=InSample.MAE; ISRMSE(i,panel,n)=InSample.RMSE; ISAR2(i,panel,n)=InSample.AdjR2;
        ISFPVAL(i,panel,n)=InSample.fpval; 
        IISDMAE(i,panel,n)=InSample.DMAE1; IISDRMSE(i,panel,n)=InSample.DRMSE1;
        ISDMAE(i,panel,n)=InSample.DMAE2; ISDRMSE(i,panel,n)=InSample.DRMSE2;
        
        OOSMean(i,panel,n)=OutSample1.Mean; OOSStd(i,panel,n)=OutSample1.Std; 
        OOSMAE(i,panel,n)=OutSample1.MAE; OOSRMSE(i,panel,n)=OutSample1.RMSE; 
        OOSDMAE(i,panel,n)=OutSample1.DMAE; OOSDRMSE(i,panel,n)=OutSample1.DRMSE;
        OOSDRMSEopt(i,panel,n)=OutSample1.DRMSEopt;
        OOSDRMSEoptRolling(i,panel,n)=OutSample1.DRMSEoptroll;
        OOSAR2(i,panel,n)=OutSample1.AdjR2;
        
        OOSMSET(i,panel,n)=OutSample2.MSE_T; OOSMSEF(i,panel,n)=OutSample2.MSE_F;
        OOSENCNEW(i,panel,n)=OutSample2.ENC_NEW; OOSLambda(i,panel,n)=OutSample2.LAMB;
        
        OOSC90T(i,panel,n)=OutSample2.Crt_T(1);  OOSC90F(i,panel,n)=OutSample2.Crt_F(1);  OOSC90N(i,panel,n)=OutSample2.Crt_N(1); 
        OOSC95T(i,panel,n)=OutSample2.Crt_T(2);  OOSC95F(i,panel,n)=OutSample2.Crt_F(2);  OOSC95N(i,panel,n)=OutSample2.Crt_N(2);         
        OOSC99T(i,panel,n)=OutSample2.Crt_T(3);  OOSC99F(i,panel,n)=OutSample2.Crt_F(3);  OOSC99N(i,panel,n)=OutSample2.Crt_N(3); 
        EuncMS(IFBEG(K+1,panel,n):IDEND(K+1,panel,n),panel,n)=Eunc;
        EolsMS(IFBEG(K+1,panel,n):IDEND(K+1,panel,n),panel,n)=Eols;
        
        panel=2;
        [InSample,OutSample1,OutSample2,Eunc,Eols]=...
            OneSet_ModSel(N,R{N},XALL,IDBEG(K+1,panel,n),IDEND(K+1,panel,n),IFBEG(K+1,panel,n),IncludeCAY,CAYALL);
        for panel=2:3
            ISMAE(i,panel,n)=InSample.MAE; ISRMSE(i,panel,n)=InSample.RMSE; ISAR2(i,panel,n)=InSample.AdjR2;
            ISFPVAL(i,panel,n)=InSample.fpval;
            IISDMAE(i,panel,n)=InSample.DMAE1; IISDRMSE(i,panel,n)=InSample.DRMSE1;
            ISDMAE(i,panel,n)=InSample.DMAE2; ISDRMSE(i,panel,n)=InSample.DRMSE2;
        
            OOSMean(i,panel,n)=OutSample1.Mean; OOSStd(i,panel,n)=OutSample1.Std; 
            OOSMAE(i,panel,n)=OutSample1.MAE; OOSRMSE(i,panel,n)=OutSample1.RMSE; 
            OOSDMAE(i,panel,n)=OutSample1.DMAE; OOSDRMSE(i,panel,n)=OutSample1.DRMSE;
            OOSDRMSEopt(i,panel,n)=OutSample1.DRMSEopt;            
            OOSDRMSEoptRolling(i,panel,n)=OutSample1.DRMSEoptroll;
            OOSAR2(i,panel,n)=OutSample1.AdjR2;
            
            OOSMSET(i,panel,n)=OutSample2.MSE_T; OOSMSEF(i,panel,n)=OutSample2.MSE_F;
            OOSENCNEW(i,panel,n)=OutSample2.ENC_NEW; OOSLambda(i,panel,n)=OutSample2.LAMB;
            
            OOSC90T(i,panel,n)=OutSample2.Crt_T(1);  OOSC90F(i,panel,n)=OutSample2.Crt_F(1);  OOSC90N(i,panel,n)=OutSample2.Crt_N(1); 
            OOSC95T(i,panel,n)=OutSample2.Crt_T(2);  OOSC95F(i,panel,n)=OutSample2.Crt_F(2);  OOSC95N(i,panel,n)=OutSample2.Crt_N(2);         
            OOSC99T(i,panel,n)=OutSample2.Crt_T(3);  OOSC99F(i,panel,n)=OutSample2.Crt_F(3);  OOSC99N(i,panel,n)=OutSample2.Crt_N(3); 
            EuncMS(IFBEG(K+1,panel,n):IDEND(K+1,panel,n),panel,n)=Eunc;
            EolsMS(IFBEG(K+1,panel,n):IDEND(K+1,panel,n),panel,n)=Eols;
        end
    end
    save ErrMS7 EuncMS EolsMS;
end

ISMAE(K-1,:,:)=NaN;ISRMSE(K-1,:,:)=NaN;ISDMAE(K-1,:,:)=NaN;ISDRMSE(K-1,:,:)=NaN;ISAR2(K-1,:,:)=NaN;ISFPVAL(K-1,:,:)=NaN;
ISOOSDMAE(K-1,:,:)=NaN;ISOOSDRMSE(K-1,:,:)=NaN;ISOOSAR2(K-1,:,:)=NaN;
save Results7 IS* OOS* DBEG DEND FBEG;