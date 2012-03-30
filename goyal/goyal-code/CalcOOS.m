function OOS=CalcOOS(N,Eunc,Eols,K,dBeg,dEnd,fBeg)
% function OOS=CalcOOS(Eunc,Eols,dBeg,dEnd,fBeg)
% -------------------------------------------------------------------------
% Inputs
% N is the regression lag-1 for next period prediction, more for multi-year
% Eunc and Eols are errors
% dBeg, dEnd, and fBeg should be numbers like 50, 89, and 123
% K is the number of variables in X
% -------------------------------------------------------------------------
% Outputs
% OOS is output
% -------------------------------------------------------------------------

load CrtVal; % Diebold Marianno
E1=Eunc; E2=Eols;
E1sq=E1.^2; E2sq=E2.^2;
R=fBeg-dBeg; P=dEnd-fBeg+1; pi=P/R;

d=E1sq-E2sq;
dbar=mean(d);
Q2=0;
for ii=-(N-1):(N-1)
    absii=abs(ii);
    Q2=Q2+(N-absii)/N*sum((d(absii+1:end)-dbar).*(d(1:end-absii)-dbar));
end
Q2=Q2/P;
OOS_T=sqrt(P+1-2*N+N*(N-1)/P)*dbar/sqrt(Q2);
%OOS_T=sqrt(P-N+1)*dbar/sqrt(Q2);
OOS_F=(P-N+1)*(mean(E1sq)-mean(E2sq))/mean(E2sq);
ENC_NEW=(P-N+1)*(mean(E1sq)-mean(E1.*E2))/mean(E2sq);
lambd=(Eunc-Eols)\Eunc;

CrtVals=[interp1(Crtpi,OOS_T_90(K,:),pi) ...
        interp1(Crtpi,OOS_T_95(K,:),pi)  ...
        interp1(Crtpi,OOS_T_99(K,:),pi)  ...
        interp1(Crtpi,OOS_F_90(K,:),pi) ...
        interp1(Crtpi,OOS_F_95(K,:),pi) ...
        interp1(Crtpi,OOS_F_99(K,:),pi) ...
        interp1(Crtpi,ENC_NEW_90(K,:),pi) ...
        interp1(Crtpi,ENC_NEW_95(K,:),pi) ...
        interp1(Crtpi,ENC_NEW_99(K,:),pi)];
OOS=[OOS_T OOS_F ENC_NEW lambd CrtVals];
return;