function [Eunc,Eols]=Forecast(N,R,X,dBeg,dEnd,fBeg,IncludeCAY,CAYALL)
% function [Eunc,Eols]=Forecast(N,R,X,dBeg,dEnd,fBeg)
% -------------------------------------------------------------------------
% Inputs
% N is the regression lag-1 for next period prediction, more for multi-year
% dBeg, dEnd, and fBeg should be numbers like 50, 89, and 123
% IncludeCAY==1 is cay is to re-estimated
% CAYALL is the re-estimated cay
% -------------------------------------------------------------------------
% Outputs
% Eunc and Eols are computed for the sample period fBeg:dEnd (not dBeg:dEnd)
% -------------------------------------------------------------------------

[T k]=size(X);
Runc=R; Rols=R;
for t=fBeg-N:dEnd-N
    y=R(dBeg+N:t);
    T=length(y);
    x=[ones(T,1) X(dBeg:t-N,:)];
    xx=[1 X(t,:)];  
    if IncludeCAY==1
        x=[x(:,1:end-1) CAYALL(dBeg:t-N,t)];
        xx=[xx(1:end-1) CAYALL(t,t)];
    end
    bols=x\y;
    bunc=mean(R(dBeg+N:t));
    Rols(t+N)=xx*bols;
    Runc(t+N)=bunc;
end
Eunc=Runc(fBeg:dEnd)-R(fBeg:dEnd);
Eols=Rols(fBeg:dEnd)-R(fBeg:dEnd);
return;