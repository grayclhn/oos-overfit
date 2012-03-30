function [mae,rmse,adjr2,fpval1,fpval2,dmae,drmse,adjr2B,dmaeB,drmseB]=InSampleFun(N,R,X,dBeg,dEnd,fBeg)
% function [mae,rmse,adjr2,fpval1,fpval2,dmae,drmse,adjr2B,dmaeB,drmseB]=InSampleFun(N,R,X,dBeg,dEnd,fBeg)
% -------------------------------------------------------------------------
% Inputs
% N is the regression lag-1 for next period prediction, more for multi-year
% dBeg, dEnd, and fBeg should be numbers like 50, 89, and 123
% -------------------------------------------------------------------------
% Outputs
% mae and rmse are computed for the sample period fBeg:dEnd (not dBeg:dEnd)
% adjr2 is the r2 for the actual sample dBeg:dEnd
% fpval1 is computed based on OLS errors 
% fpval2 is computed based on NW errors 
% dmae is Delta MAE (B is for OOS period)
% drmse is Delta RMSE (B is for OOS period)
% -------------------------------------------------------------------------

y=R(dBeg+N:dEnd);
T=length(y);
x=[ones(T,1) X(dBeg:dEnd-N,:)];
[T k]=size(x);

[bols,varOls,varNwest,adjr2,var_e]=myols(x,y);
eols=y-x*bols;
bols=bols(2:end);
varOls=varOls(2:end,2:end);
varNwest=varNwest(2:end,2:end);
fstat1=bols'*inv(varOls)*bols/(k-1); % F(K-1,T-K) large values reject the null that all betas are zero
fstat2=bols'*inv(varNwest)*bols/(k-1); % F(K-1,T-K) large values reject the null that all betas are zero
fpval1=1-fcdf(fstat1,k-1,T-k);
fpval2=1-fcdf(fstat2,k-1,T-k);

y=R(dBeg+N:dEnd);
eunc=y-mean(y);

% in-sample period
ee=eols;
rmse=mean(ee.^2).^.5;
mae=mean(abs(ee));

ee=eunc;
maeuncond=mean(abs(ee));
rmseuncond=mean(ee.^2).^.5;

dmae=maeuncond-mae;
drmse=rmseuncond-rmse;

% oos-sample period
ee=eols(fBeg-dBeg:end);
rmse=mean(ee.^2).^.5;
mae=mean(abs(ee));

ee=eunc(fBeg-dBeg:end);
maeuncond=mean(abs(ee));
rmseuncond=mean(ee.^2).^.5;

dmaeB=maeuncond-mae;
drmseB=rmseuncond-rmse;

r2B=1-var(eols(fBeg-dBeg:end))/ var(eunc(fBeg-dBeg:end));
TforOOS=length(ee);
adjr2B=(1-k)/(TforOOS-k)+(TforOOS-1)*r2B/(TforOOS-k);
return;