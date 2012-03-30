function [bols,varOls,varNwest,adjr2,var_e]=myols(x,y)
% function [bols,varOls,varNwest,adjr2,var_e]=myols(x,y)

[T k]=size(x);
bols=x\y;
e=y-x*bols;
Q=inv(x'*x);
lag=floor(4*((T/100)^(2/9)));
v=e.^2;
v=x'*(x.*v(:,ones(k,1)));
for i=1:lag
   v1=e(1+i:T).*e(1:T-i);
   v1=x(1+i:T,:)'*(x(1:T-i,:).*v1(:,ones(k,1)));
   v=(1-i/(lag+1))*(v1+v1') + v;
end
varNwest=Q*v*Q*T/(T-k); 
varOls=((e'*e)/(T-k))*Q;

r2=1-var(e)/var(y);
adjr2=(1-k)/(T-k)+(T-1)*r2/(T-k);
var_e=(e'*e)/(T-k);
return;