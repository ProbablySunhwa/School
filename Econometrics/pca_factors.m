%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Jurado et al. implements PCA using the methodology provided by Bai and Ng's
%  2002 Econometrica publication "Determining the Number of Factors
%  in Approximate Factor Models".  Based on information from the
%  publication, Jurado is using the ICp2 information criteria from Bai and Ng's publication
%  to determine the PCA factors.
%  In order to construct this code, I read through Bai and Ng's publication
%  and made use of code available from Serena Ng's webpage and other online
%  sources
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function fhat = pca_factors(X,kmax)

%Establish information criteria from Bai and Ng (2002); Note, Jurado Publication uses ICp2
T=size(X,1);
N=size(X,2);
Cnt=zeros(1,kmax);
ii=1:1:kmax;
Cnt(1,:)=((N+T)/(N*T))*log(min([N;T]))*ii;

%The authors then standardize the X matrix
mean_X=repmat(mean(X),T,1);
std_X=repmat(std(X),T,1);
X_standardized=(X-mean_X)./std_X;

%Estimation of the common factors via Bia and Ng (2002)
IC=zeros(size(Cnt,1),kmax+1);
Sigma=zeros(1,kmax+1);
[ev,eigval,ev1]=svd(X_standardized'*X_standardized);
Lambda0=sqrt(N)*ev;
Fhat0=X_standardized*Lambda0/N;

for i=kmax:-1:1;
Fhat=Fhat0(:,1:i);
lambda=Lambda0(:,1:i);
chat=Fhat*lambda';
ehat=X_standardized-chat;
Sigma(i)=mean(sum(ehat.*ehat/T));
IC(:,i)=log(Sigma(i))+Cnt(:,i);
end;
Sigma(kmax+1)=mean(sum(X_standardized.*X_standardized/T));
IC(:,kmax+1)=log(Sigma(kmax+1));
ic=minindc(IC')';
ic=ic .*(ic <= kmax);

%Complete PCA
[bigt,bign]=size(X_standardized);
[Fhat0,eigval,Fhat1]=svd(X_standardized'*X_standardized);
lambda=Fhat0(:,1:ic)*sqrt(bign);
fhat=X_standardized*lambda/bign;

%Additional functions taken from Serena Ng's webpage
function pos=minindc(x)
ncols=size(x,2);
nrows=size(x,1);
pos=zeros(ncols,1);
seq=seqa(1,1,nrows);
for i=1:ncols;
dum=min(x(:,i));
dum1= seq .* ( (x(:,i)-dum) ==0);
pos(i)=sum(dum1);
end;
end

function seq=seqa(a,b,c)
seq=(a:b:(a+b*(c-1)))';
end

end