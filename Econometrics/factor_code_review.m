[n m] = size(factornominal);
means = mean(factornominal);
X = factornominal - repmat(means,n,1);
S = (X'*X)/(n-1); % this is the covariance matrix
% V are eigenvectors, D are eigenvalues
[V,L] = eig(S); % V is in reverse order of SE (ie SE goes 1->4 is equiv to 4->1 here)
[e,i]=sort(diag(L),'descend'); % need order it so same!
V_sorted = V(:,i); % ah so row names stay same

% compute loadings A, A = V_sorted_i * eigenvalue of PC_i (i in columns)
% then choose only first 2 PCs
sds = sqrt(e); % standard deviation
A = V_sorted.*sds';
A = A(:,1:3);

% standardize, standard = A divided by its own standard deviation
standard = A./sqrt(diag(S));

% regression coefficients, B = A(diag(1/L)) = inv(S)*A
B = inv(S)*A;

% standardized component scores (have variance = 1) = X*B
C = X*B;

% raw component scores (have variance = eigenvalues) = X*V
R = X*V;



[