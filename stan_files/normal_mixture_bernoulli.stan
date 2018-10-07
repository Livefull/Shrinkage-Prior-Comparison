data{
	int N_train; //number of observations training and validation set
	int p; //number of predictors
	real y_train[N_train]; //response vector
	matrix[N_train, p] X_train; //model matrix
	//test set
	int N_test; //number of observations test set
	matrix[N_test, p] X_test; //model matrix test set
	real y_test[N_test]; // test labels
}
parameters{
	real mu; //intercept
	real<lower=0> sigma2; //error variance
	vector[p] beta; // regression parameters
	//hyperparameters prior
	vector<lower=0>[p] tau2; //variance slab
}
transformed parameters{
	real<lower=0> sigma; //error sd
	vector[N_train] linpred; //mean normal model
	sigma = sqrt(sigma2);
	linpred = mu + X_train*beta;
}
model{
 //prior regression coefficients: normal mixture
 tau2 ~ inv_gamma(0.5, 0.5);
 
 for(j in 1:p){
 	target += log_sum_exp(bernoulli_lpmf(0 | 0.5) + normal_lpdf(beta[j] | 0, sqrt(0.001)), 
 						  bernoulli_lpmf(1 | 0.5) + normal_lpdf(beta[j] | 0, sqrt(tau2[j])));
 }		
	
	
 //priors nuisance parameters: uniform on log(sigma^2) & mu
	target += -2 * log(sigma); 
	
 //likelihood
	y_train ~ normal(linpred, sigma);
}
generated quantities{ //predict responses test set
	vector[N_test] y_pred; //predicted responses
	vector[N_test] test_log_lik;
	
	y_pred = mu + X_test* beta;
	for (n in 1:N_test) test_log_lik[n] = normal_lpdf(y_test[n] | mu + X_test[n, ] * beta, sigma);
	

	
}		
		
