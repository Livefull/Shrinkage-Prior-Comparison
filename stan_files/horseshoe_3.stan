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
	vector[p] z; // regression parameters
	real <lower=0> r1_global;
	real <lower=0>  r2_global;
	vector <lower =0>[p] r1_local;
	vector <lower =0>[p] r2_local;

}
transformed parameters{
	vector[p] beta;
	real<lower=0> sigma; //error sd
	vector[N_train] linpred; //mean normal model
	real<lower=0> tau;
	real<lower=0> ta2;
	vector<lower=0>[p] lambda;
	sigma = sqrt(sigma2);
	lambda = r1_local  .* sqrt(r2_local);
	tau = r1_global * sqrt(r2_global );
	ta2 = tau*tau;
	beta = z .*lambda;
	linpred = mu + X_train*beta;
}
model{
	
	// Gaussian prior for intercept
	mu ~ normal(0, 10); 
	z ~ normal(0, 1); 

	// half-t priors for lambdas
	r1_local ~ normal (0.0,  1);
	r2_local ~ inv_gamma (0.5, 0.5*ta2);

	// half t-prior for tau
	r1_global ~ normal (0.0,  1);
	r2_global ~ inv_gamma (0.5, 0.5*sigma2);
	
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
		
	
	
	
