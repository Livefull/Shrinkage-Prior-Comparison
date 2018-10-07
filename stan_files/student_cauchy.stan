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
	vector[p] beta_raw; // standard t variables
	//hyperparameters prior
	real<lower=0> prec;
	real<lower=0> lambda; //penalty parameter
}
transformed parameters{
	real<lower=0> sigma; //error sd
	vector[p] beta; // regression parameters
	vector[N_train] linpred; //mean normal model
	beta = sqrt(sigma2/lambda) * prec^(0.5) * beta_raw;
	sigma = sqrt(sigma2);
	linpred = mu + X_train*beta;
}
model{
 //prior regression coefficients: lasso
	mu ~ normal(0, 10);
 	prec ~ gamma(0.5, 0.5);
 	beta_raw ~ normal(0, 1); // implies beta ~ student_t(1, 0, sqrt(sigma2/lambda)
	
	lambda ~ cauchy(0, 1);
	
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
	
	
