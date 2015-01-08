#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
	DATA_VECTOR(temp); //lenght=nobs
	DATA_FACTOR(site); //length=nobs
	DATA_FACTOR(year); //length=nobs
	DATA_MATRIX(X0); //nobs x predictor
	DATA_MATRIX(Xsite); //nobs x npred_site
	DATA_MATRIX(Xyear); //nobs x npred_year
	
	PARAMETER(log_sigma);
	PARAMETER_VECTOR(site_pred_log_sd);
	PARAMETER_VECTOR(year_pred_log_sd);
	PARAMETER_VECTOR(B0); //predictor
	PARAMETER_MATRIX(Bsite); //site x npred_site
	PARAMETER_MATRIX(Byear); //year x npred_year
	Type jnll=0;
	
	int nobs=temp.size();
	int nsite=Bsite.rows();
	int nyear=Byear.rows();
	int npred_site=Bsite.cols();
	int npred_year=Byear.cols();
	
	//Get sd on correct scale
	Type sigma = exp(log_sigma);
	vector<Type> year_pred_sd=exp(year_pred_log_sd);
	vector<Type> site_pred_sd=exp(site_pred_log_sd);
	vector<Type> XB0= X0*B0; //each elem is for one obs
	vector<Type> mu_stream(nobs);
	ADREPORT(mu_stream);
	
	for(int i=0; i<nobs; i++)
	{
		mu_stream(i)=XB0(i);
		//mu_stream(i)+=(Bsite.row(site(i))*Xsite.row(i)).sum().vector();
		//mu_stream(i)+=(Byear.row(year(i))*Xyear.row(i)).sum().vector(); //REyear
    for(int j=0; j<npred_site; j++){
      mu_stream(i)+=(Bsite(site(i),j)*Xsite(i,j));
    }	
    for(int j=0; j<npred_year; j++){
    	mu_stream(i)+=(Byear(year(i),j)*Xyear(i,j)); //REyear
    }	
		jnll+= -dnorm(temp(i), mu_stream(i), sigma, true);
	}	
	for(int i=0; i<nsite; i++)
	{
		for(int j=0; j<npred_site; j++)
		{
			jnll+= -dnorm(Bsite(i,j), Type(0), site_pred_sd(j), true);
		}
	}
	for(int i=0; i<nyear; i++)
	{
		for(int j=0; j<npred_year; j++)
		{
			jnll+= -dnorm(Byear(i,j), Type(0), year_pred_sd(j), true);
		}
	}
	return jnll;
}
