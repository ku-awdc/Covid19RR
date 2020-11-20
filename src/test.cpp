#include <TMB.hpp>

// Symmetric unbiased hyperbolic distribution in 1D, parametrized by shape parameter
// zeta>0 and scale parameter delta>0. If zeta is near 0, this is approximately a Gaussian
// with variance delta^2/zeta. If zeta is large, this is approximately a two-sided
// exponential with rate zeta/delta.
template<class Type> Type dhyperbolic(Type x,Type delta, Type zeta,int give_log=0)
{
  Type ld = -zeta*sqrt(pow(x/delta,2) + 1.0); // Log unnormalized density
  Type lc = log(2*delta)  + log(besselK(zeta,Type(1.0)));

  if(!give_log) return exp(ld - lc) ;
  else return ld - lc;
}

template<class Type>
Type objective_function<Type>::operator() ()
{
  DATA_VECTOR(nTests);
  DATA_VECTOR(nPos);
  DATA_INTEGER(modelswitch);
  DATA_SCALAR(RefTests);
  
  PARAMETER_VECTOR(logI);
  PARAMETER_VECTOR(r);

  PARAMETER(beta);
  PARAMETER(logIsigma);    Type Isigma    = exp(logIsigma);
  PARAMETER(logtau);       Type rsigma    = exp(logIsigma - logtau);
  PARAMETER(logrzeta);     Type rzeta     = exp(logrzeta);

  int nT = nTests.size();

  vector<Type> Epos(nT);    // Expected positives
  vector<Type> logCorrPos(nT); // Expected Corrected positives to RefTests  

  Type ans = 0;

  for(int i=0; i<nT ; ++i)
    {
      Epos(i) = exp(logI(i)+beta*log(nTests(i))); 
      ans -= dpois(nPos(i),Epos(i),1);
    }

  for(int i = 1; i<nT ; ++i)
    {
      ans -= dnorm(logI(i)-logI(i-1)-r(i-1),Type(0),Isigma,1);  
    }

  for(int i = 2; i<nT ; ++i)
    {
      switch(modelswitch)
	{
	case 1:
	  ans -= dhyperbolic(r(i-1)-r(i-2),rsigma,rzeta,1);
	  break;
	case 2:
	  ans -= dnorm(r(i-1)-r(i-2),Type(0),rsigma,1);
	  ans += pow(logrzeta,2);
	  break;
	}
    }

  return(ans);
}
