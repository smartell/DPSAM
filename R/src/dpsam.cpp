#include <Rcpp.h>
#include <Rcpp/Benchmark/Timer.h>
using namespace Rcpp; 

 

 /**
  * @brief Class stock
  * @details The basic stock class that is used for Stock Reduction Analysis
  * 
  * @param _stock Is a list containing 3 lists 
  * (1: stockPars, 2: fisheryData, 3: surveyData)
  * 
  */
 class stock
 {
 private:
    List m_stock;

    int m_nage;

    double m   ;
    double linf;
    double k   ;
    double a   ;
    double b   ;
    double ah  ;
    double gh  ;
    double phie;
    double fmsy;
    double msy;
    double sel50;
    double sel95;
    

    NumericVector m_age;
    

 public:
     stock(List _stock)
     :m_stock(_stock) 
     {
     	calcAgeSchedule(m_stock);
      calcSteepnessBo();
     	// Rcpp::Rcout<<as<double>(m_stock["phie"])<<std::endl;
     	Rcpp::Rcout<<phie<<std::endl;
     }

     
    List get_stock()       { return m_stock; }
    void set_stock(List x_){ m_stock = x_;   }

    void calcAgeSchedule(List &stock);
    void calcSteepnessBo();
     
 };

 void stock::calcAgeSchedule(List &stock)
 {
 	  m 	  = as<double>(stock["m"]);
 	  linf  = as<double>(m_stock["linf"]);
    k     = as<double>(m_stock["k"]);
    a     = as<double>(m_stock["a"]);
    b     = as<double>(m_stock["b"]);
    ah    = as<double>(m_stock["ah"]);
    gh    = as<double>(m_stock["gh"]);
    fmsy  = as<double>(m_stock["fmsy"]);
    msy   = as<double>(m_stock["msy"]);
    sel50 = as<double>(m_stock["sel50"]);
    sel95 = as<double>(m_stock["sel95"]);
    m_age = as<NumericVector>(m_stock["age"]);

    m_nage  = m_age.size();
    Rcpp::Rcout<<m_nage<<std::endl;
    NumericVector la( m_nage );
    NumericVector wa( m_nage );
    NumericVector fa( m_nage );
    NumericVector va( m_nage );
    NumericVector lx( m_nage );
    
    
    NumericVector ma = plogis(m_age,ah,gh);
    phie = 0;

    
    int i;
    for (i = 0; i < m_nage; ++i)
    {
      Rcpp::Rcout<<m_age[i]<<std::endl;
      la[i] = linf*(1.0-exp(-k*m_age[i]));
      wa[i] = a * pow(la[i],b);
      fa[i] = wa[i] * ma[i];
      va[i] = 1.0/(1.0+(exp(-log(19.)*((i-sel50)/(sel95-sel50)))));
    
      lx[i] = exp(-m*(m_age[i]-min(m_age)));
      if(m_age[i] == max(m_age) )
      {
        lx[i] = lx[i] / (1.-exp(-m));
      }
      phie += lx[i] * fa[i];
    }

    

    m_stock["lx"]   = lx;
    m_stock["la"]   = la;
    m_stock["wa"]   = wa;
    m_stock["fa"]   = fa;
    m_stock["va"]   = va;
    m_stock["phie"] = phie;
 }

 void stock::calcSteepnessBo()
 {
    NumericVector lz( m_nage );
    NumericVector za( m_nage );
    NumericVector sa( m_nage );
    NumericVector oa( m_nage );
    NumericVector va( m_nage );
    NumericVector wa( m_nage );
    NumericVector fa( m_nage );
    NumericVector qa( m_nage );
    NumericVector t2( m_nage );
    NumericVector t3( m_nage );


    va = as<NumericVector>(m_stock["va"]);
    wa = as<NumericVector>(m_stock["wa"]);
    fa = as<NumericVector>(m_stock["fa"]);

    double phif = 0;
    double dlz_df;
    double dphif_df;
    double dphiq_df;

    int i;
    for (i = 0; i < m_nage; ++i)
    {
      za[i] = m + fmsy * va[i];
      sa[i] = exp(-za[i]);
      oa[i] = (1.-sa[i]);
      qa[i] = va[i]*oa[i]/za[i];
      t2[i] = wa[i]*va[i]*va[i]/za[i];
      t3[i] = exp(-za[i]) - oa[i]/za[i];

      if(m_age[i] == min(m_age) )
      {
        lz[i]    = 1.0;
        dlz_df   = 0.0;
        dphif_df = 0.0;
        dphiq_df = 0.0;
      }
      else //if(i != m_age.begin() )
      {
        lz[i] = lz[i-1] * sa[i-1];
        dlz_df = dlz_df * sa[i-1] - lz[i-1]*va[i-1]*sa[i-1];
      }
      if(m_age[i] == max(m_age))
      {
        lz[i] = lz[i] / oa[i];
        dlz_df = dlz_df/sa[i] -
                 lz[i-1]*sa[i-1]*va[i]*sa[i] /
                 (oa[i]*oa[i]);
                 
      }

      phif = phif + lz[i] * fa[i];
      Rcpp::Rcout<<lz[i]<<std::endl;
    }

 }

 /**
  * @brief Stock Reduction Analysis.
  * @details 
  * @return [description]
  */
 class sra
 {
 private:
 	stock m_stock;
 	
 public:
 	sra(const stock &c_stock)
 	:m_stock(c_stock) {}
 	
 	void assessmentMethod(void) {}
 	
 	
 };




 RCPP_MODULE(sra_module) 
 {
    class_<stock>("stock")
    .constructor<List>()
    .property( "m_stock", &stock::get_stock, &stock::set_stock )
    .method( "calcAgeSchedule", &stock::calcAgeSchedule )
    .method( "calcSteepnessBo", &stock::calcSteepnessBo )
    ;

    class_<sra>("Stock_Reduction_Analysis")
	.constructor<stock>()
	.method( "assessmentMethod", &sra::assessmentMethod )
	;
 }

