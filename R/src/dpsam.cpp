#include <Rcpp.h>
#include <Rcpp/Benchmark/Timer.h>
using namespace Rcpp; 
// [[Rcpp::plugins(cpp11)]]
 

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

    int m_sage;
    int m_nage;
    int m_ageSize;

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
    double reck;
    double bo;
    double ro;
    double re;
    double spr;
    double sel50;
    double sel95;
    

    NumericVector m_age;
    NumericVector m_lx;
    NumericVector m_fa;
    NumericVector m_va;
    NumericVector m_wa;


    DataFrame fishery_df;
    
    friend class sra;
 public:
     stock(List _stock)
     :m_stock(_stock) 
     {
     	calcAgeSchedule();
      calcSteepnessBo();
     	
     	Rcpp::Rcout<<"phie = "<<phie<<std::endl;
     }

     
    List get_stock()       { return m_stock; }
    void set_stock(List x_){ m_stock = x_;   }

    void calcAgeSchedule();
    void calcSteepnessBo();
     
 };

 void stock::calcAgeSchedule()
 {
 	  m 	  = as<double>(m_stock["m"]);
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

    fishery_df = as<DataFrame>(m_stock["data"]);

    m_sage  = min(m_age);
    m_nage  = max(m_age);
    m_ageSize = m_age.size();

    Rcpp::Rcout<<"size of age = "<<m_ageSize<<std::endl;
    NumericVector la( m_ageSize-1 );
    NumericVector wa( m_ageSize-1 );
    NumericVector fa( m_ageSize-1 );
    NumericVector va( m_ageSize-1 );
    NumericVector lx( m_ageSize-1 );
    
    
    NumericVector ma = plogis(m_age,ah,gh);
    phie = 0;

    
    int i;
    for (i = 0; i < m_nage; ++i)
    {
      la[i] = linf*(1.0-exp(-k*m_age[i]));
      wa[i] = a * pow(la[i],b);
      fa[i] = wa[i] * ma[i];
      va[i] = 1.0/(1.0+(exp(-log(19.)*((m_age[i]-sel50)/(sel95-sel50)))));
    
      lx[i] = exp(-m*(m_age[i]-m_sage));
      if(m_age[i] == max(m_age) )
      {
        lx[i] = lx[i] / (1.-exp(-m));
        // Rcpp::Rcout<<"Plus group age = "<<lx[i]<<" "<<lx[i-1]<<std::endl;
      }
      phie += lx[i] * fa[i];
      // Rcpp::Rcout<<i<<"\t"<<m_age[i]<<" \t"<<lx[i];
      // Rcpp::Rcout<<"\t"<<la[i]<<"\t"<<va[i]<<std::endl;
    }

    m_lx = lx;
    m_fa = fa;
    m_va = va;
    m_wa = wa;

    m_stock["lx"]   = lx;
    m_stock["la"]   = la;
    m_stock["wa"]   = wa;
    m_stock["fa"]   = fa;
    m_stock["va"]   = va;
    m_stock["phie"] = phie;
 }

 void stock::calcSteepnessBo()
 {
    NumericVector lz( m_ageSize-1 );
    NumericVector za( m_ageSize-1 );
    NumericVector sa( m_ageSize-1 );
    NumericVector oa( m_ageSize-1 );
    NumericVector va( m_ageSize-1 );
    NumericVector wa( m_ageSize-1 );
    NumericVector fa( m_ageSize-1 );
    NumericVector qa( m_ageSize-1 );
    NumericVector t2( m_ageSize-1 );
    NumericVector t3( m_ageSize-1 );


    va = as<NumericVector>(m_stock["va"]);
    wa = as<NumericVector>(m_stock["wa"]);
    fa = as<NumericVector>(m_stock["fa"]);

    double phif = 0;
    double phiq = 0;
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

      if(m_age[i] == m_sage )
      {
        lz[i]    = 1.0;
        dlz_df   = 0.0;
        dphif_df = 0.0;
        dphiq_df = 0.0;
      }
      else
      {
        lz[i] = lz[i-1] * sa[i-1];
        dlz_df = dlz_df * sa[i-1] - lz[i-1]*va[i-1]*sa[i-1];
      }
      if(m_age[i] == m_nage )
      {
        // Rcpp::Rcout<<"Plus Group "<<m_age[i]<<std::endl;
        lz[i] = lz[i] / oa[i];
        dlz_df = dlz_df/sa[i] -
                 lz[i-1]*sa[i-1]*va[i]*sa[i] /
                 (oa[i]*oa[i]);
      }
      dphif_df += fa[i]*dlz_df;
      dphiq_df += wa[i]*qa[i]*dlz_df + lz[i]*t2[i]*t3[i];
      phif     += lz[i]*fa[i];
      phiq     += lz[i]*qa[i]*wa[i];
      Rcpp::Rcout<<i<<"\t"<<m_age[i]<<" \t";
      Rcpp::Rcout<<m_lx[i]<<"\t"<<lz[i]<<std::endl;
    }
    reck = phie/phif - (fmsy*phiq*phie/(phif*phif)*dphif_df) / (phiq+fmsy*dphiq_df);
    re   = msy / (fmsy*phiq);
    ro   = re*(reck-1.0)/(reck-phie/phif);
    bo   = ro * phie;
    spr  = phif/phie;
    Rcpp::Rcout<<"Unfished recruits = "<<ro<<std::endl;
 }

 /**
  * @brief Stock Reduction Analysis.
  * @details 
  * @return [description]
  */
 class sra: public stock
 {
 private:
  int m_yearSize;

  double so;
  double beta;

  IntegerVector m_year;
  NumericVector m_chat;
  NumericVector m_cpue;

  NumericMatrix m_N;
 	
 public:
 	sra(const stock &c_stock)
 	:stock(c_stock) {}
 	
  void initializeModel(void);
 	void ageStructuredModel(void);
 	
 	
 };

 void sra::initializeModel(void)
 {
    m_year = fishery_df["year"];
    m_chat = fishery_df["catch"];
    m_cpue = fishery_df["cpue"];
    
    m_yearSize = m_year.size();

    so   = reck/phie;
    beta = (reck-1.0)/bo;

 }

 void sra::ageStructuredModel(void)
 {
    
    
    NumericVector ft(m_yearSize-1);
    NumericVector bt(m_yearSize-1);
    NumericMatrix N(m_yearSize,m_ageSize-1);

    double sa,za;
    

  
    // Initial numbers at age
    for (int j = 0; j < m_ageSize; ++j)
    {
      N(0,j) = ro * m_lx[j];
      // Rcpp::Rcout<<N(0,j)<<" ";
    }
    // N(0,_) = ro * m_lx;
    bt[0] = sum(N(0,_)*m_wa);
    

    // Rcpp::Rcout<<nyrs<<std::endl;
    for (int i = 0; i < m_yearSize; ++i)
    {
      ft[i] = 0;
      double sbt = 0;// = sum(N(i,_) * m_fa);
      for (int j = 0; j < m_ageSize; ++j)
      {
        sbt += N(i,j) * m_fa[j];
        Rcpp::Rcout<<m_fa[j]<<" ";
      }
      Rcpp::Rcout<<"\n"<<bo<<"\t"<<sbt<<"\t"<<so*sbt/(1.+beta*sbt)<<"\n"<<std::endl;

      // Update numbers at age
      for (int j = 0; j < m_ageSize; ++j)
      {
        za  = m + ft[i] * m_va[j];
        sa  = exp(-za);

        // New recruits.
        if(m_age[j] == m_sage)
        {
          N(i+1,j) = so*sbt/(1.+beta*sbt);
        }

        // Survive each cohort
        if(m_age[j] != m_nage)
        {
          N(i+1,j+1) = N(i,j) * sa;
        }

        // Plus group
        if(m_age[j] == m_nage)
        {
          N(i+1,j) += N(i,j) * sa;
        }

        Rcpp::Rcout<<j<<" age "<<m_age[j]<<"\t"<<za<<" "<<N(i,j)<<std::endl;
      }


      // N(i+1,0)   = so*sbt/(1.+beta*sbt);
      // Rcpp::Rcout<<"\n";
      // for (int j = 0; j < m_ageSize; ++j)
      // {
      //   za[j]    = m + ft[i]*m_va[j];
      //   sa[j]    = exp(-za[j]);
      //   N(i+1,j+1) = N(i,j) * sa[j];
      //   if(m_age[j] == m_nage)
      //   {
      //     N(i+1,j+1) += N(i,j+1) * sa[j+1];
      //     Rcpp::Rcout<<j<<" "<<m_age[j]<<std::endl;
      //   }

      //   // Rcpp::Rcout<<N(i,j)<<" ";
      //   Rcpp::Rcout<<j<<" age "<<m_age[j]<<"\t"<<za[j]<<" "<<N(i,j)<<std::endl;
      // }
    //   Rcpp::Rcout<<"age "<<m_age[m_nage]<<"\t"<<za[m_nage]<<" "<<N(i,m_nage)<<std::endl;
    //   bt[i+1] = sum( N(i+1,_) * m_wa );
      
    //   // Rcpp::Rcout<<i<<"\t"<<N(i,0)<<"\t"<<N(i,m_nage)<<"\t";
    //   // Rcpp::Rcout<<so*sbt/(1.+beta*sbt)<<std::endl;
    }




    // Rcpp::Rcout<<N(0,m_nage-1)<<"\t"<<N(1,m_nage-2)<<std::endl;
  
 }



 RCPP_MODULE(sra_module) 
 {
    class_<stock>("stock")
    .constructor<List>()
    .property( "m_stock", &stock::get_stock, &stock::set_stock )
    .method( "calcAgeSchedule", &stock::calcAgeSchedule )
    .method( "calcSteepnessBo", &stock::calcSteepnessBo )
    ;

    class_<sra>("sra")
	.constructor<stock>()
  .method( "ageStructuredModel", &sra::ageStructuredModel )
	.method( "initializeModel", &sra::initializeModel )
	;
 }

