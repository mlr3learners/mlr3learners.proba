#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector C_Akritas(NumericMatrix truth, NumericVector unique_times,
                        NumericVector FX_train, NumericVector FX_predict,
                        NumericMatrix newdata,
                        double lambda) {

  double t;
  double prod;
  double num;
  double den;
  double FXn;

  NumericMatrix surv(newdata.nrow(), unique_times.size());

  for (int n = 0; n < newdata.nrow(); n++) {
    FXn = FX_predict[n];
    for (int i = 0; i < unique_times.size(); i++) {
      prod = 1;
      for (int j = 0; j <= i; j++) {
        t = unique_times[j];
          num = 0;
          den = 0;
          for (int l = 0; l < FX_train.size(); l++) {
            if (fabs(FXn - FX_train[l]) <= lambda && (truth(l, 0) >= t)) {
              num += (truth(l, 0) == t) * truth(l, 1);
              den += 1;
            }
          }
          if (den != 0) {
            prod *= (1 - num/den);
          }
      }
      surv(n, i) = prod;
    }
  }

  return surv;
}
