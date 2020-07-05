#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector C_Akritas(NumericMatrix truth, NumericVector unique_times,
                        NumericVector FX_train, NumericVector FX_predict,
                        NumericMatrix newdata,
                        double lambda) {

  double t;
  double prod;
  double den;
  double FXn;

  NumericMatrix surv(newdata.nrow(), unique_times.size());

    for (int n = 0; n < newdata.nrow(); n++) {
      FXn = FX_predict[n];
      for (int i = 0; i < unique_times.size(); i++) {
        t = unique_times[i];
        prod = 1;
        for (int j = 0; j < FX_train.size(); j++) {
          if (truth(j, 0) <= t && truth(j, 1) == 1 && (fabs(FXn - FX_train[j]) <= lambda)) {
              den = 0;
              for (int l = 0; l < FX_train.size(); l++) {
                den += (fabs(FXn - FX_train[l]) <= lambda) * (truth(l, 0) >= truth(j, 0));
              }
              prod *= (1 - 1.0/den);
          }
        }
        surv(n, i) = prod;
      }
    }

    return surv;
}
