#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector SimGrowth_kd(DataFrame DF, double ProbPest, double cmdMin, 
                        double cmdMax, double tempMin, double tempMax, double climLoss){
  NumericVector Growth = DF["Growth"]; //convert to vectors
  NumericVector NoMort  = DF["NoMort"];
  NumericVector MeanDead = DF["MeanDead"];
  NumericVector Ruin = DF["Suit"];//think about this one
  NumericVector climCMD = DF["CMD"];
  NumericVector climMax = DF["Tmax_sm"];
  NumericVector climMin = DF["Tmin_sp"];
  
  int numYears = Growth.length();
  NumericVector Returns(numYears);
  double height, percentDead, percentRuin, climDead, climDiff, diffProp;
  int prevTrees, numDead, i;
  int nTrees = 100;
  for(i = 0; i < numYears; i++){
    height = sum(Growth[Rcpp::Range(0,i)]);
    Returns[i] = nTrees*height;
    climDead = 0;
    if(climCMD[i] > cmdMax){ // CMD max 
      climDiff = climCMD[i] - cmdMax;
      diffProp = (climDiff*100)/(cmdMax - cmdMin);
      climDiff = 1 - exp(-climLoss*diffProp);//1 - exp function
      climDead += climDiff*nTrees;
      //Rcout << "Too dry \n";
    }
    if(climCMD[i] < cmdMin){
      climDiff = cmdMin - climCMD[i];
      diffProp = (climDiff*100)/(cmdMax - cmdMin);
      climDiff = 1 - exp(-climLoss*diffProp);//1 - exp function
      climDead += climDiff*nTrees;
      //Rcout << "Too wet \n";
    }
    if(climMax[i] > tempMax){
      climDiff = climMax[i] - tempMax;
      diffProp = (climDiff*100)/(tempMax - tempMin);
      climDiff = 1 - exp(-climLoss*diffProp);//1 - exp function
      climDead += climDiff*nTrees;
      //Rcout << "Too hot \n";
    }
    if(climMin[i] < tempMin){
      climDiff = tempMin - climMin[i];
      diffProp = (climDiff*100)/(cmdMax - cmdMin);
      climDiff = 1 - exp(-climLoss*diffProp);//1 - exp function
      climDead += climDiff*nTrees;
      //Rcout << "Too cold \n";
    }
    if(climDead > nTrees){
      climDead = nTrees;
    }
    nTrees = nTrees - climDead;
    if(Rcpp::runif(1,0,1)[0] <= ProbPest){//pest outbreak
      percentRuin = rgamma(1, Ruin[i], 0.07)[0];
      if(percentRuin > 1){
        percentRuin = 1;
      }
      numDead = percentRuin*nTrees;
      nTrees = nTrees - numDead;
    }else if(Rcpp::runif(1,0,100)[0] > NoMort[i]){//regular environmental loss
      percentDead = Rcpp::rgamma(1, 1.5, MeanDead[i])[0];
      numDead = (percentDead/100)*prevTrees;
      nTrees = nTrees - numDead;
    }
  }
  return(Returns);
}
