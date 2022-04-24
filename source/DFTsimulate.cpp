#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;


class DFT{
    private:
        double time_step; // time step
        double sqrt_time_step; // used for Wiener process
        int num_step; // number of steps 
        int num_points; // number of points
        vec cumsum; // integration of 1/(1-t) for all the time steps
        vec split_time; // time when the points split
        vec split_value; // value when the points split
        double sigma; // Wiener process variance parameter
        mat traces; // traces of all the points

    public:
        DFT(int num=6, double interval=0.00001, double stdev=1){
            num_points = num;
            time_step = interval;
            num_step = (int)1/time_step;
            sqrt_time_step = sqrt(time_step);
            sigma = stdev;
            traces = zeros(num_points, num_step + 1);
            split_time.zeros(num_points - 1);
            split_value.zeros(num_points - 1);
            cumsum = zeros(num_step);
            for (int i=0; i<num_step; i++){
                if (i == 0){
                    cumsum(i) = 3 / (1 - i * time_step) * time_step;
                } else{
                    cumsum(i) = cumsum(i-1) +  3 / (1 - i * time_step) * time_step;
                }
            }
        }

        void simulate(){
            // first point
            for (int j=1; j<num_step+1; j++){
                traces(0, j) = traces(0, j-1) + randn() * sigma * sqrt_time_step; 
            }
            for (int i=1; i<num_points; i++){
                vec adjusted_cumsum = cumsum / i;
                double sample = - log(1 - randu());
                int step_separate = accu(adjusted_cumsum < sample);
                int start = 0;// the starting timepoint to simulate Wiener process of its own
                
                if (step_separate == 0){
                    start = 1;
                } else if (step_separate == num_step){
                    start = num_step; // break away at the very last step
                } else{
                    start = step_separate + 1;
                }

                split_time(i-1) = (start - 1) * time_step;
                split_value(i-1) = 0;
                
                if (start > 1){
                    int copy_id = randi(distr_param(0, i-1));
                    split_value(i-1) = traces(copy_id, start-1);
                    traces(i, span(0, start - 1)) = traces(copy_id, span(0, start - 1));
                } 
                
                for (int j=start; j<num_step + 1; j++){
                    traces(i, j) = traces(i, j-1) + randn() * sigma * sqrt_time_step;
                }
            }
        }

        List get_output(){
            return List::create(Named("timestep") = time_step,
                                Named("SplitTime") = split_time,
                                Named("SplitValue") = split_value,
                                Named("data") = traces);
        }

};

//[[Rcpp::export]]
List simulate_DFT(int num=6, double interval=0.00001, double stdev=1){
    DFT newprocess(num, interval, stdev);
    newprocess.simulate();
    return newprocess.get_output();
}
