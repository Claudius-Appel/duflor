#include <Rcpp.h>
#include <array>
#include <tuple>
#include <iostream>
#include <vector>
using namespace Rcpp;
// Learn more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/




//' @title 'C++'-implementation of [rectangularRange_HSV()]
//'
//' @note
//' The use of  [rectangularRange_HSV()] is strongly discouraged in favour of this function,
//' due to its drastically slower execution.
//'
//' @inheritParams .main_args
//' @param H respective component of a `pixel.array`
//' @param S respective component of a `pixel.array`
//' @param V respective component of a `pixel.array`
//' @param image_width Width of `pixel.array`, as returned via `dim(pixel.array)[1]`
//' @return A list-object with the following elements (when supplying one one pair of bounds)
//' - `pixel.idx` - pixel-locations of pixels detected between lower and upper bound.
//'
//' Upon failure to find any matching pixels, an empty matrix of dimensions `[0, 1:2]` is returned.
//' @export
// [[Rcpp::export]]
DataFrame rectangularRange_HSV_cpp(NumericVector H,
                                   NumericVector S,
                                   NumericVector V,
                                   std::vector<double> upper_bound,
                                   std::vector<double> lower_bound,
                                   int image_width) {
    int repetitions = H.size();
    std::vector<int> row_indices;
    std::vector<int> col_indices;
    for (int i = 0; i < repetitions; i++) {
        if ((lower_bound[0] <= H[i]) && (H[i] <= upper_bound[0])
                && (lower_bound[1] <= S[i]) && (S[i] <= upper_bound[1])) {
            int row = i % image_width;
            int col = i / image_width;
            row_indices.push_back(row);
            col_indices.push_back(col);
        }
    }
    return DataFrame::create(_["x"] = row_indices,
                             _["y"] = col_indices);
}


//' @title 'C++'-implementation of [rectangularRange_HSV()]
//'
//' @note
//' The use of  [rectangularRange_HSV()] is strongly discouraged in favour of this function,
//' due to its drastically slower execution.
//' Unlike [rectangularRange_HSV_cpp], this function will threshold on the Value-parameter as well.
//'
//' @inheritParams .main_args
//' @param H respective component of a `pixel.array`
//' @param S respective component of a `pixel.array`
//' @param V respective component of a `pixel.array`
//' @param image_width Width of `pixel.array`, as returned via `dim(pixel.array)[1]`
//' @return A list-object with the following elements (when supplying one one pair of bounds)
//' - `pixel.idx` - pixel-locations of pixels detected between lower and upper bound.
//'
//' Upon failure to find any matching pixels, an empty matrix of dimensions `[0, 1:2]` is returned.
//' @export
// [[Rcpp::export]]
DataFrame rectangularRange_HSV_cpp2(NumericVector H,
                                   NumericVector S,
                                   NumericVector V,
                                   std::vector<double> upper_bound,
                                   std::vector<double> lower_bound,
                                   int image_width) {
    int repetitions = H.size();
    std::vector<int> row_indices;
    std::vector<int> col_indices;
    for (int i = 0; i < repetitions; i++) {
        if ((lower_bound[0] <= H[i]) && (H[i] <= upper_bound[0])
                && (lower_bound[1] <= S[i]) && (S[i] <= upper_bound[1])
                && (lower_bound[2] <= V[i]) && (V[i] <= upper_bound[2])) {
            int row = i % image_width;
            int col = i / image_width;
            row_indices.push_back(row);
            col_indices.push_back(col);
        }
    }
    return DataFrame::create(_["x"] = row_indices,
                             _["y"] = col_indices);
}
