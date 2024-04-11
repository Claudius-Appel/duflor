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
//' @seealso [rectangularRange_HSV_iteronce_cpp()]
//'
//' @inheritParams .main_args
//' @param H respective component of a `pixel.array`
//' @param S respective component of a `pixel.array`
//' @param V respective component of a `pixel.array`
//' @param image_width Width of `pixel.array`, as returned via `dim(pixel.array)[1]`
//' @param check_V boolean toggle to also check the `VALUE`-component of an HSV-pixel
//' @return A list-object with the following elements (when supplying one one pair of bounds)
//' - `pixel.idx` - pixel-locations of pixels detected between lower and upper bound.
//'
//' Upon failure to find any matching pixels, an empty matrix of dimensions `[0, 1:2]` is returned.
//'
//'
//' @export
// [[Rcpp::export]]
DataFrame rectangularRange_HSV_cpp(NumericVector H,
                                   NumericVector S,
                                   NumericVector V,
                                   Nullable<NumericVector> upper_bound,
                                   Nullable<NumericVector> lower_bound,
                                   int image_width,
                                   bool check_V) {
    // Rcout << "The value of v : " << l << "\n";
    // Rcout << "The value of H : " << H.length() << "\n";
    int repetitions = H.size();
    std::vector<int> row_indices;
    std::vector<int> col_indices;
    // first check if the input bounds are NULL or not.
    NumericVector upper_bound_;
    NumericVector lower_bound_;
    if (upper_bound.isNotNull()) {
        upper_bound_ = upper_bound;
    }
    if (lower_bound.isNotNull()) {
        lower_bound_ = lower_bound;
    }
    if (check_V) {
        if (upper_bound_.length()!=3) {
            stop("'upper_bound' must be a vector of 3 numerical values.");
        }
        if (lower_bound_.length()!=3) {
            stop("'lower_bound' must be a vector of 3 numerical values.");
        }
        for (int i = 0; i < repetitions; i++) {
            if ((lower_bound_[0] <= H[i]) && (H[i] <= upper_bound_[0])
                    && (lower_bound_[1] <= S[i]) && (S[i] <= upper_bound_[1])
                    && (lower_bound_[2] <= V[i]) && (V[i] <= upper_bound_[2])) {
                int row = i % image_width + 1;
                int col = i / image_width + 1;
                row_indices.push_back(row);
                col_indices.push_back(col);
            }
        }
    } else {
        if ((upper_bound_.length()!=2) && (upper_bound_.length()!=3)) {
            stop("'upper_bound' must be a vector of 3 or 2 numerical values.");
        }
        if ((lower_bound_.length()!=2) && (lower_bound_.length()!=3)) {
            stop("'lower_bound' must be a vector of 3 or 2 numerical values.");
        }
        for (int i = 0; i < repetitions; i++) {
            if ((lower_bound_[0] <= H[i]) && (H[i] <= upper_bound_[0])
                    && (lower_bound_[1] <= S[i]) && (S[i] <= upper_bound_[1])) {
                int row = i % image_width + 1;
                int col = i / image_width + 1;
                row_indices.push_back(row);
                col_indices.push_back(col);
            }
        }
    }
    return DataFrame::create(_["x"] = row_indices,
                             _["y"] = col_indices);
}
