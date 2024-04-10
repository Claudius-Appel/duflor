#include <Rcpp.h>
#include <array>
#include <tuple>
#include <iostream>
#include <vector>
using namespace Rcpp;

//' @title optimised 'C++'-implementation of [rectangularRange_HSV()]
//'
//' @note
//' The use of  [rectangularRange_HSV()] & [rectangularRange_HSV_cpp()] is strongly discouraged in favour of this function,
//' due to its drastically slower execution.
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
//' @examples
//' \dontrun{
//' library(duflor)
//' ## load example data
//' file_path <- load_extdata("duflor-icon.png")
//' pixel.array <- load_image(file_path,F,T)
//' spectrums <- getOption("duflor.default_hsv_spectrums")
//'
//' ## convert spectrums to matrix
//' nlb <- do.call(rbind,spectrums$lower_bound)
//' nub <- do.call(rbind,spectrums$upper_bound)
//'
//' ## strip dimnames-attributes
//' dimnames(nlb) <- c()
//' dimnames(nub) <- c()
//' ## extract matches
//' result <- rectangularRange_HSV_iteronce_cpp(H = pixel.array[,,,1],
//'                                             S = pixel.array[,,,2],
//'                                             V = pixel.array[,,,3],
//'                                             upper_bound = nub,
//'                                             lower_bound = nlb,
//'                                             image_width = dim(pixel.array)[1],
//'                                             check_V = T)
//' ## add names to results-matrix.
//' names(result) <- names(spectrums$lower_bound)
//' }
//' @export
// [[Rcpp::export]]
List rectangularRange_HSV_iteronce_cpp(NumericVector H,
                                       NumericVector S,
                                       NumericVector V,
                                       NumericMatrix upper_bound,
                                       NumericMatrix lower_bound,
                                       int image_width,
                                       bool check_V) {
    int repetitions = H.size();
    List results;

    for (int j = 0; j < upper_bound.rows(); j++) {
        // re-init vectors for the indices
        std::vector<int> row_indices;
        std::vector<int> col_indices;

        // Preallocate memory for indices vectors
        row_indices.reserve(repetitions);
        col_indices.reserve(repetitions);
        if (check_V) {
            if (upper_bound.cols()!=3) {
                stop("'upper_bound' must be a vector of 3 numerical values.");
            }
            if (lower_bound.cols()!=3) {
                stop("'lower_bound' must be a vector of 3 numerical values.");
            }
        } else {
            if ((upper_bound.cols()!=2) && (upper_bound.cols()!=3)) {
                stop("'upper_bound' must be a vector of 3 or 2 numerical values.");
            }
            if ((lower_bound.cols()!=2) && (lower_bound.cols()!=3)) {
                stop("'lower_bound' must be a vector of 3 or 2 numerical values.");
            }
        }
        // store static bounds in variables
        double lower_bound_H = lower_bound(j, 0);
        double lower_bound_S = lower_bound(j, 1);
        double lower_bound_V = lower_bound(j, 2);
        double upper_bound_H = upper_bound(j, 0);
        double upper_bound_S = upper_bound(j, 1);
        double upper_bound_V = upper_bound(j, 2);

        for (int i = 0; i < repetitions; i++) {
            if (check_V) {
                // Check bounds and add indices if condition is met
                if ((lower_bound_H <= H[i]) && (H[i] <= upper_bound_H)
                        && (lower_bound_S <= S[i]) && (S[i] <= upper_bound_S)
                        && (lower_bound_V <= V[i]) && (V[i] <= upper_bound_V)) {
                    int row = (i % image_width) + 1;
                    int col = (i / image_width) + 1;
                    row_indices.push_back(row);
                    col_indices.push_back(col);
                }
            } else {
                // Check bounds and add indices if condition is met
                if ((lower_bound_H <= H[i]) && (H[i] <= upper_bound_H)
                        && (lower_bound_S <= S[i]) && (S[i] <= upper_bound_S)) {
                    int row = (i % image_width) + 1;
                    int col = (i / image_width) + 1;
                    row_indices.push_back(row);
                    col_indices.push_back(col);
                }
            }
        }

        // compute N, then create a matrix for the indices computed above
        int n = row_indices.size();
        IntegerMatrix idx(n, 2);
        std::copy(row_indices.begin(), row_indices.end(), idx.column(0).begin());
        std::copy(col_indices.begin(), col_indices.end(), idx.column(1).begin());

        // assign names
        colnames(idx) = CharacterVector::create("x", "y");

        // compute img.fraction
        int hs = H.size();
        double fraction = static_cast<double>(n) / (int)hs;
        // Rcout << "The value of H.size : " << H.size() << "\n";
        // Rcout << "The value of Hs : " << hs << "\n";
        // Rcout << "The value of n : " << n << "\n";
        // Rcout << "The value of fraction : " << fraction << "\n";

        // Init the return list
        List result_entry;
        // insert:
        // pixel.idx > results[[k]]$pixel.idx
        // pixel.count > results[[k]]$pixel.count
        // img.fraction > results[[k]]img.fraction
        result_entry["pixel.idx"] = idx;
        result_entry["pixel.count"] = n;
        result_entry["img.fraction"] = fraction;

        // Push the entry into the results list
        results.push_back(result_entry);
    }
    return results;
}


