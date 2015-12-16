#include <Rcpp.h>
using namespace Rcpp;

#include <iostream>
#include <fstream>
using namespace std;

// [[Rcpp::export]]
std::string one2JSON(CharacterVector x) {

  int nSize = x.size();

  // loop over single entries in 'x'
  std::ostringstream chContent;
  for (int i = 0; i < nSize; i++) {
    if (i == 0) {
      chContent << x[i] << ",";
    } else if (i == 1) {
      chContent << x[i] << ','<<'"';
    } else if (i > 1  && i < (nSize - 1)) {
      chContent << x[i] <<'"'<<','<<'"' ;
    } else {
      chContent << x[i] << '"';
    }
  }

  // create string
  std::string chOut;
  chOut = std::string("[") +   chContent.str() + "]";

  return chOut;
}

// // [[Rcpp::export]]
// std::string all2JSON(NumericMatrix x) {
//
//   // initialize variables
//   int nRows = x.nrow();
//
//   NumericVector dXY(2);
//   std::string chOut;
//
//   if (nRows == 1) {
//     dXY[0] = x(0, 0);
//     dXY[1] = x(0, 1);
//
//     chOut = one2JSON(dXY);
//
//   } else {
//     // number of iterations
//     int nLast = nRows - 1;
//
//     // loop over all pairs of coordinates
//     for (int i = 0; i < nRows; i++) {
//
//       // current x and y coordinates
//       dXY[0] = x(i, 0);
//       dXY[1] = x(i, 1);
//
//       // create string
//       if (i == 0) {
//         chOut = std::string("[") + one2JSON(dXY) + ",";
//       } else if (i < nLast) {
//         chOut = chOut + one2JSON(dXY) + ",";
//       } else if (i == nLast) {
//         chOut = chOut + one2JSON(dXY) + "]";
//       }
//     }
//   }
//
//   // return concatenated string
//   return chOut;
// }


// [[Rcpp::export]]
List all2JSONlist(CharacterMatrix x) {

  // initialize variables
  int nRows = x.nrow();
  List lsOut(nRows);

  int nSize = x.ncol();
  CharacterVector chContent(nSize);

  // loop over rows
  for (int i = 0; i < nRows; i++) {

    // loop over columns
    for (int j = 0; j < nSize; j++) {
      chContent[j] = x(i, j);
    }

    // create string
    lsOut[i] = one2JSON(chContent);
  }

  // return concatenated string
  return lsOut;
}
