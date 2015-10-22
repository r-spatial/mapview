#include <Rcpp.h>
using namespace Rcpp;

////////////////////////////////////////////////////////////////////////////////
// standard pattern used for odd column indices ////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
std::string brewPopupRowC(std::string colname, std::string value) {

  std::string chColString;
  chColString = std::string("<td>") + colname + "</td>";

  std::ostringstream ssVal;
  ssVal << value;

  std::string chValString;
  chValString = std::string("<td>") + ssVal.str() + "</td>";

  std::string chOutString;
  chOutString = std::string("<tr>" + chColString + chValString + "</tr>");

  return chOutString;
}

////////////////////////////////////////////////////////////////////////////////
// alternative pattern used for even column indices ////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
std::string brewPopupRowAltC(std::string colname, std::string value) {

  std::string chColString;
  chColString = std::string("<td>") + colname + "</td>";

  std::ostringstream ssVal;
  ssVal << value;

  std::string chValString;
  chValString = std::string("<td>") + ssVal.str() + "</td>";

  std::string chOutString;
  chOutString = std::string("<tr class=\'alt\'>" + chColString + chValString + "</tr>");

  return chOutString;
}

////////////////////////////////////////////////////////////////////////////////
// merge alternating string patterns per row ///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
std::string mergePopupRows(CharacterVector names, CharacterVector values) {

  int nSize = values.size();
  std::string chOut = std::string("");

  std::ostringstream ssName;
  std::ostringstream ssValue;

  for (int i = 0; i < nSize; i++) {

    ssName << names[i];
    ssValue << values[i];

    // odd
    if (i%2 == 0) {
      chOut = chOut + brewPopupRowC(ssName.str(), ssValue.str());

    // even
    } else {
      chOut = chOut + brewPopupRowAltC(ssName.str(), ssValue.str());
    }

    ssName.str(std::string());
    ssValue.str(std::string());
  }

  return chOut;
}


// /*** R
// # odd version
// identical(
//   mapview:::brewPopupRow("carat", .5),
//   mapview:::brewPopupRowC("carat", .5)
// )
//
// # even version
// identical(
//   mapview:::brewPopupRowAlt("price", 23.75),
//   mapview:::brewPopupRowAltC("price", 23.75)
// )
// */

////////////////////////////////////////////////////////////////////////////////
// convert all columns in a data.frame to 'character'///////////////////////////
////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
CharacterMatrix df2String(DataFrame x) {

  int nCols = x.size();
  CharacterVector chContents = as<CharacterVector>(x[1]);

  int nRows = chContents.size();
  CharacterMatrix chOut(nRows, nCols);

  for (int i = 0; i < nCols; i++) {
    chContents = as<CharacterVector>(x[i]);
    chOut(_, i) = chContents;
  }

  return chOut;
}