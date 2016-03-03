#include <Rcpp.h>
using namespace Rcpp;

#include <string>
#include <fstream>
#include <streambuf>


////////////////////////////////////////////////////////////////////////////////
// Replace string with another string (function taken from /////////////////////
// https://stackoverflow.com/questions/2896600/how-to-replace-all-occurrences-of-a-character-in-string) ////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
std::string gsubC(const std::string& pattern, const std::string& replacement,
                  std::string x) {
  size_t start_pos = 0;
  while((start_pos = x.find(pattern, start_pos)) != std::string::npos) {
    x.replace(start_pos, pattern.length(), replacement);
    start_pos += replacement.length();
  }
  return x;
}

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
// standard pattern used for coordinates ///////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
std::string brewPopupCoords(std::string colname, std::string value) {

  std::string chColString;
  chColString = std::string("<td>") + colname + "</td>";

  std::ostringstream ssVal;
  ssVal << value;

  std::string chValString;
  chValString = std::string("<td>") + ssVal.str() + "</td>";

  std::string chOutString;
  chOutString = std::string("<tr class=\'coord\'>" + chColString + chValString + "</tr>");

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

    // feature id or coords
    if (names[i] == "Feature ID" |
          names[i] == "Longitude" | names[i] == "Latitude") {
      chOut = chOut + brewPopupCoords(ssName.str(), ssValue.str());
    } else {

      // even variable columns
      if (i%2 == 0) {
        chOut = chOut + brewPopupRowC(ssName.str(), ssValue.str());

      // odd variable columns
      } else {
        chOut = chOut + brewPopupRowAltC(ssName.str(), ssValue.str());
      }

    }

    ssName.str(std::string());
    ssValue.str(std::string());
  }

  return chOut;
}

////////////////////////////////////////////////////////////////////////////////
// merge alternating string patterns per row ///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
std::string createTemplate(std::string tmpPath) {
  std::ifstream file(tmpPath.c_str());
  std::string line, data;

  std::ostringstream ssLines;

  // import lines iteratively
  while(std::getline(file, line))
  {
    std::stringstream linestream(line);
    std::getline(linestream, data, '\n');

    ssLines << data;
  }

  return ssLines.str();
}

////////////////////////////////////////////////////////////////////////////////
// Create list with string patterns per row ////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
List listPopupTemplates(CharacterMatrix x, CharacterVector names,
                        std::string tmpPath) {

  // number of rows and columns
  int nRows = x.nrow();
  int nCols = x.ncol();

  // intermediary variables
  CharacterVector chVal(nCols);
  std::string chStr;

  // output list
  List lsOut(nRows);

  // import template
  std::string chTemplate = createTemplate(tmpPath);
  std::string chTmp = chTemplate;

  // create strings for each single row
  for (int i = 0; i < nRows; i++) {
    chVal = x(i, _);
    chStr = mergePopupRows(names, chVal);

    chTmp = gsubC("<%=pop%>", chStr, chTmp);
    lsOut[i] = chTmp;

    // reset intermediary string
    chTmp = chTemplate;
  }

  return lsOut;
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