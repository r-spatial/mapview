#include <Rcpp.h>
using namespace Rcpp;

#include <string>
#include <fstream>
#include <streambuf>


////////////////////////////////////////////////////////////////////////////////
// Avoid known MinGW std::to_string() bug (code taken from /////////////////////
// https://stackoverflow.com/questions/12975341/to-string-is-not-a-member-of-std-says-g-mingw)
////////////////////////////////////////////////////////////////////////////////

#include<sstream>
template <typename T>
std::string to_string(T value) {
  //create an output string stream
  std::ostringstream os;

  //throw the value into the string stream
  os << value;

  //convert the string stream into a string and return
  return os.str() ;
}

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
std::string brewPopupRowC(std::string index, std::string colname,
                          std::string value, bool rowIndex) {

  std::string chIndString;
  if (rowIndex) {
    chIndString = std::string("<td>") + index + "</td>";
  } else {
    chIndString = std::string("<td></td>");
  }

  std::string chColString;
  chColString = std::string("<td>") + "<b>" + colname + "&emsp;</b>" + "</td>";

  std::ostringstream ssVal;
  ssVal << value;

  std::string chValString;
  chValString = std::string("<td align='right'>") + ssVal.str() + "&emsp;</td>";

  std::string chOutString;
  chOutString = std::string("<tr>" + chIndString + chColString + chValString + "</tr>");

  return chOutString;
}

////////////////////////////////////////////////////////////////////////////////
// alternative pattern used for even column indices ////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
std::string brewPopupRowAltC(std::string index, std::string colname,
                             std::string value, bool rowIndex) {

  std::string chIndString;
  if (rowIndex) {
    chIndString = std::string("<td>") + index + "</td>";
  } else {
    chIndString = std::string("<td></td>");
  }

  std::string chColString;
  chColString = std::string("<td>") + "<b>" + colname + "&emsp;</b>" + "</td>";

  std::ostringstream ssVal;
  ssVal << value;

  std::string chValString;
  chValString = std::string("<td align='right'>") + ssVal.str() + "&emsp;</td>";

  std::string chOutString;
  chOutString = std::string("<tr class=\'alt\'>" + chIndString + chColString + chValString + "</tr>");

  return chOutString;
}

////////////////////////////////////////////////////////////////////////////////
// standard pattern used for coordinates ///////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
std::string brewPopupCoords(std::string colname, std::string value) {

  std::string chIndString;
  chIndString = std::string("<td></td>");

  std::string chColString;
  chColString = std::string("<td>") + "<b>" + colname + "</b>" + "</td>";

  std::ostringstream ssVal;
  ssVal << value;

  std::string chValString;
  chValString = std::string("<td align='right'>") + ssVal.str() + "&emsp;</td>";

  std::string chOutString;
  chOutString = std::string("<tr class=\'coord\'>" + chIndString + chColString + chValString + "</tr>");

  return chOutString;
}

////////////////////////////////////////////////////////////////////////////////
// merge alternating string patterns per row ///////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
std::string mergePopupRows(CharacterVector names, CharacterVector values,
                           bool rowIndex) {

  int nSize = values.size();
  std::string chOut = std::string("");

  std::ostringstream ssName;
  std::ostringstream ssValue;

  for (int i = 0; i < nSize; i++) {

    ssName << names[i];
    ssValue << values[i];

    // feature id or coords
    if ((names[i] == "Feature ID") |
          (names[i] == "Longitude") | (names[i] == "Latitude")) {
      chOut = chOut + brewPopupCoords(ssName.str(), ssValue.str());
    } else {
      // even variable columns
      if (i%2 == 0) {
        chOut = chOut + brewPopupRowC(to_string(i), ssName.str(), ssValue.str(), rowIndex);

        // odd variable columns
      } else {
        chOut = chOut + brewPopupRowAltC(to_string(i), ssName.str(), ssValue.str(), rowIndex);
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
// Convert String to UTF-8 //////////////// ////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

String enc2utf8_string(const String & x) {
  String str(x);
  str.set_encoding(CE_UTF8);
  return str;
}

CharacterVector enc2utf8_chrvec(const CharacterVector & x) {
  CharacterVector str(x.size());
  std::transform(x.begin(), x.end(), str.begin(), enc2utf8_string);
  return str;
}

////////////////////////////////////////////////////////////////////////////////
// Create list with string patterns per row ////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

// [[Rcpp::export]]
StringVector listPopupTemplates(CharacterMatrix x, CharacterVector names,
                        std::string tmpPath, bool rowIndex) {

  // number of rows and columns
  int nRows = x.nrow();
  int nCols = x.ncol();

  // intermediary variables
  CharacterVector chVal(nCols);
  std::string chStr;

  // output list
  StringVector lsOut(nRows);

  // import template
  std::string chTemplate = createTemplate(tmpPath);
  String chTmp = chTemplate;

  // create strings for each single row
  for (int i = 0; i < nRows; i++) {
    chVal = enc2utf8_chrvec(x(i, _));
    chStr = mergePopupRows(names, chVal, rowIndex);

    chTmp = gsubC("<%=pop%>", chStr, chTmp);
    lsOut[i] = enc2utf8_string(chTmp);

    // reset intermediary string
    chTmp = chTemplate;
  }

  return lsOut;
}

// /*** R
// # odd version
// identical(
//   brewPopupRow("carat", .5),
//   brewPopupRowC("carat", .5)
// )
//
// # even version
// identical(
//   brewPopupRowAlt("price", 23.75),
//   brewPopupRowAltC("price", 23.75)
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
