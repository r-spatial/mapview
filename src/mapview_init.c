#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP mapview_all2JSONlist(SEXP);
extern SEXP mapview_anyNA(SEXP);
extern SEXP mapview_brewPopupCoords(SEXP, SEXP);
extern SEXP mapview_brewPopupRowAltC(SEXP, SEXP, SEXP);
extern SEXP mapview_brewPopupRowC(SEXP, SEXP, SEXP);
extern SEXP mapview_createTemplate(SEXP);
extern SEXP mapview_df2String(SEXP);
extern SEXP mapview_gsubC(SEXP, SEXP, SEXP);
extern SEXP mapview_listPopupTemplates(SEXP, SEXP, SEXP);
extern SEXP mapview_mergePopupRows(SEXP, SEXP);
extern SEXP mapview_one2JSON(SEXP);
extern SEXP mapview_rowNA(SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"mapview_all2JSONlist",       (DL_FUNC) &mapview_all2JSONlist,       1},
  {"mapview_anyNA",              (DL_FUNC) &mapview_anyNA,              1},
  {"mapview_brewPopupCoords",    (DL_FUNC) &mapview_brewPopupCoords,    2},
  {"mapview_brewPopupRowAltC",   (DL_FUNC) &mapview_brewPopupRowAltC,   3},
  {"mapview_brewPopupRowC",      (DL_FUNC) &mapview_brewPopupRowC,      3},
  {"mapview_createTemplate",     (DL_FUNC) &mapview_createTemplate,     1},
  {"mapview_df2String",          (DL_FUNC) &mapview_df2String,          1},
  {"mapview_gsubC",              (DL_FUNC) &mapview_gsubC,              3},
  {"mapview_listPopupTemplates", (DL_FUNC) &mapview_listPopupTemplates, 3},
  {"mapview_mergePopupRows",     (DL_FUNC) &mapview_mergePopupRows,     2},
  {"mapview_one2JSON",           (DL_FUNC) &mapview_one2JSON,           1},
  {"mapview_rowNA",              (DL_FUNC) &mapview_rowNA,              1},
  {NULL, NULL, 0}
};

void R_init_mapview(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
