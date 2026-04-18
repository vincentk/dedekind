set_property(TARGET "dedekind::dedekind_ieee"
  PROPERTY IMPORTED_CXX_MODULES_RELEASE
    "dedekind.ieee.approx=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/ieee/approx.cppm"
    "dedekind.ieee=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/ieee/ieee.cppm"
)
