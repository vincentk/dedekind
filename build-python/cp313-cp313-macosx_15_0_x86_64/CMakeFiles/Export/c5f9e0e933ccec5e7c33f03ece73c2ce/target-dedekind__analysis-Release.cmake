set_property(TARGET "dedekind::dedekind_analysis"
  PROPERTY IMPORTED_CXX_MODULES_RELEASE
    "dedekind.analysis=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/analysis/analysis.cppm"
    "dedekind.analysis:exterior=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/analysis/exterior.cppm"
    "dedekind.analysis:forms=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/analysis/forms.cppm"
    "dedekind.analysis:hamilton=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/analysis/hamilton.cppm"
    "dedekind.analysis:kernels=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/analysis/kernels.cppm"
)
