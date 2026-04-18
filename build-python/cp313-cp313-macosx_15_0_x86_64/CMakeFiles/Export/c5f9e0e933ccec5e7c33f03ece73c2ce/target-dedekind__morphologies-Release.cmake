set_property(TARGET "dedekind::dedekind_morphologies"
  PROPERTY IMPORTED_CXX_MODULES_RELEASE
    "dedekind.morphologies:archimedean=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/morphologies/archimedean.cppm"
    "dedekind.morphologies:cyclic=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/morphologies/cyclic.cppm"
    "dedekind.morphologies=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/morphologies/morphologies.cppm"
)
