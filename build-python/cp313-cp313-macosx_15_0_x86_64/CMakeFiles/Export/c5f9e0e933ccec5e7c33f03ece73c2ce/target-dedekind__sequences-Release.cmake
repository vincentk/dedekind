set_property(TARGET "dedekind::dedekind_sequences"
  PROPERTY IMPORTED_CXX_MODULES_RELEASE
    "dedekind.sequences:curve=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/sequences/curve.cppm"
    "dedekind.sequences:limits=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/sequences/limits.cppm"
    "dedekind.sequences:net=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/sequences/net.cppm"
    "dedekind.sequences:path=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/sequences/path.cppm"
    "dedekind.sequences:ranges=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/sequences/ranges.cppm"
    "dedekind.sequences=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/sequences/sequences.cppm"
)
