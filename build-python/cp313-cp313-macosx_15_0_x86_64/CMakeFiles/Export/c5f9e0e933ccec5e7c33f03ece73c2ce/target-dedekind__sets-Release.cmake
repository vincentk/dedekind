set_property(TARGET "dedekind::dedekind_sets"
  PROPERTY IMPORTED_CXX_MODULES_RELEASE
    "dedekind.sets:boundaries=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/sets/boundaries.cppm"
    "dedekind.sets:expressions=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/sets/expressions.cppm"
    "dedekind.sets:family=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/sets/family.cppm"
    "dedekind.sets:interop=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/sets/interop.cppm"
    "dedekind.sets:mereology=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/sets/mereology.cppm"
    "dedekind.sets:relational=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/sets/relational.cppm"
    "dedekind.sets=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/sets/sets.cppm"
    "dedekind.sets:singleton=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/sets/singleton.cppm"
)
