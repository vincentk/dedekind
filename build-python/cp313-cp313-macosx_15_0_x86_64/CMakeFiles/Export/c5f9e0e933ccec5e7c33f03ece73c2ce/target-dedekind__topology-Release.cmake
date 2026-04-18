set_property(TARGET "dedekind::dedekind_topology"
  PROPERTY IMPORTED_CXX_MODULES_RELEASE
    "dedekind.topology:interval=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/topology/interval.cppm"
    "dedekind.topology:neighborhood=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/topology/neighborhood.cppm"
    "dedekind.topology=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/topology/topology.cppm"
)
