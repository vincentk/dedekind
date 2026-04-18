set_property(TARGET "dedekind::dedekind_geometry"
  PROPERTY IMPORTED_CXX_MODULES_RELEASE
    "dedekind.geometry:affine=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/geometry/affine.cppm"
    "dedekind.geometry:euclidean=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/geometry/euclidean.cppm"
    "dedekind.geometry=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/geometry/geometry.cppm"
    "dedekind.geometry:hilbert=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/geometry/hilbert.cppm"
    "dedekind.geometry:inner_product=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/geometry/inner_product.cppm"
    "dedekind.geometry:lattice=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/geometry/lattice.cppm"
    "dedekind.geometry:linear_map=${_IMPORT_PREFIX}/lib/modules/dedekind/dedekind/geometry/linear_map.cppm"
)
