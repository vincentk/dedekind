
%"struct.dedekind::sequences::Path" = type { %"class.std::function", i64 }
%"class.std::function" = type { %"class.std::_Function_base", ptr }
%"class.std::_Function_base" = type { %"union.std::_Any_data", ptr }
%"union.std::_Any_data" = type { %"union.std::_Nocopy_types" }
%"union.std::_Nocopy_types" = type { { i64, i64 } }

$_ZN8dedekind9sequencesW8dedekindW9sequences7is_evenE = comdat any

$"_ZN8dedekind9sequencesW8dedekindW9sequences4PathINS_8categoryS1_W8category5TruthINS4_S5_5BooleEEENS_4setsS1_W4sets3\E2\84\B5ILm0EEENS9_SA_19ExtensionalCardinalImEEED2Ev" = comdat any

$_ZNSt17_Function_handlerIFN8dedekind8categoryW8dedekindW8category5TruthINS1_S3_5BooleEEENS0_4setsS2_W4sets19ExtensionalCardinalImEEENS0_9sequencesS2_W9sequences7is_evenMUlSA_E_EE9_M_invokeERKSt9_Any_dataOSA_ = comdat any

$_ZNSt17_Function_handlerIFN8dedekind8categoryW8dedekindW8category5TruthINS1_S3_5BooleEEENS0_4setsS2_W4sets19ExtensionalCardinalImEEENS0_9sequencesS2_W9sequences7is_evenMUlSA_E_EE10_M_managerERSt9_Any_dataRKSH_St18_Manager_operation = comdat any

$__clang_call_terminate = comdat any

$_ZTIN8dedekind9sequencesW8dedekindW9sequences7is_evenMUlNS_4setsS1_W4sets19ExtensionalCardinalImEEE_E = comdat any

$_ZTSN8dedekind9sequencesW8dedekindW9sequences7is_evenMUlNS_4setsS1_W4sets19ExtensionalCardinalImEEE_E = comdat any

@_ZN8dedekind9sequencesW8dedekindW9sequences7is_evenE = linkonce_odr dso_local global %"struct.dedekind::sequences::Path" zeroinitializer, comdat, align 8
@_ZGVN8dedekind9sequencesW8dedekindW9sequences7is_evenE = linkonce_odr dso_local global i64 0, comdat($_ZN8dedekind9sequencesW8dedekindW9sequences7is_evenE), align 8
@__dso_handle = external hidden global i8
@_ZTIN8dedekind9sequencesW8dedekindW9sequences7is_evenMUlNS_4setsS1_W4sets19ExtensionalCardinalImEEE_E = linkonce_odr dso_local constant { ptr, ptr } { ptr getelementptr inbounds (ptr, ptr @_ZTVN10__cxxabiv117__class_type_infoE, i64 2), ptr @_ZTSN8dedekind9sequencesW8dedekindW9sequences7is_evenMUlNS_4setsS1_W4sets19ExtensionalCardinalImEEE_E }, comdat, align 8
@_ZTVN10__cxxabiv117__class_type_infoE = external global [0 x ptr]
@_ZTSN8dedekind9sequencesW8dedekindW9sequences7is_evenMUlNS_4setsS1_W4sets19ExtensionalCardinalImEEE_E = linkonce_odr dso_local constant [98 x i8] c"N8dedekind9sequencesW8dedekindW9sequences7is_evenMUlNS_4setsS1_W4sets19ExtensionalCardinalImEEE_E\00", comdat, align 1
@llvm.global_ctors = appending global [2 x { i32, ptr, ptr }] [{ i32, ptr, ptr } { i32 65535, ptr @__cxx_global_var_init, ptr @_ZN8dedekind9sequencesW8dedekindW9sequences7is_evenE }, { i32, ptr, ptr } { i32 65535, ptr @_GLOBAL__sub_I_showcase_02_lattice_singleton.cpp, ptr null }]
@llvm.used = appending global [1 x ptr] [ptr @_ZN8dedekind9sequencesW8dedekindW9sequences7is_evenE], section "llvm.metadata"

; Function Attrs: mustprogress nofree noinline norecurse nosync nounwind willreturn memory(none) uwtable
define dso_local noundef zeroext i1 @witness_lattice_square_singleton() local_unnamed_addr #0 {
  ret i1 true
}

; Function Attrs: nofree nounwind uwtable
define internal void @__cxx_global_var_init() #1 section ".text.startup" comdat($_ZN8dedekind9sequencesW8dedekindW9sequences7is_evenE) personality ptr @__gxx_personality_v0 {
  %1 = load atomic i8, ptr @_ZGVN8dedekind9sequencesW8dedekindW9sequences7is_evenE acquire, align 8
  %2 = icmp eq i8 %1, 0
  br i1 %2, label %3, label %8

3:                                                ; preds = %0
  %4 = tail call i32 @__cxa_guard_acquire(ptr nonnull @_ZGVN8dedekind9sequencesW8dedekindW9sequences7is_evenE) #9
  %5 = icmp eq i32 %4, 0
  br i1 %5, label %8, label %6

6:                                                ; preds = %3
  tail call void @llvm.memset.p0.i64(ptr noundef nonnull align 8 dereferenceable(16) @_ZN8dedekind9sequencesW8dedekindW9sequences7is_evenE, i8 0, i64 16, i1 false)
  store ptr @_ZNSt17_Function_handlerIFN8dedekind8categoryW8dedekindW8category5TruthINS1_S3_5BooleEEENS0_4setsS2_W4sets19ExtensionalCardinalImEEENS0_9sequencesS2_W9sequences7is_evenMUlSA_E_EE9_M_invokeERKSt9_Any_dataOSA_, ptr getelementptr inbounds nuw (i8, ptr @_ZN8dedekind9sequencesW8dedekindW9sequences7is_evenE, i64 24), align 8, !tbaa !9
  store ptr @_ZNSt17_Function_handlerIFN8dedekind8categoryW8dedekindW8category5TruthINS1_S3_5BooleEEENS0_4setsS2_W4sets19ExtensionalCardinalImEEENS0_9sequencesS2_W9sequences7is_evenMUlSA_E_EE10_M_managerERSt9_Any_dataRKSH_St18_Manager_operation, ptr getelementptr inbounds nuw (i8, ptr @_ZN8dedekind9sequencesW8dedekindW9sequences7is_evenE, i64 16), align 8, !tbaa !13
  store i64 0, ptr getelementptr inbounds nuw (i8, ptr @_ZN8dedekind9sequencesW8dedekindW9sequences7is_evenE, i64 32), align 8, !tbaa !14
  %7 = tail call i32 @__cxa_atexit(ptr nonnull @"_ZN8dedekind9sequencesW8dedekindW9sequences4PathINS_8categoryS1_W8category5TruthINS4_S5_5BooleEEENS_4setsS1_W4sets3\E2\84\B5ILm0EEENS9_SA_19ExtensionalCardinalImEEED2Ev", ptr nonnull @_ZN8dedekind9sequencesW8dedekindW9sequences7is_evenE, ptr nonnull @__dso_handle) #9
  tail call void @__cxa_guard_release(ptr nonnull @_ZGVN8dedekind9sequencesW8dedekindW9sequences7is_evenE) #9
  br label %8

8:                                                ; preds = %6, %3, %0
  ret void
}

; Function Attrs: nofree nounwind
declare i32 @__cxa_guard_acquire(ptr) local_unnamed_addr #2

; Function Attrs: inlinehint mustprogress nounwind uwtable
define linkonce_odr dso_local void @"_ZN8dedekind9sequencesW8dedekindW9sequences4PathINS_8categoryS1_W8category5TruthINS4_S5_5BooleEEENS_4setsS1_W4sets3\E2\84\B5ILm0EEENS9_SA_19ExtensionalCardinalImEEED2Ev"(ptr noundef nonnull align 8 dereferenceable(40) %0) unnamed_addr #3 comdat align 2 personality ptr @__gxx_personality_v0 {
  %2 = getelementptr inbounds nuw i8, ptr %0, i64 16
  %3 = load ptr, ptr %2, align 8, !tbaa !13
  %4 = icmp eq ptr %3, null
  br i1 %4, label %10, label %5

5:                                                ; preds = %1
  %6 = invoke noundef zeroext i1 %3(ptr noundef nonnull align 8 dereferenceable(32) %0, ptr noundef nonnull align 8 dereferenceable(32) %0, i32 noundef 3)
          to label %10 unwind label %7

7:                                                ; preds = %5
  %8 = landingpad { ptr, i32 }
          catch ptr null
  %9 = extractvalue { ptr, i32 } %8, 0
  tail call void @__clang_call_terminate(ptr %9) #10
  unreachable

10:                                               ; preds = %1, %5
  ret void
}

; Function Attrs: nofree nounwind
declare i32 @__cxa_atexit(ptr, ptr, ptr) local_unnamed_addr #2

; Function Attrs: nofree nounwind
declare void @__cxa_guard_release(ptr) local_unnamed_addr #2

; Function Attrs: mustprogress nocallback nofree nounwind willreturn memory(argmem: write)
declare void @llvm.memset.p0.i64(ptr writeonly captures(none), i8, i64, i1 immarg) #4

; Function Attrs: mustprogress uwtable
define linkonce_odr dso_local i8 @_ZNSt17_Function_handlerIFN8dedekind8categoryW8dedekindW8category5TruthINS1_S3_5BooleEEENS0_4setsS2_W4sets19ExtensionalCardinalImEEENS0_9sequencesS2_W9sequences7is_evenMUlSA_E_EE9_M_invokeERKSt9_Any_dataOSA_(ptr noundef nonnull align 8 dereferenceable(16) %0, ptr noundef nonnull align 8 dereferenceable(8) %1) #5 comdat align 2 {
  %3 = load i64, ptr %1, align 8, !tbaa !17
  %4 = trunc i64 %3 to i8
  %5 = and i8 %4, 1
  %6 = xor i8 %5, 1
  ret i8 %6
}

; Function Attrs: mustprogress uwtable
define linkonce_odr dso_local noundef zeroext i1 @_ZNSt17_Function_handlerIFN8dedekind8categoryW8dedekindW8category5TruthINS1_S3_5BooleEEENS0_4setsS2_W4sets19ExtensionalCardinalImEEENS0_9sequencesS2_W9sequences7is_evenMUlSA_E_EE10_M_managerERSt9_Any_dataRKSH_St18_Manager_operation(ptr noundef nonnull align 8 dereferenceable(16) %0, ptr noundef nonnull align 8 dereferenceable(16) %1, i32 noundef %2) #5 comdat align 2 personality ptr @__gxx_personality_v0 {
  switch i32 %2, label %7 [
    i32 0, label %5
    i32 1, label %4
  ]

4:                                                ; preds = %3
  br label %5

5:                                                ; preds = %3, %4
  %6 = phi ptr [ %1, %4 ], [ @_ZTIN8dedekind9sequencesW8dedekindW9sequences7is_evenMUlNS_4setsS1_W4sets19ExtensionalCardinalImEEE_E, %3 ]
  store ptr %6, ptr %0, align 8, !tbaa !18
  br label %7

7:                                                ; preds = %5, %3
  ret i1 false
}

declare i32 @__gxx_personality_v0(...)

; Function Attrs: noinline noreturn nounwind uwtable
define linkonce_odr hidden void @__clang_call_terminate(ptr noundef %0) local_unnamed_addr #6 comdat {
  %2 = tail call ptr @__cxa_begin_catch(ptr %0) #9
  tail call void @_ZSt9terminatev() #10
  unreachable
}

declare ptr @__cxa_begin_catch(ptr) local_unnamed_addr

; Function Attrs: cold nofree noreturn
declare void @_ZSt9terminatev() local_unnamed_addr #7

declare void @_ZGIW8dedekindW8category() local_unnamed_addr

declare void @_ZGIW8dedekindW4sets() local_unnamed_addr

declare void @_ZGIW8dedekindW7algebra() local_unnamed_addr

declare void @_ZGIW8dedekindW7numbers() local_unnamed_addr

declare void @_ZGIW8dedekindW5order() local_unnamed_addr

; Function Attrs: uwtable
define internal void @_GLOBAL__sub_I_showcase_02_lattice_singleton.cpp() #8 section ".text.startup" {
  tail call void @_ZGIW8dedekindW8category()
  tail call void @_ZGIW8dedekindW4sets()
  tail call void @_ZGIW8dedekindW7algebra()
  tail call void @_ZGIW8dedekindW7numbers()
  tail call void @_ZGIW8dedekindW5order()
  ret void
}

attributes #0 = { mustprogress nofree noinline norecurse nosync nounwind willreturn memory(none) uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { nofree nounwind uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #2 = { nofree nounwind }
attributes #3 = { inlinehint mustprogress nounwind uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #4 = { mustprogress nocallback nofree nounwind willreturn memory(argmem: write) }
attributes #5 = { mustprogress uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #6 = { noinline noreturn nounwind uwtable "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #7 = { cold nofree noreturn }
attributes #8 = { uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #9 = { nounwind }
attributes #10 = { noreturn nounwind }

!llvm.module.flags = !{!0, !1, !2, !3}
!llvm.errno.tbaa = !{!5}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 8, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 2}
!5 = !{!6, !6, i64 0}
!6 = !{!"int", !7, i64 0}
!7 = !{!"omnipotent char", !8, i64 0}
!8 = !{!"Simple C++ TBAA"}
!9 = !{!10, !12, i64 24}
!10 = !{!"_ZTSSt8functionIFN8dedekind8categoryW8dedekindW8category5TruthINS1_S3_5BooleEEENS0_4setsS2_W4sets19ExtensionalCardinalImEEEE", !11, i64 0, !12, i64 24}
!11 = !{!"_ZTSSt14_Function_base", !7, i64 0, !12, i64 16}
!12 = !{!"any pointer", !7, i64 0}
!13 = !{!11, !12, i64 16}
!14 = !{!15, !16, i64 32}
!15 = !{!"_ZTSN8dedekind9sequencesW8dedekindW9sequences4PathINS_8categoryS1_W8category5TruthINS4_S5_5BooleEEENS_4setsS1_W4sets3\E2\84\B5ILm0EEENS9_SA_19ExtensionalCardinalImEEEE", !10, i64 0, !16, i64 32}
!16 = !{!"long", !7, i64 0}
!17 = !{!16, !16, i64 0}
!18 = !{!12, !12, i64 0}
