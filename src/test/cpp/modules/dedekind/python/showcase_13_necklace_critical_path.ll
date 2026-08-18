
%"struct.dedekind::sequences::FiniteSeq" = type { %"struct.std::__1::array", i64 }
%"struct.std::__1::array" = type { [32 x %"struct.dedekind::optimization::Edge"] }
%"struct.dedekind::optimization::Edge" = type { i64, i64 }
%"struct.dedekind::algebra::Tropical" = type { i8, i64 }
%"struct.dedekind::sequences::FiniteNet" = type { %"struct.std::__1::array.1" }
%"struct.std::__1::array.1" = type { [27 x %"struct.dedekind::algebra::Tropical"] }

@_ZN12_GLOBAL__N_114necklace_edgesE = internal unnamed_addr constant %"struct.dedekind::sequences::FiniteSeq" { %"struct.std::__1::array" { [32 x %"struct.dedekind::optimization::Edge"] [%"struct.dedekind::optimization::Edge" { i64 0, i64 4 }, %"struct.dedekind::optimization::Edge" { i64 1, i64 3 }, %"struct.dedekind::optimization::Edge" { i64 1, i64 5 }, %"struct.dedekind::optimization::Edge" { i64 2, i64 4 }, %"struct.dedekind::optimization::Edge" { i64 3, i64 7 }, %"struct.dedekind::optimization::Edge" { i64 4, i64 6 }, %"struct.dedekind::optimization::Edge" { i64 4, i64 8 }, %"struct.dedekind::optimization::Edge" { i64 5, i64 7 }, %"struct.dedekind::optimization::Edge" { i64 6, i64 10 }, %"struct.dedekind::optimization::Edge" { i64 7, i64 9 }, %"struct.dedekind::optimization::Edge" { i64 7, i64 11 }, %"struct.dedekind::optimization::Edge" { i64 8, i64 10 }, %"struct.dedekind::optimization::Edge" { i64 9, i64 13 }, %"struct.dedekind::optimization::Edge" { i64 10, i64 12 }, %"struct.dedekind::optimization::Edge" { i64 10, i64 14 }, %"struct.dedekind::optimization::Edge" { i64 11, i64 13 }, %"struct.dedekind::optimization::Edge" { i64 12, i64 16 }, %"struct.dedekind::optimization::Edge" { i64 13, i64 15 }, %"struct.dedekind::optimization::Edge" { i64 13, i64 17 }, %"struct.dedekind::optimization::Edge" { i64 14, i64 16 }, %"struct.dedekind::optimization::Edge" { i64 15, i64 19 }, %"struct.dedekind::optimization::Edge" { i64 16, i64 18 }, %"struct.dedekind::optimization::Edge" { i64 16, i64 20 }, %"struct.dedekind::optimization::Edge" { i64 17, i64 19 }, %"struct.dedekind::optimization::Edge" { i64 18, i64 22 }, %"struct.dedekind::optimization::Edge" { i64 19, i64 21 }, %"struct.dedekind::optimization::Edge" { i64 19, i64 23 }, %"struct.dedekind::optimization::Edge" { i64 20, i64 22 }, %"struct.dedekind::optimization::Edge" { i64 21, i64 25 }, %"struct.dedekind::optimization::Edge" { i64 22, i64 24 }, %"struct.dedekind::optimization::Edge" { i64 22, i64 26 }, %"struct.dedekind::optimization::Edge" { i64 23, i64 25 }] }, i64 32 }, align 8
@_ZN8dedekind8categoryW8dedekindW8category10identity_vINS_7algebraS1_W7algebra8TropicalIyLb1EEENSt3__14plusIS7_EEEE = linkonce_odr local_unnamed_addr constant %"struct.dedekind::algebra::Tropical" { i8 1, i64 0 }, align 8
@llvm.global_ctors = appending global [1 x { i32, ptr, ptr }] [{ i32, ptr, ptr } { i32 65535, ptr @_GLOBAL__sub_I_showcase_13_necklace_critical_path.cpp, ptr null }]

; Function Attrs: mustprogress nofree noinline norecurse nosync nounwind ssp willreturn memory(none) uwtable
define noundef i64 @witness_necklace_reachable() local_unnamed_addr #0 {
  ret i64 1
}

; Function Attrs: mustprogress nofree noinline norecurse nosync nounwind ssp willreturn memory(none) uwtable
define noundef i64 @witness_necklace_critical() local_unnamed_addr #0 {
  ret i64 8
}

; Function Attrs: mustprogress nofree noinline norecurse nosync nounwind ssp willreturn memory(none) uwtable
define noundef i64 @witness_necklace_sensitivity_critical() local_unnamed_addr #0 {
  ret i64 1
}

; Function Attrs: mustprogress nofree noinline norecurse nosync nounwind ssp willreturn memory(none) uwtable
define noundef i64 @witness_necklace_sensitivity_floated() local_unnamed_addr #0 {
  ret i64 0
}

; Function Attrs: mustprogress nofree noinline norecurse nosync nounwind ssp willreturn memory(none) uwtable
define i64 @witness_necklace_critical_between(i64 noundef %0, i64 noundef %1) local_unnamed_addr #0 {
  %3 = alloca %"struct.dedekind::sequences::FiniteNet", align 8
  %4 = icmp ugt i64 %0, 26
  %5 = icmp ugt i64 %1, 26
  %6 = or i1 %4, %5
  br i1 %6, label %47, label %7

7:                                                ; preds = %2
  call void @llvm.lifetime.start.p0(ptr nonnull %3)
  %8 = getelementptr inbounds nuw %"struct.dedekind::algebra::Tropical", ptr %3, i64 %0
  call void @llvm.memset.p0.i64(ptr noundef nonnull align 8 dereferenceable(432) %3, i8 0, i64 432, i1 false)
  call void @llvm.memcpy.p0.p0.i64(ptr noundef nonnull align 8 dereferenceable(16) %8, ptr noundef nonnull align 8 dereferenceable(16) @_ZN8dedekind8categoryW8dedekindW8category10identity_vINS_7algebraS1_W7algebra8TropicalIyLb1EEENSt3__14plusIS7_EEEE, i64 16, i1 false)
  br label %9

9:                                                ; preds = %9, %7
  %10 = phi i64 [ 0, %7 ], [ %41, %9 ]
  %11 = getelementptr inbounds nuw i8, ptr @_ZN12_GLOBAL__N_114necklace_edgesE, i64 %10
  %12 = getelementptr inbounds nuw i8, ptr %11, i64 8
  %13 = load i64, ptr %12, align 8, !tbaa !10, !noalias !13
  %14 = getelementptr inbounds nuw %"struct.dedekind::algebra::Tropical", ptr %3, i64 %13
  %15 = load i8, ptr %14, align 8
  %16 = getelementptr inbounds nuw i8, ptr %14, i64 8
  %17 = load i64, ptr %16, align 8
  %18 = load i64, ptr %11, align 8, !tbaa !16, !noalias !13
  %19 = getelementptr inbounds nuw %"struct.dedekind::algebra::Tropical", ptr %3, i64 %18
  %20 = load i8, ptr %19, align 8
  %21 = getelementptr inbounds nuw i8, ptr %19, i64 8
  %22 = load i64, ptr %21, align 8
  %23 = trunc i64 %13 to i32
  %24 = sdiv i32 %23, -3
  %25 = srem i32 %23, 3
  %26 = add nsw i32 %25, -1
  %27 = sext i32 %26 to i64
  %28 = add nsw i32 %24, 4
  %29 = sext i32 %28 to i64
  %30 = mul nsw i64 %27, %29
  %31 = tail call i64 @llvm.smax.i64(i64 %30, i64 0)
  %32 = trunc nuw i8 %20 to i1
  %33 = add i64 %31, %22
  %34 = and i8 %20, 1
  %35 = select i1 %32, i64 %33, i64 0
  %36 = trunc nuw i8 %15 to i1
  %37 = tail call i64 @llvm.umax.i64(i64 %17, i64 %33)
  %38 = select i1 %36, i8 1, i8 %34
  %39 = select i1 %32, i64 %37, i64 %17
  %40 = select i1 %36, i64 %39, i64 %35
  store i8 %38, ptr %14, align 8
  store i64 %40, ptr %16, align 8
  %41 = add nuw nsw i64 %10, 16
  %42 = icmp eq i64 %41, 512
  br i1 %42, label %43, label %9

43:                                               ; preds = %9
  %44 = getelementptr inbounds nuw %"struct.dedekind::algebra::Tropical", ptr %3, i64 %1
  %45 = getelementptr inbounds nuw i8, ptr %44, i64 8
  %46 = load i64, ptr %45, align 8
  call void @llvm.lifetime.end.p0(ptr nonnull %3)
  br label %47

47:                                               ; preds = %2, %43
  %48 = phi i64 [ %46, %43 ], [ -1, %2 ]
  ret i64 %48
}

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.start.p0(ptr captures(none)) #1

; Function Attrs: mustprogress nocallback nofree nosync nounwind willreturn memory(argmem: readwrite)
declare void @llvm.lifetime.end.p0(ptr captures(none)) #1

; Function Attrs: mustprogress nofree noinline norecurse nosync nounwind ssp willreturn memory(none) uwtable
define noundef i64 @witness_necklace_path_value() local_unnamed_addr #0 {
  ret i64 8
}

; Function Attrs: mustprogress nocallback nofree nounwind willreturn memory(argmem: write)
declare void @llvm.memset.p0.i64(ptr writeonly captures(none), i8, i64, i1 immarg) #2

; Function Attrs: mustprogress nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias writeonly captures(none), ptr noalias readonly captures(none), i64, i1 immarg) #3

declare void @_ZGIW8dedekindW7algebra() local_unnamed_addr

declare void @_ZGIW8dedekindW8analysis() local_unnamed_addr

declare void @_ZGIW8dedekindW12optimization() local_unnamed_addr

declare void @_ZGIW8dedekindW9sequences() local_unnamed_addr

; Function Attrs: ssp uwtable
define internal void @_GLOBAL__sub_I_showcase_13_necklace_critical_path.cpp() #4 section "__TEXT,__StaticInit,regular,pure_instructions" {
  tail call void @_ZGIW8dedekindW7algebra()
  tail call void @_ZGIW8dedekindW8analysis()
  tail call void @_ZGIW8dedekindW12optimization()
  tail call void @_ZGIW8dedekindW9sequences()
  ret void
}

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare i64 @llvm.smax.i64(i64, i64) #5

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare i64 @llvm.umax.i64(i64, i64) #5

attributes #0 = { mustprogress nofree noinline norecurse nosync nounwind ssp willreturn memory(none) uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cmov,+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "tune-cpu"="generic" }
attributes #1 = { mustprogress nocallback nofree nosync nounwind willreturn memory(argmem: readwrite) }
attributes #2 = { mustprogress nocallback nofree nounwind willreturn memory(argmem: write) }
attributes #3 = { mustprogress nocallback nofree nounwind willreturn memory(argmem: readwrite) }
attributes #4 = { ssp uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cmov,+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "tune-cpu"="generic" }
attributes #5 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }

!llvm.module.flags = !{!0, !1, !2, !3, !4}
!llvm.errno.tbaa = !{!6}

!0 = !{i32 2, !"SDK Version", [2 x i32] [i32 26, i32 1]}
!1 = !{i32 1, !"wchar_size", i32 4}
!2 = !{i32 8, !"PIC Level", i32 2}
!3 = !{i32 7, !"uwtable", i32 2}
!4 = !{i32 7, !"frame-pointer", i32 2}
!6 = !{!7, !7, i64 0}
!7 = !{!"int", !8, i64 0}
!8 = !{!"omnipotent char", !9, i64 0}
!9 = !{!"Simple C++ TBAA"}
!10 = !{!11, !12, i64 8}
!11 = !{!"_ZTSN8dedekind12optimizationW8dedekindW12optimization4EdgeE", !12, i64 0, !12, i64 8}
!12 = !{!"long", !8, i64 0}
!13 = !{!14}
!14 = distinct !{!14, !15, !"_ZN8dedekind9sequencesW8dedekindW9sequences4foldINS0_S2_9FiniteSeqINS_12optimizationS1_W12optimization4EdgeELm32EEENS0_S2_9FiniteNetINS_7algebraS1_W7algebra8TropicalIyLb1EEELm27EEEZNS5_S6_16semiring_closureISD_Lm27ENSA_SB_12TropicalPlusENSt3__14plusISD_EES8_3$_1Qsr8dedekind8categoryE10IsSemiringIT_T1_T2_EEESL_mmRKT3_T4_EUlRSE_RKS7_E_EET0_RKSL_SW_SM_: argument 0"}
!15 = distinct !{!15, !"_ZN8dedekind9sequencesW8dedekindW9sequences4foldINS0_S2_9FiniteSeqINS_12optimizationS1_W12optimization4EdgeELm32EEENS0_S2_9FiniteNetINS_7algebraS1_W7algebra8TropicalIyLb1EEELm27EEEZNS5_S6_16semiring_closureISD_Lm27ENSA_SB_12TropicalPlusENSt3__14plusISD_EES8_3$_1Qsr8dedekind8categoryE10IsSemiringIT_T1_T2_EEESL_mmRKT3_T4_EUlRSE_RKS7_E_EET0_RKSL_SW_SM_"}
!16 = !{!11, !12, i64 0}
