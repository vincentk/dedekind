
%"struct.dedekind::optimization::HalfspaceTriple" = type { i32, i32, i32 }

@llvm.global_ctors = appending global [1 x { i32, ptr, ptr }] [{ i32, ptr, ptr } { i32 65535, ptr @_GLOBAL__sub_I_showcase_12b_lp_unimodular_runtime.cpp, ptr null }]

; Function Attrs: mustprogress noinline nounwind ssp uwtable
define i32 @witness_lp_axis_aligned_x(ptr noundef %0, i64 noundef %1, i32 noundef %2, i32 noundef %3) local_unnamed_addr #0 {
  %5 = tail call { i64, i8 } @_ZN8dedekind12optimization6detailW8dedekindW12optimization26maximize_impl_axis_alignedIiEENS0_S3_15VertexCandidateIT_EENSt3__14spanIKNS0_S3_15HalfspaceTripleIS6_EELm18446744073709551615EEES6_S6_(ptr %0, i64 %1, i32 noundef %2, i32 noundef %3)
  %6 = extractvalue { i64, i8 } %5, 0
  %7 = trunc i64 %6 to i32
  ret i32 %7
}

; Function Attrs: mustprogress noinline nounwind ssp uwtable
define i32 @witness_lp_axis_aligned_y(ptr noundef %0, i64 noundef %1, i32 noundef %2, i32 noundef %3) local_unnamed_addr #0 {
  %5 = tail call { i64, i8 } @_ZN8dedekind12optimization6detailW8dedekindW12optimization26maximize_impl_axis_alignedIiEENS0_S3_15VertexCandidateIT_EENSt3__14spanIKNS0_S3_15HalfspaceTripleIS6_EELm18446744073709551615EEES6_S6_(ptr %0, i64 %1, i32 noundef %2, i32 noundef %3)
  %6 = extractvalue { i64, i8 } %5, 0
  %7 = lshr i64 %6, 32
  %8 = trunc nuw i64 %7 to i32
  ret i32 %8
}

; Function Attrs: mustprogress noinline nounwind ssp uwtable
define zeroext i1 @witness_lp_axis_aligned_feasible(ptr noundef %0, i64 noundef %1, i32 noundef %2, i32 noundef %3) local_unnamed_addr #0 {
  %5 = tail call { i64, i8 } @_ZN8dedekind12optimization6detailW8dedekindW12optimization26maximize_impl_axis_alignedIiEENS0_S3_15VertexCandidateIT_EENSt3__14spanIKNS0_S3_15HalfspaceTripleIS6_EELm18446744073709551615EEES6_S6_(ptr %0, i64 %1, i32 noundef %2, i32 noundef %3)
  %6 = extractvalue { i64, i8 } %5, 1
  %7 = trunc i8 %6 to i1
  ret i1 %7
}

; Function Attrs: mustprogress nounwind ssp uwtable
define linkonce_odr { i64, i8 } @_ZN8dedekind12optimization6detailW8dedekindW12optimization26maximize_impl_axis_alignedIiEENS0_S3_15VertexCandidateIT_EENSt3__14spanIKNS0_S3_15HalfspaceTripleIS6_EELm18446744073709551615EEES6_S6_(ptr %0, i64 %1, i32 noundef %2, i32 noundef %3) local_unnamed_addr #1 {
  %5 = icmp eq i64 %1, 0
  br i1 %5, label %95, label %6

6:                                                ; preds = %4, %63
  %7 = phi i8 [ %64, %63 ], [ 0, %4 ]
  %8 = phi i8 [ %65, %63 ], [ 0, %4 ]
  %9 = phi i8 [ %66, %63 ], [ 0, %4 ]
  %10 = phi i8 [ %67, %63 ], [ 0, %4 ]
  %11 = phi i32 [ %68, %63 ], [ 0, %4 ]
  %12 = phi i32 [ %69, %63 ], [ 0, %4 ]
  %13 = phi i64 [ %72, %63 ], [ 0, %4 ]
  %14 = phi i32 [ %70, %63 ], [ 0, %4 ]
  %15 = phi i32 [ %71, %63 ], [ 0, %4 ]
  %16 = getelementptr inbounds nuw %"struct.dedekind::optimization::HalfspaceTriple", ptr %0, i64 %13
  %17 = load i32, ptr %16, align 4, !tbaa !10
  %18 = add i32 %17, 1
  %19 = icmp ult i32 %18, 3
  %20 = getelementptr inbounds nuw i8, ptr %16, i64 4
  %21 = load i32, ptr %20, align 4, !tbaa !12
  %22 = add i32 %21, 1
  %23 = icmp ult i32 %22, 3
  %24 = and i1 %19, %23
  br i1 %24, label %25, label %95

25:                                               ; preds = %6
  %26 = icmp sgt i32 %17, 0
  br i1 %26, label %27, label %33

27:                                               ; preds = %25
  %28 = trunc nuw i8 %8 to i1
  %29 = getelementptr inbounds nuw i8, ptr %16, i64 8
  %30 = load i32, ptr %29, align 4, !tbaa !13
  br i1 %28, label %31, label %63

31:                                               ; preds = %27
  %32 = tail call i32 @llvm.smin.i32(i32 %30, i32 %12)
  br label %63

33:                                               ; preds = %25
  %34 = icmp slt i32 %17, 0
  br i1 %34, label %35, label %44

35:                                               ; preds = %33
  %36 = getelementptr inbounds nuw i8, ptr %16, i64 8
  %37 = load i32, ptr %36, align 4, !tbaa !13
  %38 = sub nsw i32 0, %37
  %39 = trunc nuw i8 %7 to i1
  %40 = icmp sge i32 %11, %38
  %41 = select i1 %39, i1 %40, i1 false
  %42 = select i1 %41, i32 %11, i32 %38
  %43 = select i1 %41, i8 %7, i8 1
  br label %63

44:                                               ; preds = %33
  %45 = icmp sgt i32 %21, 0
  %46 = getelementptr inbounds nuw i8, ptr %16, i64 8
  %47 = load i32, ptr %46, align 4, !tbaa !13
  br i1 %45, label %48, label %52

48:                                               ; preds = %44
  %49 = trunc nuw i8 %10 to i1
  br i1 %49, label %50, label %63

50:                                               ; preds = %48
  %51 = tail call i32 @llvm.smin.i32(i32 %47, i32 %15)
  br label %63

52:                                               ; preds = %44
  %53 = icmp slt i32 %21, 0
  br i1 %53, label %54, label %61

54:                                               ; preds = %52
  %55 = sub nsw i32 0, %47
  %56 = trunc nuw i8 %9 to i1
  %57 = icmp sge i32 %14, %55
  %58 = select i1 %56, i1 %57, i1 false
  %59 = select i1 %58, i32 %14, i32 %55
  %60 = select i1 %58, i8 %9, i8 1
  br label %63

61:                                               ; preds = %52
  %62 = icmp sgt i32 %47, -1
  br i1 %62, label %63, label %95

63:                                               ; preds = %50, %31, %48, %27, %54, %35, %61
  %64 = phi i8 [ %7, %61 ], [ %7, %54 ], [ %7, %31 ], [ %7, %27 ], [ %43, %35 ], [ %7, %50 ], [ %7, %48 ]
  %65 = phi i8 [ %8, %61 ], [ %8, %54 ], [ 1, %31 ], [ 1, %27 ], [ %8, %35 ], [ %8, %50 ], [ %8, %48 ]
  %66 = phi i8 [ %9, %61 ], [ %60, %54 ], [ %9, %31 ], [ %9, %27 ], [ %9, %35 ], [ %9, %50 ], [ %9, %48 ]
  %67 = phi i8 [ %10, %61 ], [ %10, %54 ], [ %10, %31 ], [ %10, %27 ], [ %10, %35 ], [ 1, %50 ], [ 1, %48 ]
  %68 = phi i32 [ %11, %61 ], [ %11, %54 ], [ %11, %31 ], [ %11, %27 ], [ %42, %35 ], [ %11, %50 ], [ %11, %48 ]
  %69 = phi i32 [ %12, %61 ], [ %12, %54 ], [ %32, %31 ], [ %30, %27 ], [ %12, %35 ], [ %12, %50 ], [ %12, %48 ]
  %70 = phi i32 [ %14, %61 ], [ %59, %54 ], [ %14, %31 ], [ %14, %27 ], [ %14, %35 ], [ %14, %50 ], [ %14, %48 ]
  %71 = phi i32 [ %15, %61 ], [ %15, %54 ], [ %15, %31 ], [ %15, %27 ], [ %15, %35 ], [ %51, %50 ], [ %47, %48 ]
  %72 = add nuw i64 %13, 1
  %73 = icmp eq i64 %72, %1
  br i1 %73, label %74, label %6, !llvm.loop !14

74:                                               ; preds = %63
  %75 = trunc nuw i8 %64 to i1
  %76 = trunc nuw i8 %65 to i1
  %77 = select i1 %75, i1 %76, i1 false
  %78 = trunc nuw i8 %66 to i1
  %79 = select i1 %77, i1 %78, i1 false
  %80 = trunc nuw i8 %67 to i1
  %81 = select i1 %79, i1 %80, i1 false
  br i1 %81, label %82, label %95

82:                                               ; preds = %74
  %83 = icmp slt i32 %69, %68
  %84 = icmp slt i32 %71, %70
  %85 = select i1 %83, i1 true, i1 %84
  br i1 %85, label %95, label %86

86:                                               ; preds = %82
  %87 = icmp slt i32 %2, 0
  %88 = select i1 %87, i32 %68, i32 %69
  %89 = icmp slt i32 %3, 0
  %90 = select i1 %89, i32 %70, i32 %71
  %91 = zext i32 %90 to i64
  %92 = shl nuw i64 %91, 32
  %93 = zext i32 %88 to i64
  %94 = or disjoint i64 %92, %93
  br label %95

95:                                               ; preds = %6, %61, %4, %82, %74, %86
  %96 = phi i64 [ 0, %74 ], [ %94, %86 ], [ 0, %82 ], [ 0, %4 ], [ 0, %61 ], [ 0, %6 ]
  %97 = phi i8 [ 0, %74 ], [ 1, %86 ], [ 0, %82 ], [ 0, %4 ], [ 0, %61 ], [ 0, %6 ]
  %98 = insertvalue { i64, i8 } poison, i64 %96, 0
  %99 = insertvalue { i64, i8 } %98, i8 %97, 1
  ret { i64, i8 } %99
}

declare void @_ZGIW8dedekindW12optimization() local_unnamed_addr

; Function Attrs: ssp uwtable
define internal void @_GLOBAL__sub_I_showcase_12b_lp_unimodular_runtime.cpp() #2 section "__TEXT,__StaticInit,regular,pure_instructions" {
  tail call void @_ZGIW8dedekindW12optimization()
  ret void
}

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare i32 @llvm.smin.i32(i32, i32) #3

attributes #0 = { mustprogress noinline nounwind ssp uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cmov,+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "tune-cpu"="generic" }
attributes #1 = { mustprogress nounwind ssp uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cmov,+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "tune-cpu"="generic" }
attributes #2 = { ssp uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cmov,+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "tune-cpu"="generic" }
attributes #3 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }

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
!10 = !{!11, !7, i64 0}
!11 = !{!"_ZTSN8dedekind12optimizationW8dedekindW12optimization15HalfspaceTripleIiEE", !7, i64 0, !7, i64 4, !7, i64 8}
!12 = !{!11, !7, i64 4}
!13 = !{!11, !7, i64 8}
!14 = distinct !{!14, !15}
!15 = !{!"llvm.loop.mustprogress"}
