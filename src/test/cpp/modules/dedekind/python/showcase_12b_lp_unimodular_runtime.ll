
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
  br i1 %5, label %99, label %6

6:                                                ; preds = %4, %67
  %7 = phi i8 [ %68, %67 ], [ 0, %4 ]
  %8 = phi i8 [ %69, %67 ], [ 0, %4 ]
  %9 = phi i8 [ %70, %67 ], [ 0, %4 ]
  %10 = phi i8 [ %71, %67 ], [ 0, %4 ]
  %11 = phi i32 [ %72, %67 ], [ 0, %4 ]
  %12 = phi i32 [ %73, %67 ], [ 0, %4 ]
  %13 = phi i64 [ %76, %67 ], [ 0, %4 ]
  %14 = phi i32 [ %74, %67 ], [ 0, %4 ]
  %15 = phi i32 [ %75, %67 ], [ 0, %4 ]
  %16 = getelementptr inbounds nuw %"struct.dedekind::optimization::HalfspaceTriple", ptr %0, i64 %13
  %17 = load i32, ptr %16, align 4, !tbaa !10
  %18 = add i32 %17, 1
  %19 = icmp ult i32 %18, 3
  %20 = getelementptr inbounds nuw i8, ptr %16, i64 4
  %21 = load i32, ptr %20, align 4, !tbaa !12
  %22 = add i32 %21, 1
  %23 = icmp ult i32 %22, 3
  %24 = and i1 %19, %23
  br i1 %24, label %25, label %99

25:                                               ; preds = %6
  %26 = icmp eq i32 %17, 0
  br i1 %26, label %33, label %27

27:                                               ; preds = %25
  %28 = icmp eq i32 %21, 0
  br i1 %28, label %29, label %99

29:                                               ; preds = %27
  %30 = getelementptr inbounds nuw i8, ptr %16, i64 8
  %31 = load i32, ptr %30, align 4, !tbaa !13
  %32 = icmp eq i32 %31, -2147483648
  br i1 %32, label %99, label %37

33:                                               ; preds = %25
  %34 = getelementptr inbounds nuw i8, ptr %16, i64 8
  %35 = load i32, ptr %34, align 4, !tbaa !13
  %36 = icmp eq i32 %35, -2147483648
  br i1 %36, label %99, label %50

37:                                               ; preds = %29
  %38 = icmp sgt i32 %17, 0
  br i1 %38, label %39, label %43

39:                                               ; preds = %37
  %40 = trunc nuw i8 %8 to i1
  %41 = tail call i32 @llvm.smin.i32(i32 %31, i32 %12)
  %42 = select i1 %40, i32 %41, i32 %31
  br label %67

43:                                               ; preds = %37
  %44 = sub nsw i32 0, %31
  %45 = trunc nuw i8 %7 to i1
  %46 = icmp sge i32 %11, %44
  %47 = select i1 %45, i1 %46, i1 false
  %48 = select i1 %47, i32 %11, i32 %44
  %49 = select i1 %47, i8 %7, i8 1
  br label %67

50:                                               ; preds = %33
  %51 = icmp sgt i32 %21, 0
  br i1 %51, label %52, label %56

52:                                               ; preds = %50
  %53 = trunc nuw i8 %10 to i1
  %54 = tail call i32 @llvm.smin.i32(i32 %35, i32 %15)
  %55 = select i1 %53, i32 %54, i32 %35
  br label %67

56:                                               ; preds = %50
  %57 = icmp slt i32 %21, 0
  br i1 %57, label %58, label %65

58:                                               ; preds = %56
  %59 = sub nsw i32 0, %35
  %60 = trunc nuw i8 %9 to i1
  %61 = icmp sge i32 %14, %59
  %62 = select i1 %60, i1 %61, i1 false
  %63 = select i1 %62, i32 %14, i32 %59
  %64 = select i1 %62, i8 %9, i8 1
  br label %67

65:                                               ; preds = %56
  %66 = icmp sgt i32 %35, -1
  br i1 %66, label %67, label %99

67:                                               ; preds = %52, %39, %58, %43, %65
  %68 = phi i8 [ %7, %65 ], [ %7, %58 ], [ %7, %39 ], [ %7, %52 ], [ %49, %43 ]
  %69 = phi i8 [ %8, %65 ], [ %8, %58 ], [ 1, %39 ], [ %8, %52 ], [ %8, %43 ]
  %70 = phi i8 [ %9, %65 ], [ %64, %58 ], [ %9, %39 ], [ %9, %52 ], [ %9, %43 ]
  %71 = phi i8 [ %10, %65 ], [ %10, %58 ], [ %10, %39 ], [ 1, %52 ], [ %10, %43 ]
  %72 = phi i32 [ %11, %65 ], [ %11, %58 ], [ %11, %39 ], [ %11, %52 ], [ %48, %43 ]
  %73 = phi i32 [ %12, %65 ], [ %12, %58 ], [ %42, %39 ], [ %12, %52 ], [ %12, %43 ]
  %74 = phi i32 [ %14, %65 ], [ %63, %58 ], [ %14, %39 ], [ %14, %52 ], [ %14, %43 ]
  %75 = phi i32 [ %15, %65 ], [ %15, %58 ], [ %15, %39 ], [ %55, %52 ], [ %15, %43 ]
  %76 = add nuw i64 %13, 1
  %77 = icmp eq i64 %76, %1
  br i1 %77, label %78, label %6, !llvm.loop !14

78:                                               ; preds = %67
  %79 = trunc nuw i8 %68 to i1
  %80 = trunc nuw i8 %69 to i1
  %81 = select i1 %79, i1 %80, i1 false
  %82 = trunc nuw i8 %70 to i1
  %83 = select i1 %81, i1 %82, i1 false
  %84 = trunc nuw i8 %71 to i1
  %85 = select i1 %83, i1 %84, i1 false
  br i1 %85, label %86, label %99

86:                                               ; preds = %78
  %87 = icmp slt i32 %73, %72
  %88 = icmp slt i32 %75, %74
  %89 = select i1 %87, i1 true, i1 %88
  br i1 %89, label %99, label %90

90:                                               ; preds = %86
  %91 = icmp slt i32 %2, 0
  %92 = select i1 %91, i32 %72, i32 %73
  %93 = icmp slt i32 %3, 0
  %94 = select i1 %93, i32 %74, i32 %75
  %95 = zext i32 %94 to i64
  %96 = shl nuw i64 %95, 32
  %97 = zext i32 %92 to i64
  %98 = or disjoint i64 %96, %97
  br label %99

99:                                               ; preds = %33, %29, %27, %6, %65, %4, %86, %78, %90
  %100 = phi i64 [ 0, %78 ], [ %98, %90 ], [ 0, %86 ], [ 0, %4 ], [ 0, %65 ], [ 0, %6 ], [ 0, %27 ], [ 0, %29 ], [ 0, %33 ]
  %101 = phi i8 [ 0, %78 ], [ 1, %90 ], [ 0, %86 ], [ 0, %4 ], [ 0, %65 ], [ 0, %6 ], [ 0, %27 ], [ 0, %29 ], [ 0, %33 ]
  %102 = insertvalue { i64, i8 } poison, i64 %100, 0
  %103 = insertvalue { i64, i8 } %102, i8 %101, 1
  ret { i64, i8 } %103
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
