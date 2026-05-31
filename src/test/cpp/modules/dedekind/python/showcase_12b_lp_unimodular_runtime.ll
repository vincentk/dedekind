
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
  %5 = mul nuw nsw i64 %1, 12
  %6 = getelementptr inbounds nuw i8, ptr %0, i64 %5
  %7 = icmp eq i64 %1, 0
  br i1 %7, label %96, label %8

8:                                                ; preds = %4, %64
  %9 = phi i8 [ %65, %64 ], [ 0, %4 ]
  %10 = phi i8 [ %66, %64 ], [ 0, %4 ]
  %11 = phi i8 [ %67, %64 ], [ 0, %4 ]
  %12 = phi i8 [ %68, %64 ], [ 0, %4 ]
  %13 = phi i32 [ %69, %64 ], [ 0, %4 ]
  %14 = phi i32 [ %70, %64 ], [ 0, %4 ]
  %15 = phi i32 [ %71, %64 ], [ 0, %4 ]
  %16 = phi i32 [ %72, %64 ], [ 0, %4 ]
  %17 = phi ptr [ %73, %64 ], [ %0, %4 ]
  %18 = load i32, ptr %17, align 4, !tbaa !10
  %19 = add i32 %18, 1
  %20 = icmp ult i32 %19, 3
  %21 = getelementptr inbounds nuw i8, ptr %17, i64 4
  %22 = load i32, ptr %21, align 4, !tbaa !12
  %23 = add i32 %22, 1
  %24 = icmp ult i32 %23, 3
  %25 = and i1 %20, %24
  br i1 %25, label %26, label %96

26:                                               ; preds = %8
  %27 = icmp sgt i32 %18, 0
  br i1 %27, label %28, label %34

28:                                               ; preds = %26
  %29 = trunc nuw i8 %10 to i1
  %30 = getelementptr inbounds nuw i8, ptr %17, i64 8
  %31 = load i32, ptr %30, align 4, !tbaa !13
  br i1 %29, label %32, label %64

32:                                               ; preds = %28
  %33 = tail call i32 @llvm.smin.i32(i32 %31, i32 %14)
  br label %64

34:                                               ; preds = %26
  %35 = icmp slt i32 %18, 0
  br i1 %35, label %36, label %45

36:                                               ; preds = %34
  %37 = getelementptr inbounds nuw i8, ptr %17, i64 8
  %38 = load i32, ptr %37, align 4, !tbaa !13
  %39 = sub nsw i32 0, %38
  %40 = trunc nuw i8 %9 to i1
  %41 = icmp sge i32 %13, %39
  %42 = select i1 %40, i1 %41, i1 false
  %43 = select i1 %42, i32 %13, i32 %39
  %44 = select i1 %42, i8 %9, i8 1
  br label %64

45:                                               ; preds = %34
  %46 = icmp sgt i32 %22, 0
  %47 = getelementptr inbounds nuw i8, ptr %17, i64 8
  %48 = load i32, ptr %47, align 4, !tbaa !13
  br i1 %46, label %49, label %53

49:                                               ; preds = %45
  %50 = trunc nuw i8 %12 to i1
  br i1 %50, label %51, label %64

51:                                               ; preds = %49
  %52 = tail call i32 @llvm.smin.i32(i32 %48, i32 %16)
  br label %64

53:                                               ; preds = %45
  %54 = icmp slt i32 %22, 0
  br i1 %54, label %55, label %62

55:                                               ; preds = %53
  %56 = sub nsw i32 0, %48
  %57 = trunc nuw i8 %11 to i1
  %58 = icmp sge i32 %15, %56
  %59 = select i1 %57, i1 %58, i1 false
  %60 = select i1 %59, i32 %15, i32 %56
  %61 = select i1 %59, i8 %11, i8 1
  br label %64

62:                                               ; preds = %53
  %63 = icmp sgt i32 %48, -1
  br i1 %63, label %64, label %96

64:                                               ; preds = %51, %32, %49, %28, %55, %36, %62
  %65 = phi i8 [ %9, %62 ], [ %9, %55 ], [ %9, %32 ], [ %9, %28 ], [ %44, %36 ], [ %9, %51 ], [ %9, %49 ]
  %66 = phi i8 [ %10, %62 ], [ %10, %55 ], [ 1, %32 ], [ 1, %28 ], [ %10, %36 ], [ %10, %51 ], [ %10, %49 ]
  %67 = phi i8 [ %11, %62 ], [ %61, %55 ], [ %11, %32 ], [ %11, %28 ], [ %11, %36 ], [ %11, %51 ], [ %11, %49 ]
  %68 = phi i8 [ %12, %62 ], [ %12, %55 ], [ %12, %32 ], [ %12, %28 ], [ %12, %36 ], [ 1, %51 ], [ 1, %49 ]
  %69 = phi i32 [ %13, %62 ], [ %13, %55 ], [ %13, %32 ], [ %13, %28 ], [ %43, %36 ], [ %13, %51 ], [ %13, %49 ]
  %70 = phi i32 [ %14, %62 ], [ %14, %55 ], [ %33, %32 ], [ %31, %28 ], [ %14, %36 ], [ %14, %51 ], [ %14, %49 ]
  %71 = phi i32 [ %15, %62 ], [ %60, %55 ], [ %15, %32 ], [ %15, %28 ], [ %15, %36 ], [ %15, %51 ], [ %15, %49 ]
  %72 = phi i32 [ %16, %62 ], [ %16, %55 ], [ %16, %32 ], [ %16, %28 ], [ %16, %36 ], [ %52, %51 ], [ %48, %49 ]
  %73 = getelementptr inbounds nuw i8, ptr %17, i64 12
  %74 = icmp eq ptr %73, %6
  br i1 %74, label %75, label %8

75:                                               ; preds = %64
  %76 = trunc nuw i8 %65 to i1
  %77 = trunc nuw i8 %66 to i1
  %78 = select i1 %76, i1 %77, i1 false
  %79 = trunc nuw i8 %67 to i1
  %80 = select i1 %78, i1 %79, i1 false
  %81 = trunc nuw i8 %68 to i1
  %82 = select i1 %80, i1 %81, i1 false
  br i1 %82, label %83, label %96

83:                                               ; preds = %75
  %84 = icmp slt i32 %70, %69
  %85 = icmp slt i32 %72, %71
  %86 = select i1 %84, i1 true, i1 %85
  br i1 %86, label %96, label %87

87:                                               ; preds = %83
  %88 = icmp slt i32 %2, 0
  %89 = select i1 %88, i32 %69, i32 %70
  %90 = icmp slt i32 %3, 0
  %91 = select i1 %90, i32 %71, i32 %72
  %92 = zext i32 %91 to i64
  %93 = shl nuw i64 %92, 32
  %94 = zext i32 %89 to i64
  %95 = or disjoint i64 %93, %94
  br label %96

96:                                               ; preds = %8, %62, %4, %83, %75, %87
  %97 = phi i64 [ 0, %75 ], [ %95, %87 ], [ 0, %83 ], [ 0, %4 ], [ 0, %62 ], [ 0, %8 ]
  %98 = phi i8 [ 0, %75 ], [ 1, %87 ], [ 0, %83 ], [ 0, %4 ], [ 0, %62 ], [ 0, %8 ]
  %99 = insertvalue { i64, i8 } poison, i64 %97, 0
  %100 = insertvalue { i64, i8 } %99, i8 %98, 1
  ret { i64, i8 } %100
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
