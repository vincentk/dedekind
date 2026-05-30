
@llvm.global_ctors = appending global [1 x { i32, ptr, ptr }] [{ i32, ptr, ptr } { i32 65535, ptr @_GLOBAL__sub_I_showcase_12b_lp_unimodular_runtime.cpp, ptr null }]

; Function Attrs: mustprogress nofree noinline norecurse nosync nounwind ssp willreturn memory(argmem: read) uwtable
define range(i32 -2147483647, -2147483648) i32 @witness_lp_axis_aligned_x(ptr noundef readonly captures(address) %0, i64 noundef %1, i32 noundef %2, i32 noundef %3) local_unnamed_addr #0 {
  %5 = mul nuw nsw i64 %1, 12
  %6 = getelementptr inbounds nuw i8, ptr %0, i64 %5
  %7 = icmp eq i64 %1, 0
  br i1 %7, label %84, label %16

8:                                                ; preds = %66
  %9 = trunc nuw i8 %74 to i1
  %10 = trunc nuw i8 %73 to i1
  %11 = select i1 %9, i1 %10, i1 false
  %12 = trunc nuw i8 %72 to i1
  %13 = select i1 %11, i1 %12, i1 false
  %14 = trunc nuw i8 %71 to i1
  %15 = select i1 %13, i1 %14, i1 false
  br i1 %15, label %77, label %84

16:                                               ; preds = %4, %66
  %17 = phi i8 [ %74, %66 ], [ 0, %4 ]
  %18 = phi i8 [ %73, %66 ], [ 0, %4 ]
  %19 = phi i8 [ %72, %66 ], [ 0, %4 ]
  %20 = phi i8 [ %71, %66 ], [ 0, %4 ]
  %21 = phi i32 [ %70, %66 ], [ 0, %4 ]
  %22 = phi i32 [ %69, %66 ], [ 0, %4 ]
  %23 = phi i32 [ %68, %66 ], [ 0, %4 ]
  %24 = phi i32 [ %67, %66 ], [ 0, %4 ]
  %25 = phi ptr [ %75, %66 ], [ %0, %4 ]
  %26 = load i32, ptr %25, align 4, !tbaa !10
  %27 = icmp sgt i32 %26, 0
  br i1 %27, label %28, label %34

28:                                               ; preds = %16
  %29 = trunc nuw i8 %18 to i1
  %30 = getelementptr inbounds nuw i8, ptr %25, i64 8
  %31 = load i32, ptr %30, align 4, !tbaa !12
  br i1 %29, label %32, label %66

32:                                               ; preds = %28
  %33 = tail call i32 @llvm.smin.i32(i32 %31, i32 %24)
  br label %66

34:                                               ; preds = %16
  %35 = icmp slt i32 %26, 0
  br i1 %35, label %36, label %45

36:                                               ; preds = %34
  %37 = getelementptr inbounds nuw i8, ptr %25, i64 8
  %38 = load i32, ptr %37, align 4, !tbaa !12
  %39 = sub nsw i32 0, %38
  %40 = trunc nuw i8 %17 to i1
  %41 = icmp sge i32 %21, %39
  %42 = select i1 %40, i1 %41, i1 false
  %43 = select i1 %42, i32 %21, i32 %39
  %44 = select i1 %42, i8 %17, i8 1
  br label %66

45:                                               ; preds = %34
  %46 = getelementptr inbounds nuw i8, ptr %25, i64 4
  %47 = load i32, ptr %46, align 4, !tbaa !13
  %48 = icmp sgt i32 %47, 0
  br i1 %48, label %49, label %55

49:                                               ; preds = %45
  %50 = trunc nuw i8 %20 to i1
  %51 = getelementptr inbounds nuw i8, ptr %25, i64 8
  %52 = load i32, ptr %51, align 4, !tbaa !12
  br i1 %50, label %53, label %66

53:                                               ; preds = %49
  %54 = tail call i32 @llvm.smin.i32(i32 %52, i32 %22)
  br label %66

55:                                               ; preds = %45
  %56 = icmp slt i32 %47, 0
  br i1 %56, label %57, label %66

57:                                               ; preds = %55
  %58 = getelementptr inbounds nuw i8, ptr %25, i64 8
  %59 = load i32, ptr %58, align 4, !tbaa !12
  %60 = sub nsw i32 0, %59
  %61 = trunc nuw i8 %19 to i1
  %62 = icmp sge i32 %23, %60
  %63 = select i1 %61, i1 %62, i1 false
  %64 = select i1 %63, i32 %23, i32 %60
  %65 = select i1 %63, i8 %19, i8 1
  br label %66

66:                                               ; preds = %57, %55, %53, %49, %36, %32, %28
  %67 = phi i32 [ %24, %55 ], [ %33, %32 ], [ %24, %36 ], [ %31, %28 ], [ %24, %53 ], [ %24, %57 ], [ %24, %49 ]
  %68 = phi i32 [ %23, %55 ], [ %23, %32 ], [ %23, %36 ], [ %23, %28 ], [ %23, %53 ], [ %64, %57 ], [ %23, %49 ]
  %69 = phi i32 [ %22, %55 ], [ %22, %32 ], [ %22, %36 ], [ %22, %28 ], [ %54, %53 ], [ %22, %57 ], [ %52, %49 ]
  %70 = phi i32 [ %21, %55 ], [ %21, %32 ], [ %43, %36 ], [ %21, %28 ], [ %21, %53 ], [ %21, %57 ], [ %21, %49 ]
  %71 = phi i8 [ %20, %55 ], [ %20, %32 ], [ %20, %36 ], [ %20, %28 ], [ 1, %53 ], [ %20, %57 ], [ 1, %49 ]
  %72 = phi i8 [ %19, %55 ], [ %19, %32 ], [ %19, %36 ], [ %19, %28 ], [ %19, %53 ], [ %65, %57 ], [ %19, %49 ]
  %73 = phi i8 [ %18, %55 ], [ 1, %32 ], [ %18, %36 ], [ 1, %28 ], [ %18, %53 ], [ %18, %57 ], [ %18, %49 ]
  %74 = phi i8 [ %17, %55 ], [ %17, %32 ], [ %44, %36 ], [ %17, %28 ], [ %17, %53 ], [ %17, %57 ], [ %17, %49 ]
  %75 = getelementptr inbounds nuw i8, ptr %25, i64 12
  %76 = icmp eq ptr %75, %6
  br i1 %76, label %8, label %16

77:                                               ; preds = %8
  %78 = icmp slt i32 %67, %70
  %79 = icmp slt i32 %69, %68
  %80 = select i1 %78, i1 true, i1 %79
  br i1 %80, label %84, label %81

81:                                               ; preds = %77
  %82 = icmp slt i32 %2, 0
  %83 = select i1 %82, i32 %70, i32 %67
  br label %84

84:                                               ; preds = %4, %8, %77, %81
  %85 = phi i32 [ 0, %8 ], [ %83, %81 ], [ 0, %77 ], [ 0, %4 ]
  ret i32 %85
}

; Function Attrs: mustprogress nofree noinline norecurse nosync nounwind ssp willreturn memory(argmem: read) uwtable
define range(i32 -2147483647, -2147483648) i32 @witness_lp_axis_aligned_y(ptr noundef readonly captures(address) %0, i64 noundef %1, i32 noundef %2, i32 noundef %3) local_unnamed_addr #0 {
  %5 = mul nuw nsw i64 %1, 12
  %6 = getelementptr inbounds nuw i8, ptr %0, i64 %5
  %7 = icmp eq i64 %1, 0
  br i1 %7, label %84, label %16

8:                                                ; preds = %66
  %9 = trunc nuw i8 %74 to i1
  %10 = trunc nuw i8 %73 to i1
  %11 = select i1 %9, i1 %10, i1 false
  %12 = trunc nuw i8 %72 to i1
  %13 = select i1 %11, i1 %12, i1 false
  %14 = trunc nuw i8 %71 to i1
  %15 = select i1 %13, i1 %14, i1 false
  br i1 %15, label %77, label %84

16:                                               ; preds = %4, %66
  %17 = phi i8 [ %74, %66 ], [ 0, %4 ]
  %18 = phi i8 [ %73, %66 ], [ 0, %4 ]
  %19 = phi i8 [ %72, %66 ], [ 0, %4 ]
  %20 = phi i8 [ %71, %66 ], [ 0, %4 ]
  %21 = phi i32 [ %70, %66 ], [ 0, %4 ]
  %22 = phi i32 [ %69, %66 ], [ 0, %4 ]
  %23 = phi i32 [ %68, %66 ], [ 0, %4 ]
  %24 = phi i32 [ %67, %66 ], [ 0, %4 ]
  %25 = phi ptr [ %75, %66 ], [ %0, %4 ]
  %26 = load i32, ptr %25, align 4, !tbaa !10
  %27 = icmp sgt i32 %26, 0
  br i1 %27, label %28, label %34

28:                                               ; preds = %16
  %29 = trunc nuw i8 %18 to i1
  %30 = getelementptr inbounds nuw i8, ptr %25, i64 8
  %31 = load i32, ptr %30, align 4, !tbaa !12
  br i1 %29, label %32, label %66

32:                                               ; preds = %28
  %33 = tail call i32 @llvm.smin.i32(i32 %31, i32 %24)
  br label %66

34:                                               ; preds = %16
  %35 = icmp slt i32 %26, 0
  br i1 %35, label %36, label %45

36:                                               ; preds = %34
  %37 = getelementptr inbounds nuw i8, ptr %25, i64 8
  %38 = load i32, ptr %37, align 4, !tbaa !12
  %39 = sub nsw i32 0, %38
  %40 = trunc nuw i8 %17 to i1
  %41 = icmp sge i32 %21, %39
  %42 = select i1 %40, i1 %41, i1 false
  %43 = select i1 %42, i32 %21, i32 %39
  %44 = select i1 %42, i8 %17, i8 1
  br label %66

45:                                               ; preds = %34
  %46 = getelementptr inbounds nuw i8, ptr %25, i64 4
  %47 = load i32, ptr %46, align 4, !tbaa !13
  %48 = icmp sgt i32 %47, 0
  br i1 %48, label %49, label %55

49:                                               ; preds = %45
  %50 = trunc nuw i8 %20 to i1
  %51 = getelementptr inbounds nuw i8, ptr %25, i64 8
  %52 = load i32, ptr %51, align 4, !tbaa !12
  br i1 %50, label %53, label %66

53:                                               ; preds = %49
  %54 = tail call i32 @llvm.smin.i32(i32 %52, i32 %22)
  br label %66

55:                                               ; preds = %45
  %56 = icmp slt i32 %47, 0
  br i1 %56, label %57, label %66

57:                                               ; preds = %55
  %58 = getelementptr inbounds nuw i8, ptr %25, i64 8
  %59 = load i32, ptr %58, align 4, !tbaa !12
  %60 = sub nsw i32 0, %59
  %61 = trunc nuw i8 %19 to i1
  %62 = icmp sge i32 %23, %60
  %63 = select i1 %61, i1 %62, i1 false
  %64 = select i1 %63, i32 %23, i32 %60
  %65 = select i1 %63, i8 %19, i8 1
  br label %66

66:                                               ; preds = %57, %55, %53, %49, %36, %32, %28
  %67 = phi i32 [ %24, %55 ], [ %33, %32 ], [ %24, %36 ], [ %31, %28 ], [ %24, %53 ], [ %24, %57 ], [ %24, %49 ]
  %68 = phi i32 [ %23, %55 ], [ %23, %32 ], [ %23, %36 ], [ %23, %28 ], [ %23, %53 ], [ %64, %57 ], [ %23, %49 ]
  %69 = phi i32 [ %22, %55 ], [ %22, %32 ], [ %22, %36 ], [ %22, %28 ], [ %54, %53 ], [ %22, %57 ], [ %52, %49 ]
  %70 = phi i32 [ %21, %55 ], [ %21, %32 ], [ %43, %36 ], [ %21, %28 ], [ %21, %53 ], [ %21, %57 ], [ %21, %49 ]
  %71 = phi i8 [ %20, %55 ], [ %20, %32 ], [ %20, %36 ], [ %20, %28 ], [ 1, %53 ], [ %20, %57 ], [ 1, %49 ]
  %72 = phi i8 [ %19, %55 ], [ %19, %32 ], [ %19, %36 ], [ %19, %28 ], [ %19, %53 ], [ %65, %57 ], [ %19, %49 ]
  %73 = phi i8 [ %18, %55 ], [ 1, %32 ], [ %18, %36 ], [ 1, %28 ], [ %18, %53 ], [ %18, %57 ], [ %18, %49 ]
  %74 = phi i8 [ %17, %55 ], [ %17, %32 ], [ %44, %36 ], [ %17, %28 ], [ %17, %53 ], [ %17, %57 ], [ %17, %49 ]
  %75 = getelementptr inbounds nuw i8, ptr %25, i64 12
  %76 = icmp eq ptr %75, %6
  br i1 %76, label %8, label %16

77:                                               ; preds = %8
  %78 = icmp slt i32 %67, %70
  %79 = icmp slt i32 %69, %68
  %80 = select i1 %78, i1 true, i1 %79
  br i1 %80, label %84, label %81

81:                                               ; preds = %77
  %82 = icmp slt i32 %3, 0
  %83 = select i1 %82, i32 %68, i32 %69
  br label %84

84:                                               ; preds = %4, %8, %77, %81
  %85 = phi i32 [ 0, %8 ], [ %83, %81 ], [ 0, %77 ], [ 0, %4 ]
  ret i32 %85
}

; Function Attrs: mustprogress nofree noinline norecurse nosync nounwind ssp willreturn memory(argmem: read) uwtable
define zeroext i1 @witness_lp_axis_aligned_feasible(ptr noundef readonly captures(address) %0, i64 noundef %1, i32 noundef %2, i32 noundef %3) local_unnamed_addr #0 {
  %5 = mul nuw nsw i64 %1, 12
  %6 = getelementptr inbounds nuw i8, ptr %0, i64 %5
  %7 = icmp eq i64 %1, 0
  br i1 %7, label %81, label %16

8:                                                ; preds = %66
  %9 = trunc nuw i8 %74 to i1
  %10 = trunc nuw i8 %73 to i1
  %11 = select i1 %9, i1 %10, i1 false
  %12 = trunc nuw i8 %72 to i1
  %13 = select i1 %11, i1 %12, i1 false
  %14 = trunc nuw i8 %71 to i1
  %15 = select i1 %13, i1 %14, i1 false
  br i1 %15, label %77, label %81

16:                                               ; preds = %4, %66
  %17 = phi i8 [ %74, %66 ], [ 0, %4 ]
  %18 = phi i8 [ %73, %66 ], [ 0, %4 ]
  %19 = phi i8 [ %72, %66 ], [ 0, %4 ]
  %20 = phi i8 [ %71, %66 ], [ 0, %4 ]
  %21 = phi i32 [ %70, %66 ], [ 0, %4 ]
  %22 = phi i32 [ %69, %66 ], [ 0, %4 ]
  %23 = phi i32 [ %68, %66 ], [ 0, %4 ]
  %24 = phi i32 [ %67, %66 ], [ 0, %4 ]
  %25 = phi ptr [ %75, %66 ], [ %0, %4 ]
  %26 = load i32, ptr %25, align 4, !tbaa !10
  %27 = icmp sgt i32 %26, 0
  br i1 %27, label %28, label %34

28:                                               ; preds = %16
  %29 = trunc nuw i8 %18 to i1
  %30 = getelementptr inbounds nuw i8, ptr %25, i64 8
  %31 = load i32, ptr %30, align 4, !tbaa !12
  br i1 %29, label %32, label %66

32:                                               ; preds = %28
  %33 = tail call i32 @llvm.smin.i32(i32 %31, i32 %24)
  br label %66

34:                                               ; preds = %16
  %35 = icmp slt i32 %26, 0
  br i1 %35, label %36, label %45

36:                                               ; preds = %34
  %37 = getelementptr inbounds nuw i8, ptr %25, i64 8
  %38 = load i32, ptr %37, align 4, !tbaa !12
  %39 = sub nsw i32 0, %38
  %40 = trunc nuw i8 %17 to i1
  %41 = icmp sge i32 %21, %39
  %42 = select i1 %40, i1 %41, i1 false
  %43 = select i1 %42, i32 %21, i32 %39
  %44 = select i1 %42, i8 %17, i8 1
  br label %66

45:                                               ; preds = %34
  %46 = getelementptr inbounds nuw i8, ptr %25, i64 4
  %47 = load i32, ptr %46, align 4, !tbaa !13
  %48 = icmp sgt i32 %47, 0
  br i1 %48, label %49, label %55

49:                                               ; preds = %45
  %50 = trunc nuw i8 %20 to i1
  %51 = getelementptr inbounds nuw i8, ptr %25, i64 8
  %52 = load i32, ptr %51, align 4, !tbaa !12
  br i1 %50, label %53, label %66

53:                                               ; preds = %49
  %54 = tail call i32 @llvm.smin.i32(i32 %52, i32 %22)
  br label %66

55:                                               ; preds = %45
  %56 = icmp slt i32 %47, 0
  br i1 %56, label %57, label %66

57:                                               ; preds = %55
  %58 = getelementptr inbounds nuw i8, ptr %25, i64 8
  %59 = load i32, ptr %58, align 4, !tbaa !12
  %60 = sub nsw i32 0, %59
  %61 = trunc nuw i8 %19 to i1
  %62 = icmp sge i32 %23, %60
  %63 = select i1 %61, i1 %62, i1 false
  %64 = select i1 %63, i32 %23, i32 %60
  %65 = select i1 %63, i8 %19, i8 1
  br label %66

66:                                               ; preds = %57, %55, %53, %49, %36, %32, %28
  %67 = phi i32 [ %24, %55 ], [ %33, %32 ], [ %24, %36 ], [ %31, %28 ], [ %24, %53 ], [ %24, %57 ], [ %24, %49 ]
  %68 = phi i32 [ %23, %55 ], [ %23, %32 ], [ %23, %36 ], [ %23, %28 ], [ %23, %53 ], [ %64, %57 ], [ %23, %49 ]
  %69 = phi i32 [ %22, %55 ], [ %22, %32 ], [ %22, %36 ], [ %22, %28 ], [ %54, %53 ], [ %22, %57 ], [ %52, %49 ]
  %70 = phi i32 [ %21, %55 ], [ %21, %32 ], [ %43, %36 ], [ %21, %28 ], [ %21, %53 ], [ %21, %57 ], [ %21, %49 ]
  %71 = phi i8 [ %20, %55 ], [ %20, %32 ], [ %20, %36 ], [ %20, %28 ], [ 1, %53 ], [ %20, %57 ], [ 1, %49 ]
  %72 = phi i8 [ %19, %55 ], [ %19, %32 ], [ %19, %36 ], [ %19, %28 ], [ %19, %53 ], [ %65, %57 ], [ %19, %49 ]
  %73 = phi i8 [ %18, %55 ], [ 1, %32 ], [ %18, %36 ], [ 1, %28 ], [ %18, %53 ], [ %18, %57 ], [ %18, %49 ]
  %74 = phi i8 [ %17, %55 ], [ %17, %32 ], [ %44, %36 ], [ %17, %28 ], [ %17, %53 ], [ %17, %57 ], [ %17, %49 ]
  %75 = getelementptr inbounds nuw i8, ptr %25, i64 12
  %76 = icmp eq ptr %75, %6
  br i1 %76, label %8, label %16

77:                                               ; preds = %8
  %78 = icmp sge i32 %67, %70
  %79 = icmp sge i32 %69, %68
  %80 = select i1 %78, i1 %79, i1 false
  br label %81

81:                                               ; preds = %77, %4, %8
  %82 = phi i1 [ false, %8 ], [ false, %4 ], [ %80, %77 ]
  ret i1 %82
}

declare void @_ZGIW8dedekindW12optimization() local_unnamed_addr

; Function Attrs: ssp uwtable
define internal void @_GLOBAL__sub_I_showcase_12b_lp_unimodular_runtime.cpp() #1 section "__TEXT,__StaticInit,regular,pure_instructions" {
  tail call void @_ZGIW8dedekindW12optimization()
  ret void
}

; Function Attrs: nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare i32 @llvm.smin.i32(i32, i32) #2

attributes #0 = { mustprogress nofree noinline norecurse nosync nounwind ssp willreturn memory(argmem: read) uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cmov,+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "tune-cpu"="generic" }
attributes #1 = { ssp uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cmov,+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "tune-cpu"="generic" }
attributes #2 = { nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none) }

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
!12 = !{!11, !7, i64 8}
!13 = !{!11, !7, i64 4}
