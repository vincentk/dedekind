; Compiled fixture: showcase_09b_lp_runtime_residual.cpp at -O2 with clang++ 22.1.3.
; See the .cpp's header for the structural reading: same kernel as
; showcase_09 (compile-time, NTTP), called at runtime — residual loop
; nest visible, none of the algorithm folded away.  Read alongside
; showcase_09_lp_vertex_typed_constant.ll (`ret i64 2`) for the contrast.

%"struct.dedekind::optimization::HalfspaceTriple" = type { double, double, double }

@llvm.global_ctors = appending global [1 x { i32, ptr, ptr }] [{ i32, ptr, ptr } { i32 65535, ptr @_GLOBAL__sub_I_showcase_09b_lp_runtime_residual.cpp, ptr null }]

; Function Attrs: mustprogress nofree noinline norecurse nosync nounwind ssp willreturn memory(argmem: read) uwtable
define double @witness_lp_runtime_x(ptr noundef readonly captures(none) %0, i64 noundef %1, double noundef %2, double noundef %3) local_unnamed_addr #0 {
  %5 = icmp eq i64 %1, 0
  br i1 %5, label %74, label %11

6:                                                ; preds = %68, %11
  %7 = phi double [ %15, %11 ], [ %69, %68 ]
  %8 = phi i1 [ %13, %11 ], [ %70, %68 ]
  %9 = phi double [ %12, %11 ], [ %71, %68 ]
  %10 = icmp eq i64 %16, %1
  br i1 %10, label %74, label %11, !llvm.loop !10

11:                                               ; preds = %4, %6
  %12 = phi double [ %9, %6 ], [ 0.000000e+00, %4 ]
  %13 = phi i1 [ %8, %6 ], [ false, %4 ]
  %14 = phi i64 [ %16, %6 ], [ 0, %4 ]
  %15 = phi double [ %7, %6 ], [ 0.000000e+00, %4 ]
  %16 = add nuw i64 %14, 1
  %17 = icmp ult i64 %16, %1
  br i1 %17, label %18, label %6

18:                                               ; preds = %11
  %19 = getelementptr inbounds nuw %"struct.dedekind::optimization::HalfspaceTriple", ptr %0, i64 %14
  %20 = load double, ptr %19, align 8, !tbaa !12, !noalias !15
  %21 = getelementptr inbounds nuw i8, ptr %19, i64 8
  %22 = load double, ptr %21, align 8, !tbaa !22, !noalias !15
  %23 = getelementptr inbounds nuw i8, ptr %19, i64 16
  br label %24

24:                                               ; preds = %68, %18
  %25 = phi double [ %12, %18 ], [ %71, %68 ]
  %26 = phi i1 [ %13, %18 ], [ %70, %68 ]
  %27 = phi i64 [ %16, %18 ], [ %72, %68 ]
  %28 = phi double [ %15, %18 ], [ %69, %68 ]
  %29 = getelementptr inbounds nuw %"struct.dedekind::optimization::HalfspaceTriple", ptr %0, i64 %27
  %30 = getelementptr inbounds nuw i8, ptr %29, i64 8
  %31 = load double, ptr %30, align 8, !tbaa !22, !noalias !15
  %32 = load double, ptr %29, align 8, !tbaa !12, !noalias !15
  %33 = fneg double %32
  %34 = fmul double %22, %33
  %35 = tail call double @llvm.fmuladd.f64(double %20, double %31, double %34)
  %36 = fcmp oeq double %35, 0.000000e+00
  br i1 %36, label %68, label %37

37:                                               ; preds = %24
  %38 = load double, ptr %23, align 8, !tbaa !23, !noalias !15
  %39 = getelementptr inbounds nuw i8, ptr %29, i64 16
  %40 = load double, ptr %39, align 8, !tbaa !23, !noalias !15
  %41 = fneg double %40
  %42 = fmul double %22, %41
  %43 = tail call double @llvm.fmuladd.f64(double %38, double %31, double %42)
  %44 = fdiv double %43, %35
  %45 = fmul double %38, %33
  %46 = tail call double @llvm.fmuladd.f64(double %20, double %40, double %45)
  %47 = fdiv double %46, %35
  br label %51

48:                                               ; preds = %51
  %49 = add nuw i64 %52, 1
  %50 = icmp eq i64 %49, %1
  br i1 %50, label %62, label %51, !llvm.loop !24

51:                                               ; preds = %48, %37
  %52 = phi i64 [ %49, %48 ], [ 0, %37 ]
  %53 = getelementptr inbounds nuw %"struct.dedekind::optimization::HalfspaceTriple", ptr %0, i64 %52
  %54 = getelementptr inbounds nuw i8, ptr %53, i64 16
  %55 = load double, ptr %54, align 8, !tbaa !23, !noalias !25
  %56 = load double, ptr %53, align 8, !tbaa !12, !noalias !25
  %57 = getelementptr inbounds nuw i8, ptr %53, i64 8
  %58 = load double, ptr %57, align 8, !tbaa !22, !noalias !25
  %59 = fmul double %47, %58
  %60 = tail call double @llvm.fmuladd.f64(double %56, double %44, double %59)
  %61 = fcmp uge double %55, %60
  br i1 %61, label %48, label %68

62:                                               ; preds = %48
  %63 = fmul double %3, %47
  %64 = tail call double @llvm.fmuladd.f64(double %2, double %44, double %63)
  %65 = fcmp uge double %25, %64
  %66 = select i1 %26, i1 %65, i1 false
  br i1 %66, label %68, label %67

67:                                               ; preds = %62
  br label %68

68:                                               ; preds = %51, %67, %62, %24
  %69 = phi double [ %28, %24 ], [ %28, %62 ], [ %44, %67 ], [ %28, %51 ]
  %70 = phi i1 [ %26, %24 ], [ true, %62 ], [ true, %67 ], [ %26, %51 ]
  %71 = phi double [ %25, %24 ], [ %25, %62 ], [ %64, %67 ], [ %25, %51 ]
  %72 = add nuw i64 %27, 1
  %73 = icmp eq i64 %72, %1
  br i1 %73, label %6, label %24, !llvm.loop !26

74:                                               ; preds = %6, %4
  %75 = phi double [ 0.000000e+00, %4 ], [ %7, %6 ]
  ret double %75
}

; Function Attrs: mustprogress nofree noinline norecurse nosync nounwind ssp willreturn memory(argmem: read) uwtable
define double @witness_lp_runtime_y(ptr noundef readonly captures(none) %0, i64 noundef %1, double noundef %2, double noundef %3) local_unnamed_addr #0 {
  %5 = icmp eq i64 %1, 0
  br i1 %5, label %74, label %11

6:                                                ; preds = %68, %11
  %7 = phi double [ %12, %11 ], [ %69, %68 ]
  %8 = phi i1 [ %14, %11 ], [ %70, %68 ]
  %9 = phi double [ %13, %11 ], [ %71, %68 ]
  %10 = icmp eq i64 %16, %1
  br i1 %10, label %74, label %11, !llvm.loop !10

11:                                               ; preds = %4, %6
  %12 = phi double [ %7, %6 ], [ 0.000000e+00, %4 ]
  %13 = phi double [ %9, %6 ], [ 0.000000e+00, %4 ]
  %14 = phi i1 [ %8, %6 ], [ false, %4 ]
  %15 = phi i64 [ %16, %6 ], [ 0, %4 ]
  %16 = add nuw i64 %15, 1
  %17 = icmp ult i64 %16, %1
  br i1 %17, label %18, label %6

18:                                               ; preds = %11
  %19 = getelementptr inbounds nuw %"struct.dedekind::optimization::HalfspaceTriple", ptr %0, i64 %15
  %20 = load double, ptr %19, align 8, !tbaa !12, !noalias !27
  %21 = getelementptr inbounds nuw i8, ptr %19, i64 8
  %22 = load double, ptr %21, align 8, !tbaa !22, !noalias !27
  %23 = getelementptr inbounds nuw i8, ptr %19, i64 16
  br label %24

24:                                               ; preds = %68, %18
  %25 = phi double [ %12, %18 ], [ %69, %68 ]
  %26 = phi double [ %13, %18 ], [ %71, %68 ]
  %27 = phi i1 [ %14, %18 ], [ %70, %68 ]
  %28 = phi i64 [ %16, %18 ], [ %72, %68 ]
  %29 = getelementptr inbounds nuw %"struct.dedekind::optimization::HalfspaceTriple", ptr %0, i64 %28
  %30 = getelementptr inbounds nuw i8, ptr %29, i64 8
  %31 = load double, ptr %30, align 8, !tbaa !22, !noalias !27
  %32 = load double, ptr %29, align 8, !tbaa !12, !noalias !27
  %33 = fneg double %32
  %34 = fmul double %22, %33
  %35 = tail call double @llvm.fmuladd.f64(double %20, double %31, double %34)
  %36 = fcmp oeq double %35, 0.000000e+00
  br i1 %36, label %68, label %37

37:                                               ; preds = %24
  %38 = load double, ptr %23, align 8, !tbaa !23, !noalias !27
  %39 = getelementptr inbounds nuw i8, ptr %29, i64 16
  %40 = load double, ptr %39, align 8, !tbaa !23, !noalias !27
  %41 = fneg double %40
  %42 = fmul double %22, %41
  %43 = tail call double @llvm.fmuladd.f64(double %38, double %31, double %42)
  %44 = fdiv double %43, %35
  %45 = fmul double %38, %33
  %46 = tail call double @llvm.fmuladd.f64(double %20, double %40, double %45)
  %47 = fdiv double %46, %35
  br label %51

48:                                               ; preds = %51
  %49 = add nuw i64 %52, 1
  %50 = icmp eq i64 %49, %1
  br i1 %50, label %62, label %51, !llvm.loop !24

51:                                               ; preds = %48, %37
  %52 = phi i64 [ %49, %48 ], [ 0, %37 ]
  %53 = getelementptr inbounds nuw %"struct.dedekind::optimization::HalfspaceTriple", ptr %0, i64 %52
  %54 = getelementptr inbounds nuw i8, ptr %53, i64 16
  %55 = load double, ptr %54, align 8, !tbaa !23, !noalias !34
  %56 = load double, ptr %53, align 8, !tbaa !12, !noalias !34
  %57 = getelementptr inbounds nuw i8, ptr %53, i64 8
  %58 = load double, ptr %57, align 8, !tbaa !22, !noalias !34
  %59 = fmul double %47, %58
  %60 = tail call double @llvm.fmuladd.f64(double %56, double %44, double %59)
  %61 = fcmp uge double %55, %60
  br i1 %61, label %48, label %68

62:                                               ; preds = %48
  %63 = fmul double %3, %47
  %64 = tail call double @llvm.fmuladd.f64(double %2, double %44, double %63)
  %65 = fcmp uge double %26, %64
  %66 = select i1 %27, i1 %65, i1 false
  br i1 %66, label %68, label %67

67:                                               ; preds = %62
  br label %68

68:                                               ; preds = %51, %67, %62, %24
  %69 = phi double [ %25, %24 ], [ %25, %62 ], [ %47, %67 ], [ %25, %51 ]
  %70 = phi i1 [ %27, %24 ], [ true, %62 ], [ true, %67 ], [ %27, %51 ]
  %71 = phi double [ %26, %24 ], [ %26, %62 ], [ %64, %67 ], [ %26, %51 ]
  %72 = add nuw i64 %28, 1
  %73 = icmp eq i64 %72, %1
  br i1 %73, label %6, label %24, !llvm.loop !26

74:                                               ; preds = %6, %4
  %75 = phi double [ 0.000000e+00, %4 ], [ %7, %6 ]
  ret double %75
}

; Function Attrs: mustprogress nofree noinline norecurse nosync nounwind ssp willreturn memory(argmem: read) uwtable
define zeroext i1 @witness_lp_runtime_feasible(ptr noundef readonly captures(none) %0, i64 noundef %1, double noundef %2, double noundef %3) local_unnamed_addr #0 {
  %5 = icmp eq i64 %1, 0
  br i1 %5, label %74, label %11

6:                                                ; preds = %68, %11
  %7 = phi i1 [ %12, %11 ], [ %69, %68 ]
  %8 = phi i1 [ %14, %11 ], [ %70, %68 ]
  %9 = phi double [ %13, %11 ], [ %71, %68 ]
  %10 = icmp eq i64 %16, %1
  br i1 %10, label %74, label %11, !llvm.loop !10

11:                                               ; preds = %4, %6
  %12 = phi i1 [ %7, %6 ], [ false, %4 ]
  %13 = phi double [ %9, %6 ], [ 0.000000e+00, %4 ]
  %14 = phi i1 [ %8, %6 ], [ false, %4 ]
  %15 = phi i64 [ %16, %6 ], [ 0, %4 ]
  %16 = add nuw i64 %15, 1
  %17 = icmp ult i64 %16, %1
  br i1 %17, label %18, label %6

18:                                               ; preds = %11
  %19 = getelementptr inbounds nuw %"struct.dedekind::optimization::HalfspaceTriple", ptr %0, i64 %15
  %20 = load double, ptr %19, align 8, !tbaa !12, !noalias !35
  %21 = getelementptr inbounds nuw i8, ptr %19, i64 8
  %22 = load double, ptr %21, align 8, !tbaa !22, !noalias !35
  %23 = getelementptr inbounds nuw i8, ptr %19, i64 16
  br label %24

24:                                               ; preds = %68, %18
  %25 = phi i1 [ %12, %18 ], [ %69, %68 ]
  %26 = phi double [ %13, %18 ], [ %71, %68 ]
  %27 = phi i1 [ %14, %18 ], [ %70, %68 ]
  %28 = phi i64 [ %16, %18 ], [ %72, %68 ]
  %29 = getelementptr inbounds nuw %"struct.dedekind::optimization::HalfspaceTriple", ptr %0, i64 %28
  %30 = getelementptr inbounds nuw i8, ptr %29, i64 8
  %31 = load double, ptr %30, align 8, !tbaa !22, !noalias !35
  %32 = load double, ptr %29, align 8, !tbaa !12, !noalias !35
  %33 = fneg double %32
  %34 = fmul double %22, %33
  %35 = tail call double @llvm.fmuladd.f64(double %20, double %31, double %34)
  %36 = fcmp oeq double %35, 0.000000e+00
  br i1 %36, label %68, label %37

37:                                               ; preds = %24
  %38 = load double, ptr %23, align 8, !tbaa !23, !noalias !35
  %39 = getelementptr inbounds nuw i8, ptr %29, i64 16
  %40 = load double, ptr %39, align 8, !tbaa !23, !noalias !35
  %41 = fneg double %40
  %42 = fmul double %22, %41
  %43 = tail call double @llvm.fmuladd.f64(double %38, double %31, double %42)
  %44 = fdiv double %43, %35
  %45 = fmul double %38, %33
  %46 = tail call double @llvm.fmuladd.f64(double %20, double %40, double %45)
  %47 = fdiv double %46, %35
  br label %51

48:                                               ; preds = %51
  %49 = add nuw i64 %52, 1
  %50 = icmp eq i64 %49, %1
  br i1 %50, label %62, label %51, !llvm.loop !24

51:                                               ; preds = %48, %37
  %52 = phi i64 [ %49, %48 ], [ 0, %37 ]
  %53 = getelementptr inbounds nuw %"struct.dedekind::optimization::HalfspaceTriple", ptr %0, i64 %52
  %54 = getelementptr inbounds nuw i8, ptr %53, i64 16
  %55 = load double, ptr %54, align 8, !tbaa !23, !noalias !42
  %56 = load double, ptr %53, align 8, !tbaa !12, !noalias !42
  %57 = getelementptr inbounds nuw i8, ptr %53, i64 8
  %58 = load double, ptr %57, align 8, !tbaa !22, !noalias !42
  %59 = fmul double %47, %58
  %60 = tail call double @llvm.fmuladd.f64(double %56, double %44, double %59)
  %61 = fcmp uge double %55, %60
  br i1 %61, label %48, label %68

62:                                               ; preds = %48
  %63 = fmul double %3, %47
  %64 = tail call double @llvm.fmuladd.f64(double %2, double %44, double %63)
  %65 = fcmp uge double %26, %64
  %66 = select i1 %27, i1 %65, i1 false
  br i1 %66, label %68, label %67

67:                                               ; preds = %62
  br label %68

68:                                               ; preds = %51, %67, %62, %24
  %69 = phi i1 [ %25, %24 ], [ %25, %62 ], [ true, %67 ], [ %25, %51 ]
  %70 = phi i1 [ %27, %24 ], [ true, %62 ], [ true, %67 ], [ %27, %51 ]
  %71 = phi double [ %26, %24 ], [ %26, %62 ], [ %64, %67 ], [ %26, %51 ]
  %72 = add nuw i64 %28, 1
  %73 = icmp eq i64 %72, %1
  br i1 %73, label %6, label %24, !llvm.loop !26

74:                                               ; preds = %6, %4
  %75 = phi i1 [ false, %4 ], [ %7, %6 ]
  ret i1 %75
}

; Function Attrs: mustprogress nocallback nocreateundeforpoison nofree nosync nounwind speculatable willreturn memory(none)
declare double @llvm.fmuladd.f64(double, double, double) #1

declare void @_ZGIW8dedekindW12optimization() local_unnamed_addr

; Function Attrs: ssp uwtable
define internal void @_GLOBAL__sub_I_showcase_09b_lp_runtime_residual.cpp() #2 section "__TEXT,__StaticInit,regular,pure_instructions" {
  tail call void @_ZGIW8dedekindW12optimization()
  ret void
}

attributes #0 = { mustprogress nofree noinline norecurse nosync nounwind ssp willreturn memory(argmem: read) uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cmov,+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "tune-cpu"="generic" }
