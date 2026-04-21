target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@_ZN7fixtureL10EU_MEMBERSE = internal unnamed_addr constant [27 x ptr] [ptr @.str, ptr @.str.1, ptr @.str.2, ptr @.str.3, ptr @.str.4, ptr @.str.5, ptr @.str.6, ptr @.str.7, ptr @.str.8, ptr @.str.9, ptr @.str.10, ptr @.str.11, ptr @.str.12, ptr @.str.13, ptr @.str.14, ptr @.str.15, ptr @.str.16, ptr @.str.17, ptr @.str.18, ptr @.str.19, ptr @.str.20, ptr @.str.21, ptr @.str.22, ptr @.str.23, ptr @.str.24, ptr @.str.25, ptr @.str.26], align 16
@.str = private unnamed_addr constant [3 x i8] c"AT\00", align 1
@.str.1 = private unnamed_addr constant [3 x i8] c"BE\00", align 1
@.str.2 = private unnamed_addr constant [3 x i8] c"BG\00", align 1
@.str.3 = private unnamed_addr constant [3 x i8] c"HR\00", align 1
@.str.4 = private unnamed_addr constant [3 x i8] c"CY\00", align 1
@.str.5 = private unnamed_addr constant [3 x i8] c"CZ\00", align 1
@.str.6 = private unnamed_addr constant [3 x i8] c"DK\00", align 1
@.str.7 = private unnamed_addr constant [3 x i8] c"EE\00", align 1
@.str.8 = private unnamed_addr constant [3 x i8] c"FI\00", align 1
@.str.9 = private unnamed_addr constant [3 x i8] c"FR\00", align 1
@.str.10 = private unnamed_addr constant [3 x i8] c"DE\00", align 1
@.str.11 = private unnamed_addr constant [3 x i8] c"GR\00", align 1
@.str.12 = private unnamed_addr constant [3 x i8] c"HU\00", align 1
@.str.13 = private unnamed_addr constant [3 x i8] c"IE\00", align 1
@.str.14 = private unnamed_addr constant [3 x i8] c"IT\00", align 1
@.str.15 = private unnamed_addr constant [3 x i8] c"LV\00", align 1
@.str.16 = private unnamed_addr constant [3 x i8] c"LT\00", align 1
@.str.17 = private unnamed_addr constant [3 x i8] c"LU\00", align 1
@.str.18 = private unnamed_addr constant [3 x i8] c"MT\00", align 1
@.str.19 = private unnamed_addr constant [3 x i8] c"NL\00", align 1
@.str.20 = private unnamed_addr constant [3 x i8] c"PL\00", align 1
@.str.21 = private unnamed_addr constant [3 x i8] c"PT\00", align 1
@.str.22 = private unnamed_addr constant [3 x i8] c"RO\00", align 1
@.str.23 = private unnamed_addr constant [3 x i8] c"SK\00", align 1
@.str.24 = private unnamed_addr constant [3 x i8] c"SI\00", align 1
@.str.25 = private unnamed_addr constant [3 x i8] c"ES\00", align 1
@.str.26 = private unnamed_addr constant [3 x i8] c"SE\00", align 1

; Function Attrs: mustprogress nofree noinline norecurse nosync nounwind willreturn memory(none) uwtable
define dso_local noundef zeroext i1 @pruning_compile_time_noop(ptr noundef readnone captures(none) %0) local_unnamed_addr #0 {
  ret i1 false
}

; Function Attrs: mustprogress nofree noinline norecurse nosync nounwind willreturn memory(read, inaccessiblemem: none, target_mem0: none, target_mem1: none) uwtable
define dso_local noundef zeroext i1 @pruning_runtime_guard(ptr noundef readonly captures(none) %0, ptr noundef readonly captures(none) %1, i32 noundef %2) local_unnamed_addr #1 {
  %4 = load i8, ptr %0, align 1, !tbaa !9
  %5 = getelementptr inbounds nuw i8, ptr %0, i64 1
  %6 = getelementptr inbounds nuw i8, ptr %0, i64 2
  br label %7

7:                                                ; preds = %25, %3
  %8 = phi i64 [ 0, %3 ], [ %26, %25 ]
  %9 = getelementptr inbounds nuw ptr, ptr @_ZN7fixtureL10EU_MEMBERSE, i64 %8
  %10 = load ptr, ptr %9, align 8, !tbaa !10
  %11 = load i8, ptr %10, align 1, !tbaa !9
  %12 = icmp eq i8 %4, %11
  br i1 %12, label %13, label %25

13:                                               ; preds = %7
  %14 = load i8, ptr %5, align 1, !tbaa !9
  %15 = getelementptr inbounds nuw i8, ptr %10, i64 1
  %16 = load i8, ptr %15, align 1, !tbaa !9
  %17 = icmp eq i8 %14, %16
  br i1 %17, label %18, label %25

18:                                               ; preds = %13
  %19 = load i8, ptr %6, align 1, !tbaa !9
  %20 = icmp eq i8 %19, 0
  br i1 %20, label %21, label %25

21:                                               ; preds = %18
  %22 = getelementptr inbounds nuw i8, ptr %10, i64 2
  %23 = load i8, ptr %22, align 1, !tbaa !9
  %24 = icmp eq i8 %23, 0
  br i1 %24, label %28, label %25

25:                                               ; preds = %21, %18, %13, %7
  %26 = add nuw nsw i64 %8, 1
  %27 = icmp eq i64 %26, 27
  br i1 %27, label %28, label %7, !llvm.loop !13

28:                                               ; preds = %21, %25
  %29 = phi i1 [ true, %21 ], [ false, %25 ]
  %30 = icmp eq i32 %2, 0
  br i1 %30, label %54, label %31

31:                                               ; preds = %28
  %32 = zext i32 %2 to i64
  br label %33

33:                                               ; preds = %51, %31
  %34 = phi i64 [ 0, %31 ], [ %52, %51 ]
  %35 = getelementptr inbounds nuw ptr, ptr %1, i64 %34
  %36 = load ptr, ptr %35, align 8, !tbaa !10
  %37 = load i8, ptr %36, align 1, !tbaa !9
  %38 = icmp eq i8 %4, %37
  br i1 %38, label %39, label %51

39:                                               ; preds = %33
  %40 = load i8, ptr %5, align 1, !tbaa !9
  %41 = getelementptr inbounds nuw i8, ptr %36, i64 1
  %42 = load i8, ptr %41, align 1, !tbaa !9
  %43 = icmp eq i8 %40, %42
  br i1 %43, label %44, label %51

44:                                               ; preds = %39
  %45 = load i8, ptr %6, align 1, !tbaa !9
  %46 = icmp eq i8 %45, 0
  br i1 %46, label %47, label %51

47:                                               ; preds = %44
  %48 = getelementptr inbounds nuw i8, ptr %36, i64 2
  %49 = load i8, ptr %48, align 1, !tbaa !9
  %50 = icmp eq i8 %49, 0
  br i1 %50, label %54, label %51

51:                                               ; preds = %47, %44, %39, %33
  %52 = add nuw nsw i64 %34, 1
  %53 = icmp eq i64 %52, %32
  br i1 %53, label %54, label %33, !llvm.loop !13

54:                                               ; preds = %47, %51, %28
  %55 = phi i1 [ false, %28 ], [ true, %47 ], [ false, %51 ]
  %56 = and i1 %29, %55
  ret i1 %56
}

attributes #0 = { mustprogress nofree noinline norecurse nosync nounwind willreturn memory(none) uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { mustprogress nofree noinline norecurse nosync nounwind willreturn memory(read, inaccessiblemem: none, target_mem0: none, target_mem1: none) uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

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
!9 = !{!7, !7, i64 0}
!10 = !{!11, !11, i64 0}
!11 = !{!"p1 omnipotent char", !12, i64 0}
!12 = !{!"any pointer", !7, i64 0}
!13 = distinct !{!13, !14}
!14 = !{!"llvm.loop.mustprogress"}
