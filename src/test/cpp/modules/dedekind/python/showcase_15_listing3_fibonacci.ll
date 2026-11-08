
@llvm.global_ctors = appending global [1 x { i32, ptr, ptr }] [{ i32, ptr, ptr } { i32 65535, ptr @_GLOBAL__sub_I_showcase_15_listing3_fibonacci.cpp, ptr null }]

; Function Attrs: mustprogress nofree noinline norecurse nosync nounwind willreturn memory(none) uwtable
define dso_local i64 @listing3_fibonacci(i64 noundef %0) local_unnamed_addr #0 personality ptr @__gxx_personality_v0 {
  %2 = icmp eq i64 %0, 0
  br i1 %2, label %13, label %3

3:                                                ; preds = %1
  %4 = add i64 %0, -1
  %5 = icmp eq i64 %4, 0
  br i1 %5, label %13, label %6

6:                                                ; preds = %3, %6
  %7 = phi i64 [ %10, %6 ], [ %4, %3 ]
  %8 = phi i64 [ %9, %6 ], [ 1, %3 ]
  %9 = phi i64 [ %11, %6 ], [ 1, %3 ]
  %10 = add i64 %7, -1
  %11 = add i64 %9, %8
  %12 = icmp eq i64 %10, 0
  br i1 %12, label %13, label %6, !llvm.loop !9

13:                                               ; preds = %6, %1, %3
  %14 = phi i64 [ 0, %1 ], [ 1, %3 ], [ %9, %6 ]
  ret i64 %14
}

declare i32 @__gxx_personality_v0(...)

declare void @_ZGIW8dedekindW8category() local_unnamed_addr

; Function Attrs: uwtable
define internal void @_GLOBAL__sub_I_showcase_15_listing3_fibonacci.cpp() #1 section ".text.startup" {
  tail call void @_ZGIW8dedekindW8category()
  ret void
}

attributes #0 = { mustprogress nofree noinline norecurse nosync nounwind willreturn memory(none) uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }
attributes #1 = { uwtable "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cmov,+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "tune-cpu"="generic" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.errno.tbaa = !{!4}

!0 = !{i32 8, !"PIC Level", i32 2}
!1 = !{i32 7, !"PIE Level", i32 2}
!2 = !{i32 7, !"uwtable", i32 2}
!4 = !{!5, !6, i64 0}
!5 = !{!"__libc_errno", !6, i64 0}
!6 = !{!"int", !7, i64 0}
!7 = !{!"omnipotent char", !8, i64 0}
!8 = !{!"Simple C++ TBAA"}
!9 = distinct !{!9, !10, !11}
!10 = !{!"llvm.loop.mustprogress"}
!11 = !{!"llvm.loop.unroll.disable"}
