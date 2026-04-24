
@llvm.global_ctors = appending global [1 x { i32, ptr, ptr }] [{ i32, ptr, ptr } { i32 65535, ptr @_GLOBAL__sub_I_showcase_01_diagonal_contradiction.cpp, ptr null }]

; Function Attrs: mustprogress nofree noinline norecurse nosync nounwind ssp willreturn memory(none) uwtable
define noundef zeroext i1 @witness_empty_diagonal_cut() local_unnamed_addr #0 {
  ret i1 false
}

declare void @_ZGIW8dedekindW8category() local_unnamed_addr

declare void @_ZGIW8dedekindW4sets() local_unnamed_addr

declare void @_ZGIW8dedekindW7algebra() local_unnamed_addr

declare void @_ZGIW8dedekindW7numbers() local_unnamed_addr

; Function Attrs: ssp uwtable
define internal void @_GLOBAL__sub_I_showcase_01_diagonal_contradiction.cpp() #1 section "__TEXT,__StaticInit,regular,pure_instructions" {
  tail call void @_ZGIW8dedekindW8category()
  tail call void @_ZGIW8dedekindW4sets()
  tail call void @_ZGIW8dedekindW7algebra()
  tail call void @_ZGIW8dedekindW7numbers()
  ret void
}

attributes #0 = { mustprogress nofree noinline norecurse nosync nounwind ssp willreturn memory(none) uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cmov,+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "tune-cpu"="generic" }
attributes #1 = { ssp uwtable "frame-pointer"="all" "min-legal-vector-width"="0" "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cmov,+cx16,+cx8,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "tune-cpu"="generic" }

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
