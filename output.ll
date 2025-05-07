; ModuleID = 'PigletJIT'
source_filename = "PigletJIT"

@fmt_int = private unnamed_addr constant [6 x i8] c"%lld\0A\00", align 1

declare i32 @printf(ptr, ...)

declare ptr @malloc(i64)

declare void @free(ptr)

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #0

define i64 @main() {
entry:
  %printf_call = call i32 (ptr, ...) @printf(ptr @fmt_int, i64 3)
  ret i64 0
}

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
