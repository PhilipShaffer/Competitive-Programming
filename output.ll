; ModuleID = 'PigletJIT'
source_filename = "PigletJIT"

@fmt_int = private unnamed_addr constant [6 x i8] c"%lld\0A\00", align 1

declare i32 @printf(ptr, ...)

declare ptr @malloc(i64)

define i64 @main() {
entry:
  %var_0 = alloca [3 x i64], align 8
  %array_malloc = call ptr @malloc(i64 24)
  %elem_ptr = getelementptr i64, ptr %array_malloc, i64 0
  store i64 3, ptr %elem_ptr, align 4
  %elem_ptr1 = getelementptr i64, ptr %array_malloc, i64 1
  store i64 4, ptr %elem_ptr1, align 4
  %elem_ptr2 = getelementptr i64, ptr %array_malloc, i64 2
  store i64 5, ptr %elem_ptr2, align 4
  store ptr %array_malloc, ptr %var_0, align 8
  %printf_call = call i32 (ptr, ...) @printf(ptr @fmt_int, i64 5)
  ret i64 0
}
