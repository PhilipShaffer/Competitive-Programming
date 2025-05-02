; ModuleID = 'PigletJIT'
source_filename = "PigletJIT"

@fmt_int = private unnamed_addr constant [6 x i8] c"%lld\0A\00", align 1
@fmt_int.1 = private unnamed_addr constant [6 x i8] c"%lld\0A\00", align 1

declare i32 @printf(ptr, ...)

declare ptr @malloc(i64)

define i64 @main() {
entry:
  %var_0 = alloca [3 x i64], align 8
  %array_malloc = call ptr @malloc(i64 24)
  %elem_ptr = getelementptr i64, ptr %array_malloc, i64 0
  store i64 1, ptr %elem_ptr, align 4
  %elem_ptr1 = getelementptr i64, ptr %array_malloc, i64 1
  store i64 2, ptr %elem_ptr1, align 4
  %elem_ptr2 = getelementptr i64, ptr %array_malloc, i64 2
  store i64 3, ptr %elem_ptr2, align 4
  store ptr %array_malloc, ptr %var_0, align 8
  %array_ptr_load = load ptr, ptr %var_0, align 8
  %elem_ptr3 = getelementptr i64, ptr %array_ptr_load, i64 0
  %elem_load = load i64, ptr %elem_ptr3, align 4
  %printf_call = call i32 (ptr, ...) @printf(ptr @fmt_int, i64 %elem_load)
  %array_ptr_load4 = load ptr, ptr %var_0, align 8
  %elem_ptr5 = getelementptr i64, ptr %array_ptr_load4, i64 0
  store i64 42, ptr %elem_ptr5, align 4
  %array_ptr_load6 = load ptr, ptr %var_0, align 8
  %elem_ptr7 = getelementptr i64, ptr %array_ptr_load6, i64 0
  %elem_load8 = load i64, ptr %elem_ptr7, align 4
  %printf_call9 = call i32 (ptr, ...) @printf(ptr @fmt_int.1, i64 %elem_load8)
  ret i64 0
}
