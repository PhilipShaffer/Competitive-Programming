; ModuleID = 'PigletJIT'
source_filename = "PigletJIT"

@fmt_int = private unnamed_addr constant [6 x i8] c"%lld\0A\00", align 1

declare i32 @printf(ptr, ...)

define i64 @main() {
entry:
  %var_0 = alloca ptr, align 8
  %array_tmp = alloca [5 x double], align 8
  %elem_ptr = getelementptr [5 x double], ptr %array_tmp, i64 0, i64 0
  store double 1.000000e+00, ptr %elem_ptr, align 8
  %elem_ptr1 = getelementptr [5 x double], ptr %array_tmp, i64 0, i64 1
  store double 2.000000e+00, ptr %elem_ptr1, align 8
  %elem_ptr2 = getelementptr [5 x double], ptr %array_tmp, i64 0, i64 2
  store double 3.000000e+00, ptr %elem_ptr2, align 8
  %elem_ptr3 = getelementptr [5 x double], ptr %array_tmp, i64 0, i64 3
  store double 4.000000e+00, ptr %elem_ptr3, align 8
  %elem_ptr4 = getelementptr [5 x double], ptr %array_tmp, i64 0, i64 4
  store double 5.000000e+00, ptr %elem_ptr4, align 8
  store ptr %array_tmp, ptr %var_0, align 8
  %var_tmp = load ptr, ptr %var_0, align 8
  %printf_call = call i32 (ptr, ...) @printf(ptr @fmt_int, i64 8589934862)
  ret i64 0
}
