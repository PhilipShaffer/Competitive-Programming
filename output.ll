; ModuleID = 'PigletJIT'
source_filename = "PigletJIT"

@fmt_int = private unnamed_addr constant [6 x i8] c"%lld\0A\00", align 1
@fmt_int.1 = private unnamed_addr constant [6 x i8] c"%lld\0A\00", align 1

declare i32 @printf(ptr, ...)

declare ptr @malloc(i64)

declare void @free(ptr)

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: readwrite)
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias nocapture readonly, i64, i1 immarg) #0

define i64 @main() {
entry:
  %var_6 = alloca double, align 8
  %var_5 = alloca double, align 8
  %var_4 = alloca double, align 8
  store double 2.000000e+00, ptr %var_4, align 8
  store double 3.000000e+00, ptr %var_5, align 8
  %var_tmp = load double, ptr %var_4, align 8
  %var_tmp1 = load double, ptr %var_5, align 8
  %calltmp = call double @fun_0(double %var_tmp, double %var_tmp1)
  store double %calltmp, ptr %var_6, align 8
  %var_tmp2 = load double, ptr %var_6, align 8
  %fcmp_tmp = fcmp ogt double %var_tmp2, 4.000000e+00
  %ifcond = icmp ne i1 %fcmp_tmp, false
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %entry
  %printf_call = call i32 (ptr, ...) @printf(ptr @fmt_int, i64 300)
  br label %ifcont

else:                                             ; preds = %entry
  %printf_call3 = call i32 (ptr, ...) @printf(ptr @fmt_int.1, i64 400)
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  ret i64 0
}

define double @fun_0(double %arg_1, double %arg_2) {
entry:
  %var_3 = alloca double, align 8
  %arg_22 = alloca double, align 8
  store double %arg_2, ptr %arg_22, align 8
  %arg_11 = alloca double, align 8
  store double %arg_1, ptr %arg_11, align 8
  %var_tmp = load double, ptr %arg_11, align 8
  %var_tmp3 = load double, ptr %arg_22, align 8
  %subtmp = fsub double %var_tmp, %var_tmp3
  store double %subtmp, ptr %var_3, align 8
  %var_tmp4 = load double, ptr %var_3, align 8
  ret double %var_tmp4
}

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: readwrite) }
