; ModuleID = 'main'
source_filename = "main"

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.1 = private unnamed_addr constant [4 x i8] c"%f\0A\00", align 1

define i32 @main() {
entry:
  %y = alloca float, align 4
  %x = alloca float, align 4
  store float 5.000000e+00, ptr %x, align 4
  store float 1.000000e+01, ptr %y, align 4
  %x1 = load float, ptr %x, align 4
  %y2 = load float, ptr %y, align 4
  %cmptmp = fcmp olt float %x1, %y2
  %booltmp = zext i1 %cmptmp to i32
  %ifcond = icmp ne i32 %booltmp, 0
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %entry
  %printf = call i32 (ptr, ...) @printf(ptr @fmt, i32 100)
  br label %ifcont

else:                                             ; preds = %entry
  %y3 = load float, ptr %y, align 4
  %printf4 = call i32 (ptr, ...) @printf(ptr @fmt.1, float %y3)
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %iftmp = phi i32 [ 0, %then ], [ 0, %else ]
  ret i32 0
}

declare i32 @printf(ptr, ...)
