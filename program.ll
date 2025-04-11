; ModuleID = 'main'
source_filename = "main"

@fmt = private unnamed_addr constant [5 x i8] c"%ld\0A\00", align 1
@fmt.1 = private unnamed_addr constant [5 x i8] c"%ld\0A\00", align 1

define i64 @main() {
entry:
  %z = alloca i64, align 8
  %y = alloca i64, align 8
  %x = alloca i64, align 8
  store i64 5, ptr %x, align 4
  store i64 10, ptr %y, align 4
  %x1 = load i64, ptr %x, align 4
  %y2 = load i64, ptr %y, align 4
  %addtmp = add i64 %x1, %y2
  store i64 %addtmp, ptr %z, align 4
  %x3 = load i64, ptr %x, align 4
  %y4 = load i64, ptr %y, align 4
  %cmptmp = icmp slt i64 %x3, %y4
  %booltmp = zext i1 %cmptmp to i64
  %ifcond = icmp ne i64 %booltmp, 0
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %entry
  %x5 = load i64, ptr %x, align 4
  %printf = call i64 (ptr, ...) @printf(ptr @fmt, i64 %x5)
  br label %ifcont

else:                                             ; preds = %entry
  %y6 = load i64, ptr %y, align 4
  %printf7 = call i64 (ptr, ...) @printf(ptr @fmt.1, i64 %y6)
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  ret i64 0
}

declare i64 @printf(ptr, ...)
