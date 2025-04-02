; ModuleID = 'main'
source_filename = "main"

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.1 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

define i32 @main() {
entry:
  %y = alloca i32, align 4
  %x = alloca i32, align 4
  store i32 9, ptr %x, align 4
  store i32 10, ptr %y, align 4
  %x1 = load i32, ptr %x, align 4
  %y2 = load i32, ptr %y, align 4
  %cmptmp = icmp slt i32 %x1, %y2
  %booltmp = zext i1 %cmptmp to i32
  %ifcond = icmp ne i32 %booltmp, 0
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %entry
  %x3 = load i32, ptr %x, align 4
  %printf = call i32 (ptr, ...) @printf(ptr @fmt, i32 %x3)
  br label %ifcont

else:                                             ; preds = %entry
  %y4 = load i32, ptr %y, align 4
  %printf5 = call i32 (ptr, ...) @printf(ptr @fmt.1, i32 %y4)
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %iftmp = phi i32 [ 0, %then ], [ 0, %else ]
  ret i32 0
}

declare i32 @printf(ptr, ...)
