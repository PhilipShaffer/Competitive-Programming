; ModuleID = 'main'
source_filename = "main"

define i32 @main() {
entry:
  %y = alloca i32, align 4
  %x = alloca i32, align 4
  store i32 5, ptr %x, align 4
  store i32 10, ptr %y, align 4
  %x1 = load i32, ptr %x, align 4
  %y2 = load i32, ptr %y, align 4
  %cmptmp = icmp slt i32 %x1, %y2
  %booltmp = zext i1 %cmptmp to i32
  %ifcond = icmp ne i32 %booltmp, 0
  br i1 %ifcond, label %then, label %else

then:                                             ; preds = %entry
  %x3 = load i32, ptr %x, align 4
  %ascii = add i32 %x3, 48
  %print = call i32 @putchar(i32 %ascii)
  %newline = call i32 @putchar(i32 10)
  br label %ifcont

else:                                             ; preds = %entry
  %y4 = load i32, ptr %y, align 4
  %ascii5 = add i32 %y4, 48
  %print6 = call i32 @putchar(i32 %ascii5)
  %newline7 = call i32 @putchar(i32 10)
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %iftmp = phi i32 [ %x3, %then ], [ %y4, %else ]
  ret i32 %iftmp
}

declare i32 @putchar(i32)
