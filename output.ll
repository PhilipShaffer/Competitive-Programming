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
  br label %ifcont

else:                                             ; preds = %entry
  %y4 = load i32, ptr %y, align 4
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %iftmp = phi i32 [ %x3, %then ], [ %y4, %else ]
  ret i32 %iftmp
}
