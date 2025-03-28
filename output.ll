; ModuleID = 'while_lang'
source_filename = "while_lang"

declare i32 @printf(ptr, ...)

define i32 @main() {
entry:
  br i1 true, label %then, label %else

then:                                             ; preds = %entry
  br label %ifcont

else:                                             ; preds = %entry
  br label %ifcont

ifcont:                                           ; preds = %else, %then
  %iftmp = phi i32 [ 5, %then ], [ 10, %else ]
  ret i32 %iftmp
}
