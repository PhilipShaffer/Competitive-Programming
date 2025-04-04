; ModuleID = 'main'
source_filename = "main"

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1
@fmt.1 = private unnamed_addr constant [4 x i8] c"%f\0A\00", align 1
@fmt.2 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

define i32 @main() {
entry:
  %v = alloca i32, align 4
  %u = alloca float, align 4
  %y = alloca i32, align 4
  store i32 4, ptr %y, align 4
  store float 0x40112CE640000000, ptr %u, align 4
  store i32 0, ptr %v, align 4
  %y1 = load i32, ptr %y, align 4
  %printf = call i32 (ptr, ...) @printf(ptr @fmt, i32 %y1)
  %u2 = load float, ptr %u, align 4
  %printf3 = call i32 (ptr, ...) @printf(ptr @fmt.1, float %u2)
  %v4 = load i32, ptr %v, align 4
  %printf5 = call i32 (ptr, ...) @printf(ptr @fmt.2, i32 %v4)
  ret i32 0
}

declare i32 @printf(ptr, ...)
