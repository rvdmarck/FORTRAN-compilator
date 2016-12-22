@formatString = constant [4 x i8] c"%d\0A\00"
declare i32 @getchar()
declare i32 @printf(i8*,...)
define i32 @readInt(){
	entry:
		%msg = getelementptr inbounds [4 x i8], [4 x i8]* @formatString, i32 0, i32 0
		%res = alloca i32
		%digit = alloca i32
		store i32 0, i32* %res
		br label %read
	read:
		%char = call i32 @getchar()
		%num = sub i32 %char, 48
		store i32 %num, i32* %digit
		%comp1 = icmp sle i32 0, %num
		%comp2 = icmp sge i32 9, %num
		%comp3 = and i1 %comp1, %comp2
		%comp = icmp eq i1 %comp3, 1
		br i1 %comp, label %save, label %exit
	save:
		%0 = load i32, i32* %res
		%1 = load i32, i32* %digit
		%2 = mul i32 %0, 10
		%3 = add i32 %2, %1
		store i32 %3, i32* %res
		br label %read
	exit:
		%ex = load i32, i32* %res
		ret i32 %ex
}
define void @main(){
	entry:
		%msg = getelementptr inbounds [4 x i8], [4 x i8]* @formatString, i32 0, i32 0
		%_nombre = alloca i32
		%_resultat = alloca i32
		%_compteur = alloca i32
		%0= call i32 @readInt()
		store i32 %0, i32* %_nombre
		%1= call i32 @readInt()
		store i32 %1, i32* %_resultat
		%2 = add i32 1, 2
		store i32 %2, i32* %_resultat
		%3 = load i32, i32* %_nombre
		%4 = icmp sge i32 %3, 0
		br i1 %4, label %IfEqual0, label %IfUnequal0
	IfEqual0:
		%loop0count = alloca i32
		store i32 1, i32* %loop0count
		br label %loop0
	loop0:
		%5 = load i32, i32* %loop0count
		%6 = icmp eq i32 %5,10
		br i1 %6, label %endloop0, label %continueloop0
	continueloop0:
		%7 = load i32, i32* %loop0count
		%8 = add i32 1, %7
		store i32 %8, i32* %loop0count
		%9 = load i32, i32* %_resultat
		%10 = load i32, i32* %_compteur
		%11 = mul i32 %9, %10
		store i32 %11, i32* %_resultat
		br label %loop0
	endloop0:
		%12 = load i32, i32* %_resultat
		%var0 = call i32(i8,...) @printf(i8* %msg, i32 %12)
	IfUnequal0:
		%13 = sub i32 0, 1
		%var0 = call i32(i8,...) @printf(i8* %msg, i32 %13)
	ret void
}

