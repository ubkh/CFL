
.class public forloop2.forloop2
.super java/lang/Object

.method public static write(I)V
    .limit locals 1
    .limit stack 2
    getstatic java/lang/System/out Ljava/io/PrintStream;
    iload 0
    invokevirtual java/io/PrintStream/print(I)V
    return
.end method

.method public static writes(Ljava/lang/String;)V
    .limit stack 2
    .limit locals 1
    getstatic java/lang/System/out Ljava/io/PrintStream;
    aload 0
    invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V
    return
.end method

.method public static read()I
    .limit locals 10
    .limit stack 10
    ldc 0
    istore 1 ; this will hold our final integer
    Label1:
    getstatic java/lang/System/in Ljava/io/InputStream;
    invokevirtual java/io/InputStream/read()I
    istore 2
    iload 2
    ldc 10 ; test for the newline delimiter for Unix
    isub
    ifeq Label2
    iload 2
    ldc 13 ; test for the carriage -return in Windows
    isub
    ifeq Label2
    iload 2
    ldc 32 ; the space delimiter
    isub
    ifeq Label2
    iload 2
    ldc 48 ; we have our digit in ASCII, have to subtract it from 48
    isub
    ldc 10
    iload 1
    imul
    iadd
    istore 1
    goto Label1
    Label2:
    ; when we come here we have our integer computed
    ; in local variable 1
    iload 1
    ireturn
.end method

.method public static main([Ljava/lang/String;)V
    .limit locals 200
    .limit stack 200
   ldc 1
   istore 0
For_begin_0:
   iload 0
   ldc 10
   if_icmpgt For_end_1
   ldc 1
   istore 0
For_begin_2:
   iload 0
   ldc 10
   if_icmpgt For_end_3
   iload 0
   invokestatic forloop2/forloop2/write(I)V
Block_4:
   iload 0
   ldc 1
   iadd
   istore 0
   goto For_begin_2
Block_3:
For_end_3:
Block_2:
   iload 0
   ldc 1
   iadd
   istore 0
   goto For_begin_0
Block_1:
For_end_1:
Block_0:

    return
.end method
