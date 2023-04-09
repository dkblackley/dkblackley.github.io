---
layout: post
title: FizzBuzz in Assembly
categories: [Programming, Reversing]
---

I've recently lost my mind. Most of you programmers know that can only mean one of two things. I've either been [programming in JavaScript](google.com) or programming in Assembly. I've only lost my mind and not my sense of self worth, which narrows it down to Assembly. If you're considering working with assembly too, I just want you to know that there's people out there who care about you and there are numerous support groups to [help you in these intrusive thoughts](google.com). If it's already too late for you then continue reading to see my adventures in ASM...

Now that all the sane people have left, I'm with those who are either insane or ignorant. In case of the latter, allow me to present an introduction. In the beginning, there was nothing. Then there was the [turing machine](google.com), and Alan saw that the turing machine was good. Then there was [Some other stuff I don't really remember]([google.com](https://programcounter.home.blog/2019/06/08/how-the-cpu-works-according-to-lmclittle-man-computer/)) and we ended up with [Assembly in 1947](https://en.wikipedia.org/wiki/Assembly_language). An assembly language is usually identifiable by the fact that each line corresponds to one machine instruction. This means that we can't just define a string on a single line and then use it. Instead we:

1. Define the area in memory that our string is going to stay in
2. Define the length of the data
3. Move the message into the correct register
4. Move the length of the data into the into correct register
5. perform any operations required using the correct OPCODE, or interrupt the kernel to perform a system call

This isn't as bad as it sounds, but is certainly a shift coming from a high level language like Python. Let's look at an easy example.

## Hello, World

asm files are composed primarily of 3 sections (though you can use more). The first section we'll talk about is the data section. The data section is where we store our global and static variables. We signify it by placing a "section .data" line in our .asm file and then define the variables using keywords like "db":

```assembly
section .data
    message: db "Hello, World", 10 ; 10 is the newline character
    mes_len: equ $ - message ; "$" means current position, then we take away the address of the message to get the length of the message
```

"db" means that we want to reserve a single byte of data. Those of you who know the inner workings of C know that a String is simply an array of characters, each character being a single byte. This is what we're doing in this line. Obviously "Hello, World" is more than a single byte, so instead their representation would be more like ["H", "e", "l", "l", "o", " ", "w", "o", "r", "l", "d", "\n", "\0"]. Of course, an array is a high level representation, so the variable "message" is actually a  pointer to memory address which holds the binary data of each character. We also need to know how long this data actually is [so that we know when to stop reading it](https://stackoverflow.com/a/45386640), which is what the line starting with "equ" is for. The next section is the "section .text", where we define our logic:

```assembly
section .text
    global _start

_start:
    mov edx, mes_len ; Store where the bytes end for the data in the data register
    mov ecx, message ; Store the reference to the message that we're going to write in the counter register
    mov ebx, 1 ; Tell the base register that we want to push to the stdout
    mov eax, 4 ; Move 4 into the Accumulator which is the equivalent of calling sys write on linux
    int 0x80 ; int means interrupt, 0x80 means interrupt the kernel to make our system calls

    mov eax, 1 ; Move 1 into accumulator, which is sys exit on linux
    mov ebx, 0 ; exit code 0
    int 0x80
```

The text section simply points to the "_start" method, so that we know the entrypoint of our program. We see in our start method that we do several operations. The first thing we do is populate our CPU registers. For the uninitiated, a CPU register is just some memory built onto the CPU, that the CPU uses to store any data it's currently working on. [There are many different types of register](https://www.swansontec.com/sregisters.html) and I don't have time to go into the history of all the different ones. For our use case, we first use the MOV opcode to move the "mes_len" into the data register. The data register was traditionally used for things like holding the remainder when doing division operations (storing the most significant bits in the data register and least significant in the accumulator)essentially simulating a 64-bit register. We can also use the data register for certain I/O operations, but here we're using these registers with the int opcode. int means that we're interrupting the kernel for a system call. We move 4 into the eax register (traditionally used as the accumulator) to signal the kernel that we want to perform a sys_write. We also put a 1 into the ebx to write to the stdout and put the reference to the message in the counter register, ecx. We've been talking about how a lot of these registers have different meanings historically vs. how we're using them now. This is because the System calls are free to use the registers as they [define them](https://faculty.nps.edu/cseagle/assembly/sys_call.html). This confusion can become a common theme when working with assembly, there are multiple ways to perform the same function and there are changes based on architecture, OS, or the whims of God.

The last section to discuss is one we don't use here, but will shortly. The block starting symbol section is used for variables that [will be assigned and changed at runtime](https://en.wikipedia.org/wiki/.bss). Here we see us reserving 4 bytes (32 bits) for the number we'll use for Fizz, the number we'll use for Buzz and the number we'll use for the iterations. 

To run this code we simply type:

```bash
nasm -f elf hello-world.asm
ld -m elf_i386 -s -o hello-world hello-world.o
```

```assembly
section .bss
	fizz resb 4
	buzz resb 4

	itera resb 4
```

## FizzBuzz

Now lets move onto loops and the stack. 


```assembly
section .text
    global _start

_start:
    mov dword, [fizz], 3 ; set fizz to 3
    mov dword, [buzz], 5 ; set buzz to 5
    mov dword, [iter], 0

    cmp dword [iter], loop_end ; If iteration is equal to end, jump to end
	je end

    ; pushl is the same as push, we just specify that we're using a long value, which can usually be inferred
    push $iter ; push current iteration onto the stack
    push $fizz ; push fizz onto the stack


    push $iter ; push current iteration onto the stack
    push $buzz ; push buzz onto the stack

    cmp dword [iter], loop_end ; If iteration is equal to end, jump to end
	je end

find_remainder

print_fizz:
    mov ecx, "fizz" ; Store the reference to the message that we're going to write in the counter register
    mov ebx, 1 ; Tell tha base register that we want to push to the stdout
    mov eax, 4 ; Move 4 into the Accumulator which is the equivalent of calling sys write on linux
    int 0x80 ; int means interrupt, 0x80 means interrupt the kernal to make our system calls

print_buzz:

print_fizzbuzz:


end:
    mov eax, 1 ; Move 1 into accumulator, which is sys exit on linux
    mov ebx, 0 ; exit code 0
    int 0x80


section .bss
	fizz resb 4
	buzz resb 4
    
    ; The remainders
    remainder_f resb 4
    remainder_b resb 4

	iter resb 4


section .global
    loop_end dd 100 ; only run 100 iteration

    fizz_word db "Fizz"
    fizz_len: equ $ - fizz_word

    buzz_word db "Buzz"
    buzz_len: equ $ - buzz_word

    fizzbuzz_word db "Fizzbuzz!"
    fizzbuzz_len: equ $ - fizzbuzz_word
```
## Congratulations