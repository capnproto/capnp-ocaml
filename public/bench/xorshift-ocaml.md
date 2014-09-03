---
layout: page
title: "Assembly for OCaml Xorshift"
permalink: /public/bench/xorshift-ocaml.html
---

{% highlight gas %}
camlFastRand__next_1015:
.cfi_startproc
.L100:
	movq	camlFastRand@GOTPCREL(%rip), %rsi
	movq	24(%rsi), %rax
	movq	(%rax), %rdi
	movq	%rdi, %rbx
	decq	%rbx
	salq	$11, %rbx
	xorq	%rbx, %rdi
	orq	$1, %rdi
	movq	32(%rsi), %rbx
	movq	(%rbx), %rbx
	movq	%rbx, (%rax)
	movq	32(%rsi), %rax
	movq	40(%rsi), %rbx
	movq	(%rbx), %rbx
	movq	%rbx, (%rax)
	movq	40(%rsi), %rax
	movq	48(%rsi), %rbx
	movq	(%rbx), %rbx
	movq	%rbx, (%rax)
	movq	48(%rsi), %rbx
	movq	%rdi, %rax
	shrq	$8, %rax
	movq	24(%rsi), %rdx
	movq	(%rdx), %rdx
	shrq	$19, %rdx
	movq	(%rbx), %rcx
	xorq	%rdx, %rcx
	xorq	%rdi, %rcx
	xorq	%rax, %rcx
	orq	$1, %rcx
	movq	%rcx, (%rbx)
	movq	48(%rsi), %rax
	movabsq	$8589934591, %rbx
	movq	(%rax), %rdi
	andq	%rbx, %rdi
	movq	%rdi, (%rax)
	movq	48(%rsi), %rax
	movq	(%rax), %rax
	ret
	.cfi_endproc
{% endhighlight %}
