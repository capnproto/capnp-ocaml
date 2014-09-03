---
layout: page
title: "Assembly for C Xorshift (gcc -O2)"
permalink: /public/bench/xorshift-c.html
---

{% highlight gas %}
capnp_bench_nextFastRand:
.LFB10:
	.cfi_startproc
	movl	x.2445(%rip), %eax
	movl	w.2448(%rip), %ecx
	movl	%eax, %edx
	sall	$11, %edx
	xorl	%eax, %edx
	movl	y.2446(%rip), %eax
	movl	%eax, x.2445(%rip)
	movl	z.2447(%rip), %eax
	movl	%ecx, z.2447(%rip)
	movl	%eax, y.2446(%rip)
	movl	%ecx, %eax
	shrl	$19, %eax
	xorl	%ecx, %eax
	xorl	%edx, %eax
	shrl	$8, %edx
	xorl	%edx, %eax
	movl	%eax, w.2448(%rip)
	leaq	1(%rax,%rax), %rax
	ret
	.cfi_endproc
{% endhighlight %}

