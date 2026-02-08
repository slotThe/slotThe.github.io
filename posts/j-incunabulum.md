---
title: "The J Incunabulum"
date: 2026-01-11
last-modified: 2026-01-11
two-column: true
tags: c, j, array-lang
---

<div class="fullwidth">
Ever wanted to find out what
[that one page](https://code.jsoftware.com/wiki/Essays/Incunabulum)
of totally cryptic C is all about?
</div>

<!--more-->

<div class="fullwidth">
While I'm waiting for *current disastrous weather event* to clear up, why not do something fun,
like trying to understand an interpreter for a line-noise language that someone wrote in a different kind of line-noise language almost 40 years ago?

If you're out of the loop:
the [J Incunabulum](https://code.jsoftware.com/wiki/Essays/Incunabulum)
is a one-page interpreter for the [J](https://www.jsoftware.com/) programming language,
written in ~1989 by [Arthur Whitney](https://en.wikipedia.org/wiki/Arthur_Whitney_(computer_scientist)),
of [k](https://en.wikipedia.org/wiki/K_(programming_language))
and [q](https://en.wikipedia.org/wiki/Q_(programming_language_from_Kx_Systems)) fame.

Note that this post is a bit of an experiment, in various ways.
The most visually striking might be that I'm trying out a semi-experimental two-column layout for displaying code on the right,
and some hopefully enlightening commentary on the left.
This is probably completely broken if you view this post in an RSS viewer, or on a phone,
with the best-case being that you just experience this as a normal, linear, post.
Sorry about that.
If you notice any CSS irregularities because you're using a vertical monitor with a 1504×2256 resolution or something,
please [let me know](https://tony-zorman.com/about.html).
</div>

---

<div class="fullwidth">
So, the goal for today is to understand an interpreter for a tiny array-style programming language.
Note that all of the names and explanations I'll come up with are probably slightly incorrect.
I don't know how Arthur Whitney thinks, nor am I sure that the names all fit.
Plus, I don't actually know C, so it may well be true that I messed up some of the casts,
and the original program actually worked slightly differently.
Consult at your own risk.

Further, this is not the minimal number of changes to get this to
compile on a modern machine—for example, I switched to ANSI C function
definitions—, but it helped me better understand what's going on.
Here's the minimal set of changes to [the original](https://code.jsoftware.com/wiki/Essays/Incunabulum) I've found to work:

<details><summary>Diff</summary>
``` diff
@@ -1,11 +1,11 @@
-typedef char C;typedef long I;
+typedef char C;typedef long long I;
 typedef struct a{I t,r,d[3],p[2];}*A;
 #define P printf
 #define R return
 #define V1(f) A f(w)A w;
 #define V2(f) A f(a,w)A a,w;
 #define DO(n,x) {I i=0,_n=(n);for(;i<_n;++i){x;}}
-I *ma(n){R(I*)malloc(n*4);}mv(d,s,n)I *d,*s;{DO(n,d[i]=s[i]);}
+I *ma(n){R(I*)malloc(n*8);}mv(d,s,n)I *d,*s;{DO(n,d[i]=s[i]);}
 tr(r,d)I *d;{I z=1;DO(r,z=z*d[i]);R z;}
 A ga(t,r,d)I *d;{A z=(A)ma(5+tr(r,d));z->t=t,z->r=r,mv(z->d,d,r);
  R z;}
@@ -34,8 +34,8 @@
 A ex(e)I *e;{I a=*e;
  if(qp(a)){if(e[1]=='=')R st[a-'a']=ex(e+2);a= st[ a-'a'];}
  R qv(a)?(*vm[a])(ex(e+1)):e[1]?(*vd[e[1]])(a,ex(e+2)):(A)a;}
-noun(c){A z;if(c<'0'||c>'9')R 0;z=ga(0,0,0);*z->p=c-'0';R z;}
-verb(c){I i=0;for(;vt[i];)if(vt[i++]==c)R i;R 0;}
+I noun(c){A z;if(c<'0'||c>'9')R 0;z=ga(0,0,0);*z->p=c-'0';R z;}
+I verb(c){I i=0;for(;vt[i];)if(vt[i++]==c)R i;R 0;}
 I *wd(s)C *s;{I a,n=strlen(s),*e=ma(n+1);C c;
  DO(n,e[i]=(a=noun(c=s[i]))?a:(a=verb(c))?a:c);e[n]=0;R e;}
```
</details>
Anyways, let's crack on.
</div>

---

# The array type

We first include some libraries, which were apparently implicit in the original thing.

``` c
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
```

The next three definitions are just utility functions:
`P` and `R` are to make the code even terser,
and `DO` is a neat way to compactify loops.
For example, given some `int a=0`, then `DO(n,a+=i)` is the same as `for(int i=0; i<n; ++i) a+=i;`.
Notice how the argument `x` has access to the loop variable `i`.

``` c
#define P printf
#define R return
#define DO(n,x) {I _n=(n),i=0;for(;i<_n;++i){x;}}
```

Next, some typedefs. `C` for `char`, `V` for `void` (added by me), and `long long` for `I`.
The last definition is particularly important,
as `I` will serve as an opaque type for some of the program:
it could either hold a character, an actual number (in which case it's seen as an index into an array of function pointers),
or a pointer to an array (this includes numbers as [rank][] 0 arrays).
The original program was written on a 32-bit machine, so one had `typedef long I;`,
but on modern systems we need 64-bit integers for this.

``` c
typedef char C; typedef void V; typedef long long I;
```

Defining functions works with `V1` for [monadic][monadic] [verbs][verb] (functions in J parlance)
and `V2` for [dyadic][dyadic] ones.
Functions defined this way take "implicit" arguments `a` and `w`
(or just `w` in the monadic case).
I guess this is based on APL's α and ω arguments.

``` c
#define V1(f) A f(A w)
#define V2(f) A f(A a, A w)
```

This is the main attraction—the array struct `a`,
with pointer type `A`.
It contains the following fields:

  + `t`, the type: this is 0 if the array is unboxed, and 1 if it is [boxed][box].
  + `r`, the [rank][rank]: a number from 0 to 2, indicating the rank of the array
    (whether it's a [scalar][scalar], a [vector][vector], or a [matrix][matrix]).
  + `d`, the dimension(?): the [shape][shape] vector of the array; i.e., the number of elements in each dimension.
  + `p`, the pointer(payload?): a pointer to the rest of the data.
    It's a [flexible array member](https://en.wikipedia.org/wiki/Flexible_array_member),
    but I'm honestly not sure why it's not just declared as `p[]`.

``` c
typedef struct a{I t,r,d[3],p[2];}*A;
```

Now we can define small helpers:
`ma` allocates an integer array of size `n` and returns it,
`mv` moves data from one array to another,
and `tr` returns the number of elements of an array, given its rank and its dimension vector.
Perhaps `tr` stands for "total rank" or something similar.

``` c
I*ma(I n){R(I*)malloc(n*sizeof(I));}
V mv(I*d,I*s,I n){DO(n,d[i]=s[i]);}
I tr(I r,I*d){I z=1;DO(r,z=z*d[i]);R z;}
```

The next function is one of the main attractions.
I'd imagine `ga` stands for "general allocator"/"generate array" or something in that vein,
and it does exactly what it says on the tin:
it creates a new array `z`,
allocates the total number of elements needed (`tr(r,d)`),
plus five spots for `t`, `r`, and `d` itself,
and then copies everything over.

``` c
A ga(I t,I r,I*d){A z={0};z=(A)ma(5+tr(r,d));z->t=t,z->r=r,mv(z->d,d,r);
 R z;}
```

# Verbs

Now, we get to defining some [verbs][verb].
The easiest to understand is surely the not-implemented `find`!
But `id` is a close second.

``` c
V2(find){}
V1(id){R w;} /* expands to A id(A w){return w;} */
```

The [iota][iota] verb enumerates the integers from `0` to one minus its argument (it returns an empty array if given `0`),
`sha` returns the [shape][shape] vector of the array,
and `size` returns the length of the first dimension.

``` c
V1(iota){I n=*w->p;A z=ga(0,1,&n);DO(n,z->p[i]=i);R z;}
V1(sha){A z=ga(0,1,&w->r);mv(z->p,w->d,w->r);R z;}
V1(size){A z=ga(0,0,0);*z->p=w->r?*w->d:1;R z;}
```

Addition is implemented as one would expect:
allocate a result array with the same shape as the input,
and add corresponding elements.

``` c
V2(plus){I r=w->r,*d=w->d,n=tr(r,d);
 A z=ga(0,r,d);DO(n,z->p[i]=a->p[i]+w->p[i]);R z;}
```

[Cat][catenate] joins two arrays into a single vector,
and [box][enclose] wraps an array in a scalar box.
This is, I think, the first instance of abusing `I` as an opaque type:
notice the cast `*z->p=(I)w` to turn the entire given array into the first element of `z->p`
(which is an integer array).

``` c
V1(box){A z=ga(1,0,0);*z->p=(I)w;R z;}
V2(cat){I an=tr(a->r,a->d),wn=tr(w->r,w->d),n=an+wn;
 A z=ga(w->t,1,&n);mv(z->p,a->p,an);mv(z->p+an,w->p,wn);R z;}
```

[From][from] implements selection.
Given an index `a` (or an array, in which case only the first value is used) and an array `w`,
it returns the sub-array at that index.

``` c
V2(from){I r=w->r-1,*d=w->d+1,n=tr(r,d);
 A z=ga(w->t,r,d);mv(z->p,w->p+(n**a->p),n);R z;}
```

[Reshape][reshape] changes the shape of an array.
The left argument specifies the new shape.
The, in my opinion, incredibly cursed last line copies elements cyclically if needed.
We first fill all of `w` into `z`,
and then walk two pointers, one ahead of the other by `wn`, forward `n-wn` steps,
filling up elements as we go.
Later iterations can already access the ones resulting from earlier memmoves.

``` c
V2(rsh){I r=a->r?*a->d:1,n=tr(r,a->p),wn=tr(w->r,w->d);
 A z=ga(w->t,r,a->p);mv(z->p,w->p,wn=n>wn?wn:n);
 if(n-=wn)mv(z->p+wn,z->p,n);R z;}
```

# The REPL

The main part of the program is a small REPL.
It reads a line, calls `wd` (word) to parse it,
then executes it with `ex`,
and finally pretty-prints it with `pr`.

``` c
int main(){C s[99];while(fgets(s,99,stdin)){pr(ex(wd(s,strlen(s)-1)));}}
```

## Parsing

Let's first define the verb table `vt`,
giving names to the functions defined above.
There are arrays of function pointers for monadic (`vm`) and dyadic (`vd`) verbs;
for example, `6#1,2` is reshape, `<a` is box, and so on.

``` c
C vt[]="+{~<#,"; /* +    {    ~    <   #   ,    */
A(*vd[])(A,A)  = {0,plus,from,find,0  ,rsh,cat},
 (*vm[])(A)    = {0,id  ,size,iota,box,sha,0  };
```

For parsing we need two small utility functions that check whether the given character is a [noun][] or a verb.
Here, a noun is a number between 0 and 9,
which is then immediately transformed into a rank 0 array, cast to an integer, and returned.
In case of a verb, return the index of it in the `vt` array incremented by one.
The plus one is due to both functions returning a plain 0 if nothing matches,
so both `vd` and `vm` have that as an "error state".
Note that this is different from inputting a literal 0, which would be transformed into an array.

``` c
I noun(C c){A z;if(c<'0'||c>'9')R 0;z=ga(0,0,0);*z->p=c-'0';R(I)z;}
I verb(C c){I i=0;for(;vt[i];)if(vt[i++]==c)R i;R 0;}
```

Parsing is done character by character,
exactly as one would expect: allocate an integer array `e`,
check if each character is a noun or a verb,
and store the result back into `e`.
If it is neither, just store the character's value.
The `x = cond1 ? y : cond2 ? z : w`-type ternary operator chaining is a classic for Whitney-style programs.
Make of that what you will.

``` c
I*wd(C*s,I n){I a,*e=ma(n+1);C c;
  DO(n,e[i]=(a=noun(c=s[i]))?a:(a=verb(c))?a:c);e[n]=0;R e;}
```

## Execution

Onto execution.
There's enough memory for 26 variables, `a` to `z`, which are stored in the `st` array.
Execution—like parsing—is done character by character.
There are two queries that will be important here:
with `qp` one can ask whether the character is a letter,
and `qv` checks if it's a verb (all of the symbols `+{~<#,` have an ASCII value less than `a`).

``` c
A st[26]; I qp(I a){R a>='a'&&a<='z';} I qv(I a){R a<'a';}
```

Execution itself is done right-to-left,
using a recursive descent approach.
If the currently scrutinised character is a letter,
and is followed by an equals sign, then we execute whatever is to the right of that equals sign,
store the information in `st`, and return.
Otherwise, whatever value happens to be in `st` for that letter is cast to an integer and stored in `a`.
Now `a` either points to a character in the input,
is an actual integer (in which case it's an index into the `vm` or `vd` arrays),
or is a pointer to an array.
Next, we check if `a` is a verb;
if it is, then it must be a monadic one,
so we call the corresponding function pointer with the result of first evaluating its argument.
If `a` is not a verb, we check if there is anything to its right.
If yes, this thing to its right must be a dyadic function,
so we call that dyadic function with its left argument `a`—which is already evaluated, and we now know must be an array—, and with the array its right argument evaluates to.
If none of those conditions match, `a` must be a lone array that is already fully evaluated, so return that.

``` c
A ex(I*e){I a=*e;
  if(qp(a)){if(e[1]=='=')R st[a-'a']=ex(e+2); a=(I)st[a-'a'];}
  R qv(a)?(*vm[a])(ex(e+1)):e[1]?(*vd[e[1]])((A)a,ex(e+2)):(A)a;}
```

## Pretty printing

`pi` prints an integer, and `nl` prints a newline.

``` c
V pi(I i){P("%lld ",i);} V nl(){P("\n");}
```

The main pretty-printing function is `pr`.
The first line prints the shape vector of the given array.
The second line checks whether the array is boxed;
if it's not, then it just prints the array as a flat list—hence the shape information in the first line, I suppose.
If it is a boxed array, we indicate that with `< `, and recurse.
Here we dangerously reinterpret the integer that's stored in each `w->p[i]` as a pointer to an array.

``` c
V pr(A w){I r=w->r,*d=w->d,n=tr(r,d);DO(r,pi(d[i]));nl();
 if(w->t)DO(n,P("< ");pr((A)w->p[i]))else DO(n,pi(w->p[i]));nl();}
```

# Conclusion

Obviously, this code is *super* brittle and basically segfaults if you look at it wrong.
When holding it just right, however, it does sort of work,
and then it feels a bit magical—here are the core ideas of an array programming language in barely 40 lines of C.
Plus, it made me feel a bit like a computer science archaeologist.
An afternoon well spent, I'd say!
If you're interested in playing with this yourself,
I've pushed everything in this post to a
[Codeberg repository](https://codeberg.org/slotThe/J-Incunabulum).

```
$ gcc -std=c89 j.c && ./a.out
    1,2,8,7
4
1 2 8 7
    a=~7
7
0 1 2 3 4 5 6
    a+a
7
0 2 4 6 8 10 12
    b=2,3
2
2 3
    c=b#a
2 3
0 1 2 3 4 5
    a=<1,2,3,4

< 4
1 2 3 4

    b=a,1
2
94660986832144 1
```

[box]: https://aplwiki.com/wiki/Box
[catenate]: https://aplwiki.com/wiki/Catenate
[dyadic]: https://aplwiki.com/wiki/Dyadic_function
[enclose]: https://aplwiki.com/wiki/Enclose
[from]: https://aplwiki.com/wiki/From
[iota]: https://aplwiki.com/wiki/Index_Generator
[matrix]: https://aplwiki.com/wiki/Matrix
[monadic]: https://aplwiki.com/wiki/Monadic_function
[noun]: https://aplwiki.com/wiki/Array_(syntax)
[rank]: https://aplwiki.com/wiki/Rank
[reshape]: https://aplwiki.com/wiki/Reshape
[scalar]: https://aplwiki.com/wiki/Scalar
[shape]: https://aplwiki.com/wiki/Shape
[vector]: https://aplwiki.com/wiki/Vector
[verb]: https://aplwiki.com/wiki/Function
