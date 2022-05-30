---
layout: post
title:  "Computer Algebra PickUP"
date: 2022-05-30 11:11:52 +0800
math: true
categories: learning
---
# Computer Algebra PickUP
采用的教材是[Joel S. Cohen Computer Algebra and Symbolic Computation](https://www.ukma.edu.ua/~yubod/teach/compalgebra/%5BJoel_S._Cohen%5D_Computer_algebra_and_symbolic_comp(BookFi.org).pdf). 

我坚定我的决心了, 不仅要学, 还要用最作死的方式来学 -- 我要用我不熟悉的Lisp Scheme, 来编写代码并且运行. 祝愿我好运吧. (笑哭). 

(又, 我用的是[LispPad](http://lisppad.objecthub.net)来运行代码的. 这个玩意应该在[macOS](https://apps.apple.com/de/app/lisppad/id1258939760?l=en&mt=12)和[iOS](https://apps.apple.com/us/app/lisppad-go/id1565747728)上都能用, 非常方便并且还有很多方便的库. 如果是Unix的话, 也可以使用[MIT-Scheme](https://www.gnu.org/software/mit-scheme/). 或者是[Chez Scheme](https://cisco.github.io/ChezScheme/#intro). 不过, 最后我还是使用的是LispPad, 毕竟无脑又简单嘛. 参考的手册在[这里](https://groups.csail.mit.edu/mac/ftpdir/mit-scheme/7.7/7.7.1/doc-pdf/scheme.pdf)和[这里](https://r6rs.mrliu.org))

(不过保险起见, 所有的代码我都再用Ruby实现一遍吧. 不过应该不会是同步的, 毕竟写代码很麻烦. 不过Ruby的代码现在还是改不掉那种丑陋的样子... )

(注: 因为我的数学不够好, 所以里面的所有数学名词, 能够瞎说的我都用自己的语言来瞎说了. )

## Quick Ref
- [Computer Algebra PickUP](#computer-algebra-pickup)
  - [Quick Ref](#quick-ref)
  - [Integers, Rational Numbers and Fields](#integers-rational-numbers-and-fields)
    - [Integers](#integers)
      - [带余除法(qutient $q$, remainder $r$)](#带余除法qutient-q-remainder-r)
      - [最大公约数 (Greatest Common Divisor Algorithm)](#最大公约数-greatest-common-divisor-algorithm)
      - [算术基本定理 Fundamental Theorem of Arithmetic](#算术基本定理-fundamental-theorem-of-arithmetic)
      - [中国剩余定理 Chinese Remainder Problem:](#中国剩余定理-chinese-remainder-problem)
      - [Integer Exercise](#integer-exercise)
    - [Rational Number Arthmetic](#rational-number-arthmetic)

## Integers, Rational Numbers and Fields
### Integers
#### 带余除法(qutient $q$, remainder $r$)
$$\forall a, b \neq 0, \exist! q \ s.t. \  a = q b + r, 0 \leq r \leq \vert b\vert  - 1$$

```scheme
(quotient a b)  ; MPL: iquot(a, b)
(remainder a b) ; MPL: irem(a, b)

(define iquot (lambda (a b) (quotient  a b)))
(define irem  (lambda (a b) (remainder a b)))
```

<details>
<summary><bold>irem</bold>与<bold>iquot</bold>的性质</summary>
<li>零元素为$m$, 特征为$m$: $a m + b = b (\mathrm{mod} m)$</li>
<li>加法结合律: $a + b + c = a + (b + c) (\mathrm{mod} m)$</li>
<li>保留加法: $a + b = a (\mathrm{mod} m) + b (\mathrm{mod} m)$ <br> 保留乘法: $a b = (a (\mathrm{mod} m)) (b (\mathrm{mod} m))$ <br> $\Rightarrow a^n = (a (\mathrm{mod} m))^n$ </li>
<li>$\mathrm{irem}(c b, c m) = c \mathrm{irem}(b, m), c > 0$</li>
</details><br>

整除 $b$ is $a$ divisor: $a \vert  b \Leftrightarrow (irem\ a\ b) = 0$, 且满足:
* 自反性: $a \vert  b \wedge b \vert  a \Rightarrow a = \pm b$
* 和运算: $a \vert  b \wedge a \vert  c \Rightarrow c \vert  (a + b)$
* 乘运算: $c \vert  a \Rightarrow c \vert  (a \cdot b)$
* 传递性: $a \vert  b \wedge b \vert  c \Rightarrow a \vert  c$
* 互素消除: $c \vert  (a b) \wedge \gcd(a, c) = 1 \rightarrow c \vert  b$
* 素因子: $c \vert  (a b) \wedge \mathrm{prime}\ c \rightarrow c \vert  a \vee c \vert  b$
* 互素因子: $a \vert  c \wedge b \vert  c \wedge \gcd$

#### 最大公约数 (Greatest Common Divisor Algorithm)
$\gcd (a, b) = d > 0 \Leftrightarrow \forall e((e\vert a \wedge e\vert b) \rightarrow e\vert d)$    
by definition, $\gcd (0, 0) = 0$. 
* 存在性
* 唯一性
* $\gcd (b, 0) = \vert b\vert $

> **Euclid's Greatest Common Divisor Algorithm**:    
> let $r = irem(a, b), \gcd(a, b) = \gcd(b, r)$    

```scheme
(define (gcd a b)
  (if (eq? b 0)
    (abs a)
    (gcd b (irem a b))))
```

<details>
<summary>(目前)没什么鸟用的小技术</summary>

Tail Recursion 尾递归优化: 对于那些函数在尾部有调用自身的函数, 编译器可以自动将函数编译为循环的形式来减少对栈的使用和负担. (并且同时也能够保持代码的抽象可读性)
</details><br>

在计算过程中, 会发现有这样的过程: 
1. $a = q_1 b + r_1 \Rightarrow r_1 = a - q_1 b$
2. $b = q_2 r_1 + r_2 \Rightarrow r_2 = b - q_2 (a - q_1 b) = -q_2 a + (1+q_1 q_2)b$
3. $r_1 = q_3 r_2 + r_3$
4. $\cdots \Rightarrow r_i = r_{i-2} - q_i r_{i-1} = (m_{i-2} - q_i m_{i-1}) a + (n_{i-2} - q_i n_{i-1}) b$
5. $r_{\rho-2} = q_{\rho} r_{\rho-1} + r_{\rho}, r_{\rho} = 0, \gcd(a, b) = r_{\rho - 1}$

于是可以有$\gcd(a, b) = m a + n b$, 得到以下的拓展算法: 

> **Extended Euclid's Greatest Common Divisor Algorithm**:    

```scheme
(define (ext-gcd a b)
  (define (iter-ext-gcd a b mm nn m n)
    (if (eq? b 0)
      (if (> a 0) (list a mm nn) (list (- a) (- mm) (- nn)))
      (let ((q (iquot a b)))
         (iter-ext-gcd b (remainder a b) m n (- mm (* q m)) (- nn (* q n))))))
  (iter-ext-gcd a b 1 0 0 1))
```

代码写得有点丑...

> 互素 (relatively prime): $\gcd(a, b) = 1$

#### 算术基本定理 Fundamental Theorem of Arithmetic
任一整数均可进行素因子分解: $n = \prod^s_n p_i^{n_i}$

(书中给出的是一个叫做`ifactor`的函数来分解整数, 但是并没有给出算法, 或者是虽然有, 但是电子版书里面没有带吧. 所以我只能自己给出一个比较烂的算法. )

<details>
<summary>ifactor 试除法</summary>

因为数学和编程都不是很会, 所以最后写出了一个丑陋的函数, 这个对稍微大一点的整数运算的速度就会奇慢无比. 

{% highlight scheme %}
(define (ifactor n)
  (define (times n p k)
  	(if (not (= 0 (remainder n p)))
    	(list n p k)
    	(times (quotient n p) p (+ 1 k))))

  (define (iter-ifactor n p)
    (cond
      ((= 1 n) '())
      ((prime? p)
       (let ((t (times n p 0)))
         (if (= 0 (car (cddr t)))
           	 (iter-ifactor n (+ 1 p))
           	 (append (list (cdr t)) (iter-ifactor (car t) (+ 1 p))))))
      (else (iter-ifactor n (+ 1 p)))))

  (iter-ifactor n 2))
{% endhighlight %}

之所以称这个算法是一个不好的算法, 是因为这个算法的思路就是从小到大依次取遍所有可能的素数, 然后将这些素数给作为要尝试的东西去试试. 
</details>
<br>

#### 中国剩余定理 Chinese Remainder Problem: 
1. 在同余意义下的相等: $\mathrm{irem}(a, c) = \mathrm{irem}(b, c)$
2. 于是得到同余方程: $f(a) \equiv b (\mathrm{mod} c)$
3. 最后得到一般的同余方程组:    
  $$\left\{\begin{array}{lll} x & \equiv & x_1 (\mathrm{mod} m_1) \\ x & \equiv & x_2 (\mathrm{mod} m_2) \\ \cdots & \cdots & \cdots \\ x & \equiv & x_k (\mathrm{mod} m_k) \\\end{array}\right. \quad \gcd(m_i, m_j) = 1, \forall i, j$$    
4. 对于上面的同余方程组, 先取其中两个:    
$$\left\{\begin{array}{lll} x & \equiv & x_1 (\mathrm{mod} m_1) \\ x & \equiv & x_2 (\mathrm{mod} m_2) \end{array}\right.$$    
  然后利用一个小trick, $\gcd(m_1, m_2) = c m_1 + d m_2$    
  $\Rightarrow c m_1 x_2 = (1 - d m_2) x_2 \equiv x (\mathrm{mod} \ m_2)$    
  于是构造$x = c m_1 x_2 + d m_2 x_1$满足问题条件.

(a solution of a system of integer remainder equations)

```scheme
(define (chinese-remainder eqs)
   (if (null? (cdr eqs))
     (let  ((x (caar eqs))
            (m (car (cdr (car eqs)))))
       (list (remainder x m) m))
     (let* ((x1 (caar eqs))
            (m1 (car (cdr (car eqs))))
            (x2 (car (car (cdr eqs))))
            (m2 (car (cdr (car (cdr eqs)))))
            (egcd (ext-gcd m1 m2))
            (c (car (cdr egcd)))
            (d (car (cddr egcd)))
            (x (+ (* c m1 x2) (* d m2 x1)))
            (m (* m1 m2)))
       (chinese-remainder
         (append (list (list x m))
                 (cddr eqs))))))
```

#### Integer Exercise
挑的都是大概可能会做的来做的, 不一定保证正确. 

* 向上取整和向下取整 floor and ceiling

```scheme
(define (floor n)
  (iquot (car n) (car (cdr n))))

(define (ceiling n)
  (let ((a (car n))
        (b (car (cdr n))))
    (if (= 0 (irem a b))
        (iquot a b)
        (+ (iquot a b)))))
```

(这两个函数在Scheme里面已经有内置了, 所以我在代码里面把他们注释了. )

* integer_divisors

(我承认, 我摆烂了, 这个东西想不出特别好的算法, 所以我就用暴力枚举的方法来做了. )

```scheme
(define (integer-divisors n)
  (define (iter t)
    (cond
      ((> (square t) n)
       '())
      ((= (irem n t) 0)
       (append
         (list t (- t)
               (iquot n t) (- (iquot n t)))
         (iter (+ 1 t))))
      (else (iter (+ 1 t)))))

  (iter 1))
```

* number of digits 统计给出输入整数$n$中的不同数字的出现次数? (原文: Give a procedure `Number_of_digits(n)` that returns the number of digits in n. )

我觉得对CAS好像没什么用... 所以不写了. 

* 基底转换, 把$n$转换为用$b$为基底的数字

```scheme
(define (base-rep n b)
  (if (= 0 n)
    '()
    (append (list (irem n b))
            (base-rep (iquot n b) b))))
```

* **Proof**: $a = u \gcd(a, b), b = v \gcd(a, b) \Rightarrow \gcd(u, v) = 1$    
  $$\left\{\begin{array}{lll}a & = & u (c a + d b) \\ b & = & v (c a + d b)\end{array}\right. \Rightarrow \gcd(a, b) = c a + d b = u \gcd(a, b) c + v \gcd(a, b) d \\ \Rightarrow 1 = u c + v d = \gcd(u, v)$$

* **Proof**: $\gcd(a, b) = 1 \Rightarrow \gcd(a^t, b^t) = 1$    
  使用数学归纳法证明即可, 当$\gcd(a^t, b^t) = 1$成立, 即$\phi_t a^t + \psi_t b^t = 1$, 又因为$\gcd(a, b) = 1 = \phi_1 a + \psi_1 b = 1$, 于是就能够构造$\phi_1 a (\phi_t a^t + \psi_t b^t) (\phi_1 a + \psi_1 b) = \phi_1 a$和$\psi_1 b = \cdots$, 加起来就好了. 

(大概是这么证明的吧...)

* **Proof**: $\gcd(c a, c b) = \vert c \vert \gcd(a, b)$    
  $$d = \gcd(c a, c b) = u c a + v c b \Rightarrow d = c d', d' = u a + c b$$    
  然后用反证法, 证明不存在比$d'$大的值即可. 

* $\mathrm{lcm}(a, b) = \frac{\vert a b \vert}{\gcd(a, b)}$
  * **Proof**: $\mathrm{lcm}(a, b) = m a + n b$    
    let $d = \gcd(a, b), a = k d, b = t d$, $\mathrm{lcm}(a, b) = t k (u a + v b)$. 
  * 代码... 

### Rational Number Arthmetic
**Define**: 
1. $b > 1$
2. $\gcd(a, b) = 1$

因为目前没有做值的类型的处理, 所以就只是单纯地做一个简单的做法. 以后重新构造的时候需要注意这个方面的东西. 

关于命名, 如果可以的话, 我会用那种和善的方式来命名. 

```scheme
(define (frac a b)               ; MPL FracOp
  (let ((d (gcd a b)))
    (if (> d 0)
      (list 'frac (/ a d) (/ b d))
      (list 'frac (/ (- a) d) (/ (- b) d)))))
```

```scheme
(define-syntax sum
  (syntax-rules ()
    ((_) 0)
    ((_ e0)
     (if (integer? e0) (frac e0 1) e0))
    ((_ e1 e2 ...)
     (if (integer? e1)
       (list 'add (frac e1 1) (sum e2 ...))
       (list 'add e1 (sum e2 ...))))))
```

<details>
<summary>Lisp わくわく</summary>
<p>「わくわく」とは、嬉しい・楽しいことが起きると期待して興奮し、心を躍らせ、心が落ち着かないさまを表現する語。</p>

<p>嗯, 现在稍微理解了一点点Lisp宏(<a href="http://www.shido.info/lisp/scheme_syntax_e.html">参考的教程看这里</a>)的美妙之处了. 也能够理解一点点Lisp为什么要有括号这样的奇怪的形式了. 因为括号的形式将过程用list这样类似于数据的形式来储存起来, 所以就有这样的一种可能性: <strong>为什么不将过程当做数据一样处理了之后再运行呢?</strong> 于是就有了宏. (至少我是这样认为的. ) 这样的话, 哪怕一开始的语言的表达能力弱得离谱, 但是也能够通过宏的形式来变得超级牛皮. </p>

<p>(又: 在做<a href="{{ site.github.url }}/ruby/ri-lang/">りlang</a>的时候, 我认为这样的语法格式的一个好处就是对于Parser来说非常的方便. 后来尝试过对中文的一个Parser, 遇到的问题就是格式不好搞... 最后就全身疾而终了. )</p>

<p>感觉这样的就不能够叫做宏了, 在<a href="https://zh.wikipedia.org/wiki/巨集">维基百科</a>上面的介绍是这样的: 绝大多数情况下，“宏”这个词的使用暗示着将小命令或动作转化为一系列指令。(因为目前我还没有这些编程基础, 所以一切都是在乱说)个人感觉就像是文本的按规则查找和替换. 然而, 在Lisp里面, 感觉更像是一种对输入的过程先处理在运算一样的东西. 有点像是Ruby里面的元编程? (这个之前我觉得有点难, 所以跳过没学. 以后再说. )</p>

<details>
<summary>关于群魔乱舞的一些乱七八糟的想法</summary>
<p>这个是我在学这个东西的同时突然想到的一个东西, 觉得很有意思所以记录了下来. 因为太短了所以就不单独开一个篇章来记录了. </p>

<p>人类的创造力应该是十分强大的. 至少我是这么希望的. 但是我目前感觉有种遗憾: 我一直在学习去掌握那帮比我早出生好多年的人的创造, 却没有办法去学习如何来像他们一样搞出那么多的乱七八糟的东西. </p>

<p>以我之前的<a href="{{ site.github.url }}/ruby/ri-lang/">りlang</a>为例, 现在看看, 发现里面受到我学逆向的影响实在是太深了. 并且里面的数据的表达方式也受到一般的想法太深了. 最后实现出来的东西完全就是C的那一套玩意, 完全没有Lisp(除了语法上的形式)的味道. (用我们老师diss声学的话来说就是太工程了, 虽然我觉得这样又没有什么问题... )</p>

<p>但是一开始Lisp的感觉是怎么样的呢? 我觉得简直是一种超级炫酷的想法. 这样的想法真的是数学家的那种美妙的点子. (<a href="https://zh.wikipedia.org/wiki/LISP#Lisp_1.5的7个原始运算">见Lisp1.5的7个原始运算</a>) 这样简直就像是图灵机一样, 或者就像是数学公理一样, 超级酷的好不好. </p>

<p>或者是Lambda演算, (图灵机什么的就不用说了, 毕竟状态转变的想法我觉得在类似C编程的过程中自然就能够理解了. ) 之前看了一点点的书, 感觉有一种新的想法. 忽然能够理解离散数学里面函数的映射的重要性了. 为什么要把运算看作是运算呢? 为什么不把它看作是一种映射关系呢? 于是运算就变成了两点之间的连线了... </p>

<p>然后是震惊我好久的形式逻辑(确切来说是一阶逻辑, 当时我在学离散数学的一阶逻辑, 然后我们老师多说了一嘴: 高阶逻辑用一阶逻辑来表示是困难的. 一开始的人工智能... 于是我就开始搜索了一下), 哇, 真得可以有这样有趣的方式来编程的吗? (我是指<a href="https://zh.wikipedia.org/wiki/Prolog">Prolog</a>) 还有是<a href="https://www.haskell.org">Haskell</a> 之类的... 虽然我没有真的实际操作过这些语言, 但是这些语言都有很棒的一些精神(至少它们宣言的精神很棒). </p>

<p>当然了, 那些整活的语言我就不说了. (可以看看<a href="https://www.youtube.com/watch?v=6avJHaC3C2U">The Art of Code</a>, 里面有很多整活的语言介绍. ) 虽然要说实际用它们来编写一个什么大工程之类的事情的, 我认为可能性并不大. 但是它们都指出了一种可能性, 那就是一种新的发展方向的可能性. 不过大概就跟《未选择的路》一样吧, 小径深幽, 难知前路; 大道一条, 路标插满. </p>

<p>不过现在看来, 这样的近乎群魔乱舞的编程语言, 真的是非常有意思. 至少对我这个门外汉来说, 就像是刘姥姥进大观园一样有意思. 这里我确实要同意一下我们计科导老师讲的一个话(当时我把りlang给他看): 假如你要设计自己的编程语言的话, 一定要让自己的语言和别的编程语言有所不同才行. 可能是老师认为我还没有能力干一些很有意思的事情, 所以他指出的方向大多是增加计算能力, 拓展计算功能的方向等(具体我忘了, 当时太紧张了). 可惜对于我这个门外汉来说, 大概我只能够学点道道, 让精通术的计算机系的同学们来承担老师的厚望了... 残念. </p>

<p>啊, 不管它了. </p>

<p>不过因为我还是刚开始接触这些, 肯定没有那些从高中开始就接触计算机的大佬牛... 里面的话说得不一定对, 就算错了, 那就看作是我现在理解错了吧... (高中的我要是能够使用电脑来跑代码, 而不是猫在教室里面看书的话, 我也不至于现在这么玩了... )</p>
</details>

<p>嗯, 编写了这个sum函数之后, 上面的想法就是我突然之间想到的, 觉得精妙无比, 特此记录下这些狗屁文字. 估计这样的理解可能是十分肤浅的一种理解了. </p>
</details>