---
layout: post
title:  "概率论与数理统计 - 期中"
date: 2022-10-22 23:49:44 +0800
math: true
categories: notes
---
# 概率论与数理统计 - 期中
虽然就要考试了, 但是上半个学期学得实在是有些迷幻. 
所以考前复习几乎和预习是一个样的. 

> Not only god knows, I know, and by the end of this semester, 
> you will know.  
> Sidney Coleman

## 一些概念
* 随机试验
* 样本空间 $\Omega$: 把所有的事件都表示出来的一个集合  
  于是事件的运算就变成了集合的运算
* 概率空间 $$\mathcal{F} \subseteq 2^\Omega, \mathrm{card}2^\Omega = \#2^\Omega = 2^{\#\Omega}$$  
  在样本空间 $$\Omega = \{w_1, \dots, w_n\}$$ 
  上定义 代数 $\mathcal{F} \subseteq 2^\Omega$. 
  代数中的元素 $A \in \mathcal{F}$ 有映射
  $P: \mathcal{F} \rightarrow [0, 1], A \mapsto P(A)$, 
  于是形成概率空间 $(\Omega, \mathcal{F}, P)$. 
* 随机变量: $X: \Omega \rightarrow \mathbb{R}, \omega \mapsto X(\omega)$  
  随机向量: $$\{X_1, \dots, X_n\}$$
* 条件概率 $P(A \mid B) = \frac{P(A B)}{P(B)}, P(A) = P(A \mid B)P(B)$ 
  * 离散
  * 连续: $f_{X\mid Y}(x\mid y) = \frac{f(x, y)}{f_Y(y)}$
* 期望 $EX = \sum P_i X_i = \int_{-\infty}{\infty} x f(x) \mathrm{d}x$: 
  * $E(\lambda X + \mu Y) = \lambda EX + \mu EY$, 线性
  * $E X = W X \cdot E Y \mathrm{iff} P(A ⋂ B) = P(A)P(B)$
* 条件期望: $E(X\mid A) = \sum X_i P(X_i \mid  A) = \int_{- \infty}{\infty} = x f(x\mid A)dx$  
  如果 $$A = \{Y = y\}, E(X\mid Y = y)=\sum x_i P(X = x_i \mid  Y = y) = \int_{- \infty }{\infty} x f_{X\mid Y}(x\mid y)\mathrm{d}x$$.  
  注意, $E(X\|Y)$ 不是一个数, 而更像是一个函数: 
  $$E(X\mid Y) (ω) = \sum E(X\mid Y=y)\boldsymbol{1}_{\{Y = y\}}(ω) $$
  * $E(E(X\mid Y)) = E X$
  * $E(X \mid X) = X$
  * $E(h(X)Y\mid X) = h(X) E(Y\mid X)$
  * $X, Y$ 独立, $E(X\mid Y) = EX$
  * $E(aX+bY\mid Z) = aE(X\mid Z)+bE(Y\mid Z)$
  * $E(E(X\mid Y)\mid Y) = E(X\mid Y)$
  * $E((X - E(X\mid Y)) \cdot h(X)) = 0$  
	记 $(X - E(X\mid Y)) \cdot h(X) = Z$, 于是计算 $E(E(Z\mid Y)) = EZ$ 
* 方差 $\mathrm{Var} X = E(X-EX)^2 = EX^2 - (EX)^2$
* 协方差 $Cov(X,Y) = E[(X-EX)(Y-EY)] = E(X Y) - (EX)(EY)$
* 全概率公式 (条件概率的推广) $P(A) = \sum_i P(B \mid E_i) P(E_i)$, 
  其中 $E_i$ 为 $\Omega$ 的一个划分. 

  > 一个好的分划就能够简化计算. 
  
* 贝叶斯公式: $P(A \vert B) = \frac{A \bigcap B}{P(B)}$
* 示性函数: $\mathbb{1}_{E_n} = 1\ \mathrm{if}\ E_n\ \mathrm{else}\ 0$  
  可以把这个示性函数看作是一个随机变量. 
  * 计数: 如果令 $$X_i = \mathbb{1}_{\{第i次正面向上\}}$$, 
	于是利用 $X = \sum X_i$ 就能够实现类似于计数的方式来计算得到一个分布. 
* 分布函数: $F(X) = P(X \leq x)$
  * 生存函数: $P(X>x) = 1-F(x)=S(x)$
  * 离散: $F(X) = \sum P(X)$
  * 连续: $F(X) = \int f(t) \mathrm{d}t, f(x) = F'(x)$ 为概率密度函数
  * 多维随机变量的情况 (随机向量):  
	$$\{ω\} \mapsto x = (x_1, x_2, \cdots, x_n)$$, 形成随机向量. 
	则 $F(x, y) = P(X \leq x, Y \leq y) = \int_0^x\int_0^y f(x, y)\mathrm{d}x\mathrm{d}y$. 
	其中 $f(x, y)$ 为一个联合分布.  
	从联合分布到边缘分布:  
	$P(X \leq x) = P(X \leq x, Y < + \infty) = lim_{y \rightarrow \infty} F(x, y)$
	* 离散: $P(X = x) = \sum P(X = x, y)$
	* 连续: $f_X(x) = \int_{-\infty}^{\infty}f(x, y)\mathrm{d}y$
* 独立性 $P(A \bigcap B) = P(A) P(B) ↔ A, B$ 独立  
  (注: )

* 概率不等式: 
  * Markov 不等式: 
  
  $$X \geq 0, EX < +\infty ⇒ ∀ c > 0, P(X \geq c) \leq \frac{EX}{c}$$
  
  * Cebyshev 不等式
  
  $$X: E|X|^2 < +\infty ⇒ ∀c>0,P(|X-EX|\geq c)\leq \frac{\mathrm{Var}X}{c^2}$$
  
* 中心极限定理: 
  设 $X_i$ 独立同分布 (与 $X$ 分布相同. )
  $E X = \mu, \mathrm{Var} X = \sigma^2 (0 < σ^2 < ∞)$, 
  令 $S_n=X_1 + \cdots + X_n$, 则当 $n \rightarrow \infty$, 
  $\frac{S_n - EX}{\sqrt{n \mathrm{Vat}X}} \rightarrow N(0,1)$
* 大数定律 (频率逼近概率): 
  * 弱大数定律: 设 $X_1, X_2, \cdots, X_n$ 独立同分布, 
	$E|X|^2 < +∞$, 令 $S_n = \sum X_i$, 则 
	$\forall ε > 0, P(|\frac{S_n}{n} - EX | > ε) \rightarrow 0$ 
  * 强大数定律: $\frac{S_n}{n} \rightarrow EX. $
* Story Proof: 分别为左式和右式讲一个故事, 并证明这两个故事是等价的. 

## 随机变量和概率分布和其他

| 分布         | 表达式                                    | $EX$               | $\mathrm{Var}X$    | $Et^X$             |
|--------------|-------------------------------------------|--------------------|--------------------|--------------------|
| 伯努利分布   | $P(X=1)=p,P(X=0)=1-p$                     | $p$                | $p(1-p)$           | $pt+(1-p)$         |
| 二项分布     | $P(k)=C_n^kp^k(1-p)^{n-k}$                | $np(1-p)$          | $np(1-p)$          | $(pt+(1-p))^n$     |
| 泊松分布     | $\frac{e^{-\lambda}\lambda^k}{k!}$        | $\lambda$          | $\lambda$          | $E^{\lambda(t-1)}$ |
| 负二项分布   | $P(k)=C_{r+k-1}^{r-1}(1-p)^kp^r$          | $\frac{r(1-p)}{p}$ | $r\frac{1-p}{p^2}$ |                    |
| 标准正态分布 | $\frac{1}{\sqrt{2\pi}}e^{-\frac{x^2}{2}}$ | $0$                | $1$                |                    |
| $\cdots$     |                                           |                    |                    |                    |

(关于随机变量的分布之类的计算, 一般的操作就是难算, 非常的难算. 
然后就是各种利用求和关系变来变去... )

## Exercise
* 随机事件
  
  > 写出随机事件和样本空间
  
  > 星期二男孩问题  
  > (关于知道的越多越怎么样的问题... 属于是变态问题)  
  > * 有两个小孩, 一个是男孩, 且在上海出生
  >   (假设某个小孩出生在上海的概率为 $\frac{1}{30}$),
  >   那么另外一个也是男孩的概率. 
  > * 
  
  > 事件的运算: 
  > * 证明 $P(A_1 \cup \cdots \cup A_n) = \sum_{i = 1}^n P(A_i) - \sum_{i \leq i < j \leq n} P(A_i \cap A_j) + \sum_{1 \leq i < j \leq n} P(A_i \cap A_j \cap A_k) + \cdots + (-1)^{n+1} P(A_1 \cap \cdots \cap A_n)$  
  >   就是容斥定理啊. 
  > * 从数集 $$\{\dots\}$$ 中取一个数, 不能被 $k$, 不能被 $l$ 整除. 
  >   这种就是利用上面的容斥定理来计算. 
  
* 事件的独立性
  
  > * 证明 $A, B$ 独立的充要条件是 $P(A \mid B) = P(A \mid \hat{B})$  
  >   类似的问法还有很多... 都是在问什么和什么独立的条件. 
  >   做的基本思路是这样的: 推导 $P(A)P(B) = P(A \cap B)$.

* 事件分布
  
  > 计算事件的分布表 (离散), 计算随机变量的分布函数

* 一些值的计算
  * 期望
  * 方差
  * 协方差
  * 矩
* 高端的证明: Story Proof  
  需要注意的是, 一般给左边等式找了一个含义之后, 还要给右边等式也找一个, 
  哪怕再怎么简单的含义. (只是为了说明这是一个 Proof, 仅此而已. )

