---
layout: post
title:  "Mid-term `Preview' of Calculus"
date:   2022-04-16 19:13:36 +0800
math: true
categories: notes
---
# ~~Preview~~
## 都学了啥
### 寒假的补充部分
#### 反常积分

$$\lim_{a \rightarrow x_1, b \rightarrow x_2} \int_a^b f(x) \mathrm{d}x, x_1, x_2 \in \mathbb{R} \cup \{\pm  \infty \}$$

1. 无界区间的反常积分
  * **柯西主值**: $\lim_{A\rightarrow\infty}\int_{-A}^{A} f(x) \mathrm{d}x$
  * **柯西收敛准则**: $\forall \varepsilon > 0, \exist N > 0, \forall A, B > N, \vert \int_A^B f(x) \mathrm{d}x\vert  < \varepsilon$
2. 无界函数的反常积分
  * **瑕点**: 积分收敛的点
  * **柯西收敛准则**: $\forall \varepsilon > 0, \exist \delta > 0, \forall A, B \in (a, a + \delta), \vert \int_A^B f(x) \mathrm{d}x\vert  < \varepsilon$

绝对收敛$\Rightarrow$收敛

#### 反常积分的收敛判断
* 非负函数的反常积分判断方法
  * **三明治方法**: 比较另外的已知收敛性的函数
  * **p-判断法**: $f(x) < \frac{1}{x^p}, (p>1)$或者$\lim_{x \rightarrow \infty} x^p f(x) = c, (p > 1)$
* 一般函数的反常积分判断方法
  * **Dirichlet判别法**: 
    1. $F(u) = \int^u_{x_0} f(x) \mathrm{d}x$有界
    2. $g(x) \rightarrow 0$单调趋于零
  * **Abel判别法**: 
    1. $\int^{\infty}_{x_0}f(x) \mathrm{d}x$收敛
    2. $g(x)$单调有界

> 如何判断为反常积分是否收敛: 
> * p收敛: 一般对于多项式的积分比较适用
>   > $$\int_{0}^{+\infty} \frac{1}{\sqrt[3]{1+x^4}} \mathrm{d}x \leadsto \frac{1}{x^{4/3 > 1}} \Rightarrow 收敛$$
> * Dirichlet和Abel收敛
>   > $$\int_1^{+\infty} \frac{\sin \sqrt{x}}{x} \mathrm{d}x {u = \sqrt{x} \above{} \Rightarrow } \int_1^{+\infty} \frac{\sin u}{u} \mathrm{d}u\\\int_1^{+\infty}\sin u\mathrm{d}x 有界, \frac{1}{u} \rightarrow 0 \Rightarrow 收敛$$
> 
>   > $$\int \vert \frac{\sin x}{x} \vert \mathrm{d}x \geq \int \vert \frac{\sin^2 x}{x} \vert \mathrm{d}x = \int \vert \frac{1 - \cos 2x}{2x} \vert \mathrm{d}x \\ \int \frac{1}{2x} \mathrm{d}x 发散, \int \vert \frac{\cos 2x}{2x} \vert \mathrm{d}x 收敛\\ \Rightarrow 原积分发散$$

### 函数项级数 - 函数列
* **定义**: 称$\{f_n\vert f_n:E \rightarrow \mathbb{R}\}$为*函数列*,   
  并且称$D = \{x \in E \vert  \lim_{n\rightarrow\infty} f_n \in \mathbb{R}\}$为其*收敛域*.    
  $f(x) = \lim_{n\rightarrow\infty} f_n$为*极限函数*

* **逐点收敛**: 
  $$\forall x \in D, \forall \varepsilon > 0, \exist N \in \mathbb{N}, \forall n > N, \vert f_n(x) - f(x)\vert  < \varepsilon$$
* **一致收敛**: 
  $$\forall \varepsilon > 0, \exist N \in \mathbb{N}, \forall n > N, \vert \vert f_n(x) - f(x)\vert \vert _D < \varepsilon \Leftrightarrow \{f_n(x)\} \rightrightarrows f(x)$$    
  其中, 
  $$\vert \vert f\vert \vert _D = \sup_{x \in D} \{f(x)\}$$ 
  称为*最大值范数*, 
  满足三角形不等式    
  *注意区分逐点收敛和一致收敛的区别, 一个是绝对值, 一个是最大值范数. 逐点收敛和一致收敛是不一样的.*
* **柯西收敛准则**: $\vert \vert f_n - f_m\vert \vert  < \varepsilon, \forall n, m > N(\varepsilon)$
* ***不一致收敛*的充要条件**: $\exist \{x_n\} \subset D, {f_n(x) - f(x)} \nrightarrow 0$
* **内闭一致收敛**: 闭区间$(\subset D)$上的一致收敛
* Dini定理: $[a,b]上连续的函数列\{f_n\}, 对\forall x \in [a,b] 有 \{f_n(x)\}递减趋于零 \Rightarrow \{f_n\} 一致趋于0$
* **等度连续**: $\forall \varepsilon > 0, \exist \delta(\varepsilon) > 0, \forall x, y \in E, \vert x - y \vert < \delta(\varepsilon), n \geq 1, \vert f_n(x) - f_n(y) \vert < \varepsilon$     
  等度连续$\Rightarrow f_n(x)$连续, 反之不然
* Ascoli-Arzela定理: 
  1. $\{f_n(x)\}$逐点有界
  2. $\{f_n(x)\}$等度连续
  
  则存在一致收敛的子函数列. 
* 一致连续的性质: 极限, 积分, 微分求和换序

> 注: 上面的收敛域指的是使得函数列$\{f_n\}$相对$n$极限存在的$x$的区域

> 最大值范数的使用:    
> $\lim_{n\rightarrow\infty} \sup f_n(x)$, 
> 这种一般是先把外层的极限固定, 然后计算出里面的最大值之后再去取极限.     
> 然后用这种方法就可以检测是否一致收敛了. 

> 函数列的**逐点收敛**判断:    
> 通过构造一个$x(n)$然后代入, 让$n \rightarrow \infty$的时候$f_n\nrightarrow f(x)$    
> 比如: 
> * $f_n(x) = x^n$, 在$x \geq 1$时就不收敛
> * $\sin \frac{x}{n}$, 取$x=n$则不收敛

> 函数列的**一致收敛**验证:    
> 先算出$f(x)$然后做差$f_n - f$, 计算出差最大的时候(一般求导, 得到$x(n)$), 
> 然后代入$f_n(x(n))$看看会不会趋于无穷.      
> 比如: 
> * $f_n(x) = (1+\frac{x}{n})^n, x \in [0, 1]$     
>   极限函数$f(x) = e^x$显然, 然后就是$\sup\{\vert f_n - f \vert\}$, 用求导的方式计算出导数恒大于零, 于是单调递增, 所以最大值会在$x=1$取到, 但是收敛. 
> * $f_n(x) = n x e^{-n x^2}, x \in (0, \infty)$     
>   先求极限函数$\lim_{n \rightarrow \infty} f_n(x) = 0$, 然后再求$g(x) = \vert f_n(x) - f\vert$相对$x$的最大值$x(n)$, 然后看看是否会有$\lim_{n \rightarrow \infty} g(x(n)) \rightarrow 0$
> 
> 若要证明**不**一致收敛, 就需要找一个让$\{f_n(x_n) - f(x_n)\}$不收敛到0的子列$\{x_n\}$    
> 比如: 
> * $f_n(x) = \frac{n x}{1 + n^2 x^2}$, 构造$x_n = \frac{1}{n}$就会让$f_n = \frac{1}{2}$, 不收敛, 所以$(0, 1)$就是不一致收敛的, 然后$(1, +\infty)$是一致收敛的. 

> 一致连续函数列的性质的利用:    
> 

### 函数项级数的一致收敛性
> 注: 这个是指部分和函数列在$n$趋于无穷的时候的收敛性质

* 部分和: $S_n(x) = \sum_{k=1}^{n} f_n(x)$
* 收敛区域: $\{x \vert \lim_{n \rightarrow \infty} S_n(x) \in \mathbb{R}\}$    
  收敛点, 发散点

> 对于能够给出$S_n(x)$解析表示的问题, 一般直接就可以当作是$\{S_n(x)\}$函数列来做了. 

> $\sum \frac{(-1)^n}{n+\sin x}$, 一般看到$(-1)^n$就会想到Dirichlet判别法: 
> * $v_n = \frac{1}{n + \sin x}$对$n$单调
> * $u_n= (-1)^n$ 有界

### 幂级数
* **Abel 第一定理**: $\sum_{n=0}^{\infty} a_n x^n$在$x_0$收敛, 
  则在$(- \vert x_0 \vert, \vert x_0 \vert)$区间收敛. 
* **Abel 第二定理**: 对收敛区间为$(-R, R)$的幂级数: 
  1. 幂级数在收敛区间上内闭一致收敛
  2. 若幂级数的一个端点收敛, 则幂级数在$(-R, R]$或$[-R, R)$收敛
* 幂级数的**收敛半径**, **收敛区间**    
  *注意求出了收敛半径, 收敛区间还要考虑边界情况是否包含.*
* 收敛半径的确定方法: 
  * **比率判别法**: 收敛半径$R = \lim_{n \rightarrow \infty} \vert \frac{a_n}{a_{n+1}} \vert$
  * **根式判别法**: 收敛半径$R = \lim_{n \rightarrow \infty} \frac{1}{\sqrt[n]{a_n}}$
* 幂级数性质: 
  * 连续
  * 积分换序
  * 导数换序   
    导数后收敛半径不变, 但是端点的收敛性可能会改变
* 泰勒级数: 泰勒展开的级数

> 计算收敛半径以及判断收敛域
> * 比式
> * 根式

> 二项式展开     
> 
> $$(1 - x)^\alpha = \sum^\infty_{k=0} C_{\alpha}^k (-t)^k$$

### 傅立叶级数 - 三角级数
* 三角级数
  * 收敛性
  * 唯一性
* 傅立叶级数: 

$$a_n = \int_{-\frac{T}{2}}^{\frac{T}{2}}f(x) \cos(\frac{2 n \pi}{T} x) \mathrm{d}x \\ b_n = \int_{-\frac{T}{2}}^{\frac{T}{2}}f(x) \sin (\frac{2 n \pi}{T} x) \mathrm{d}x$$

* 函数内积: $\langle f, g \rangle = \int_a^b f(x) g(x) \mathrm{d} x$    
  对于三角函数系, 内积就是
* 范数: $\vert\vert f(x) \vert\vert_{L^2} = \sqrt{\langle f, f\rangle}$
* 内积正交: $\langle f, g \rangle = 0$
* Parseval 不等式:    
  对于标准正交函数系

$$\langle f - f_n , f - f_n\rangle = \langle f, f \rangle - \sum_{k=1}^n a_k^2$$

其中$a_k = \langle f, \phi_n \rangle, f_n(x) = \sum_{k=1}^n a_k \phi_k(x)$, 
并且满足$\sum_{n=1}^{+\infty} a_n^2 \leq \langle f, f \rangle$. 

* 贝塞尔不等式: $\frac{a_0^2}{2} + \sum_{n=1}^\infty(a_n^2 + b_n^2) \leq \frac{2}{T} \int_{-\frac{T}{2}}^{\frac{T}{2}}f^2(x)\mathrm{d}x$
* 黎曼-勒贝格定理: $\lim_{n \rightarrow \infty} \int_0^\pi f(x) \cos n x \mathrm{d}x = 0, \lim_{n \rightarrow \infty} \int_0^\pi f(x) \sin n x \mathrm{d}x = 0$
* $\lim_{n \rightarrow \infty} \int_0^\pi f(x) \sin (n + \frac{1}{2})x \mathrm{d}x = 0,\lim_{n \rightarrow \infty} \int_0^\pi f(x) \cos (n + \frac{1}{2})x \mathrm{d}x = 0$

* 傅立叶级数逐点收敛性质: 
  * 积分表达式: $S_n(x) = \frac{1}{\pi} \int_{-\pi}^{\pi} f(x+t) \frac{\sin(n+\frac{1}{2})t}{2\sin\frac{t}{2}}\mathrm{d}t$    
  Dirichlet核: $$D_n(x) = \left\{ \begin{array}{ll} \frac{\sin(n+\frac{1}{2})x}{2 \sin \frac{x}{2}} && x \neq 2 k \pi \\ n + \frac{1}{2} && x = 2 k \pi \end{array} \right.$$
  * 逐点收敛定理: $f$在周期区间$[-\pi, \pi]$上分段光滑, 则
  $$\frac{f(x^+) + f(x^-)}{2} = \frac{a_0}{2} + \sum^\infty_{n=1}(a_n \cos n x+b_n \sin n x)$$
  * 逐项积分定理, 即积分可以换序. 
  * 逐点可导定理, 即$\frac{f'(x^+)+f'(x^-)}{2} = \sum_{n=1}^\infty (n b_n \cos n x + n a_n \sin n x)$
  * 按Cesaro和收敛: $\frac{f(x^+)+f(x^-)}{2} = \lim_{n \rightarrow \infty} \frac{1}{n} \sum_{k=1}^{n-1} S_k(x)$

> 正交函数系的证明
> * 对于那些多阶导数的做法, 一般采用分部积分:    
> 
>   $$\int_{-1}^{1} L_n(x)L_m(x) \mathrm{d}x = 0, n \neq m$$
> 
>   其中$L_n = \frac{1}{2^n n!}\frac{\mathrm{d}^n}{\mathrm{d} x^n}[(x^2 - 1)^n]$, 
>   对于这个问题, 可以将

### 多元函数平面点集
* $B_r(\boldsymbol{a}) = \{(x_1, x_2) \in \mathbb{R}^2: \vert\vert \boldsymbol{x} - \boldsymbol{a} \vert\vert \leq r\}$
* $[a, b] \times [c, d] = \{(x_1, x_2) \in \mathbb{R}^2: a \leq x_1 \leq b, c \leq x_2 \leq d\}$
* 领域(方形或者圆形), 空心领域
* 补集: $E^C$
* 内点: $\exist \boldsymbol{a}: U(\boldsymbol{a}) \subset E$    
  内点的集合: $int(E)$或者$E^0$
* 外点: $\exist \boldsymbol{a}: U(\boldsymbol{a}) \subset E^C$
* 边界点: $\forall \boldsymbol{a}: U(\boldsymbol{a}) \cap E \neq \varnothing$
* 边界: $\partial E$
* 聚点: $\forall U^0(\boldsymbol{a}), U^0(\boldsymbol{a}) \cap E \neq \varnothing$    
  聚点的集合: $E^d$
* 孤立点: $\boldsymbol{a} \in E \wedge \exist U^0(\boldsymbol{a}), U^0(\boldsymbol{a}) \cap E = \varnothing$
* 开集: $E = int(E)$
* 闭集: $E^C$为开集, 即闭集与开集互为补集
* 连通: $E = A \cup B, A \cap B = \varnothing, A, B \neq \varnothing, A \cap B^d \neq \varnothing \vee A^d \cap B \neq \varnothing$
* 路径联通: 可以用一条路径来连接任意两点

> (区域)连通$\Rightarrow$路径连通, 路径连通$\nRightarrow$连通

* 开域: 非空连通开集
* 闭域: 开域与其边界点的并$E \cup \partial E$
* 闭包: $\bar{E} = E \cup E^d$
* 集合直径
* 集合之间的距离

> 平面点集的关系: 
> * $\overline{A \cup B} = \overline{A} \cup \overline{B}$    
>   $\overline{A \cap B} \subset \overline{A} \cap \overline{B}$    
>   这种代入定义分类证明: 
>   1. $x \in A \cap B$: $x \in A \wedge x \in B \Rightarrow x \in A \cap B$
>   2. $x \in (A \cap B)^d$: $\exist \{x_n \rightarrow x, x_n \in A \cap B\}$
> 
>   类似的还有$(A \cap B)^0 = A^0 \cap B^0$:    
>   $\forall x \in (A \cap B)^0 \Leftrightarrow \forall x \in A^0 \cap x \in B^0$
> * 


### 二元函数
#### 极限前置信息

$$\boldsymbol{a} \rightarrow \boldsymbol{a}_0 \Leftrightarrow x_n \rightarrow x_0, y_n \rightarrow y_0$$

* 柯西收敛准则: $\vert\vert \boldsymbol{a}_n - \boldsymbol{a}_m \vert\vert < \varepsilon$
* 闭区间套定理: 闭集合列$\{D_n\}$满足: 
  * $D_n \supset D_{n+1}$
  * $d(D_n) \rightarrow 0$
  
  则存在唯一的点$\boldsymbol{a}\in\cap_{n=1}^\infty D_n$
* 聚点定理: 有界无限点集$E\subset\mathbb{R}^2$中至少有一个聚点
* 紧致性定理: 有界无穷点列$\{\boldsymbol{a}_n\}\subset\mathbb{R}^2$一定存在收敛子列
* 有限覆盖定理: 开集族$\{\Delta_\alpha\}, D \subset \cup \Delta_\alpha$, 
  则存在有限开集覆盖$D$, 即$D \subset \Delta_1 \cup \cdots \cup \Delta_n$

#### 二元函数极限
* 重极限: 
  $$f(\boldsymbol{x}) \rightarrow A$$
  :聚点 
  $$\boldsymbol{x}_0 \in D$$
  , 
  $$\forall \varepsilon > 0, \exist \delta(\boldsymbol{x}_0, \varepsilon) > 0, s.t. \forall \boldsymbol{x} \in U_\delta^0(\boldsymbol{x}_0) \cap D, \vert f(\boldsymbol{x}) - A\vert < \varepsilon$$
  * 归结原则: 
    $$\lim_{\boldsymbol{x} \rightarrow \boldsymbol{x}_0} f(\boldsymbol{x}) = A \Leftrightarrow$$
    1. $\forall E \subset D$, 对聚点
    $$\boldsymbol{x}_0$$
    有
    $$\lim_{\boldsymbol{x}\rightarrow\boldsymbol{x}_0} f(\boldsymbol{x}) = A$$    
    即区域大小和极限无关
    1. $\forall \{\boldsymbol{x}_n\} \subset D \setminus\{\boldsymbol{x}_0\}$满足$\{f(\boldsymbol{x}_n)\} \rightarrow A$    
    任意子列收敛
* 累次极限: 按$x, y$顺序先后取极限    
  $\lim_{x \rightarrow x_0}\lim_{y \rightarrow y_0} f(x, y) = A$    
  可以存在, 或者不存在, 或者存在但是两个不相等
  * 累次极限和重极限的存在性没有必然联系
  * 若存在, 则于重极限相等

> 二元函数的极限的计算: (重极限)
> * 对于$(x, y) \rightarrow (0, 0)$的情况, 适合$y = m x$, 
>   然后将问题转化为一元函数$f(x, m x)$的极限问题
> * 对于齐次的$f(x, y)$, 或者能够化简成齐次的函数, 想办法化简成齐次的函数.     
>   比如: $\lim_{(x, y) \rightarrow(0, 0)}{\frac{x^2 y}{x^4 + y^2}}$
> * 善于使用放缩:    
>   $$\lim_{(x,y) \rightarrow (+ \infty, +\infty)} (\frac{x y}{2x^2 + 3 y^2})^{x^2} \\ 2 x^2 + 3 y^2 \geq 2\sqrt{6} x y \Rightarrow \lim_{x \rightarrow +\infty} (\frac{1}{2\sqrt{6}})^{x^2} \rightarrow 0$$

#### 连续
* **定义**: $\forall \varepsilon > 0, \exist \delta > 0, s.t. \forall x \in D \cap U_\delta(\boldsymbol{x}_0)\ \vert f(\boldsymbol{x}) - f(\boldsymbol{x}) \vert < \varepsilon$
* 可去间断点
* 连续的充要条件: 
  * 全增量趋于零
* 有界闭集连续函数性质: 
  * 有界性
  * 有最大值和最小值
  * 一致连续
  * 介质定理

#### 微分
* 偏导数
  * 偏导数存在不代表连续    
    比如
    $$f(x, y) = \left\{\begin{array}{ll}\frac{x y}{x^2 + y^2} & x^2 + y^2 \neq 0 \\ 0 & x^2 + y^2 = 0\end{array}\right.$$
  * 连续不一定有偏导数
* 法向量和切平面
* 方向导数$\frac{\partial f}{\partial \boldsymbol{s}} = (\nabla f) \cdot \frac{\boldsymbol{s}}{s}$

## 题目
> (一致)连续函数列的一致收敛极限仍为(一致)连续函数

* 连续函数的版本: 
  $$f_n \rightrightarrows f: \vert f(x) - f(y) \vert \leq \vert f(x) - f_n(x) \vert + \vert f_n(x) - f_n(y) \vert + \vert f_n(y) - f(y) \vert \rightarrow 0$$
* 一致连续函数列的版本: 
  $$\vert f_n(x) - f_n(y) \vert$$
  只与$\vert x-y \vert$有关

> $f_{n+1}(x) = \int_a^x f_n(t) \mathrm{d}t, f_n \rightrightarrows 0$

(遇到有递推式的积分, 可以)利用绝对值不等式来放缩: $\vert f_{n+1}(x) \vert = \int_a^x f_n(t) \mathrm{d}t \leq \int_a^x \vert f_n(t) \vert \mathrm{d}t \leq M_n (x - a)$, 其中$M_n$为这一段的函数最大值(积分中值定理的放缩). 

> $$\sum \frac{x + (-1)^n n}{x^2 + n^2} = \frac{x}{x^2 + n^2} + \frac{(-1)^n n}{x^2 + n^2}$$

通过将原来的东西拆成两部分来分别判断, 前面的部分比较好判断, 后面的部分$\frac{n}{x^2 + n^2}$关于$n$单调减少, 然后$(-1)^n$有界. 用Dirichlet判断. 

> $$\vert u_n(x_1) + u_n(x_2) \vert \leq L \vert x_1 - x_2 \vert \Rightarrow \sum \frac{u_n}{2^n} 连续可导$$

证明导数的极限逐点收敛, 最后一致收敛. 

> 利用积分和导数的求和方法: 
> * $\sum n(n+1) x^n$, 用积分来做问题, 不过要先除掉一个$x$
> * $\sum n^3 x^n$, 最笨的方法就是反复变换, 反复积分    
>   巧妙一点的方法就是$n^3 = (n+1)(n+2)(n+3) + \alpha(n+1)(n+2) + \beta(n+1) + \gamma$, 拆开之后再分开求, 会简单一点点. 

> 

> $\overline{A} = \{p \in \mathbb{R}^2 : \rho (p, A) = 0\}$

对这种的证明就利用定义和分类来做, 对于$x \in A$是显然的, 然后对于$x \in A^d$的只需要$\{x_n \in A\}\rightarrow x$即可. 

> $\forall p, q, \vert \rho(p, A) - \rho(q, A) \vert \leq \rho (p, q)$

这个不可以直接说$\vert \rho(p, x)_{= \rho(p, A)} - \rho(q, x) \vert \leq \rho (p, q)$, 因为$A$是否是有界闭集会比较麻烦. 

> $$\int_0^T f(x) \mathrm{d}x = 0 \Rightarrow \int_0^T \vert f(x) \vert^2 \mathrm{d} x (\frac{T}{2\pi})^2 \int_0^T |f'(x)|^2 \mathrm{d}x$$
> 
> $f$连续分段光滑

于是把$f$展开成$\sum a_n \cos n x + b_n \sin n x$一样的东西, 然后展开有: 

$$\int f^2 \leftrightarrow \sum a_n^2 + b_n^2 \\ \int (f')^2 \leftrightarrow \sum n^2 a_n^2 + n^2 b_n^2 \\ \Rightarrow n > 1: 成立$$

## 歪理
记录一些自己的理解, 不一定正确. 

* 函数的一致收敛就是为了证明$\lim_{x \rightarrow x_0} \lim_{n \rightarrow \infty} f_n(x) = \lim_{n \rightarrow \infty} \lim_{x \rightarrow x_0} f_n(x)$, 也就是可以交换. 

## 后记
> 这个妹妹我是见过的. 
> 
> 曹雪芹 《红楼梦》

但是有什么用呢? 