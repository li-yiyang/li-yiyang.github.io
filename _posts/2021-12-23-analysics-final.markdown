---
layout: post
title:  "Analysics First Semester The End"
date:   2022-01-04 12:14:03 +0800
categories: jekyll update
math: true
---
# 微积分期末复习
就是一个慌. 

## 问题驱动的复习
下半学期主要在学导数和积分. (才怪呢! 实际上是可以导数的条件和可以积分的条件), 
嘛, 要是真的按照教的来复习的话, 我觉得时间可能有些尴尬. 所以我现在就很功利地, 
先针对老师的期末练习里面出现的题目类型来进行一个习的复. 

### 可积分, 可微分的条件
* 不定积分 - 微分的逆运算
  * *连续函数*一定有原函数     
    > 连续函数:        
    > $$\lim_{x \rightarrow x_0} f(x) = f(x_0) \\ \Leftrightarrow \lim_{x \rightarrow x_0^-} f(x) = f(x_0) 左连续 \And \lim_{x \rightarrow x_0^-} f(x) = f(x_0) 右连续 \\ \Leftrightarrow \forall \varepsilon > 0, \exists \delta (x_0, \varepsilon), x \in U(x_0, \delta), f(x) < f(x_0) + \varepsilon 上半连续 \\ \And \forall \varepsilon > 0, \exists \delta (x_0, \varepsilon), x \in U(x_0, \delta), f(x) < f(x_0) - \varepsilon 下半连续 $$   
    > 说明$f$在$x_0$点连续.       
    > 一致连续函数: $$\forall \varepsilon > 0, \exists \delta, \forall |x_1 - x_2| < \delta, |f(x_1) - f(x_2)| < \varepsilon$$
  * *第二类间断点*的函数 **可能** 有原函数
  * *第一类间断点*的函数 **没有** 原函数
    > 间断点及其分类: 
    > * 第一类间断点: 左右极限都存在
    >   * 可去间断点: 左右极限相等, 不等于该点函数值
    >   * 跳跃间断点: 左右极限不相等
    > * 第二类间断点: 左右极限至少一个不存在
    >   * 无穷间断点: 左右极限在$\overline{\mathbb{R}}$中存在, 有一个是无穷大
    >   * 震荡间断点: 想想这个, $\sin(\frac{1}{x})$, 极限至少一个不存在
* 定积分
  * 可积的第一充分必要条件: $f \in R([a, b]) \Leftrightarrow S = s$     
    (其中$S$是达布上和, $s$为达布下和)
    > 达布和: 取任意剖分, 然后累计$\sup$(上和)或$\inf$(下和)
  * 可积的第二充分必要条件: $f \in R([a, b]) \Leftrightarrow \forall \varepsilon > 0, \exists \mathbb{T}, S(\mathbb{T}) - s(\mathbb{T}) < \varepsilon$      
  * 可积的第三充分必要条件: $f \in R([a, b]) \Leftrightarrow \forall \varepsilon > 0, \eta > 0, \exists \mathbb{T}$, 
    属于$\mathbb{T}$中的所有小区间对应的增幅$\omega_k \geq \varepsilon$
    的总长度
    $$\sum_{\omega_k \geq \varepsilon} \Delta x_k < \eta$$
  * Lebesque 定理: $f \in R([a, b]) \Leftrightarrow f$定义域内有界, 
    并且不连续点集合是*零测集*. 
    > 零测集: 就是$$\sum^{\infty}\|I_n\|\leq \varepsilon$$
  * 可积函数类
    * $f \in C([a, b])$
    * $f$有界并且只有有限个间断点
    * $f$在$[a,b]$上单调
* 微分
  * $f(x)$在$x_0$可导 $$\Leftrightarrow f'_{+}(x) = f'_{-}(x)$$

### 应用
* 泰勒展开    
  $$f(x) = \sum_{k=0}^{n} \frac{f^{(n)}(x_0)}{k!}(x-x_0)^k + o((x-x_0)^n)$$
  * 麦克劳林多项式: 就是$x_0 = 0$的在$0$点附近的展开
  * 佩亚诺型余项: $o((x-x_0)^n)$
  * 拉格朗日型余项: $o((x-x_0)^n) = \frac{f^{(n+1)}(\xi)}{(n+1)!}(x-x_0)^{n+1}, \xi \in (x, x_0) \mathrm{or} (x_0, x)$    
    在近似计算的时候就用这样的展开方式, 让拉格朗日余项小于误差即可. 
  * 积分余项: $$o((x-x_0)^n) = \frac{1}{n!} \int_{x_0}^{x} (x-t)^n f^{(n+1)}(t) \mathrm{d} t$$
* 牛顿法近似计算: 
  $$x_{n} = x_{n-1} - \frac{f(x_{n-1})}{f'(x_{n-1})} \\ \Delta \leq \frac{|f(x_{n-1})|}{\min\{f'(x)\}}$$
* 中值定理
  * $$\exists f'(a) f'(b) < 0 \Rightarrow \exists f(\xi) = 0, \xi \in (a, b)$$
* 介值定理
* 洛必达: 适用于$\frac{\cdot}{\infty}$型, 和$\frac{0}{0}$型. 
* 函数的性质
  * 极值点: 一阶导数为零的点, 并且两侧的导数异号
  * 驻点: 一阶导数为零
  * 拐点: 二阶导数为零
    * 下凸函数定义: $f(\lambda x_1 + (1 - \lambda) x_2) $
  * 渐进性: 向$x$, $y$轴或者$y = a x + b$渐进
  * 最值: 通过单调性来判断, 然后还要考虑边界点和不连续点

### 计算
* 微分
* 积分
  * 不定积分
    * 递推公式
  * 定积分
  * 变量替换
    * 有理分式
  * 分部积分

## 题目
### 连续性
> $$f(x) \in C, g(x) = \lim_{t \rightarrow x} f(x) \Rightarrow g(x) \in C$$

一道利用定义证明的题目, 只需要检验$\lim_{x \rightarrow x_0} g(x) = g(x_0)$即可

> $$D(x) = \left\{ \begin{array}{ll} 1 & x \in \mathbb{Q} \\ 0 & x \in \mathbb{R} \backslash \mathbb{Q} \end{array} \right., x D(x)只在x = 0连续$$

在$x=0$点的连续性利用定义就可以很好证明了, 在$x \neq 0$处, 需要想一种方法, 
只需要构造$\varepsilon = \frac{1}{2}$即可, 这样的话, 
就会产生在有理数点和无理数点取到的函数值的差大于$\varepsilon$. 
(还利用了*有理数在实数中稠*)

> ($\mathbb{R}$上)连续函数有最大(小)值

假如没有的话, 就可以构造子列, 并且这个子列就会一直大下去, 最后和连续函数矛盾. 

> $$f \in C([0, 1]), f(0) = f(1), \exists x \in [0, \frac{1}{2}], s.t. f(x) = f(x + \frac{1}{2})$$

类似这样的题目就是要构造一个$g(x) = f(x) - f(x + \frac{1}{2})$的函数, 
然后证明在$[0, \frac{1}{2}]$上有零点. 零点的证明就是用中值定理来试试. 

这里的题目就可以利用: $g(0) + f(\frac{1}{2}) = f(0) - f(\frac{1}{2}) + f(\frac{1}{2}) - f(1) = 0 \Rightarrow \exists x$

> $f \in C([0, + \infty))$一致连续, $\forall x \in [0, 1), \lim_{n \rightarrow \infty} f(x + n) = 0 \Rightarrow lim_{x \rightarrow} f(x) = 0$

这道题要注意, 极限不可以直接就和在一起, 
因为每一个的极限所对应的$N$都是不一样的. 这个很要命, 并且还没法历遍, 
因为在$[0, 1)$中的数是不可数的. 

所以就是通过(连续函数的性质)将$[0, 1)$分割为$\delta$长度的小区间, 
在每个小区间中选择一个代表元, 对每个区间的代表元的$N$历遍, 
然后对于同一个区间的数, 它们就和代表元之间的差是可以忽略的, 于是证明完毕. 

其他的一些技巧: 
* $$\max\{f, g\} = \frac{f+g}{2} + \frac{|f+g|}{2}$$
* $$\min\{f, g\} = \frac{f+g}{2} - \frac{|f-g|}{2}$$
* $$|\sqrt[3]{y} - \sqrt[3]{x}| \leq |\sqrt[3]{y-x}|$$


一些注意的点: 
* 证明在一个区间上连续的时候不要只考虑几个点, 
  因为很有可能会有焦点无穷密集的时候, 就会让用定义证明点连续很困难. 
* $\mathbb{R}$上连续周期函数一定一致连续, 证明的时候考虑$[-T, T]$上的一致连续, 
  防止出现类似于$x_1 = 0 + \varepsilon_1, x_2 = T - \varepsilon_2$
* 一致连续函数对加减法封闭, 对乘除法不封闭     
  $x \times x$不连续, 因为取$x_n = \sqrt{n + 1}, y_n = \sqrt{n} \Rightarrow x_n^2 - y_n^2 \equiv 1 > \varepsilon$     
  (类似的可以证明不一致连续)
* 

### 微分
> $f$在$x=0$可导, 
> $$a_n \rightarrow 0^-, b_n \rightarrow 0^+ \Rightarrow \lim_{n \rightarrow \infty} \frac{f(b_n) - f(a_n)}{b_n - a_n} = f'(0)$$

要证明相等, 可以利用$|a - b| < \varepsilon$的思路, 构造
$$|(\frac{f(b_n)-f(0)}{b_n} - f'(0))\frac{b_n}{b_n - a_n} - (\frac{f(a_n)-f(0)}{a_n} - f'(0))\frac{a_n}{b_n - a_n}|$$
然后利用三角不等式, 最后证明在极限的情况下为零就可. 

> $$(f g)^{(n)} = \sum_{i=0}^{n} (\begin{array}{l} i \\ n\end{array}) f^{(i)} g^{(n-i)} \\ \Rightarrow y = \arctan (x), y' = \frac{1}{1+x^2} \\ (1+x^2) y^{(n+1)} + 2 n x y^{(n)} + n (n-1) y^{(n-1)} = 0$$

函数乘积的求导方法. 

> $f(x+y) = f(x) + f(y)$

利用构造的方式, (虽然个人感觉思路有点像是线性代数), 只要利用零元
$f(0) = f(0) + f(0)$, $f(n) = n f(n)$, 
$f(n \times \frac{1}{n}) =  n f(\frac{1}{n}) \Rightarrow f(\frac{1}{n}) = \frac{1}{n} f(1)$, 然后就可以构造得到$\forall x \in \mathbb{Q}, f(x) = f(1) x$

> $f(b) = f(a) + \frac{1}{2}(b - a)[f'(a) + f'(b)] - \frac{1}{12}(b - a)^3 f'''(\xi)$

对于类似的长得就像是中值定理的题, 往往通过变换将含有$\xi$的项放一边, 
不含有的则放在另一边. 

一般来说会得到两种: 
$$\left\{\begin{array}{l} f^{(n)}(\xi) = F(x) \\ f^{(n)}(\xi) = \frac{F(x)}{G(x)} \end{array} \right.$$

前面那种就去找零点, 然后就可以得到
$F(a) = F(b) = 0 \Rightarrow \exists \xi \in (a, b), F'(\xi) = 0$

然后后面那种就找一个零点, 然后
$\frac{F(x)}{G(x)} = \frac{F(x) - F(a)}{G(x) - G(a)} = \frac{F'(x)}{G'(x)}$

或者也有另外一种想法, 就是通过待定系数法, 让含有$\xi$的项成为一个系数, 
然后反解得到结果. 

或者也可以利用拉格朗日余项展开的方式. 想法就是对于一个区间, 
中点的函数值关于端点分别展开, 可以得到好的结论. 

> $f$二阶可导, 有界, $\exists f''(\xi) = 0$

只要说明不能恒$>0$或$<0$, 即可. 正难则反. 

有一个推论: 有界的凸函数一定是常函数. 

> $f(x)$在$(a, b)$上可导, 然后$f'(x)$单调增加$\Rightarrow$连续函数

可导函数的性质: 可导$\Rightarrow$连续

> $f \equiv C, f(a) = f(c) = f(b), a < c < b \Rightarrow \exists x, f''(x) < 0$

还是反证法, 假如$f''(x) \geq 0$, 即上凸函数, 然后就画一条连接$a,b$的直线, 
然后就会产生矛盾. 

> $\lim_{x \rightarrow 0} \frac{1}{x^2} - \frac{1}{\sin^2 x} = \frac{1}{3}$

不妨直接通风, 然后就直接利用泰勒展开就好了. 
(或者也可以直接用Stokz公式)

> $\frac{f(a+h)+f(a-h)-2f(a)}{h^2} = \frac{f''(a+\theta h) + f''(a - \theta h)}{2}$

思想就是一个待定系数法的想法, 就是$A = \frac{f(a+h)+f(a-h)-2f(a)}{h^2}$, 
然后就可以得到$F(x) = f(a + x) + f(a - x) - 2f(a) - A x^2$. 
然后就有$F(0) = F(h) = 0$就可以得到了. 

> $1^{\infty}$ 类型的极限, 取对数然后尝试洛必达. 

> $$|f''| \leq M \Rrightarrow |f'(0)| + |f'(a)| \leq M a$$

思路就是可以证明$f'$有最大值. 

> $f(x) = \left( \frac{a_1^x + a_2^x + \cdots + a_n^x}{n} \right)^{\frac{1}{x}}$ 幂平均不等式
> * $\lim_{x \rightarrow 0} f(x) = \sqrt{n}{a_1 \cdots a_n}$
> * $\lim_{x \rightarrow \infty} f(x) = max\{a_1, \cdots, a_n\}$
> * $f$在$\mathbb{R}$上递增

注意的点: 
* 取极限的时候, 只有乘除的时候可以利用替换的方法, 不能在加减的时候替换. 
  比如分子$\tan(x) - \sin(x)$不可以直接小量代了, 建议**展开**或者**洛必达**. 
* 凸函数未必都有二阶导数, 所以想用二阶导数来验证凹凸性的时候就要很小心, 
  因为很有可能会遇到很尴尬的事情. 一般可以利用凹凸性的定义来验证. 
* 拉格朗日中值定理不可以推极限, 因为拉格朗日中值定理的存在$f'(\xi)$的存在, 
  不代表一定连续, 所以取极限的时候不一定极限存在. 

### 积分
> $\int \tan^{n \geq 2} x \mathrm{d}x = \int \tan^{n - 2} x \frac{1 - \cos^2 x}{\cos^2 x} \mathrm{d} x = \int \tan^{n-2} \mathrm{d} \tan x - \int \tan^{n-2} x \mathrm{d} x$

> $\int \frac{\sqrt{n}{1 + \sqrt{4}{x}}}{\sqrt{x}} \mathrm{d} x$

这里的想法就是换元积分, 思路就是如何把根号消除: 
$t = \sqrt{3}{1+\sqrt{4}{x}} \Rightarrow \int 12 t^3 (t^3 - 1) \mathrm{d} t$. 
面对这种有很复杂的东西(根号)的时候, 往往会想把最复杂的东西给换元了. 

> 对欧式: $A = \int e^{a x} \sin b x \mathrm{d} x, B = \int e^{b x} \sin a x \mathrm{d} x$

就是一个二元递推式, 然后解线性递推方程. 

> 欧拉变换: 
> $$\sqrt{a x^2 + b x + c} = \left\{ \begin{array}{l} t - \sqrt{a x} \\ t x - \sqrt{c} \end{array} \right.$$
> 这样换元有时候可以很妙

> 柯西不等式: $\int_a^b (f(x) - t g(x))^2 \mathrm{d} x \geq 0 \Rightarrow \Delta_t = 4(\int_a^b f^2 \mathrm{d} x \int_a^b g^2 \mathrm{d} x - \int_a^b f g \mathrm{d} x) \leq 0 \Rightarrow \int_a^b f^2 \mathrm{d} x \int_a^b g^2 \mathrm{d} x \leq \int_a^b f g \mathrm{d} x)$     

这个证明的思想就是利用二次函数的判别式的方法来证明, 这个思想很棒, 
用了一个和$x$完全无关的$t$, 然后有一个结论. 

> 柯西不等式的应用: $\int_0^{\pi} x a^{\sin x}\mathrm{d} x \int_0^{\frac{\pi}{2}} x a^{- \cos x}\mathrm{d} x = \int_{\pi}^{0} (\pi - t) a^{\sin t}\mathrm{d} t \int_0^{\frac{\pi}{2}} x a^{\sin x}\mathrm{d} x = \pi \int_0^{\frac{\pi}{2}} a^{\cos t} \mathrm{d} t \int_0^{\frac{\pi}{2}} a^{- \cos x} \mathrm{d} x \geq \pi (\int_0^{\frac{\pi}{2}} 1 \mathrm{d} x)^2$

> 积分中的洛必达法则: 
> $$\lim_{x \rightarrow \infty} \frac{\int_0^x f(t)\mathrm{d}t}{x} = \lim_{x \rightarrow \infty} \frac{f(x)}{1} = f(\infty) = A$$

其实用到了一个变上限积分的思路

(用定义来证明也不是不行, 但是还是这个爽啊. )

> 分部积分: $I_{m,n} = \int_0^{\frac{\pi}{2}} \sin^m x \cos^n x \mathrm{d} x = \int_0^{\frac{\pi}{2}} \sin^m x (\cos^{n-1} x \mathrm{d} x)= - m I_{m,n} + (n -1) I_{m,n-2} \Rightarrow I(m, n) = \frac{m - 1}{m + n} I_{m - 2, n}$    
> 并且$I(0, n) = \frac{(n - 1)!!}{(n)!!} \frac{\pi}{2}$

注: 
* 有些时候, 试试看用定义(达布和或者说是黎曼和之类的)也会让计算简单

### 其他一些奇奇怪怪题
> $$\int_0^{\frac{\pi}{2}} \frac{1}{1 + \tan^2 x} \mathrm{d} x$$

对于这种带三角函数的$0$到$\pi$的积分的函数, 往往可以尝试反过来积分, 
就是从$\pi$到$0$积分, 然后构造一个变换的元$u = \frac{\pi}{2} - x$: 
$$= \int_{\frac{\pi}{2}}^{0} - \frac{\mathrm{d} u}{1 + \cot^3 u} \\ = \int^{\frac{\pi}{2}}_{0} \frac{\tan^3 u \mathrm{d} u}{1 + \tan^3 u} \\ = 1 - \int_0^{\frac{\pi}{2}} \frac{1}{1 + \tan^2 x} \mathrm{d} x = \frac{1}{2}$$

类似的方法还有: 

> $$\int_0^1 x (1 - x)^6 \mathrm{d} x$$

也是改变积分的方向, 从$1$积到$0$, 就好了$t = 1 - x$: 
$$- \int_1^0 (1-t) t^6 \mathrm{d} t$$

> $f(x)$在$[0, 1]$上下凸, 证明$e^{f(x)}$下凸. 

这个的证明不可以直接用二阶导数, 因为没有说是否二阶可导, 假如二阶可导, 
那简直就是简单到笑掉大牙了. 只能从定义出发证明. 记$F(x) = e^{f(x)}$, 则: 
$$F(\lambda x_1 + (1 - \lambda) x_2) < \lambda F(x_1) + (1 - \lambda) F(x_2)\\ \Leftarrow e^{f(\lambda x_1 + (1 - \lambda) x_2)} \leq e^{\lambda f(x_1) + (1 - \lambda) f(x_2)} \leq \lambda e^{f(x_1)} (1 - \lambda) e^{f(x_2)}$$

> $\mathbb{R}$上非常值的连续周期函数一定有最小周期

反证法, 假如没有最小的周期, 构造一个$|x_1 - x_2| < \delta \Rightarrow |f(x_1) - f(x_2)| < \varepsilon$, 假如周期比$\delta$小就会和非常值矛盾. 

> $$f可导, f(0) = 0, f(x) > 0, \exists \xi \in (0, 1), \frac{f'(\xi)}{3f(\xi)} = \frac{f'(1 - \xi)}{1 - \xi}$$

构造函数就好了, 这种有系数的话, 就可以去想想构造一个幂函数. 
$$f(\xi)f^3(1 - \xi)$$

> $$f \in C^3(\mathbb{R}), f'(0) = 1, f''(0) = 0, f'''(0) < 0, a_1 = 1, a_{n+1} = f(a_n), \lim_{n \rightarrow \infty} a_n = 0 = a, \lim_{n\rightarrow\infty} = ?$$

利用泰勒展开: 
$$f(a_n) = f(0) + f'(0) a_n + \frac{1}{2} f''(0) a_n^2 + \frac{1}{6} f'''(0) a_n^3 + o(a_n^3) = a_n + \frac{1}{6} f'''(0) a_n^3 + o(a_n^3) \Rightarrow \frac{1}{a_{n+1}^2} - \frac{1}{a_n^2} = \frac{a_n^2 - a_{n+1}^2}{a_n^2 a_{n + 1}^2} = \frac{a_n^2 - (f(a_n))^2}{a_n^4} = \frac{a_n^2 - (a_n + \frac{1}{6} f'''(0) a_n^3 + o(a_n^3))^2}{a_n^4} \Rightarrow = - \frac{1}{3}f'''(0)$$

## 后记
没救了, 题目的题型分布和格式确实和期末练习一样, 但是就是感觉有点不对味. 

泪目... 