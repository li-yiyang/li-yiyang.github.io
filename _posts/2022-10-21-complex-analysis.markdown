---
layout: post
title:  "Complex Analysis"
date: 2022-10-22 11:38:27 +0800
math: true
categories: notes
---
# Complex Analysis
> 复变函数学十遍... 

虽然仔细想了想, 发现虽然东西不多, 但是都不好算啊... 
并且作为一个称职的菜狗, 还把基本上所有的证明都略过了. 
(虽然一般的结论和数学分析里面的都差不多(目前). )

## 复数
* 四则运算, 求模之类的, 反正计算的时候选一个好一点的坐标系会比较方便, 
  比如遇到指数相关的大多数是极坐标, 遇到实部虚部的时候大多数是$x-y$
* 共轭: $z = x + iy \rightarrow z^* = x - i y$  
  其中共轭是对合运算: $$(z^*)^* = z, \mathrm{Re} z = \frac{z + z^*}{2}, \mathrm{Im} z = \frac{z - z^*}{2i}$$, 对四则运算保持运算. 
* 三角不等式: $\|z_1 + z_2\| \leq \|z_1\| + \|z_2\|$
* $ρ e^θ$
  
  > 利用上面的方法可以用来计算三角函数的和:  
  > $S_n(\theta) = \sum_{i = 1}^{n} \sin i \theta$  
  > 用欧拉公式, $\sin k\theta = \frac{e^{ikθ} - e^{-ik\theta}}{2i}$ 
  > 将求和变成对等比级数的求和. 
  
  > 用来计算 $\theta = \sum \arctan{\frac{n_i}{d_i}}$:  
  > 设 $z_i = d_i + i n_i$, 于是就有 $\theta = \mathrm{arg}(\prod z_i)$.  

* 康托完备性: Cauchy 数列均收敛于 $\mathbb{C}$, 复数域康托完备
* 代数学基本定理: 复数多项式一定有根
  * 一般的操作就是 $R(z) = \prod_i(z^{\alpha_i} - c_i e^{i\theta_i})$, 
	然后就能够解: $z = \sqrt[\alpha_i]{c_i} e^{i \frac{\theta_i + 2 \lambda \pi}{k_i}}$. 
  * 或者是用来将多项式化简成一次因式的基, 即计算出 $z_i$, 然后得到结果. 
* 欧拉公式: $\cos\theta + i \sin\theta = e^{i\theta}$  
  于是可以有倍角公式: $\sin n\theta = \mathrm{Im} (e^{i\theta})^n = \mathrm{Im} (\cos\theta + i \sin\theta)^n$. 于是就有了倍角公式了. 
* 复数几何: 平移$\pm z$, 伸缩$\lambda$, 旋转: $e^{i\theta}$
* 球极投影  
  (虽然不一定有用, 但是可以记录一下: 基本的做法就是一个三角形的相似而已. )
* 多值性: 割线和黎曼面  
  分支点阶数: $k$ 阶分支点 (绕 $k + 1$ 圈回到原来的值), $\infty$ 阶分支点. 

## 函数
* $\mathrm{sinh} = \frac{e^z - e^{-z}}{2}, \mathrm{cosh} = \frac{e^z + e^{-z}}{2}$  
  (注: 和三角函数不一样, 里面没有 $i$ 的出现. )
* 类似于 $\mathrm{ln}z = \mathrm{ln} |z| + i \mathrm{Arg}z$ 
  这样的的函数有多值性 
* 对于函数极限与趋近极限的路径无关. 
* 解析函数 (全纯函数): 在 $E$ 上解析的函数, 若 $E = \mathbb{C}$, 
  则为整函数. 
* 调和函数: $ΔH=0$.  
  C-R 条件 $⇒$ 解析函数的实部或虚部为调和函数
  

### 集合
平面点集的定义. 

### 导数

$$\lim_{\Delta z \rightarrow 0} \frac{f(z+Δz) - f(z)}{Δz} = \frac{\mathrm{d} f(z)}{\mathrm{d} z}$$

* C-R 条件: $\frac{∂u}{∂x} = \frac{∂v}{∂y}, \frac{∂u}{∂y} = -\frac{∂v}{∂x}$  
  若 C-R 条件不满足, 那么就不可求导. (虽然满足也不一定可导就是了, 
  一般来说, 证明不可导就用一个反例来就好了. )  
  (注: 一个简单的, 但估计不太严谨的记忆方法就是 $(u,v)$, $(x,y)$, 
  $(ρ, \varphi)$, 看作是按顺序来的一对, 顺序相同的同号, 反的就是异号. )

### 积分
一般的积分和数学分析里面的差不多. 

* 柯西公式: 
  $f(\alpha) = \frac{1}{2πi} \oint_l \frac{f(z)}{z-α} \mathrm{d}z$  
  (证明的思路类似构造一个非常贴近 $\alpha$ 点的小圆和外面的大圆. )
  * 导数: 
	$f^{(n)}(z) = \frac{n!}{2\pi i} \oint_l \frac{f(\xi)}{(\xi - z)^{n+1}}\mathrm{d}\xi$
  * Morera 定理: 
	$\forall l \subset E, \oint_l f(z) \mathrm{d}z = 0 ⇒ f(z)$解析 
  * Liouville 定理: 
	$f(z)$ 为整函数, 并且有界, 则常数.  
	(证明思路: 构造 $\xi \rightarrow \infty$ 的积分路径 $l$. )

## 极限, 级数
极限: $∀ε > 0, ∃N, n > N ⇒ \|z_n - z\| < ε$, 
可以看作是实部合虚部的分别的极限. 

**级数 (和) 的收敛性判断**: 
* Cauchy

  $$n > N ⇒ |\sum_{k = n + 1}^{n + p} z_k | < ε$$

  原形就是 $z_m - z_n < ε$ 的一个变形. 
* Leibniz

  $$a_k ↓ \rightarrow 0 ⇒ \sum_{k=1}^∞(-1)^{k-1}a_k \rightarrow S$$

  其实就是 Dirichlet 的一种形式
* Dirichlet

  $$\{a_k\} 单调 \rightarrow 0, \mathrm{sup}\{b_k\} \leq M ⇒ \sum_{k=1}^∞a_kb_k$$
  
**一些结果**: 
* 条件收敛, 绝对收敛
* 级数的加减乘除

### 幂级数
* 收敛半径: 
  $\frac{1}{R} = \lim_{n \rightarrow ∞}\frac{a_{k+1}}{a_{k}}$  
  在洛朗级数里面, 内圆收敛半径: 
  $\frac{1}{r} = \lim_{n \rightarrow ∞}\frac{a_{-k}}{a_{-(k+1)}}$  
  或者使用 $\frac{1}{R} = \lim_{n \rightarrow \infty}\sqrt[n]{a_n}$
* 泰勒展开
* 解析延拓: 不知道算是一种概念上的东西还是一个操作性的东西... 
* 洛朗级数展开(双边级数展开): 类似泰勒展开, 在操作上加入一个负幂部分. 
  * 可去奇点, 本心奇点, ($m$ 阶) 极点

## Exercises
* 平面点集: 
  > 点集类型: $0 < \mathrm{arg} \frac{z-i}{z+i} < \frac{π}{4}$, 之类的. 
  > 想法就是 $\mathrm{arg}\frac{a + b i}{\|z\|}$ 变成那样再算. 

* 复数运算
  > Cauchy-Schwarz 不等式:  
  > $$\|\sum_{j = 1}^n c^*_jd_j\|^2 \leq (\sum_{j=1}^n \|c_j\|^2) \cdot (\sum_{k=1}^n \| d_k \|^2)$$  
  > 证明的思路是这样的: 利用内积和韦达定理. 
  
  > 三角函数:  
  > $$\sum_{k=1}^n (\cot \frac{k \pi}{2n+1})^2 = \frac{n(2n-1)}{3}$$  
  > 想法是利用欧拉公式和多项式的根和系数的关系: 
  > * 欧拉公式:  
  $$\cos m\theta + i \sin m\theta = e^{im\theta} = (\cos\theta + i\sin\theta)^m ⇒ \frac{\cos m\theta + i\sin mθ}{\sin^mθ} = \sum_{l=0}^m \left(\begin{array}{l}m\\l\end{array}\right) i^l\cot^{m-l}\theta$$
  > * 可以看出解为 $k\pi/(2n+1)$, 利用韦达定理可以得到和的表达式. 
  
  > 无穷嵌套:  
  > $$z = \sqrt{i + \sqrt{i + \cdots}}$$  
  > 简单的做法就是 $z = \sqrt{i + z}$, 然后计算检测是否满足结果.  
  > 复杂的做法就是写成 $z = \rho e^{i\theta}$ 或者是 $z = a + b i$ 的形式. 

* 复数几何
  > $z_1, z_2, z_3: z_1^2 + z_2^2 + z_3^2 = z_1 z_2 + z_2 z_3 + z_3 z_1 ⇔ z_1, z_2, z_3$ 形成等边三角形  

* 解析函数
  
  > 实值解析函数为常数  
  > 证明思路: 直接使用 C-R 条件即可. 
  
  > 已知解析函数的一部分 (实部或者虚部), 求解解析函数 (复势).  
  > * 首先验证是否是解析函数, 检测那部分是否满足拉普拉斯方程. 
  > * 求解解析函数的一部分 (如果有必要的话):  
  >   比如有一个圆族方程: $x^2 + y^2 = r^2$, 
  >   那么把变动的那一项写出来, 就能够得到等势线族方程. 
  >   等势线族方程对应 $u$. 电力线族对应 $v$. 
  > * 计算的方法主要有三个: 
  >   * 凑全微分 (最优先选择)
  >   * 代数方法 (虽然比较抽象, 但是某些时候比较好算)  
  > 
  >     $$u(x, y) = \frac{1}{2} (f(x+iy) + f^*(x-iy)), v(x, y) = \frac{1}{2i} (f(x+iy) - f^*(x-iy))$$
  > 
  >     基石 $↑$, 于是计算的时候, 
  >     相当于只需要计算 $f(z) = 2u(\frac{z}{2}, \frac{z}{2i}) - u(0,0) + iC$ 即可. 
  >   * 路径积分 (最容易理解, 但是最难算)
  > 
  > 虽然但是, 这个是一个考验熟练度的活... 

* 积分

  > $$\frac{1}{2\pi} \int_{0}^{2\pi} \frac{R\cos\theta}{R^2 - 2rR\cos\theta + r^2} \mathrm{d}\theta = \frac{r}{R^2 - r^2}$$
  > 
  > 思路是这样的: 积分的时候遇到三角函数有理分式的时候, 
  > 可以利用欧拉公式: $z = e^{i\theta}$ 来替换 $\mathrm{d}\theta = \frac{\mathrm{d} x}{iz}$.  
  > 这样问题就变成了有理分式积分. 对于有理分式积分, 思路是这样的:  
  > 将其变成 $\sum \frac{1}{Q(z)} + \sum P(z)$ 的形式, 其中 $Q,P$
  > 为有理多项式. 对于 $\frac{1}{Q(z)}$ 的形式, 通过留数定理来计算.  
  > (Note: 留数定理: $$\oint_C f(z) \mathrm{d}z = 2 \pi i \mathrm{Res} f(z_0)$$, 其中留数为 $\lim_{z \rightarrow z_0} (z-z_0)f(z) = \mathrm{Res} f = a_{-1}$. )

* 级数

  > 计算收敛半径: 
  > * $\sum(a_k + b_k)z^k \rightarrow \mathrm{min} (r_a, r_b)$, 
  >   类似这样的, 虽然可能不太好算, 但是可以从收敛圆的那个图直观感受一下. 
  > * $\sum \frac{z^k}{1-z^k}$
  >   这样的求收敛半径的. 不会做就先猜后证吧... 猜好边界位置之后, 
  >   然后利用放缩来证明, (比如放缩成等比数列求和之类的). 
  
  > 级数展开的问题: 
  > * 泰勒展开: 
  >   * $(1+z)^{1/z}$ 这样的, 可以先求对数, 然后 $\frac{1}{z} \sum\cdots$. 
  >   * $\arctan z$ 这样的, 可以先求导 $\frac{1}{1+z^2}$, 后展开, 再积分. 
  >   * $\mathrm{ln} (1+e^z)$ 这样的, 可以先将 $e^z$ 视为一个 $u$ 展开, 
  >     然后再继续展开. (看来要记一下常用展开)
  > * 洛朗级数: 
  >   * 分数形 $\frac{f(z)}{(z-z_0)^k}$, 
  >     其中 $f(z)$ 是一个只需要普通泰勒展开的
