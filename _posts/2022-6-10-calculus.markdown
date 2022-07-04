---
layout: post
title:  "Calculus The End"
date: 2022-07-04 12:24:02 +0800
math: true
categories: notes
---
# 危
在期中寄后, 微积分就真的很危险, 所以开始复习. 

## 多元函数微分学
### 几何直观理解
#### 切线与法平面
切线看作一种曲线的一阶线性近似. $\vec{s} = (\delta x, \delta y, \delta z)$. 

但是换一种理解方式, 将曲线看作是曲面的交线, 于是切线同时落在两个曲面上, 和两个法向量都垂直, 于是可以得到$\vec{s} = \vec{n_1} \times \vec{n_2}$. 

#### 切平面与法向量
法向量和平面上的任意直线垂直$\leftrightarrow$两个向量的叉乘和这两个向量都垂直. 

于是出于这样的认识, $$\vec{n} = \vec{a}_{\in \varPi} \times \vec{b}_{\in \varPi}$$, 对于空间中的平面, 看作是空间曲线的交错织成的网, 对网上的交叉曲线应用一阶的线性近似得到切向量, $$\vec{a} = (1, 0, \delta_x z)$$. 于是就可以得到法向量: $$\vec{n} = \left\vert\begin{array}{lll} \hat{i} & \hat{j} & \hat{k}\\ 1 & 0 & \delta_x z\\ 0 & 1 & \delta_y z \end{array}\right\vert$$. (参数方程的话就是$\vec{f}(s, t) \rightarrow \vec{a} = \delta_s \vec{f}$, 隐函数同理. )

#### 极值
梯度$\nabla \phi$表现了标量场的变化趋势, 当$\nabla \phi = 0$的时候, 取到极值. (参数方程同理. )

采用多元函数的泰勒展开可以更进一步知道是极大值还是极小值. 展开的方式: $$f(\vec{r}) = f(\vec{r}_0) + (\nabla f \cdot) \delta \vec{r} + \delta\vec{r}^T \mathbb{H}_f \delta\vec{r}$$. (其中$$\mathbb{H}_f = \left(\begin{array}{ll} \partial_{xx} f & \partial_{x y} f \\ \partial_{y x} f & \partial_{yy} f\end{array}\right)$$称为Henssin矩阵. )

~~太难理解了~~

仿照电多极矩展开的方式(虽然也是泰勒展开公式就是了, 参见《电动力学》郭硕鸿(第三版) 63页. )

$$f(\vec{r} + \delta \vec{r}) = \sum_n (-1)^n \frac{(\delta \vec{r} \cdot \nabla)^n f}{n!}$$

这样方便记忆. 

当一阶导无法判断的时候, 通过判断二阶导(或高阶导)来判断极值情况. 

当有约束$g$的存在的时候, 于是可以构造函数$L = f - \lambda g$来处理问题. 就将问题转化为了一般的极值问题了. 

## 积分
### 含参量积分 - 感觉这个可能会是一个重点...
$$F(x) = \int_{c(x)}^{d(x)} f(x, y) \mathrm{d}y$$

重积分和反常含参量积分同理. 

#### 连续性
(矩形区域上的)连续函数可以交换极限和积分顺序. 同函数的连续性定义一样: 

$$\vert F(x + \delta x) - F(x) \vert \leq \int_a^b f(x + \delta x, y) - f(x, y) \mathrm{d}y < \varepsilon$$

$$\Rightarrow \lim_{x \rightarrow x_0} \int_{a(x)}^{b(x)} f \mathrm{d} y = \int_{a(x)}^{b(x)} \lim_{x \rightarrow x_0} f \mathrm{d} y$$

(想法就是连续性: $\delta_x F(x) < \varepsilon$来证明. )

#### 可微性
(矩形区域偏导数连续的函数)积分和求导可以交换顺序. 

$$\Rightarrow \partial_x \int f \mathrm{d} y = \int \partial_x f \mathrm{d} y$$

(证明的思路就是通过: $\lim_{\Delta x \rightarrow 0}\frac{F(x + \Delta x) - F(x)}{\Delta x} = \int_c^d f'_x(x + \theta \Delta x, y)\mathrm{d}y$. )

并且还可以有推论: $F'(x) = \nabla H = (\frac{\partial}{\partial x} + \frac{\partial}{\partial c}c'(x) + \frac{\partial}{\partial d}d'(x))$

#### 可积性
(矩形区域上的)连续函数积分运算可以交换顺序. 

$$\int_x \int_y f \mathrm{d} y \mathrm{d} x = \int_y \int_x f \mathrm{d} x \mathrm{d} y$$

(思路就是利用可导性对积分的积分先求导, 然后说导数一致得到积分一致. 中间有一个消去常量的操作. )

> $\varphi(x) = \frac{1}{(n-1)!} \int_0^x (x - t)^{n-1} f(t) \mathrm{d}t$的$n$阶导数在$x=0$存在. 并且$\varphi^(n) = f(x), \varphi^(i) = 0$

做法就是硬求导. 

#### 让积分变得更好积分
方法就是尽可能把积分区域通过换元换成矩形... 或者是尽可能地让积分变量分离: $\iint f(x, y) \mathrm{d}x\mathrm{d}y = \int g(x)\mathrm{d}x \int h(y)\mathrm{d}y$. 

积分中的微元换元: $\mathrm{d} \rightarrow J(\frac{e_1, \cdots, e_n}{e_1', \cdots, e_n'}) \mathrm{d}'$

### 欧拉积分
#### $\Gamma$函数

$$\Gamma(s) = \int_0^\infty x^{s-1}e^{-x} \mathrm{d}x, s > 0$$

$\Gamma$函数可以分解成两部分: $\int_0^1 x^{s-1}e^{-x} \mathrm{d}x+\int_1^\infty x^{s-1}e^{-x} \mathrm{d}x = \Gamma_1(s) + \Gamma_2(s)$. 然后就能够说收敛. 

性质: 
* $\Gamma(1) = \Gamma(2) = 1, \Gamma(\frac{1}{2}) = \sqrt{\pi}$    
  (当作结论记住算了...)
* 定义域内连续可导    
  (见下方的习题, 想法就是放缩. )
* $\Gamma(s+1) = s\Gamma(s)$    
  利用分部积分公式: $\int_0^\infty x^{s}e^{-x}\mathrm{d}x = s\int_0^\infty x^{s-1}e^{-x}\mathrm{d}x$
* 余元公式: $\Gamma(p)\Gamma(1-p) = \frac{\pi}{\sin p \pi}$    
  $\Gamma(p)\Gamma(1-p) = \Beta(1-p, p) = \frac{\pi}{\sin p \pi}$
* 延拓: $\Gamma(s) = \frac{\Gamma(s+1)}{s}$
* 其他形式
  * $\Gamma(s) = 2\int_0^\infty y^{2s-1}e^{-y^2}\mathrm{d}y$
  * $\Gamma(s) = p^s\int_0^\infty y^{s-1}e^{-py}\mathrm{d}y$

#### $\Beta$函数

$$\Beta(p, q) = \int_0^1 x^{p-1}(1-x)^{q-1}\mathrm{d}x, p, q>0$$

性质: 
* 定义域连续    
  思路也是放缩, 把$p \geq p_0 > 0, q \geq q_0 > 0$, 于是$x^{p-1}(1-x)^{q-1} \leq x^{p_0 - 1}(1-x)^{q_0 - 1}$收敛. 
* 对称$\Beta(p, q) = \Beta(q, p)$    
  想法就是变换积分顺序就好了. 
* 递推    
  $\Beta(p, q) = \frac{q - 1}{p+q-1}\Beta(p, q-1), p>0, q>1$    
  推论: $\Beta(m, n) = \frac{(m-1)!(n-1)!}{(m+n-1)!}$
* Dirichlet公式    
  $\Beta(p, q) = \frac{\Gamma(p)\Gamma(q)}{\Gamma(p+q)}$
* 余元公式    
  $\Beta(p, 1-p) = \frac{\pi}{\sin p \pi}$

### 空间积分
#### 曲线积分
$$\int_a^b f(\vec{r}) \delta \vec{r}$$

## 习题
### 一致收敛的判别准则
对于含参量反常积分: 

$$\int_c^{\infty} f(x, y) \mathrm{d}y$$

* Cauchy准则: 一致收敛$\Leftrightarrow \forall \varepsilon > 0, \exist N(\varepsilon) > C, N_2 > N_1 > N \forall x \in [a, b], \vert \int_{N_1}^{N_2} f(x, y) \mathrm{d}y < \varepsilon$    
  (回忆数列收敛的柯西公式)
* Wiestrass判别法: 一致收敛$\Leftarrow \vert f(x, y) \vert \leq g(y), \int_c^{\infty} g(y) \mathrm{d}y$    
  (迫敛定理)
* Abel判别法: (一致收敛 + 有界)
  * $\int_c^\infty f(x, y) \mathrm{d}y$一致收敛
  * $\sup g(x, y)$有界
* Dirichlet判别法: (有限 + $\rightarrow 0$)
  * $\exists M > 0, \forall N > c, \sup \vert \int_c^N f(x, y) \mathrm{d}y \vert \leq M$有界
  * $\lim_{y \rightarrow \infty} \sup g(x, y) = 0$单调趋于0

(Abel和Dirchlet的判别法可以类比数列收敛的判别法. )

> $\int_0^\infty e^{-kx}\cos{a x}\mathrm{d}x, k>0, a\in\mathbb{R}$一致收敛

* 一致收敛的证明: 利用柯西判别准则, $\vert I_S^T \vert < \varepsilon$. 其中$I_S^T$可以通过分布积分公式来得到$\leq \frac{2}{k}(e^{-kS}+e^{-kT})\leq \frac{4}{k}e^{-kN}<\varepsilon$然后放缩. 
* 和函数: 其实先分布积分之后再求极限就可以得到了.

> $\int_0^\infty \frac{\cos x y}{1+x^2}\mathrm{d}x$一致收敛

放缩, 因为$\frac{\cos x y}{1+x^2} < \frac{1}{1+x^2}$. 后者收敛. 

> $\int_0^\infty e^{-xy}\frac{\sin x}{x}\mathrm{d}x$一致收敛

$\int_0^\infty \frac{\sin x y}{x}\mathrm{d}y$一致收敛, $e^{-xy}$单调一致有界. 

> $\int_1^{\infty}\frac{y \sin x y}{1+y^2}\mathrm{d}y, (0,\infty)$内闭一致收敛

想法就是Dirichlet判别法. $\int_a^N \sin x y\mathrm{d}y$有界, $\frac{y}{1+y^2}$单调趋于零. 

### 积分变换公式
一个简单的记忆方法: 

就是从环路定理和高斯定理出发:

$$\begin{array}{lll}\oint_C \vec{f} \cdot \mathrm{d}\vec{l} & = & \int_S (\nabla \times \vec{f}) \cdot \mathrm{d} \vec{S}\\\oint_S \vec{f} \cdot \mathrm{d}\vec{S} & = & \int_V (\nabla \cdot \vec{f}) \mathrm{d}V\end{array}$$

然后把$\nabla$算符限制到对应的空间(平面或者立体). 

#### $\oint_C \frac{x \mathrm{d}y - y \mathrm{d}x}{x^2 + y^2}$
* 对于不经过原点的封闭曲线: $\nabla \times \vec{f} = 0$
* 对于经过原点的封闭曲线, 将路径分割成一段圆弧(一般是用于$\vec{f}(\vec{r})$, 如果是椭圆的话, 先仿射变换变成圆. )和一段不经过原点的曲线, 这样就能够变成一个和切角有关的结果. (和曲面的操作类似. )

#### $$\int_S \frac{\partial f}{\partial \vec{n}} \mathrm{d}S$$

利用一个结论: $\frac{\partial f}{\partial \vec{n}} = (\nabla f) \cdot \vec{n}$, 其中$\vec{n}$为单位向量. 然后利用高斯公式将面积分转化为体积分: $=\int_V \Delta f\mathrm{d} V$. 
* 对于面积分: $\int_S f_x \mathrm{d}y \wedge \mathrm{d}z + f_y \mathrm{d}z \wedge \mathrm{d}x + f_z \mathrm{d}x \wedge \mathrm{d}y = \int_S \vec{f} \cdot \mathrm{d} \vec{S} = \int_V \nabla \cdot \vec{f} \mathrm{d}V$. 假如$\nabla \cdot \vec{f} = c$的话就能方便积分了. 
* 如果$\nabla \cdot \vec{f}$在某一点附近不存在(如$\frac{1}{r}$这种. )就通过在这个附近做一个圆(或者椭圆, 比如$\vec{f}(a^2x^2+b^2y^2+c^2z^2)$)然后减去这个面. 
* 如果只有一部分面, 就将这个面补全然后变成体积分. 

#### 积分和微分的变换
* $$\nabla \cdot \vec{f} = \lim_{V \rightarrow 0} \frac{\iint_V \vec{f} \cdot \mathrm{d}\vec{S}}{V}$$
* $$(\nabla \times \vec{f}) \cdot \vec{n}_S = \lim_{S \rightarrow 0} \frac{\oint_C \vec{f} \times \mathrm{d}\vec{l}}{S}$$

### 简单积分
#### 曲线积分类型和关系
* 一型曲线积分: $\int_L f \mathrm{d} s$ (给人的感觉更像是一种自然坐标系下的积分)
* 二型曲线积分: $\int_L \vec{f} \cdot \mathrm{d}\vec{l}$ (做功的累积)    
  二型曲线积分和一型曲线积分的相互转化: $\int_L \vec{f} \cdot \mathrm{d}\vec{l} = \int_L f \cos (\hat{f}, \hat{s})\mathrm{d}s$
* 一型曲面积分: $\int_S f \mathrm{d}S$
* 二型曲面积分: $\int_S \vec{f} \cdot \mathrm{d}\vec{S}$    
  相互转换: $\int_S \vec{f} \cdot \mathrm{d}\vec{S} = \int_S f \cos(\hat{f}, \hat{S})\mathrm{d}S$

#### 计算
* 旋转体积分: $f(x)$为曲线, 绕$x$轴转一圈, 然后求体积. 就$V = \int_a^b \pi f^2(x) \mathrm{d}x$. 
* 面积分与体积分: 一般换一个好一点的参考系会有很大的帮助. 还有就是要注意面的正负号. 
  * $\\int_D (x-y^2)e^y\mathrm{d}x\mathrm{d}y, D:y=2, y^2-y-x=0,y^2+2y-x=0$    
    换元: $u = x - y^2, v = y$. 这种就是看积分区域和积分元素, 能不能变成矩形. 换元之后就需要考虑积分区域和$J(\frac{x, y}{u, v})$的体积变换. 
  * 上面的一般可以有极坐标, 球坐标等等. 常见的变换有: 
    * $x - y = u, x+y = v$
    * $a r \cos \theta + b r \sin \theta$
    * ...

### 含参量积分
#### 一致收敛判断
> $$\int_0^\infty \frac{\sin \alpha x}{x} \mathrm{d}x$$    
> 判断一致收敛性. ($\alpha \in (0, 1)$)

和函数列的一致收敛类比, $F(\alpha) = \sum \int_{a_i}^{b_i} f(x, \alpha) \mathrm{d}x$. 于是套用柯西收敛判别: $\sup \vert \int_{a_m}^{b_n} f(x, \alpha) \mathrm{d}x \vert < \varepsilon$. (看作是$\vert F(\alpha)_m - F(\alpha)_n \vert < \varepsilon$)

#### 积分
利用连续性: (对含参量积分求极限的问题): 

* $\lim_{\alpha \rightarrow 0} \frac{\mathrm{d}x}{1+x^2+\alpha^2}$    
  利用的是连续性. 先求极限然后积分. 

利用导数可交换: 
* 计算导数的问题: 
  * $\nabla^2 \int_0^a\frac{f(t)\mathrm{d}t}{\sqrt{(x-t)^2+y^2+z^2}} = 0$    
    先不管导数, 直接提进去计算, 然后就是装模作样的化简过程了. 
* 构造能求导的: (先构造一个能求导的, 然后求导. )
  * $\int_0^1\frac{\ln(1+x)}{1+x^2}\mathrm{d}x$    
    $I(\alpha) = \int_0^1\frac{\ln(1+\alpha x)}{1+x^2}\mathrm{d}x$. 然后先对$\alpha$求导, 再交换积分. 就完事...
  * $\int_0^\infty \frac{\sin x}{x}\mathrm{d}x$    
    同上$I = \int_0^\infty \frac{\sin \alpha x}{x}\mathrm{d}x\Rightarrow I = \int_0^1\frac{\partial}{\partial \alpha}\frac{\sin x}{x}\mathrm{d}x\mathrm{d}\alpha = \frac{\pi}{2}$(这个方法不太好)    
    构造$F(p) = \int_0^\infty e^{-px}\frac{\sin \alpha x}{x}\mathrm{d}x = \arctan\frac{\alpha}{p}$    
    或者用复数来积分. 
  * $\int_0^\pi \ln(1+\frac{\cos x}{2})\mathrm{d}x$    
    构造$\int_0^\pi \ln(1+y\cos x)\mathrm{d}x$
* 就是先求导再积分的
  * $\int_0^\pi \ln(1-2\alpha \cos^2 x + \alpha^2)\mathrm{d}x$    
  * $\int_0^{\frac{\pi}{2}}\ln(a^2\sin^2x+b^2\cos^2x)\mathrm{d}x$    
    竟然是$1$的妙用, 换元$u = \tan x$, $I = \int_0^{\frac{\pi}{2}}\ln(\frac{a^2u^2+b^2}{u^2+1})(u^2+1)\mathrm{d}u$
* 利用导数来构造积分递推的: 
  * $I = \int_0^\infty e^{-x^2}\cos r x\mathrm{d}x$
    就对$r$求导, 得到$I' = -\frac{r}{2}I$, 然后解微分方程. 


利用积分可交换: (求积分的问题): 
* 通过换成双重积分来处理: 
  * $\int_0^1 \frac{x^b-x^a}{\ln x}\mathrm{d}x$    
    利用的是积分可换的性质. $=\int_0^1\int_a^b x^y \mathrm{d}y=\ln \frac{1+b}{1+a}$
  * $\int_0^1 \sin(\ln\frac{1}{x}) \frac{x^{b-1}-x^{a-1}}{\ln x}\mathrm{d}x$    
    这个做换元: $\ln x \rightarrow u, \int_{-\infty}^{0}\sin u \frac{(e^u)^{b-1}-(e^u)^{a-1}}{u}\mathrm{d}u$, 然后把$(e^u)^{b-1}-(e^u)^{a-1}$换成$\int_{a-1}^{b-1}u(e^u)^y\mathrm{d}y$, 问题解决. 
  * $\int_0^\infty e^{-px}\frac{\sin b x - \sin a x}{x}\mathrm{d}x$    
    $\frac{\sin b x - \sin a x}{x} = \int_a^b \cos x y\mathrm{d}y$

### 欧拉函数
#### $\Gamma(s) = \int_0^\infty t^{s-1}e^{-t} \mathrm{d}t$ 收敛且无穷可微
(参考$\Gamma$函数的连续和可导的证明方法)

将$\Gamma(s)$看成是$\Gamma_1(s) = \int_0^1 x^{s-1}e^{-x}\mathrm{d}x$和$\Gamma_2(s) = \int_1^\infty x^{s-1}e^{-x}\mathrm{d}x$的组合. 

核心思想是放缩, 令$s \in [a, b]$: 
* $\Gamma_1(s)$中, $x^{s-1}e^{-x} \leq x^{a-1}e^{-x}$, 后者收敛
* $\Gamma_2(s)$中, $x^{s-1}e^{-x} \leq x^{b-1}e^{-x}$, 后者收敛

于是可以说一致收敛, 至于导数, 可以有$\int_0^\infty \frac{\partial}{\partial s}(x^{s-1}e^{-x})\mathrm{d}x = \int_0^\infty x^{s-1}e^{-x}\ln x\mathrm{d}x$在$(0, \infty)$上一致收敛. 于是任意可导. 

* $\Gamma(s) = \sum\frac{(-1)^n}{n!}\frac{1}{s+n}+\Gamma_1(s)$

#### 变换成欧拉函数的积分
* $\lim_{n \rightarrow \infty} \int_0^\infty e^{x^n} \mathrm{d}x = 1$    
  设$t = x^n$, 于是$\int_0^\infty \frac{1}{n} t^{\frac{1}{n} - 1}e^{-t}\mathrm{d}t = \frac{1}{n}\Gamma(\frac{1}{n}) = \Gamma(\frac{1}{n}+1)$, 求完极限之后就是$1$了. 
* $\Gamma(\frac{1}{2}\pm n) = n!\Gamma(\frac{1}{2}) = n!\sqrt{n}$    
  用的是一个递推关系. 
* $\int_0^1 (\ln\frac{1}{x})^{\alpha-1}\mathrm{d}x = \Gamma(\alpha)$
  换元$u = \ln\frac{1}{x}$, 然后就可以有: $u^{\alpha - 1}e^u\mathrm{d}u$

### 多元函数的性质
#### 极小值点
> $$\mathbb{H}_f(x_0, y_0)$$行列式为正, 并且二阶偏导数$$\partial_{x,x}^2f(x_0, y_0), \partial_{y,y}^2 f(x_0, y_0)$$有一个为正, 则$$(x_0, y_0)$$为极小值点. 

判断极小(大)值点的方法: $$\mathbb{H}_f$$是正定还是半正定. $$\det\mathbb{H}_f = f_{xx}f_{yy}-f_{xy}^2>0\Rightarrow f_{xx}, f_{yy}>0$$. (然后应该是要用线性代数的方法来判断的. 这里因为是二阶, 所以化简完后就是$$\mathbb{H}_f = \mathrm{diag}(f_{xx}, f_{yy} - \frac{f_{xy}^2}{f_{xx}})$$, 然后就能够判断正定与否. )

#### 凸函数证明
> 证明$f$为凸函数$\Leftrightarrow \mathbb{H}_f = \nabla^2 f(x)$为半正定的. 

凸函数$\Leftrightarrow \lambda f(b) + (1 - \lambda) f(a) \geq f(\lambda b + (1 - \lambda) a)$. ($\lambda$可以随便选)

证明: 
* $\Leftarrow$: 半正定$\Leftrightarrow f(b) - f(a) \geq \nabla f(a) (b - a) \Leftrightarrow (b - a)^T \mathbb{H}_f (b - a) \Rightarrow \mathbb{H}_f$
* $\Rightarrow$: $f(a + \lambda (b - a)) = f(a) + \lambda \nabla f (b - a) + \lambda^2(b-a)^T \mathbb{H}_f (b-a) \Rightarrow f(a + \lambda (b - a)) - f(a) = \lambda \nabla f (b - a) + \lambda^2(b-a)^T \mathbb{H}_f (b-a) \geq \lambda \nabla f (b - a)$

#### $\sqrt[n]{x_1 x_2 \cdots x_n} \leq \frac{x_1 + x_2 + \cdots + x_n}{n}, \sum x_i = a, (a > 0)$
Lagrand乘子法: $L = x_1 x_2 \cdots x_n + \lambda (x_1 + \cdots + x_n)$, 然后$\frac{\partial L}{\partial x_i} = 0$. 完事了. 

然后对于这个极值的类型, 通过判断$\mathbb{H}_f$的正定还是负定来判断. (如果要判断非正定, 只要找一个正定即可. )

## 最后
感谢老师和助教...