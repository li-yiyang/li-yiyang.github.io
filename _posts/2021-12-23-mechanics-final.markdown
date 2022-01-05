---
layout: post
title:  "Mechanics The End"
date:   2022-01-03 11:12:50 +0800
categories: jekyll update
math: true
---
# 力学 The End
就是对这个学期学的力学的*完全的*不完全归纳复习. 
应该会有和之前的重复的部分, 就当是一个学吧. 

假如用一句话来概括的话, 经典力学就是所有满足伽利略变换不变性的力学性质. 

> 伽利略变换: 是没有原点的仿射变换
> * 3 个平动
> * 3 个平移
> * 3 个转动
> * 1 个时间平移

## 理论部分
### PLA (Principle of Least Action)
作为我认为的一种很妙的思想, 我觉得我应该把*最小作用量原理*写到最前面. 

* 和光学的类比

### Newton's Principa
牛顿的三个定律: 
* **N1**: 
* **N2**: force is the time rate of momentum. $\boldsymbol{F} = \frac{\mathbb{d}\boldsymbol{p}}{\mathbb{d}t}$
* **N3**: action and reaction

一个重要概念: 加速度和速度是独立的量. 

#### 刚体

$$\boldsymbol{r}(\nabla_{\boldsymbol{v}} \cdot \boldsymbol{\omega}) = \boldsymbol{I} \boldsymbol{\omega}$$

其中二阶张量$\boldsymbol{I} = \boldsymbol{r}\nabla_{\boldsymbol{v}} = ...$

#### First Principle
> That (Noether's) theorem has been guiding star to 
> the 20th and 21st theoretical physics.      
> -- Frank Wilctek

* Impluse:       
  $$\boldsymbol{p}_1 - \boldsymbol{p}_2 = \int_{t_1}^{t_2} \boldsymbol{f} \mathrm{d} t$$
* Energy:      
  $$\boldsymbol{f} = \frac{\mathbb{d}\boldsymbol{p}}{\mathbb{d}t} = m \frac{\mathbb{d}\boldsymbol{v}}{\mathbb{d}t} \\ \boldsymbol{f} \cdot \boldsymbol{v} = m \boldsymbol{v} \cdot \frac{\mathbb{d}\boldsymbol{v}}{\mathbb{d}t} \\ \int_{\boldsymbol{r}_1}^{\boldsymbol{r}_2} \boldsymbol{f} \cdot \mathrm{d}\boldsymbol{r} = \int_{\boldsymbol{v}_1}^{\boldsymbol{v}_2} \frac{1}{2} m \mathrm{d}(\boldsymbol{v}^2) = \Delta T$$       
  并且对于保守场:       
  $$ \boldsymbol{F} = - \nabla \phi$$
* Angular Momentum:      
  $$\boldsymbol{L} = \boldsymbol{r} \times \boldsymbol{p} \\ \frac{\mathrm{d} \boldsymbol{L}}{\mathrm{d} t} = \boldsymbol{v} \times \boldsymbol{p}_{= m \boldsymbol{v}} + \boldsymbol{r} \times \boldsymbol{f} = \boldsymbol{r} \times \boldsymbol{f} \\ \mathrm{d} \boldsymbol{L} = \boldsymbol{r} \times \boldsymbol{f} \mathrm{d} t = \boldsymbol{M}$$

(注: 类似于这样的, 从第一性原理出发构建一个理论框架, 
比如有连续介质力学的八公理系统, 里面就有一致性定理和完备性定理)

### Lagrange Analytic Mechanics
关键是找到**拉氏量**. 

对于保守系统: 

$$L = T - V \\
\frac{\mathrm{d}}{\mathrm{d}}(\frac{\partial L}{\partial q_i}) - \frac{\partial L}{\partial \dot{q_i}} = 0$$

然后就通过解方程就可以得到运动的结论了. 

#### Virtual Displacement & Real Displacement
和Real Displacement不同的是, Virtual Displacement是一个时间冻结下的运动, 
就是说, 假如Real Displacement是: 

$$\mathrm{d} \boldsymbol{r} = \frac{\partial \boldsymbol{r}}{\partial t} \mathrm{d} t + \sum \frac{\partial \boldsymbol{r}}{\partial q_{i}} \mathrm{d} q_{i}$$

那么Virtual Displacement就会是: 

$$\delta \boldsymbol{r} = \sum \frac{\partial \boldsymbol{r}}{\partial q_{i}} \delta q_{i}$$

其中的$\delta q_i$是任意的满足约束的位移(广义坐标), 这样的偏移量是任意的, 
(只要是满足约束的). 

(注: 这样的由广义坐标组成的空间叫做Configuration Space位形空间, 由Riemann 
Geometry)

并且关于这个, 还有两个拉格朗日关系: 

$$\left\{ \begin{array}{lll} \frac{\partial}{\partial \dot{q_{\beta}}} (\frac{\mathrm{d} \boldsymbol{r}}{\mathrm{d} t}) & = & \frac{\partial \boldsymbol{r}}{\partial q_{\beta}} \\ \frac{\partial }{\partial q_{\beta}} (\frac{\mathrm{d} \boldsymbol{r}}{\mathrm{d} t}) & = & \frac{\mathrm{d}}{\mathrm{d} t} (\frac{\partial \boldsymbol{r}}{\partial q_{\beta}})\end{array} \right.$$

#### Euler's Theorem of Homogenous Functioin
对于$\lambda$次齐次函数: 

$$f(\alpha x_1, \cdots, \alpha x_n) = \alpha^{\lambda} f(x_1, \cdots, x_2) \\ \frac{\mathrm{d} f}{\mathrm{d} \alpha} = \frac{\partial f}{\partial (\alpha x_i)} \frac{\partial (\alpha x_i)}{\partial \alpha} = \lambda \alpha^{\lambda - 1} f \\ \Rightarrow \alpha \frac{\mathrm{d} f}{\mathrm{d} \alpha} = \lambda f$$

于是可以说有这样的结论: 

$$\alpha \frac{\mathrm{d} f}{\mathrm{d} \alpha} = \lambda f$$

#### 位力定律
对动能(速度的2次齐次函数)应用Euler齐次函数定理: 

$$2 K = \frac{\partial K}{\partial \boldsymbol{v}} \boldsymbol{v} = \boldsymbol{p} \frac{\mathrm{d} \boldsymbol{r}}{\mathrm{d} t} = \frac{\mathrm{d}}{\mathrm{d} t}(\boldsymbol{p} \cdot \boldsymbol{r}) - \frac{\mathrm{d} \boldsymbol{p}}{\mathrm{d} t} \cdot \boldsymbol{r}$$

对两端取时间平均, 相对一个时间无穷大的情况, 然后对于有限的运动, 
即$(\boldsymbol{p} \cdot \boldsymbol{r})$有限, 所以相对无穷的时间平均, 
$\lim_{\tau \rightarrow \infty} \langle m \rangle_{\tau} = 0$. 于是得到: 

$$\overline{2K} = \overline{- \boldsymbol{r} \cdot \boldsymbol{F}} = \overline{\boldsymbol{r} \cdot \frac{\partial U}{\partial \boldsymbol{r}}} = \lambda \overline{U}$$

于是能量就可以写成

$$E = (\frac{\lambda}{2} + 1) \overline{U}$$

#### 力学相似性
就是怎样的坐标和时间变换才能满足力学系统的不变性. 

对于拉氏方程: $L = T - V$, 可以知道$T = \frac{1}{2} m \dot{q}^2$, 
也就是会有在相似变换$q \mapsto \alpha q, t \mapsto \beta t$下, 
有$L' = T' - V' = (\frac{\alpha}{\beta})^{\lambda_{T}} T - (\alpha)^{\lambda_{V}} V$, 这里假设$T$, $V$都是齐次函数. 

于是就可以说, 假如$L$要满足条件, 即$L / L' = c$就是只差一个系数的话, 
那么就可以保证方程不变, 也就是力学相似性. 
#### Lagrange 的一些用法
* Ignorable Coordinate (Cyclic Coordinate)      
  对于$L$中不显含$q_{\alpha}$的系统, 关于广义动量守恒, 
  即$\frac{\mathrm{d}}{\mathrm{d} t}(\frac{\partial L}{\partial \dot{q_{\alpha}}}) = 0$, 
  于是就可以得到守恒的结论. 
* Energy Conservation     
  对于$\frac{\partial L}{\partial t} = 0$, 就有能量守恒式的形式. 

#### Lagrange Equtation Proof
证明拉格朗日方程的方法. 

从达朗贝尔原理开始: 

$$ \boldsymbol{F} - \sum m \delta \ddot{\boldsymbol{r}_i} = 0$$

利用拉格朗日关系可以得到, 以前有写过. 略. 

或者也可以通过$\delta S = 0$, 利用作用量的思路来做事: 

(一个很新的做法)

$$$$

(或者干脆就$\delta \int L \mathrm{d} t = 0 \Rightarrow \mathrm{Lagrange\ Equation}$, 利用变分法里面的欧拉公式. )

### Hamilton Mechanics
Hamilton量的构造: 

$$H = \sum_{\alpha} p_{\alpha} \dot{q}_{\alpha} - L$$

(这里要注意的就是, 其中$L(q, \dot{q}, t)$, 要想构造出$H(q, p, t)$的话, 
就要把$L$中的$\dot{q}$做换元代掉. )

$$\left\{ \begin{array}{lll}\dot{p}_i & = & - \frac{\partial H}{\partial q_i} \\ \dot{q}_i & = & \frac{\partial H}{\partial p_i}\end{array}\right.$$

* Legendre Transformation      
  变换的核心思想就是
  $$\mathrm{d} L = \frac{\partial L}{\partial q} \mathrm{d} q + \frac{\partial L}{\partial \dot{q}} \mathrm{d} \dot{q}$$
  就是$H$函数的全微分, 目标就是为了改变函数的变元, 将$L(q, \dot{q})$变成
  $H(q, p)$的变元, 为了达到这样的目的, 就需要把全微分的形式变掉. 
  也就是变成
  $$\mathrm{d} H = \frac{\partial H}{\partial q} \mathrm{d} q + \frac{\partial H}{\partial p} \mathrm{d} \dot{p}$$
  的形式, 为了达到这样的结果, 构造$H = \frac{\partial L}{\partial \dot{q}} \dot{q} - L = p \dot{q} - L$, 
  于是就可以得到一个$\mathrm{d} H = \dot{q} \mathrm{d} p - \dot{p} \mathrm{d} q$
* 补充的关系式: $\frac{\partial H}{\partial t} = - \frac{\partial L}{\partial t}$, 
  也就是说, 对于不含时间的$L$的系统, $H$守恒
* 补充的说明: 对于保守系统, $H = E$

#### Possion Bracket

$$\frac{\mathrm{d} f}{\mathrm{d} t} = \frac{\partial f}{\partial t} + [f, H]$$



### Special Relativity
Lorentz Transformation: 

$$\gamma = \frac{1}{\sqrt{1 - (\frac{v}{c})^2}} \\ x' = \gamma (x - v t) \\ y' = y \\ z' = z \\ t' = \gamma (t - \frac{v x}{c^2})$$

#### Minkowski Spacetime


#### Energy Momentum Relation

$$E^2 = p^2 c^2 + m^2 c^2$$

(推导的方法就是$E = \gamma m c^2$, 然后通过改变分母就可以拆分得到结果. )

### Quantum Physics (Very Basic)
#### 修正条件
当作用量的量级和$h$相同甚至更小的时候就需要对经典物理学进行修正. 
可以在海森堡不确定性原理里面看到. 

**Heisenberg Uncertainty Principle**

$$\begin{array}{lll} \Delta E \Delta t & \geq & \hbar / 2 \\ \Delta \boldsymbol{p} \cdot \Delta \boldsymbol{x} & \geq & \hbar / 2 \\ \Delta \tau \Delta \theta & \geq & \hbar / 2 \end{array}$$

#### Schodingger Equation
1st order in time, 2nd order in space, 不满足洛伦兹协变

$$H = \frac{p^2}{2m} + V = E \\ H \mapsto \hat{E} = i \hbar \frac{\partial}{\partial t}, \hat{\boldsymbol{p}} = i \hbar \nabla \\ - \frac{\hbar}{2m} \nabla^2 \psi + V \psi = i \hbar \frac{\partial}{\partial t} \psi$$

这里的运用了一种叫做算符化的神奇操作, 我不是很清楚, 但是我们老师说可以做. 
这样的操作可以做就是了, 咳, 我还是以后学啦. 

#### 克莱因-戈登方程
2st order in time, 2nd order in space, 满足洛伦兹协变, 但是时间二阶无法描述
氢原子行为. 狄拉克将空间降阶位一阶, 但还是没有解决负能量的问题. 

这个方程是满足洛伦兹协变的. 

比如通过洛伦兹变换, 可以得到: 

$$\frac{\partial}{\partial x} = \frac{\partial}{\partial x'} \frac{\partial x'}{\partial x} + \frac{\partial}{\partial t'} \frac{\partial t'}{\partial t} = \gamma \frac{\partial}{\partial x'} - \frac{\gamma v}{c^2} \frac{\partial}{\partial t'} \\ \frac{\partial^2}{\partial x^2} = \gamma^2 \frac{\partial^2}{\partial x'^2} + (\frac{\gamma v}{c^2})^2 \frac{\partial^2}{\partial t'^2} - 2 \frac{\gamma^2 v}{c^2} \frac{\partial^2}{\partial x' \partial t} \\ \frac{\partial}{\partial t} = - \gamma v \frac{\partial}{\partial x'} + \gamma \frac{\partial}{\partial t'}$$

代入到Klein-Gordon Equation中, 

$$(\square + \mu^2)\psi = 0$$

只需要看前面的算子部分就好了, (经过化简可以得到)变换前后只是把$t$变成$t'$, 
$x$变成$x'$而已. 于是说明是保持洛伦兹变换的. 

### Others
#### Dimension
Principle of Dimensional Homogeneity: 
* 方程两边对应的**量纲的次数**相等
* 方程两边的**导数**次数相等
* 爱丁顿: 量纲一致性包括张量方程中的**协变**和**逆变**一致

#### 谐振子模型
> 有学者认为, 理论物理的$75\%$都是谐振子模型

首先, 是简单的利用牛顿定律列出**动力学方程**的做法: 

$$m \ddot{\boldsymbol{x}} = - k \boldsymbol{x}\\
I \ddot{\theta} = M = \lambda \theta$$

实际上直接就: 

$$ \ddot{q} + \omega^2 q = 0$$

还有利用**能量守恒**的方法写出方程: 

$$ E = \frac{m \dot{x}^2}{2} + \frac{1}{2} k x^2 = T + V \\ \frac{\mathrm{d} E}{\mathrm{d} t} = 0 \Rightarrow m \ddot{x} + k x = 0$$

对谐振子模型的**相对论**处理方式: 

对质量展开: 

$$m = \gamma m_0 \simeq m_0 (1 + \frac{1}{2} \frac{v^2}{c^2} + \frac{3}{8} \frac{v^4}{c^4} + \cdots),\ (\frac{v}{c} \ll 1)$$

考虑原来的周期: 

$$ \tau = 2\pi \sqrt{\frac{\langle m \rangle_t}{k}}$$

(其中$\langle m \rangle_t$就是质量对时间的平均, 即
$$\langle m \rangle_t = \frac{1}{\tau} \int_{0}^{\tau} m(t) \mathrm{d} t$$
)

当然, 也可以对位移进行平均, 结果是类似的. 

$$\langle m \rangle_{\boldsymbol{x}} = \frac{1}{2A} \int_{-A}^{A} m(x) \mathrm{d} x$$

**量子的振动**

考虑一维振动, $V = \frac{1}{2} m \omega^2 x^2$

$$H = \frac{p^2}{2 m} + V = E \\ S = \int p \mathrm{d} q \Rightarrow p = \frac{\partial S}{\partial x} \\ $$

**振动的波方程**

对于弦的波动方程: 

$$\mu \frac{\partial^2 u}{\partial t^2} = T \frac{\partial^2 u}{\partial x^2} \\ \Leftrightarrow \square u = 0$$

#### 量
根据量的类型: 
* 强度量 **Intensive Quantity**: 密度, 温度
* 广延量 **Extensice Quantity**: 体积, 面积 *可以累加*

根据量的关系, 也就是共轭(conjugate): 
* Energy Conjugation: i.e. $W = \boldsymbol{F} \Delta \boldsymbol{x}$
* Action Conjugation: i.e. $S = \int \boldsymbol{p} \cdot \boldsymbol{q}$
  

#### 对称和守恒
> 诺顿定理: 守恒律对应不变性(Symmetry invariance)

* 黑洞三毛定律: 进入黑洞中, 仍然保持不变的量是: **质量**, **电荷**, **角动量**
* 在经典力学中, 有动量, 动量矩, 能量, LRL矢量守恒

**Mass Continuity Equation**

$$\frac{\partial \rho}{\partial t} + \nabla \cdot (\rho \boldsymbol{v}) = 0$$

**Time Reversal**

$$t \mapsto - t \\ i \mapsto -i \\ \boldsymbol{x} \mapsto -\boldsymbol{x} \\ \boldsymbol{v} \mapsto -\boldsymbol{v} \\ \boldsymbol{a} \mapsto \boldsymbol{a} \\ \boldsymbol{J} \mapsto -\boldsymbol{J} \\ \boldsymbol{E} \mapsto \boldsymbol{E} \\ \boldsymbol{B} \mapsto -\boldsymbol{B}$$

**Charge Conjugation**

$$\rho \mapsto - \rho \\ \boldsymbol{E} \mapsto - \boldsymbol{E} \\ \boldsymbol{B} \mapsto - \boldsymbol{B} \\ \boldsymbol{J} \mapsto - \boldsymbol{J}$$

**Conjugate Transpose 共轭转置**

$$(\boldsymbol{A}^H)_{i j} = \overline{\boldsymbol{A}_{j i}}$$

#### Noether's Theorem Proof
* Space Translatioin Invariant <-> Momentum Conservation
* Time Translation Invariant <-> Energy Conservation
* Space Rotation Invariant <-> Angular Momentum Conservation
* Mass Conservation (也是一种能量守恒)

#### Carriers
* **Graviton** 引力子 (尚未被发现) 引力
  $\frac{G M_p}{\hbar c} \sim 5.9 \times 10^{-39}$
* **Bosons** 玻色子 弱力
* **Photons** 光子 电磁力
  $\alpha = \frac{e^2}{4 \pi \varepsilon_0 \hbar c} \approx \frac{1}{137}$
* **Gluons** 胶子 强力

#### 算符化
虽然我对这个不是很清楚, 但是这个好像就是基于一个假设, 
就是假如一个方程成立, 那么将它们(中的比如说$\boldsymbol{p}$, $E$)
算符化后作用到波函数上, 方程仍然成立. 

首先有一个关系: (在一个波中)

$$\left\{ \begin{array}{lll} \boldsymbol{p} & = & \hbar \boldsymbol{k} \\ E & = & \hbar \omega \end{array}\right.$$

然后对于物质波: $\varphi = e^{i (\boldsymbol{k} \cdot \boldsymbol{r} - \omega t)} = e^{\frac{i}{\hbar}(\boldsymbol{p} \cdot \boldsymbol{r} - E t)}$

于是就可以得到了好结果. 

$$\frac{\partial \varphi}{\partial t} = - \frac{i E}{\hbar} \varphi \\ \Rightarrow \hat{E} = i \hbar \frac{\partial}{\partial t} \\ \nabla \varphi = \frac{i}{\hbar} \nabla(\boldsymbol{p} \cdot \boldsymbol{r}) \varphi = \frac{i}{\hbar} \boldsymbol{p} \varphi \\ \Rightarrow \hat{\boldsymbol{p}} = - i \hbar \nabla$$

(注: 虽然赵爹说这个是一个推导过程, 但是实际上我觉得里面有一个假设, 
认为物质的分布概率就是满足一种类似于波的分布的假设, 然后根据这样的概率, 
就得到了物质波的一个假设. )

#### 变换
规范变换. 

### Math
#### 平面极坐标系

$$\left\{ \begin{array}{lll} \mathrm{d} \hat{\boldsymbol{e}}_{\theta} & = & - \dot{\theta} \hat{\boldsymbol{e}}_{r} \\ \mathrm{d} \hat{\boldsymbol{e}}_{r} & = & \dot{\theta} \hat{\boldsymbol{e}}_{\theta} \end{array}\right.$$

(注: 可以利用$SO(2)$的来帮助理解. )

#### 线性
满足这样的方程的就是线性方程: 

$$f(\alpha X + \beta Y) = \alpha f(X) + \beta f(Y)$$

如何验证一个方程是线性的, 只要将两个*可行解*的*线性组合*代入原方程, 
然后只要这个方程是线性的, 那么就说明这个方程就是线性方程. 

#### 算子还有指标运算
这里的算子的运算规则就是限制在Cartesian Coordinate System中的. 
(嗯, 没错, 在别的地方还会又一些不一样的地方, 可以看张量运算的地方. )

$$\vec{a} \times \vec{b} = a_i b_j \varepsilon_{i j k} \hat{e}_k \\ \vec{a} \cdot \vec{b} = a_i b_j \delta_{i j} \\ \varepsilon_{k i j} \varepsilon_{k m n} = \delta_{i m} \delta_{j n} - \delta_{i n} \delta_{j m} \\ a \times (b \times c) = b (a \cdot c) - c (a \cdot b)$$

其中还有一个Einstein Summation Convenrion, 就是对重复的指标进行求和, 
(在张量运算里面, 同时有上指标和下指标的进行求和), 比如: 

$$\delta_{i i} = \delta_{1, 1} + \delta_{2, 2} + \delta_{3, 3} = 3$$

对于$\nabla$算符, 可以通过分量的写法来做, 也可以直接通过算符整体的运算来做.

$$\nabla \cdot \boldsymbol{r} = 3 \\ \nabla r = \boldsymbol{r} \\ \nabla(\boldsymbol{A} \cdot \boldsymbol{r}) = \boldsymbol{A} \\ \nabla f(\boldsymbol{r}) = \frac{\mathrm{d} f}{\mathrm{d} \boldsymbol{r}}$$

(注: 最后的就是形式的求导. )

并且对算符的运算可以利用算符的矢量性和算符性来计算. 

#### 矢量微积分的例子
**物质导数**

$$\frac{\mathrm{d}\boldsymbol{v}}{\mathrm{d}t} = \frac{\partial \boldsymbol{v}}{\partial t} + \sum \frac{\partial \boldsymbol{v}}{\partial q_i} \frac{\mathrm{d} q_i}{\mathrm{d}t} = \frac{\partial \boldsymbol{v}}{\partial t} + (\boldsymbol{v} \cdot \nabla) \boldsymbol{v}$$

并且这里还有一个方向导数的概念: 

$$\frac{\partial f}{\partial \boldsymbol{a}} = (\boldsymbol{a} \cdot \nabla) f$$

**Navier-Stokes Equation**

$$\rho \frac{\mathrm{d} \boldsymbol{v}}{\mathrm{d} t} = - \nabla p + \mu \nabla^2 \boldsymbol{v} + \rho \boldsymbol{g} \\ \Leftrightarrow \rho (\frac{\partial \boldsymbol{v}}{\partial t} + (\boldsymbol{v} \cdot \nabla) \boldsymbol{v})= - \nabla p + \mu \nabla^2 \boldsymbol{v} + \rho \boldsymbol{g} $$

**Lamb Vector**    

$$\boldsymbol{L} = (\nabla \times \boldsymbol{v}) \times \boldsymbol{v} = \boldsymbol{\omega} \times \boldsymbol{v}$$

**伯努利方程**     
首先要知道一些成立条件: 
* Steady State 定常 $\frac{\partial \boldsymbol{v}}{\partial t} = 0$
* Inviscous 无黏 $\mu = 0$
* Conservative 有势 $\boldsymbol{g} = - \nabla (g z)$
* Uniform 均质 $\rho = \mathrm{const} \Rightarrow - \frac{1}{\rho} \nabla p = - \nabla (\frac{p}{\rho})$      
  
于是可以发现Navier-Stokes Equation
  
**$2\omega$的故事**     

#### Possion Bracket
1809年, 泊松提出了一种求导的顺序: 

$$[f, g] = \sum_k (\frac{\partial f}{\partial q_k} \frac{\partial g}{\partial p_k} - \frac{\partial f}{\partial p_k} \frac{\partial g}{\partial q_k})$$

(注: 在朗道的书里面, 泊松括号和正常的差了一个负号. )

对于Possion括号, 有一些基本的性质: 
* 常数为零: $[f, C] = 0$
* 反对称性: $[f, g] = - [g, f]$
  * $\Rightarrow [f, f] = 0$
* 链式法则: $[f_1 f_2, g] = f_1 [f_2, g] + f_2 [f_1, g]$
* 时间变化率: $\frac{\partial}{\partial t} [f, g] = [\frac{\partial}{\partial t} f, g] + [f, \frac{\partial}{\partial t} g]$        
  (不就是利用了偏微分求导的可交换性嘛...)
* $[f, [g, h]] + [g, [h, f]] + [h, [f, g]] = 0$      
  (像是轮换? )
* 泰勒展开
  * $f(t) = f_0 + [f, H] t + [[f, H], H] \frac{t^2}{2} + \cdots$
* 简单的几个结论: 
  * $[q_k, f] = \frac{\partial f}{\partial p_k}$
  * $[p_k, f] = \frac{\partial f}{\partial q_k}$
  * $[q_{\alpha}, p_{\beta}] = \delta_{\alpha \beta}$

#### Group Theory
对于旋转矩阵: 

$$\boldsymbol{A} = \left( \begin{array}{ll} \cos \theta & \sin \theta \\ - \sin \theta & \cos \theta \end{array} \right) \in SO(2)$$

## 历史部分
### 科学和哲学以及玄学(神学)
> All **definite** knowledge - so I should contend - belongs to science; 
> all **dogma** as to what surpasses definite knowledge belongs to theology. 
> But between theology and science there is a **No Man's Land**, 
> exposed to attack by both sides; this No Man's Land is philosophy.      
> *B.Russell, The History of Western Philosophy (ed. 1965)*

#### 西方哲学
* 哲学的框架: 
  * **Methodology** 方法论 - 哲学的枝叶
  * **Epistemology** 认识论 - 哲学的树干
    * **Empirrcalism** 经验主义     
      也叫英伦经验主义
      * John Locke (England)
      * George Berkeley (Ireland)
      * David Huume (Scotland)    
        提出了**因果律** (和欧几里德公里体系并称为*两千年来科学史最伟大的成就*)
    * **Rationalism** 理性主义
      * Rene Decartes (France)     
        > I think therefore I am. 
      * Spinoza (Netherlands)     
        每个人心中都有一个不同的God
      * Leibniz (German)    
        单子论提出者, 撬棍$\int$发明者    
        (*单子论*: 认为是事物最根本的原素, 不可再分, 没有时空延展性, 
        是抽象的存在, 即形而上粒子; *原子论*: 类似于原子概念的理论)
    * 两种的融合
      * Wolff, 将哲学分为*理论哲学*和*应用哲学*
      * Kant, *统一*经验主义和理性主义, **认为知识源于经验, 形成于理性**   
        提出了*二律背反*
  * **Ontology** 本体论 - 哲学的树根
* 现代哲学的起点: 笛卡尔的三个梦(风雪交加, 军营, 莱茵河畔)
* Philosophy 的第一次提出是由数学家 Pythagoras 提出的      
  > Pythagoraenism: "All is number. "       
  当Pythagoras完成了自己的这个定理的证明后, 就杀了100头公牛(oxen)来庆祝, 
  所以当公牛们开始发抖的时候, 有新的定理就得到了证明. (坊间笑话)
* Pythagoras 还有一个理论 *Music of the Spheres*, 认为天上的音乐一直都有, 
  但是人们听惯了却自动忽略了. Kappler受其影响, 写了 *Harmonies of the World*. 
  (coldplay 也有一个这个名字的专辑)
* Karl Max的论文*Democriyus vs. Epicurus*
* Guess对哲学的批判: 
  > 你在当代哲学家谢林, 黑格尔, 內斯·冯·埃森贝克和他们的追随者身上
  > 看到同样的东西 -- 数学上的无能; 他们的理论怎能不使你毛骨悚然? 
  > 读读古代哲学史中当时的大人物 -- 柏拉图和其他人(我把亚里士多德除外) -- 
  > 都提出了一些错误的理论. 甚至康德本人也不怎么样. 
  > 我认为他对分析命题和综合命题所作的区分, 要么是平凡不足道的, 
  > 要么是错误的.        
  > -- Guess 对 舒马赫的信

### 伽利略的故事合集
* 伽利略的科学方法
  * Deduction: 归纳演绎
  * Experiment: 和实验结合
  * Thought Experiment: 思想实验
  * Mathematics: 和数学结合
* 伽利略的科学哲学
  * Empirist 经验主义者
    > Whewell and Mach classified Galileo as empirist 
    > because of the drop test at the Leaning tower of Pisa. 
  * Rationalist 理性主义者
    > Galileo Galilei: Mathematics is the language in which 
    > God has written the Universe. 
    > so the French Alexander Koyré classified Galilei as rationalist. 
* 伽利略的所有子女都不是婚姻关系所生的

### 牛顿的故事合集
* **Newtonianism** and **Darwinism**    
  other than Newtonianism, the Darwinism thinks that the system is 
  **non-time-reversal**, **nonlinear** and is a **complex** system. 
* 牛顿力学为何只产生于天体而不是车行马走中?     
  模型更加的简单, (数学上的)更加和谐干净, 上帝视角和蚂蚁视角, 牛顿范式: 
  通过微分方程来研究, 并且研究对象比较少, 数学处理起来比较简单. 
  历史原因: Halley把自己的行星观测结果给了老牛, 
  说自己看不懂, 然后爵爷就脱口而出, "椭圆", 然后把自己解释计算过程的东西拓展, 
  最后写成了原理一书. 
* Emst Mach 在自己的著作*力学史评*中提出了$\boldsymbol{F} = m \boldsymbol{a}$, 
  这本书中重炮轰击*绝对时空观*, 对Einstein影响很大. 注意这里的$m$有引力质量, 
  惯性质量的区别. 
* 牛顿的学生是财政部长, 所以在牛顿写完了原理之后, 对物理学的兴趣减少了之后, 
  牛顿就当了皇家铸币厂厂长. (金本位就是他提出来的)
* 厂里的事都是他外甥女婿干的, 家里的事又是他外甥女干的, 
  现在很多的牛顿的事都是他外甥女(一个传记作者)写出来的
* "From the paw, I saw the lion. "   
  有一次伯努利想了一个很棒的题目来考牛顿, (最速降线), 
  然后在牛顿的外甥女的记载中: 下午五点, 牛顿爵士拖着疲惫的身体从厂里回来, 
  用完晚餐, 就开始做伯努利出的题, 凌晨三点钟才睡觉. 然后就把这封信寄了回去, 
  (匿名的), 然后伯努利就说了上面那句金句. 

### 欧拉的故事合集
* Euler's Master Degree Papers: 
  *Comparison between the Philosophies of Decartes & Newton*
* Euler vs Humme
  > Sir, since ... (据说是一个数学公式), hence the god exists, say. 
* Euler提出了对微元体的定义:     
  可以说就是这样的微元体的有点像是哲学思辨的思想才产生了流体力学的一部分基础. 
  * 宏观无穷小
  * 微观无穷大

### 拉格朗日故事集
* 19岁, Professor
* Before 20, wrote a letter to Euler, showing his ambition: 
  **One principle to unit solid and fluids.**
* 他分析力学中没有一张图片, 受到Kant的影响, 认为要贯彻自己分析力学的本体
  > He owned no his phliosophy, he was deeply affected by Kant. 
* 分析力学的思想还来自Johann Bernouli的Vitrual Velocity的思想, 
  提出了Principle of Virtual Work
* Euler 是个好老师, 为了吸引Lagrange的兴趣, 把自己的很多发现都压下不发表, 
  留给Lagrange来做. 
* 23岁时, 受Euler推荐做了柏林科学院的外籍院士
* 庆幸自己家里破产了, 不然就学不成数学了
* Legendre 帮助 Lagrange出版了他的书, 甚至动用自己的社会力量, 
  请巴黎的马里神父出版了分析力学. 
  虽然出版了书之后Lagrange就对自己的书也不在意了. 
* 1792年, 丧偶9年的Lagrange同天文学家勒莫尼埃的女儿
  Renée·Francoise·Adelaide结婚, Adelaide同情他的遭遇, 执意要和他结婚, 
  并一直矢志不渝, 所以Lagrange才开始重新着手分析力学的修改. 

### 薛定谔故事合集
* 刚到维也纳大学, 一周上课11学时, 特别是周二最忙
* 上课有一门就是分析力学
* 和Feynmann一样, 手很快, 想明白了就不把问题给学生了, 所以据说当他研究生就很惨
* 明人不做暗事, 薛定谔和情人出去绝不掩饰
* 薛定谔关于自己获得诺贝尔奖的说法: 
  (对Itha Junger说的, Itha Junger14岁和薛定谔相遇? )
  > I didn't write everything down at once, I kept changing here and there
  >  until finally I got the equation. When I got it, 
  > I knew I got the Norble Prize. 

### 爱因斯坦的故事合集
* 被老师Minkowski批评为"lazy dog", 因为他老是逃课
* 但是Einstein的逃课是为了学电磁学, 所以在他本科毕业了之后, 
  对麦克斯韦方程有惊人的认识
* 逃课还有一个原因是因为他认为Weber老师上的课too old fashioned
* Einstein本科毕业了之后没有工作, 只好靠自己的同学的父亲在专利局干活, 
  收入很低, 抽的烟也很烂

### 数学的三大主义
* 直觉主义 **Intuitionism**: Hermann Weyl     
  Brouwer认为数学是一定可以被构造的, 在博士论文中提出了直觉主义; 
  Hilbert为他写了推荐信, 但是后来发现他和自己的完全不一样, 
  甚至还把自己的学生Weyl骗走了, 于是打算把他撤职; 
  Weyl调和了两个思想. 
* 形式主义 **Formalism**: David Hilbert     
  Gödel Incomplereness Theorem否定了形式主义
* 逻辑主义 **Logicism**: Russel

其他的数学主义: 
* **Mathematical Platonism** 数学柏拉图主义 (Penrose提出)     
  有点像是"地球没了你也照样转", 认为存在绝对的数学世界     
  类似的有Karl Popper的Three World Theory: 有三个世界, Physical World, 
  Subjective World, Objective World. 从单纯的唯心唯物的二分(笛卡尔), 
  到了加入客观知识的三分(康德). 
* **Conventionalism** 约定主义: Henri Poincaré    
  客观性的标准也是建立在主观公理的基础上, 就是人为规定的convention, 
  对于这样的公理约定, 要满足不和公认的事实矛盾.     
  虽然一开始约定主义是来源于政治的: 
  * 伊壁鸠鲁: 在肯定感觉主义的基础上继承并发展德漠克利特的原子说, 
    强调了人们在感觉, 经验方面相互约定的必要性, 进一步把约定的思想推广到政治, 
    道德, 法律等领域(后代社会契约论)
  * 哲学: 人类社会越发展, 自然关系在人身上的作用就越淡化, 
    人就越成为社会的存在物. 
    > 个人只有作为交换价值的生产者才能存在, 而这种情况就已经包含着
    > 对个人的自然存在的完全否定, 因而个人完全是由社会决定的.     
    > 马克思在分析资本主义关系中的个人
  * 数学: 主要体现是Poincaré主导的(前)直觉主义
    > He held that axioms in geometry should be chosen for the results 
    > they produce, not for their apparent coherence with - possibly flawed - 
    > human intuitions about the physical world.

对待数学的几种态度
* 本质主义
* 科学实在论
* 工具主义

### Gödel 不完备性定理
完备性和一致性不可兼得. 

注记: 
* Heisenberg Uncetainty
* Niels Bohr Complementarity
* Self-Reference Paradox 自我指涉的悖论

关于Gödel 不完备性定理的第一条有三种证明: 
* 模型论 - 塔尔斯基
* 证明论 - 罗德尔 · 罗瑟
* 递归论 - 丘奇 · 图灵    
  递归论的方法我应该可以理解, 主要的

关于Gödel: 
* John Wheeler有一段时间关注Heisenberg Uncetainty和Gödel Incompleteness的
  两个理论的联系(其中一个给出了定量的不可兼得的程度, 另外一个则没有), 但是, 
  当他跑去问Gödel的时候(当时Gödel在屋子里面, 腿上盖着一条毛毯), 听到之后, 
  立刻大怒: "Get Out!" (据说是因为当时Einstein很烦量子力学, 每天和Gödel散步时, 
  就把Gödel washbrained了)
* Gödel年轻的时候娶了一个比他大六岁的舞女, 晚年只信任自己的老婆
  (只吃老婆做的饭, 但是他老婆身体也不是很好, 所以也有说他是饿死的)
  和王浩(华人里最厉害的逻辑学家, 为Gödel写了传记)
* Einstein 享受和Gödel一起上下班聊天的快乐
  
### 叔本华的哲学
> 人生实如钟摆, 在痛苦与倦怠中徘徊.     
> Schopenhauer's Pendulem

* "艺术是人们逃离痛苦的途径之一"
* 被评价为无家可归者, 悲观主义者, 悲剧主义的代表
* 叔本华信奉古印度哲学, 吠檀多学派
* 和黑格尔的争斗: 故意选择和黑格尔相同的上课时间
* 薛定谔非常佩服叔本华
  * 薛定谔后期, 说自己虽然是个物理学家, 但是除了上课, 其他时间都在搞哲学. 
    写了一本哲学小书: *What is Life?*, 利用了负熵的概念, 第一次定义了死亡: 
    上趋于无穷大就是死亡
  * 叔本华很喜欢狗(named Atma), 在自己的书中有猫的存在, 于是影响了薛定谔, 
    这就是薛定谔的猫的来历(不知道); 也有说是因为用猫更加受欢迎. 
* 叔本华"骂"狗: "You are not a dog! You are a human! a human!"

### Quantum Computer
> Nature isn't classical, damnit, 
> and if you want to make a Simulation of nature, 
> you'd better make it quantum mechanical, and by golly, 
> it's a wonderful problem because it doesn't look easy.     
> Richard Feynman, 1981, First Conf. Physics and Computation, MIT


### 杂项
* Heisemberg原来是搞流体力学的, 博士论文解不出来, 最后只好猜了一个解
  (后来这个解被林家翘(导师:冯 · 卡门)证明了)
* 狄拉克原来是搞EE的, "工程学对我最大的启发: 工程学能够容忍误差"
* **the Feynman principle**: do not fool yourself
  > The first principle is that you must not fool yourself, 
  > and you are the easiest person to fool
* 老玻尔也干了很多的糊涂的事, 比如曾经就提出了动量不守恒的猜测BKS   
  > If so(BKS), I'd rather be a cobbler or even am employee 
  > in a gambling house than a physics.     
  > Einstein对BKS的反对
* Einstein 也有错误地时候, 晚年认为引力波不存在, 结果被退稿了, 
  于是就对审稿人大发雷霆, "我给你们发稿子是让你们发表的, 不是让你们审稿的. "
  审稿人说, 这是为了保留他的名誉. (编辑的重要性)
* 物理学的梗: 
  > No idea from Lifshitz, No words from Landan. (朗道力学的梗)
* 社会静力学: Comte, 弹性: 偏离了正常秩序后恢复正常的能力
* Spencer: *Social Statics*, *First Pinciples*, 
  认为人生最大的意义就是为了得到最大的幸福
  > the greatest happiness of greatest number      
  (注: 我觉得老师课上的这个概括不是特别的到位, 因为我看了书的一小部分, 
  里面虽然一开始就提出了这个概念, 但是随之就开始了思考: 什么是幸福? 
  因为幸福是一个对不同的人有不同的答案的问题. 所以我认为, 我需要继续读下去. )
* 经济物理学: Ecopnohysics:Stanley
* 心理学中的场论
* 历史微积分: 托尔斯泰
  > 只有采取无限小的观察单位 -- 历史的微分, 并运用积分的方法得到这些无限小的
  > 总和, 我们才能得到问题的答案 -- 历史的规律, 正是这种微积分, 
  > 纠正了人类由于只观察个别单位所不能不犯下的和无法避免的错误.      
  > *战争与和平*
* 世界科学中心的转移: 意大利 -> 法国 -> 英国 -> 德国 -> 美国, 日本     
  世界科学中心需要有**学术大师**和**教育大师**

## 其他部分
### 目标
再一次提一下这个学习的目标: 
* social responsibility
* leader education
* liberal arts
* philosophy of science
* critical thinking

胡适的君子标准: 
* 不降志(屡败屡战)
* 不辱身
* 不追求时髦
* 也不回避危险

### 后记
以上的内容, 几乎完全没有在考试中出现, 笑. (哭)

就是挺迷惑的. 