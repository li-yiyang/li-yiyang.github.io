---
layout: post
title:  "Static Physics"
date: 2022-10-26 15:21:13 +0800
math: true
categories: notes
---
# Static Physics
上半个学期基本上在讲热力学, 虽然上学期刚学热学, 但是仍然学得跟狗屎一样. 

## Five Postulate
1. Equilibrium States are completely and uniquely specified
   by $U, V, N$ macroscopically.  
   通过 $U, V, N$ 三个宏观量来描述平衡态. 
2. Fundamental Equation: $S(U, V, N)$.  
   基本方程 $S(U, V, N)$, 通过最大熵定理来得到稳定态关系. 
3. $S$ is extensive, $S$ is continuous and differentiable 
   w.r.t $U, V, N$, $S$ increase monotonically with $U$.  
   $S$ 是广延量, 及 $S(\lambda U, \lambda V, \lambda N) = \lambda S$,
   系统的 $S$ 为各组分的 $S_i$ 的和. $S$ 对 $U, V, N$ 连续可导, 
   且 $\frac{\partial S}{\partial U} > 0$
4. A macroscopic system samples every permissible microstate 
   with **equal probability**.  
   宏观状态是等可能微观状态的和. 
5. $$(\frac{\partial U}{\partial S})_{V,N} \rightarrow 0 \Rightarrow S \rightarrow 0$$.  
   实际上就是 $T = (\frac{\partial U}{\partial S})_{V,N} \rightarrow 0$ 时,
   熵消失. 

## Fundamental Equations
### Intensive Parameters
将公式变形成: $U(S, V, N)$. 然后有: 

$$ \left\{\begin{array}{lll} (\frac{\partial U}{\partial S})_{V, N} & \equiv & T\\ -(\frac{\partial U}{\partial V})_{S,N} & \equiv & P\\ (\frac{\partial U}{\partial N}) & \equiv & \mu_j\end{array}\right.$$

为强度量: $T(\lambda S, \lambda V, \lambda N) = T(S, V, N)$ 等. 
可以将公式变形成微分形式: 

$$\mathrm{d} U = T \mathrm{d} S - P \mathrm{d} V + \mu \mathrm{d} N = \mathrm{d} Q + \mathrm{d} W_M + \mathrm{d} W_C$$

对单位组分 $u = U/N, s = S/N, v = V/N$, 有: 

$$\mathrm{d} u = T \mathrm{d} s - P \mathrm{d} v$$

(Note: 通过 $u(s,v)$ 来求 $U(S,V)$, 
只需要把 $u = u(s,v) \Rightarrow U/N = u(S/N, V/N)$ 做替换即可. )

这一方法可以写成抽象的定义: 

对于广延量的基本方程 $Y(X_0, X_1, \dots)$, 其中$Y, X_i$ 均为广延量. 
其一阶导数为强度量, $F_i = \frac{\partial Y}{\partial X_i}$. 

**关于平衡时的强度量**: 
* 温度: 
  * 两系统相互达到平衡, 则有温度相等的结果:  
	显然, 平衡的时候有最大熵: 
	
	$$\mathrm{d} S = 0 \Rightarrow (\frac{\partial S^{(1)}}{\partial U^{(1)}} ) \mathrm{d} U_1 + (\frac{\partial S^{(2)}}{\partial U^{(2)}} ) \mathrm{d} U_2 = 0 \Rightarrow T^{(1)} = T^{(2)}$$
	
  * 同理, 也能够得到能量传递方向从高能量到低能量的结论:  
	有熵增: 
	
	$$\Delta S \simeq (\frac{1}{T^{(1)}} + \frac{2}{T^{(2)}}) \Delta U^{(1)} \Rightarrow T^{(1)} > T^{(2)} \Leftrightarrow \Delta U^{(1)} < 0$$
	
	其中使用了 $\Delta U^{(1)} = \Delta U^{(2)}$ 的结论. 
* 力学量: (压强)  
  证明方法同上: 
  
  $$ \mathrm{d} S = (\frac{1}{T^{(1)}} - \frac{1}{T^{(2)}})\mathrm{d} U + (\frac{P^{(1)}}{T^{(1)}} - \frac{P^{(2)}}{T^{(2)}}) \mathrm{d} V^{(1)} \Rightarrow P^{(1)} = P^{(2)} $$
  
* 化学量:  
  
  $$\mathrm{d} S = (\frac{1}{T^{(1)}} - \frac{1}{T^{(2)}})\mathrm{d} U - (\frac{\mu_1^{(1)}}{T^{(1)}} -\frac{\mu_1^{(2)}}{T^{(2)}}) \mathrm{d} N_1^{(1)} \Rightarrow \mu_1^{(2)} = \mu_1^{(1)}$$

也能够类似地得到差不多的一般结论: 

#### Eular Equation
对于广延量有 
$Y(\lambda X_0, \lambda X_1, \dots) = \lambda Y(X_0, X_1, \dots)$ 结论, 
于是利用欧拉公式 (大概叫这个): 

$$\frac{\partial Y(\lambda X_i)}{\partial \lambda} = Y(X_i) \Rightarrow Y(X_i) = X_i \frac{\partial Y(\lambda X_i)}{\partial \lambda X_i} \overset{\lambda = 1}{\Rightarrow} Y(X_i) = X_i \partial_{X_i} Y$$

(其中省略了求和, 用了一些简化注记, 方便我输入. )

这样有什么用? 答案是这样虽然在结果上非常的没有用, 但是可以把
广延量和强度量联系在一起 (而不是只是微分的形式) 如:

$$U = TS - PV + \mu N, S = \frac{1}{T} U + \frac{P}{T} V - \frac{\mu}{T} N$$

#### Gibbs-Duhem Relation
$$S \mathrm{d} T - V \mathrm{d} P + N \mathrm{d} \mu = 0,\ \mathrm{or}\ \mathrm{d} \mu = -s \mathrm{d} T + v \mathrm{d} P$$

推导的方式就是利用欧拉公式和广延量的公式即可. 
一般性的结论: 

记 $P_i = \partial_{X_i} Y$ (这个 $P_i$ 的记法, 
不知道会不会让人想到广义动量, 毕竟在形式上这个和广义动量那么像), 
那么在欧拉公式的基础上: 

$$Y = X_i P_i \Rightarrow \mathrm{d} Y = P_i \mathrm{d} X_i + X_i \mathrm{d} P_i$$

而又有一般的微分表达式: $\mathrm{d} Y = P_i \mathrm{d} X_i$

于是不难得到 $X_i \mathrm{d} P_i = 0$

### Legendre Transformations
一般的结论是这样的: 
$Y(X_i) \Rightarrow \psi(X_0, \dots, P_i, \dots X_n) = Y - P_i X_i$,
即对于一个基本方程 $Y(X_i)$, 通过 Legendre 变换可以将其变成和强度量
$P_i$ 表示的方程. 

(在力学里面的 Legendre 变换: $L(q, \dot{q}) \rightarrow H(q, p) = p q - L$)

一个一般的证明: 

首先规定: 

$$Y = Y(X_0, X_i), P_k = \frac{\partial Y}{\partial X_k}$$

$$P = \frac{Y - \psi}{X - 0} \Rightarrow \psi = Y - P X, \mathrm{d} \psi = - X \mathrm{d} P$$

于是就类似于力学里面的 $L \rightarrow H$ 的关系了. 

(注: 用 $Y[X_i]$ 来表示 $\psi = Y - P_i X_i$ 的一个变化. 方便记. )

一般的结论可能过于抽象了点, 下面是热力学里面的各种结论: 

**Thermodynamic Potentials**: 

$$F = U[T] \Rightarrow \mathrm{d} F = - S \mathrm{d} T - P \mathrm{d} V + \mu \mathrm{d} N$$

$$H = U[P] \Rightarrow \mathrm{d} H = T \mathrm{d} S + V \mathrm{d}P + \mu \mathrm{d}N$$

$$G = U[T,P] \Rightarrow \mathrm{d} G = - S \mathrm{d} T - P \mathrm{d} V + \mu \mathrm{d} N$$

$$\mathrm{d} U = T \mathrm{d} S - P \mathrm{d} V + \mu \mathrm{d} N$$

这四个势函数就是之后反复捣腾的东西, 总之之间的关系比较重要, 需要记住. 

**Generalized Massieu Functions**: 

同样的方法, 应用到熵函数上可以得到: 

$$S[\frac{1}{T}] = S - \frac{1}{T} U = - \frac{F}{T}$$

$$S[\frac{P}{T}] = S - \frac{P}{T} V$$

$$S[\frac{1}{T}, \frac{P}{T}] = S - \frac{1}{T} U - \frac{P}{T} V = - \frac{G}{T}$$

等等. (注: 这里应该需要找几个例子去继续了解一下. )

#### Extremum Principle
**Helmholtz Potential Minimum Principle**:  
The equilibrium value of any unconstrained internal parameer in
a system in diathermal contact with a heat reservoir minimizes
the Helmholtz Potential over the manifold of states for which $T = T^r$.

**Enthalpy Minimum Principle**:  
The equilibrium value of any unconstrained internal parameter in a 
system in contact with a pressure reservoir minimizes the enthalpy
over the manifold of states of constant pressure (equal to that of
the pressure reservoir). 

**Gibbs Potential Minimum Principle**:  
The equilibrium value of any unconstrained internal parameter in a 
system in contact with a thermal and a pressure reservoir minimizes
the Gibbs potential at constant temperature and pressure. 

一般性的结论: 如果在强度量 $P_i$ 一定的情况下, 那么 $U[P_i]$
在平衡的时候达到最大值. 证明方法: 

令热源 (reservoir) $U^r$, 有 $P_i^r$, 于是对于考虑的对象和热源组成的系统, 
在平衡的时候应该有: 

$$\mathrm{d}(U + U^r) = \mathrm{d} U - P^r_i \mathrm{d}X^r_i = 0 \Rightarrow \mathrm{d} (U - P^r_i X^r_i) = 0 \Rightarrow \mathrm{d} U[P_i] = 0$$

(注意, 这里用了平衡的时候, $P_i = P^r_i$ 相等的结果. )

同理, 利用 $\mathrm{d}^2 U^{total} > 0$, 也能够得到类似的结论. 

于是上面的那些最小势的结论就变成了: 
* $T \equiv T_0 \Rightarrow \mathrm{d} F = 0$
* $P \equiv P_0 \Rightarrow \mathrm{d} H = 0$
* $T, P \equiv = T_0, P_0 \Rightarrow \mathrm{d} G = 0$

(Note: 同理, 也能够有对 Massieu Functions 的结论. 不过是 $S$ 最大理论. 
因为有结论, 当 $S$ 极大时, $U$ 极小. )

#### Potential and Works
上面那三个都是势能嘛, 所以他们的改变量就和能量挂钩. 

* $T \equiv T_0:\ \mathrm{d} W_{RWS} = - \mathrm{d} F$
* $P \equiv P_0:\ \mathrm{d} Q = \mathrm{d} H$
* $T, P \equiv T_0, P_0:\ \mathrm{d}G = \nu_j \mathrm{d} \tilde{N}$, 
  其中: 化学式为 $0 \rightleftarrows \sum\nu_j A_j$, 
  平衡的时候应该有: $ \frac{\mathrm{d}N_i}{\nu_i} = \mathrm{d} \tilde{N}$

## Maxwell Relations
又是一个大头项目. 我老是记不住这里的东西. 太容易忘记了. 

基本的原理是这样的, 利用偏导数的可交换性得到: 

$$\frac{\partial^2 U}{\partial X_i \partial X_j} = \frac{\partial^2 U}{\partial X_j \partial X_i} \Rightarrow \frac{\partial P_i}{\partial X_j} = \frac{\partial P_j}{\partial X_i}$$

这里用的一个条件就是 $\mathrm{d} U = T \mathrm{d} S - P \mathrm{d} V + \dots$.

> Note: 如何记忆 Maxwell 关系:  
> 虽然书中给了一个绝妙的办法, 但是时间来不及了, 所以, 
> 我的记法是: 
> 
> $$(\frac{\partial P_i}{\partial P_j})_{X_i} = (\frac{\partial X_j}{\partial X_i})_{P_j}$$
> 
> 即 Maxwell 关系可以将强度量和广延量的微分关系进行一个替换. 

利用 Maxwell 关系可以将微分关系用前面的 $c_P, \kappa$ 等的东西来表示. 
是一个比较重要的部分. 主要的操作是这样的: 

$$\begin{array}{llll} (\frac{\partial X}{\partial Y})_Z & = & 1/(\frac{\partial Y}{\partial X})_Z & (a)\\ (\frac{\partial X}{\partial Y})_Z & = & (\frac{\partial X}{\partial W})_Z / (\frac{\partial Y}{\partial W})_Z & (b)\\ (\frac{\partial X}{\partial Y})_Z & = & -(\frac{\partial Z}{\partial Y})_Z/(\frac{\partial Z}{\partial Z})_Z&(c)\end{array}$$

1. 如果表达式中有势能项 ($U, F, H, G$), 就把他们拉到分子的位置上: 
   对于在分子位置上的势能, 利用 $Y = P_i \mathrm{d} X_i$ 来展开成微分形式,
   要注意 $Z$ 位置规定了那些量是常数, 也就是 $\mathrm{d}Z$ 为零. 
   * 比如在 $Y$ 处, 使用方法 $(a)$, 
	 于是就是计算 $(\frac{\partial Y}{\partial X})_Z$
   * 比如在 $Z$ 处, 使用方法 $(c)$, 
	 于是就是分别计算两个 $\partial Z$ 的项即可
   * 比如在 $X$ 处, 直接就解决了
2. 如果表达式里面有化学式 $\mu$, 那么将其移动到分子的位置上 (方法同 1 ), 
   再用 Gibbs-Duhem 换成 
   $\mathrm{d} \mu = -s \mathrm{d} T + v \mathrm{d} p$
3. 如果表达式里面有熵 $S$, 将其移动到分子的位置上. (方法同 1 )
   如果能用 Maxwell 关系化简的话, 就可以化简了; 
   反之, 如果不能的话, 那么就在 $\partial S$ 下面的分母加上一个
   $\partial T$ (方法为 $(b)$), 这样的话, 就能够得到 $c_V$ 或 $c_P$ 了. 
4. 如果表达式里面有体积 $V$, 将其移动到分子的位置上 (同1),
   于是应该可以得到 $\alpha$ 或者 $\kappa_T$. 
5. 到此为止, 应该能够得到 $c_V, c_P, \alpha, \kappa_T$ 的表达形式. 
   然后用 $c_V = c_P - T v \alpha^2/\kappa_T$ 来替换 $c_V$, 
   最终就能够得到 $c_P, \alpha, \kappa_T$ 的表达式了. 

> Note:  
> 上面这样操作的原因应该是因为二阶导数只和 $T, V, P, Q$ 
> 即一些常见的可测量量有关. 上面的操作都是如何将势能量通过拆微分的方法, 
> 移动到分子上来化简, 或者是利用 Gibbs-Duhem 公式来去掉 $\mu$ 
> 这种没法化简的, 或者是把 $S$ 当作 $Q$ 来用的.  
> 所以记不清做法的话, 就往这个方向这样努力吧... (悲)

### Second Derivatives
强度量都是一阶导数, 考虑二阶导数的量有: 

$$\left\{\begin{array}{lllll} \alpha & = & \frac{1}{v} (\partial_T v)_P & &\\ \kappa & = & -\frac{1}{v}(\partial_P v)_T & &\\ c_P & = & T(\partial_T s) & = \frac{T}{N} (\partial_T S)_P & = \frac{1}{N} (\mathrm{d}_T Q)_P\end{array}\right.$$

## Examples
### Temperature Unit

$$R = N_A k_B = 8.3144 J/mole \cdot K$$

### Ideal Gas and Others
#### Ideal Gas
理想气体有状态方程和内能方程: 

$$PV = NRT, U = cRT$$

想要求 Fundamental Equation, 即 $S = \cdots$. 
使用欧拉公式得到的 $U = T S - P V + \mu N$, 
于是得到 $S = \frac{1}{T} U + \frac{P}{T} V + \frac{\mu}{T} N$. 

利用状态方程和内能方程可以得到 $\frac{1}{T}, \frac{P}{T}$, 
唯一需要算的就是 $\frac{\mu}{T}$, 认为是 $u,v$ 的函数 (显然). 
然后就能够用 Gibbs-Duhem 方程来计算
$\mathrm{d}(\frac{\mu}{T}) = \frac{\mathrm{d} \mu}{T} = \cdots$ 

最终得到的结果如下: 

$$S = N s_0 + N R \mathrm{ln} (\frac{U}{U_0})^c(\frac{C}{V_0})(\frac{N}{N_0})^{-(c+1)}, s_0 = (c+1)R - (\frac{\mu}{T})_0$$

整体的思路是这样的: 
* 因为状态方程和内能方程中是用强度量 $P_i$ 来描述的, 
  所以想法就是利用欧拉公式来计算
  $S = \frac{1}{T} U + \frac{P}{T} V + \frac{\mu}{T} N$. 
* 计算过程中, 因为化学量不知道, 但是知道 $T,P$, 
  所以用 Gibbs-Duhem 公式来计算 $\mu$. 
  
**理想气体的混合熵**:  
虽然混合熵有一个非常农民的解释方法: 

> Taking a single component process of free expansion:
> 
> Before expansion, the its initial micro distribution number $\Omega$ is
> $\Omega_i$, its micro distribution number of final state is $\Omega_f$.
> Consider the volume is divided into $n$ parts, a particle can choose to stay
> in space $i = 1, 2, \ldots, n$. Thus all the possible distribution should be
> $\underbrace{\{ 1, \ldots, n \} \times \cdots \times \{ 1, \ldots, n
> \}}_{n_j}$. (in which the $n_j$ is the number of particles. )
> 
> The $\Delta S = k \ln \frac{\Omega_f}{\Omega_i} = - k \ln
> \frac{\Omega_i}{\Omega_f}$, taking that $\frac{\Omega_i}{\Omega_f} = P (N_j
> \ \mathrm{particles}\ \mathrm{in}\ \mathrm{space}\ i) = \left( \frac{V_i}{V}
> \right)^{n_j} \Rightarrow \Delta S_j = - k n_j \ln \frac{V_i}{V}$.
> 
> Therefore, the after the expansion $S_j' = S_j + \Delta S_j$.
> 
> The mixing progress thould be $S_{\mathrm{mixing}} = \sum S_j' = \sum S_j +
> \sum \Delta S_j$, $\sum \Delta S_j = - \sum k n_j \ln \left( \frac{V_j}{V}
> \right) = - \sum R N_j \ln \frac{V_j}{V} \overset{\ast}{=} - \sum R N_j \ln
> \frac{N_j}{N}$.

(摘自我当时的笔记. 想法就是利用同等体积的混合不会产生熵变的前提, 
然后利用熵是态函数, 构造一个先膨胀成等体积, 然后再混合的过程来计算熵变. )

#### Ideal Van Der Waals Fluid
理想范德瓦尔斯气体状态方程: 

$$P = \frac{RT}{v-b} - \frac{a}{v^2}$$

最终计算的结果应该是: $S = NR \mathrm{ln} ((v-b)(u+a/v)^c) + Ns_0$. 

> 计算过程:  
> * 由状态方程可以得到
>   $\frac{P}{T} = \frac{R}{v-b} - \frac{a}{v^2} \frac{1}{T}$
> * 尽管不能够像理想气体一样得到 $\frac{1}{T}$, 
>   但是可以认为 $\frac{1}{T} = f(u,v)$
> * 于是 $\mathrm{d} s = \frac{1}{T} \mathrm{d} u + \frac{P}{T} \mathrm{d} v$
>   然后利用二阶偏导数的交换性来计算出 $f$ 的表达式

#### Electromagn Radiation
有状态方程和势函数方程: 

$$U = bVT^4, P = \frac{u}{3V}$$

同上, 能够得到 $S = \frac{1}{T} U + \frac{P}{T} V \Rightarrow S = \frac{4}{3} b^{1/4} U^{3/4} V^{1/4}$. 

#### Rubber Band
弹性绳的状态方程和势函数: 

$$\mathcal{J} = bT \frac{L-L_0}{L_1-L_0}, U = cL_0T$$

于是还是利用欧拉方程, 能够得到 $S = S_0 + cL_0 \mathrm{ln} \frac{U}{U_0} - \frac{b}{2(L_1-L_0)} (L-L_0)^2$. 

#### Magentic Systems
有状态方程和势函数: 

$$B_e = (\frac{\partial U}{\partial I})_{S,V,N}, U = TS - PV + B_e I + \mu N$$

注意到这时候有新的一个强度量: $B_e$, 以及与其对应的广延量 $I$. 
所以在前面的 Gibbs-Duhem 方程里面就要有: 

$$S \mathrm{d} T - V \mathrm{d} P + I \mathrm{d} B_e + N \mathrm{d} \mu = 0$$

这样的话, 就能够将一些量代掉. 最终的结果应该是: 

$$U = NRT_0 \mathrm{exp}(\frac{S}{NR} + \frac{I^2}{N^2I_0^2})$$

> Note:  
> 一个简单的结论是这样的, 因为状态方程和势函数一般是能够被测量的, 
> 而熵函数 (基本方程) 则不是这样的. 为了从可测量量得到熵函数, 
> 方法就是通过上面的欧拉方程来得到熵函数.  
> 或者... 可以考虑用别的方法来, 比如 Maxwell 关系之类的. 

## 热机
#### Reversible Progress
可能需要了解一些热力学过程, 以及对应的一些参数的意义. 

(比如 $\gamma = \frac{c_P}{c_V} = \frac{s+2}{s}$, 经常忘. )

#### Maximum Work Theorem
由冷源 (**R**everible **H**eat **S**ource) 和热源形成一个系统, 
对外界 (**R**everible **W**ork **S**ource) 做功. 

首先有能量守恒: 

$$\mathrm{d} U + \mathrm{d} Q_{RHS} + \mathrm{d} W_{RWS} = 0$$

然后是有熵增条件: 

$$\mathrm{d} S_{tot} = \mathrm{d} S + \frac{\mathrm{d} Q_{RHS}}{T_{RHS}} \geq 0$$

(Note: 这里的 $S_{tot}$ 即系统的整体的熵, 是由冷源和热源的熵变的和. )

于是就能够得到最大功的表达式了.

#### Carnot Cycle and Other Cycles
emmm... 其实不同的循环不过就是不同的过程而已. 比较麻烦的就是如何计算. 
如果用热学里面的方法直接暴力积分的话, 可能会非常难算. 
不过已经知道熵函数的话: 

就能够用前面的 $\Delta W = T_{RHS} \Delta S - \Delta U$. 

(Note: 前面的 $S, T, U$ 这些都是表示的是工作介质的热力学量. 
但是考虑的时候, 是考虑冷源和热源组成的系统的熵变. )

**热机效率**

$$\varepsilon_e = 1-T_c/T_h$$

* Otto 循环:  

  $$\varepsilon_{Otto} = 1 - (\frac{V_B}{V_A})^{\frac{c_p - c_v}{c_v}}$$

* Brayton 循环:  

  $$\varepsilon_e = 1 - (\frac{P_A}{P_B})^{\frac{c_p - c_v}{c_p}}$$

#### Inversible Progress

### Chemical Reaction
首先是化学反应的平衡条件: 

$$0 \rightleftarrows \sum_j \nu_j A_j \Rightarrow \sum_j \mu_j \nu_j = 0$$

#### Chemical Reactions in Ideal Gases

$$\mu_j = RT[\phi_J(T) + \mathrm{ln} P + \mathrm{ln} x_j]$$

其中 $\mu_j$ 为一个化学式, $\phi$ 为温度函数, $x_j$ 为摩尔分数. 

**Equilibrium Constant** 平衡常数: 

$$ \mathrm{ln} K(T) \equiv - \sum_J \nu_j \phi_j (T)$$

$$ \frac{\mathrm{d} H}{\mathrm{d} \tilde{N}} = - T \frac{\partial}{\partial T} (\sum v_j \mu_j)_{P, N_1, N_2} = R T^2 \frac{\mathrm{d}}{\mathrm{d} T} \mathrm{ln} K(T)$$

(啊... 实在是不太会, 我不会化学啊!!! 这就是高中化学的背刺么... )

### Stability of Thermodynamic Systems
对于稳定的热力学系统, 应该有: (对于单个量的变化)

$$(\frac{\partial^2 S}{\partial X_k^2})_{X_i} \leq 0 \Leftrightarrow S(X_k + \Delta X_k) + S(X_k - \Delta X_k) \leq 2 S(X_k)$$

即 $\delta S = 0, \delta^2 S \leq 0$ 的一个最大熵的平衡. 

对于多个量的变化, 用的是多元函数的判断方法, 即那个很难记的 Henssin 矩阵. 
(或者用 $f(\boldsymbol{X} ) = \sum (-1)^k \frac{(\delta \boldsymbol{X} \cdot \nabla)^n f}{n!}$). 

(只能希望不会考到了就是了. )

> 两个的情况:  
> 
> $$ \frac{\partial^2 Y}{\partial X_i^2}\frac{\partial^2 Y}{\partial X_j^2} - \frac{\partial^2 Y}{\partial X_i \partial X_j} \geq 0$$

同样的方法也能够应用到势最大问题上面. 

### Phase Transitions
#### First Order Phase Transition
一阶相变, $G$ 不连续. 

> Note: 二阶相变, $G'$ 不连续 

**Latent Heat**  
潜热, 就: 

$$l_{LS} = T (s^{(L)} - s^{(S)}) = T \Delta s = \Delta h$$

**潜热的变化**  
* Clapeyron Equation  
  
  $$\frac{\mathrm{d} P}{\mathrm{d} T} = \frac{l}{T \Delta v} $$

  考虑在相变界面上的两个点: $A, B$, 在这个点上, 
  化学式发生了不连续的变化, 即 $\mu_P \rightarrow \mu_P'$. 
  在相变的时候, 应该有: $\mu_P = \mu_P'$.   
  证明的方法就是利用 Gibbs-Duhem 方程: 
  $\Delta \mu = v \mathrm{d} P - s \mathrm{d} T$, 
  然后 $\Delta \mu = \mu_B - \mu_A = \mu_B'-\mu_A'$  
  于是得到 $\frac{\mathrm{d} P}{\mathrm{d} T} = \frac{l}{T \Delta v} $. 

## Exercise
* Fundamental Equations

  > 检测 $S(U, V, N)$ 是否合理, 基本上就是在检测: 
  > * $S(\lambda U, \lambda V, \lambda N) \overset{?}{=} \lambda S$
  > * $\frac{\partial S}{\partial U} \overset{?}{>} 0$
  
  > 通过 $S(U, V, N)$ 来计算: 
  > * $U, V, N$ 就直接化简即可, 如果知道表达式的话
  > * $T, P, \mu$ 通过对 $U$ 进行一次求导  
  >   并且没准还能够通过这样的方式来计算得到状态方程.  
  >   (Note: 如果只有 $T, P$ 的话, 可以用 Gibbs-Duhem 公式来计算出 $\mu$, 
  >   如: $\mathrm{d}\mu = -T \mathrm{d}s + P \mathrm{d} v$, 然后积分, 
  >   得到结果. )

  > 计算得到基本方程: 
  > * 知道 $T(S,V,N), P(S,V,N), \mu(S,V,N)$ 或者
  >   $T(s,v), P(s,v), \mu(s,v)$ 的形式  
  >   方法是直接利用欧拉公式: $U = T S - P V + \mu N$
  > * 只能知道两个上面的东西:  
  >   方法是利用 Gibbs-Duhem 公式得到第三个:
  >   $\mathrm{d} \mu = -s \mathrm{d}T + v \mathrm{d}P$
  > * 或者是直接暴力积分: $\mathrm{d} u = T \mathrm{d} s - P \mathrm{d} v$  
  >   比如已知 $PV^k = g(S) \Rightarrow \frac{\partial U}{\partial V} = - \frac{g(S)}{V^k} \Rightarrow U = \cdots$

  > 计算平衡位置: (不过一般都伴随着对能量改变的计算)
  > * 强度量相等  
  >   不过还是要搞清楚用来平衡的东西是啥. 比如有什么半透膜什么之类的, 
  >   不然可能会搞混. 
  > * 熵最大
  > * 势最小

* 热机
  > 计算最大功和熵变:  
  > 一般的做法就是写出熵变 $\Delta S = \int \mathrm{d} U / T$, 
  > 然后利用熵增: $\Delta S \geq 0$ 得到温度的最小或最大. 
  > 于是能够得到 $W = -\Delta U$ 的最大. 

  > 计算某一段过程的做功, 吸热等等之类的东西. 
  > * 一般的方法就是暴力积分, 如果路径都给的非常清晰的话
  > * 但是也有可能会遇到路径非常模糊不清, 描述十分奇怪的情况, 
  >   这个时候估计可以利用势函数的一个路径无关的特性, 直接计算端点. 
  > * 或者可以试试看沿着某一条直线进行积分. 比如已知 $(\partial_X Y)_Z$, 
  >   没准可以利用走直线 (单解一个方向的偏微分) 的方式来求解变化量. 
  
## 后记
就这样吧... 
