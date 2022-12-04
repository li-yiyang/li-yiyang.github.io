---
layout: post
title:  "统计力学部分"
date: 2022-12-04 11:06:05 +0800
math: true
categories: notes
---
# 热力学统计物理 - 统计力学部分
虽然还早, 但是因为这学期网课学得实在是太烂, 
不得不在写作业的时候回顾一下. 

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [热力学统计物理 - 统计力学部分](#热力学统计物理---统计力学部分)
    - [简单的大纲](#简单的大纲)
    - [微观状态等可能](#微观状态等可能)
    - [分布](#分布)
        - [Boltzmann Distribution](#boltzmann-distribution)
        - [配分函数](#配分函数)
        - [Maxwell-Boltzmann Speed Distribution](#maxwell-boltzmann-speed-distribution)
        - [Fermi-Diac](#fermi-diac)
        - [Bose-Einstein](#bose-einstein)
        - [Maxwell-Boltzmann](#maxwell-boltzmann)
    - [Examples](#examples)
        - [态密度](#态密度)
        - [Dilute Gas](#dilute-gas)

<!-- markdown-toc end -->


## 简单的大纲
* **Principles**: Every possible configuration of a system under
  constraints is equally probable.  
  微观状态等可能. 
* **Distribution**
  * **Distribution Functions**
  * **Boltzmann Distribution**: 波尔兹曼分布
  * **Fermi-Dirac Distribution**: 费米 - 狄拉克分布, 费米气体
  * **Bose-Einstein Distribution**: 玻色 - 爱因斯坦分布, 玻色 - 爱因斯坦气体
* **Density of States**: 态密度
* **Dilute gases**: 稀薄气体

## 微观状态等可能
> **The averaging principle**:  
> All accessible microstates are equally likely. 

理论力学学过, 描述一个系统: 
$H(q_1, q_2, \cdots, q_n, p_1, p_2, \cdots, p_n)$. 

那么对于一个描述完整的系统, 其中的 $q_i$, $p_i$ 可能的取值空间,
即在一个相空间中 (的一个部分). 那么在可能的相空间中的每一点,
出现的概率都是一样的. 

> 如对于一个振动解: $E_n = (n + \frac{1}{2})\hbar \omega$, 
> 那么对于每个能量态上的可能性: 
> $\propto A_n = \pi \sqrt{2 E_n / k}\sqrt{2mE_n} = \frac{2\pi E_n}{\omega}$.
> 于是 $\Delta A = h$. 

这样的一个系统, 在宏观观测的时候, 并不能观测到所有的信息, 
即 $H(q_i, p_i) \mapsto S(U, V, N_i, \cdots)$ 是一个满射但不是单射.

于是对于一个宏观观测的系统, 对应的应该就是所有的微观观测的结果:

$$P(S(U, V, N_i, \cdots)) = \sum P(H(q_i, p_i) \mapsto S(U, V, N_i, \cdots))$$

> 对于一个微观状态数为 $2$ 的一个系统, 
> 其微观可能的状态只有 $\uparrow$ 或者 $\downarrow$. 
> 于是对于一个宏观分布: $P(n_1\uparrow, n_2\downarrow)$, 
> 其对应的微观状态数为 $\Omega = \frac{(n_1+n_2)!}{n_1!n_2!}$.

对于两个系统, $S_1, \Omega_1$ 和 $S_2, \Omega_2$ 来说, 
其平衡时应有 $S_1 + S_2, \Omega_1 \times \Omega_2$. 
于是猜测有 $S = k_B \mathrm{ln}\Omega$. 

## 分布

$$f(\varepsilon_i) = \frac{1}{B e^{\varepsilon_i/k_BT} \begin{array}{ll}+1 & (FD)\\+0 & (MB)\\-1 & (BE)\end{array}},\qquad \mathrm{where}\ e^{\alpha} = e^{\mu/k_BT} = \frac{1}{B} = A$$

($\Delta n_i = \omega(\varepsilon_i) f(\varepsilon)$)

### Boltzmann Distribution

$$n_i = \frac{N}{\sum_j e^{\beta\varepsilon_j}} e^{\beta\varepsilon_i}$$

其中 $\beta = -\frac{1}{k_B T}$

Boltzmann 分布将相空间对能量进行分划: 
* 每个能量上 $\varepsilon_i$ 有 $n_i$ 个粒子, 
  于是有数量和能量守恒:
  
  $$\sum_i n_i = N, \sum_i n_i \varepsilon_i = E$$

* 对应 ${n_j}$ 的宏观分布的微观状态数为:

  $$\Omega({n_j}) = \frac{N!}{\prod_j n_j!}$$

* 在宏观上取最大可能的分布, 即对应 $\Omega$ 最大的宏观状态. 
  方法就是利用拉格朗日乘子法来做: 
  
  $$L = \mathrm{ln}\Omega + \alpha (\sum n_i - N) + \beta (\sum n_i \varepsilon_i - E) \Rightarrow \frac{\partial L}{\partial n_i} = 0$$
  
  其中对 $\Omega$ 做对数, 方便计算. 并且可以使用 Stirling 公式,
  $\lim_{x \rightarrow \infty} \mathrm{ln}x! = x\mathrm{ln}x$. 
  
  于是得到 $n_i = e^{\alpha + \beta \varepsilon}$. 
  回带到方程中可以得到解. 
  
  其中, 利用 $(\frac{\partial S}{\partial E})_{V,N} = k_B (\frac{\partial \mathrm{ln}\Omega}{\partial E}) = \frac{1}{T}$ 得到 $\beta$. 

### 配分函数
引入配分函数 (Zustandssumme, the sum over all states): 

$$Z = \sum_j e^{\beta\varepsilon_j}$$

<details>
<summary>一些没用但很有用的小知识</summary>
der Zustand: 状况, 情况; die Summe: 和, 总数; s: 连接用的
</details>

于是:

* Boltzmann 分布中:

  $$n_i = \frac{N}{Z} e^{\beta\varepsilon}$$

* 能量
  
  $$U = \sum n_i \varepsilon_i = \frac{N}{Z} \frac{\partial Z}{\partial \beta} = N \frac{\partial \mathrm{ln}Z}{\partial \beta}$$
  
  于是可以在能量的基础上计算得到热容: $C_V = \frac{\partial U}{\partial T}$

* 系统  
  对于一个系统的能量分划的标准: $\varepsilon = \sum_i \varepsilon_i$, 
  可以看作是多个子系统的和. 假设每个子系统 $\varepsilon_i$ 的能量取值,
  都是满足相同分布的一个随机变量. 那么对于这样的系统, 其配分函数为:
  
  $$Z_i = \sum_i e^{\beta\epsilon_i}$$
  
  以双态的系统为例: 一个粒子能量分布取值有 $\epsilon_{\downarrow}$ 和 
  $\epsilon_{uparrow}$ 两个能级. 那么对于有两个这样的粒子组成的系统, 
  应该有 $\varepsilon = \epsilon_{\downarrow, \uparrow}, \epsilon_{\uparrow, \uparrow}, \epsilon_{\uparrow, \downarrow}, \epsilon_{\downarrow, \downarrow}$
  
  于是 $Z = \sum e^{\beta\varepsilon} = Z_i \times Z_i$
  
  于是可以得出, 对于可以区分的粒子的系统, 其 $Z = \prod_i Z_i = [Z_1]^N$,
  而对于不可区分的全同粒子, $Z = \frac{[Z_1]^N}{N!}$

* 自由能  
  利用热力学中的勒让德变换可以得到:
  
  $$F = U - TS = - N k_B \mathrm{ln}Z$$
  
  于是还能够反过来写出 $S$ 的表达式:
  
  $$S = \frac{1}{T} (U - F) = \frac{N}{T} (\frac{\partial }{\partial \beta} + k_B)\mathrm{ln}Z$$
  
  或者是:
  
  $$S = - (\frac{\partial F}{\partial T})_{V,N}$$

* 简并能级 Degenerate Energy Level  
  
  举一个例子, 对于钠黄光的一个能级 
  $\varepsilon_i = (n + \frac{1}{2})\hbar\omega$ 可能对应不只一条能级,
  或者说, 因为能级区分不大, 实际其中有多个能级 ($\omega_i$ 个). 
  但是这些能级被简单当作一个能量来处理, 所以就叫做简单合并. 
  
  于是本来应该写成 
  $Z = \cdots + e^{\beta\varepsilon_i^{(1)}} + e^{\beta\varepsilon_i^{(2)}} + \cdots + e^{\beta\varepsilon_i^{(\omega_i)}} + \cdots$,
  现在就要变成: $Z = \cdots + \omega_i e^{\beta\varepsilon_i} + \cdots$.
  
  于是现在的配分函数就变成了: 
  
  $$Z = \sum_i \omega_i e^{\beta\varepsilon_i}$$
  
  并且之前的结论还是成立的. 
  
  <details>
  <summary>一个不靠谱的解释</summary>
  <p>
    之前我一直难以理解这个概念, 于是我说, 差不多得了. 
    欸, 这概念还真的就是差不多得了的意思. 
  </p>
  <p>
    在维基百科上是这样介绍的: 
	简并是指被当作同一较粗糙物理状态的两个或多个不同的较精细物理状态. 
  </p>
  <p>
    好冷的笑话... 
  </p>
  </details>

* 态密度 DOS  
  对于不同的能级 $\varepsilon_i$, 对应有不同的能量简并度 $\omega_i$,
  于是做两者之间的映射关系: $\omega(\varepsilon_i)$. 
  
  于是:
  
  $$Z = \sum_i \omega(\varepsilon_i) e^{\beta\varepsilon_i}$$
  
  拓展到连续能量分布, 就变成: 
  
  $$Z = \int_0^\infty \omega(\varepsilon) e^{\beta\varepsilon} \mathrm{d}\varepsilon$$

  具体的态密度在 [下面](#态密度) 会记录. 

### Maxwell-Boltzmann Speed Distribution
由配分函数的结论可以得到: 
$n_i = \frac{N}{Z} \omega_i e^{\beta\varepsilon_i}$

连续化, 得到 $n(v)\mathrm{d}v = \frac{N}{Z} \omega(v) e^{\beta \varepsilon(v)}\mathrm{d}v$. 
其中, $\omega(v)$ 通过 $\omega(p)$ 来计算得到. 

于是有: 

$$n(v) \mathrm{d}v = \frac{N}{Z} \frac{V m^2}{2\pi^2\hbar^3} v^2 e^{-mv^2/(2k_BT)}\mathrm{d}v$$

* 平均速度: $\sqrt{\frac{8k_BT}{\pi m}}$
* 方均速度: $\frac{3k_BT}{m}$
* 最概然速度: $\sqrt{\frac{2k_BT}{m}}$

### Fermi-Diac
<details>
<summary>Indistinguishability and Quantum Mechanics</summary>
<p>
对于一个系统, 可以用 $\psi(1, 2)$ 来表示有两个粒子的系统. 
那么交换这两个粒子, 再交换回来, 应该是对称的一个操作, 即:
</p>

$$\psi(1, 2) = e^{i\delta}\psi(2, 1) = e^{i\delta}e^{i\delta}\psi(1, 2) \Rightarrow e^{i\delta} = \pm 1$$

<p>
于是对于不同的 $e^{i\delta}$, 对应的结果也不同: 
</p>

<p>
$e^{i\delta} = -1$, 于是 $\psi(1, 1) = - \psi(1, 1) = 0$, 
即不存在相通能级的粒子, 即 Pauli Exclusion Principle. 
对应 Fermions. 
</p>

<p>
$e^{i\delta} = 1$, 于是可以交换, 对应 Bosons. 
</p>
</details>

对于 Fermions, 每个能量状态下的粒子只有 $0$ 或者 $1$ 个. 
于是对于简并度为 $\omega_i$ 的能级 $\varepsilon_i$, 
拥有 $n$ 个粒子的可能性为 $t_i = \frac{\omega_i!}{n_i!(\omega_i - n_i)!}$.
于是总的状态数就变成了: $\Omega = \prod t_i$.

于是类比 Boltzmann 分布中的推导方式, 可以得到:

$$\sum_i \mathrm{ln}(\frac{\omega_i - n_i}{n_i}) + a + b \varepsilon_i = 0$$

$$\Rightarrow n_i = \frac{\omega_i}{e^{-a-b\varepsilon_i} + 1}$$

其中, $b = \beta$, 将 $a$ 写作 $-\beta\mu$. 

因为一般研究的是分布函数 (distribution function), 
定义 $f = n_i / \omega$ 为平均每个状态下有的粒子数 
(average number of particles in a state)

于是得到 Fermi-Dirac 分布:

$$f_{FD}(\varepsilon) = \frac{1}{e^{(\varepsilon - \mu)/k_BT} + 1}$$

### Bose-Einstein
和 Fermions 不同的是, 每个能量状态下的粒子可以有任意多个, 
所以在简并度为 $\omega_i$ 的能级 $\varepsilon_i$, 
拥有 $n$ 个粒子的可能性为 $t_i = \frac{(n_i + \omega_i - 1)!}{n_i!(\omega_i - 1)!} \approx \frac{(n_i + \omega_i)!}{n_i! \omega_i!}$.

于是同理也能得到:

$$\sum_i \mathrm{ln}\frac{n_i + \omega_i}{n_i} + a + b\varepsilon = 0$$

$$\Rightarrow f_{BE}(\varepsilon) = \frac{1}{e^{(\varepsilon - \mu) / k_BT} - 1}$$

### Maxwell-Boltzmann

$$t_i = \frac{(n_i + \omega_i)!}{n_i! \omega_i!} \approx \frac{\omega_i^{n_i}}{n_i!}$$

$$\Rightarrow f_{MB}(\varepsilon) = \frac{1}{e^{(\varepsilon - \mu)/k_BT} + 0}$$

## Examples
### 态密度
就怎么找到一个态密度函数. 

* 一维驻波  
  对于一维驻波, 限制在 $a$ 的区域内, 其半波长为 $\frac{\lambda_n}{2} = \frac{a}{n}$. 
  于是其动量为 $p_n = \frac{h n}{2a}$.
* 态密度  
  简单的计算是这样的: 
  
  $$\omega(\varepsilon)\mathrm{d}\varepsilon = \omega(\boldsymbol{p})\vert\mathrm{d}\boldsymbol{p}\vert$$
  
  其中, 用这样的假定: 被考察的系统被限制在体积 $V = a^3$ 的一个立方体内. 
  于是 $\vert\mathrm{d}\boldsymbol{p}\vert = (\Delta p_x)(\Delta p_y)(\Delta p_z) = \frac{h^3}{V}$. 
  
  而 $\omega(\boldsymbol{p})$ 的计算可以这样来, 首先假定均匀,
  即动量方向, 或者说波矢指向是球对称的, 那么就可以有:
  
  $$\omega(k)\mathrm{d}k = \frac{V}{\pi^3} \times \frac{1}{8}4\pi k^2\mathrm{d}k$$
  
  于是最终得到: $\omega(p) = \frac{V p^2}{2\pi^2\hbar^3}$
  
  (相当于是做了一个 $\omega \rightarrow \boldsymbol{p}$ 的一个换元. 
  对易关系就是 $\varepsilon = \varepsilon(p)$ 这样的一个东西. 当然,
  也不一定就一定是 $p$, 比如根据对称性是空间分布对称, 或者别的什么,
  有一个 $\chi$ 满足均匀分布, 那么就应该可以类似来做. 也就是说, 
  前面的这些操作就是为了将不均匀分布的 $\varepsilon$ 变换成一个均匀的,
  容易计算 $\omega(\chi)$ 的东西. )
  
  于是就有:
  
  $$\omega(\varepsilon) = V\frac{p^2}{2\pi^2\hbar^3}(\frac{\mathrm{d} \varepsilon}{\mathrm{d} p})^{-1}$$
  
  * 自由气体: $\varepsilon(p) = \frac{p^2}{2m}$  
	
	$$\omega(\varepsilon) = \frac{V}{4\pi^2}(\frac{2m}{\hbar^2})^{3/2}\varepsilon^{1/2}$$
  * 光子气体 (电辐射): $\varepsilon = p c$
  
    $$\omega(\varepsilon) = V\frac{\varepsilon^2}{2\pi^2\hbar^3c^3}$$
  * 固体中的声子: $\varepsilon = p v_S$, 其中 $v_S = \nu \lambda$  
	
	利用 $\omega(\lambda)$ 来计算即可. 

### Dilute Gas
即不考虑粒子之间的相互作用. 

一般的操作步骤:
1. 计算 DOS  

   $$\omega(\varepsilon) = \omega(\zeta)(\frac{\mathrm{d} \varepsilon}{\mathrm{d} \zeta})^{-1}$$

   一般是根据 $\omega(\zeta)$ 均匀分布的东西来计算的. 
   * $\omega(p) = \frac{V p^2}{2\pi^2\hbar^3}$
   * $\omega(\lambda) = \frac{8\pi V}{\lambda^4}$
2. 然后计算得到一个粒子的配分函数
   
   $$Z = \int_0^\infty \omega(\varepsilon) e^{\beta\varepsilon}\mathrm{d}\varepsilon$$
3. 于是就知道了对一个系统的配分函数
   * 全同粒子: $Z = [Z_1]^N/N!$
   * 非全同粒子: $Z = [Z_1]^N$
4. 然后就能够计算得到 $U, F, S, C_V, \cdots$
   * $E = \frac{\partial}{\partial\beta} \mathrm{ln}Z$
   * $C_V = (\frac{\partial E}{\partial T})_{V, N}$
   * $F = - k_B T \mathrm{ln} Z_N$
   * $S = -(\frac{\partial F}{\partial T})$
   * $P = - (\frac{\partial F}{\partial V})_{T, N}$

* 理想单原子气体  
  根据 $\omega(p)$ 可以计算得到 $\omega(\varepsilon) = \frac{V}{4\pi^2} (\frac{2m}{\hbar^2})^{3/2}\varepsilon^{1/2}$. 

  于是可以简单计算得到对一个粒子的配分函数: 

  $$Z_1 = \int_0^\infty \omega(\varepsilon)e^{\beta\varepsilon}\mathrm{d}\varepsilon = V (\frac{2\pi m}{\beta h^2})$$
  
  拓展到多粒子的系统: 
  
  $$Z = \frac{V^N}{N!} (\frac{2\pi m}{\beta h^2})^{3N/2}$$
  
  于是就能计算得到 
  
  $$E = \frac{\partial}{\partial\beta} \mathrm{ln}Z \Rightarrow = \frac{3}{2}N k_B T$$

* 在单原子的基础上的拓展:  
  考虑单个粒子的配分函数, 其为 $Z_1 = \sum e^{\beta(\varepsilon_{trans} + \varepsilon_{rot} + \varepsilon_{vib})} = Z_{trans} \times Z_{rot} \times Z_{vib}$
  
  于是拓展计算即可. 其中 $Z_{trans}$ 即为单原子平动的结果, 
  而其他的则分别对应转动, 振动的结果:
  * $Z_{rot}$:  
	* $\omega_J = 2J+1$
	* $\varepsilon_J^{rot} = J(J + 1)\hbar^2$
  * $Z_{vib}$  
  
  于是这样还能给出能量在不同温度下的解放
