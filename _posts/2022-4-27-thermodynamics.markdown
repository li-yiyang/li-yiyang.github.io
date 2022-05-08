---
layout: post
title:  "Thermodynamics Mid-Term"
date: 2022-05-08 22:47:34 +0800
math: true
categories: notes
---
# 热学
## 平衡态和状态方程
### 状态参量和平衡态
定义和概念: 
* 热力学系统: 就是问题的研究对象. 一般由**大量**微观粒子组成. (*大量*是因为热力学是建立在统计意义上的描述. )
  * 系统的分类: (根据系统的不同性质可以分为)
    * 系统与外界关系: 开放(有物质交换), 封闭(无物质交换), 绝热(无物质交换, 并且也没有热量传递), 孤立(没有物质交换也没有能量交换)
    * 系统与组成部分: 单元(同一种粒子), 多元(多种粒子)
    * 系统均匀性: 单相(同一种状态), 复相(多种状态, 比如固液气)
* 外界(环境): 和系统相互作用(能量, 物质)的东西
* 状态参量: 

| 状态参量 | 广延量 |  强度量  |
|----------|--------|----------|
| 几何参量 |  体积  | 单位体积 |
| 力学参量 |   力   |   压强   |
| 化学参量 | 物质量 |  数密度  |
| 电磁参量 | 带电量 | 电荷密度 |
| 热学参量 |  热量  |   温度   |

* 平衡态: 外界**无影响**, 系统各部分宏观量长期不变
* 稳定态: 外界**影响**下, 系统各部分宏观量长期不变

(宏观一致, 微观在变. 状态参量只有在平衡态才有意义. 但是也可以把局部当作是[(近)平衡态](#近平衡态的输运过程). )

### 温度
**热力学第零定律**

若两个热力学系统均与第三个系统处于热平衡状态, 那么这两个热力学系统也处于热平衡状态. 

(假如不是的话, 估计就会有像这样的古怪情况: A, B相邻, 达到热平衡, C和A接触后与A达到平衡, 这个时候分离A, B, 然后B却会和C不平衡. 就像是和AB整体平衡, 却和部分不平衡的矛盾. )

* 热平衡: 通过热接触一段时间后达到的宏观状态不再变化的状态
* 温标三要素: 
  * 测温物质
  * 测温属性: 最好是单值(对应的多值的类型比如有磁滞回曲线)线性容易观测的性质(比如水银热胀冷缩)    
    并非测温属性都是线性的. 所以会对结果产生影响. 
  * 标准点: 水的三相点(注意和大气压下冰水混合物的温度不同)    
    区别的原因: 溶有空气, 压力变化影响
* 理想气体温标: 通过理想气体的$p V = k_B T$的物态方程推出在$p \rightarrow 0$时的$T$. 
* $273.15K=0 ^\circ C$, 水的三相点$273.16K=0.01 ^\circ C$
* 华氏温标: $\frac{t_F}{^\circ F} = 32 + \frac{\frac{9}{5} t_{CS}}{^\circ C}$
* 热力学温标: 不依赖于物质特性, 建立在热力学第二定律基础上

### 状态方程
* 相图: 将状态参量$(p,V,T)$做为坐标点画在空间中    
  关于看相图, 在相图上的每一点都是对应一个状态. 看分割线就可以确定物质所在的状态. 
* 定义: 就是状态参量描述的系统的方程. 
* 理想气体方程: $pV = nRT$
* 范德瓦尔斯方程: $(p + \frac{a}{V_m^2})(V_m - b) = RT$ or $(p + \frac{\nu^2 a}{V^2})(V - \nu b) = \nu R T$    
  内压强: $\Delta p = \frac{a}{V_m^2}, a = 4 V_0 \varepsilon_0 N_A^2$, 分子体积$b = 4 N_A V_0$    
  计算临界点$\frac{\partial p}{\partial V} = 0, \frac{\partial^2 p}{\partial V_m^2} = 0 \Rightarrow a = 3 V_{mc}^2 p_c, b = \frac{V_{mc}}{3}$
* 道尔顿分压原理
* 杠杆原理: 两相共存区中的点, 两相物质质量比满足杠杆定律: $\frac{x_1}{x_2} = \frac{V_m - V_2}{V_1 - V_m}$, 其中$V_m = x_1 V_1 + x_2 V_2$, $x_i$为质量分数. 然后$V_i$是摩尔体积. 

> 范德瓦尔斯气体$p = \frac{\nu R T}{V - \nu b} - \frac{\nu^2 a}{V^2}$, 查理定律: $p = p_0(1 + \alpha t)$, 于是得到$\alpha_p = \frac{R}{R T_0 - \frac{(V- \nu b) \nu a}{V^2}} \Rightarrow p_0 \rightarrow 0, \alpha_p = \frac{1}{T_0}$

### 物态
* (等压)热膨胀系数: $\alpha = \frac{1}{V}(\frac{\partial V}{\partial T})_p$    
  直观感觉就是体积的热胀冷缩. 几何参量的热性质. 
* 等体压强系数: $\beta = \frac{1}{p}(\frac{\partial p}{\partial T})_V$    
  直观感觉就是高压锅: 加热升温升压. 力学参量的热性质. 
* 等温压缩系数: $\kappa = \frac{-1}{V}(\frac{\partial V}{\partial p})_T$    
  直观感觉就是弹簧: $F = - k x$. 力学参量和几何参量的关系. 

简单的理解就是简单的物态方程$f(p, V, T)$由力学参量$p$, 几何参量$V$, 热学参量$T$决定. 那么在固定其中一个, 就能够知道另外两个之间的相互关系. 

然后根据一个简单的微积分关系: $(\frac{\partial p}{\partial T})(\frac{\partial T}{\partial V})(\frac{\partial V}{\partial p})=1$, 就能够得到$\kappa p \beta = \alpha$. 

> 通过$\alpha, \kappa, \beta$来计算系统的状态方程.    
> 设状态方程为$f(p, V, T) = 0$, 于是可以像解偏微分方程一样来解出答案.    
> 比如: $f(t, l, T) = 0$, 已知$t = A T (\frac{l}{l_0} - (\frac{l}{l_0})^2)$

### 物质的微观结构
* 分子势模型: 
  * 钢球模型
  * Sutherland
  * Lennard-Jones
* 物态的微观解释
  * 固态: $\overline{E_k} \ll E_p$
  * 液态: $\overline{E_k} \simeq E_p$
  * 气态: $\overline{E_k} \gg E_p$
* 理想气体的微观解释
  * 理想气体压强: $p=\frac{1}{3}mn\overline{v^2} = \frac{2}{3} n \overline{E_k} = n k_B T, \overline{E_k} = \frac{3}{2}k_B T$
  * 理想气体内能: 
  * 传热过程的一种解释: 碰撞将能量从动能大的传到了动能小的粒子上. (但是不能解释等质量对心碰撞的速度交换)
* 细致平衡原理: 平衡态气体中, 正向过程和逆向过程相平衡存在. 

## 平衡态的统计规律
### 概率论
> 哥白尼原则: 在$P$的概率下, 观测到事件$A$, 那么该事件可能存在的值为$\frac{2 t_0}{1 - P}$

> 星期二男孩: 知道的信息越多, 越能确定. 

### 无序系统
完全无序应该是指前一个状态和后一个状态没有任何关系. 

平衡态系统的统计规律是建立在完全无序的假设下推导出的统计规律. 其宏观表现应该符合实验结果. 

### 概率和分布
* 一维随机行走:    
  向左概率$p_{L}$, 向右概率$p_R$, 一共走了$N$步, 其中向右$n$步, 则出现在$2n - N$处的概率为: $P(2n - N) = C_N^n p_R^n p_R^{N-n}$    
  于是最终位置为$D = \sum_0^N P(2n - N) (2n - N)$, 对于各项同性$p_L=p_R$的情况, $D = 0$. (这样是符合直觉的. )
* 二维随机行走:    
  每次步长相等(为1), 方向随机, 最终位置$(\sum X_i, \sum Y_i)$, 距离原点的距离为$R^2 = (\sum X_i)^2 + (\sum Y_i)^2 = \triangle = \sum(X_i^2 + Y_i^2) = M \Rightarrow R = \sqrt{M}$    
  (为什么不像一维随机行走一样是会回到原点? 因为这个时候的$X_i$和$Y_i$是相互关联的. )
  其中$\triangle$的过程: 
  * 因为$X_i$是随机的, 分布为$-1,1$对称, $\overline{X_i} = 0$
  * 因为$X_i$和$X_j$独立, 所以$\overline{X_i X_j} = \overline{X_i} \overline{X_j} = 0$
  * $(\sum X_i)^2 = \sum X_i X_j + \sum X_i^2 = \sum X_i^2$
* 布朗运动    
  $$\frac{\mathrm{d}\overline{x^2}}{\mathrm{t}} = \frac{k_B T}{3\pi a \eta} (1 - e^{-\frac{6\pi a \eta}{m} t})$$, 其中

* 随机事件: 
  * 独立事件$P(A_i, A_j) = P(A_i) P(A_j)$    
    例: 发生了$A_i$后又发生了$A_j$
  * 独立相容事件$P(A_i + A_j) = P(A_i) + P(A_j) - P(A_i)P(A_j)$    
    例: 发生$A_i$和$A_j$的概率. 
  * 互不相容事件$P(A_i + A_j) = P(A_i) + P(A_j)$    
    例: 发生$A_i$和$A_j$的概率, 并且两个事件不会同时发生. 
* 随机变量: 分立随机变量, 连续随机变量
  * 归一律: $\sum P(x_i) = 1$ 和 $\int f(x) \mathrm{d}x = 1$
  * 平均值: $\sum P(x_i) x_i$ 和 $\int x f(x) \mathrm{d}x$
  * 一次矩: $\overline{\Delta x} = \overline{x - \overline{x}} \equiv 0$  
  * 二次矩: $\overline{(\Delta x)^2} = \sum P(x_i) (x_i - \overline{x})^2$    
    随机变量偏离平均高值的度量, 叫做**色散**. 二次矩的平方根叫**均方差**$\sigma$.    
    在高斯分布中的均方差表示实验数据的可信程度. 即$P(\lambda \sigma) = \int_{-\sigma}^{\sigma} f(x) \mathrm{d}x$
  * $n$次矩: $\overline{(\Delta x)^n}$
  * 有意义的统计系统必须要求各次矩有限. 复杂系统各次矩无限.
* 分布: 
  * 二项式分布: $P(n_1) = C_N^{n_1} p^{n_1} q^{N-n_1}$
    * 涨落: $\sigma = \sqrt{N p q}$     
      相对涨落: $\frac{\sigma}{\overline{n_1}} = \sqrt{\frac{q}{p N}}$
  * 泊松分布: $N p = \lambda, p \rightarrow 0$, 于是二项式分布趋于泊松分布: $P_{\lambda}(n_1) = (\frac{\lambda^{n_1}}{n_1 !}) e^{-\lambda}$    
    在空间或时间上等几率事件
  * 高斯分布: $f(x) = \frac{1}{\sqrt{2\pi}} e^{-\frac{1}{2} x^2}$
    * 假设: 分布函数沿径向指数减少: $f(x) = C e^{-\alpha x^2}$    
      然后根据**归一化**得到$f(x) = \sqrt{\frac{\alpha}{\pi}} e^{-\alpha x^2}$
    * 平均值: $\overline{x} = \int_{-\infty}^{\infty} x f(x) \mathrm{d} x = 0$    
      这是一个偶函数, 平均值自然是中心. 假如有移动的话$f(x) = f(x-\mu)$, 那么平均值就是$\mu$了. 
    * 均方差: $\alpha = \frac{1}{2\sigma^2} \Rightarrow \frac{1}{\sqrt{2\pi} \sigma} e^{-\frac{1}{2}(\frac{x}{\sigma})^2}$
  * 正态分布: $f(x) = \frac{1}{\sqrt{2\pi}} e^{-\frac{1}{2} x^2}$    
    即在高斯分布中将$\frac{x}{\sigma} \rightarrow x$
  * 随机行走的径向分布: 在单方向上的分布函数: $$g(x) = \frac{1}{\sqrt{2\pi}\sigma} e^{-\frac{1}{2}(\frac{x}{\sigma})^2}$$, 假设在$x, y$方向上的分布是独立的, 于是在位置$(x,y)$的概率为$g(x)g(y)\mathrm{d}x\mathrm{d}y$. 转换为极坐标表示, 得到$f(r, \theta)$, 对$\theta$积分就能够得到径向分布$G(r) = \frac{r}{\sigma^2} e^{\frac{r^2}{2 \sigma^2}}$
    * 最大概然位置: $\partial_r G = 0 \Rightarrow r_p = \sigma$
    * 平均位置: $\overline{r} = \sqrt{\frac{\pi}{2}} \sigma$
    * 方均根: $r_{r.m.s} = \sqrt{2} \sigma$    
      方均位置: $2\sigma^2$

### 麦克斯韦速度分布与速率分布
#### 配分函数
配分函数: $Z = \sum \Delta \omega_l e^{-\beta E_l}$

可以用来确定定域子系统的一切热力学函数: 
* $$P(a_l^*) = \frac{a_l^*}{N} = \frac{\Delta \omega_l e^{-\alpha - \beta E_l}}{\sum \Delta \omega_l e^{-\alpha - \beta E_l}} = \frac{1}{Z} \Delta \omega_l e^{-\beta E_l}$$
* $\frac{Z}{N} = e^{\alpha} \Rightarrow \alpha = \ln \frac{Z}{N}$
* $E = - N \frac{\partial \ln Z}{\partial \beta}$

#### 麦克斯韦分布
相空间中子空间的体积: $\mathrm{d} \omega = \mathrm{d}x\mathrm{d}y\mathrm{d}z\mathrm{d}p_x\mathrm{d}p_y\mathrm{d}p_z$

对于理想气体, 因为没有相互作用产生势能, 所以能量只有动能: $E = \frac{p_x^2 + p_y^2 + p_z^2}{2 m}$

于是配分函数为: $Z = \int_V e^{\beta E} \mathrm{d} \omega = V (\frac{2\pi m}{\beta})^{3/2}$, 于是可以计算$\alpha = \ln \frac{V}{N} (\frac{2 \pi m}{\beta})^{3/2}, E = \frac{3}{2} \frac{N}{\beta}$, 代入理想气体$\frac{2}{3}E = \frac{N}{\beta} \Rightarrow \beta = \frac{1}{k_B T}, \alpha = \ln \frac{V (2\pi m k_B T)^{3/2}}{N}$. 

然后对几何空间积分得到速度分布: 
$$(\frac{m}{2\pi k_B T})^{3/2} e^{-\frac{1}{k_B T} E_k}$$

或者对动量空间积分得到位置分布: (均匀分布)

* 最概然速率: $\frac{\partial f}{\partial v} = 0 \Rightarrow v_p = \sqrt{\frac{2 k_B T}{m}}$
* 平均速率: $\overline{v} = \sqrt{\frac{8 k_B T}{\pi m}}$
* 方均根速率: $v_{r.m.s} = \sqrt{\frac{3 k_B T}{m}}$

#### 速度变换
$g(v_i) = \sqrt{\frac{m}{2 \pi k_B T}} e^{-\frac{m v_i^2}{2 k_B T}}$

> 坐标变换    
> 直角坐标系变成球坐标系: $f(v_x, v_y, v_z)\mathrm{d}v_x\mathrm{d}v_y\mathrm{d}v_z = g(v_x) g(v_y) g(v_z) v^2 \sin \theta \mathrm{d}v\mathrm{d}\theta\mathrm{d}\phi$.     
> 其实就是坐标体积元的变化. 

> 标度变换: 令速率单位$v_p = \sqrt{\frac{2 k_B T}{m}}$, 得到无量纲速率$u = \frac{v}{v_p}$, 最后得到$f(u) = \frac{4}{\sqrt{\pi}} u^2 e^{-u^2}$    
> 逃逸速度: $\frac{\Delta N}{N} = \int_{k'}^{\infty} f(u) \mathrm{d}u$

> 泄流速率: $n \int_0^{\infty} v_x g(v_x) \mathrm{d}v_x = \frac{1}{4} n \overline{v}$

### 近独立系统的最概然分布
#### 术语和基本概念
* 相空间(粒子在相空间的统计分布)    
  > 对于多粒子系统, 确定系统的状态需要知道所有例子在相空间的位置和轨道, 这一百是不可能的, 也是没有必要的.    
  > -- 玻尔兹曼
* 微观状体和宏观状态的区别: 
  * 微观状态: 微观可分, 即两两不同. 所以微观状态数有$2^N$个
  * 宏观状态: 宏观不必可分, 即任意一对小球交换不改变宏观状态. 所以微观状态数为$N+1$个

#### 等概率原理
> 对于处于平衡态的孤立系统, 其各个可能的微观状态出现的概率都相等.     
> -- Boltzmann

#### 理想气体的分布
对于所研究的$N$个粒子, 将其相空间分为$k$个子空间, (如果要应用微积分的话, 就要将子空间分割得足够小, $k \rightarrow \infty$), 于是落在特定子空间的几率为$P(\Delta \omega)$. 对于微观状态有$k^N$种状态, 宏观状态有$\Omega_a = \frac{(N + k - 1)!}{N!(k-1)!}$种(隔板法). 

所以对于特定的微观分布$a = \{a_1, a_2, \cdots, a_k\}$概率: 

$$P(\Delta \Omega) = \prod_{i = 1}^k P(\Delta \omega_i)^{a_i}$$

即对应的微观态的分布是相对独立的事件. 

然后考虑微观粒子交换不改变宏观状态, 宏观对应的状态有$$\omega = \frac{N!}{\prod_{i=1}^k a_i!}$$, 于是宏观状态的出现概率为$P_N(a) = \omega P(\Delta \Omega)$

> 在平衡状态下, 如果分子数目足够大, 宏观系统的状态可以用最大概然分布代表. 

求导, $\delta [P_N(a)] = 0$, 为了方便计算, 将问题等价为$\delta [\ln(P_N(a))] = 0$. 并且考虑能量守恒和数量守恒的约束, 利用拉格朗日方法$N = \sum a_l, E = \sum E_l a_l, L(a) = \sum \ln(P_N(a)) - \overline{\alpha} a_l - \beta E_l a_l$. 

其中的$ln P_N = \ln N! - \sum \ln a_l! + \sum a_l \ln P(\Delta \omega_l)$, 应用Stirling公式, 将$\ln m! \simeq m(\ln m - 1)$, 于是$\delta [\ln(P_N(a))] = -\sum \ln \frac{a_l}{P(\Delta \omega_l)} \delta a_l$. 

于是得到$\delta L = - \sum (\ln \frac{a_l}{P(\Delta \omega_l)} + \overline{\alpha} + \beta E_l) \delta a_l = 0 \Rightarrow \ln a_l + \overline{\alpha} + \beta E_l = \ln P(\Delta \omega_l) \Rightarrow a_l = P(\Delta \omega_l) e^{\overline{\alpha} + \beta E_l}$

即, $n = n_0 e^{-\beta E_l}$. 能量越高, 数量越少. 

考虑对分布的一个偏离:
$$\ln P_N(a + \delta a) = \ln P_N(a) + \delta \ln P_N + \frac{1}{2} \delta^2 \ln P_N(a) + o(\delta a^3) \simeq \frac{1}{2} \delta^2 \ln P_N(a) = -\frac{1}{2} \delta \sum \ln (\frac{a_l}{P(\Delta \omega_l)} \delta a_l) = - \frac{1}{2} \sum (\frac{\delta a_l}{a_l^*})^2 a_l^*$$

所以出现的相对几率: $$\frac{P_N(a + \delta a)}{P_N(a)} = e^{-\frac{1}{2}\sum(\frac{\delta a_l}{a_l^*})^2 a_l^*} \simeq e^{-\frac{1}{2}(\frac{\delta a_l}{a_l^*})^2 N}$$

> 在大气中的气体偏离平均分布的分析: 考虑光波长为边长的立方体里面的偏离. 

### 熵
态函数的熵: $S = k_B \ln P_N(a)$

在宏观状态(同一种表现)下可以对应有多种微观分布(多种不同的分布). 对于每种分布等概率, 那么总共的宏观概率就是微观分布的个数乘上概率, 那么微观分布的种数越多, 宏观出现的概率就越大, 就更容易出现. 所以系统在平衡态微观状态数最多, 最无序, 信息最少(因为可能性太多了, 所以不容易确定究竟是哪一个, 所以信息变少了). 

### 玻尔兹曼分布
$$f_{MB}(\varepsilon) = C e^{-\frac{\varepsilon}{k_B T}}$$

### 能均分定理 - 气体的内能
每个自由度都有$\frac{1}{2}k_B T$的能量, 

### 量子气体

## 近平衡态的输运过程
### 气体碰撞概率与平均自由程
> [未若柳絮因风起]({{ site.github.url }}/physics/catkin/)中的逻辑就是平均自由程和碰撞粘合概率的东西. 

* 相对碰撞质量: $\mu = \frac{m_1 m_2}{m_1 + m_2}$
* 相对速度: $\overline{u} = \sqrt{2} \overline{v} = \sqrt{\frac{8 k_B T}{\pi \mu}}$
* 碰撞面积: $\sigma = \pi (r_1 + r_2)^2$
* 平均碰撞次数: $\overline{N} = n \sigma \overline{u} \Delta t$
* 平均碰撞概率: $\overline{Z} = \sqrt{2} \sigma \overline{v} n$
* 平均高自由程: $\overline{\lambda} = \frac{1}{\sqrt{2}\sigma n} = \frac{k_B T}{\sqrt{2} \sigma p}$
* 单位体积碰撞次数: $Z_{AA} = \frac{1}{2} n \overline{Z}$
* 碰撞概率: $P(\lambda) = 1 - e^{-\lambda / \overline{\lambda}}$
* 自由程概率密度函数: $f(\lambda) = \frac{1}{\overline{\lambda}} e^{-\lambda / \overline{\lambda}}$

### 输运过程的宏观规律
* 弛豫
* 输运(平衡条件): 
  * 力学
  * 热学
  * 化学
* 输运方式: 
  * 粘滞
  * 热传导
  * 扩散

#### 粘滞
$f = - \eta \frac{\mathrm{d} u}{\mathrm{d} z} S$

#### 扩散
$J_M = -D \frac{\partial \rho}{\partial z} \Delta S$

#### 传导
* 傅立叶传导定律: $h = - \kappa \frac{\partial T}{\partial z}$
* 辐射传热: $E = \sigma T^4$
* 对流传热 - 牛顿冷却定律: $\frac{\mathrm{d}Q}{\mathrm{d}t} = \kappa (T_1 - T_2) S$

### 气体输运现象的微观解释
$D = \frac{1}{3} \overline{v} \overline{\lambda}$, $\eta = \frac{1}{3} \overline{v} \overline{\lambda} \rho$, $\kappa = \frac{1}{3} \overline{v} \overline{\lambda} \rho C_V$

## 练习
* 温标
  * 等温, 等体, 等压系数来描述
  * 或者利用物态方程来搞
  * 温标的零度
* 物态方程
  * 理想气体状态方程
  * 范德瓦尔斯气体方程
  * 道尔顿分压
* 等温, 等体, 等压系数的使用
  * 计算状态方程(类似于解方程)
* 能量守恒
* 速度分布的使用
  * 坐标变换
  * 计算平均, 最概然等
  * 泄流数
* 偏移平均分布
* 平均自由程
  * 碰撞概率
  * 真空度
* 粘滞, 传热, 扩散
  * 计算方法