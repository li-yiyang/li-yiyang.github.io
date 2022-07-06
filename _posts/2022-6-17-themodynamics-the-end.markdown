---
layout: post
title:  "Themodynamics The End"
date: 2022-06-17 09:23:20 +0800
math: true
categories: notes
---
# 热学
学得迷迷糊糊的, 没有一个很好的物理图像. 

## 热力学第一定律
热力学第一定律: 能量守恒在涉及热现象宏观过程中的具体表现. (能量守恒, 否定了第一类永动机. )

$$\mathrm{d}U = \mathrm{d}W + \mathrm{d}Q$$

(这里取向内做功为正. 是从外界的角度看的. )

* $\mathrm{d}W$: 做功是过程量(和路径有关). 做功可以有多种形式, 和几何, 力学, 电磁, 化学参量相联系, 有:
  * 体积功: $\delta W = -p \delta V$    
    (注意这里的$p$是外界的参量, 不一定等于(只有在准静态过程中才有相等的条件)气体的状态量. )
  * 表面张力功: $\delta W = \sigma \delta S$
  * 弹性力功: $\delta W = T \delta l$
  * 电源电动势做功: $\delta W = U \delta q$
* $\mathrm{d} Q$: 热量传递(以及化学反应, 相变潜热等的热量变化)也是一个过程量.    
  热容$C = \frac{\mathrm{d}Q}{\mathrm{d}T} = \frac{\mathrm{d}U + p\mathrm{d}V}{\mathrm{d}T}$    
  (下面的都是理想气体. )     
  * 等容过程: $C_V = \frac{\mathrm{d}U}{\mathrm{d}T} \Rightarrow U = C_V T$    
    关于$C_V$: $C_V = \frac{i}{2} R$, 对于单分子气体$i = 3$, 刚性双分子$i = 5$, 多原子$i = 7$. 
  * 等压过程: $C_p = \frac{\mathrm{d}H}{\mathrm{d}T} = (C_V + 1) R$
  * 多方过程($pV^n=\mathrm{const}$): $C_n = C_V - \frac{R}{n - 1}$
* $\mathrm{d} U$: 内能是一个状态量. (于是绝热过程中, $\mathrm{d} Q = 0 \Rightarrow \mathrm{d} U = \mathrm{d} W$为态函数. )

### 准静态过程
进行得足够缓慢, 以至于系统连续经过的每一个中间态都可以近似为平衡态(系统和外界平衡, 在外界看来的参量就是系统所处的参量). 属于是一种**理想过程**, 没有耗散, 能量转换效率最高. 是一种**可逆过程**. 

描述系统的状态量: 
* 焓(Enthalpy): $H = U + p V$    
  对应等压过程: $\mathrm{d} U = - p\mathrm{d}V - (V \mathrm{d} p)_{= 0} + \mathrm{d}Q = -\mathrm{d}(p V) + \mathrm{d}Q \Rightarrow \mathrm{d}Q = \mathrm{d}H = \mathrm{d}(U + pV)$
* 熵: $\mathrm{d}S = \frac{\hat{\mathrm{d}} Q}{T}$(其中$\hat{\mathrm{d}} Q$代表吸热. )    
  在准静态过程中, 对理想气体, 有$\oint \frac{\hat{\mathrm{d}} Q}{T} = 0$. (通过构造元过程来搞. )    
  $T\mathrm{d}S = \mathrm{d}U + p \mathrm{d} V$(在对任意工作物质成立, 通过热力学第二定律证明. )
* 亥姆霍兹自由能: $F = U - TS$    
  对应**等温过程**, 系统对外做功小于等于自由能的减少. $\delta W \leq \delta F$, (等号在可逆过程取到). 可以看作是等温过程中向外的最大做功.    
  自由能减少原理: 等温等体过程中, $\mathrm{d}F \leq 0$, 即等温等体过程沿着系统自由能减少的方向进行. (用力学的能量观点看就是, 物体的运动方向是势能的减少方向. 即$\ddot{x} = -\nabla\varphi$)
* 吉布斯自由焓: $G = F + p V$    
  对应**等温等压**过程, $\mathrm{d}G \leq 0$, 即自发的等温等压过程沿着吉布斯自由焓减少的方向进行. 

热力学函数关系: 

| 热力学函数 | $U(T, V)$ | $H(T, p)$ | $F$ | $G$ |
| ========== | ========= | ========= | ========= | == |
| 准静态过程微分关系 | $\mathrm{d}U = T\mathrm{d}S - p\mathrm{d}V$ | $\mathrm{d}H = T\mathrm{d}S + V\mathrm{d}p$ | $\mathrm{d}F = -S\mathrm{d}T - p\mathrm{d}V$ | $\mathrm{d}G = -S\mathrm{d}T + V\mathrm{d}p$ |
| 自发过程微分关系 | $\mathrm{d}U \leq T\mathrm{d}S - p\mathrm{d}V$ | $\mathrm{d}H \leq T\mathrm{d}S + V\mathrm{d}p$ | $\mathrm{d}F \leq -S\mathrm{d}T - p\mathrm{d}V$ | $\mathrm{d}G \leq -S\mathrm{d}T + V\mathrm{d}p$ |
| 偏导数关系 | $$\mathrm{d}U = (\frac{\partial U}{\partial S})_V\mathrm{d}S + (\frac{\partial U}{\partial V})_S\mathrm{d}V$$ | $$\mathrm{d}H = (\frac{\partial H}{\partial S})_{p}\mathrm{d}S + (\frac{\partial H}{\partial p})_{S}\mathrm{d}p$$ | $$\mathrm{d}F = (\frac{\partial F}{\partial T})_{V}\mathrm{d}T + (\frac{\partial F}{\partial V})_{T}\mathrm{d}V$$ | $$\mathrm{d}G = (\frac{\partial G}{\partial T})_{p}\mathrm{d}T + (\frac{\partial G}{\partial p})_{T}\mathrm{d}p$$ |
| 麦克斯韦关系 | $(\frac{\partial T}{\partial V})_S = -(\frac{\partial p}{\partial S})_V$ | $(\frac{\partial T}{\partial p})_S = (\frac{\partial V}{\partial S})_p$ | $(\frac{\partial S}{\partial V})_T = (\frac{\partial p}{\partial T})_V$ | $(\frac{\partial S}{\partial p})_T = -(\frac{\partial V}{\partial T})_p$ |

偏导数关系: (其实就是准静态过程的偏微分关系的东西.)
* $$(\frac{\partial U}{\partial S})_{V} = T$$, $$(\frac{\partial U}{\partial V})_{S}\mathrm{d} = -p$$
* $$(\frac{\partial H}{\partial S})_{p} = T$$, $$(\frac{\partial H}{\partial p})_{S} = V$$
* $$(\frac{\partial F}{\partial T})_{V} = -S$$, $$(\frac{\partial F}{\partial V})_{T} = -p$$
* $$(\frac{\partial G}{\partial T})_{p} = -S$$, $$(\frac{\partial G}{\partial p})_{T} = V$$

热容: ($S,T$的关系)
* $C_V = (\frac{\partial U}{\partial T})_V = (\frac{\partial U}{\partial S})_V(\frac{\partial S}{ \partial T})_V = T(\frac{\partial S}{\partial T})_V \Rightarrow (\frac{\partial S}{\partial T})_V = \frac{C_V}{T}$
* $C_p = (\frac{\partial H}{\partial T})_p = (\frac{\partial H}{\partial S})_p(\frac{\partial S}{\partial T})_p = T (\frac{\partial S}{\partial T})_p \Rightarrow (\frac{\partial S}{\partial T})_p = \frac{C_p}{T}$



### 循环与热机
一个系统从某一个状态出发, 经过一系列的过程回到原来的状态. (看作是广义坐标空间下的一条闭合的曲线? )

热机效率: $\eta = \frac{W}{Q_{in}}$, 冷机效率: $\eta = \frac{Q_{in}}{W}$, 对于**卡诺循环**$\eta = \frac{T_h - T_l}{T_h}$(热机), $\eta = \frac{T_l}{T_h - T_l}$(冷机). 以及**奥托循环**$\eta = 1 - \frac{1}{(\frac{V_{max}}{V_{min}})^{\gamma - 1}}$, 和狄塞尔循环. 

> 卡诺定理: 任意两个固定温度之间的可逆机效率和卡诺机相等

将一半物质内能和体积的关系与物质状态联系在一起: 

$$(\frac{\partial U}{\partial V})_T = T (\frac{\partial p}{\partial T})_V - p$$

(证明方法: 构造微小(小得足够看作是一个平行四边形)卡诺正循环. $\delta W_{out} = \delta p \delta V, \delta Q_{in} = \delta U + \delta W_{in} = \delta V (p - \frac{1}{2}\delta p) + \delta U$. 然后考虑可逆机效率和卡诺机相等(卡诺定理), 得到$\eta = \frac{\delta T}{T} \Rightarrow T \frac{\delta p}{\delta T} = p + \frac{\delta U}{\delta V}$)

### 微观图像

$$\delta U = (\sum E \delta a_l^*)_{传热} + (\sum a_l^* \delta E)_{做功}$$

$$\begin{array}{l}\mathrm{d}W = \sum_\lambda (\hat{Y}_\lambda)_{广义力} (\mathrm{d}y_\lambda)_{广义位移} \\= \sum a_l^* \sum \frac{\partial E_l}{\partial y_\lambda} \mathrm{d} y_\lambda = \sum_\lambda (\sum_l \frac{\partial E_l}{\partial y_l} a_l^*)_{能量变化} \mathrm{d}y_\lambda\end{array}$$

对于广义力的计算, 利用配分函数可以计算. $$\bar{Y}_\lambda = \frac{N}{Z} \sum\frac{\partial E_l}{\partial y_\lambda} e^{-\beta E_l} \Delta \omega_l = - \frac{N}{\beta}\frac{\partial}{\partial y_\lambda}\ln Z$$. 

## 热力学第二定律
* 克劳修斯表述: 不可能把热量从低温物体传到高温物体而不引起任何其他变化
* 开尔文表述: 不可能从单一热源吸取热量使之完全转变为有用的功而不产生其他影响.    
  或: 第二类永动机(从单一热源吸热对外做功但不产生其他任何影响的机械)是不可能造成的. 

克劳修斯表述和开尔文表述完全等价. (证明思路: 反证法. )

### 数学表述
卡诺定理: 
* 相同的高温热源和相同的低温热源之间工作的一切可逆热机效率相等. 与工作物质无关
* 不可逆热机的效率$1 - \frac{Q_2'}{Q_1'} = \eta' < \eta = 1 - \frac{T_2}{T_1}$

克劳修斯不等式: 
* $\oint\frac{\bar{d}Q_{吸热}}{T} \leq 0$
* $\sum\frac{Q_{吸热_i}}{T_i} \leq 0$

### 可逆过程
* (仅)准静态过程都是可逆过程
* 真空自由膨胀不是可逆过程    
  $\Delta S_{free} = \nu R \ln \frac{V}{V_0} > 0$
* 实际过程: (非准静态, 有耗散, 相不平衡过程)都是不可逆过程

### 熵
对可逆过程, $\oint\frac{\bar{d}Q_{吸热}}{T} = 0$, 是一个和路径无关的态函数. 所以$S = \int_R \frac{\bar{d}Q_{吸热}}{T} + S_0$, 于是$\mathrm{d} S = \frac{\bar{d}Q_{吸热}}{T}$

利用热力学第一定律, $T\mathrm{d}S = \mathrm{d}U + p \mathrm{d}V = \mathrm{d}H - V \mathrm{d}p$. 

应用**内能公式**(比较重要): $(\frac{\partial U}{\partial V})_T = T(\frac{\partial p}{\partial T})_V - p$, 焓公式: $(\frac{\partial H}{\partial p})_T = -T(\frac{\partial V}{\partial T})_p + V$, 于是可以得到熵的计算方法: 

* $S = \int\frac{C_V}{T}\mathrm{d}T+\int(\frac{\partial p}{\partial T})_V\mathrm{d}V$
* $S = \int\frac{C_p}{T}\mathrm{d}T-\int(\frac{\partial V}{\partial T})_p\mathrm{d}p$

放到理想气体里面: 
* $S(T,V) = C_V\ln\frac{T}{T_0}+\nu R\ln\frac{V}{V_0}$
* $S(T,p) = C_p\ln\frac{T}{T_0}-\nu R \ln\frac{p}{p_0}$

具体一点就是: 
* 可逆等温: $\Delta S = -\nu R\ln\frac{p}{p_0}$
* 可逆绝热: $\Delta S = 0$ (显然, 毕竟不进行热交换. )
* 可逆等压: $\Delta S = C_p \ln\frac{T}{T_0}$
* 可逆等体: $\Delta S = C_V\ln\frac{T}{T_0}$
* 可逆多方: $\Delta S = C_m\ln\frac{T}{T_0}$

**熵增加原理**: 在**绝热**过程中, $\Delta S \geq 0$. 
* 仅在绝热过程中成立(无物质交换)
* 熵定理: 态函数熵的存在性, 熵增加原理, 热力学温标的引入. 
* 热平衡条件: 熵取得极值, $\mathrm{d}S = 0$

#### 统计意义
玻尔兹曼熵: (和宏观的熵是一致的. )

$$S_B = k_B\ln\Omega$$

* 熵是系统宏观状态对应的微观状态的多少(无序程度)的度量
* 熵高对应的微观状态数目多, (于是根据微观状态等几率的假设), 微观状态数出现最多的宏观状态更容易出现. 反之则不容易出现. 
* 熵增加是从有序到无序的过程. 

用数学来表述就是: $\Omega' \geq \Omega$. 

统计意义: 孤立系统的自发过程总是从有序向无序的过度. 概率小的出现状态会向概率大的宏观状态过渡. 而不可逆过程在统计意义上, 更应该看作是一种概率小到可以忽略为零的一种几乎不可能(有逆过程)的过程. 

#### 化学势
在开放系统中$\mathrm{d}G = -S\mathrm{d}T + V\mathrm{d}p + \mu\mathrm{d}N$, 体现了因为粒子交换导致的能量变化. $\mu = (\frac{\partial G}{\partial N})_{T,p}$

即开放系统的热力学基本方程: $T \mathrm{d}S = \mathrm{d}U + \mathrm{d}V - \mu \mathrm{d}N$, 对于单相物质, 每个分子所含有的自由焓应该相等, 即$\mu = (\frac{\partial G}{\partial N})_{T,p} = const \Rightarrow \mu = \frac{G}{N}$. 

等温等压的条件下, 平衡条件: $\mathrm{d}G = 0\Rightarrow\mu_1 = \mu_2$, 即各项物质化学势相等. (直观理解就是物质(不同的相)的量的变化不会改变系统的能量. )

### 热力学第三定律
#### 热力学温标
* 与测温物质无关, 定义了两个温度的比值. 

### 物态
#### 液体
* 定义: 具有一定体积, 没有弹性, 没有一定形状的流动物体
* 分类: 
  * 范德瓦尔斯液体: 无电偶极矩
  * 极性液体: 有电偶极矩
  * 缔和性液体: 分子之间吸引力大, 结构稳固, 粘滞性大
  * 金属液体: 存在自由电子, 传热, 导电性能好
  * 量子液体: 粘滞性消失, 超流态等量子现象
  * 液晶: 固体液体过渡状态
* 研究思路: 
  * 稠密液体(临界点以上没有太大区别)
  * 无序固体(三相点附近)
  * 径向分布函数
* 性质
  * 难被压缩
  * 热膨胀 $\alpha = \frac{1}{V}(\frac{\partial V}{\partial T})_p$ 一般比固体大
  * 热容量: $C_p \approx 3R, C_V \approx C_p, C_V \neq C_p$
  * 热运动看作是在平衡位置附近的振动
  * 黏度: 温度越低, 黏度越大$\eta \approx \frac{1}{3}\bar{\nu}\bar{\lambda}\rho$
  * 导热: 一般较差(定域碰撞), 金属液体会比较好(自由电子热运动传热)
  * 扩散: 跳跃式扩散
* 表面性质: 
  * 表面张力(表面层内分子之间的相互作用): $f = \sigma \Delta l, W = \sigma S$
  * (表面张力)微观解释: $\sigma = (1-k)\frac{\Lambda_m}{N_A}(\frac{\rho N_A}{\mu})^{2/3}$, 其中$k$为分子表面的平均键, 一般为$0.7$
  * 表面自由能: $F_S = \sigma A_S$
  * 表面张力与温度关系: $\frac{\partial \sigma}{\partial T} = \frac{1 - k}{N_A^{1/3}}\frac{\mathrm{d}\Lambda_m}{\mathrm{d}T}(\frac{\rho}{\mu})^{2/3}$
  * 压強差: $\delta p = \sigma (\frac{1}{R_1} + \frac{1}{R_2})$
* 浸润现象
  * 浸润角: $\theta = \pi$完全不浸润(形成一个球), $\theta = 0$完全浸润(摊开). 
  * 液(2)固(1)气(3)浸润角: $\cos \theta = \frac{\alpha_{13} - \alpha_{12}}{\alpha_{23}}$
  * 热力学函数的考虑: 即总表面自由能最小. (记一个球冠面积公式: $S = 2\pi R h$)
  * 带气泡的粗糙表面: $F = \alpha_{sg}A_{sg} + \alpha_{sl}A_{sl} + \alpha_{gl}A_{gl} \Rightarrow \cos \theta = f_{湿润比值} - 1 + r_{表面粗糙度}f\cos\theta_Y$

### 相平衡
#### 稳定条件
不同的情况: 
* 体积涨落: $(\frac{\partial^2G}{\partial V^2})_S \geq 0 \Rightarrow \kappa_S = -\frac{1}{V}(\frac{\partial V}{\partial p})_S \geq 0$, 即绝热压缩系数大于零. 
* 熵涨落: $(\frac{\partial^2 U}{\partial S^2})_V \geq 0 \Rightarrow C_V > 9$
* 粒子数涨落: $C_V > 0, \kappa_T \geq 0, \frac{\partial \mu}{\partial N} > 0$
* 液气杠杆原理: 
  * 体积分数: $x_G (V_G - V) = x_L (V - V_L)$, $V$为平衡时体积, $x$为体积分数. 即平衡时体积分数之比类似杠杆分布. 
  * 自由能: $\frac{F - F_G}{V_G - V} = \frac{F_L - F_G}{V_G - V_L}$, 即在相平衡的时候, 总摩尔自由能$F$在$F_G$, $F_L$的连线上. (利用了体积的杠杆原理)
  * 失稳分解: $F-V$为W型曲线, 不稳定的中间凸起会向两边走. 
  * 成核长大: 
  * 沸腾与凝结
  * 临界温度: $\Delta T_c = \frac{2\alpha \rho V_g T}{R \Lambda (\rho_l - \rho_g)}$

相平衡: $\mu_1 = \mu_2$

#### 相变
* 克拉伯龙方程: $\frac{\mathrm{d}p}{\mathrm{d}T} = \frac{\Lambda_m}{T(V_\beta = V_\alpha)}$
* 饱和蒸气压: $\ln\frac{p}{p_0} = \frac{\Delta H^{mol}(T_0) - \Delta C_p^{mol}T_0}{RT_0} (1-\frac{T_0}{T}) + \frac{\Delta C_p^{mol}}{R \ln\frac{T}{T_0}}$

## Examples
### 声速确定绝热指数
$$a = \sqrt{\frac{\partial p}{\partial \rho}}$$

用绝热过程: 

### 大气层压力随温度高度的变化
#### 等温大气
对于等温大气, 考虑的是玻尔兹曼分布: 

$$p = p_0 e^{-\frac{m g z}{k_B T}}$$

#### 绝热大气模型
利用绝热方程: 

$$p^{1-\gamma}T^{\gamma} = p_0^{1-\gamma} T_0^{\gamma} \Rightarrow \frac{\mathrm{d}T}{T} = \frac{\gamma - 1}{\gamma}\frac{\mathrm{d}p}{p} = - \frac{\gamma - 1}{\gamma}\frac{M g}{R T}\mathrm{d}z$$

$$\Rightarrow \begin{array}{lll} T & = & T_0 (1 - \frac{M g}{R T_0}\frac{\gamma - 1}{\gamma} z)\\ p & = & p_0 (1 - \frac{M g}{R T_0}\frac{\gamma - 1}{\gamma} z)^{\frac{\gamma}{\gamma - 1}}\end{array}$$

#### 饱和绝热模型
气体是饱和气体, 会发生相变. 
* 考虑变化过程中会出现有相变:    
  $$C_V \mathrm{d}T = -\Lambda \mathrm{d}\nu - (p \mathrm{d}V)_{= R \mathrm{d}T - V \mathrm{d}p}$$    
  $$\begin{array}{llll}\Rightarrow & C_p \mathrm{d}T & = & - \Lambda \mathrm{d}\nu + R T \frac{\mathrm{d}p}{p}\\ \Rightarrow^{\frac{\mathrm{d}p}{p} = -\frac{M g}{R T} \mathrm{d}z} & C_p \mathrm{d}T & = & - \frac{\gamma - 1}{\gamma}(\frac{M g}{R} + \frac{\Lambda}{R} \mathrm{d}z) \end{array}$$
* 简单做法, 不考虑相变对过程的改变     
  思路就是把$\frac{\mathrm{d}\nu}{\mathrm{d}z}$的量看作是一个常数来搞. 

### 范德瓦尔斯气体内能

$$\begin{array}{llll} & p & = & \frac{\nu R T}{V - \nu b} - \frac{\nu^2 a}{V^2}\\ \Rightarrow & \frac{\partial U}{\partial V} & = & T \frac{\partial p}{\partial T} - p = \frac{\nu^2 a}{V^2} \\ \Rightarrow & U & = & \int \frac{\partial U}{\partial T} \mathrm{d}T + \int \frac{\partial U}{\partial V} \mathrm{d}V \\ & & = & C_V T - \frac{\nu^2 a}{V} + U_0\end{array}$$

### 卡诺定理
#### 不等式的应用
> $T_1$, $T_2$的物体最终变成$T'$, 过程等压$C_p$. 证明$W = C_p (T_1 + T_2 - 2T') \leq C_p (T_1 + T_2 - 2\sqrt{T_1 T_2})$

$\mathrm{d}Q_{吸热} = C_p \mathrm{d}T_1, \eta = \frac{\mathrm{d}W}{\mathrm{d}Q_{吸热}} \leq 1 - \frac{T_2}{T_1} \Rightarrow \mathrm{d}W = C_p(\mathrm{d}T_1 - \mathrm{d}T_1) \leq -(1 - \frac{T_2}{T_1})C_p\mathrm{d}T_1$, 然后积分可以得到结论. 

#### 克拉伯龙方程
> 相变中的压强和温度的关系. 

考虑$Q_{吸热} = \delta \nu \Lambda^{mol}$, $$\delta V = \delta\nu(V^{mol}_\beta - V^{mol}_\alpha)$$, 假如是可逆过程: $$\eta = \frac{\delta W}{Q_{吸热}} = \frac{\delta T}{T} = \frac{\delta p \delta V}{\delta \nu \Lambda^{mol}} \Rightarrow = \frac{\mathrm{d}p}{\mathrm{d}T} = \frac{\Lambda^{mol}}{T(\Lambda_\beta^{mol} - V_\alpha^{mol})}$$

于是可以用这个公式来反推熔点随压强的变化率: $\frac{\mathrm{d}T}{\mathrm{d}p} = \frac{T(\V_{液态}^{mol} - \V_{气态}^{mol})}{\Lambda^{mol}}$

#### 熵变的计算
> 自由膨胀的熵变

用理想气体来看的话, 就是$\Delta S = \int\frac{C_V}{T}\mathrm{d}T+\int\frac{\nu R}{V}\mathrm{d}V$, 初始状态和终止状态分别对应$V, V_0$, 温度不变的话. 那么久可以得到$\Delta S = \nu R \ln\frac{V}{V_0}$. 

而从统计的角度来看: 把空间看作是$V_1$, $V_2$的两个区域(一开始在$V_1$里面. ), 然后自由膨胀后充满了$V_1+V_2$的空间. 于是膨胀过程就是从$\Omega = 1$(只能处于一个状态, 即所有粒子都在$V_1$的里面的状态. )变成了既可能在$V_1$, 也可能在$V_2$的状态. 比如(仍然在原来的空间中)概率为$\frac{V_1}{(V_1+V_2)}$. $S_B = k_B \ln\frac{\Omega'}{\Omega} = k_B \ln (\frac{V_1+V_2}{V_2})^N = \nu R \ln \frac{V}{V_0}$. 

> 气体混合过程的熵变. 

> 热传递过程的熵变. 

> 三个相同物体, 热容$C$, 温度分别为$T_A=T_B=300K,T_C=100K$, 利用热机来, 求最终的温度. 

热力学第二定律: $C\Delta T_A + C\Delta T_B + C\Delta T_C = 0 \Rightarrow T_A' + T_B' + T_C' = T_A + T_B + T_C$

然后考虑三个物体没有和外界相互交换, 所以看作孤立系统, 有: $\Delta S = \int_A \frac{C\mathrm{d}T}{T} + \int_B \frac{C\mathrm{d}T}{T} + \int_C \frac{C\mathrm{d}T}{T} = \ln\frac{T_A'}{T_A} + \ln\frac{T_B'}{T_B} + \ln\frac{T_C'}{T_C} = 0 \Rightarrow T_A'T_B'T_C' = T_AT_BT_C$. 

于是可以解出可能的解. 

## 后记
JT公式是啥来着? 危, 考试的时候完全忘了... 计算出来的一些结论和我的直觉又有一些出入. 就是怪. 

不管了. 至少是考完了. 