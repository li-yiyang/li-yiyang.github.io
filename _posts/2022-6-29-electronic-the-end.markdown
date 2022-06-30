---
layout: post
title:  "Electromagnetism The End"
date: 2022-06-30 11:55:47 +0800
math: true
categories: notes
---
# Electromagnetism The End
后半学期主要是在讲磁介质和电路, 略带一提电磁波. 

## 介质 - 以磁介质为主
### 磁介质
(可能的一种)微观解释是分子电流: $\vec{M} = \frac{\sum \vec{m}_{分子}}{\delta V}$, 于是比较理论的一种计算方法就是通过计算$\vec{m}$来计算$\vec{M}$, 然后得到$\vec{B} = \vec{B}_0 + \mu_0 \vec{M}$. 

$$\vec{B} = \vec{B}_0 + \mu_0 \vec{M} = \mu \vec{M}$$

#### 磁荷观点

$$\vec{H} = \frac{1}{4\pi \mu_0}\frac{q_m}{r^2} \hat{e}_r$$

> 无限大均匀磁荷面: $H = \frac{\sigma}{2\mu_0}$

和电场类比

> 磁偶极子: $\vec{p}_m = q_m \vec{l}$

和电场类比, 受到的力都是$\vec{F} = - \vec{p}_m \cdot \vec{H}, - \vec{p} \cdot \vec{E}$. (对于磁矩: $\vec{m} = i \vec{S}$, 收到的力矩为: $\vec{m} \times \vec{B}$)

#### 磁路
将磁路和电路类比: $\varepsilon = N I_0 = \oint_L \vec{H} \cdot \mathrm{d}\vec{l}$, $R = \frac{l}{\mu S}$, $\Delta \varepsilon = H l = \Phi_B R$, 于是可以得到$\varepsilon = \sum H_i l_i = \Phi_B \sum R_i$

> 一个闭合铁芯有一个小缺口, 求缺口的磁场强度和铁芯的有效磁导率.     
> (已知铁芯截面$S$, 缺口$l_2$长度, 铁芯长度$l_1$)

磁通量$\Phi_{B_0} = \frac{\varepsilon}{R_{m_0}} = \frac{N I_0}{R_{m_0}} = B_0 S, \Phi_{B} = \frac{\varepsilon}{R_{m}} = \frac{N I_0}{R_{m}} = B S$, 有效(相对)磁导率为$\mu = \frac{B}{B_0} = \frac{\Phi}{\Phi_0} = \frac{R_{m_0}}{R_0} = \frac{l_1 /(\mu\mu_0 S) + l_2 / (\mu_0 S)}{l / (\mu_0 S)}$. 

(注意到磁电动势是相等的. )

### 电介质

$$\vec{D} = \varepsilon \vec{E} = \varepsilon_0 \vec{E} + \vec{P}$$

## 电路 - 以交流电路为主
### 复数电路 - 矢量解法之类的略, 用处不大
将直流电路中的所有东西换成复数形式即可: 

$$\begin{array}{lll}\tilde{U} & = & U_0 e^{i( \omega t + \varphi)}\\\tilde{I} & = & I_0 e^{i( \omega t + \varphi)}\\ \tilde{Z_R} & = & R \\\tilde{Z_L} & = & i \omega L \\\tilde{Z_C} & = & \frac{1}{i \omega C}\end{array}$$

不过在交流电路中有一个麻烦的地方: 就是品质因数$Q = \frac{P_{无功}}{P_{有功}}$, 有功功率, 视在功率这些定义. 
* $Q$可以看作是$i$的功和Re的功的比. 
  * 周期能耗比: $Q = 2 \pi \frac{W_S}{W_R}$
  * 频率选择性: $\delta f = \frac{f_0}{Q}$ (滤波能力)
  * 电压分配(到电阻上的比): $Q = \frac{U_C}{U} = \frac{U_L}{U} = \frac{Z_C}{R} = \frac{1}{\omega_0CR} = \frac{Z_L}{R} = \frac{\omega_0L}{R}$
* 瞬时功率: $$P(t) = \mathrm{Re} (\tilde{U} \tilde{I}^*) = \frac{1}{4}(\tilde{U}\tilde{I}^* + \tilde{U}^*\tilde{I})$$
* 平均功率: $\bar{P} = \frac{1}{T}\int_0^TP(t)\mathrm{d}t$
* 视在功率: $S = U I = \sqrt{P_{有功}^2 + P_{无功}^2}$

### 电路模型
* 交流电桥: 直流电桥中, 桥所分割的两臂比值相等. 在交流电桥中亦成立: $$\frac{\tilde{Z}_{左1}}{\tilde{Z}_{左2}} = \frac{\tilde{Z}_{右1}}{\tilde{Z}_{右2}}$$    
  于是可以用来测量电感, 电容等. 
* 变压器: 
  * 电压变比公式: $$\frac{\tilde{U}_1}{\tilde{U}_2} = - \frac{N_1}{N_2}$$
  * 电流变比公式: $$\frac{\tilde{I}_1}{\tilde{I}_2} \approx - \frac{N_1}{N_2}$$    
    (实际的推导考虑电感互感. $$L_1 (\tilde{I}_1 + \tilde{I}_0) = - M_{21} \tilde{I}_2$$)
  * 电阻: $\tilde{Z}_1 = (\frac{N_1}{N_2})^2 \tilde{Z}_2$    
    (从变压器输入端看的阻抗. 于是和直流电路比较, 就能够得到阻抗的匹配: 即分到最大的电压. )
* 三相交流电: (麻烦的地方主要是定义)
  * 负载连接方法
    * 星形连接: 并联, 共地. 输入端和共地端电压差相等, 相位差$120^\circ$. 
    * 三角连接: 顾名思义, 两相之间电压差相等, 相位差$120^\circ$. 
    * 平均功率: $\bar{P} = 3 U_\varphi I_\varphi \cos \varphi$
* 电路的互感: 在基尔霍夫方程组中列方程的时候, 对于互感, 需要注意方向. 即同名端和异名端. 从同名端流入时, (电势降)为正号; 反之为负号. 

## 电磁波
* 能量密度: $w = \frac{1}{2}(\vec{E} \cdot \vec{D} + \vec{H} \cdot \vec{B})$
* 能流密度: $\vec{S} = \vec{E} \times \vec{B}$

## 之前的东西的复习
因为被离散数学背刺了, 所以也把前半学期学的复习一下. 

### 麦克斯韦方程组

$$\left\{\begin{array}{lll} \nabla \cdot \vec{D} & = & \rho_0\\ \nabla \cdot \vec{B} & = & 0 \\ \nabla \times \vec{E} & = & - \frac{\partial \vec{B}}{\partial t}\\ \nabla \times \vec{H} & = & \vec{J} + \frac{\partial \vec{D}}{\partial t}\end{array}\right.$$

边界条件可以通过构造微元来求出来. 

### 恒定电流 - 电路
在导体中, 有$\vec{j} = \sigma \vec{E}$, $\sigma$为电导率. 对导体电流的微观解释就是带电粒子的移动$\vec{j} = n q \vec{v}$(但是对超导体来说, 并不是这样的. ). 

对于电路的问题, 最简单粗暴的做法就是: 基尔霍夫方程组: 
* 电压降相等(沿电流方向, 从电源负极到正极电压升, 反之电压降; 沿电流方向通过电阻电压降. )
* 电流流入等于流出. 

### 电磁力

$$\vec{f} = q(\vec{E} + \vec{v} \times \vec{B})$$

### 电磁场的相对论变换

$$\begin{array}{lll}\vec{E}'_\parallel & = & \vec{E}_\parallel\\\vec{B}'_\parallel & = & \vec{B}_\parallel\\\vec{E}'_\perp & = & \gamma (\vec{E} + \vec{v} \times \vec{B})_\perp \\ \vec{B}'_\perp & = & \gamma (\vec{B} - \frac{1}{c^2}\vec{v} \times \vec{E})_\perp\end{array}$$

### 势能
* $$\varphi = \int_{V_0} \frac{1}{4\pi\varepsilon_0}\frac{\rho_0}{r}\mathrm{d}V$$
* $$\vec{A} = \frac{\mu}{4\pi} \int_V \frac{\vec{J}_0}{r}\mathrm{d}V$$
* $$\nabla^2 \varphi = -\rho/\varepsilon$$
* $$\nabla^2 \vec{A} = -\mu \vec{J}$$