---
layout: post
title:  "Elctronic"
date:   2022-04-29 09:54:23 +0800
math: true
categories: notes
---
# 电磁学
> 实验和逻辑思维的胜利

## 静电场
* 库伦定律: $\boldsymbol{F} = \frac{Q Q'}{4 \pi \varepsilon_0 r^3} \boldsymbol{r}$    
  关于库仑定律: 
  * 是否平方反比: 假如$F = \frac{Q Q'}{4 \pi \varepsilon_0} \frac{1}{r^{2+\delta}}, \delta \neq 0$, 那么对其用级数展开. 
  * 超距作用和场的相互作用
  * 场: 场是一种物质, 有一种空间的概念, 电场是一个受力电荷在空间中的作用
* 高斯定理: $\oint_S \boldsymbol{E} \cdot \mathrm{d} \boldsymbol{S} = \frac{1}{\varepsilon_0} \int_V \rho \mathrm{d}V$    
  * 高斯定理依赖于平方反比定律     
    一个简单的说明: 
    
    $$\begin{array}{lll}\nabla (\frac{\boldsymbol{r}}{r^{n + 1}}) & = & (\nabla \frac{1}{r^{n + 1}}) \cdot \boldsymbol{r} + \frac{1}{r^{n + 1}} \nabla \cdot \boldsymbol{r}\\ & = & -\frac{n+1}{r^{n+2}}(\nabla r) \cdot \boldsymbol{r} + \frac{3}{r^{n+1}} \\ & = & \frac{2 - n}{r^{n+1}} = 0 \Leftrightarrow n = 2\end{array}$$

  * 散度的局域性质: 只有在有电荷分布的地方才有不为零的散度
* (真空)静电场的散度(高斯定理的微分形式): $\nabla \cdot \boldsymbol{E} := \lim_{V\rightarrow 0} \oint_S \boldsymbol{E} \cdot \mathrm{d} \boldsymbol{S} = \frac{\rho}{\varepsilon_0}$
* 静电场的旋度: $\oint_C \boldsymbol{E} \cdot \mathrm{d}\boldsymbol{l} = 0 \Leftrightarrow \nabla \times \boldsymbol{E} = \boldsymbol{0}$     
  说明静电场是无旋场, 也就是保守场, 存在标势. 
* 电势: $$\varphi(\boldsymbol{x}) = \int_V \frac{\rho(\boldsymbol{x}_{源})}{4 \pi \varepsilon_0 r} \mathrm{d}V_{源}$$     
  假如空间中的电荷分布给定, 就能够通过电势来确定电场$\boldsymbol{E} = -\nabla \varphi$

### 恒稳电流
* 电流密度: $I = \int_S \boldsymbol{j} \cdot \mathrm{d}\boldsymbol{S}$
* 电荷守恒的微分形式: $\nabla \cdot \boldsymbol{j} + \frac{\partial \rho}{\partial t} = 0$    
  (证明的话, 可以取高斯面然后对体积取极限)
* $\boldsymbol{J} = \sigma \boldsymbol{E}$

#### 电路
* 基尔霍夫环路定律
* 叠加原理
* 等效电流源和等效电压源
* 电桥, 星-三角变换

## 静磁场
* 毕奥-萨伐尔定律: $\frac{\mu_0}{4\pi}\int_{V} \frac{\boldsymbol{J}(\boldsymbol{x}') \times \boldsymbol{r}}{r^3} \mathrm{d}V' = \frac{\mu_0}{4\pi} \oint_L \frac{I \mathrm{d}\boldsymbol{l}\times\boldsymbol{r}}{r^3}$    
  关于毕奥-萨伐尔定律: 
  * 实验验证:      
    实验验证实际上经历了两个过程, 第一个过程是在垂直于通电直导线的平面上径向对称放置条形磁铁, 观察通电后的转动方向来通过力矩判断是否**平方反比**. 第二个过程则是通过将导线弯折成一个角, 在角平分线延长线上测量受力和角度的关系, 得到和**角度的关系**. 
  * 电流元是不满足牛顿第三定律的     
    这个是要对全回路来说才满足牛顿第三定律. 
* 环量与旋度: $\oint_L \boldsymbol{B}\cdot \mathrm{d}l = \mu_0 I \Leftrightarrow \nabla \times \boldsymbol{B} = \mu_0 \boldsymbol{J}$
* 散度: $\int_S \boldsymbol{B} \cdot \mathrm{d}S = 0 \Leftrightarrow \nabla \cdot \boldsymbol{B} = 0$

## 麦克斯韦方程组

$$\left\{\begin{array}{lll} \nabla \cdot \boldsymbol{E} & = & \frac{\rho}{\varepsilon_0} \\ \nabla \times \boldsymbol{E} & = & -\frac{\partial \boldsymbol{B}}{\partial t} \\ \nabla \cdot \boldsymbol{B} & = & 0 \\ \nabla \times \boldsymbol{E} & = & \mu_0(\boldsymbol{J} + \varepsilon_0 \frac{\partial \boldsymbol{E}}{\partial t}) \end{array}\right.$$

#### 洛伦兹力

$$\boldsymbol{F} = q(\boldsymbol{v}\times\boldsymbol{B} + \boldsymbol{E})$$

通过洛伦兹力可以推导出场的能量: 

$$\boldsymbol{F} \cdot \boldsymbol{v}_e = -\frac{\partial }{\partial t} \int_V w \mathrm{d}V \\ (\boldsymbol{v}_e \times \boldsymbol{B} + \boldsymbol{E}) \cdot q \boldsymbol{v}_e = q \boldsymbol{v}_e \cdot \boldsymbol{E} = \boldsymbol{E} \cdot \int_V \boldsymbol{J} \mathrm{d}V = -\frac{\partial }{\partial t} \int_V w \mathrm{d}V \\- \frac{\partial w}{\partial t} =  \boldsymbol{J} \cdot \boldsymbol{E} = (\nabla \times \boldsymbol{H} - \frac{\partial \boldsymbol{D}}{\partial t}) \cdot \boldsymbol{E} \\ \Rightarrow \frac{\partial w}{\partial t} = \nabla \cdot (\boldsymbol{E} \times \boldsymbol{H}) + \boldsymbol{E} \cdot \frac{\partial \boldsymbol{D}}{\partial t} + \boldsymbol{H}\cdot \frac{\partial \boldsymbol{B}}{\partial t}$$

其中$\boldsymbol{S} = \boldsymbol{E} \times \boldsymbol{H}$为能流密度, $\frac{\partial w}{\partial t} = \boldsymbol{E} \cdot \frac{\partial \boldsymbol{D}}{\partial t} + \boldsymbol{H}\cdot \frac{\partial \boldsymbol{B}}{\partial t}$为能量变化率. 

#### 相对论变换

$$\begin{array}{lll} \boldsymbol{E}_{\bot}' & = & \gamma (\boldsymbol{E} - \boldsymbol{v}\times\boldsymbol{B})_{\bot} \\ \boldsymbol{B}_{\bot}' & = & \gamma(\boldsymbol{B} - \frac{\boldsymbol{v}}{c^2} \times \boldsymbol{E}) \\ \boldsymbol{E}_{//}' & = & \boldsymbol{E}_{//} \\ \boldsymbol{B}_{//}' & = & \boldsymbol{B}_{//} \end{array}$$

平行于坐标系速度的分量不变, 垂直分量发生变化. 

## 电磁介质
极化和磁化的一个简单的解释: 分子本身的电偶和磁矩在外电场或磁场的作用下, 原本的电偶和磁矩的分布不再是均匀分布, 而是有一定的指向性, 于是产生了宏观的效应. 

### 电介质
* 取向极化: 空间中的电偶极子原本是各向同性角分布的, 但是在外场作用下出现了朝向分布的不均
* 位移极化: 电中心粒子原本正负电荷中心重合, 在外场的作用下出现了电荷中心位移, 产生了电偶极. 

* 电极化强度矢量: $\boldsymbol{P} = \frac{\sum_i \boldsymbol{p}_i}{\Delta V}$   
  即在空间中的分子电偶极子的宏观产生的效应. 这个时候的$\Delta V$应该有宏观无穷小, 微观无穷大的一种特点.     
  于是**穿出**面元的电荷大小为: $\boldsymbol{P}\cdot\mathrm{d}\boldsymbol{S}$, 所以对高斯定理进行一个修补即有: $\int_S \boldsymbol{E} = \frac{1}{\varepsilon_0}(\rho_0 - \int_S \boldsymbol{P}\cdot\mathrm{d}\boldsymbol{S})$, (上面的是符号的原因是因为前面的电荷大小是穿出的电荷大小, 留下的都是"负"电荷, 所以是负号. ), 变化为微分形式就是$\nabla \cdot \boldsymbol{D}= \nabla \cdot (\varepsilon_0\boldsymbol{E} + \boldsymbol{P}) = \rho_0$
* $\nabla \cdot \boldsymbol{D} = \rho_0$
* 对于各向同性的介质, 可以有极化强度矢量和外场成线性关系: $\boldsymbol{P} = \chi_e \boldsymbol{E} \Rightarrow \boldsymbol{D}=\varepsilon \boldsymbol{E} = (1 + \chi_e) \varepsilon_0 \boldsymbol{E} = \varepsilon_r \varepsilon_0 \boldsymbol{E}$

### 磁介质
* 原本粒子的磁矩空间中角分布各向同性, 但是在外场作用下出现指向一致性

> 注: 分子电流的解释并不一定总是适用, 因为分子电流解释不了电子磁矩之类的. 

* 磁偶极矩: $\boldsymbol{m} = i \boldsymbol{a}$, $\boldsymbol{a}$是微小面元. 
* 宏观磁偶极矩: $\boldsymbol{M} = \frac{\sum \boldsymbol{m}}{\Delta V}$
* 宏观磁化电流密度: $\int_S \boldsymbol{J}_M \cdot \mathrm{d} \boldsymbol{S}= \oint_L \boldsymbol{M} \cdot \mathrm{d} \boldsymbol{l} \Leftrightarrow \boldsymbol{J}_M = \nabla \times \boldsymbol{M}$    
  直观的图像就是把磁偶极子看作是一种环状电流, 然后电流相互抵消, 留下的净环状电流在积分环路上的作用就是磁化电流. 
* 极化电流密度: $\frac{\partial \boldsymbol{P}}{\partial t} = \frac{\sum e_i \dot{\boldsymbol{x}}}{\Delta V} = \boldsymbol{J}_P$     
  直观的图像就是电偶极子两端的电荷移动产生了电流. 
* $\nabla \times \boldsymbol{B} = \mu_0 (\boldsymbol{J} + \boldsymbol{J}_P + \boldsymbol{J}_M + \varepsilon_0 \frac{\partial \boldsymbol{E}}{\partial t}) \Rightarrow \nabla \times \frac{\boldsymbol{B}}{\mu_0} = \frac{\partial \rho_0}{\partial t} + \frac{\partial \boldsymbol{P}}{\partial t} + \nabla \times \boldsymbol{M} \Rightarrow \nabla \times \boldsymbol{H} = \nabla \times (\frac{\boldsymbol{B}}{\mu_0} - \boldsymbol{M}) = \boldsymbol{J} + \frac{\partial \boldsymbol{D}}{\partial t}$

### 边界条件
* 切向:   
  利用环路定理来做: $(F^{//}_i - F^{//}_o) \mathrm{d} l = j \mathrm{d} l$
  * 电场: $$
  * 磁场: 
* 法向:    
  利用高斯定理来做: $(\boldsymbol{F}^{\perp}_i - \boldsymbol{F}^{\perp}_o) \cdot \mathrm{d} \boldsymbol{S} = \sigma \mathrm{d} S$
  * 电场: $\oint \boldsymbol{D} \cdot \mathrm{d} \boldsymbol{S} \Rightarrow D^{\perp}_1 - D^{\perp}_2 = \sigma^{e}$
  * 磁场: $\nabla \cdot \boldsymbol{B} = 0 \Rightarrow B^{\perp}_1 = B^{\perp}_2$

## 电磁感应
磁生电: 

$$\varepsilon = \oint_L \boldsymbol{E} \cdot \mathrm{d} \boldsymbol{l} = -\frac{\partial}{\partial t} \int_S \boldsymbol{B} \cdot \mathrm{d} \boldsymbol{S} \\ \Leftrightarrow \nabla \times \boldsymbol{E} = - \frac{\partial}{\partial t}\boldsymbol{B}$$

位移电流和磁场: 

$$\nabla \times \boldsymbol{B} = \mu_0 (\boldsymbol{J} + \varepsilon_0 \frac{\partial \boldsymbol{E}}{\partial t})$$

