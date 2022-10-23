---
layout: post
title:  "理论力学 - 期中"
date: 2022-10-23 18:15:11 +0800
math: true
categories: notes
---
# 理论力学 - 期中
这必须感谢大一的力学课, 目前学到的东西基本在赵爹课上都有讲过. 
只是讲得更加细致一些. 

## 数学
1. 泰勒展开:  

   $$f(x_0 + x) = \sum_{k=0}^\infty \frac{f^{(k)}}{k!}(x - x_0)^k$$
   
   * $e^x = \sum_{k=0}^\infty \frac{x^k}{k!}$
   * $sin x = \sum_{k=0}^\infty (-1)^k \frac{x^{2k+1}}{(2k+1)!}$

2. 导数:  
   * $\frac{\partial f}{\partial x} = \frac{\partial f}{\partial y} \frac{\partial f}{\partial x}$: 链式法则
   * $\frac{\partial f}{\partial \boldsymbol{s}} = \nabla f \cdot \frac{\boldsymbol{s}}{s}$: 方向导数  
	 推广: $\nabla f = \frac{\partial f}{\partial \boldsymbol{r}}$
   * $\nabla$: 三角算子, 需要了解它在不同坐标系下的表现, 注意其矢量性和算符性即可. 
   * $\nabla, \nabla \cdot, \nabla \times$: 梯度, 散度, 旋度
 
3. 坐标系及其变换:  
   **指标**: (详细的操作可以去参考之前的[笔记]({{ site.github.url }}/notes/tensor-with-general-bases/), 之后可能会有更新. 或者去找别的, 比如[ustc](http://staff.ustc.edu.cn/~xtao/courses/ce/downloads/cc.pdf) (之前的笔记写得太水了, 还是不看了... ))  
   
   (注: 这里按照的是上面的 [ustc](http://staff.ustc.edu.cn/~xtao/courses/ce/downloads/cc.pdf) 的规定. 可以看看下面的 [附录: 曲线坐标系](#附录-曲线坐标系), 这里就不写了. )

   * 球坐标系
   * 柱坐标系
   * 极坐标系
   * 自然坐标系

## 牛顿
### $\boldsymbol{F} = m \boldsymbol{a}$
写了公式之后剩下的基本就是数学了. 比如 $\boldsymbol{a}$ 不同坐标系下的展开. 
比如展开完了之后的运算. 

在 $\boldsymbol{a}$ 的展开里面, 主要就是对基底的求导: 

虽然可以通过 $$\dot{\boldsymbol{e}}_{q_j} = \frac{\partial}{\partial t}(\frac{1}{h_{q_j}} \frac{\partial r_i}{\partial q_j}) e_i$$ 来计算, 
但是并不是很方便的感觉... 

常见的有: 

* 极坐标系 (柱坐标系): $\boldsymbol{a} = (\ddot{r} - r \dot{\theta}^2)e_r + (r^2\ddot{\theta} + 2 \dot{r}\dot{\theta})e_\theta$
* 球坐标系: 好复杂啊... 

### 守恒定律
* 动量 $\frac{\mathrm{d}\boldsymbol{p}}{\mathrm{d}t} = \boldsymbol{F}$
* 角动量 $\frac{\mathrm{d}\boldsymbol{L}}{\mathrm{d}t} = \boldsymbol{r} \times \boldsymbol{F}$
* 动能 $\frac{\mathrm{d}V}{\mathrm{d}\boldsymbol{r}} = \boldsymbol{F}$

在拉格朗日方程里面, 守恒定律对应着运动积分: 
* 可遗坐标: 
  $\frac{\partial L}{\partial q} = 0 \Leftrightarrow \frac{\mathrm{d}}{\mathrm{d} t} (\frac{\partial L}{\partial \dot{q}}) = 0$  
  对应的 $p = \frac{\partial L}{\partial \dot{q}}$ 为广义动量积分. 
* 广义能量积分: 
  $\frac{\mathrm{d} H}{\mathrm{d} t} = -\frac{\partial L}{\partial t}$

## 拉格朗日

$$\frac{\mathrm{d}}{\mathrm{d}t}(\frac{\partial L}{\partial \dot{q}_i}) - \frac{\partial L}{\partial q_i} = 0$$

或者写成最小作用量原理的形式: 

$$S = \int_{t_1}^{t_2} L \mathrm{d}t \Rightarrow \delta S = 0$$

利用 **欧拉方程** (和 $e^{i\theta}$ 那个不一样) 有: 

$$\delta S = 0 \Leftrightarrow \frac{\partial F}{\partial y} - \frac{\mathrm{d}}{\mathrm{d} t} (\frac{\partial F}{\partial y'})$$

### 约束
对于 $n$ 个质点的系统, 有 $s$ 个约束, 那么系统的自由度为 $3n-s$. 
即每增加一个约束, 减少一个自由度. 

利用拉格朗日乘子法可以计算得到约束力. 方法即是在 $L$ 中添加一个约束方程, 
比如约束满足的方程是 $G(x_i, \dot{x}_i, t)$, 那么就让 $L' = L + \lambda G$. 
这样的话, 再计算出结果反代入约束中解出 $\lambda$ 即可. 

### 虚功原理

对于一个约束: $f(\boldsymbol{r}, t) = 0$, 
其对应的约束力 $\boldsymbol{N} = \lambda \nabla f(\boldsymbol{r}, t)$, 
对一堆约束力, 有虚功原理: 

$$\sum \lambda_i \nabla f_i \cdot \delta\boldsymbol{r} + \sum \boldsymbol{F}_j \cdot \delta \boldsymbol{r}_j = 0$$

(实功不一定为零, 只有稳定约束才是, 
即 $-\lambda \frac{\partial f}{\partial t}\mathrm{d} t$)

**达朗贝尔原理**: 

$$\sum(\boldsymbol{F}_i - m_i \ddot{\boldsymbol{r}}_{i}) = 0$$

## 刚体
### 欧拉角
转动矩阵: (绕的始终是随体坐标系)

进动: (绕 $z$)

$$D = \left(\begin{array}{lll} \cos\phi & \sin\phi & 0\\ -\sin\phi & \cos\phi & 0\\ 0 & 0 & 1\end{array}\right)$$

章动: (绕 $x$)

$$C = \left(\begin{array}{lll} 1 & 0 & 0\\ 0 & \cos\theta & \sin\theta\\ 0 & -\sin\theta & \cos\theta\end{array}\right)$$

自转: (绕 $z$)

$$B = \left(\begin{array}{lll} \cos\psi & \sin\psi & 0\\ -\sin\psi & \cos\psi & 0\\ 0 & 0 & 1\end{array}\right)$$

上课时候用的转动矩阵顺序是 $A = BCD$

### 动力学方程
推导欧拉运动方程的时候, 除了矢量法 (emm... 几何不太好, 所以不会); 
可以使用代数一点的方法来计算: 

对基底的变换来做. (刚体坐标系)

$$(\boldsymbol{\omega} \times \boldsymbol{r}_0) \mathrm{d} t = \boldsymbol{R}^{- 1}(\boldsymbol{R} (t + \mathrm{d} t) - \boldsymbol{R} (t)) \boldsymbol{r} \Rightarrow (\boldsymbol{\omega} \times) = \boldsymbol{R}^{- 1} \frac{\mathrm{d} \boldsymbol{R}}{\mathrm{d} t}$$
	 
(地面坐标系)

$$\boldsymbol{\omega} \times \boldsymbol{r} \mathrm{d} t = (\boldsymbol{\omega}\times) \boldsymbol{R} \boldsymbol{r}_0 \mathrm{d} t = (\boldsymbol{R} (t + \mathrm{d} t) - \boldsymbol{R} (t)) \boldsymbol{r}_0 \Rightarrow (\boldsymbol{\omega}\times) = \frac{\mathrm{d} \boldsymbol{R}}{\mathrm{d} t} \boldsymbol{R}^{- 1}$$

一些更加有用的东西: (是指更多用来算的东西, 不是说好不好用)

$$\boldsymbol{J} = \sum m_i (\boldsymbol{\omega} \boldsymbol{r}_i^2 -\boldsymbol{r}_i(\boldsymbol{\omega}\cdot \boldsymbol{r}_i))$$

$$\boldsymbol{I}_{ij} = \int_V \rho(r^2\delta_{ij} - x_ix_j)\mathrm{d}V$$

## 中心力场
**比内方程**: 

$$h^2u^2(\frac{\mathrm{d}^2 u}{\mathrm{d} \phi^2} + u) = -\frac{F}{m'}$$

其中 $h$ 为角动量, $u = \frac{1}{r}$, 其实证明并不是很难, 关键就在于如何把
$\frac{\mathrm{d} }{\mathrm{d} t} \rightarrow \mathrm{d}\phi$, 
就是利用角动量守恒以及来改变求导的底. 

(为了防止忘记一些结论: $r = \frac{p}{1-\varepsilon \cos\phi}$, 
上面的 $p$ 叫做焦点参数, $\varepsilon$ 叫做离心率. )

## Exercise
* 坐标系及其变换
  
  > 写出运动坐标
  
  > 写出坐标变换
  
* 利用虚功原理计算广义力  
  虽然耍流氓的方式就是, 简单的问题全部用牛顿法算一遍, 然后检验.  
  **需要注意的是, 对于虚功原理, 需要确认作用力和自由的坐标**.  
  (虽然不知道这样是否严谨, 但是如果想要得到约束力的话, 
  就要破坏约束力对应的约束, 使其重新变成一个自由坐标, 但是破坏之后, 
  对应的运动约束应该是什么样的, 就有点不好搞了. )  
  (所以最理想的方式还是 $\lambda \nabla g$ 的形式. )

* 计算 $L$ 以及通过 $L$ 来计算运动方程.  
  这个就... 直接 $L = T - V$ 来算就好了. 算的时候注意有没有把一些已经存在的, 
  显而易见的约束给不小心代入了. 比如 $x^2 + y^2 = r^2$ 直接带掉了之类的. 
  代入了之后可能会忘了, 导致求约束的时候自己给忘了. 
  
* 欧拉角的转动
* 刚体  
  (一个流氓的办法: 如果算不来刚体的转动结果, 没准直接把刚体看作是质点组, 
  没准估计算得出来. 虽然是下策. )

## 附录: 曲线坐标系
(别问我为何要这样, 明明考试前还要这般作死. 因为应该是考后写的. )
 
### 基矢
将向量用 $\boldsymbol{r} = x e_x + y e_y + z e_z$ 来表示. 对于一个坐标系
$(x^1, x^2, x^3)$, 用 $\boldsymbol{r} = x(x^i) e_x + \cdots$ 的形式来表示. 

这样的好处是在进行坐标变换的时候, 只需要对基底进行变换即可. 
比如在计算转动系的欧拉运动方程的时候, 就是用这样的对基底进行变换的方法来解决问题. 
而这个基底就可以和直角坐标系的坐标轴类比, (虽然它可能是弯的). 
对于位移投影, 就是 $\nabla f \cdot \boldsymbol{e}_i$, 于是写成简单的形式就变成了: 
$\frac{\partial f}{\partial x_i} \boldsymbol{e}_i$. 

定义基矢量: $e_i = \frac{\partial \boldsymbol{r}}{\partial x^i}$, 
定义逆基矢: $e^i = \frac{e_j \times e_k}{e_1 \cdot (e_2 \times e_3)}$, 
其中 $V = e_1 \cdot (e_2 \times e_3)$ 来对基矢归一化. 
也同时等于 $V = e^1 \cdot (e^2 \times e^3)$. 
并且有: $e^i = \nabla x^i$. 

之所以定义逆基矢, 应该是为了有能得到像直角坐标系下正交归一的结果的想法. 
即: $$\boldsymbol{e}_i \boldsymbol{e}_j = \delta_{ij}$$. 这样的结果. 

### 度规
定义度规系数: 

$$g_{ik} = e_i \cdot e_k = g_{ki}, g^{ik} = e^i \cdot e^k = g^{ki}$$

度规张量: 

$$G = g_{ik} e^i e^k = g^{ik} e_i e_k$$

> 爱因斯坦约定: (稍微和平时讲的有点点不一样)  
> 一对求和指标总是一上一下

用前面定义的基矢量: 

$$\mathrm{d} x^i e_i = \mathrm{d} x_j e^j \Rightarrow \mathrm{d} x^i = \mathrm{d} x_j g^{ij}, \mathrm{d} x_j = \mathrm{d} x_i g_{i j} \Rightarrow g^_{jk}g_{ik} = \delta_i^j$$

### 剩下的? 
嗯, 之后再写吧... (逃)

~~毕竟摆烂更重要~~
