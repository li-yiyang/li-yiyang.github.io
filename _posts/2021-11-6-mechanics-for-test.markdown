---
layout: post
title:  "Mechanics Preparing for Mid-Term Death"
date:   2021-11-06 11:25:46 +0800
categories: notes
math: true
---
# Mechanics Preparing for Mid-Term Death
> 再回首, 背影已远走  
> 再回首, 眼泪 朦胧  
> ...  
> 曾经在幽幽暗暗反反复复中追问  
> 然后才知平平淡淡从从容容才是真  

开始回顾一下我学过的力学, 时间有限, 还是先想到哪里就是哪里. 
然后在做一点补充(看笔记). 

## $\vec{F} = m \vec{a}$
牛顿是$\vec{F} = \frac{\mathrm{d}\vec{p}}{\mathrm{d}t}$
这样写的, 至于后面是Match写成了$\vec{F} = m \vec{a}$的形式. 

这样的形式实际上在相对论情况下也不是不能用, 
只要把质量从静止质量改成动质量就好: $m = \gamma m_0$
其中里面的$\gamma$是相对论的一个系数. 

**相对论洛伦兹变换**: (沿$x$轴方向运动)

$$
  x' = \gamma (x - u t)\\
  y' = y\\
  z' = z\\
  t' = \gamma (t - \frac{u x}{c^2})\\
  \gamma = \frac{1}{\sqrt{1-(\frac{v}{c})^2}}
$$

类似的, 对于转动的物体, 可以有相同的方程:   
$$ \vec{M} = \mathcal{I} \ddot{\theta}$$
(其中$\mathcal{I}$是**转动惯量**, 是一个二阶张量, 
实际上就是一个
$\int (\mathcal{I} - \vec{r} \otimes \vec{r}) \mathrm{d} V$
的积分. 

牛顿定律满足时间反演, 线性和简单性. 
> 时间反演就是$t \rightarrow -t$
> 然后把$i \rightarrow -i$
> (波函数里面用)

## 算符
算符就是一种对数学操作的抽象记号, 比如说:  
$$\nabla = \frac{\partial}{\partial e_i} \hat{e}_i$$  
就是这样的一种抽象. 

$\nabla$有矢量性和算符性, 可以这样子进行运算. 
当然也可以写成分量的形式用指标运算来做. 

量子力学中也有算符, 如动量和能量的算符: 
$$\hat{p} = -i \hbar \nabla$$
$$\hat{E} = i \hbar \frac{\partial}{\partial t}$$

推导方式就是拿波函数
$\psi = e^{\frac{i}{\hbar}(\vec{p}\cdot\vec{r} - E t)}$
对时间求导, 对位置求导即可得到. 

## 指标运算基础
点乘和叉乘满足下面的运算:  
$$ \hat{e}_i \cdot \hat{e}_j = \delta_{i j}$$
$$ \hat{e}_i \times \hat{e}_j 
  = \varepsilon_{i j k} \hat{e}_k$$

然后对于Levi-Civita Symbol$\varepsilon_{i j k}$:  
$$\varepsilon_{i j k} \varepsilon_{i m n}
  = (\delta_{j m}\delta_{k n}- \delta_{j n}\delta_{k m})$$  
这个公式可以用行列式来理解, 看成是: 
$$ \varepsilon_{a_1, a_2, a_3} \varepsilon_{b_1, b_2, b_3}
  = \det (\delta_{a_i b_j}) $$

## 平均思想
位力定律
$$ 2 \bar{K} = \lambda \bar{U} $$

### 用位力定律得到理想气体方程
首先能均分定理得到动能应该是  
$$K = 3 N \times \frac{1}{2} k_B T$$

考虑位力中的力项为$\mathrm{d} \vec{F} = p \mathrm{d} \vec{A}$
$$\bar{\vec{r} \cdot \vec{F}} 
  = \sum \vec{r}_i \cdot (p \mathrm{d} \vec{A})$$
因为在内部的是对称的, 所以位力累加为零, 
接下来就只需要考虑在表面的位力项, 所以就相当于是对表面考虑, 
变成对表面积分. 面积分化作体积分即可. 
$$\bar{\vec{r} \cdot \vec{F}}
  = p \oint_S \vec{r} \cdot \mathrm{d} \vec{A}
  = p \int_V \nabla \cdot \vec{r} \mathrm{d} V 
  = 3 p V$$

### 相对论谐振子中对质量取平均的方法.

## Maxwell's Equations & Basic Group Theory
$$ 
  \nabla \cdot E = \frac{\rho}{\varepsilon}\\
  \nabla \cdot B = 0 \\
  \nabla \times E = - \frac{\partial B}{\partial t}\\
  \nabla \times B 
    = \mu_0 
      (J + \varepsilon_0 \frac{\partial E}{\partial t})
$$

实际上就是要对$\nabla$算符的运算要精通. 

$$ \nabla v = 0 $$

多思考这些公式的含义. 

## 对称和守恒
对称性和守恒定律相联系

经典力学就是满足伽利略变换不变的性质. 

## Oscillator
谐振子模型
$$\ddot{q} = \omega^2 q$$

## 量纲分析
单位上的一致, 对(时间)求导的阶数一致, 对协变和逆变的一致. 

## 感觉自己太蠢了
这个复习感觉没什么用了, 所以就记这么点吧. 

(实际上是因为时间来不及了)

## 考后
我是笨蛋. 

这张试卷感觉重新定义了"简答题", 
总共八道每题五分的简答题让我感到了世界的冷漠. 
> 写出麦克斯韦方程组并由此证明电荷守恒以及真空电磁波方程

对不起, 我推导错了, 符号反了. 

罢了, 只愿能低空飞过, 不求人上人了. 