---
layout: post
title:  "Err, Long Time No See..."
date:   2021-11-09 21:30:02 +0800 
categories: jekyll update
math: true
---
# Long Time No See
好吧, 但是这个很正常的. 
具体的原因有很多, 解释起来很简单: 
忘了. 

这次我的关注点在于矢量和张量, 虽然高中就学过的东西, 
但是怎么说呢? 掌握的还不够好吧, 还是高中真的就什么也没有学, 
我现在发现我竟然对这本书(A Brief on Tensor Analysis)
里面的矢量很陌生, 虽然运算都还算行, 但是里面的理解和说法很新. 

目前学到了第二章, 但是有一部分没有搞清楚, 
所以这次就只是记录第一章的一些要点. 

(题外话: 外国本科undergraduate 的东西真是硬核, 
除了这本张量的教材, 里面还有什么Measure, 
Topology, and Fractal Geometry等等. )

## 张量和矢量的物理学意义
> The magic of this theory will hardly fail to 
> impose itself on anybody who has truly understood it;
> it represents a genuine triumph of the method of 
> absolute differential calculus, founded by
> Gauss, Riemann, Christoffel, Ricci and Levi-Civita.
> 
> -- Albert Einstein

在Einstein口中的"absolute differential calculus"
就是现在的"tensor anlysis"张量分析. 

作者认为: 想要将物理学定律和数学公式联系在一起, 
就会有一个"dichotomy": 若要定量地描述物理现象, 就要引入量, 
所以就需要"frame"
(类似于一个映射, 把物理世界和$E^3 \times \mathbb{R}$
联系在一起的一个数学结构? 这里有点没懂. )
和坐标系统来刻画. 但是物理原理是不随着坐标变化而变化的, 
所以原理应该又是一种"frame- and coordinate-free"的东西. 
也就是所谓的"invariant form"不变形式. 

啊, 有点难. 

这个是作者讲的, 我现在要慢慢理解一下. 

> frame: 就是标架
> 
> 我们可以说标架确定了一组基矢, 
> 并且这个基矢是和位置有关的. 
> 这是因为这个是一个曲线坐标系. 
> 
> (来自未来的注记)

## 讨论的范围
书的作者好像比较注重连续介质力学, 所以讨论的限定会是在
三维Euclidean空间, 至于广义相对论的4维Riemannian 
manifold, 还有数学的$n$维任意空间就是题外话了. 

(应该线性代数会讲吧. )

## 向量 - Directed Line Segments
考虑有向线段$\bar{A B}$, 方向从$A$点指向$B$点, 
经过平移变换和空间中的另外的有向线段$\bar{C D}$
大小和方向都重合, 那么$\bar{A B}=\bar{C D}$, 
于是这样的所有的相等的有向线段构成的集合, 
也就是一个等价类, 则用$\vec{v}$的向量来表示里面的代表元. 

(为了方便, 以后我就不打出矢量符号了. 
`\vec`还是有点表示不方便, `\boldsymbol`就好, 还好看. )

定义单位向量
$\bar{\boldsymbol{v}} 
  = \frac{\boldsymbol{v}}{|\boldsymbol{v}|}$

> directed line segments: 有向线段  
> head/tail: 首尾  
> a parallel translation: 平移变换   
> euivalence class: 等价类  
> example of euivalence class: 代表元

### 加法 - Addition of Two Vectors
学过向量的应该都懂. 
* **Head-to-Tail-Rule**
* **The Parallelogrm Rule**

### 数乘 - Multiplication of a Vector $\boldsymbol{v}$ by a Scalar $\alpha$
Think about similar triangles, and it would be easy. 

数乘的话可以看作是向量空间和域的一个二元运算: 

$$V \times \mathfrak{K} \rightarrow V$$

其中满足单位元, 结合律, 分配律: 

$$e \boldsymbol{v} = \boldsymbol{v}\\
a ( b \boldsymbol{v} ) = ( a b ) \boldsymbol{v} \\
a ( \boldsymbol{v} + \boldsymbol{u} ) = 
a \boldsymbol{v} + a \boldsymbol{u}\\
( a + b ) \boldsymbol{v} = a \boldsymbol{v} + b \boldsymbol{v}$$

### 点乘&叉乘 - Dot Product & Cross Product
没有太大问题吧? 

有一个轮换积, 代表是平行六面体的体积(volume):
$$\mathrm{vol} (\boldsymbol{a} \times \boldsymbol{b}) 
  \cdot \boldsymbol{c}$$

有一个三重积(triple product): 
$$(\boldsymbol{a} \times \boldsymbol{b}) 
  \times \boldsymbol{c}
  = (\boldsymbol{a} \cdot \boldsymbol{c})
    \boldsymbol{b}
    - 
    (\boldsymbol{b} \cdot \boldsymbol{c})
    \boldsymbol{a}$$
(我的记忆方法: 远交近攻)

## 张量 - Tensor
应该是二阶张量. 

考虑一个投影: 
$$\mathrm{Proj}_{\boldsymbol{u}} \boldsymbol{v}
  = (\boldsymbol{v} \cdot \bar{\boldsymbol{u}})
    \bar{\boldsymbol{u}}$$
然后考虑这样的操作的抽象: 
(就是取出任意的$\boldsymbol{v}$都有的形式或结构)
$$\mathrm{Proj}_{\boldsymbol{u}}
  = \bar{\boldsymbol{u}} \bar{\boldsymbol{u}}$$

从这里可以看出为什么说"并矢", 就是把两个向量排在一起, 
就是一个二阶张量了. 

用这样的方式来理解二阶张量感觉方便一点, 但是作者又在序言里面说, 
二阶张量就是一种线性的算符, 将矢量映射成矢量. 

(我觉得这个更妙, 因为这样相当于是说很多的运算都可以用张量来说明了. 
比如后面的$\boldsymbol{\omega} \times$
就可以看成是一个斜对称的二阶张量. )

### 二阶张量的种类
* symmetric 对称的 $\mathcal{T} = \mathcal{T}^T$
* skew(antisymmetric) 斜对称 $\mathcal{T} = - \mathcal{T}^T$
* singular $\exists \boldsymbol{v} \neq \boldsymbol{0}, 
            \quad
            \mathcal{T} \boldsymbol{v} = \boldsymbol{0}$

运用人类的心智, 可以将任何一个二阶张量分解为对称和斜对称的: 
$$\mathcal{T} = \frac{1}{2} (\mathcal{T} + \mathcal{T}^T)
              + \frac{1}{2} (\mathcal{T} - \mathcal{T}^T)$$

注意, 二阶张量的相等就是
$$\mathcal{S} = \mathcal{T}
  \Leftrightarrow
  \boldsymbol{u} \cdot \mathcal{S} \boldsymbol{v}
  = \boldsymbol{u} \cdot \mathcal{T} \boldsymbol{v}, 
  \quad \forall \boldsymbol{u}, \boldsymbol{v}$$

(和映射的相等类比. )

二阶张量的转置就是: 
$$\boldsymbol{u} \cdot \mathcal{T} \boldsymbol{v}
  = \boldsymbol{v} \cdot \mathcal{T}^T \boldsymbol{u}$$

(显然, 可以发现二阶张量可以想成这样: 

$$ \mathcal{T} = \boldsymbol{u} \boldsymbol{v} \Rightarrow \mathcal{T}^T = ( \boldsymbol{u} \boldsymbol{v} )^T = \boldsymbol{v}^T \boldsymbol{u}^T$$ )

## Trace
$$\mathrm{tr}(\boldsymbol{u} \boldsymbol{v})
  = \boldsymbol{u} \cdot \boldsymbol{v}$$

## Again, Physics
回到物理, 这些向量还有张量和物理的关系. 

### Two Fundamental Points
* 代表不同对象的向量是属于不同的线性空间的  
  很好理解, 力和电场无法相加
* 向量不一定完全代表着对象的某个特点
  比如刚体绕轴转动的"角位移"是无法相加的(不是平行四边形法则), 
  (某些)速度也无法相加, 因为这样没什么意义. 

## 后记
分量形式还有一点点运算技巧留到下次. 

(逃)

```
========2021.11.29=======
```

## 注: 关于张量
张量实际上就是满足在变换过程中仍然线性的东西. 

## 补 -- 关于二阶张量的一些运算
$$ \mathcal{T} = \boldsymbol{u} \boldsymbol{v} \\ \mathcal{T} \boldsymbol{v} = \boldsymbol{u} \boldsymbol{v} \cdot \boldsymbol{w} = \boldsymbol{u} ( \boldsymbol{v} \cdot \boldsymbol{w} )$$

可以发现, 二阶张量就是一个投影, 然后换了一个新的基底. 
这样就和作者的观点呼应了. 

$$ \boldsymbol{u} \times \sim \left(\begin{array}{lll}
  0 & - u_x & u_y \\
  u_z & 0 & - u_x \\
  - u_y & u_x & 0
\end{array} \right) $$