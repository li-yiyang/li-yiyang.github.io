---
layout: post
title:  "Benseral Bases and Tensor Notation"
date:   2022-08-23 23:08:51 +0800
categories: notes
math: true
---
```plain
[2022/8/23]: 开始更新... 
```

# 第二章
现在发现了线性代数真是重要, 学了以后竟然可以看懂了. 

## General Bases
for any fixed set of **noncoplanar** vectors, 
即互不共面的一组矢量, 就可以构成线性空间的一组基. 
而判断是否为一组基的方式就可以利用线代课上学的行列式:   
$$\mathrm{det}(\boldsymbol{g}_i) \neq 0$$

(这里做一个小小的注记: 我这本书中的向量的角标好像和线性代数中的不同, 
和线代中的行向量的角标在下面$\boldsymbol{A}_{(i)}$不同, 
这本书中的角标在下面的应该是列向量$\boldsymbol{g}_i$. 
应该是这样的, 虽然书上没有详细的这样规定的样子, 
但是我发现不这样的话, 就会很奇怪. 也许是我弄错了. )

于是用简单的线性代数知识就会知道, 
线性空间中的任一向量都可以表示为基矢的线性组合:    
$$\forall \boldsymbol{v} \in \langle \boldsymbol{g}_i \rangle, 
  \boldsymbol{v} = \sum v^i \boldsymbol{g}_i $$

(这里在加一个注记, 在张量里面的讨论, 除了特殊说明外, 
上面的标记都是角标而不是乘幂. 
并且这里的角标写在上面是为了后面用的, 
也是为了和下面的行向量的系数区分开来. )

(这里在加一个注记: 就是我们考虑的这个玩意就是相当于一时函数, 
魔法吧? $$\mathrm{def} f( \boldsymbol{e}_i ) = u_i \quad \mathrm{therefore} f( \boldsymbol{v} ) = v_i f( \boldsymbol{e}_i )$$ )

(当然, 有列向量就会有行向量: $$\forall \boldsymbol{v} \in \langle \boldsymbol{g}_i \rangle, 
  \boldsymbol{v} = \sum v_i \boldsymbol{g}^i $$
)

(这里可以想象一下: 
$$\left( \begin{array}{lll}\boldsymbol{g}_1 & \boldsymbol{g}_2 & \boldsymbol{g}_3 \end{array} \right) \left( \begin{array}{l} \boldsymbol{\alpha}_1 \\ \boldsymbol{\alpha}_2 \\ \boldsymbol{\alpha}_n \end{array} \right) = \sum_i \alpha_i \boldsymbol{g}_i$$
这样的话就会得到一个很好的对线性代数中的对向量和矩阵运算的理解. )

(这里插入一个说明, 之前的张量的笔记实际上是和坐标系无关的, 
因为实际上是一个抽象的概念. 但是为什么还要分量呢? 
明明分量和坐标系的选取是有关的, 并且分量还很丑. 
这是因为这样子能算啊, 傻孩子. )

### Einstein Summation Convention
为了简化标记, 
伟大的Einstein创造了一种杰出的"偷懒"方法, 
即Einstein Summation Convention:    
> The summation convention applies only 
> when one **dummy index** 
> is "on the roof", and the other 
> is "in the cellar".   
> 
> "Dummy" means that this symbol
> for index can be replaced 
> by any other symbol 
> without affecting 
> the value of the sum. 

翻译过来就是在上标和下标中同时出现的指标就是"dummy index", 
这样的指标就是可以换成新的不同的标号来求和:    
$$\sum v^i \boldsymbol{g}_i = v^i \boldsymbol{g}_i\\
  \sum a_k v^i \boldsymbol{g}_i = 
  a_k \sum v^i \boldsymbol{g}_i = 
  a_k v^i \boldsymbol{g}_i$$

### Reciproal Base Vectors
一个朴素的想法: 

假如我们想要有一种很好的运算方式, 就是当我们做向量点乘的时候, 
最好有像在平面直角坐标系中的那样的简单的形式:   
$$u^i \boldsymbol{g}_i \cdot v_j \boldsymbol{v}^j
  = u^i v_i$$

(一点注记: 上面的等式不一定是成立的, 正确的写法是
$u^i v_j \boldsymbol{g}_i \cdot \boldsymbol{v}^j$, 
具体的原因就是可能会存在像这样的情况: 
$\boldsymbol{g}_i \cdot \boldsymbol{v}^j \neq 0,
i \neq j$, 但是这样不是超级麻烦么. )

(所以, )这样的话我们就会有一种朴素的需要: 
$\boldsymbol{g}_i \cdot \boldsymbol{v}^j=0, 
i \neq j$

为了达到这样的结果, 不妨就先来一个小操作: 

先把这组列向量基记为: 
$ A = (\boldsymbol{g}_1, \boldsymbol{g}_2, \cdots, \boldsymbol{g}_n) $

对应的行向量的基记为: 
$$B = \left( \begin{array}{l} \boldsymbol{g}^1 \\ \boldsymbol{g}^2 \\ \cdots\\ \boldsymbol{g}^n \end{array} \right)$$

于是可以得到:   
$$B A = \left(\begin{array}{llll} \boldsymbol{g}^1 \cdot \boldsymbol{g}_1 & \boldsymbol{g}^1 \cdot \boldsymbol{g}_2 & \cdots & \boldsymbol{g}^1 \cdot \boldsymbol{g}_n \\ \boldsymbol{g}^2 \cdot \boldsymbol{g}_1 & \boldsymbol{g}^2 \cdot \boldsymbol{g}_2 & \cdots & \boldsymbol{g}^2 \cdot \boldsymbol{g}_n \\ \cdots & \cdots & \cdots & \cdots \\ \boldsymbol{g}^n \cdot \boldsymbol{g}_1 & \boldsymbol{g}^n \cdot \boldsymbol{g}_2 & \cdots & \boldsymbol{g}^n \cdot \boldsymbol{g}_n \\ \end{array} \right)$$

然后假如这个朴素的想法能够实现的话, 
就会发现$B A$矩阵的元素除了对角线上的元素, 
其他所有的元素都应该是$0$, 那么这就是一个单位矩阵$E_n$, 
所以不难看出这两个$A$和$B$是互逆的. 

所以就得到了已知一组基, 求另外一组基的方法了, 
**就是求出对应的逆矩阵**, 完事. 

(感觉这就是为什么赵爹上课的时候, 
会讲到一个让我迷幻的$\delta^i_j$
对应单位矩阵的东西了. )

好的, 有了上面的这个规定, 就可以引入数学符号来表示: 
$$\boldsymbol{g}^i \cdot \boldsymbol{g}_j = \delta^i_j = \left\{ \begin{array}{ll} 1 & \mathrm{if} \quad i = j\\ 0 & \mathrm{if} \quad i \neq j \end{array} \right.$$  

($\delta^i_j$就是**Kronecker Delta**. )

下面的出现的矢量就都是这样规定的两组矢量了. 

### The Roof and Cellar Compoents of Vector
现在回到$v_i$和$v^i$的区别, 
书中的说法是$v_i$就叫做cellar components, 
$v^i$就叫做roof components, 
很形象. 

虽然这个不是正式的名称. 
正式的名称就是大名鼎鼎的**covariant**$v_i$和
**contravariant**$v^i$. 
确实, 目前这些名字对我来说也很meaningless, 
摘录一下作者的吐槽: 
> ("covariant" and "contravariant" are) names
> that seem to me awkward and meaningless. 
> "Roof" and "cellar" also have mnemonic value 
> in matrix theory where $A^i_j$ is sometimes
> used to denote the element of a matrix $A$
> that sits in the $i$th row and $j$th column. 

(但是又有说协变和逆变的概念是在坐标系变换的时候出现的一个概念, 
假如和坐标系变化相同的就是协变, 比如伽利略变换的不变性之类的? 

这里还有一件事, 你可以试一试这样理解, 协变是在自己向量空间的变化, 
逆变是变化到对偶空间的东西. )

那么自然可以想到一个已知基底来求分量的方法:    
$$v^i \boldsymbol{g}_i \cdot \boldsymbol{g}_j = v^j$$

这样的话就很简单可以得到$v^j$分量了. 

同理,    
$$v_j = \boldsymbol{v} \cdot \boldsymbol{g}_j$$

## Let's Get Calculation
### Dot Product
$$\boldsymbol{u} \cdot \boldsymbol{v} = u^i v_j \boldsymbol{g}_i \cdot \boldsymbol{g}^j = u^i v_j \delta^i_j = u_i v^i = u^i v_i$$

### Cross Product
$$\boldsymbol{u} \times \boldsymbol{v} = (\boldsymbol{u} \times \boldsymbol{v})_k \boldsymbol{g}^k\\(\boldsymbol{u} \times \boldsymbol{v})_k = (\boldsymbol{u} \times \boldsymbol{v}) \cdot \boldsymbol{g}_k = u^i v^j (\boldsymbol{g}_i \times \boldsymbol{g}_j) \cdot \boldsymbol{g}_k\\ \mathrm{let} \  \varepsilon_{i j k} = (\boldsymbol{g}_i \times \boldsymbol{g}_j) \cdot \boldsymbol{g}_k \Rightarrow (\boldsymbol{u} \times \boldsymbol{v})_k = u^i v^j \boldsymbol{\varepsilon}_{i j k}$$

上面的东西还是很好懂的. 

补充一个: 

$$\varepsilon^{ijk} \varepsilon_{pqr} \equiv \left| \begin{array}{lll} \delta^i_p & \delta^i_q & \delta^i_r \\ \delta^j_p & \delta^j_q & \delta^k_r \\ \delta^k_p & \delta^k_q & \delta^k_r \end{array} \right|$$

## A Second Order Tensor Has Four Sets of Components in General. 
把张量和向量的乘法类比为之前的那个投影: 

$$\boldsymbol{T} \boldsymbol{v} = \boldsymbol{T} (v^i \boldsymbol{g}_i) = v^i \boldsymbol{T} \boldsymbol{g}_i$$

可以发现这个时候, $\boldsymbol{T} \boldsymbol{g}_i$
就好像是一个新的$\boldsymbol{g}'_i$的基底. 
(诶, 这个时候是不是可以说张量就是一个换基底的玩意. )
于是就把$\boldsymbol{T} \boldsymbol{g}_i$记为基底的形式:
$\boldsymbol{T}_i$. 

对于一个基底(也就是另一种程度上的向量)来说, 
又可以把它分割成几个基底的线性组合: 

$$\boldsymbol{T}_i = T_{i j} \boldsymbol{g}^j$$

于是

$$\boldsymbol{T} = T_{i j} \boldsymbol{g}^i \boldsymbol{g}^j$$

当然不是只有这样的一种分解方法. 

$$T^{ij} = \boldsymbol{g}^i \cdot \boldsymbol{T} \boldsymbol{g}^j\\ T^i_{\cdot j} = \boldsymbol{g}^i \cdot \boldsymbol{T}\boldsymbol{g}_j\\ T^{\cdot i}_{j} = \boldsymbol{g}_j \cdot \boldsymbol{T} \boldsymbol{g}^i$$

注意到这里的 $$T^{\cdot i}_{j}$$ 和 $$T^i_{\cdot j}$$ 是不一样的. 
(可以用之前的投影还有坐标变换的思想来看, 相当于是不同的基底, 
经过了张量的变换, 在不同的坐标系下面的分量. 实际的意义不一样. )
但是什么时候会相等呢? 

## 稍微回顾一下

[2022/08/23] 重新来继续看... emmmm... 不知道算不算不忘初心了, 乐. 
不过发现之前写文档的代码风格和自己之前写的东西都有点忘了, 所以现在
稍微回顾一下: 

* 有向线段 -- 向量: 
  * 加法
  * 数乘
* 二阶张量 -- 2 rank Tensor: 
  * 并矢: 如 $$\mathrm{Proj}_{\boldsymbol{u}} = \boldsymbol{u} \boldsymbol{u}$$, 
	其中 $$\mathrm{Proj}_{\boldsymbol{u}} \boldsymbol {v} = \boldsymbol{v} \cdot \boldsymbol{u} \boldsymbol{u}$$. 
	( 嗯, 这个投影的例子可以用来理解各种各样的后面的操作. 还是挺重要的. )
  * 对称, 斜对称, singular( 零元 )

上面的部分有点像是整体的操作, 下面的就是细分到分量的运算. 

* 用 $v^i \boldsymbol{g}_i$ 来表示一个向量, 其中使用到了爱因斯坦求和约定: 
  对上标和下标同时出现的东西进行历遍求和. 
* 点乘: $$\boldsymbol{u} \times \boldsymbol{v} = u^i v_j \boldsymbol{g}_i \cdot \boldsymbol{g}^j = u_i v_i$$, 其中 $$\boldsymbol{g}_i \cdot \boldsymbol{g}^j = \delta^j_i$$. 
* 叉乘: $\boldsymbol{u} \times \boldsymbol{v} = u^i v^j \boldsymbol{\varepsilon}_{i j k}$ .
  其中 $$\boldsymbol{\varepsilon}_{i j k} = (\boldsymbol{g}_i \times \boldsymbol{g}_j) \cdot \boldsymbol{g}_k = \left\{\begin{array}{ll} +J & 偶置换 (i, j, k)\\ -J & 奇置换 \\ 0 & 有两个以上的角标相同\end{array}\right.$$ . 其实也就是做了一个投影换基的操作.   
  类似的 $$\boldsymbol{\varepsilon}^{i j k} = \left\{\begin{array}{ll} +J^{-1} & 偶置换 (i, j, k)\\ -J^{-1} & 奇置换 \\ 0 & 有两个以上的角标相同\end{array}\right.$$, 其中 $J = \det G$
* ( 二阶 ) 张量的表示: (A Second Order Tensor Has Four Sets of Components in General)  
  目前我是这样理解的: 还是将其认为是一种投影, 不过是一种更加有趣的投影方式:   
  $$\boldsymbol{T} \boldsymbol{v} = v^i \boldsymbol{T} \boldsymbol{g}_i\ \mathrm{or}\ v_i \boldsymbol{T} \boldsymbol{g}^i$$  

那么大概就是这样吧, 先这样. ( 感觉是不是应该再做点题目才好... 算了, 我没有那么好学, 
还是看看美好的物理部分, 用实例来学吧. )

### 一点点用来记忆的例题
( 注: 上面的基底都是单位基底 )

> 已知基底 $\boldsymbol{g}_i$, 求向量$\boldsymbol{v}$的表示$(v^i)$:   
> 其实就是解线性方程组, 就是 $\boldsymbol{v} = \boldsymbol{g}_i v^i$, 然后解出结果就好. 

> 已知基底, 求 reciproal base (对偶基).  
> 因为对偶基之间满足 $\boldsymbol{g}^i \cdot \boldsymbol{g}_j = \delta^i_j$, 所以有 $G^{-1} = (\boldsymbol{g}_j)$, 其中 $G = (\boldsymbol{g}^i)$, 于是问题变成求逆矩阵的问题. 

> 求一个张量的四组分量:  
> 比如已知了 $\boldsymbol{T} \boldsymbol{v} \sim (-2v_x + 3v_x, - v_z, v_x, + 2 v_y)$, 
> 还有给出的基底  
> 
> $$\left(\begin{array}{lll} \boldsymbol{g}_1 & \boldsymbol{g}_2 & \boldsymbol{g}_3 \end{array}\right) = \left(\begin{array}{lll} 1 & 0 & -1\\ -2 & 1 & -2 \\ 2 & 1 & 1\end{array}\right)$$  
> 
> (注: 这里有一个约定, 写的时候差点忘了, 在这本书中, 应该是用下标来表示列向量, 上标来表示行向量. 形式上是这样, 原则上应该不是这样的... )  
> 于是:   
> 
> $$\boldsymbol{T}\boldsymbol{g} = \left(\begin{array}{lll}-2 & - & 3\\ 0 & 0 & -1\\ 1 & 2 & 0\end{array}\right) \left(\begin{array}{lll} 1 & 0 & -1 \\ -1 & 1 & -2\\ 2 & 1 & 1\end{array}\right) = \left(\begin{array}{lll} 4 & 3 & 5 \\ -2 & -1 & -1 \\ -1 & 2 & -5\end{array}\right)$$
> 
> ( 注: 实际计算了之后发现了指标的一个好处了, 上面的 $\boldsymbol{T}\boldsymbol{g}_i$ 
> 就像是指标表述的那样, 是一个列向量. 指标表示了这个向量的性质... 大概吧. 在形式上是这样的. )
> 
> ... emmm... 还是有点不太理解, 先放一下...

> 换基底: 

## 用牛顿力学来学张量的微积分
### Newton's Law in Cartesian Components
把位矢写成分量的形式: 

$$\boldsymbol{x} = \boldsymbol{x}_x + \boldsymbol{x}_y + \boldsymbol{x}_z$$

于是牛二就可以写成 $\boldsymbol{f} = m \ddot{\boldsymbol{x}}_i, (i = x, y, z)$. 

### Newton's Law in Plane Polar Coordinates
位矢 $\boldsymbol{x} = \hat{\boldsymbol{x}}(r, \theta) = r \cos \theta \boldsymbol{e}_x + r \sin \theta \boldsymbol{e}_y$ 写成分量的形式: $\boldsymbol{x} = \hat{\boldsymbol{x}}(\boldsymbol{u}^j) = \hat{x}^i(\boldsymbol{u}^j)\boldsymbol{e}_i$. 

其中 $\boldsymbol{u}$ 代表着在 Plane Polar Coordinates 下的向量坐标, 
$u^j$ 代表其不同的分量. 然后 $\hat{\boldsymbol{x}}$ 表示新基底下的
向量. 

于是新基的基底 (可以叫做 cellar base vectors) 就是
$$\boldsymbol{g}_i = \frac{\partial \boldsymbol{x}}{\partial u^i} = \frac{\partial x^k}{\partial u^i} \boldsymbol{e}_k$$. 
自然的, 得到 Jaccobi 矩阵: $$J(\boldsymbol{g}_j, \cdots) = \det[x^i_{,j}]$$.
于是就可以用这样的方法来计算在该基底下的各种各样的向量的基: 
$$v_i = \boldsymbol{v} \cdot \boldsymbol{g}$$. 以及其导数等. 
