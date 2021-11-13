---
layout: post
title:  "Linear Algebra Preparing for Test"
date:   2021-11-12 18:09:53 +0800
categories: jekyll update
math: true
---
# Prepare for Linear Algebra
复习, 复习...

## 预备知识
### 等价关系 
* 反身性: 
  $a \sim a$
* 对称性: 
  $a \sim b \Rightarrow b \sim a$
* 传递性: 
  $a \sim b, b \sim c \Rightarrow a \sim c$

### 映射  
* **定义域**, **值域**  
  这里的定义和高中的"值域"是不一样的, 
  定义函数$f: X \rightarrow Y$, 
  这样的$Y$就是值域, 和高中的不一样在于: 
  $\exists y \in Y, \forall x \in X, f(x) \neq y$, 
  就是存在映射后对应$X$之外的元素. 
* **像**$Im$, **原像**  
  像的定义和高中的"值域"是一样的, 就是
  $Im f = \{y=f(x),\forall x \in X\}$
* 映射的**相等**  
  首先就要有相同的定义域和值域, 并且对同样的元素的映射结果一样, 
  即$\forall x \in X, f(x) = g(x) \Rightarrow f = g$
* 映射的**限制**和**扩张**  
  就是假如定义域和值域不一样的话, 就有映射的限制和扩张的概念. 
* 几种映射: 
  * **单射**: $f(x_1) = f(x_2) \Rightarrow x_1 = x_2$
  * **满射**: $Im f = Y$
  * **一一映射**: 即使单射也是满射, 是可逆映射  
    (实际证明的时候一般是通过构造逆映射来的)
  * **变换**: $f:X \rightarrow X$自己映射到自己身上的映射
  * **恒等映射**: $e_X: X \rightarrow X, x \mapsto x$, 是双射
  * **包含映射**: 
    $I: X \rightarrow Y, x \mapsto x, X \subset Y$, 
    记为$I:X \hookrightarrow Y$. 
* 映射的**复合**

研究映射的时候可以通过画**交换图**来表示映射关系. 

### 商映射
利用等价关系把集合划分成相等价的部分, 
在研究的时候只要取出一个元素(代表元)
来研究就可以代表这一个部分. 

**等价类**:   
$$\{x: x \sim x_0\}$$

**代表元**:   
从等价类种任意拉出一个就行

**划分**:   
划分就是把集合分成几个不交并的子集, 划分和等价类一样. 

### 编序集
**偏序**:
* 反身性: $x \leq x$
* 反对称性: $x \leq x', x' \leq x \Rightarrow x = x'$
* 传递性: $a \leq b, b \leq c \Rightarrow a \leq c$

存在偏序关系的集合就是**偏序集**, 但是也有任意元素都可以比较的**全序集**

描述偏序关系的图叫**哈塞图**

**极大元**: 在可能的比较中没有比他大的
**最大元**: 在所有比较中都是最大的, 存在即唯一, 一定是极大元  

### 置换
**置换**  
$$\pi
= \left( \begin{array}{llll}
  1 & 2 & \ldots & n\\
  \pi (1) & \pi (2) & \ldots & \pi (n)
\end{array} \right)$$

* 置换可以写成对换或者循环的乘积
* 置换是$n$元对称群
* 置换的符号$\varepsilon$:   
  对于$\pi = \sigma_1 \cdots \sigma_n$, 
  $\sigma_i$是长度为$l_i$的循环:   
  $$ \varepsilon_{\pi} 
    = (-1)^{\sum_{k=1}^{m}(l_k - 1)}$$
* 奇置换$\varepsilon_{\pi} = -1$, 
  偶置换$\varepsilon_{\pi} = 1$, 
  $S_n$中奇偶置换的个数相等, 都为$\frac{n!}{2}$个. 
* 斜对称函数满足奇置换作用在变量表上时, 函数值变号. 

**循环**  
* 不相交的循环乘积可以交换
* 置换可以写成循环的乘积:    
  方法就是先选一个起点, 然后走一圈, 闭合; 
  然后再选择下一个起点直到全部元素历遍


**对换**  
* 长度为2的循环成为对换
* 对换的逆就是其自身
* 循环可以分解为对换之积:   
  $$(i_1, i_2, \cdots, i_n) 
    = (i_1 i_n)(i_1 i_{n-1})\cdots (i_1, i_3)(i_1 i_2)$$

## 线性方程组
$$\left\{ \begin{array}{l}
     a_{11} x_1 + a_{12} x_2 + \cdots + a_{1 n} x_n = b_1\\
     a_{21} x_1 + a_{22} x_2 + \cdots + a_{2 n} x_n = b_2\\
     \vdots\\
     a_{m 1} x_1 + a_{m 2} x_2 + \cdots + a_{m n} x_n = b_m
   \end{array} \right. $$

可以记作   
$$A \boldsymbol{x} = \boldsymbol{b}$$

或者是   
$$\sum_{j=1}^n x_j A^{(j)} = B$$

高斯消元法, 把线性方程组变成阶梯型
(和原线性方程组等价, 等价关系就是有相同的解, 或者同不相容)

抽象之后就是矩阵的初等行变换

齐次线性方程组
$$\boldsymbol{b} = \boldsymbol{0}$$

| 线性方程组的类型 | 一般 | 齐次 | $m<n$一般 | $m<n$齐次 |
| ------------- | ---- | --- | -------- | --------- |
| 解的个数       | $0,1,\infin$ | $1,0$ | $0,\infin$ | $\infin$ |

### 线性方程组的分类
假如可以把线性方程组化成阶梯型  
$$\left\{ \begin{array}{lll}
     \overline{a_{11}} x_1 + \cdots \quad & = & \overline{b_1}\\
     \overline{a_{2 k}} x_k + \cdots & = & \overline{b_2}\\
     & \ldots & \\
     0 & = & \overline{b_{r + 1}}\\
     0 & = & \overline{b_n}
   \end{array} \right. $$

* 相容的(有解):  
    $\overline{b_{r + 1}}, \ldots, \overline{b_n}$
    全为零     
    或者$rank(A) = rank(A|B)$
  * 确定的(解唯一): 
    * 主未知数的个数和方程未知数个数相等, 即没有自由变量
    * $rank A = n, A \in M_{m \times n}$
  * 不确定(存在自由变量)
* 不相容(无解):   
    $\overline{b_{r + 1}}, \ldots, \overline{b_n}$
    不全为零, 也就是有$0 = b \neq 0$的矛盾的方程
  

## 矩阵
### 矩阵的本质
矩阵就是线性映射, 线性映射就是矩阵

### 矩阵的初等行列变换
矩阵的初等行(列)变换: 
* **I型初等行(列)变换** 
  $F_{i j}$交换行(列)   
  $$\left( \begin{array}{lllllllll}
    1 &  &  &  &  &  &  &  & \\
    & \ddots &  &  &  &  &  &  & \\
    &  & 0 & \ldots &  &  & 1 &  & \\
    &  & \vdots & 1 &  &  &  &  & \\
    &  &  &  & \ddots &  &  &  & \\
    &  &  &  &  & 1 &  &  & \\
    &  & 1 &  &  &  & 0 &  & \\
    &  &  &  &  &  &  & \ddots & \\
    &  &  &  &  &  &  &  & 1
  \end{array} \right)$$
* **II型初等行(列)变换** 
  $F_{i j}(\lambda)$第$j$行(列)的$\lambda$倍加到第$i$行(列)上   
  $$\left( \begin{array}{lllllll}
    1 &  &  &  &  &  & \\
    & \ddots &  &  &  &  & \\
    &  & 1 & \ldots & \lambda &  & \\
    &  &  & \ddots & \vdots &  & \\
    &  &  &  & 1 &  & \\
    &  &  &  &  & \ddots & \\
    &  &  &  &  &  & 1
  \end{array} \right)$$
* **III型初等行(列)变换**
  $F_i(\lambda)$第$i$行(列)数乘$\lambda$倍   
  $$\left( \begin{array}{lllllll}
    1 &  &  &  &  &  & \\
    & \ddots &  &  &  &  & \\
    &  & 1 &  &  &  & \\
    &  &  & \lambda &  &  & \\
    &  &  &  & 1 &  & \\
    &  &  &  &  & \ddots & \\
    &  &  &  &  &  & 1
  \end{array} \right)$$

矩阵的初等行(列)变换等价于左(右)乘上一个初等矩阵, 
并且初等矩阵是可逆的

### 向量空间
#### 向量空间定义
满足: 
* 加法交换律: $X+Y=Y+X$
* 加法结合律: $(X+Y)+Z=X+(Y+Z)$
* 零向量: $\boldsymbol{0} := (0, \cdots, 0) \quad
          s.t. \boldsymbol{0} + X = X$
* 负向量: $-X:=(-1)X \quad s.t. X + (-X) = \boldsymbol{0}$
* 单位$1$: $1X=X$
* (数)乘法结合律: $\alpha(\beta X)=(\alpha \beta)X$
* 数乘的结合律: $(\alpha + \beta)X
              = \alpha X + \beta X$
* 数乘的结合律: $\alpha(X+Y) = \alpha X + \alpha Y$

#### 子空间
满足: 
$$\alpha X + \beta Y \in V, \forall X, Y \in V$$
的就是**子空间**, 是**线性空间**的**子集**

* 子空间的交是子空间
* 子空间的并不一定是子空间
  ($x$轴和$y$轴向量的并就是反例)
* 子集的**生成**$\langle X_1, X_2, \cdots, X_n \rangle$
* **线性无关**的一堆向量生成子空间, 
  则这些向量就是这个子空间的一组**基**, 
  子空间的**维数**就是基中的向量的个数
* 子空间的基的个数**唯一**
* 对于矩阵的基的线性空间
  $dim V_A = dim \langle A_{(i)} \rangle
  = dim \langle A^{(i)} \rangle = rank A$    
  (在证明矩阵的$rank$的问题的时候可以考虑线性空间的理解方式. )
* 判断两个生成子空间是否相等, 只需要通过互相包含就可以得到相等关系: 
  左边的生成元是否落在右边的生成子空间中, 
  右边的生成元是否落在左边的生成子空间中

**线性无关**:   
$$\sum \alpha_i X_i = 0 \Leftrightarrow \alpha_i = 0$$
就说明这样的一组$X_i$就是线性无关的. 

### 秩
矩阵的秩就是化成阶梯型之后的非零行的个数. 

$$ P A Q = \left( 
  \begin{array}{ll}
    I_r & O \\
    O   & O
  \end{array} 
\right)$$

* 初等行变换不改变矩阵的秩 (证明可以考线性方程组来)

#### 求列向量的秩和极大线性无关组的方法
对矩阵$A=(X_1, X_2, \cdots, X_n)$
进行初等行变换变成阶梯型$\overline{A}$, 
则$\overline{A}$的非零行的个数就是$rank A$. 

$\overline{A}$中的$r$个非零行打头的非零元所在的列
$\overline{A}^(j_1), \overline{A}^(j_2), \cdots, 
\overline{A}^(j_r)$
就是$\overline{A}^(1), \overline{A}^(2), 
    \cdots, \overline{A}^(n)$
的一个极大线性无关组

或者也可以利用初等列变换化成阶梯型
$$\overline{A} = \left( \begin{array}{llllll}
      0 &  &  &  &  & \\
      \vdots &  &  &  &  & \\
      0 &  &  &  &  & \\
      \bar{a}_{i_1 1} &  &  &  &  & \\
      \vdots &  &  &  &  & \\
      \bar{a}_{i_2 1} & \bar{a}_{i_2 2} &  &  &  & \\
      \vdots & \vdots & \vdots &  &  & \\
      \bar{a}_{i_r 1} & \bar{a}_{i_r 2} &  &  &  & \\
      \vdots & \vdots & \vdots &  &  & \\
      \bar{a}_{m 1} & \bar{a}_{m 2} &  & 0 & \ldots & 0
    \end{array} \right)$$
于是就有
$\overline{A}^(j_1), \overline{A}^(j_2), \cdots, 
\overline{A}^(j_r)$为列向量子空间的基, 也就是
$\langle X_1, X_2, \cdots, X_n \rangle$
的极大线性无关组. 

### 矩阵的运算
* 加法
* 纯量数乘
* 乘法
  $$A_{m \times s} B_{s \times n} 
  = (\sum_{k=1}^s a_{i k} b_{k j}$$  
  $$(A B)_{(i)} = A_{(i)} B$$  
  $$(A B)^{(j)} = A B^{(j)}$$
* 转置  
  (让列向量的问题一下子就可以用到行向量上)  
  (还有别的好用的特点, 比如在斜对称
  $$det A = det(-A^T) \Rightarrow det A = 0$$)
* 逆

### 矩阵和线性映射的对应
$$\varphi : \mathbb{R}^n \rightarrow \mathbb{R}^m$$
* 加法: $\varphi (X + X') = \varphi (X) + \varphi (X')$
* 乘法: $\varphi (\lambda X) = \lambda \varphi (X)$

于是可以发现矩阵运算和线性映射的联系: 
* 加法   
  $$\begin{array}{lrll} + : & \mathcal{L} (\mathbb{R}^n, \mathbb{R}^m) \times \mathcal{L}(\mathbb{R}^n, \mathbb{R}^m) & \rightarrow & \mathcal{L} (\mathbb{R}^n, \mathbb{R}^m)\\ & (\varphi, \varphi') & \mapsto & \left( \begin{array}{ll}\varphi + \varphi' : & \mathbb{R}^n \rightarrow \mathbb{R}^m\\ & X \mapsto (\varphi + \varphi') (X) = \varphi (X) + \varphi' (X) \end{array} \right)\end{array}$$
* 纯量乘法   
  $$\begin{array}{lrll}
  \cdot \quad : & \mathbb{R}  \times \mathcal{L} (\mathbb{R}^n,
  \mathbb{R}^m) & \rightarrow & \mathcal{L} (\mathbb{R}^n, \mathbb{R}^m)\\
  & (\lambda, \varphi) & \mapsto & \left( \begin{array}{ll}
    \lambda \varphi : & \mathbb{R}^n \rightarrow \mathbb{R}^m\\
    & X \mapsto (\lambda \varphi) (X) = \lambda \varphi (X)
  \end{array} \right)
  \end{array}$$
* 乘法(线性映射的复合)


$\mathcal{L}(\mathbb{R}^n, \mathcal{R}^m)$中的线性映射
和$M_{m \times n}(\mathbb{R})$中的矩阵一一对应

### 方阵
矩阵对应的是线性映射, 方阵对应的是**线性变换**   
方阵的运算有**环**和**代数**的结构   

### 矩阵的等价
$$A \sim B \Leftrightarrow \exists P, Q可逆, B = P A Q$$

* 等价类里的元素是**同秩**
* 任一$n$阶可逆阵均可分解成$n$阶初等矩阵的积

### 逆矩阵
$$(A|E) \rightarrow (E|A^{-1})$$

### 解空间
所有$A X = 0$的解的集合形成**解空间**, 记为$V_A$   
$$dim V_A + rank A_{m \times n} = n$$   
$$dim Im \varphi_A + dim Ker \varphi_A = n$$  
(解空间也同时是核$Ker \varphi_A$)

### 基础解系
解空间的任意一组基为一个**基础解系**, 求法: 
1. 化成阶梯型, 并将每一个非零行开头的非零元上方的系数化成$0$
2. 取自由变量依次为$1$, 其余为$0$时的解, 就得到一个基矢

## 行列式
### 低阶行列式
对角线法则计算

### 行列式定义
$$det A 
  = \sum_{\sigma \in S_n} 
      \varepsilon_{\sigma} a_{1, \sigma(1)} a_{2, \sigma(2)}
        \cdots a_{n, \sigma(n)}$$

### 行列式的本质
多重线性斜对称函数
* **多重线性函数**  
$$\mathcal{D} [A_{(1)}, \ldots, A_{(k - 1)}, \lambda' A_{(k)}' + \lambda'' A_{(k)}'', A_{(k + 1)}, \ldots, A_{(n)}] = \lambda' \mathcal{D} [A_{(1)},\ldots, A_{(k - 1)}, A_{(k)}', A_{(k + 1)}, \ldots, A_{(n)}] + \lambda'' \mathcal{D} [A_{(1)}, \ldots, A_{(k - 1)}, \lambda'' A_{(k)}'', A_{(k + 1)}, \ldots, A_{(n)}]$$
* **斜对称函数**   
  任意交换两个元素(行或列)符号反号
* $$det E = 1$$
* $$det(\lambda E) = \lambda^n det(A)$$
* 有一行(列)为零的行列式为$0$
* I型初等行变换, 反号
* II型初等行变换, 不变
* III型初等行变换, 扩大$\lambda$倍
  
### 行列式公理化定义
* $$det E = 1$$
* 多重线性函数
* 斜对称函数

### 多重斜对称线性函数
任意的多重斜对称线性函数满足:   
$$\mathcal{D}(A) = det(A) \mathcal{D}(E)$$

### 行列式按行列展开
**余子式**和**代数余子式**, 就差一个代数项$(-1)^{i+j}$

### 行列式应用
**分块**:  
$$det\left(\begin{array}{ll}
  A & C\\
  O & B
\end{array}\right) = det(A) det(B)$$

**方阵乘积行列式**: 
$$det(A B) = det(A) det(B)$$

#### 矩阵可逆的判断
若   
$$det(A) \neq 0$$
则可逆  

$$A^{V} A = det(A) E = A A^{V}$$
#### 克莱默法则
$$X = \frac{A^{V}}{det(A)}B$$
#### 秩的子式判别法
行列式的最大子式的阶数就是$rank A$

## 部分习题
在矩阵中出现有一行或一列元素全相等, 
然后其他的行(列)的元素又很整齐的话, 
考虑用相等的那一行去消元. 
### $rank$
* 化成阶梯型
* 看子式, 可以用来估计一个范围先, 然后再讨论
* 基矢量
* $$dim V_1 + dim V_2 = dim(V_1 + V_2) + dim(V_1 \cap V_2)$$
* $$rank(A B) \leq rank(A) + rank(B)$$
* $$rank(A B C) + rank(B) \geq rank(A B) + rank(B C)$$
### 行列式
* 按行(列)展开然后递归, 适用于去掉一部分后结构相似的
### 分块矩阵

## 好耶(后记)
完蛋了耶, 好耶, 我为什么在高兴呢欸? 明明完蛋了的说. 

QAQ

算了一个巨难算的行列式, 给出了一个自己也不信的答案; 
证了一个奇怪的命题, 瞎掰了一个自己也不服的证明...