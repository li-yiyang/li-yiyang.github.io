---
layout: post
title:  "Mid-term `Preview' of Linear Algreba"
date:   2022-04-23 16:41:52 +0800
math: true
categories: notes
---
# Linear Algreba
## 上学期的收尾
### 多项式的根的一般性质
* 多项式的根      
  $f(X) \in 整环A[X], c \in A, f(c) = 0$     
  根是一种代数的概念, 零点是一种几何的概念. 之所以是整环, 是因为整环的根的个数和方程的次数相关. 
* 贝祖定理: 整环$A$的一个根$c \Leftrightarrow (X-c) \vert f(X) \in A[X]$
* 整环可分解成$f(X) = (X - c_1)^{k_1}\cdots(X - c_r)^{k_r} v(X)$    
  推论: $\deg f, g \leq n, \exists 两两不同的c_1, \cdots, c_{n+1} s.t. f(c_i) = g(c_i) \Rightarrow f = g$

> $\frac{f(X)}{g(X)} = \sum_i \sum_j^{m_i} \frac{h_{i, j}(X)}{g_i(X)^{j}}, g(X) = \sum_i g_i(X)^{m_i}$

### 多项式函数
* 定义: 映射$\begin{array}{lll} \widetilde{\ }: A[X] \rightarrow A^A, f & \mapsto & \widetilde{f} : A \rightarrow A \\ & & a \mapsto f(a)\end{array}$, (其中$A^A$表示$A \rightarrow A$的映射集合)    
  于是$A_{pol} = Im \widetilde{\ }$为$A^A$的一个单位元的交换置换, 称为$A$上的多项式环, 元素称为多项式函数. 
* 多项式函数的导数: $f'(X) = n a_0 X^{n-1} + (n-1) a_1 X^{n-2} + \cdots + a_{n-1}$

### 韦达公式

$$\left\{\begin{array}{lll}a_1 a_0^{-1} & = & -\sum_{i = 1}^n c_i \\ & \vdots & \\ a_k a_0^{-1} & = & (-1)^k \sum_{1 \leq i_1 < i_2 < \cdots < i_k \leq n}^n c_{i_1} c_{i_2} \cdots c_{i_k} \\ & \vdots & \\ a_n a_0^{-1} & = & (-1)^n c_1 c_2 \cdots c_k\end{array}\right.$$

> 利用韦达公式可以计算根的组合: $f(x) = x^n + b_1 x^{n-1} + \cdots + b_n$
> * $\sum a_i = b_1$
> * $\sum \frac{1}{a_i} = - \frac{b_n - 1}{b_n}$
> * $\sum a_i^2 = b_1^2 - 2 b_2$

### 对称多项式
* 在韦达公式中出现的$\sum_{1 \leq i_1 < i_2 < \cdots < i_k \leq n}^n c_{i_1} c_{i_2} \cdots c_{i_k}$属于是一种对称函数.     
  对称函数的定义: $(\pi \circ f) = f(x_{\pi (1)}, \cdots, x_{\pi (n)})$    
  于是可以类似定义对称多项式环. 
* 对称多项式基本定理: 设$A$为整环, 在$A[X_1, \cdots, X_n]$中的单项式定义上字典序: $a X_1^{i_1}X_2^{i_2}\cdots X_n^{j_n} \Leftrightarrow \exist 1 \leq t \leq n s.t. i_1 = j_1, \cdots, i_{t-1} = j_{t-1}, i_t > j_t, \cdots$
* 多项式的首项: $FT(f)$为$f$中关于字典序最大的单项式称为首项    
  $f = f_1 f_2 \cdots f_r, FT(f) = FT(f_1) \cdots FT(f_r)$
* 单项式$a X_1^{i_1} \cdots X_n^{i_n} s.t. i_1 \geq i_2 \geq \cdots \geq i_n$是单调的. 
* 任意对称多项式都可以写成初等对称多项式的多项式

> 如何将对称多项式化简成初等对称多项式的组合: 
> * 最朴素的方法: 首先将这些对称多项式通过字典序排序, 然后拿初等对称多项式来做一个减法. 不断地减下去直到不能够再做减法为止. 
> * 待定系数法: $v = X_1^{i_1} \cdots X_n^{i_n}, g_v = s_1^{i_1 - i_2} \cdots s_{n-1}^{i_n - 1} s_n^{i_n} \Rightarrow v = g_v + \cdots$

### 代数基本定理
代数基本定理: 复数域$\mathbb{C}=\mathbb{R}[X] / (X^2 + 1)$是代数封闭域, 每个复系数多项式都有(复数)根. 

**代数封闭域**满足下面四个等价条件: 
1. $F[X]$中的任一正次多项式均可分解为一次因式之积
2. $F[X]$中的既约多项式只有一次多项式
3. $F[X]$中的正次多项式在$F$中至少有一个根
4. $F[X]$中的任一多项式$f$定义的多项式函数$\widetilde{f}: F \rightarrow F, a \mapsto f(a)$为满射

> 奇数次的实系数多项式一定有实根: 利用代数基本定理就可以证明. 

### 实系数多项式
* 实系数多项式中的既约多项式为一次多项式或者是判别式小于零的二次多项式.    
  有理函数域$\mathbb{R}(X)$中的最简分式形如$\frac{\alpha}{(X+a)^m} + \frac{\beta X + \gamma}{(X^2 + b X + c)^2}$
* 整系数多项式的有理根: 
  $f = a_0 X^n + a_1 X^{n-1} + \cdots + a_{n - 1} X + a_n$的有理根$\frac{q}{p}, p, q \in \mathbb{Z}^{*} \wedge (p, q) = 1, q \vert a_0, p \vert a_n$

> 如何判断是否有重根: 
> * $f(x) \in F[X] 无重根 \Leftrightarrow (f, f') = 1$    
>   例: $\mathrm{char} K = 0, f(x)既约多项式, (f, f') = 1$, $x^p - t = x^p - (t^{\frac{1}{p}})^p = (x - t^{\frac{1}{p}})^p$
> * 判别式法: $\Delta := \prod_{i < j}(\alpha_i - \alpha_j)^2, f(x) {\mathbb{C} \above{} =} (x - \alpha_i) \cdots (x - \alpha_n)$
> * 霍纳方法算算看

> 如何求根和根的重数
> * 霍纳方法    
>   竖加斜乘:     
>   $\begin{array}{llll} & a_0 & a_1 & a_2 \\ c & \downarrow & c b_0 \downarrow & c b_1 \downarrow \\ & b_0 = a_0 + 0 \nearrow & b_1 = a_1 + c b_0 \nearrow & b_2\end{array}$
> * 利用导数来判断域上的重根     
>   $c \in F, f \in F[X]^{*}, c为重根 \Leftrightarrow f(c) = f'(c) = 0$
>   > 域$F$的特征为$0$或者素数$p, p \nmid n \Rightarrow X^n - 1 \in F[X]$只有单根.     
>   > 假设有重根, $c^n - 1 = n c^{n - 1} = 0$ , 当特征为为$0$时, $n \neq 0$所以没有公共根; 当特征为$p$的时候, $X^p - 1 = (X - 1)^p$的根$1$不是公共根. 

> 如何求出整系数多项式的有理根:    
> 1. 求出首项系数与常数项的所有因子
> 2. 求出首项因子为分母, 常数项因子为分子的所有既约分数
> 3. 代入检验, 查看是否有有理根

## 空间与型
### 向量空间
向量空间的八个条件: 

| 1 |  加法交换  | 5 |   (乘法)单位元   |
|---|------------|---|------------------|
| 2 |  加法结合  | 6 |     数乘交  换   |
| 3 | (加法)零元 | 7 |  数(的和)乘分配  |
| 4 | (加法)负元 | 8 | 数乘(元素和)分配 |

* 子空间: 关于减法和纯量乘法封闭$x, y \in U \subset V, \alpha x - \beta y \in U$
* 任一子空间的交还是子空间
* 线性无关    
  线性无关的向量的数量一定不超过生成元的个数.    
  和零放在一起的东西一定是线性相关的, 因为零是线性相关的. 
* 等价向量组$\langle X_1, \cdots, X_n \rangle = \langle Y_1, \cdots, Y_m \rangle$    
  两个线性无关的等价向量组的元素个数相等
* 向量空间的基和维数
* 坐标与向量空间同构    
  取基$e_1, \cdots, e_r$, 则所有的向量 $$\boldsymbol{X} = x_1 e_1 + \cdots + x_r e_r$$ 可以形式地表示成 $$\boldsymbol{X} = (e_1, \cdots, e_r) \left(\begin{array}{l} x_1\\ \vdots \\ x_n\end{array}\right)$$ , 称为基下的坐标. 这样的坐标和向量空间同构. 
* 基到基的转换矩阵:    
  用旧的基$e_1, \cdots, e_n$来表示新的基$e_1', \cdots, e_n'$: $(e_1', \cdots, e_n') = (e_1, \cdots, e_n)\left(\begin{array}{lll} a_{11} & \cdots & a_{1 n} \\ \cdots & \cdots & \cdots \\ a_{n1} & \cdots & a_{nn}\end{array}\right)$, 矩阵就是基$e_1, \cdots, e_n$到基$e_1', \cdots, e_n'$的**转换矩阵**. 其逆矩阵为基$e_1', \cdots, e_n'$到基$e_1, \cdots, e_n$的转换矩阵.     
  这样的话, 相当新基的向量就有$A X' = X \Rightarrow X' = A^{-1} X$
* 子空间的和    
  **向量空间为集合的线性化**    
  * $\dim (U+W) = \dim U + \dim W - \dim (U \cap W)$
  * 子空间的直和: $U_1 \oplus \cdots \oplus U_m$
    1. $\forall \boldsymbol{u} \in U, \boldsymbol{u} = \boldsymbol{u}_1 + \cdots + \boldsymbol{u}_m, \boldsymbol{u}_i \in U_i$
    2. $\boldsymbol{u}_1 + \cdots + \boldsymbol{u} = 0 \Rightarrow \boldsymbol{u}_i = 0$, 即零向量有唯一的表示
    3. $U_i \cap (U_1 + \cdots + \hat{U_i} + \cdots + U_n) = 0$
    4. $\dim U = \sum \dim U_i$
  * 补空间: $U$为$V$的$m$维子空间, 则存在$n - m$维子空间$W$, $W \oplus U = V$
  * 商空间: 关于等价关系可以定义商集, 陪集, 商映射, 商空间. 
  * 商映射$p : V \rightarrow V / U, v \mapsto \overline{v}$, 嵌入映射$i : W \rightarrow V, w \mapsto w$, 那么$p i : W \rightarrow V / U$为线性同构    
    $\mathrm{codim}_V U =\dim V / U = \dim V - \dim U$余维数    
    对于余维数为$1$的, 称为**超平面**

> 通过线性映射的观点来研究空间: 
> * 实对称矩阵集合$W$是$M_n(K)$的子空间:     
>   $$\varphi : M_n(K) \rightarrow M_n(K); A \mapsto A - A^T, \mathrm{Ker} \varphi = W$$, 所以是子空间. 
> * 子空间满足$$\dim V_1 + \cdots + \dim V_k \geq n(k-1) \Rightarrow \dim \cap V_i \geq 1$$    
>   $$\varphi : V_1 \times \cdots \times V_k \rightarrow (V \times \cdots \times V)_{k - 1个}; (\alpha_i) \mapsto (\alpha_{i - 1} - \alpha_i) \\ \Rightarrow \mathrm{Ker} \varphi \cong \cap V_i \Rightarrow \dim V_1 + \cdots + \dim V_k \geq n(k-1)$$
> * $$\dim V_1 + \dim V_2 > n, \dim V_1 \cap V_2 \geq 1$$    
>   $$\varphi : \mathbb{R}^n \rightarrow \mathbb{R}^m; x \mapsto A x, \dim \mathrm{Ker} \varphi = r$$, 于是$$\mathrm{Ker} \varphi$$就是一堆线性方程的解. 每个空间都是某个线性映射的核. 
> * $\varphi : V \rightarrow W, \dim V = \dim \mathrm{Ker} \varphi + \dim \mathrm{Im} \varphi$, 维数定理
> * 容斥定理: $\dim (W_1 + W_2) + dim W_1 \cap W_2 = \dim W_1 + \dim W_2$    
>   $f: W_1 \times W_2 \rightarrow V, (v_1, v_2) \rightarrow v_1 - v_2 \Rightarrow W_1 + W_2 = \mathrm{Im} f, W_1 \cap W_2 \rightarrow \mathrm{Ker} f$

> 维数定理的使用: 
> * $$\dim \mathbb{R}[t] / \mathbb{R}[t]_{< n} = \dim V - \dim \mathbb{R}[t]_{< n} = \infty$$
> * $$\mathbb{R}[t] / t^n \mathbb{R}[t] \Rightarrow \dim = n$$
> * $$\mathbb{R} / \mathbb{R}[t^2] \Rightarrow \dim = \infty$$

> 线性映射的证明: 可以将一个线性映射看作是多个线性映射的复合, 这样只要证明每个线性即可.    
> $f: V \times V \rightarrow W$双线性映射 $\Leftrightarrow f(\alpha, -), f(-, \alpha)$线性, $V \times V {\rightarrow_f W \rightarrow_g \above{} \rightarrow_{g(f(\alpha, -))}} U$

### 对偶空间
* $$\mathcal{L}(V, K) = \{f \vert 线性映射 f : V \rightarrow K\} =: V^{*}$$记为$$V$$的对偶空间. 
* 对偶基$e^1, \cdots, e^n$, 关系就是$e^i e_j = \delta _{i j}$
* 自反性: $\varepsilon : V \rightarrow V^{**}$为线性同构. 
* 向量组的秩的求法: $\mathrm{rank}\langle v_1, \cdots, v_m \rangle = \mathrm{rank} (f_i(v_j))$

> $$(f_1, \cdots, f_n) = (e^1, \cdots, e^n) A, (e^1, \cdots, e^n) = (f^1, \cdots, f^n) B$$    
> 首先有$$[e^1, \cdots, e^n](e^1, \cdots, e^n) = I_n \Rightarrow (f_1, \cdots, f_n) A^{-1} = (e^1, \cdots, e^n); [e^1, \cdots, e^n] = B^T [f^1, \cdots, f^n] \Rightarrow [e^1, \cdots, e^n](e^1, \cdots, e^n) = B^T [f^1, \cdots, f^n](f_1, \cdots, f_n) A^{-1} \Rightarrow B^T A^{-1} = I_n$$

### 双线性型与二次型
* 双线性$f(x, y) = x^T A y$
* 二次型$q(x) = x^T A x$
  * 对称
  * 反对称
* 转换规则

### 线性算子

## 综合
### 矩阵的关系
* 等价: $A \cong B \Leftrightarrow \exist 可逆P, Q, s.t. P A Q = B \Leftrightarrow \mathrm{rank} A = \mathrm{rank} B$    
  代表元: $\left(\begin{array}{ll}I_r & O \\ O & O\end{array}\right)$
* 合同($\mathrm{char} K \neq 2$): $A \simeq B \Leftrightarrow \exist 可逆P, s.t. P^T A P = B$
  * 对称矩阵: $A^T = A$    
    实对称阵: $A \simeq \mathrm{diag}\left(I_r, I_s, O\right), A \simeq B \Leftrightarrow (r(A) = r(B)) \wedge (s(A) = s(B))$    
    复对称阵: $A \simeq \mathrm{diag}\left(I_r, O\right)$
    * 正定: $x^T A x > 0$
    * 负定: 负定就是正定阵取负. 
  * 反对称矩阵: $A^T = -A$
* 相似: $A \sim B \Leftrightarrow \exist P, s.t. P^{-1} A P = B$

> 得到一个矩阵, 在合同的意义下变成对角阵

做法就是行变换和列变换都一起做. 做一次行变换就跟着做一次对应的列变换. 

> 利用定义来证明性质: 
> * 正定阵和半正定阵的和还是正定阵$x^T (A + B) x = x^T A x + x^T B x > 0 \Rightarrow $正定. 
> * 对称阵是$r$个对称阵的和: $\exist P s.t. A = P^T \mathrm{diag} (\lambda_1, \cdots, \lambda_r, 0) P = \sum P^T \mathrm{diag} (0, \lambda_i, 0) P$
> * $A$反对称$\Leftrightarrow X^T A X = 0$    
>   $(X^T A X)^T = X^T A^T X = - X^T A X \Rightarrow 0$

> 如何巧妙地计算合同(化二次型为标准型): 
> * 配方法
> * 主子式: $\Delta_0 = 1, \Delta_i$, 这样的话, 就可以用$f(x_1', \cdots, x_n') = \sum \frac{\Delta_{i}}{\Delta_{i-1}} x_i'^2$
> * 反对称阵: $A \simeq \mathrm{diag}\left(\left(\begin{array}{ll} 0 & 1 \\ -1 & 0\end{array}\right), \left(\begin{array}{ll} 0 & 1 \\ -1 & 0\end{array}\right), \cdots, O\right)$    
>   * 部分行列式来判断: 比如有一个$4 \times 4$的矩阵, 那么不妨计算它的一个$3 \times 3$的行列式, 若这个行列式不为零, 那么就可以说明$\mathrm{rank} F = 4$了. 
> * 实对称阵

## 后记
及格就行. 

(考试破防的一刻: 不是我做不出来, 而是我一道题做了出来, 发现证明短得让人害怕. 另一道题自以为证了出来, 却发现和老师的想法并不一样... )