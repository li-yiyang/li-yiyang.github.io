---
layout: post
title:  "Linear Algreba The End"
date: 2022-06-28 11:38:13 +0800
math: true
categories: notes
---
# Linear Algreba
## 线性算子 = 线性变换
线性映射: 

$$f: V \rightarrow W$$

* 生成元的像为像的生成元    
  理解就是$f(\sum x_i e_i) = \sum x_i f(e_i)$, 线性的体现. 
* $\dim \mathrm{Im} g f \leq \min(\dim \mathrm{Im} f, \dim \mathrm{Im} g)$
* 同构第一基本定理: $\hat{f}: V/\mathrm{Ker} f \rightarrow \mathrm{Im} f; \hat{v} \rightarrow f(v)$为线性同构.    
  对$\dim V < \infty, \dim \mathrm{Ker} f < \infty, \dim \mathrm{Im} f < \infty$, 即有限维情况下$\dim V = \dim \mathrm{Ker} f + \dim \mathrm{Im} f$     
  于是可以得到推论: 单射$f, \dim \mathrm{Ker} f = 0 \Leftrightarrow \dim V = \dim \mathrm{Im} f$.     
  而对$\dim V = \dim W < \infty f$单 $\Leftrightarrow f$ 满 $\Leftrightarrow f$同构

* 理论就是线性算子和矩阵同构($M: \mathcal{L}(V,W) \rightarrow M_{m \times n}(K)$为一个线性同构), 于是可以说线性算子可以用矩阵来表示. 
* 向量空间的泛性质: $W$中的一组$w_i$向量, 存在唯一$f: V \rightarrow W$使得$v_i \mapsto w_i$
* $rank M_f = \dim \mathrm{Im} f$为$f$不变量, 记为$\mathrm{rank} f$    
  同为不变量的还有$tr(A)$, 因为矩阵变换不改变迹. 

### 线性算子代数
* $K-$代数: 在环$(R, +, \cdot)$的基础上满足加法和纯量乘法(向量空间)和$(\lambda a) b = \lambda (a b) = a (\lambda b)$. 
* 子代数: 子环与子空间
* $\mathcal{L}(V)$的由$\mathcal{A}$生成的子代数: $K[A]$的包含$\mathcal{A}$的最小的有单位元的子代数. 

**极小多项式**: $\mu(\mathcal{A}) = \mathcal{O}, \mu_{\mathcal{A}}(t)$
* 任意线性算子存在极小多项式, 且$\deg \mu_{\mathcal{A}}(t) = \dim K[A]$    
  $\mathcal{A}$可逆$\Leftrightarrow \mu_{\mathcal{A}}(t)$的常数项非零
* $\forall f(\mathcal{A}) = \mathcal{O}, \mu_{\mathcal{A}}(t) \mid f(t)$    
  说明了极小多项式唯一. 


**特征多项式**: $\chi_{\mathcal{A}}(t) = \det(t E - A) \in K[t]$
* 特征方程: $\chi_{\mathcal{A}}(t) = 0 $, 对应的解为特征根. 
* 相似的矩阵特征多项式相等
* $n \geq 1$的复向量空间$V$上的任意线性算子均存在特征值和特征向量. 
* **代数重数**: $\lambda$作为$\chi_{\mathcal{A}}(t)$的根的重数.    
  几何重数($\dim V^\lambda$)小于代数重数. 
* 可对角化: $n$维$K-$向量空间上的线性算子$\mathcal{A}$有$n$个不同的特征值, 那么就是可对角化的. 

### 线性算子和矩阵
因为线性算子和矩阵之间有一个同构, 所以线性映射(算子)可以用矩阵来表示. 令$f(v_i) = \sum_j a_{ij} w_j$, 于是可以写出变换矩阵. $A = (a_{ij})$. 

(线性算子)矩阵的相似: 同一线性算子的不同基$e_1, \cdots, e_n \rightarrow e_1', \cdots, e_n'$(变换矩阵为$B$)下的表现: $A \sim A' = B^{-1} A B$(先变到一个$A$的基, 变换完了之后就变回$A'$的基). 

### 不变子空间和特征向量
不变子空间满足$\mathcal{A} U \subseteq U$, 将$V$直和分解为不变子空间的直和. 

> 实向量空间存在1或2维不变子空间, 复向量空间存在1维不变子空间. 

特征向量: $\mathcal{A} X = \lambda X$, 于是可以将空间根据特征向量来分解成几个不变子空间(生成元就是特征向量)的直和. 

(空间可以直和分解为几个子空间$V = \oplus W_i$. 和多项式(或者代数基本定理? )类比, 将线性算子看作是$\chi_{\mathcal{A}}(t) = \prod p^{\alpha_i}(t)$. 或者也可以看作是$\mathcal{A} = \dot{+} \mathcal{A}_i$, 于是将$A$分解为对角线上的分块矩阵, 和子空间对应. )

### 对偶子空间

### 根子空间分解

### 特殊的几个线性算子
* 零算子
* 恒等算子
* 相似算子
* 旋转算子
* 投影算子$\mathcal{P}: V \rightarrow V, \sum x_i \mapsto x_i$    
  将空间直和分解, 只保留其中一个子空间的分量的算子. 

偏概念的东西: 
* 对偶算子:    
  $$*: \mathcal{L}(V) \rightarrow \mathcal{L}(V^*), \mathcal{A} \mapsto \mathcal{A}^*$$
  
  $$\mathcal{A}^*: V^* \rightarrow V^*, f \mapsto f \mathcal{A}$$

  $\mathcal{A}$在确定基底下矩阵$A$, 对应的对偶算子矩阵$A^* = A^T$. 
* 商算子:    
  
和内积空间有关的线性算子: 
* $\theta$型(双线性型和半双线性型的统称)$$=\left\{\begin{array}{ll}双线性型 \theta = 2 & K = \mathbb{R} \\ 半双线性型 \theta = \frac{3}{2} & K = \mathbb{C}\end{array}\right.$$
* 伴随算子: $(\mathcal{A} x \vert y) = (x \vert \mathcal{A}^* y)$(算子的伴随在矩阵上的表现就是一种转置共轭, 但是矩阵的伴随是稍微不一样的一个东西. )
* $\mathcal{A} = \mathcal{A}^*$称为自伴随算子(埃尔米特算子)
* 保距算子$\Leftrightarrow$酉算子


## 若尔当标准型
**哈密顿-凯莱定理**: 特征多项式一定零化线性算子. 即$\chi_{\mathcal{A}}(\mathcal{A}) = \mathcal{O}$.  

**若尔当标准型基本定理**: 任意复方阵的若尔当标准型$J = \mathrm{diag}J_{m_i}(\lambda_i)$存在, 且在不计若尔当块($$J_m(\lambda) := \left(\begin{array}{llll} \lambda & 1 & & \\ & \ddots & \ddots & \\ & & \lambda & 1 \\ & & & 1\end{array}\right)$$)的排列顺序的意义下是唯一的. 

### 应用

### $\lambda$矩阵: $A(\lambda)$
就是以多项式$f_{ij}(\lambda)$为元素的矩阵叫做$\lambda$矩阵. 于是可以类似普通的矩阵定义秩, 伴随$A(\lambda)B(\lambda) = B(\lambda)A(\lambda) = \vert A(\lambda) \vert I$(矩阵的伴随和算子的伴随还是有点不一样的. ), 逆矩阵$A(\lambda)B(\lambda) = I$和初等变换: 
* 行(列)乘以$c \neq 0$常数. 
* 行(列)乘以$\mu(\lambda)$加到另外的行(列). 
* 交换行(列). 

和一般的矩阵类似$A(\lambda) \simeq B(\lambda) = P(\lambda) A(\lambda) Q(\lambda) = B(\lambda)$. 

行列式因子($D_i(\lambda)$为非零$i$阶子式的首系数为$1$的最大公因式. )与不变因子$\frac{D_i(\lambda)}{D_{i-1}(\lambda)}$(认为$D_0 = 1$). 并且行列式因子相等的$\lambda$矩阵秩相同, 可以认为等价$$A(\lambda) \simeq B(\lambda) \simeq \left(\begin{array}{llll}\phi_i(\lambda) & & & \\ & \ddots & & \\ & & \phi_r(\lambda) & \\ & & & O\end{array}\right)$$. 并且$\phi_i(\lambda)$为$A(\lambda)$的不变因子. 

行列式因子的用处: 一个矩阵怎么变, 只要还是相似的话, 行列式因子应该就会不变, 于是利用这个, 就可以来做题目. 

> $$\begin{array}{lll}\lambda I - A & = & \left(\begin{array}{llll}\lambda + 2 & -2 & -3 & 1\\ -3 & \lambda & 2 & -2\\ -1 & -1 & \lambda - 2 & -1 \\ -1 & 1 & 3 & \lambda\end{array}\right)\\ & \rightarrow & \left(\begin{array}{llll}0 & 0 & 0 & 0\\ 2 \lambda + 1 & \lambda - 4 & -4 & 0\\ \lambda + 1 & -3 & \lambda - 5 & 0 \\ -\lambda^2 - 2\lambda -1 & 2\lambda + 1 & 3\lambda + 3 & \lambda\end{array}\right)\end{array}$$

#### 利用$\lambda$矩阵来计算Jordan标准型. 

对特征矩阵$\lambda I - A$通过行列变换得到$$\left(\begin{array}{llll}I & & & \\ & (\lambda - \lambda_i)^{k_i} & & \\ & & \ddots & \\ & & & & (\lambda - \lambda_r)^{k_r}\end{array}\right)$$. 

于是能够说明Jordan矩阵为$$\left(\begin{array}{lll}I & &\\ & J_{k_i}(\lambda_i) & \\ & & \ddots\end{array}\right)$$

并且在计算的过程中, 消元的技巧如下: 
* $d = \gcd(f, g) = \varphi f + \psi g$, 那么就只好用辗转相除(带余除法)来计算得到$d$.    
  (注意, 根据规定, 我们不能够对行(列)乘以一个多项式, 因为这样的矩阵不可逆. )
* $f \mid g$, 那么直接就消掉了. 

### 利用Jordan标准型来计算正交阵
$$T^T A T = \left(\begin{array}{llll}I_r & & & \\ & -I_s & & \\ & & \left(\begin{array}{ll} \cos\theta_1 & -\sin\theta_1\\\sin\theta_1 & \cos\theta_1\end{array}\right) & \\ & & & \left(\begin{array}{ll} \cos\theta_t & -\sin\theta_t\\\sin\theta_t & \cos\theta_t\end{array}\right)\end{array}\right)$$

## 内积空间
$(* \vert *): V \times V \rightarrow \mathbb{R}, (x, y) \mapsto (x \vert y)$.

于是可以定义长度(有三角不等式)和夹角. 并且可以定义正交. 任意的$n$维欧氏空间和埃尔米特空间都有标准正交基. 

> 格拉姆-施密特正交化: 往一组已经正交化的$e_1, \cdots, e_{n-1}$的向量中加入一个向量. 
> 只需要$v_n = v_n - \sum (v_n \vert v_i) v_i \rightarrow e_n = \frac{v_n}{\vert v_n \vert}$, 即可. 

<details>
<summary>复习摸鱼</summary>

{% highlight scheme %}
(define (num-mul num vec)
  (map (lambda (xi) (* num xi)) vec))

(define (double-apply a b f)
  (cond ((null? a) b)
        ((null? b) a)
        (else
          (append (list (f (car a) (car b)))
                  (double-apply (cdr a) (cdr b) f)))))

(define (add a b)
  (if (and (list? a) (list? b))
      (double-apply a b (lambda (x y) (+ x y)))
      (+ a b))

(define (sub a b)
  (if (and (list? a) (list? b))
      (double-apply a b (lambda (x y) (- x y)))
      (- a b)))

(define (mul a b)
  (if (and (number? a) (number? b))
      (* a b)
      (if (number? a)
          (num-mul a b)
          (num-mul b a))))

(define (sum list f)
  (if (null? (cdr list))
      (f (car list))
      (add (f (car list))
         (sum (cdr list) f))))

(define product
  (lambda (a b)
    (if (or (null? a)
            (null? b))
        0
        (+ (* (car a) (car b))
           (product (cdr a) (cdr b))))))

(define (gram-schmidt-process bases new-base)
  (sub new-base
    (sum bases
      (lambda (base)
        (mul (product base new-base) base)))))

;; (gram-schmidt-process '((1 0 0) (0 1 0)) '(0 1 1))
;; => (0 0 1)
{% endhighlight %}

不太行, 这个有点浪费时间... (不过基本的东西都搞好了, 剩下的就是一个简单的修正了. 留给计算机代数系统来搞吧. )
</details>

于是可以定义对偶基$(- \vert e_i)$为$e_i$的对偶基. 

### 欧氏空间和埃尔米特空间

| 性质       | 欧几里得空间             | 埃尔米特空间                   |
| ---------- | ------------------------ | ------------------------------ |
| 内积的定义 | 正定性, 双线性性, 对称性 | 半双线性性, 共轭对称性, 正定性 |

### 酉矩阵
满足$A A^* = E, A^* := \bar{A^T}$叫做酉矩阵(Unitary). $\det A = 1$看出并不改变长度. $n$阶酉几何关于矩阵的乘法做成一个群, 称为$n$阶酉群. (几何直观就是旋转了一个$n$维向量空间中的向量. )

### 规范型
#### 化埃尔米特型二次型维规范型
1. $\chi_{\mathcal{A}}(t) = \det(\lambda E - A) = 0 \Rightarrow \lambda_i$为根
2. 解出$(\lambda E - A) X = 0$的特征向量$X_i$, 这些向量应该满足相互正交. (利用这个性质就可以了, 比如特征方程$(x-\lambda_1)(x-\lambda_2)^2=0$, 那么就能够说$X_1$肯定会和$X_2, X_3$正交, 然后对$X_2, X_3$之间相互使用正交化得到正交向量即可. 这样得到的$X_i$的组合排在一起就是变换矩阵$T = (X_1\ X_2\ X_3)$了. )
3. (如果对上面的基有特殊要求的话, 那么就标准化为单位向量即可. )

## 习题
### Jordan标准型
给定$A$, 求$P$, s.t. $P^{-1} A P = \mathrm{diag} (J_m)$. 
1. $\lambda$矩阵: 
  $$\lambda I - A \simeq \left(\begin{array}{lll} 1 & & \\ & 1 & \\ & & (\lambda - 1) (\lambda - 2)^2\end{array}\right) \Rightarrow A \sim \mathrm{diag} \{J_1(1), J_2(2)\}$$     
  然后解方程$A P = P J$, 将$P$看作是$(\xi_1, \xi_2, \xi_3)$的分块组合, 然后解出即可. 
2. $\det (\lambda I - A) = (\lambda - 1)(\lambda - 2)^2$    
   然后需要注意的是$\lambda = 2$的一个rank的大小. 需要判断一下. 

### 正交相似
$A$实对称, 求$T$正交, s.t. $T^T A T = \mathrm{diag}(\lambda_1, \lambda_2, \lambda_3) = T^{-1} A T$

通过保距变换将实二次型化为规范型. 

$$q(x_1, x_2, x_3) = (x_1, x_2, x_3) \left(\begin{array}{lll} 1 & -2 & 2 \\ -2 & -2 & 4 \\ 2 & 4 & -2\end{array}\right)\left(\begin{array}{l} x_1 \\ x_2 \\ x_3\end{array}\right)$$

(特别要注意的是相同特征根之间的向量的正交化. 并且因为是要求规范型, 所以"可由保距变换化为$\lambda_1 y_1^2 + \lambda_2 y_2^2 + \lambda_3 y_3^2$")

### 正定矩阵证明
* $0 \leq a_1 \leq a_2 \leq \cdots \leq a_n, A = (\frac{1}{a_n + a_j - 1})$    
  直接考虑构造内积$(f | g) = \int_0^1 f g x^{-2}\mathrm{d}x$, 向量空间为$$\sum \alpha_{i} x^{a_i} = \langle x^{a_1}, \cdots, x^{a_n} \rangle$$. 于是因为$f \geq 0, g \geq 0$所以就可以证明正定性.    
  内积是正定的$y^T A x \geq 0 \Rightarrow \forall x, x^T A x > 0 \Leftrightarrow A$正定. 

### Jordan 矩阵
1. $J_n(\lambda)^k \sim J_n(\lambda^k)$
2. $J_n(\lambda)^{-1} \sim J_n(\lambda^{-1})$    
  证明: $$J_n(\lambda) = \lambda \left(\begin{array}{lll} 1 & \frac{1}{\lambda} & \\ & 1 & \frac{1}{\lambda} \\ & & 1\end{array}\right) \sim J_n(1)$$    
  $\Rightarrow J_n(\lambda)^{-1} \sim \lambda^{-1} J_n(1)^{-1}$    
  又因为$J_n(1) = I_n + A, A^n = O \Leftrightarrow (I_n + A)(A^{n-1} - A^{n-2} + \cdots) = I_n$    
  $\Rightarrow J_n(1)^{-1} = (A^{n-1} - A^{n-2} + \cdots) \sim J_n(1)$

### 其他
* $f: \mathbb{Z} \rightarrow \mathbb{Z}/n\mathbb{Z}, k \mapsto \bar{k}$形成了一个(商)群. 一种泛性质. 
* $V / \mathbb{C}, \dim V = n, \mathcal{A} = \mathcal{L}(V)$则$\forall 1 \leq k \leq n, \exists W \subseteq V, \dim W = K$, s.t. $W$为$\mathcal{A}$不变子空间.     
  * 矩阵的证明: $$\mathcal{A}(u_1, \cdots, u_n) = (u_1, \cdots, u_n) A \Rightarrow A ((u_1, \cdots, u_n) P) = (u_1, \cdots, u_n) P \left(\begin{array}{ll} \lambda_1 & * \\ & \lambda_n\end{array}\right)$$
  * 商空间的证明: 对$\dim V = n$做归纳
    * $n= 1$显然成立
    * $n$时成立, 考虑$\dim V = n + 1$的情况, 取$\lambda$为特征值, $\xi$为对应的特征向量. $A \xi = \lambda \xi \Rightarrow \langle \xi \rangle$为不变子空间, 于是$(V / \langle \xi \rangle)_{\dim = n}$也是$\mathcal{A}$不变子空间. 于是由归纳假设就能够知道. 
* $V$为酉空间, 复内积空间, $\mathcal{A}$为正规算子, $\mathcal{B}$为算子, 若$\mathcal{A}\mathcal{B} = \mathcal{B}\mathcal{A}$, 则$\mathcal{A}\mathcal{B}^* = \mathcal{B}^*\mathcal{A}$    
  证明: $\mathcal{A}$正规算子$\Rightarrow \exists u_1, \cdots, u_n, \mathcal{A}u_{ij} = \lambda_i u_{ij}$    
  要证明$\mathcal{A}\mathcal{B}^{\circledast} = \mathcal{B}^{\circledast}\mathcal{A}$, 即证明$\langle \mathcal{A}\mathcal{B}^{\circledast}, u_{kk} \rangle = \langle \mathcal{B}^{\circledast}\mathcal{A} u_{i j}, u_{kk} \rangle$

## All Done...
就这样吧... 生活不只是诗和远方, 而是眼前的苟且偷生... 