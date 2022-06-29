---
layout: post
title:  "Discreate Mathematica The End"
date: 2022-06-29 16:48:57 +0800
math: true
categories: notes
---
# 离散数学
## 函数
### 定义与相关概念
* 从集合的观点, 将函数看作是一种特殊的二元关系. $x F y \Rightarrow y = F(x)$. 
* $F = G \Rightarrow F \subseteq G \wedge G \subseteq F$, 即函数的相等与表达式无关, 仅与定义域(dom), 值域(ran)和映射关系$F(x) = G(x)$有关. 
* 映射的集合$B^A := \{f \vert f: A \rightarrow B\}$
  1. $A = \varnothing \wedge B = \varnothing \Rightarrow B^A = \varnothing^\varnothing = \{\varnothing\}$
  2. $A = \varnothing \wedge B \neq \varnothing \Rightarrow B^A = B^\varnothing = \{\varnothing\}$
  3. $A \neq \varnothing \wedge B = \varnothing \Rightarrow B^A = \varnothing^A = \varnothing$
* 满射$\mathrm{ran}f = B$, 单射$\forall y \exist !x \in f(x) = y \Leftrightarrow ker f = 0$, 双射

### 通过~~函数~~映射来研究问题
* 集合等势: $A \approx B \Leftrightarrow f: A \leftrightarrow B$, 即存在双射.     
  例: $\mathbb{Z} \approx \mathbb{N} \approx \mathbb{Q} \not{\approx} \mathbb{R}$    
  又: $A \approx A \times A$; 注: $\mathrm{card} \mathbb{N} = \alef_0, \mathrm{card} \mathbb{R} = \alef$
* 集合的优势: $A \lessdot B \Leftrightarrow f: A \leftarrow B$, 即存在一个单射. (真优势是一个非满射. )

## 图 - 概念超级多
无序基: $$A \& B = \{ \{a, b \} \vert a \in A \wedge b \in B \} = B \& A$$. 

(对应的有序对就是笛卡尔积. )

### 无向图与有向图以及相关概念
* $G = \langle V, E \rangle$有序二元组. 其中: 
  * $V$为非空有穷集, 称为**顶点集**. 元素为**顶点**或**结点**. 
  * $E = V \& V$时, 为有穷多重子集, 称为**边集**, 元素为**无向边**. $G$为无向图. 
  * $E = V \times V$时, 为
* 顶点数(图的**阶数**): $\vert V(G) \vert$, 边数: $\vert E(G) \vert$
* 边数为零的图叫做**零图**, 记作$N_{\vert V(G) \vert}$. 其中$N_1$称为平凡图. (毕竟就只有一个点嘛. )
* $V(G) = \varnothing$的图称为**空图**, 记为$G = \varnothing$ (看来顶点更加好用诶. )
* 标定图和非标定图. 点和边是否有名字. 
* **有向图的基图**: $E = V \times V$退化为$E = V \& V$, 失去方向信息. 
* 边$e = (v_i, v_j)$与点$v_i$的关系, 通过**关联次数**来体现: 关联次数就是点在边上出现的次数. 关联次数为$2$的话就是一个**环**. 
* 相邻: $v_i$与$v_j$相邻$\Leftrightarrow \exists e = (v_i, v_j)$, $e_i$与$e_j$相邻$\Leftrightarrow e_i = (v_i, v_k), e_j = (v_k, v_j)$. 
* 孤立点: 没有边关联的顶点. 
* **无向图**中, **平行边**两个顶点一样. 一对顶点的平行边的数量叫做**重数**. 有平行边的叫**多重图**. 没有的叫**简单图**. 
* **无向图**中链接顶点的**边数**的数量叫**度数**($d_G(v)$). 在**有向图**里面被叫做**入度**($d_D^-(v)$进入的边数)和**出度**($d_D^+(v)$出去的边). (向外出用加号. ) 合起来在有向图叫做度数$d(v)=d^-(v)+d^+(v)$    
  并且还可以定义最大(出/入)度$\Delta(v)$和最小(出/入)度$\delta(v)$.    
  很明显, 应该有结论(握手定理): 
  * 无向图中, 所有顶点的度数之和等于边数的两倍. $\sum_i d(v_i) = 2 \vert E(G) \vert$
  * 有向图中, 所有顶点的度数之和等于边数的两倍(退化为基图就知道了). 所有顶点的入度之和等于所有顶点的出度之和. (有点像是流入和流出相等啊. )$\sum_i d^+(v_i) = \sum_j d^-(v_i)$
  * 奇度顶点个数为偶数. (因为顶点度数之和为偶数, 偶数个奇数之和才是偶数. )
  * $n$阶无向简单图中$\Delta(G) \leq n - 1$. (因为只能和$n-1$个点连啊. )
* 图的**同构**: $G_1 \simeq G_2 \Leftrightarrow \exists f: V_1 \leftrightarrow V_2, e = (v_i, v_j) \leftrightarrow f(e) = (f(v_i), f(v_j))$
* **完全图**: **$n$阶无向简单图**中每个顶点都和$n-1$个顶点相邻. (记作$K_n$), 变数$\frac{n(n-1)}{2}$    
  对于**有向简单图**, 每个顶点都连接到$n-1$个顶点. 边数$n(n-1)$    
  基图是$K_n$的有向简单图称为$n$阶竞赛图. 边数$\frac{n(n-1)}{2}$
* **补图**(类比补集): (补图是相对于完全图的边的补集. $\hat{G} = \langle V, \hat{E}_{= K_n - E} \rangle$)    
  对于$G \simeq \hat{G}$的$G$为自补图. 
* **$k$-正则图**: $\forall v \in V d(v) = k$    
  彼得松图是$3$正则图. 
* 子图和母图(类比子集, 是边和顶点的关系. )$G'\subseteq G \Leftrightarrow V' \subseteq V \wedge E' \subseteq E$, 同理可定义真子图. $V' = V$时为**生成子图**. 还有导出子图. 
* 删除边: $G - e$, 删除顶点$G - v$(同时删除关联边), 收缩边$G \diagdown e$(把边用点来替换), 加新边: $G \cup (u, v)$(在两个点之间加新边). 
* 通路: $\Gamma = v_{i_0} e_{j_1} v_{i_1} e_{j_2} \cdots e_{j_l} v_{i_l}$, 始点$v_{i_0}$, 终点$v_{i_l}$, 长度为边的数量.
  * 当$v_{i_0} = v_{j_l}$称为回路. 
  * 当$e_{j_m} \neq e_{j_n} (m \neq n)$称为简单通路(同理简单回路). 
  * 初级路径(路径): 不经过相同的**点**和**边**(始点和终点重合除外, 叫做初级回路(回路), 根据长度称为奇(偶)圈); 有**边**重复出现的为复杂通路(回路).    
    显然有最大的初级通路$n-1$, 并且若存在通路, 则一定存在长度小于$n-1$初级通路. 
* 带权图: $G = \langle V, E, W \rangle, W : E \rightarrow K, e \mapsto w$, (就是每个边对应一个权重. ) 于是可以定义边长度$W(P)$和路径的**距离**以及**最小路径**.    
  最短路径算法(Dijkstra标号法). (不好一下子掌握, 书上写的伪代码太草了. 一个简单的感觉就是深度优先搜索算法, 在不产生重复路径的基础上找到最小的路径, 不过每次都走最短的路径? 是这样吗? )
* 连通性: $u \sim v$说明$u$, $v$之间存在通路, 是联通的. (是一个等价关系. )
* 连通分支: 对于$V_i$($V$关于顶点连通性的一个等价类)的导出子图$G[V_i]$称为**连通分支**.    
  $p(G)$为$G$的连通分支数. 对于连通图: $p(G) = 1$, 对于非联通图$p(G) \geq 2$. 对于零图$p(G) = n$. (直观理解就是把所有连在一起的点全部缩在一起, 不连通的就分开了. )
* 连通程度: 
  * 割点(集): $V' \subset V, p(G - V') > p(G)$, 对于任意的$V'' \subset V', p(G - V'') = p(G)$, 称$V'$为割点集. 当只有一个元素的时候, 称这个点(元素)为割点. (删除点)
  * 割集(割边): 定义和割点集类似. (删除边)
  * 连通度: 点连通度 $$\kappa (G) = \min\{\vert V' \vert 为点割集\}$$, $\kappa$连通图.     
    边连通度 $$\lambda (G) = \min \{\vert E' \vert 为边割集\}$$, 若$\lambda(G) \geq r$, 则$G$为$r$边连通图. 
  * 对于无向图: $\kappa(G) \leq \lambda(G) \leq \delta(G)$
  * 对于有向图: 两点之间存在通路. $v_i \rightarrow v_j$称为可达. $v_i \leftrightarrow v_j$称为相互可达.     
    有向图的基图是连通图, 称为(弱)连通图; $\forall v_i, v_j \in V, v_i \rightarrow v_j \vee v_j \rightarrow v_i$称为单向连通图; 同理有强连通图(双向连通). 强连通图一定存在经过每个顶点至少一次的回路. 
* 极大路径: $\Gamma$的始点和终点都不与$\Gamma$外的顶点相邻, 称为极大路径. (如果相邻, 那么就扩大至改点. )
* 表示方法: 
  * 集合: $$V = \{v_i\}, E = \{(v_i, v_j)\} \mathrm{or} \{\langle v_i, v_j \rangle\}$$
  * 画图(略)
  * 关联矩阵: $$M(G) = (m_{ij})_{n \times m}$$, 其中$m_{ij}$为顶点$v_i$与$e_j$的关联次数. 
  * 临接矩阵: $$M(G) = (a_{ij}^{(1)})_{n \times n}$$, 其中$a_{ij}^{(1)}$为$v_i$到$v_j$顶点之间的边数. 
  * 可达矩阵: $$M(G) = (p_{ij})_{n \times n}$$, 其中$$p_{ij} = \left\{\begin{array}{ll}1 & v_i 可达 v_j \\ 0 & 反之\end{array}\right.$$

### 树
* 无向树定义: 
  * 任意两个顶点之间存在唯一路径
  * 没有回路, 边数$m =$顶点数$n-1$
  * 连通, $m = n - 1$
  * 连通, 任何边都为桥
  * 没回路, 任意两点之间加上边得到圈
* 无向树的一些名词: 
  * 森林: 每个连通分支都是树的无向图
  * 树叶: 无向树中的悬挂顶点
  * 分至点: 度数大于二的顶点
  * 生成树: 生成子图是树, 叫生成子图为生成树. 在生成树中的边叫树枝, 不在生成树的叫弦, 弦的导出子图叫余树$\hat{T}$.     
    有生成树的图为连通图. 并且在无向连通图中$m \geq n - 1$
  * 最小生成树: 把树的边带上权值, 权值最小的叫做最小生成树. (避圈法)    
    (算法类似于深度优先算法, 先沿最小路径走到底(出现回路), 然后返回到第一个不会出现回路的分支点, 然后重复. )
* 有向树: 基图是无向树的有向图
  * 根树: (树根)入度为零, 其余顶点入度为一, 并且树叶出度为零, 出度也是为一的叫内点, 出度不为一的叫分支点. 层数, 树高, 家族树, 祖先后代, 兄弟...
  * 有序树: 对根树中相同层的元素都标定次序的树. 
  * $r$叉树: 每个顶点至多$r$个儿子. 恰有$r$个儿子的叫做$r$叉正则树. 每片树叶的层数都是树高的, 叫做$r$叉完全正则树. 
  * 在二叉正则有序树中: 左根树, 右根树    
    二叉树的历遍方法: 左儿子, 树根, 右儿子, ... 
  * Huffman 算法以及huffman编码. 
<details><summary>Huffman算法</summary>
{% highlight ruby %}
  def count(nodes)
    point.is_a?(Array) ? point.map{|p| count(p)}.sum : point
  end
  def huffman_tree(nodes)
    return nodes if nodes.length <= 1
    sort = nodes.sort_by { |point| count(point) }
    huffman-tree([sort[0..1]].append sort[2..])
  end
{% endhighlight %}
</details>

* 性质: 
  * $n$阶非平凡无向树, $T$至少有两片树叶. 

### 特殊的图
#### 欧拉图 - 七桥问题, 一笔画问题
* 欧拉通路: 经过所有边一次, 且经过所有顶点的通路.
* 欧拉回路: 通路变成回路. 
* 欧拉图: 具有欧拉回路的图. 半欧拉图: 只有通路, 但是没回路. 
* 是欧拉图当且仅当: 
  * 无向图$G$连通且没有奇度顶点
  * 有向图$D$是强连通的并且每个顶点的入度等于出度
* 是半欧拉图当且仅当:
  * 无向图连通的, 恰有两个奇度顶点
  * 有向图是单向连通, 又两个奇度顶点, 一个入度比出度大$1$, 另一个出度比入度大$1$, 其余顶点入度等于出度. 

#### 哈密顿图 - 在欧拉图的基础上放弃经过所有边一次的前提, 但是加上只经过一次点的前提. 
* 哈密顿通路: 经过顶点一次且仅一次的通路
* 哈密顿回路: 定义类似上面的东西
* 哈密顿图: 具有哈密顿回路的图. 半哈密顿图. 
* $n$阶无向简单图中任意不相邻的顶点$u$, $v$都有$d(u) + d(v) \geq n - 1$, 则存在哈密顿通路.     
  一个推论是当$n\geq 3$时, $d(u) + d(v) \geq n$存在哈密顿回路.     
  (该结论一般用来证明不是哈密顿通路, 证明是哈密顿通路的方法一般是给出一个哈密顿通路. )
* 货郎问题: $n$个城市仅经过一次, 最后回到出发点, 路程最短. (没有有效算法. )

#### 二部图
* 定义: 将图中的顶点分成两组不交的集合, 使得边$$V = V_1 \dot{+} V_2, E = \{e = (v_i, v_j) \vert v_i \in V_1, v_j \in V_2\}$$. 并且定义零图为二部图. 
* 无向图是二部图: $G$中没有奇圈. 
* 匹配: (字面意思理解就是$V_1$和$V_2$中的顶点可以两两连线, 任意两条边不会相邻. ). 完美匹配. 选定一个匹配, 则有: 非匹配边, 匹配边, 饱和点(和匹配边相关联), 非饱和点, 交错路径(匹配边和非匹配边交替构成的路径), 可增广的交错路径(起点终点都是非饱和点)
* 最大匹配: 没有可增广的交错路径
* 相异性条件: $V_1$中任意$k$个顶点至少与$V_2$中$k$个顶点相邻则存在完备匹配($\vert M \vert = \vert V_1 \vert$). ($\vert V_1 \vert \leq \vert V_2 \vert$)
* 存在$t$使得$V_1$中顶点至少关联$t$边, $V_2$中至多关联$t$条边, 则存在完备匹配. 

#### 平面图
* 定义: 将无向图画在平面上, 使得边除了在顶点处不会相交的图. 
* 平面图的子图是平面图. 非平面图的母图是非平面图
* 平面嵌入(就是把图画成平面图). 面: 平面图将平面分割成几个面. 无限面(外部面), 有限面(内部面), 边界, 面的次数(边界长度). 
* 平面图的次数之和为边数的两倍. 
* 极大平面图: 简单平面图任意两个不相邻的顶点之间加一条边后就会成为非平面图的图. 
* 极大平面图: 每个面的次数为$3$
* 极小非平面图: 非平面图任意两个不相邻的顶点之间减一条边后就会成为平面图的图. 
* 欧拉公式: 顶点数$n$, 边数$m$, 面数$r$, 则$n - m + r = 2$    
  对于有$k$个连通分支的平面图, 有$n - m + r = k + 1$. 
* 连通的平面图, 每个面的次数至少$l \geq 3$, 则边数$m$和顶点$n$的关系: $m \leq \frac{l}{l - 2} (n - 2)$

#### 图着色问题
无向图无环, 对每个顶点涂一个颜色, 是的相邻的顶点颜色不同, 称图为一个**点着色**, 用$k$种颜色给图着色叫做$k$-可着色. 

* 二部图: $2-$可着色

**面着色**. 利用对偶图将面着色问题转化为点着色问题. (对偶图就是把面看作顶点, 把边和原来的边相切. )

* 任何平面图都是$4-$可着色的. 

## 数论
### 整除
* 整除: $a \mid b \Leftrightarrow b = k a$    
  $p \mid a b \Leftarrow p \mid a, p \mid b, p$素数.    
  唯一分解定理: $\forall n, n = \prod_i p_i^{\alpha_i}$
* 带余除法: $b = k a + r, 0 \leq a < b$
* 最大公因子: $(a, b) = d = \varphi a + \psi b$    
  (辗转相除法, 并且如何计算$\varphi, \psi$. )
* 整数部分(向下取整): $[x] \leq x [x] + 1$
* $a^k \mid\mid b$恰被$k$次方整除: $a^k \mid b, a^{k+1} \nmid b$
* $a = \alpha (p, n) = \sum_{j = 1}^\infty [\frac{n}{p^j}], p^\alpha \mid\mid n!$

### 同余
* $a \equiv b (\mathrm{mod} m)$
* 于是根据同余建立等价类(一个环)    
  * 同余类中任意两个整数($\bar{a_1} = \bar{a_2} = \bar{x}$)和$m$的最大公约数相等. 
  * 完全剩余系: $$\{\bar{0}, \bar{1}, \cdots, \bar{m - 1}\}$$
  * 既约同余类(互素同余类): $(r, m) = 1$, 既约同余类的个数记为$\phi(m)$(Euler函数)    
    $$\phi (m) = m \prod_{p \mid m}(1 - \frac{1}{p})$$
  * 既约剩余系: 在完全剩余系的基础上加上一个和$m$互素的条件. 即任意和$m$互素的元素都能在既约剩余系中找到等价类. $$\{\bar{1}, \cdots, \bar{r}, \cdots \bar{m-1}\}, (r, m) = 1$$
* 模逆元: $c a \equiv 1 (\mathrm{mod} m)$    
  计算方法: $(a, m) = 1 = \varphi a + \psi m \Rightarrow c = \varphi$
* $$\mathbb{Z}/_{m} + c = \mathbb{Z}/_{m}$$: 给完全剩余系加上一个偏移量仍然是完全剩余系
* 既约剩余系加上$k m$的便宜量仍为既约剩余系(显然)
* $(a, m) = 1$, 于是$S$为完全(既约)剩余系的充要条件是$aS$为完全(既约)剩余系    
* 理论基础: $$\mathbb{Z}/_{m_1} + m_1 \mathbb{Z}/_{m_2} = \mathbb{Z}/_{m_1 m_2}$$
* $m = m_1 m_2, (m_1, m_2) = 1$, 于是$\bar{x} = m_2 \bar{x}^{(1)} + m_1 \bar{x}^{(2)}$为完全(既约)剩余系. 
* 中国剩余: 解同余方程. 

### 同余方程
* $a x \equiv b (\mathrm{mod} m)$有解的充要条件是$(a, m) \mid b$, 并且其解数为$(a, m)$. 解为$x \equiv x_0 + \frac{m}{(a, m)} t (\mathrm{mod} m)$
* 同余方程的笨解法: $x \equiv x_1 (\mathrm{mod} m_1), x \equiv x_2 (\mathrm{mod} m_2)$, 然后$\varphi m_1 + \psi m_2 = 1$, 于是可以有$x \equiv \psi x_1 + \varphi x_2 (\mathrm{mod} m_1 m_2)$然后以此类推. 
* $f(x) = a_n x^n + \cdots + a_0 \equiv 0 (\mathrm{mod} p), p$为素数的解数$\leq n$
* $f'(x) \equiv 0, f(x) \equiv 0 (\mathrm{mod} p)$没有公共解. 那么$f(x) \equiv 0 (\mathrm{mod} p^l)$的解数等于$f(x) \equiv 0 (\mathrm{mod} p)$的解数. 

### 欧拉定理和费马小定理
* $a^{\phi(m)} \equiv 1 (\mathrm{mod} m), (a, m) = 1$    
  $a^p \equiv a (\mathrm{mod} p), p$为素数. 

### 二次剩余
* $x^2 \equiv n (\mathrm{mod} m)$有解, 则$n$称为模$m$的二次剩余. 否则叫做二次非剩余. 
* $$\left(\frac{n}{p}\right) \left\{\begin{array}{ll} 1 & n 为模 p 的二次剩余 \\ -1 & n 不是模 p 的二次剩余 \end{array}\right.$$
* $$\left(\frac{n}{p}\right) = \left(\frac{n'}{p}\right)$$, $n \equiv n' (\mathrm{mod} p)$
* $p$为奇素数, 缩系中有$\frac{1}{2}(p - 1)$个二次剩余, $\frac{1}{2}(p-1)$个二次非剩余, 且$1^2, 2^2, \cdots, (\frac{1}{2}(p-1))^2$为二次剩余. 
* $p$奇素数, $p\nmid n$, $n^{frac{p-1}{2}} \equiv \left(\frac{n}{p}\right) (\mathrm{mod} p)$

## 习题
### 函数

### 图
#### 握手定理
> 每个顶点度数为$5$的$8$阶无向图有几条边

$\sum_i^8 d(v_i) = 8 \times 5 = 2 \vert E(G) \vert \Rightarrow \vert E(G) \vert = 20$

> 画出$n$阶非同构的无向树

树的定义说明$m = n - 1 = 5$, 握手定理说明$6$顶点的度数和为$10$, 利用$\delta(T) \geq 1, \Delta(T) \leq 5$就能够开始历遍顶点度数的可能性. 

#### 图同构
> $K_n$中非同构的圈

只需要判断圈的长度就可以判断是否同构了. $n-2$

#### 特殊的图
> $G$是非平凡欧拉图, $\lambda(G) \geq 2$

只要证明$G$的任一边都非桥, 假如是桥的话, 就不能返回, 所以不是桥. 

### 数论
#### 整除
* 证明整除
  > $3 \mid n(n+1)(2n+1)$

  这个只要证明$3 \mid n, 3 \mid n+1, 3 \mid 2n+1$有一个成立就好, 于是反证法. 

  > $(a x_0 + b y_0) \mid (a x + b y)$, 其中$a x_0 + b y_0$为最小的$a x+ b y$

  这个只要利用最大公因数的性质, 假如有比它更小的...

* 计算最大公因数
  > $(a^m - b^m, a^n - b^n) = a^{(m,n)} - b^{(m,n)}$

  实际上就是在对指数进行辗转相除而已. 

* 素数无穷多
  > $\prod p_i + 1$也是素数

  > $F_n = 2^{2^n} + 1$, $d \mid F_n, d \nmid F_m, m \neq n$

#### 同余
> 模的运算: 
> 1. $a n \equiv c (\mathrm{mod} m) \Leftrightarrow n \equiv a^{-1} c (\mathrm{mod} m)$
> 2. $(a b)^{-1} = a^{-1}b^{-1}$

类似的证明都是通过利用同余等式的乘法和加法来实现的. 

> $0^2, 1^2, \cdots, (m-1)^2$一定不是完全剩余系

只要证明存在$a^2 \equiv b^2$即可. 然后相减$(a + b)(a - b) \equiv 0$, 于是就能说$a + b = m$

> 既约剩余系$r_1, \cdots, r_s$为小于$m/2$切与$m$既约的正整数, 说明$-r_s, \cdots, -r_1, r_1, \cdots, r_s$为既约剩余系. 于是说$2 \mid \phi(m)$.    
> 并且得到既约剩余系的所有元素和为$\bar{0}$, 最小正既约剩余系的各数之和为$m\phi(m)/2$

只要证明不存在和$m$既约但是又不在$r_i$中的数即可. (不知道这个能不能作为结论用. )

> $$\sum_{x \mathrm{mod} m} \left\{\frac{a x + b}{m}\right\} = \frac{1}{2}(m-1), (a, m) = 1$$    
> $$\sum_{x \mathrm{mod} m, (x, m) = 1} \left\{\frac{a x}{m}\right\} = \frac{1}{2} \phi(m), (a, m) = 1$$

其实用到的结论就是$(a, m) = 1, a \mathbb{Z}/m = \mathbb{Z}/m$, 以及对于完全剩余系来说, 加一个便宜量不会变, 于是就变成对完全(既约)剩余系的求和. 

> $m^{\phi(n)} + n^{\phi(m)} \equiv 1 (\mathrm{mod} m n), (m, n) = 1$

用到的结论就是$n^{\phi(m)} \equiv 1 (\mod m), m^{\phi(n)} \equiv 1 (mod n)$

> 整系数多项式$(f(x))^p \equiv f(x^p) (\mathrm{mod} p)$

利用类似于二项式展开的方式去做即可. 

> 素数$p > 2$, $a > 1$    
> 1. $a^p - 1$的素因数$q$必是$a - 1$的因数, 或者是$q \equiv 1 (\mathrm{mod} 2p)$
> 2. $a^p + 1$的素因数$q$必是$a + 1$的因数, 或者是$q \equiv 1 (\mathrm{mod} 2p)$
> 3. $2^{k p} + 1$的素数无穷多

$(a \pm 1)(a^{p - 1} + \cdots)$这样分解.

> $b > 1, n \geq 1, n \mid \phi(b^n - 1)$

#### 同余方程
> $(a, m) = 1, g(y) = f(a y+ b)$

## 总结
我, 就这样吧. 