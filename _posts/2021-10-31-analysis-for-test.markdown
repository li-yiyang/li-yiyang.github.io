---
layout: post
title:  "微积分的期中准备"
date:   2021-10-31 16:42:19 +0800 
categories: notes
math: true
---
# 微积分的临时抱佛脚
虽然说起来很不光彩, 但是这样的复习感觉有违我最低能量点生活的宗旨. 
(不如说是与最小作用量生活的原则相悖吧. )

但是这毕竟是第一次, 还是准备一下. 

## 集合和映射
集合之间通过建立映射关系来描述集合性质(关系).  
如集合的**等势关系** 

$$ X \sim Y $$

实际就是说这两的集合之间存在着双射, 等势关系就是一个等价关系. 

举个例子: 
可数集: 就是和自然数集$\mathbb{N}$或自然数集的有限子集等势的集合. 

(但是可数**不代表**可以从大到小地排序. )

可以有下面的结论: **_任意有限多个可数集的笛卡尔积是可数集_** . 

证明的思路就是构造一个表格来数(毕竟是可数集嘛). 

(这样的方法也可以用来说明有理数可数, 方法是构造
$S=\{0, \frac{1}{2}, \frac{1}{3}, \frac{2}{3}, \frac{1}{4}, \cdots \}$ 
这样的集合和$\mathbb{N}=\{0,1,2,\cdots\}$进行笛卡尔积, 
笛卡尔积完了以后再做一个相加的映射就是了. 
$(a,b) \in S \times \mathbb{N} \rightarrow a + b \in \mathbb{Q}$)

## 实数理论
### 实数不可数
假如实数可数(取其部分$[0,1)$), 
那么就可以构造一个数表, 第$i$行$j$列的数就是$x_i$的第$j$位小数, 
那么只要再取这样的数, 满足它的第$k$位小数和第$k$行$k$列的数不同, 
并且也不是$0$或$9$, 就可以说明这个数不在这个数表里, 但是又是实数, 
所以矛盾. 

### 实数的公理化定义
1. 加法公理: 定义了加法
2. 乘法公理: 定义了乘法
3. 序公理: 定义了实数之间的大小关系, 说明了实数是一个全序集. 
4. 完备性公理: 保证了实数理论是包含所有实数的理论

### 实数中的有理数
_**阿基米德原理**_

$$
  \forall h \in \mathbb{R}^{*}, 
  \exists k \in \mathbb{N}, 
  \forall x \in \mathbb{R}, 
  h k \leq x < h (k + 1)
$$

用人话说就是不管数轴的单位长度是多少, 数轴上的点一定落在格子里. 

_**有理数在实数中稠**_

$$
  \forall x, y \in \mathbb{R}, x < y
  \exists c \in \mathbb{Q}, x < c < y
$$

就是虽然有理数分布不连续, 但是它很多, 足够填满数轴? 
(证明方法就用阿基米德原理构造一个有理数, 这样的有理数是由
$N: y - x > \frac{1}{N}$作为分母, 存在性由
$N - 1 \leq \frac{1}{y - x} < N$给出
$k$就是$(k - 1) \frac{1}{N} \leq x < k \frac{1}{N}$, 
于是就有$c$了. )

### 实数域上的基本定理
_**完备性公理**_

$$
  \forall X, Y \subset \mathbb{R}, 
  \forall x \in X, \forall y \in Y, x \leq y
  \exists c \in \mathbb{R}, x \leq c \leq y
$$

就想成"两面包夹黄油", 举个例子, 假如要夹出一个无理数$\sqrt{2}$
只需要构造集合$X = \{x^2 \leq 2\}$, $Y = \{y^2 \geq 2\}$,
然后就可以夹出这个集合了. 

_**确界原理**_

_实数集的任何唯恐有上(下)界的子集必有上(下)确界_

用完备性夹出来的确界. 

_**闭区间套定理**_

$$
  I_n = [a_n, b_n] \neq \varnothing, 
  I_{i+1} \subset I_i \Rightarrow
  [\sup{a_n}, \inf{b_n}] = \cap_{n=1}^{\infty} I_n\\
  \mathrm{if} \quad \forall \varepsilon > 0, 
  |I_k \neq \varnothing| = b_k - a_k < \varepsilon
  \Rightarrow
  \exists! c = \sup{a_n} = \inf{b_n} 
  \in \cap_{n=1}^{\infty} I_n
$$

可以想成一个不断收缩的套索, 把要找的值给套住. 

_**有界数列必有收敛子列**_
可以用二分法的方式来理解, 就像是用对分法来查找一个数列的聚点. 
(即每次都选择有无穷多项的那一边)

_**柯西列收敛**_
可以用到数项级数里, 但是要注意柯西列的定义. 

$$
  \forall \varepsilon > 0, \exists N(\varepsilon)
  \forall n, m > N, |a_n - a_m| < \varepsilon
$$

不要忘了$n$, $m$是任意的而不是相邻的. 

_**有限覆盖定理**_

$$
  [a, b] \subset \cup_{\sigma \in 开区间族\Sigma} \sigma
  \Rightarrow 
  \exists \Sigma_n = \{\sigma_1, \cdots, \sigma_n\}
    \subset \Sigma, 
  [a, b] \subset \cup^{n}_{i=1} \sigma_i
$$

把无限转为有限. 

## 数列极限
数列极限的想法就是去掉有限项, 然后在剩下的无限项中都有某些好性质. 
大概这就是数学分析吧. 或者说这就是数学? (因为前面的可以暴力穷举. )

### 数列极限定义的两种说法
#### 几何味 邻域外只有可数多项

$$
  数列 \{a_n\} 收敛 
  \Leftrightarrow 
  \forall \varepsilon,
  \{a_n\} \cap \overline{U_{\varepsilon}(a)}
  只有有限项
$$

#### 代数味 $\varepsilon - N$语言

$$
  \forall \varepsilon > 0, 
  \exists N, \forall n > N, 
  |a_n - A| < \varepsilon
$$

(用$\varepsilon - N$语言证明极限的时候,  
可以直接就把$N$代入到$|a_n - A| < \varepsilon$中, 
解出范围, 这个过程中可以考虑考虑**放缩**. )

(还有的技巧就是**利用三角不等式放缩**, 就是先构造两个极限
$|a_n - A| < \varepsilon / 2$再和在一起证明)

### 证明极限
#### 迫敛定理

$$
  a_n \leq b_n \leq c_n, 
  a_n \rightarrow B,
  c_n \rightarrow B,
  \Rightarrow b_n \rightarrow B
$$

#### 单调有界必收敛
利用数列的有界性得到收敛性, 再来就可以利用子列的收敛相等得到极限,
或是利用递推公式来计算极限. (有界性可以通过数归来得到. )

#### 上下极限相等的数列收敛
很像迫敛定理, 只不过上下极限相等的收敛性是这个数列主动的结果(笑).

#### 所有子列极限相等的数列收敛
证明极限的时候就可以通过把原来不好证明的数列变成几个好证明的, 
比如分奇数和偶数的单调数列. 

### 极限性质
* 唯一性 
  (假如有一个数列的两个子列收敛性不一致, 
  那么就说明这个数列不收敛, 这样的收敛点就是**聚点**)
* 有界性
  (假如不有界, 就可以构造一个发散子列, 和收敛矛盾. )
* 保号性
* 四则运算 
  (不要忘了四则运算的前提是极限存在, 这个常常会忘了证明导致错误)

### Stokz定理
适用于$\frac{*}{\infty}$型和$\frac{0}{0}$型, 
(注意这里的"型"不可以略, 不然会被老师diss)

(实际上就是离散形式的洛必达法则)

## 数项级数
### 性质
1. 线性
2. 去掉有限项仍然收敛
   (往往就用这个做法来吧前面的有限项看作是常数, 
   然后在趋向无穷的项面前可以被扔掉. )
3. 收敛的必要条件是$\lim_{n \rightarrow \infty} a_n = 0$
4. 结合律 (任意加括号也收敛, 往往用于反证法. )
5. **正项级数**交换律 (可以交换顺序)

### 正项级数收敛的判别法
* 比较判别法 类似于迫敛定理
* 比式判别法 就是通过计算极限
  $\lim_{n \rightarrow \infty} \frac{u_{n+1}}{u_n}$, 
  假如比$1$小就是收敛, 反之发散
  (但是等于$1$要额外考虑, 实际上就是类等比数列). 
* 根式判别法 就是通过计算极限
  $\lim_{n \rightarrow \infty} \sqrt[n]{u_n}$, 
  假如比$1$小就是收敛, 反之发散
  (但是等于$1$也要额外考虑, 实际上也是类等比数列).

(根式判别法比比式判别法使用范围要广)

### 一般级数收敛
_**Abel 变换**_

$$
  \sum^n u_k v_k = 
  \sigma_n v_n - \sum^{n-1} \sigma_k (v_{k+1} - v_k)
$$

类似于分部积分

_**Abel 引理**_

$$
  \mathrm{if} \quad \{v_1, \cdots, v_n\} 单调, \\
  |\sum u_k v_k| \leq 3 \max |\sigma_k| \max |v_k|
$$

_**Abel 判别法**_

_**Dirichlet 判别法**_

_**条件收敛 绝对收敛**_

### 级数乘法
_**柯西定理**_

## 函数极限

## 习题回顾
1. $r = \sum^{\infty}_{i=0} p_i q^i, 
    p_n = 0, n > N 或 p_n 循环
    \Leftrightarrow r \in \mathbb{Q}$  
  即*有理数一定可以写成分数形式, 反之则不是有理数*.  
  (这个的理解用到了无限循环小数变分数的方法, 实际上就是
  把循环节移出小数点后再用做差的方式得到整数倍数, 
  $(10^T - 1)r = [10^T r]$; 反过来, 
  假如是分数, 那么分数变成小数的话, 小数的位置上最多只有$0 \sim q-1$
  这样的取值, 并且后面的位数由前面的位数决定, 所以一定会发生循环, 
  即对于被一位数除的情况是后一位由前一位(前几位)唯一确定)

2. 证明等式:
    1. $a = b \Leftrightarrow \forall \varepsilon > 0, 
    |a - b| < \varepsilon$
    2. $a = b \Leftrightarrow a \leq b, b \leq a$

3. 上下界的不等式的证明思路: 
   就是通过定义来留下一个可以动的变量, 对这个变量自然上下界都任意满足:

   $$
      \inf f + \inf g < f + g
      \Rightarrow
      \inf f + \inf g < \inf \{f + g\}
   $$

4. $\varepsilon - N$ 证明
   只需要构造$N$即可
5. 分段估计: 
  * $$\lim_{n \rightarrow \infty} a_n = a 
     \Rightarrow \lim \frac{\sum i (a_i -a)}{\sum i} = a$$  
    实际上只要证明$a=0$的情况, 然后就可以把分子的求和式化成两部分:  
    $$|\frac{a_1 + 2 a_2 + \cdots + N a_N}{\sum i}| + 
      |\frac{(1+N) a_{N+1} + \cdots}{\sum i}|
    $$
    于是前面的相当于$\frac{C}{\infty}$, 
    后面的相当于$\varepsilon$, 
    所以就可以做到合起来小于$\varepsilon$, 就是收敛. 
6. 极限运算: 
   * $$\lim a_n = \lim b_n = \lim c_n = \frac{a+b+c}{3}$$  
     可以得到:   
     $$
        \lim (a_n + b_n + c_n) = a+b+c\\
        \lim (b_n - c_n) = 0\\
        \lim (b_n - a_n) = 0
     $$  
     (注意只有存在的极限才可以加减, 不存在的是不可以直接加减. )
   * $$\lim (a_{2n} + 2 a_n) = 0, \{a_n\}有界$$  
     
7. 柯西收敛: 
   * $$
       \lim \frac{\sum a_i}{n} = a 
       \Rightarrow 
       \lim \frac{a_n}{n} = 0
     $$  
     因为收敛, 所以有$S_n = \frac{\sum a_i}{n}$满足:  
     $$
      |S_{n+1} - S_n| \rightarrow 0
      \Rightarrow 
      |\frac{a_{n+1}}{n+1}-\frac{S_n}{(n+1)n}| \rightarrow 0
     $$
   * 一般看不出收敛到那里的东西就用柯西收敛法则, 
     (或者利用上下极限收敛, 要么就用子列收敛):   
     $$n a_n \rightarrow A, S_n = \sum n(a_n - a_{n+1})
       \Rightarrow
       \sum a_n收敛$$  
     利用柯西列的形式:  
     $$|S_n - S_m| 
        = |n a_n - m a_m + \sum a_i| < \varepsilon $$
     所以就收敛
8. 利用一些重要的结论
   * $e = \lim (1+n)^n = \sum_{k=0}^{\infty} \frac{1}{k!}$

## 后记
### 一点对数学的想法
感觉数学就是一个在做抽象的过程, 就是遇到问题, 想到一个解决办法还不够, 
一定要给出一个更加普适的结论, 然后再抽象... 结果抽象完了, 没了直观, 
回来却连自己开始的问题用这个抽象的方法解释起来就又一点"麻烦". 

我说的这个麻烦是指理解上的麻烦, 可能写起来不会很麻烦. 

### 考试小记
(这只是我考完试的感觉, 万一试卷发下来之后可能会有很大的不一样. )

(前面肯定还有没有写完的, 这是因为临时抱佛脚的缘故. 原谅我. )

考试怎么说呢? 感觉和高中的考试很不一样, 感觉就是坐下, 然后就站起来了, 
没有什么真实的感觉. 好吧, 大概和我的写题方式有关. 我是一开始看到题目, 
有什么思路就先堆上去, 然后快速地下一道题, 结果写完了之后回头再看, 嗯, 
虽然我觉得应该是基本上对的, 但是感觉有一点不严谨, 然后就开始修修补补, 
修改了之后又觉得可以写得再简单一点(我被数学同化了?! 这就是抽象么? )
然后卷面就开始野蛮生长了. (希望老师能原谅我. )

总之, 希望有一个好成绩. 

### katex
现在转投了[katex](https://katex.org), 
发现这个渲染数学公式真的是又快又好. 
就是国内的文章里很少见到罢了, 等过了这个坎, 我再想办法, 
看能不能写一个katex踩坑指南. 

### 考完后的katex小教程
实际做法和`mathjax`一致, 
只需要在`_include`文件夹里新建一个`katex.html`
然后把官网上的脚本拷贝进去: 
{% highlight html %}
<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/katex@0.15.0/dist/katex.min.css" integrity="sha384-SfHjyzed8eCsTSa4t2GoMc4WnsCLa6cQpFqPRCaizz0FlQUOyafw/AyIUCQU/KuM" crossorigin="anonymous">
<script defer src="https://cdn.jsdelivr.net/npm/katex@0.15.0/dist/katex.min.js" integrity="sha384-JRVVAdBKoQa7uhd8heKqlQyzByQCC57fpvrCw9iSahjP5bLB5b+hX0klEdjZmsH6" crossorigin="anonymous"></script>
<script defer src="https://cdn.jsdelivr.net/npm/katex@0.15.0/dist/contrib/auto-render.min.js" integrity="sha384-+XBljXPPiv+OzfbB3cVmLHf4hdUFHlWNZN5spNQ7rmHTXpd7WvJum6fIACpNNfIR" crossorigin="anonymous"></script>
<script>
    document.addEventListener("DOMContentLoaded", function() {
        renderMathInElement(document.body, {
          // customised options
          // • auto-render specific keys, e.g.:
          delimiters: [
              {left: '$$', right: '$$', display: true},
              {left: '$', right: '$', display: false},
              {left: '\\(', right: '\\)', display: false},
              {left: '\\[', right: '\\]', display: true}
          ],
          // • rendering keys, e.g.:
          throwOnError : false
        });
    });
</script>
{% endhighlight %}
接下来把`head.html`给改了就好. (略)