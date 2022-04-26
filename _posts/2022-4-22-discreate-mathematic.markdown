---
layout: post
title:  "Discreate Mathematics"
date:   2022-04-25 13:02:15 +0800
math: true
categories: notes
---
# 离散数学
> * 数理逻辑
> * 数据的组织和处理 - 数据结构: 根据研究的对象的特点来进行研究: 
>   * 没有很强的相互关系: 集合论
>   * 有一些相互关系: 图论, 组合论
>   * 相互关系很强: 代数
> * 初等数论 - 密码学

选择的是网络安全的离散数学, 和计算机系的侧重点应该是最后的密码学部分相关的初等数论. 学的过程虽然有点平淡, 但是有些不起眼的地方还是有些很有趣的地方.

我觉得这门课(目前)最难的是一堆定义, 平时基本当作是本来就该这样的东西, 现在被非常严谨地重新定义了一遍. 感觉看世界的镜片被突然矫正了一遍... 学的时候没啥感觉, 但是要考试, 要把这一堆的定义记住还是有点麻烦的. 

## 形式逻辑
* 命题: 真值确定的表达式
  * 原子命题: 用小写英文字母$p$, $q$, $r$来表示简单命题, 其值为$1$, $0$. 
  * 复合命题: 通过联结词将符合原子命题组成一个复合命题. 
    1. 括号
    2. 否定: $\urcorner q$
    3. 合取: $q \wedge p$, 析取: $q \vee p$    
       名字的记法就是: "合"字尖尖向上, 所以是$\wedge$. 
    4. 蕴含: $q \rightarrow p$
    5. 等价: $q \leftrightarrow p$
    6. 与非: $p \uparrow q \Leftrightarrow \urcorner (p \wedge q)$    
       或非: $p \downarrow q \Leftrightarrow \urcorner (p \vee q)$
  * 联结词的完备集: $S = \{\urcorner, \wedge, \vee\}$和$S = \{\urcorner, \vee\}$, 或者$S = \{\uparrow\}$, $S = \{\downarrow\}$
* 合式公式$A(p_1 p_2 \cdots p_n)$, 命题变项$p_1 p_2 \cdots p_n$, 赋值    
  直观感觉就是有一种函数映射的感觉, 在这样的映射下, 于是就有矛盾式($Im A = \{0\}$), 也有重言式($Im A = \{1\}$). 
* 等值演算: 假如两个合式公式在任何赋值下的值都相等, 那么$A \Leftrightarrow B$
  * **双重否定**为肯定: $\urcorner \urcorner A \Leftrightarrow A$
  * **幂等律**: $A \vee A \Leftrightarrow A, A \wedge A \Leftrightarrow A$    
    直观感觉就是同一件事扩大外延等于没有扩大, 同一件事强调内涵等于没有强调
  * **交换律**: $A \wedge B \Leftrightarrow B \wedge A, A \vee B \Leftrightarrow B \vee A$
  * **分配律和结合律**: $(A \wedge B) \vee C \Leftrightarrow (A \vee C) \wedge (B \vee C), (A \vee B) \wedge C \Leftrightarrow (A \wedge C) \vee (B \wedge C)$    
    直观感觉就是这两个是相互对称的. 
  * **德摩根律**: $\urcorner (A \vee B) \Leftrightarrow \urcorner A \wedge \urcorner B, \urcorner (A \wedge B) \Leftrightarrow \urcorner A \vee \urcorner B$    
    直观感受就是一个翻转的感觉, 连作用符号也可以进行翻转. 
  * **吸收律**: $A \wedge (A \vee B) \Leftrightarrow A, A \vee (A \wedge B) \Leftrightarrow A$
  * **零律**: $A \wedge 0 \Leftrightarrow 0, A \vee 1 \Leftrightarrow 1$     
    直观感受就是管你是什么, 直接拍成$0$或$1$
  * **同一律**: $A \vee 0 \Leftrightarrow A, A \wedge 1 \Leftrightarrow A$    
    直观感受就是不管怎么搞还是不变
  * **排中律**: $A \vee \urcorner A \Leftrightarrow 1$    
    直观感受就是历遍所有情况. 
  * **矛盾律**: $A \wedge A \Leftrightarrow 0$    
    直观感受就是排除所有情况. 其实用德摩根定律应用于排中律即可. 
  * **蕴含等值式**: $p \rightarrow q \Leftrightarrow \urcorner p \vee q$    
    这样就可以做到公式化简了. 
  * **等价等值式**: $p \leftrightarrow q \Leftrightarrow (p \rightarrow q) \wedge (q \rightarrow p)$
  * **假言易位**: $p \rightarrow q \Leftrightarrow \urcorner q \rightarrow \urcorner p$    
    否命题和逆否命题等价. 
  * **等价否定等值式**: $p \leftrightarrow q \Leftrightarrow \urcorner q \leftrightarrow \urcorner p$
  * **归谬论**: $(A \rightarrow B) \wedge (A \rightarrow \urcorner B) \Leftrightarrow \urcorner A$    
    一个东西, 既能推出真, 也能推出假, 哪里都适用, 那么肯定就不是个好东西. 万能膏药或者包治百病的东西没一个好家伙. 
* 主析取范式和主合取范式    
  公式范式就是用析取与合取来表示等值公式, 但是可以有很多的形式, 所以定义标准型为主析取, 主合取范式. 所有简单合取式都存在主合取范式和主析取范式, 并且还是唯一的. 
  * 极大项和极小项:     
    极大项是成假赋值, 如$p \vee q = M_0$, 极小项是成真赋值, 如$p \wedge q = m_3$, 这种东西的命名方式就是把输入按照顺序排成的二进制来转换成一个数作为下标.     
    如何记忆: 极小项, "小"字向下, 所以$\wedge$, 也就是$m = p \wedge q$之类的. 然后极大项, "大"要扩大, 所以是析取$\vee$. 
  * 主析取式: $m_{i_1} \vee m_{i_2} \vee \cdots \vee m_{i_k}$    
    主合取式: $M_{i_1} \wedge M_{i_2} \wedge \cdots \wedge M_{i_k}$    
    简单的理解就是, 假如有真值表:      
   
    | $p$ | $q$ | $A$ |
    |-----|-----|-----|
    | $0$ | $0$ | $1$ |
    | $0$ | $1$ | $0$ |
    | $1$ | $0$ | $1$ |
    | $1$ | $1$ | $0$ |

    那么就可以写出范式: $A = M_0 \wedge M_2 = m_1 \vee m_3$, 方法: 假如是找主合取式, 就找出成真的输入, 然后搞出对应的极大值. 
* 逻辑推理
  * 形式表述: 对前提$\{A_1, \cdots, A_k\}$, 对结论$B$, 那么逻辑推理即形式化记作: $\{A_1, \cdots, A_k\} \vdash B \Leftrightarrow A_1 \wedge \cdots \wedge A_k \rightarrow B$, 当右侧为重言式的时候, 逻辑推理成立, 记作$\{A_1, \cdots, A_k\} \Vdash B$
  * 推理定律
    * **附加律**: $A \Rightarrow (A \vee B)$    
      扩大外延
    * **化简律**: $(A \wedge B) \Rightarrow A$    
      缩小内涵
    * **假言推理**: $(A \rightarrow B) \wedge A \Rightarrow B$    
      理解就是前提推出结论. 
    * **拒取式**: $(A \rightarrow B) \wedge \urcorner B \Rightarrow A$    
      假言推理的逆否命题的感觉. 
    * **析取三段论**: $(A \vee B) \wedge (B \rightarrow C) \Rightarrow (A \rightarrow C)$    
      类似于一个排除法, 两者合起来是真的, 一个是假的, 那么只能剩下一个是真的了. 
    * **等价三段论**: $(A \leftrightarrow B) \wedge (B \leftrightarrow C) \Rightarrow (A \leftrightarrow C)$    
      就是把等价关系拆成两个来证明. 
    * **构造性二难**: $(A \rightarrow B) \wedge (C \rightarrow D) \wedge (A \vee C) \Rightarrow (B \vee D)$    
      条件成立, 那么结论应该也要成立的感觉.     
      特殊的形式是$(A \rightarrow B) \wedge (\urcorner A \rightarrow B) \Rightarrow B$
    * **破坏性二难**: $(A \rightarrow B) \wedge (C \rightarrow D) \wedge (\urcorner B \wedge \urcorner D) \Rightarrow (\urcorner A \vee \urcorner C)$    
      感觉就像是结论都不成立, 那么条件应该是不成立的. 
    * **等值式可以产生两个推理定律**: $A \Leftarrow B, A \Rightarrow B, B \Rightarrow A$
* 自然推理系统: 
  * 形式系统的组成: 
    1. 非空字母表$A(I)$
    2. 符号合式公式集$E(I) \subset A(I)$
    3. 公理集$A_X(I) \subset E(I)$
    4. 推理规则集$R(I)$    
  于是$I = \langle A(I), E(I), A_X(I), R(I) \rangle$组成过了形式系统. 其中$\langle A(I), E(I)\rangle$为形式语言系统, $\langle A_X(I), R(I) \rangle$为形式演算系统. 
  * 于是自然推理系统$P$如下定义: 
    1. $A(I)$由命题变项符号, 联结词符号, 括号和逗号组成
    2. 合式公式
    3. 推理规则: 前提引入(可以使用前提), 结论引入(结论被证明后可以时候), 置换规则(可以调换)
  * 证明方法: 附加前提证明法, 归谬法
* 一阶逻辑
  * 命题
  * 个体词: $a, b, c$个体常项, $x, y, z$个体变项    
    个体域: 个体词的取值范围, 类似于定义域一样的东西
  * 谓词: 用来表示个体词和个体词之间相互关系的东西. (类似于一个函数的样子)     
    谓词常项: 表示具体性质或关系的谓词     
    谓词变项: 表示抽象或泛指的性质或关系的谓词     
    多元谓词: 有多个变元, 类似于多元函数
  * 量词: 全称量词$\forall$, 存在量词$\exist$
  * 一阶语言:     
    非逻辑符号集合$L$生成的一阶语言$\mathcal{L}$的字母表: 
    * 符号表:
      * 非逻辑符号: 个体常项符号, 函数符号, 谓词符号
      * 逻辑符号: 个体变项符号, 量词符号, 联结词符号, 括号和逗号
    * 项: 由个体常项和个体变项和函数的有限次组合是项.    
      $a, x, x + y, f(x), g(x, y)$
    * 原子公式: $R(x_1, \cdots, x_n)$
    * 合式公式: 原子公式之间的有限次逻辑运算和应用量词组成的东西就是原子公式. 
    * 指导变元, 辖域: 指导变元在辖域中约束出现, 否则是自由出现. 自由出现的变元可以随意变换名字. 
  * 公式的解释:     
    直观的理解就是将符号$L$和意义$D_I$对应起来. 
    1. 非空个体域$D_I$
    2. $\forall a \in L, \exist \overline{a} \in D_I$为$a$在$I$中的解释. 
    3. $\forall f \in L, \exist \overline{f} \in D_I, \overline{f}: D_I^n \rightarrow D_I$称函数符号的解释. 
    4. $\forall F \in L, \exist \overline{F} \in D_I$    
  * 等值演算
    * 逻辑命题的代换规则
    * 量词消去
    * 量词否定    
    $\urcorner \forall x A(x) \Leftrightarrow \exist x \urcorner A(x)$     
    $\urcorner \exist x A(x) \Leftrightarrow \forall x \urcorner A(x)$
    * 量词辖域收缩和扩张    
      $\forall x (A(x) \rightarrow B) \Leftrightarrow \exist x A(x) \rightarrow B$    
      $\exist x (A(x) \rightarrow B) \Leftrightarrow \forall x A(x) \rightarrow B$
      会变符号的就只有这两个, 其他的都不会变化. 
    * 置换规则: $A \Leftrightarrow B, \varPhi(A) \Leftrightarrow \varPhi(B)$
    * 换名规则: 自由变量名字随便换
    * 代替规则
  * 前束范式: 就是把所有的变量全部提到前面. 

## 集合论
> 讨论的是朴素集合论, 非公理化集合论.    
> 集合是最基础的数学结构.    
> 研究集合关系的时候是利用逻辑推导来机械地定义集合关系的. 

* 表示方法: 列举元素$\{a, b, c\}$或者谓词表示方法$\{x \vert x \in \mathbb{R} \wedge x^2 - 1 = 0\}$
* 元素关系: 无序, 不重复
* 集合关系: 子集, 真子集, 空集, 幂集$P(A) = \{x \vert x \subseteq A\}$, 全集
* 集合运算: 并, 交, 相对补(差集)$A - B = \{x \vert x \in A \wedge x \notin B\}$, 绝对补$E - A = \sim A$, 对称差$A \oplus B = (A \cup B) - (A \cap B)$, 广义并和广义交(对集合的集合$A$的所有元素都做一个并或交)
* 文氏图
* 容斥定理: $\vert A \cup B \vert = \vert A \vert + \vert B \vert - \vert A \cap B \vert$
* 有序对: 两个元素按照顺序排列组成的二元组$\langle x, y \rangle$
* 笛卡尔积: $A \times B = \{\langle x, y \rangle \vert x \in A, y \in B\}$    
  没有交换律, 结合律, 对并和交满足分配律, $A \times \varnothing = \varnothing \times B = \varnothing$, $A \subseteq C \wedge B \subseteq D \Rightarrow A \times B \subseteq C \times D$, $\vert A \vert = n, \vert B \vert = m, \vert A \times B \vert = n m$
* 二元关系
  * 定义, 定义域$\mathrm{dom} R$, 值域$\mathrm{ran} R$, 域$\mathrm{fld} R = \mathrm{dom} R \cup \mathrm{ran} R$, 逆运算, 合成运算
  * 表示: 关系矩阵$R = (r_{i j}), r_{i j} = x_i R x_j ? 1 : 0$, 关系图
  * 关系的限制
  * 关系的性质: 
    * 自反$\forall x (x \in A \rightarrow \langle x, x \rangle \in R) \Leftrightarrow I_A \subset R$    
      反自反$\forall x (x \in A \rightarrow \langle x, x \rangle \notin R) \Leftrightarrow R \cap I_A = \varnothing$
    * 对称$\forall x \forall y (\langle x, y \rangle \rightarrow \langle y, x \rangle) \Leftrightarrow R^{-1} = R$    
      反对称$\forall x \forall y (\langle x, y \rangle \wedge \langle y, x \rangle \rightarrow x = y) \Leftrightarrow R^{-1} \cap R \subseteq I_A$
    * 传递性$\forall x \forall y (\langle x, y \rangle \wedge \langle y, z \rangle \rightarrow \langle x, z \rangle) \Leftrightarrow R \circ R \subseteq R$
  * 闭包    
    满足某种性质并且包含原本集合的最小关系.    
    $r(R) = R^0 \cup R$自反闭包    
    $s(R) = R \cup R^{-1}$对称闭包    
    $t(R) = R \cup R^2 \cup \cdots$传递闭包, 使用Warshall算法可以计算. (虽然规模小的时候还是暴力计算更快)
* 等价关系
  * 定义: 自反, 传递, 对称    
    感觉和线性代数里面讲得差不多. 
  * 等价类: $[x]_R = \{y \vert y \in A \wedge x R y\}$    
    代表元    
  * 商集: 等价类的集合$A / R$    
    商集对应划分
  * 陪集: 代表元的集合
* 偏序关系$\preceq$
  * 定义: 自反, 反对称, 传递
  * 偏序集: $\langle A, \preceq \rangle$
  * 哈斯图
  * 最小元$y \in As.t. \forall x (x \in B \rightarrow y \preceq x)$    
    最大元$y \in A s.t. \forall x (x \in B \rightarrow x \preceq y)$    
    极大元$y \in A s.t. \forall x (x \in B \wedge y \preceq x \rightarrow x = y)$     
    极小元$y \in A s.t. \forall x (x \in B \wedge x \preceq y \rightarrow x = y)$

## 例题
> 判断一个东西是不是命题

一定是一句陈述句, 并且判断的结果一定是唯一的. (**感叹句**, **祈使句**, **疑问句**绝对都不是命题, 陈述句中的**悖论**, 判断结果**不唯一确定**的也不是命题. )

> 将陈述句转换为逻辑命题

我觉得最难的是蕴含和博大精深的汉语言. 
* $P \rightarrow Q$
  * 只要$P$就$Q$
  * 如果$P$则$Q$
  * 只有$P$才$Q$
  * 除非$P$才$Q$
  * 因为$P$所以$Q$
* $\urcorner P \rightarrow Q$
  * 除非$P$否则$Q$
  * $P$否则$Q$
  * $P$除非$Q$

> 计算合式公式的真值以及判断类型

能化简的先化简. 判断公式类型的时候, 可以用真值表来做. (但是如果偷懒的话, 可以化简成主析取范式或者主合取范式. 或者也可以反解. 反解的方式对成真/假赋值来说还算是比较简单的做法. )

真值表还能够用来证明两个表达式是等值的. 

> 如何求主析取范式和主合取范式

主析取范式: $\wedge M_i$, 主合取范式$\vee m_i$. 主析取范式是极大项, , 所以是大写, 成假. 主合取范式是极小项, 所以是小写, 成真. 简单的想法就是, 析取范式成真只要有一个成极大项成真即可; 而合取范式只要有一个成假就会假. 

我觉得对于非常小的变元数量的表达式, 还是直接用真值表来做比较方便. 其实原理就是如何通过成真赋值和成假赋值来计算主析取范式和主合取范式. 
* 已知成真赋值$000, 011, 110$, 然后就能够知道极小项$m_0, m_3, m_6$, 于是就能够写出主合取范式: $m_0 \vee m_3 \vee m_6$
* 已知成假赋值$010, 011$, 于是就能够知道$M_2, M_3$, 所以主析取范式就是$M_2 \wedge M_3$

或者通过化简的方法来计算. 假如没有某一个变元, 就通过$\cdots \vee (P \wedge \urcorner Q)$或者$\cdots \wedge (P \vee \urcorner P)$来变出来. 

主析取范式和主合取范式之间的相互转换利用德摩根定律就可以直接转换了. 方法就是把大写换成小写, 或者小写换成大写, 然后转化符号即可. 

> 构造符号化证明过程
> * $p \Rightarrow p \vee q$    
>   $p$: $2$是偶数, $q$: $3$是奇数, 于是附加律就是"若$2$是偶数, 则$2$是偶数或$3$是奇数"
> * $p \wedge q \Rightarrow p$    
>   因为$p$和$q$, 所以$p$
> * ...

> 利用逻辑来证明    
> 例: $\urcorner (p \rightarrow q) \wedge q, p \vee q, r \rightarrow s$为前提, 证明结论$r$    
> $\urcorner (p \rightarrow q) \wedge q \Leftrightarrow p \wedge \urcorner q \wedge q \Leftrightarrow 0 \Leftrightarrow 0 \wedge r \Rightarrow r$

> 构造自然推理系统来证明推理    
> 就是要说明构造符号集, 然后公式集. 如此而已. 

> 如何用一阶逻辑来将命题符号化

首先要确定个体域, 然后写出表达式, 最后写出里面的谓词表示的意义. 

> 一阶逻辑里面要搞清楚个体词的作用范围, 这样就不会化简错误了. 

> 解释公式

给定解释和赋值, 直接计算逻辑就好了. 

> 求一阶逻辑的前束范式

$\forall x (A(x) \rightarrow B) \Leftrightarrow \exist x A(x) \rightarrow B$    
$\exist x (A(x) \rightarrow B) \Leftrightarrow \forall x A(x) \rightarrow B$

用这样的方法就能够化简了. 

> 求关系矩阵, 求关系图, 求关系的闭包

> 求哈斯图

## 后记
算了, 就这样吧. 概念还是记不清楚. 