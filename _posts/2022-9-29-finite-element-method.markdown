---
layout: post
title:  "[Learning Note] COMSOL"
date: 2022-10-14 11:36:36 +0800
math: true
categories: learning
---
<!-- this is a style bar, i will later consider adding the style to all the page, 
but not now. later. -->
<style>iframe {border: none; width: 100%; height: 50vh;}</style>

# COMSOL and FEM
尽管之前在 [Dot, Dot, Dot and ...]({{ site.github.url }}/physics/dot-dot-dot-and/)里面也有一点点简单的介绍. 
但是毕竟还是一个类似于科普的简单东西, 具体的细节也不是很清楚, 
没有太多的实际使用价值. 

最近又要用到, 所以开始学一点点. 参考的是 COMSOL for Engineer
中的第 2 章. (书好不好我不知道, 随便选的... 嗯, 看了之后觉得这个书挺好的, 
讲得比较实用. 几乎只有操作, 并且操作写得非常详细. 巨无比详细. 
强力推荐. )

因为干活需要, 所以主要还是 COMSOL 的学习, FEM 等以后再专业地学. 
关于软件, 这个我不知道是哪里来的. 莫名其妙就出现了, 
所以别问我怎么搞. 大概是学生版吧. 

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [COMSOL and FEM](#comsol-and-fem)
    - [Basic Concepts](#basic-concepts)
        - [Matrix Approach](#matrix-approach)
        - [Shape Functions](#shape-functions)
    - [COMSOL](#comsol)
        - [Create A Project](#create-a-project)
            - [Space Dimension](#space-dimension)
            - [Physics](#physics)
            - [Study Type](#study-type)
        - [Basic of A Project](#basic-of-a-project)
            - [Model Builder](#model-builder)
            - [Geometry](#geometry)
                - [Geometry Modeling in Comsol](#geometry-modeling-in-comsol)
                - [Modeling via CAD](#modeling-via-cad)
            - [Materials](#materials)
            - [Physics](#physics-1)
                - [Magnetic Field](#magnetic-field)
                - [Plate](#plate)
            - [Meshs](#meshs)
            - [Study](#study)
            - [Results](#results)
            - [Graphics](#graphics)
    - [COMSOL Examples](#comsol-examples)
        - [Static and Dynamic Analysis of Structures](#static-and-dynamic-analysis-of-structures)
            - [Thin Plate](#thin-plate)
            - [Dynamic Plate](#dynamic-plate)
        - [Internal Flows](#internal-flows)
        - [Unsteady Heat Transfer](#unsteady-heat-transfer)
        - [Electrical Circuits](#electrical-circuits)
        - [Multiphysics](#multiphysics)
        - [Others](#others)
            - [Solenoid](#solenoid)

<!-- markdown-toc end -->

## Basic Concepts
一个麻烦的物理问题, 往往往往只需要简单的方程 (PDE/ODE) 就能描述. 
当然, 除了一些特别的问题以外, 往往都是一些没有解析解的答案. 
并且求解也往往极其 "机械而繁琐". 

(虽然不一定全部都是这样, 比如对于一些问题, 比如平面电势场, 
在费曼讲义里面就有一种特别的用弹性膜去近似模拟的方法. )

众所周知, 想要解一个微分方程, 需要有边界条件 (boundary conditions). 
并且在知道了边界条件的时候, 相当于也规定了一个求解的区域 (geometry), 
即想要了解这个区域内的物理量的信息. 

FEM 计算的过程先是将 geometry 划分为许多的小块 (meshing). 
取其中的顶点 (node) 标记物理信息. 在划分完了之后, 
就会对这个顶点集进行一个计算操作. 具体有如下两种过程: 

1. Minimization of a functional using variational principle approach  
   常见的 Euler-Lagrange 方程和最小作用量原理对应, 后者是一个积分: 
   $S = \int_a^b L \mathrm{d}t$, 一般对这个积分进行一个求最小值的努力. 
2. Weighted residual approach  
   一些没法列出作用量积分的情况, 或者说这样的积分计算太麻烦了, 
   比如 Navier-Stokes 方程, 流动问题, 热传导方程和非线性的偏微分方程. 

(也就是一个是积分, 一个是微分观点咯? 在积分观点下, 就是要求出最小作用量,
在微分观点下, 就是要得到局域的 residue 的最小 -- 这个我的理解是这样的: 
考虑 $\boldsymbol{F} = -k \boldsymbol{x}$ 的问题, 可以迭代计算
得到 $\boldsymbol{x}$ 的收敛的值, 使得每次迭代的 
$\boldsymbol{F} + k\boldsymbol{x}_i \rightarrow 0$. )

### Matrix Approach
![example-2-1]({{ site.github.url }}/_img/simulate-physics/fem-2-3.png "尽管这个东西用简单的牛顿力学应该算是容易算的... 但是还就是那个你已经学会 1+1 了, 现在来计算一下... 的问题. ")

将每个点都标号 (assign node numbers), 于是两根杆之间的力: 

$$\overbrace{\frac{A E}{L}\left(\begin{array}{llll}c^2 & c s & -c^2 & -c s\\ c s & s^2 & - c s & - s^2\\ - c^2 & - c s & c^2 & c s\\ - c s & - s^2 & c s & s^2\end{array}\right)}^{k_y} \left\{\begin{array}{l} u_i \\ v_i \\ u_j \\ v_j\end{array}\right\} = \left\{\begin{array}{l} F_{ix} \\ F_{iy} \\ F_{jx} \\ F_{jy}\end{array}\right\}$$

其中, $u,v$ 代表的是相对位移量. 而 $c = \cos \alpha = \frac{x_j - x_i}{L}, s = \sin \alpha = \frac{y_j - y_i}{L}, L = \sqrt{(x_j - x_i)^2 + (y_j - y_i)^2}$. 

上面的式子得到的类似于 $k_y u = F$. 可以发现, 每个方程中的未知数 $u$
不是每个都不同的, 而是有重复的, 所以通过适当的组合就能够得到一个
用来描述整个系统中所有质点的矩阵方程. 

![fig-2-6]({{ site.github.url }}/_img/simulate-physics/fem-2-6.png "这个图片里面用到的好像是一个被叫做平移矩阵的方法. 不过具体还是不用管了. ")

### Shape Functions
其实感觉就像是理论力学中的约束方程. 用线性方程 (或者说线性约束) 的方式来描述. 
比如说有一个线性约束: (有那么点像贝塞尔曲线)

$$u = u_1 (1 - \frac{x}{L}) + u_2 \frac{x}{L}$$

于是就能够用矩阵的形式来描述运动. 

## COMSOL
嗯, 为了快速上手, 所以得快点上手软件. (为了和教材一致, 所以用的是英文版). 
用的版本应该是 COMSOL 6.0. (害, 书里面的版本是 4.3, 希望不会有太多差别. )

(注: 感觉还是英文版好用... 网上某些教程的中文版翻译实在是过于生草. )

确实没有太大的差别, 就启动界面稍微有点不同而已. 在做了一个例子之后, 
我觉得 COMSOL 的一个操作逻辑有点像是图形化的编程, 并且, 
还有点那种没有循环的一步步编程. 

### Create A Project
一般的操作就是使用带有引导的 Model Wizard. 

#### Space Dimension
然后就是指定要研究的物体的维数. 我发现里面有一个叫做 2D Axisymmetric, 
看起来估计可以用来简化计算的样子? (目前[2022-10]没试过). 

#### Physics
就是加入要用来考虑的物理场. 有点像是导入特定的软件包. 一般按需选择, 
貌似如果选了的话就要把参数条件都满足了, 否则最后运行的时候会报错. 
(好像很合理, 没有需求的话加什么包... )

#### Study Type
这个时候就发现了英文和中文版的语言差异了: 

* Stationary - 稳态, 嗯, 还算好理解. 用来求解稳定解. 
* Time Dependent - 瞬态, 这个就不太好理解了, 更加坑爹的是, 我的
  中文翻译和网上的还不一样. 这, 过度离谱了. 用来求解随时间演化的问题. 
* 其他的暂时没用过, 之后用过之后再说. 

### Basic of A Project
![comsol-overview]({{ site.github.url }}/_img/simulate-physics/comsol-overview.png "不过每个人的界面应该稍微有点不一样, 这个是我随便截的一个图. ")

嘛, 只看界面的话和一般的软件差不多, 一堆窗口内的小窗口, 还有一个菜单栏, 
编辑栏之类的东西. 就是不知道该怎么操作. 

主要的逻辑应该是从左到右看的: Model Builder - Settings - ... (Others), 
就是在 Model Builder 里面, 有点像是一个写代码的地方, 然后在 Settings 
里面, 有点像是给每一个代码都加上参数. 

#### Model Builder
在 Model Builder 里面, 用的应该是一种类似于树状, 线性的逻辑结构. 
具体的话有点像是 C 语言里面嵌套的花括号. 在花括号里面的逻辑是从上到下的. 

一个文件主要有四个部分 (目前来看是的): 

1. Global Definations: 可以用来定义全局变量 (Parameters), 
   以及材料 (Matrials). 关于全局变量的话, 我觉得还是写上单位比较妙,
   单位的写法类似下面这样: `1[mm]`, 因为后来发现只有数值可能会有点问题. 
2. Compotent: 主要的东西在这一层.   
   里面主要会有 Definations (目前不太清楚是啥), Geometry (几何关系),
   Materials (材料), 对应的物理的包 (比如 Megnetic 之类的), 最后是
   Mesh (网格, 见前面的 FEM 部分). 
3. Study: 仿真模拟的步骤设置, 里面会有一堆的 Step
4. Results: 结果, 貌似除了一个好的建模和模拟以外, 还需要很好的结果处理, 
   (貌似有一个高级的名词叫做 "后处理")

不过其实如果哪一个步骤 (在 COMSOL 里面叫做 Node, 节点) 有问题, 
在图标上面就会有显示一个小叉, 并且在运行的时候会停止并报错. 
然后就可以像是填表格一样把这些信息给填写清楚. 还算是比较轻松. 

#### Geometry
简单的几何建模可以在 COMSOL 里面完成, 但是, 它还能够直接从其他的 CAD
软件里面导入. (可以参考 [CAD Import Module](https://www.comsol.com/cad-import-module))

##### Geometry Modeling in Comsol
因为我觉得这里的建模的难度还挺麻烦的, 所以之后如果有复杂的建模我觉得还是用
CAD 软件来干这样的专业的活. 这里就记录一点点我觉得可能有用的东西: 

不知道是不是我的问题, 在建立完模型之后, 要点一下 Build All 才能看到之前建的模型. 
有点不像是所见即所得的感觉. 不过无所谓, 之后在 COMSOL 里面的建模思路大概就是这样: 

点击 Geometry 在 Settings 里面设置单位 (Length Unit). 

![comsol-geo-set]({{ site.github.url }}/_img/simulate-physics/comsol-geo-set.png "本来我懒得放图的, 但是, 想到自己之前因为没有图, 在网上看别人的文字描述一脸懵逼. 哎, 还是放图吧. ")

比如要建一个圆柱壳, 那么就先建立两个圆柱体 Cylinder (可以在 Model Builder 
栏右键点击 Geometry, 然后在弹出菜单里面选择; 或者直接在编辑的菜单栏里面选. ), 
建立了的圆柱在 Model Builder 栏选中后可以在 Settings 里面编辑信息. 

接着再使用逻辑布尔运算来得到两圆柱向减的圆柱壳: 

![comsol-geo-bool]({{ site.github.url }}/_img/simulate-physics/comsol-geo-bool.png "其实也可以在菜单栏上面找到对应图标的操作的, 并且就算你不认识图标, 将鼠标在上面悬停, 就像你看到这段文字一样, 应该就会弹出光标显示图标对应的操作名称了. ")

在 Settings 里面点选那个像开关一样的东西, 可以在 Graphics 里面选择对象, 
然后添加到选择框中. (或者可以自己输入). 但是其中有一个小小的坑爹的地方, 
在 COMSOL 中, Graphics 里面看到的东西是不能随意改视角的, (大概, 
如果真的可以, 当我在瞎说). 那么选择的时候就比较麻烦了, 
可以通过把鼠标放在图上, 滚动滚轮来切换选择的对象. 

![comsol-select]({{ site.github.url }}/_img/simulate-physics/comsol-select.png "虽然我觉得这个选择方式十分的难受, 并且也不是很直观. 不过也不是不能直接像写程序一样写名称, 就在那个像剪贴板一样的图标那里输入就好... 虽然更加别扭. ")

整成代码的话估计就像是: 

```scheme
(define outside-cylinder (make-cylinder ...))
(define inside-cylinder (make-cylinder ...))
(boolean-difference outside-cylinder inside-cylinder)
;; => 得到圆柱壳
```

##### Modeling via CAD
待尝试... 先空着

#### Materials
右键 Materials, 在弹出的菜单里面选择 Add Materials from Library, 就会打开面板, 
从中选择材料就能够添加到对应的物体对象上. 

![comsol-material-co]({{ site.github.url }}/_img/simulate-physics/comsol-material-co.png "比如这里就给中间的圆柱壳加了一个铜的材料, 作为线圈. (别问我为什么 Copper 是铜, 因为派生于拉丁文 cuprum. )")

当然, 材料还会有各种各样的属性, 一般需要查. (或者测量? )

#### Physics
对于物理场, 原则上来说应该需要对每个仿真模块都要了解一下, 
毕竟它们之间有着微妙的区别. 但是我比较急, 所以用到再学吧. 

这里就只是记录一下用到的: (以后可能会更新)

##### Magnetic Field
右键菜单 Coil 添加线圈, 线圈的 Conductor model 选择 Homogenized 
multiturn, 即对应多匝线圈. 为了让线圈绕铁芯, 还要制定线圈类型
coil type 为 circular, 并且在 coil geometry 中制定绕线方向. 

##### Plate
平面静力学模块. 通过 Fixed Constraint 来设置固定的边; 通过
Edge Load 来添加负载. 

#### Meshs
网格划分, 在前面的 FEM 部分就有介绍. 对于简单的模型, 貌似, 
系统自带的网格划分就挺好用的. 至于高级的操作, 之后再试. 

#### Study
使用 `F7` 可以运行特定的一步. 

#### Results
emmmm... 目前还不太会用. 

#### Graphics
应该主要的就是视图切换和显示方式了吧... 

## COMSOL Examples
其实还是用例子来试试就好了. 

### Static and Dynamic Analysis of Structures
#### Thin Plate
研究的是 2 维平面的静力学问题, 所以在建立的时候设立的条件为: 
**2D - Plate (Structural Mechanics) - Stationary**. 

![comsol-plate-modeling]({{ site.github.url }}/_img/simulate-physics/comsol-plate-modeling.png "首先肯定是建模了, 因为这个模型很简单, 所以用 COMSOL 来建模是不太复杂的. 基本就是考虑使用 Boolean 的减法 Different 就能够把东西做出来了. ")

然后确定材料, 这里我直接使用了 Built-in 的材料库里面的 Iron. 
如果想要修改的话, 可以在对应材料的 Settings 界面里面修改相应的参数,
也能够在 Plate 的 Linear Elastic Material 节点来修改. 
(不过其实差不多, 因为后者可以通过 from material 的方式来通过材料来制定. 
调成 user defined 的话就能够自己指定. )

通过在 Plate 的右键菜单中选择 Fixed Constraint 来设置固定的边. 
(不要选错了就好, 选成 Face Constraint 里面的话就会懵逼了, 不过毕竟是选边). 

![comsol-constraint]({{ site.github.url }}/_img/simulate-physics/comsol-constraint.png "截了这么多的图, 管理起来就好麻烦... 哎, 但是不截图又不方便来解释. ")

通过 Plate 的右键菜单中的 Edge Load 来添加负载边. 

![comsol-edge-load]({{ site.github.url }}/_img/simulate-physics/comsol-edge-load.png "嗯, 大概就是这样. ")

然后在 Mesh 中, (因为不太会, 所以保留的是默认的选项): 
Physics-controlled mesh, Normal. 选择 Build All 可以看到网格的划分. 

最后在 Study 中运行对应的步骤即可得到结果. 

在 Report 的右键菜单中选择 Brief Report 可以快速生成报告. 
(虽然有一个坑爹的地方就是, Generate 只是生成, 还要 Write 才会写入. )

<details>
<summary>最终生成的报告</summary>
<iframe src="{{ site.github.url }}/_img/simulate-physics/static-plate/report.html" title="static report"></iframe>
</details>

#### Dynamic Plate
还是上一个问题的模型. 不过为了研究运动的情况, 加入新的 Study. 
(如何将一个静态问题拓展到动态问题么... )

### Internal Flows
### Unsteady Heat Transfer
### Electrical Circuits
### Multiphysics
### Others
#### Solenoid
带电螺线管的磁场. 参考了网上的一个[教程](https://www.zhihu.com/question/275800609/answer/1665704227). 

先试试一个静态的: 

Model Wizard - 3D - Magnetic Fields (mf) - Stationary

建模过程: 通过两圆柱减得到圆柱壳 - 用于表示线圈; 用圆柱表示铁芯; 
用一个大的立方体来表示空气. 

![comsol-solenoid-modeling]({{ site.github.url }}/_img/simulate-physics/comsol-solenoid-modeling.png "嗯, 就是这样, 我觉得还行吧. 觉得这个建模实际上有点过于简陋了. ")

然后为物体添加材料, 材料属性全靠瞎猜. (这里就不放了, 
之后应该需要测量一些参数. )

然后在磁场中添加一个线圈 (右键 mf 菜单 - Coil)

![comsol-solenoid-coil]({{ site.github.url }}/_img/simulate-physics/comsol-solenoid-coil.png "感觉这张图截不截都无所谓的... 啊, 为什么要截呢? ")

在 Coil 中选择圆柱壳作为线圈, 并确定线圈的类型以及线圈的一些属性. 
在 Coil 节点下的 Coil Geometry 节点添加方向. 

![comsol-solenoid-coil-set]({{ site.github.url }}/_img/simulate-physics/comsol-solenoid-coil-set.png "不过具体应该有什么属性就也是自己瞎掰了. ")

(注: 环绕线圈 - Homogenized multiturn)

![comsol-solenoid-coil-set]({{ site.github.url }}/_img/simulate-physics/comsol-solenoid-coil-geo.png "注意方向不要选错了就好. ")

最后运行 Study 即可. 

<details>
<summary>结果报告</summary>
<iframe src="{{ site.github.url }}/_img/simulate-physics/solenoid/solenoid.html" title="solenoid"></iframe>
</details>
