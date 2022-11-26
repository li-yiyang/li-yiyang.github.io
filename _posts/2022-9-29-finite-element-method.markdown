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
            - [Dynamic Plate (Eigenfrequency)](#dynamic-plate-eigenfrequency)
            - [3D Stress - Import CAD File and Complex Load](#3d-stress---import-cad-file-and-complex-load)
        - [Internal Flows](#internal-flows)
        - [Unsteady Heat Transfer](#unsteady-heat-transfer)
        - [Electrical Circuits](#electrical-circuits)
        - [Multiphysics](#multiphysics)
        - [Others](#others)
            - [Solenoid](#solenoid)
    - [Others](#others-1)
        - [AutoCAD](#autocad)
            - [Basic Commands](#basic-commands)
            - [Basic Concepts](#basic-concepts-1)
            - [3D Modeling in AutoCAD](#3d-modeling-in-autocad)
        - [Fusion 360](#fusion-360)
    - [Final Words](#final-words)

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

(Note: 在 Mac 下, COMSOL 的安装位置附近有一个叫做 `COMSOL Setup.app` 的程序, 
可以通过这个程序来添加和更改 COMSOL 的包. 在 Windows 下好像是在卸载和更改程序处. 
如果一开始安装的时候有些问题的话, 可以通过这个来修改. )

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
可以通过右键 Geometry - Import 来导入 CAD 文件. 或者使用 LiveLink 来操作. 

可惜的是, 在我的电脑上, 只能导入 `.step` 文件, 并且 LiveLink 貌似也没法工作. 
(详细的操作可以看 [3D Stress](#3d-stress---import-cad-file-and-complex-load) 中的一些操作. )

对于导入的文件, 一个需要注意的点就是要让导入的模型尽可能和坐标原点贴近. 
不然的话, 导入了之后再想要在 COMSOL 里面添加新的元素和物体的话就会有些麻烦. 
因为不知道坐标. (或者干脆全部在 CAD 软件里面建模也不是不行. )

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

#### Solid Mechanics
实体的力学模块. 和 Plate 十分类似, 都是一个 Constraint, 
Load 的这样的方式来添加约束和负载. 

#### Meshs
网格划分, 在前面的 FEM 部分就有介绍. 对于简单的模型, 貌似, 
系统自带的网格划分就挺好用的. 至于高级的操作, 之后再试. 

#### Study
使用 `F7` 可以运行特定的一步. 
使用 `F8` 可以运行整个 Study. 
或者可以直接通过右键对应步骤节点, 然后选择 Compute. 

这里记录一些用过的: 

* **Stationary**: (稳定解? )  
  貌似没什么特别需要注意的
* **Eigenfrequency**: (找特征频率? )  
  有一个在某某频率附近搜索的参数. 在 Study - Eigenfrequency - 
  Study Settings - Search for eigenfrequency around 里面设置. 
  默认是 1Hz. 
  
并且这些研究都需要指定对那些物体生效, 默认是全部选择, 
如果取消选择的话, 类似于就对这个物体删除了. 

在 Study 中可以设置对不同的参数进行模拟和计算: 在对应的 Study 节点 Settings 里, 
通过 Study Extension 的 Auxiliary Sweep 的部分添加变量的变化 
`range(start, step, stop)`, 来计算不同的结果. 

#### Results
在 Results 里面, 有一些简单的操作. 比如改变结果的展示方式, 
输入模拟仿真的报告等. 

* **增加不同的视图**:  
  在 Results 节点右键可以添加新的显示视图 (这样的视图会在报告中分别显示), 
  可以选择显示的类型为 3D, 2D, 1D Plot Group. 
  在其中一个节点 - 比如说 Stress 节点 
  (在力学模块中的结果), 然后添加 Volume, Arrow Space 等显示形式, 
  通过这些显示形式可以得到不同的表现形式. 
  
  (有点像是 CAD 中通过不同的图层来表现不同的线形. 
  这些添加的显示形式彼此之间的关系就像是同一个图片上的不同图层. )
* **改变结果的展示方式**:  
  在 Results 的节点下面, 有很多的子节点, 这些节点可以有很多的显示设置. 
  比如改变结果的单位, 图像的标题. 
  
  ![comsol-plot-edit]({{ site.github.url }}/_img/simulate-physics/comsol-plot-edit.png "这张图里面可以看到, 可以手动设置显示的单位 (Expression - Unit), 以及显示的标题 (Title - Title type, Manual), 或者是现实不同的物理量, 比如上面的一系列的设置, 选择了一个位移来显示. ")

* **导出简单的报告**:  
  在 Report 节点下面, 通过右键菜单可以加入 Brief Report 等一堆的报告类型. 
  并且在里面还能够进行一系列的设置. 
* **将模拟的不同结果依次生成关键帧形成动画**:  
  我觉得这个功能对 Time Dependent 比较好用. 在 Export 节点右键, 
  选择添加一个 Animation 节点即可. 
* **对结果进行运算处理**:  
  在 Result 的节点下面的 Derived Values 右键添加计算. 可以选择积分等等的操作. 
  生成的结果是一个表格, 通过 Results 节点下添加 1D Plot Group 可以为这个画图. 

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

(下载文件: [static-plate]({{ site.github.url }}/_img/simulate-physics/static-plate/static-plate.mph "不好说我是否完成了书里面的所有操作就是了. "))

#### Dynamic Plate (Eigenfrequency)
还是上一个问题的模型. 不过为了研究运动的情况, 加入新的 Study. 
(如何将一个静态问题拓展到动态问题么... 哦, 错了, 不是严格的 "动态",
而更像是找到本振频率? 或者是考虑在不同频率下的受力情况. 即振动环境. )

![comsol-dynamic-plate-new-study]({{ site.github.url }}/_img/simulate-physics/comsol-dynamic-plate-new-study.png "在 Study 节点下面右键菜单中添加新的 Study Step: Eigenfrequency. ")

并且, 还需要取消前一个问题中的静态求解. 

![comsol-dynamic-uncheck-study]({{ site.github.url }}/_img/simulate-physics/comsol-dynamic-uncheck-study.png "在我的电脑上的图标 (一个勾选框) 和书中的图标 (一个叉) 有点不一样, 不过在逻辑上来说, 就是要取消对原来的平面的一个静力学分析, 所以应该是差不多的. ")

因为一般的动态分析中, 原则上应该是类似这样的方程: 
$\boldsymbol{F} = m \boldsymbol{a}$, 所以除了静力学上的考虑, 
还需要提供材料质量的信息. (不过我已经用了 Built In 库, 所以忽略了. )

于是运行 Study (`F8`) 即可得到结果. 但是比较重要的是对结果的分析. 
(不然感觉和前一步没什么区别... )

可以看到, 程序给出了六个可能的振动频率. 

![comsol-dynamic-plate-eigenfrequency]({{ site.github.url }}/_img/simulate-physics/comsol-dynamic-plate-eigenfrequency.png "一个可能的例子就是用来检测结构是否会在特定频率附近发生共振, 以减少损失吧. ")

<details>
<summary>生成的报告</summary>
<iframe src="{{ site.github.url }}/_img/simulate-physics/dynamic-plate/report.html" title="dynamic plate"></iframe>
</details>

(下载文件: [static-plate]({{ site.github.url }}/_img/simulate-physics/dynamic-plate/dynamic-plate.mph "感觉还行吧, 做完了之后就是这么觉得的. "))

#### 3D Stress - Import CAD File and Complex Load
COMSOL - Model Wizard - 3D - Structural Mechanics - Solid Mechanics. 

这次模拟的是一个类似于夹持件的东西. 有点像是通过 8 个通孔 (bolt, 螺栓)
把一个棍子限制在沿杆方向. 如果只考虑垂直杆方向上的受力的话, 
需要了解这个组件的受力情况. 

我这里打算干一些高级一些的操作. (其实是因为学生版的书里面并没有模型文件, 
并且之后的模拟也需要 CAD 软件的建模. 所以我要尝试一下如何用 AutoCAD 来导入模型, 
然后在 COMSOL 里面仿真. )

虽然两个都是我不太熟的软件 (甚至 Blender 可能还更容易些... ), 但是, 
作为鸟人的一个坚持就是: 啥最作死, 就用啥. 关于 AutoCAD 的使用笔记, 
记录在下面的 [Others](#autocad) 里面. 

(花了一段时间重新回顾了一下高中机械制图的知识... 发现了一个坑爹的问题. 
我好像画的是 2 维的图. 那要怎么变成 3D 的玩意导入到 COMSOL 里面啊... )

[![comsol-modeling]({{ site.github.url }}/_img/simulate-physics/comsol-modeling.bmp)]({{ site.github.url }}/_img/simulate-physics/3d-stress/model.dwg  "虽然这么简单的几何体的图应该还算是比较好画的... 但是我还是画了挺久. 并且在我画完了之后才发现了问题的不对劲: 我不是应该画一个 3D 的东西么. 于是经过了一系列的复杂操作之后, 我才画了这么一个非常简单的一个图... 真是有些让人唏嘘. 不过可以点击图片来下载 .dwg 文件. 不过还请不要嘲笑我软件用得稀巴烂就好了. ")

淦, 在我建完模之后, 在 COMSOL 里面导入的时候, 弹出了一个很坑爹的窗口: 

![comsol-import-error-windows]({{ site.github.url }}/_img/simulate-physics/comsol-import-error-windows.png "再来几次这种坑爹的事情的话我就要考虑一下搞一台 Windows 的电脑了. ")

行吧, `.dwg` 你吃不消, 我换总行了吧... 于是通过 Fusion 360 转换成 `.step`,
(关于 [Fusion 360](#fusion-360)), 然后导入 (在 Geometry 节点, 右键菜单, 
选择 Import, 然后选择文件, 然后导入, 不过建议导入的时候选择保留 CAD 的尺寸: 
Import - Length Unit - from CAD document): 

[![comsol-import-step]({{ site.github.url }}/_img/simulate-physics/comsol-import-step.png)]({{ site.github.url }}/_img/simulate-physics/3d-stress/model.step "点击图片可以下载我的那个 STEP 文件. ")

对于导入的模型, 右键 From Union - Build Selected 来将导入的对象形成最终的模型. 

(注: 虽然 COMSOL 有一个叫做 LiveLink 的功能, 但是, 对不起, 
坑爹的 mac 让我再一次被背刺. )

虽然和案例的模型有点不一样, 但是差不多吧. 快快开始模拟的过程吧. 

并且这个案例还需要使用到一些复杂的受力 (Complex Load). 
所以参数化受力十分有必要. 首先设置全局变量: 

![comsol-3d-stress-global-para]({{ site.github.url }}/_img/simulate-physics/comsol-3d-stress-global-para.png "虽然我目前觉得比较难以将全局变量和之后定义的 Component 里面的 Definition 中的变量区分. 但是我觉得可以这样理解: 全局变量就像是简单的变量替换, 类似于一个写死的静态变量. 是一个确定的值. 但是在 Definition 中的 Variable 就像是每次计算时都会重新计算生成的一个新的值. 有点像是函数. ")

然后是 Definition 右键 Variables 设置一个相对局部的一个变量: 

![comsol-3d-stress-definition-var]({{ site.github.url }}/_img/simulate-physics/comsol-3d-stress-definition-var.png "不过也不能说是一个相对局部的一个变量, 我个人觉得这个东西应该更像是一个函数一样的东西, 因为这个东西的作用域 Geometric entity level 比较难以理解. ")

> Note: Variables used should be exactly the same as those defined
> in the Parameters table.  
> (书中的一个注记, 但是我有点不太能解释, 所以留在这里. )

比如说施加的力有这样的特征: 大小相等, 但是方向不一定, 
那么一个普通的想法就是用一个圆 ($SU(1)$) 来表示. 
通过在 Definition 节点下右键 - Coordinate Systems - Rotated System. 
可以设置一个旋转坐标系来表示施加在孔上的负载. 

![comsol-3d-rotate-sys]({{ site.github.url }}/_img/simulate-physics/comsol-3d-rotate-sys.png "建议自己去打开看看, 下面有一个比较好看的欧拉转动角的示意图. 虽然感觉多次一举, 因为这里感觉好像用不到这么复杂的转动矩阵. 但是这样作为教学实例来说挺好的. ")

(注: 上图中的坐标设置相当于绕着 $x$ 轴旋转了 `theta0` 的角度. 
这就有点像是 CAD 里面的局部坐标系. 因为想要描绘一个旋转后的力不方便, 
所以用一个相对与旋转过的坐标系的东西就比较好理解了. 哦... 
原来这就是理论力学课上老师说的在地面系描述转动系的困难么. )

然后添加材料 Structural Steel ([结构钢](https://en.wikipedia.org/wiki/Structural_steel)). 

将通孔设置为固定: 

![comsol-3d-stress-fix]({{ site.github.url }}/_img/simulate-physics/comsol-3d-stress-fix.png "右键 Solid Mechanics - Fixed Constraint, 然后在 Graphics 视图选择通孔. ")

接着添加受力: 

![comsol-3d-stress-load]({{ site.github.url }}/_img/simulate-physics/comsol-3d-stress-load.png "右键 Solid Mechanics - Boundary Load. 选择圆孔的内边缘, 即和杆相接触的面. 并且使用之前设置的 Rotated System 2 作为 Load 的坐标系, 而不是全局的坐标系. ")

(注: 如果你看不到像图中的力的图标一样的东西的话, 可以在 Solid Mechanics 节点下, 
选择 Physics Symbols 然后再勾选 Enable Physics Symbols. 关于[图例和注记](https://doc.comsol.com/5.5/doc/com.comsol.help.comsol/comsol_ref_modeling.10.18.html). )

最后建立网格 (Mesh - Build All), 进行运算 (Study - Compute), 得到结果. 

![comsol-3d-stress-result]({{ site.github.url }}/_img/simulate-physics/comsol-3d-stress-result.png "虽然这个结果让我有些意外... ")

接下来开始进行后处理: 

* 比如说想要得到施加的力的直观显示:  
  在 Results - Stress 节点下右键添加一个 Arrow Surface, 
  将其展示方式修改成受力得到箭头平面: 

	![comsol-3d-stress-arrow-surface]({{ site.github.url }}/_img/simulate-physics/comsol-3d-stress-arrow-surface.png "如果觉得箭头的显示效果不是很好看的话, 可以通过 Number of arrows, Coloring and Style 里面的参数进行修改. ")
	
* 得到多个不同的模拟结果的直观显示
* 比如说想要得到不同角度施加的力对结果的影响:  
  尽管不同角度可以通过直接一个个修改 Global Definition 中的 `theta0` 来计算, 
  但是这种反人类的操作实在不应该是人来干的. 一个自动化的操作就是让程序来计算:  
  在 Study 中的 Stationary 节点下面, 通过 Study Extansion 
  设置选项来为变化参数添加一个变化范围. 
  
  ![comsol-3d-stress-study-ext]({{ site.github.url }}/_img/simulate-physics/comsol-3d-stress-study-ext.png "这里的输入方式是这样的 range(start, step, stop). 这样运行之后得到的结果可以在 Resutls 中看见. ")
  
* 用图表的形式输出统计结果对不同角度的依赖. 



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

## Others
### AutoCAD
感谢 Autodesk, 给了我 Educational License. (估计这种感激之情, 
只能够持续到我教育优惠结束的那一刻. 在官网上注册账号, 用学信网提供的证明, 
就能够得到教育优惠了. 并且还是全家桶. 唯一稍微坑爹一点的就是只能使用最新版本? )

(其实选择 AutoCAD 的另外一个原因是因为 SolidWork 没法在 macOS 上用. 
我在思考是不是应该换一台好一点的电脑... )

#### Basic Commands
AutoCAD 的操作逻辑可以说就是一个命令式操作方式, 我觉得很舒服. 
毕竟这样比较清晰. 不会有那种拉着看看大概是这样就可以了的操作. 
(虽然也同时意味着它不直观. 以及操作及其困难. )

主要可能用到的命令有: 

* `L` 直线, 可以通过打开 Object Snap 来让选点的时候自动吸附. 
  确定起点之后, 输入 `TAN` 来吸附圆的切线. 
* `C` 圆
* `REC` 长方形
* `SPL` 曲线 
* `REG` 生成面域 (region)
* `EXT` 拉伸 (extansion)
* `SU` 差集 (substract), `UNI` 并集 (union), `IN` 交集 (intersect)
* `UCS` 定义坐标系
* `SW` 扫掠
* `LOFT` 放样
* `WE` 楔体
* `QDIM` 快速尺寸标注, `DIM` 标注, `DIMA` 角度标注, `DO` 直径标注
* `M` 移动, `RO` 平面旋转, `3DROTATE` 三维旋转
* `REV` 旋转生成三维物体
* `F` 倒角
* `TR` 删除选中的线, `E` 删除选中的对象  
  两个的区别就是 `TR` 命令会自动识别相交的部分然后删除, 
  而 `E` 命令只能删除已经画好的整体. 用的场合有点不太一样. 

主要的操作就是输入命令, 然后输入对应的参数 (用逗号可以分隔参数), 
空格或回车用来确认, `ESC` 用来撤销. 

#### Basic Concepts
在 AutoCAD 里面的操作, 实际上非常的朴实无华. (至少我目前会的就这么些). 
和纸上的那种机械制图差不多. 通过各种辅助线来确定位置和绘制形状. 

通过建立不同的图层来设置线形和逻辑关系. (对我来说) 一般是建立一个实线, 
一个辅助线, 一个中心线, 一个虚线, 一个注记线图层. 这样的操作逻辑, 
在 KiCAD 的里面的 PCB 布线也比较类似. 

#### 3D Modeling in AutoCAD
虽然网上都说 AutoCAD 的 3D 建模效果并不如其他的软件好用, 
但是手上有的, 总比装不了的 SolidWork 好. 因为目前还是简单的模型, 
所以建模的想法比较简单: **就是通过三视图然后加减得到结果**. 

具体的操作大概是这样: 

* 选择一个面, 利用 `REG` 来生成面域 (如果是圆之类的可以不用)  
  然后在这个面的基础上面, 利用 `EXT` 来拉伸出一个立体. 
  通过 `SU` 来减去孔洞. 
* 另外的面同理, 也能够得到立体. 
* 将得到的立体通过 `M` 和 `3DRORATE` 来和另外的面生成的立体对齐. 
* 通过 `IN` 等命令得到最终的形体. 

(虽然思路是这样, 但是具体操作可能还需要一些熟练度... )

至于高级的操作, 我会再去学一些... 

### Fusion 360
也是 Autodesk 的软件, 虽然这个据说比较简单, 但是我有点没法理解这个的操作逻辑. 
并且也有一个叫做 Inventor 的一个软件, 据说和 Autocad 的操作逻辑类似, 
唯一的问题就是, 在 mac 上用不了. 所以也只能使用 Fusion 360 了. 

不过不能因为自己的奇怪 xp 就不去学这样的好玩的东西啊. 
所以我还是留下了它. (其实是因为没有其他的靠谱的东西可以用了. 
毕竟不能够真的用 Blender 来建模吧. Blender 里面还是有一些离谱的地方的. )

(注: 使用的时候如果网络不是很好的话, 可能会比较难受... )

## Final Words
