---
layout: post
title:  "物理实验中的数据分析基础"
date: 2022-08-22 23:34:43 +0800
math: true
categories: notes
---
# 物理实验中的数据分析基础
朦朦胧胧暑意未消, 半天下午最是难熬. 一节课了一节课开, 幻灯片走过啥也不会... 

报头痛哭, 然后打开PPT... 

## 误差
> 一次实验测量是对待测物理量的一次估计. 

* 待测物理量的真实值称为 **真值**. 
* 误差代表着对该次估计的 **置信程度**. 

<details>
<summary> 两种思想观点 </summary>
<p>
Frequentist: 误差及其分布用于描述真值落在 $[\mathrm{Measure} - \delta, \mathrm{Measure} + \delta]$ 的概率. (为主要学的东西)
</p>

<p>
Bayesian: 通过测量不断加深对测量真值的理解. 
</p>
</details>

### 误差的来源
* 统计误差: 完全随机, 可以用统计分布来讨论
* 系统误差: 测量方法导致的误差
* 过失误差: 手残

### 书写格式
* 测量值末位与误差末位相同: $7.55 \pm 0.03, 7.6 \pm 0.1$
* 误差与测量精度一致(和数字写在一起): $(87 \pm 1) \times 10^1$
* 测量误差经过运算的可以多保留一位, 但是也只能最多保留两位: $8.63 \pm 0.25$

对于数字式仪器, 误差由仪器精度给出; 对于刻度式仪器, 误差为(一般)最小刻度. 

### 测量误差的修约 -- 四舍五入
根据有效数字来进行修约: 

* $100 \sim 354$: 保留两位有效数字
* $355 \sim 949$: 保留一位有效数字
* $950 \sim 999$: 保留成 $1000$

### 概率的观点
既然测量是一种对真实值的估计, 那么就有一种概率来对应这个估计的可能性如何的. 

于是用可能性 $f(x)$ 来表示测量值和真值的偏差为 $x$ 的可能性. ($\int_{-\infty}^{+\infty} f(x) = 1$) 而使用一些概率论的知识, 可以定义期望: $E(x) = \int x f(x) \mathrm{d}x = \mu$, 定义方差为: $V(x) = E((x - E(x))^2) = E(x^2) - \mu^2 = \sigma^2$, 其中 $\sigma \equiv \sqrt{\sigma^2}$ 叫做标准差. 

上面的就可以像是这样来理解: 对于单次测量, 有一定的可能性能够击中真值, 但是也有一定的机率偏离真值, 于是就在偏离程度上产生了一个概率分布. 

#### 合并不同实验结果
> $1.1 \pm 0.1, 10 \pm 10, 1.01 \pm 0.01, 1.001 \pm 0.001$, 如何合并成一个实验结果:   
> $a = \frac{\sum a_i / \sigma_i^2}{\sum 1 / \sigma_i^2}, \sigma = \frac{1}{\sqrt{\sum 1 / \sigma_i^2}}$

可以通过[参数估计中的方法来做](#参数估计). 

<details>
<summary> 实验结果合并 </summary>

<p>
$n$ 次测量后, 物理量 $\mu$ 的结果为 $x_i \pm \sigma_i$, 认为 $x_i \sim N(\mu, \sigma_i^2)$. 于是似然函数为: 

$$L(\mu) = \prod \frac{1}{\sqrt{2 \pi} \sigma_i} \exp (-\frac{1}{2} (\frac{x_i - \mu}{\sigma_i})^2)$$

于是就能够通过解似然方程来得到最终的结果了. 
</p>
</details>

#### 概率的传递
假如已知 $x$ 的分布, 想要得到 $y(x)$ 的方差等信息. 

* 最直接的方法: $y$ 的概率密度 $g(y_0) = \sum_{y(x) = y_0} f(x)$, 于是就能够来计算 $\sigma_y$. ( 但是太过麻烦... )
* 泰勒展开: 通过泰勒展开来得到 $E(y)$ 和 $E(y^2)$ 的值: 
  * $y(x) \approx y(\mu_x) + \sum \frac{\partial y}{\partial x_i} (x_i - \mu_i)$
  * $(y(x))^2 \approx y^2(\mu_x) + 2 y(\mu_x) \sum \frac{\partial y}{\partial x_i} (x_i - \mu_i) + (\sum \frac{\partial y}{\partial x_i} (x_i - \mu_i))^2$
  * $E(y) \approx y(\mu)$, 其中 $E(x_i - \mu_i) = 0$
  * $E(y^2) \approx y^2(\mu) + 0 + \sum (\frac{\partial y}{\partial x_i} \frac{\partial y}{\partial x_j}) V_{i j}$
  * $\sigma_y^2 \approx \sum (\frac{\partial y}{\partial x_i} \frac{\partial y}{\partial x_j}) V_{i j}$

> 两个变量的例子: $y(x_1, x_2)$, 即上文中的 $x = x_1, x_2$:  
> $$\sigma_y^2 = (\frac{\partial y}{\partial x_1})^2 \sigma_{x_1}^2 + (\frac{\partial y}{\partial x_2})^2 \sigma_{x_2}^2 + 2 \frac{\partial y}{\partial x_1} \frac{\partial y}{\partial x_2} (x_1 - \mu_{x_1}) (x_2 - \mu_{x_2}) \\ \Rightarrow (\frac{\partial y}{\partial x_1})^2 \sigma_{x_1}^2 + (\frac{\partial y}{\partial x_2})^2 \sigma_{x_2}^2 + 2 \frac{\partial y}{\partial x_1} \frac{\partial y}{\partial x_2} \frac{\mathrm{cov(x_1, x_2)}}{\sigma_{x_1}\sigma_{x_2}} \sigma_{x_1}\sigma_{x_2} \\ \Rightarrow (\frac{\partial y}{\partial x_1})^2 \sigma_{x_1}^2 + (\frac{\partial y}{\partial x_2})^2 \sigma_{x_2}^2 + 2 \frac{\partial y}{\partial x_1} \frac{\partial y}{\partial x_2} \rho \sigma_{x_1}\sigma_{x_2}$$  
> 而当 $x$ 和 $y$ 是相互独立的时候, $\rho = 0$, 于是有 $\sigma_y = (\frac{\partial y}{\partial x_1})^2 \sigma_{x_1}^2 + (\frac{\partial y}{\partial x_2})^2 \sigma_{x_2}^2$.  

<details>
<summary> 协方差的定义 </summary>
定义协方差 $\mathrm{cov}[x, y]$ 为: 

$$\mathrm{cov}[x, y] = E((x - \mu_x)(y - \mu_y)) = E(x y) - \mu_x \mu_y$$

并定义相关系数为 $\rho_{x y} = \frac{\mathrm{cov}[x, y]}{\sigma_x \sigma_y}$. 

那么两个变量相互独立的话, $f(x, y) = \Theta (x) \Psi (y) \Rightarrow E(x y) = \iint x \Theta (x) y \Psi (y) \mathrm{d}x \mathrm{d}y = \mu_x \mu_y \Rightarrow \mathrm{cov}(x, y) = 0 \Rightarrow \rho_{x y} = 0$. 
</details>

## 参数估计
这个参数估计的作用应该是为了通过观察某些结果而侧面来得到系统的相关信息. 

比如一个粒子的分布概率可能为: $f(x, \theta) = \frac{1}{\theta} e^{-\frac{x}{\theta}}, (x > 0)$. 观测到了一组数据为 $x_1, x_2, \cdots, x_n$. 想要反推得到 $\theta$ 可能的分布. ( 通过实验结果来反推可能的参数. )

一般来说, 如果能够有满足结果的参数, 那么这样的参数应该会有: 
* $b = E(\hat{theta}) - \theta \rightarrow 0$: 让概率分布的系统不确定性变小. 
* $V(\hat{\theta}) \rightarrow 0$: 让概率分布的方差 ( 统计不确定性 ) 变小. 

### 最大似然法
或者换一种想法就是, 在观测结果 $x_1, \cdots, x_n$ 附近概率应该最大, 于是构造一种函数 ( 似然函数 ): 

$$L(\hat{\theta}) = f(x-1, \cdots, x_n, \hat{\theta}) = \prod f(x_i, \hat{\theta})$$

或者为了好算, 对其取对数: $\ln L(\hat{\theta})$. 

然后取似然函数最大值的 $\hat{\theta}$ 为最大似然估计量, 即 $\frac{\partial L}{\partial \theta} = 0$, 于是得到了可能的参数 $\theta$ 的分布情况. 

而为了估计 $\theta$ 可能的方差, 则利用**信息不等式**: 

$$V(\theta) \geq (1 + \frac{\partial b}{\partial \theta})^2 / E(- \frac{\partial^2 \ln L}{\partial \theta^2})$$

确定了最小方差界. 也称为 RCF (Rao-Cramér-Frechet) 不等式. 

一般可以估计为 $V(\theta) \approx -1/E(- \frac{\partial^2 \ln L}{\partial \theta^2})$

### 最小二乘法
和最大似然法的将概率往大了凑 ( 往真值附近凑 ) 不同的思想, 最小二乘法的想法应该就是把方差往小了放. 

所以就要求函数: 

$$\chi^2(\hat{\theta}) = \sum (y_i - \lambda (x_i, \theta) (V^-1)_{ij} (y_j - \lambda(x_j, \theta))$$

取到最小值, 即最小二乘估计量 $\hat{\theta}$. 

## 后记
懒得写了, 大概就这样吧. 之后用到的时候再修改. 

其中讲的一些东西, 比如参数估计和最小二乘法的关系有点没有搞清楚, 感觉自己理解起来不是同一个东西, 所以还是不一起写了. 
