---
layout: post
title:  "Lagrange Mechanics"
date:   2021-10-29 11:14:16 +0800 
categories: jekyll update
math: true
---
# 拉格朗日力学
记录一下我重新学习拉氏函数的过程. 

(因为之前对拉氏函数的学习完全就是背了个公式的地步, 
对背后的推导和证明不是很了解, 所以在看哈密顿力学的时候, 
欠的东西就都还回来了. QAQ)

(我认为没有物理含义的物理公式就是世界上最蹩脚的数学. )

## Kinetic Energy
动能, 应该没有不知道的. 

$$ T = \int  m \ddot{\vec{r_{}}} \cdot d\vec{r_{}} \\= 
\int m \frac{d\dot{\vec{r_{}}}}{d\vec{r_{}}} \dot{\vec{r_{}}} 
\cdot d\vec{r_{}} \\= 
\int m \dot{\vec{r_{}}} d\dot{\vec{r_{}}} \\= 
\frac{1}{2} m \dot{\vec{r_{}}}^2$$

(虽然仔细想想之后觉得好像这个动能可以理解成
运动的"力", 也就是加速度对位移的累积. 感觉这样不太好, 
还是说成是, 假如在能量不发生其他形式的转换的情况下, 
外力做功全部变成动能. 这样理解会好一点吗? )

## 虚位移-达朗贝尔原理
在"平衡状态"下, 包括静力学平衡和动力学平衡, 即: 

$$ \vec{F_i} = m \ddot{\vec{r_i}} $$

(其实我觉得就是牛二)

在这个基础上, 假如来一个很小的位移, 这个位移是一个假想的位移, 
是**把时间冻结**, 在保证 $\vec{F_i}$ 和 $\ddot{\vec{r_i}}$
都不会发生变化的情况下的位移. (虽然在数学上就是两端点乘一个小量
再移项而已. )

$$ \sum_i (\vec{F_i} - m \ddot{\vec{r_i}}) \cdot 
\delta \vec{r_i} = 0$$

(这个求和不过是累加被考虑的系统里的所有质点罢了)

## 拉格朗日关系
考虑用广义坐标表示的坐标$\vec{r_i}$: 

$$ \vec{r_i} = \vec{r_i}(q_i, t) $$

于是考虑$\vec{r_i}$对时间的全导数, 类似于物质导数的求法: 

$$ \frac{\mathrm{d} \vec{r_i}}{\mathrm{d} t} = \frac{\partial \vec{r_i}}{\partial t} +
   \sum_{\alpha} \frac{\partial \vec{r_i}}{\partial q_{\alpha}}
   \dot{q}_{\alpha}$$

为了去掉这么多复杂的东西(偏导), 再进行一次求偏导(对$\dot{q_{\beta}}$), 于是: 

$$ \frac{\partial}{\partial \dot{q_{\beta}}} \left( \frac{\mathrm{d}
   \vec{r_i}}{\mathrm{d} t} \right) = \frac{\partial \vec{r_i}}{\partial
   q_{\alpha}} \frac{\partial \dot{q}_{\alpha}}{\partial \dot{q_{\beta}}} =
   \frac{\partial \vec{r_i}}{\partial q_{\alpha}} $$

说明了$\vec{r_i}$和$q_{\alpha}$相对时间的变化是一致的? 
(我瞎猜的, 毕竟我很菜. 手动狗头. )

然后再求一次偏导(对$q_{\beta}$): 

$$ \frac{\partial}{\partial q_{\beta}} \left( \frac{\mathrm{d} \vec{r_i}}{\mathrm{d}
   t} \right) = \frac{\partial^2 \vec{r_i}}{\partial q_{\beta} \partial t} +
   \sum_{\alpha} \frac{\partial^2 \vec{r_i}}{\partial q_{\beta} \partial
   q_{\alpha}} \dot{q}_{\alpha} $$

然后通过一点点巧妙的变换, 可以得到: 

$$ \frac{\partial}{\partial q_{\beta}} \left( \frac{\mathrm{d} \vec{r_i}}{\mathrm{d}
   t} \right) = \frac{\partial}{\partial t} \left( \frac{\partial
   \vec{r_i}}{\partial q_{\beta}} \right) + \sum_{\alpha}
   \frac{\partial}{\partial q_{\alpha}} \left( \frac{\partial
   \vec{r_i}}{\partial q_{\beta}} \right) \dot{q}_{\alpha} $$

(这个变换成立的基础是偏导数的可交换性质, 
从物理上来看, 就是对于我考察的这个冻结的一点, 
究竟是先取位置微元还是取时间微元, 对于最终的结果没有本质区别. )

多么巧妙啊, 这样不就是相当于一个全导数了吗? 

$$ \frac{\partial}{\partial q_{\beta}} \left( \frac{\mathrm{d} \vec{r_i}}{\mathrm{d}
   t} \right) = \frac{\mathrm{d}}{\mathrm{d} t} \left( \frac{\partial
   \vec{r_i}}{\partial q_{\beta}} \right) $$

这个说明什么? 应该是偏导数和时间的全导数的顺序可以交换吧. 

## 拉氏方程
把$\delta r$用对广义坐标的偏微分代换: 

$$ \delta r = \sum \frac{\partial r}{\partial q_{\alpha}} \delta q_{\alpha}$$

(没有时间的项是因为前面的**冻结时间**的假定)
看广义力的表达式: 

$$ \sum F 
  (\sum \frac{\partial r_i}{\partial q_{\alpha}} \delta q_{\alpha}) 
  = \sum Q_i \delta q_{\alpha} $$

再看加速度项: 

$$ m \ddot{r} \delta r = 
m \ddot{r} \sum \frac{\partial r}{\partial q_{\alpha}} \delta q_{\alpha}
$$

考虑$\ddot{r} \frac{\partial r}{\partial q_{\alpha}}$ 
这个部分, 可以用$v du = d(u v) - u dv$的套路来化简. 
(这个真的是数学上的技巧吧. )

$$ \ddot{r} \delta r = 
   \frac{\mathrm{d}}{\mathrm{d} t} \left( \dot{r} \frac{\partial r}{\partial
   q_{\alpha}} \right) + \dot{r} \frac{\mathrm{d}}{\mathrm{d} t} \left( \frac{\partial
   r}{\partial q_{\alpha}} \right)\\
   
   = \frac{\mathrm{d}}{\mathrm{d} t} \left( \dot{r} \frac{\partial \dot{r}}{\partial
   \dot{q_{\alpha}}} \right) + \dot{r}  \left( \frac{\partial
   \dot{r}}{\partial q_{\alpha}} \right)
$$

第二步的变换用到了上面的拉格朗日关系. 这样以后, 
该怎么做就想必是十分明晰了. 

于是就会有虚位移原理的公式变形: 

$$ \frac{\mathrm{d}}{\mathrm{d} t} \left( \frac{\partial T}{\partial q_{\alpha}}
   \right) + \left( \frac{\partial T}{\partial q_{\alpha}} \right) = \sum Q_i 
$$

公式里面消掉了$\delta q_{\alpha}$

这就是拉格朗日方程. 

(当然, 保守场里面广义力可以写成势能的偏导, 然后就可以放进去. 
这个下次再写, 我认为主要的就是如何得到拉格朗日关系, 
以及导出拉格朗日方程. )

## 一点反思
那如果我只是对能量对求导, 好像也可以得到类似的结论. 
那么这种方法的好处在哪里? 

我有一种猜想: 就是对能量的求导是利用了能量对时间守恒, 
或者是能量的转化(功能关系)来得到的一个相对时间的常微分方程, 
往往会难以把多个自由度问题里面的多个广义坐标分开, 
就会造成处理上的问题? 

## 题外话
这个$ L_AT_EX $ 感觉好像不知道. 虽然一开始不可以, 
但是通过一系列地google和多方面的整合资料, 
现在可以成功. 
(感觉[网上的这个说法](https://lloyar.github.io/2018/10/08/mathjax-in-jekyll.html)
真是有道理, 互联网真是像极了春秋战国, 诸子百家各种众说纷纭. )
为了成功, 我找了好多的博客. 

目前我的方案是在
`_include`里面加载一个`<head>`的脚本`mathjax.html`, 
然后模块化地调用. 

{% highlight html %}
<script src="https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML" type="text/javascript"></script>
<script type="text/x-mathjax-config">
    MathJax.Hub.Config({
        tex2jax: {
        skipTags: ['script', 'noscript', 'style', 'textarea', 'pre'],
        inlineMath: [['$','$']]
        }
    });
</script>
{% endhighlight %}

然后在`_include`里的`head.html`文件中加上

{% highlight html %}
{%- if page.math -%}
  {%- include mathjax.html -%}
{%- endif -%}
{% endhighlight %}

这样就可以做到在`markdown`文件开头加上
`math: true`就可以在文章中用`$ $`来插入公式了. 

## 后记
怎么说呢, 上面的东西不一定对, 有错误请谅解并帮忙指出. 谢啦. 

XD