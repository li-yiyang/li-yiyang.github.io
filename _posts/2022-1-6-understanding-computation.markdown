---
layout: post
title:  "Ruby : Understanding Computation"
date:   2022-01-13 11:12:50 +0800
categories: jekyll update
math: true
---
# 计算的本质 -- 读书记录
因为SICP太难了, 所以从这本简单一点的书开始看看, 顺带对应明年的计科导第一单元. 
算是预习吧? 是吧? 

注: 我这里面的所有的文字并不都是原书中摘录的, 大部分都是自己瞎写的. 
(甚至没有任何的修改, 都是直接写出来的, 估计错误也一大堆)看的话就图一乐吧. 

## 简单的ruby基础
记录一下自己之前没有太注意的点就行了. 

### proc
将函数(方法)看作数据对象来看的思想. 

```ruby
# 一种简单的proc的写法
multiply = -> x, y { x * y }

# proc的调用, 可以用call, 可以用方括号
multiply.call(6, 9) # => 54
multiply.call(2, 3) # => 6
multiply[3, 4] # => 12
```

### 类和模块
类就是一个分类的思想, 然后模块的思想就是多重继承? 

在子类里面可以用`super`来调用超类里面的同名方法. 

```ruby
class Father
  def times(x, y)
    return x * y
  end
end

class Children < Father
  def times(x, y)
    res = super(x, y)
    return -1 * res
  end
end

Children.superclass # => Father

child = Children.new
child.times(2, 3) # => -6
```

在子类里面可以拥有父类的方法, 并且, 通常的做到这样的方法是利用`module`来做, 
因为这样可以mix-in多个所谓的父类. 

### 杂项
```ruby
# inspect方法, 提供一个对象在控制台中展示的方式
o = Object.new # => #<Object:0x0000aaab0df110b8>
def o.inspect; "OOOOOOO"; end
o # => OOOOOOO

# 定义方法的时候用`*'运算符来表示数目可变的参数, 
# 虽然每个方法只能有一个可变参数, 但是可变参数的位置可以随便放
def func(a, *b, c); ; end

# 删除常量
Object.send(:remove_const, :CONST_NAME)
# 注: remove_const是一个private方法, 只能内部调用
```

## 程序和机器
"含义"的含义, 这个问题倒是没有仔细想过. 我们周围的东西都是一些符号, 
譬如虚数$i$, 就不过是满足方程$x^2 + 1 = 0$的符号而已
(实际并不是, 因为不是这样定义的, 上面说是拥有这样性质的符号的话, 感觉更好, 
不过, 直接的那样的定义感觉很好理解), 这样的符号的指意能力, 就是一种哲学的思想. 
(我现在可以理解一点"语义分析"的意思了, 大概)

考虑"形式主义", 在"我是谁? ", 这个问句里面, 有主语, 谓语, 还有宾语, 
这样的组合形成了一个句子. 然后可以适当地替换其中的成分, 就可以得到新的含义, 
然而实际上这不过是符号之间的不同组合罢了. 但是, 假如调换了顺序, 
或者改变了结构, 哪怕是同样的符号也不能够有原来的含义. 

啊这, 感觉自己在讲废话. 

回到计算机程序, 计算机程序的语言实际上就像是人的语言一样, 或者干脆说, 
就是人的语言也不是不行. 计算机的编程语言也有自己的结构, 然后对于这样的结构, 
计算机做的事情就是将语句破碎分割成片段, 然后重新根据片段构建一个结构的概念, 
接着再在结构的基础上形成语义的概念. (代码通过语法解析器生成抽象语法树AST, 
即Abstract Syntax Tree; 在这基础上继续执行得到程序的意义)

那么人是如何处理语言和语义的? 听到一句话, 然后我会下意识地去分析语法结构吗? 
但是语法结构明明是我在会说会写之后才掌握的吧? 啊, 不是, 
我觉得应该确实有这样的下意识过程. 拿学习语言的过程来看, 一开始什么也不会, 
也就有需要单纯的结构来得到语义的过程. 比如"私わ中国人です", 
就像是一个`String`的模版一样, `"#{a}わ#{b}です"`, 就好像是在说`a=b`一样. 
感觉我学的时候就是有这样的一个转换过程, 然后熟练了之后就变成了无意识的过程了. 

然后为了得到确定的语义, 也就是为了消除二义性, 就要有对语言的规范的定义. 
比如"我家热得快炸了"就有二义性, 首先可以这样断句(语义断句): 
"我/家//热得//快炸了", 就是一个"主语(定语/主语)//谓语//状语"的概念. 
然后又可以这样断句: "我家/热得快//炸了", 就是一个"主语(定语/主语)//状语". 
(注: 热得快以一种加热器, 热水用的. )
(这段语文的分析完全是随意分析的, 有错是应该很正常的. )

### 小步语义和大步语义
书中利用了一个Simple语言的例子来讲, 我就照猫画虎地试试看看能不能也来一个. 

首先是**小步语义**, 数学定义如下: 

$$\frac{\langle e_1, \sigma \rangle \leadsto_e e_1'}{\langle e_1 + e_2, \sigma \rangle \leadsto_e e_1' + e_2} \quad \frac{\langle e_2, \sigma \rangle \leadsto_e e_2'}{\langle v_1 + e_2, \sigma \rangle \leadsto_e v_1 + e_2'} \\ \frac{}{\langle n_1 + n_2, \sigma \rangle \leadsto_e n} \  \mathrm{if} \  n = n_1 + n_2 \\ \frac{\langle e_1, \sigma \rangle \leadsto_e e_1'}{\langle e_1 * e_2, \sigma \langle \leadsto_e e_1' * e_2} \quad \frac{\langle e_2, \sigma \rangle \leadsto_e e_2'}{\langle v_1 * e_2, \sigma \rangle \leadsto_e v_1 * e_2'} \\ \frac{}{\langle n_1 * n_2, \sigma \rangle \leadsto_e n} \  \mathrm{if} \  n = n_1 \times n_2 \\ \frac{\langle e_1, \sigma \rangle \leadsto_e e_1'}{\langle e_1 < e_2, \sigma \rangle \leadsto_e e_1' < e_2} \quad \frac{\langle e_2, \sigma \rangle \leadsto_e e_2'}{\langle v_1 < e_2, \sigma \rangle \leadsto_e v_1 < e_2'} \\ \frac{}{\langle n_1 < n_2, \sigma \rangle \leadsto_e \boldsymbol{\mathrm{true}}} \  \mathrm{if} \  n_1 < n_2 \quad \frac{}{\langle n_1 < n_2, \sigma \leadsto_e \boldsymbol{\mathrm{false}}} \  \mathrm{if} \  n_1 \geq n_2 \\ \frac{}{\langle x, \sigma \rangle \leadsto_e \sigma(x)} \  \mathrm{if} x \in \mathrm{dom}(\sigma) \\ \frac{\langle e, \sigma \rangle \leadsto_e e'}{\langle x = e, \sigma \rangle \leadsto_s \langle x = e', \sigma \rangle} \quad \frac{}{\langle x = v, \sigma \rangle \leadsto_s \langle \boldsymbol{\mathrm{do-nothing}}, \sigma [x \mapsto v]} \\ \frac{\langle e, \sigma \rangle \leadsto_e e'}{\langle \boldsymbol{\mathrm{if}} (e) \{ s_1 \} \boldsymbol{\mathrm{else}} \{ s_2 \}, \sigma \rangle \leadsto_s \langle s_1, \sigma \rangle} \\ \frac{}{\langle \boldsymbol{\mathrm{if}} (\boldsymbol{\mathrm{true}}) \{ s_1 \} \boldsymbol{\mathrm{else}} \{ s_2 \}, \sigma \rangle, \leadsto_s \langle s_1, \sigma \rangle} \quad \frac{}{\langle \boldsymbol{\mathrm{if}} (\boldsymbol{\mathrm{false}}) \{ s_1 \} \boldsymbol{\mathrm{else}} \{ s_2 \}, \sigma \rangle \leadsto_s \langle s_2, \sigma \rangle} \\ \frac{\langle s_1, \sigma \rangle \leadsto_s \langle s_1', \sigma' \rangle}{\langle s_1; s_2, \sigma \rangle \leadsto_s \langle s_1'; s_2, \sigma' \rangle} \quad \frac{}{\langle \boldsymbol{\mathrm{do-nothing}}; s_2, \sigma \rangle \leadsto_s \langle s_2, \sigma \rangle} \\ \frac{}{\langle \boldsymbol{\mathrm{while}} (e) \{ s \}, \sigma \rangle \leadsto_s \langle \boldsymbol{\mathrm{if}} (e) \{ s; \boldsymbol{\mathrm{while}} (e) \{ s \} \} \boldsymbol{\mathrm{else}} \boldsymbol{\mathrm{else}} \{ \boldsymbol{\mathrm{do-nothing}} \}, \sigma \rangle}$$

虽然看起来就是和一堆乱码没有任何的区别, 但是这就是利用一个迭代的定义方式, 
来得到了对于Simple的定义. 我觉得这样的定义十分的漂亮和简洁, 
所以我就用这个方法来介绍. 

首先是要理解这里面的每个元素和语义的含义, 然后请尝试理解一下: 
* 首先是**表达式**
  $\langle \mathrm{expression}, \mathrm{environment} \rangle$     
  这个实际上就是我们的$e, e_1, e_2$等等的东西. 
* 表达式会对应着**值**, $e'$这样的东西, 或者也可以叫做是**返回值**. 
* 相对于$e'$的返回值(这样的值更像是符号值), 定义一个**字面值**, 即$n$
* 并且有两个特殊的值, **true**和**false**, 是一个(特殊含义的符号值)
* 然后是**环境**$\sigma$ (或者叫做**上下文**会不会更好? )
* 在环境中有**变量**$x$, 也是一种符号值(应该不是符号值, 
  算是一种根据符号然后去找出值的映射)

(感觉自己缺乏很好的语言来描述)

那可以来几个例子: 
* $(1 * 2) + (3 * 4)$      
  这个表达式在上面的规则里面可以写成:    
  $$\langle (1 * 2) + (3 * 4), \sigma \rangle \\ \leadsto_e \langle (1 * 2), \sigma \rangle + \langle (3 * 4), \sigma \rangle \\ \leadsto (1 * 2) + (3 * 4) = 2 + 12 = 14$$      
  不要觉得这个简直就是脱裤子放屁, 因为我们可以利用这样的方法来得到复杂的结论, 
  比如说让计算机来运算更加复杂的表达式. 

```ruby
# Note: 我打算和原来的做法不一样, 
#   试试看自己写新的代码, 不是照抄原来的代码. 

# 首先是一个能够执行代码的机器, 
#   它的输入是一个表达式, 我们的机器的功能就是可以对这个表达式进行求值
#   然后还有一个环境, 在这个环境里面有我们的变量
class Machine < Struct.new(:exp, :env)
  # run方法的作用就是让机器来运行表达式
  def run
    @exp.eval
  end

  # 
end

class Expression
  # 拥有一个可变的参数列表输入
  def initialize(*arg)
    @arg = arg
  end

  # 这里的eval方法就是一个空方法, 可以说就是do-nothing
  #   具体的运算的方法要在每个子类里面定义
  def eval; ; end
end

class Add < Expression
  # 加法就是把所有的参数(字面值)加在一起
  def eval
    res = 0
    @arg.each do |item|
      if item.is_a? Expression
        res += item.eval
      else
        res += item
      end
    end
    return res
  end
end

class Mutiply < Expression
  # 乘法就是把所有的参数(字面值)乘在一起
  def eval
    res = 1
    @arg.each do |item|
      if item.is_a? Expression
        res *= item.eval
      else
        res *= item
      end
    end
    return res
  end
end

# 定义一个小于的比较
class Compare < Expression
  def initialize(a, b)
    super(a, b)
  end
end

# 于是
#   Add.new(Mutiply.new(1, 2), Mutiply.new(3, 4)).eval
#     # => 14
```

(2022-1-13: 这几天先学CTF, 有些头大. 这个先停一下. )