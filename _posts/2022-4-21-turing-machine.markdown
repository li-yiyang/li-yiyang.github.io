---
layout: post
title:  "Make A Turing Machine Yourself"
date:   2022-04-21 22:55:47 +0800
math: true
categories: ruby
---
# Make A Turing Machine Yourself
## What's Turing Machine? 
> Purely formally a Turing machine can be specified as a quadruple $T = (Q, \Sigma, s, \delta)$ where:
> * $Q$ is a finite set of states $q$
> * $\Sigma$ is a finite set of symbols
> * $s$ is the initial state, $s \in Q$
> * $\delta$ is a transition function determining the next move:       
>   $$\delta : (Q \times \Sigma) \rightarrow (\Sigma \times \{L, R\} \times Q)$$
> 
> The transition function for the machine T is a function from computation states to computation states. If $\delta (q_i, S_i) = (S_{i, j}, D, q_{i, j})$, then when the machine's state is $q_j$, reading the symbol $S_j, T$ replaces $S_j$ by $S_{i, j}$, moves in direction $D \in \{L, R\}$ and goes to state $q_{i, j}$. 
> 
> [Turing Machines form Stanford Encyclopedia of Philosophy](https://plato.stanford.edu/entries/turing-machine/)

> Note: there are also a similar but little different formal [definition](https://en.wikipedia.org/wiki/Turing_machine). 
> 
> I won't cover much of the history about Turing Machine, so if you like, please refer to the wiki for its history. 

呃, 看起来也许有一种不明觉历的庄严感, 但是实际上图灵机(Turing Machine)是一种非常简单的计算机模型. 

(注: 这个简单更应该说是结构简单. 但是虽然结构很简单很基础, 但是并不代表给它编写程序会是一种让人愉悦的事情. 我不会强调和注重编写程序的效率和速度之类的东西, 毕竟这些还是交给计算机系的人来卷吧. 我们的目标就是能动的图灵机! )

## 先把图灵机形式化的定义扔到一边
(假如你觉得图灵机的形式化定义非常容易理解的话, 那么还是直接跳过看看代码的实现吧. 因为我觉得代码就足够具有表现力了. )

### 三点一线的生活
我记得曾经有一种非常文艺地表现自己生活单调贫瘠的说法: 宿舍 - 食堂 - 教室, "三点一线的生活". 这里就体现了一个过程: 从一个状态变化到下一个状态. 每一个状态下都会执行相应的动作. 做完了动作就会移动到下一个状态. 

虽然不愿意这样说, 但是这样的生活就是一种简单的"计算机". 稍微用形式化的书写来表达就是: 

```text
宿舍: 摆大烂 -> 食堂
食堂: 吃饭   -> 教室
教室: 上课   -> 宿舍
```

我们会发现, 将我们的状态首尾相接, 就组成了一个类似于环状的结构. 于是就像是土拨鼠之日一样, 我们就处于一个循环的状态. 

![状态转移图]({{ site.github.url }}/_img/computer-toys/turing-machine/01.png "我们管这样的图叫做状态转移图, 用来直观地表现状态之间的转移关系.")

### All Works And No Play...
但是除了上课, 我们实际上还有放假啊. 假如说每天你在宿舍里都会检查一下今天的日程表, 假如期盼的假期终于来到, 那么你就打算一直在宿舍里面摆大烂. 除非你的美好假期在光影如梭中消失不见. 

这个时候, 我们做了一个读取信息的操作. 于是上面的故事就需要稍微修改一下了: 在寝室, 读取日历, 如果是"weekends", 就在寝室一直摆烂; 如果是"weekdays", 就去食堂... 那么形式化的写法就是: 

```text
寝室, read weekends: 摆烂 -> 寝室
寝室, read weekdays: 摆烂 -> 食堂
食堂, read #: 吃饭   -> 教室
教室, read #: 上课   -> 宿舍
```

(其中为了格式一致, 我们在原来的食堂和教室后面也加上了`read`的标志. )

这个时候, 我们就相当于实现了一个条件判断. 在原来的基础上, 我们通过条件的分支在原来的基础新加入了一个循环(双循环可还行). 

![状态转移图]({{ site.github.url }}/_img/computer-toys/turing-machine/02.png "注意观察里面有两个环状的结构, 也就是在'寝室'这个分支节点上生长出了两个循环的结构.")

### 开始与结束
其实我们的人生并不是这么可怕的土拨鼠之日的循环的, (像这样的无限循环能否迎来终结之日的问题, 叫做图灵停机问题, 比如说是Alan Turing为了研究哥德尔不完备性定理想出的一个东西. 这里我们就不必这么高大上地介绍了. ) 一件合理点的事情, 总该是有开始, 也总该是有结束的. 同理, 我们的"计算机"也应该有开始和结束. 

让我们加入一点点的新的状态: 

```text
开始(开学), read enrollment: 去学校 -> 寝室
寝室, read weekends: 摆烂 -> 寝室
寝室, read weekdays: 摆烂 -> 食堂
寝室, read schoolend: 回家 -> 结束
食堂, read #: 吃饭   -> 教室
教室, read #: 上课   -> 宿舍
```

请思考这两个状态有什么用. ~~我不会告诉你我在图片上面加了一个注释, 里面写了一些我自己的理解的.~~

![状态转移图]({{ site.github.url }}/_img/computer-toys/turing-machine/03.png "现在我们有了两个特殊的状态叫做开始和结束, 对于这两个状态, 一个是一开始就处于的状态, 另外一个这是处于结束状态的时候, 我们就会一直停止在这个状态, 也就是所谓的停机问题. ")

### 那么能不能把你做了什么说得更加清楚一些呢? 
其实上面我做了什么实在是有点, 怎么说呢, 太抽象了. 假如计算机能够理解的话, 估计人工智能系的那帮人要肝好久了. 那么我们就写得详细一点吧: 

什么是摆烂呢? 就是在日程表上写下"时光飞逝, 我在摆烂", 然后看着时间流逝把你的日程表从此刻拨到下一刻: 

```text
寝室, read weekends: write "时光飞逝, 我在摆烂", move next_schedule -> 寝室
```

那么这个时候, 我们相当于就已经实现了一个"完整"的图灵机了. 虽然格式不太严格, 但是总归来说还算是该有的都有了. 那么总结一下: 
* 我们的所有操作都是在一个"日程表"(或者也可以叫纸带什么的)上进行的
  * 我们可以从日程表上读取`read`数据
  * 我们也可以在日程表上写入`write`数据
  * 在每次操作(读和写)后, 我们可以选择在日程表上移动, 也就是对应着一个日程表上的位置. 
* 我们操作以及操作的规则是由我们的状态和读取的规则来确定的, 就好像是在`寝室`这个状态, 读取到的`weekends`的数据, 就决定了我们的`write`和`move`的具体的行为. 

那么假如我们想要制作一个图灵机, 我们只需要制作一个能够按照规则, 从纸带上读取, 写入, 移动, 然后转换状态的机器即可. 

## 用Ruby制作一个图灵机
为了简单, 我们将会把上面的例子里面的图灵机做一个简化, 把命令形式改写成如下形式: 

```text
状态, 读取, 写入, 移动, 下一个状态
```

并且限制我们的图灵机每次只能够读取和写入1个字符. (就是为了体现一个未经世事的图灵机的感觉. )

### 读取并识别规则集
我们肯定希望能够每次读取一个已经写好的规则(因为怎么说呢, 图灵机的规则输入还是挺麻烦的, 最好是能够保存在一个文本文件里面, 然后每次载入一个规则集. )

那么我们就写一个读取的函数: 

```ruby
def read_file(path)
  # 返回的指令集为 instructions
  instructions = {}
  # 读取文件的每一行
  File.open(path).each_line do |line|
    # 去掉空白符和换行符
    line = line.strip.gsub(" ", "").split(",")
    # 只选择符合条件的命令输入
    if line.length == 5
      # 多赋值, 只是一个小技巧而已
      key, sub_key, *value = *line
      # 初始化状态
      instructions[key] = {} unless instructions[key]
      # 写入读取的规则
      instructions[key][sub_key] = value
    end
  end
  # 返回处理过后的数据
  return instructions
end
```

假如我们讲读取的规则储存在`instructions = read_file("code.txt")`中, 这样我们就可以通过`write, move, next_state = instrcutions[state][read]`这样的方法来读取写入的数据, 移动的方向和下一个的状态名称了. 

### 执行机器
我们不妨设计这样的一个机器, 它初始化的时候, 处于`"q0"`状态, 也就是我们的开始状态. 初始化的时候会读取一个规则集. 然后初始化的时候, 我们会有一条纸带. (这里不妨设纸带一开始就是空的). 

```ruby
class Machine
  # 新建一个图灵机, 默认纸带为空
  def initialize(path, string = "")
    # 规则集
    @instructions = read_file(path)
    # 初始化
    set(string)
  end

  # 设置纸带上原本就有的内容, 初始化
  def set(string)
    # 初始状态为 "q0"
    @state = "q0"
    # 初始在纸带上的位置
    @head = 0

    @strings = {}
    # 将纸带写成{:"0" => "B", :"1" => "B"}的形式
    # 如果输入是空的话, 就变成不处理了
    string.length.times.each do |i|
      @strings[i.to_s.to_sym] = string[i]
    end if string != ""
  end

  # 从文件中读取规则集
  def read_file(path)
    # ...
  end
end
```

注意到上面我们定义了纸带的储存方法, 我们定义假如纸带上读到了没有写数据的地方的话, 就返回一个特殊字符叫做`"B"`, 这个字符可以表示这里什么也没有. 于是我们就可以构造`read`和`write`命令. 不过先把这个东西放在一边, 我们先构造一个最基本的规则实现的函数吧: 

```ruby
def step
  # read() 返回 @head 所在的位置的 @strings 上的数据
  write_char, move_to, @state = *(@instructions[@state][read()])
  # write(char) 会在 @head 所在的位置写入 char
  write(write_char)
  # move_to(forward) 会根据 forward 来移动 @head
  move(move_to)
end
```

然后我们根据上面的规定来写出一点点的读写代码: 

```ruby
def read
  # 把 @head 转换为 @strings 中的标签
  head = @head.to_s.to_sym
  # 假如没有数据, 就是 "B"
  if @strings.include? head
    return @strings[head]
  else
    return @strings[head] = "B"
  end
end

def write(char)
  @strings[@head.to_s.to_sym] = char
end

def move(forward)
  # 只有两种移动方式, 向左, 向右, 否则就报错
  if forward == "L"
    @head -= 1
  elsif forward == "R"
    @head += 1
  else
    raise "Error when moving! "
  end
end
```

于是我们就完成了最基本的图灵机的部分了. 

在irb中尝试一下, 其中的`code.txt`里面是一个将等号前面所有的`1`移动到等号后面的一个图灵机程序, (虽然我们课上叫它一进制加法, 乐). 

<details>
<summary> 其中, 我们的图灵机代码储存在"main.rb"里面, 而我们的"code.txt"里面是一个将等号前面所有的"1"移动到等号后面的一个图灵机程序, 虽然我们课上叫它一进制加法, 乐. (点击展开查看) </summary>

{% highlight text %}
q0, +, +, R, q0
q0, =, =, R, qa
q0, 1, e, R, q1
q0, E, E, L, qa
q1, 1, 1, R, q1
q1, +, +, R, q1
q1, =, =, R, q1
q1, B, 1, L, q2
q2, 1, 1, L, q2
q2, +, +, L, q2
q2, =, =, L, q2
q2, e, 1, R, q0
{% endhighlight %}
</details>

```ruby
3.0.0 :001 > load "main.rb"
 => true 
3.0.0 :002 > m = Machine.new "code.txt", "11+111="
 => #<Machine:0x0000000120866138 @instructions={"q0"=>{"+"=>["+", "R", "q0... 
3.0.0 :003 > m.step
 => 1 
```

坏了, 我们忘记了给这个东西写一个漂亮的输出了! 

现在补救还来得及, 但是既然准备上手了, 为什么不试试看给它来点高级的功能呢? 

<details>
<summary>高级代码, 点击就送. (啊, 999999!!!! 你砍了我好多刀啊. )</summary>

{% highlight ruby %}
# step 的缩写, 是带有输出的step
def s
  step()
  puts output()
end

# 输出一个字符串
 def output
  # 防止 @head 在的地方什么也没有, 
  # 所以先读一次, 至少写一个 "B" 进去
  read()
  # 输出 @head 头的位置
  num = 0
  # 输出的值
  res = ""
  # 按照 key 的数值大小来排序
  @strings.to_a.sort_by{|item| item[0].to_s.to_i }.each do |item|
    res << item[1]
    # 找到了 @head 头的位置
    num = a[0].to_s.to_i if a[0].to_s.to_i == @head
  end
  # 输出一个指向符号, 也就是 @head 在的位置
  res << "\n" + " " * num + "^"
end

# 运行, 通过设置speed来调整速度
def run(speed = 1)
  interveral_time = 1.0 / speed
  while @state != "qa"
    s()
    sleep interveral_time
  end
end

# 运行直接到结束, 但是只在最后一次输出
# debug 来选择是否每次输出
def finish(debug = false)
  debug ? s : step while @state != "qa"
  puts output
end
{% endhighlight %}

</details>

重新在irb里面载入代码: 

```ruby
3.0.0 :004 > load "test.rb"
 => true 
3.0.0 :005 > m = Machine.new "code.txt", "11+111="
 => #<Machine:0x0000000115847d38 @instructions={"q0"=>{"+"=>["+", "R", "q0... 
3.0.0 :006 > m.s
e1+111=
 ^
 => nil 
3.0.0 :006 > m.finish
 => 
11+1111=111111
        ^ 
```

为了让这个程序看起来更加好一点, 你可以做一些更多的事情, 比如修改`inspect`之类的方法等等... 就是你的喜好啦. 

(又, 为了防止你照抄我的代码, 我在我的Ruby代码里面留了一些小错误. ~~其实是一开始直接在markdown里面写代码, 运行的时候调了调, 忘了自己有没有改过来了. 不过我不管, 这就算是检查你学没学会把. 乐.~~)

## What to do next? 
这个东西我还会继续写, 不过现在我将会结束这一部分了. 接下去我会介绍如何构造图灵机的代码, 以及如何将这个黑乎乎的命令行的玩意儿放到网页上. (虽然前面的例子里面就相当于是一个如何构造的例子了. )