---
layout: post
title:  "Why's (poignant) Guide to Ruby"
date:   2021-11-18 18:06:32 +0800
categories: jekyll update
---
# Why's (poignant) Guide to Ruby
假如你不知道`_why`, 那很正常, 因为这本来就应该是他的愿望. 
(也许是他的愿望吧, 因为有一天他删除了自己的所有`github`
以及所有的社交账号, 然后就不见了. )

实际上, 我之所以会选择学习`ruby`的很大原因就是因为他的教程. 
很有趣也很厉害. 和市面上的随处可见的按部就班的编程教材不同, 
他的书更像是一种故事书和漫画书. 与其说是在教我写代码, 
不如说更像是在看一个有着宏大世界观的故事, 进入到了一个代码
(哦不, 应该是计算机的)世界. 

唯一的缺点是, 这本书没有翻译, 并且在`_why`消失了之后, 
其他人最多的贡献就只是把它更新到了新版本`ruby`的部分语法, 
但是这个新版本在现在看来也很旧了. 

(虽然很旧, 但是没关系, 因为`ruby`是向下兼容的, 笑. )

假如我要翻译这部大作的话, 我觉得这是不可能的. 
因为我做不到, 也很可能没有时间. 

网上有一个人好像在尝试翻译这本书, 
给出[链接](http://codecly259.github.io/poignant-guide-cn/), 
然而, 读着的感觉很怪, 并且还没翻完. 
英文原版是在[这](https://poignant.guide/book). 

我现在有一个小目标, 因为这本书实际上是我在很早之前读的, 
(但是没有读完), 所以我现在发现自己的`ruby`代码写得不够酷, 
所以回过来读一读`_why`的作品, 顺带记录一下这个学习心得. 

不是翻译, 更像是做注记, 或者是乱改? 

开了一个天坑的感觉...

## Expansion Pak No.1
(贴心提示: 本文是有图片的, 虽然我没有翻译, 
而是直接引用了原来的图片地址, 假如你看不到的话, 
请等一等, 没准就出来了. )

![tiger has vest, tiger likes girl robot, earth crashing into the sun](https://poignant.guide/images/tigers.vest-1.gif "阿珍爱上了阿强, 穿背心的老虎爱上了机器女孩. 哦, 不! 地球它在撞向太阳! 而我们却依旧选择歌唱... ")

首先要安装`ruby`, 这里我不使用`_why`的说法, 
因为在`macOS`自带的`ruby`里面有很多坑爹的地方, 
所以一般建议是安装`homebrew`
(这样之后就相当于把`macOS`变成了一个假)

或者安装`rvm`再安装`ruby`. 

(我去, 这也太麻烦了. 结论, 直接用? 
不是, 实际上还是值得这般折腾的. 因为以后会用到. )

对于`windows`, `ruby installer`超级香的. 
虽然我记得也有许多的坑的样子, 但是很无脑. 
[链接](https://rubyinstaller.org)
(强烈建议安装带`devkit`的, 因为某些用到`C`的`gem`需要它. 
当然, 简单使用的话是没关系的. )

打开命令行, 在`macOS`上, 这个叫`Terminal`. 
输入`ruby -v`就可以得到`ruby`版本, 
因为`-v`是`--version`的一个缩记. 

我的`Ubantu`的虚拟机上的输出是: 
```
>>> ruby -v
ruby 3.0.0p0 (2020-12-25 revision 95aff21468) [aarch64-linux]
```

> # About Ruby Versions
> 现在我手上的`ruby`版本是`3.0.0`, 
> 据说已经放出了`3.1.1_preview1`的发布
> [news](https://www.ruby-lang.org/en/news/2021/11/09/ruby-3-1-0-preview1-released/). 
> 真好. 
> 
> 仔细观察就可以发现在`ruby`的版本号里面有三个数, 
> 分别是**主版本号 (the major version)**, 
> **次版本号 (the minor version)**, 还有
> **小版本号 (the teeny version)**. 
> (我承认, 我瞎翻译的. )
> 每一次`ruby`被重构, 主版本号就会递增一位. 
> (each complete rewrite of Ruby)
> 每一次有比较大的改动(sweeping changes)的话, 
> 次版本号就会递增. 小版本号大概会每个月睡着一些零碎的修改, 
> 以及一些bug的修补然后放出. 
> 
> 并且`ruby`迭代的速度很快, 基本上放到
> **Ruby-Core mailing list**
> 上的很棒的想法往往在几天之后就会被引入`ruby`中. 
> 
> (据说Matz, `ruby`之父, 是一个很高产的人)
> 
> (据说`ruby` 的 `3.x` 版本比`2.x`快了将近 **3** 倍. )

![tiger saves the earth with ice gun. girl robot zoomed around tuxed shop...](https://poignant.guide/images/tigers.vest-2.gif "老虎用激冻枪救下了地球. 在那小小的西服店里, 美丽的机器女郎在徘徊...")

(花心老虎?! 还是纯情老虎? 这拯救地球也太扯了. 魔幻现实主义. )

`ruby`带了一个超级超级强力的工具 -- **Irb**, 
也就是**ineractive ruby**, 直接翻译过来就是交互`ruby`终端. 

(我认为这个可能来源于`lisp`的`repl`
**READ-EVAL-PRINT-LOOP**的想法... 只是猜想, 不一定对. )

在终端里输入`irb`就可以进入: 
```
3.0.0 :001 > 
```

在`irb`中, 你可以输入`ruby`的代码, 然后轻轻地一敲回车, 
你的代码就会跑起来了. 

比如说, 在窗口中输入一点点算数: 
```
3.0.0 :001 > 3000 + 500
 => 3500 
```

尽管在这行代码里面, 我们完全没有把运算的结果和变量关联在一起, 
或者是把变量打印到屏幕上, 但是这样是完全OK的, 因为这可是`irb`啊, 
它可以帮我们直接把返回值输出返回给我们. 

某种方面来说, `irb`可以算是一个很好的计算器: 
```
3.0.0 :002 > ( ( 220.00 + 34.15 ) * 1.08 ) / 12
 => 22.873500000000003 
3.0.0 :003 > "1011010".to_i( 2 )
 => 90 
3.0.0 :004 > Time.now - Time.local( 2003, "Jul", 31, 8, 10, 0 )
 => 577642071.301868
```

(就像是`_why`先生在第一章就倡导的那样, 
编程语言不应该只是"电脑的语言", 而应该是写程序的人的语言, 
我们可以读出编程语言, 就像是在读一门很简单的外语一样. )
所以`002`的代码就不过是一点点的数学: 
`220.00`加上`34.15`的和乘以`1.08`再除以`12`; 煎蛋. 
`003`的代码的意思是: 
把这一串`01`内容的二进制字符串按照`2`进制变成整数(**i**nt). 
`004`的代码做的事情是: 
把现在的时间和当地时间`2003/6/31 8:10 AM`相减, 
就会得到相差的时间(单位是秒). 

### 读一读前面的提示符(prompt)
(不知道是版本的问题还是我的问题, 
我手上的`ruby`的`irb`的提示符和`_why`的是很不一样的. 
所以我就看看我自己的说了. )

仔细观察命令行输入前面的标志: 
```
3.0.0 :005 > 
```

可以发现, 有三个部分: `3.0.0`是**版本号**, `:005`是行号, 
也就是记录了我们在`irb`里面输入了多少行, 
第三个部分就是` >`, 不要觉得看起来很寒酸, 实际上有很多的玄机. 
比如说在里面输入字符串: 
```
3.0.0 :005"> "a string I haven't finish...
```
就会发现前面的出现了一个`"`, 表示现在有一个`"`字符串没有闭合, 
很贴心啦. 同样的还有正则表达式的输入, 等等.

并且`irb`还会贴心地为你进行缩进和简单检查一下代码是不是正常的, 
比如我在输入`hash`的时候, 如果忘了加逗号: 
```
3.0.0 :012 > hash = {
3.0.0 :013 >   :a_key => :a_value
3.0.0 :014?>   :an_other_wait_something_is_wrong
```
就会出现一个`?`符号来表示`irb`可能不是很理解. 

(并且不知道是不是我自己设置了别的什么, 现在的`irb`自带语法高亮, 
看起来就很酷. 但是`python`没有, 所以应该不是我的错吧?)

### 水一点的提示符
如果你觉得前面的提示符有点烦, (极简主义者), 没问题, 
我挺你, 让我来告诉你一点点`hacking`小提示. 

(退出`irb`的方式很简单, 只要输入`exit`函数就好. )

因为`irb`里面包含了一些别的提示符的形式, 
所以不妨试一试在命令行里面输入`irb --prompt simple`, 
于是, 你就会得到一个简单的提示符. 
```
>> %w(my best friend's arm)
=> ["my", "best", "friend's", "arm"]
```
(哇哦, 北欧简约风, 笑. 这不就是某宝的推销词嘛...)

`ruby`还有很多别的提示符的选项, 除了`simple`, 
还有`xmp`(是一个完全没有提示符以及一个带缩进的输出). 
并且还有更绝的`null`, 完全没有, ~~真空出行~~. 
想要尝试这样的花样`irb`, 你只需要把`prompt`修饰符的输入改掉就好
(`irb --prompt null`). 

当然, 假如你觉得还不够强, 你可以自己来定制. 

(你只需要黑进`irb`, 敲敲手指, 吹吹口哨...)

`irb`运行中有一个`conf`对象, 它控制了`irb`的配置文件
(Irb's configuration settings), 
里面有部分的选项可以用来控制提示符. 让我们看看: 
```
>> conf.methods.grep /prompt/
=> [:prompt_i=, :prompt_s=, :prompt_c=, :prompt_n=, :prompt_mode, :prompt_mode=, :prompt_s, :prompt_c, :prompt_n, :prompt_i, :prompting?]
```

然后你就可以来上手了: 
```
>> conf.prompt_i = "%3n :> "       # 改变普通模式的提示符
  2 :>                             # 好耶
  3 :> conf.prompt_s = "%3n .%l "  # 改变字符串模式的提示符
  4 ." "a string mode... wow       # wow
  5 :> conf.prompt_c = "%3n .^ "   # 改变代码块模式的提示符
  6 .^ 5.times do
  7 .^   print "wow"
  8 .^ end
  9 :> conf.return_format = "    => %s\n" 
                                   # 改变输出符
 10 :> 4+4
    => 8                               
```

上面的`%3n`和`%l`的作用就是一种替代的作用, 
相当于形成一个模版, 让`ruby`把信息填充到这个模版里面, 
很简单的: `%3n`就是把行号按长度`3`填充到字符串中, 
`%l`就是用来把输入字符串开头的符号打印到提示符中.

假如你还想要更高级的花活, 建议去读一下
[文档](https://ruby-doc.org/stdlib-3.0.2/libdoc/irb/rdoc/IRB.html)

(这里`_why`给出的[链接](http://www.rubycentral.com/book/irb.html)挂了. )

### Tab补全
`irb`里面有一个超级好用的功能就是`tab`补全. 
```
irb --readline -r irb/completion
```
(这个功能好像现在已经被默认开启了, 所以上面的代码也许是没必要的. )

简单的来说, 假如你帅气的一敲`tab`键, 
`irb`就会帮你补全你的想要输入的代码(~~基本上靠猜~~)
(实际上就是补全): 试一试输入`[].col`然后再按下`tab`, 
然后`irb`就会贴心地帮你补充成`[].collect`, 
并且将你的光标移动到输入的最后, 让你可以继续编辑. 
(比如`[].collect_concat`等等之类的. )

但是假如有很多的匹配的时候, 
你可能就会觉得`irb`就不知道你想说什么了, 
确实, 假如你只是输入了一个: 
```
>> 42.
```
这时候想要按`tab`弹出一些补全的话好像有点没道理吧. 
(这就好像是你对这一张白纸, 按下`tab`键, 
然后祈祷出来一张写满大一统理论的论文一样. )

但是等等, `irb`还是可以的. 只要你再按一次`tab`键, 
也就是第二次`tab`时. 
`ruby`会给你一个(完整的)可以用来补全的列表来让帮助你填写. 

(这里之所以把"完整的"加了括号, 因为我发现还是有一点点不一样的, 
在`_why`给出的`irb --readline -r irb/completion`模式中, 
双次点击`tab`会得到一整个列表, 甚至还会因为列表太大了, 
会询问一下你要不要展开(向下展开). 而在正常模式下, 
这样的点击两次会向上展开列表, 展开的大小和终端的窗口大小有关, 
建议实际操作一下就知道了. )

```
...
42.untaint
42.untrust
42.untrusted?
42.upto
42.yield_self
42.zero?
3.0.0 :001 > 42.
```

(上面的是我在`irb`运行的结果)

所以现在试一试在`irb`中输入`Kernel::`然后再双击`tab`, 
锵! 你就得到了所有的核心命令. (core methods) 
记住这个小技巧, 然后经常使用的话可以带来很多的便利. 

![except the robot flew away and the ice gun went on and on](https://poignant.guide/images/tigers.vest-3.gif "但是机器女郎飞走了, 留下激冻枪一直地喷射...")

好的, 你现在就可以尽情享受和这美妙技术的二人世界了, 
我不会再打扰你了. 哦, 不, 还有一件事忘了说了! (超大声!!!)
(超越超大声的超超大声!!!!) 那就是: 

<h1 style="font-size:84pt; color:#FDD; line-height: 120%;text-align:center;">((<span style="color:#A53;">ri</span>))</h1>

(哦不, 喊的太大声了)

### (Ruby’s Own 411 or 555-1212 or Yes, Operator, Get Belgrade on the Line—I'll Be Right Here—Just Plain Hammering The Pound Key Until Someone Picks Up…)
*"你好, 是Ruby 114 接线员吗? 请将我连上线 -- 嗯, 好的, 我接上线了 -- 请保持接通并等待对方接听. "*

于是Ri接起了电话: "你好, 这里是Ri. 请问是什么类, 注意加上冒号."

于是你赶忙说: "你好接线员, 我在找一个实例函数. 
是`Enumerable#zip`"

(你在命令行里面输入了一条命令: )
```
ri Enumerable#zip
```

(假如不能运行, 比如显示`no matches found: Enumerable#zip`
的话, 没关系, 看下面的注记. )

不一会儿, 超迅速的, `ri`会给出一个结果: 
(*so swiftly that even the cat perched atop cranes his neck around, gapes and hands it the royal cup Most Blatantly Great Thing Since Michael Dorn*
老实说, 这里的翻译我有点吃不消. )
```
--------------------------------------------------------- Enumerable#zip
     enum.zip(arg, ...)                   => array
     enum.zip(arg, ...) {|arr| block }    => nil
------------------------------------------------------------------------
     Converts any arguments to arrays, then merges elements of _enum_
     with corresponding elements from each argument. This generates a
     sequence of +enum#size+ _n_-element arrays, where _n_ is one more
     that the count of arguments. If the size of any argument is less
     than +enum#size+, +nil+ values are supplied. If a block given, it
     is invoked for each output array, otherwise an array of arrays is
     returned.

        a = [ 4, 5, 6 ]
        b = [ 7, 8, 9 ]

        (1..3).zip(a, b)      #=> [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
        "cat\ndog".zip([1])   #=> [["cat\n", 1], ["dog", nil]]
        (1..3).zip            #=> [[1], [2], [3]]
```

> Note that shell quoting or escaping 
> may be required for method names 
> containing punctuation:)
> 
> 摘自`man ri`

(这里要加一句话, 就是建议在查询的时候用引号来扩住查的东西, 
就像这样: `ri "Enumerable#zip`. 
原因是这样可以防止特殊符号作祟. )

实际上`ri`就是一个无删减的`ruby`查询服务 -- 哪里不会点哪里 --
不要告诉我你从来没听过这样的好东西. 
(又是一个翻译不来的东西: 
no-money-down lifetime-supply-of-proper-explanations
虽然原文应该是表示自己的比喻超级有趣的样子. )
(参考翻译: 太快了以至于甚至是坐在顶端的猫都伸长了他的脖子, 
目瞪口呆地看着并把皇家奖杯递给它
-- 这是自Michael Dorn 以来最清楚不过的伟大的事)

假如你想要得到任何类(class)以及它全部的方法(methods), 
只要轻轻地输入: `ri Class`, (`Class`是你要问的类的名字, 
比如`ri String`). 不要紧张, 这可不是什么让你穿上宇航服, 
啪地一下把你打到太空中去开拖拉机的艰难的活. (笑) 
这简直就像是最温柔的耳边细语, 告诉你这个类的特性. 

但是你想要类的方法(class method)的信息? 没问题: 
`ri Class::method`. (比如`ri "Nokogiri::HTML::parse"`)

你想要类的实例的方法(instance methods)的信息? 没问题: 
`ri Class#method`. (比如`ri "String#to_i"`)

(等等, 如果你是一个新手的话, 还不知道什么是类, 什么是实例. 
那我建议去读`_why`的书, 也许我会继续翻译. 这里给一个很形式的说法: 
类的方法`Class.method(arg)`是这样调用的, 类的实例的方法是
`instance.method(arg`这样调用的. 
类的实例是`instance = Class.new`这样生成的, 
`new`是一个类的方法, 返回类的一个新的实例. )

假如你想要全部的类的信息, 比如说来一个清单, 列出所有的可查的类, 
没问题, `ri -l` (`-l`对应的是`--list`). (唯一的问题是, 
可能你会得到一个又长又长的列表, 估计可以从珠穆朗玛峰列到地心. )

是不是觉得命令行里面看到的纯文本格式不太好看? 没问题, 
你可以选择输出`HTML`格式: 
```
ri -Tf html String#gsub > gsub.html
```

**或者可以用彩色格式输出**
```
ri -Tf ansi String#gsub
```

(这个的输出就会一下子输出所有的`ri`文档, 这个的效果不是很友好. )

### Into the Ri Switchboard
走进`ri`. 

`ri`之所以能够如此美妙地用人话, 
轻松地来解释这些你以为可能会奇怪的程序语言, 
是离不开Dave Thomas最初的贡献的. 
(Dave Thomas是Programming Ruby的作者, 
同时也是在美国的`ruby`的推广者, 
许多的`ri`中的精心撰写的文字实际上就是部分直接来源于
Programming Ruby. 不要忘了常常谢谢Dave. )

(学一点历史很重要, 不仅在物理里, 在计算机里也很重要. )

`ri`的那些优雅的文档实际上就是从`ruby`的源代码中生成的. 

(不过我发现`_why`选择的例子已经没了, 因为试一试就知道了. 
那为什么不换一个呢? 因为我觉得没必要, 不要被例子限制了你的想象. 
实际上例子的形式很好的. 重点是形式. )

假如有这么样的代码在`ruby`的`date`类里面: 
```ruby
# Get the time of this date as [hours, minutes, seconds,
# fraction_of_a_second]
def time() self.class.day_fraction_to_time(day_fraction) end

# Get the hour of this date.
def hour() time[0] end

# Get the minute of this date.
def min() time[1] end
```

然后在终端里面我们输入: `ri Date#time`
```
-------------------------------------------------------------- Date#time
     time()
------------------------------------------------------------------------
     Get the time of this date as [hours, minutes, seconds,
     fraction_of_a_second]
```
`ri`会自动处理并帮你生成对方法的工作原理的一些解释, 
但是它实际上是希望编写代码的作者最好留下一点注释
来解释一下自己的代码. 最好的建议就是, 在每一次你写代码的时候, 
当你定义了类或方法的时候, 最好加上在前面加上一点点的简短的描述, 
然后在以后就可以生成`ri`的文档来方便阅读和理解. 

(深有感触, 感觉没有注释的代码读起来真的很累, 明明是自己昨天才写的, 
今天读起来就跟读天书一样... 并且注释可以直接变成文档, 
这个想法还可以直接就减少写文档的工作量, 妙啊. )

你可以用一些特殊的记号来让你自己的描述的格式更加的好看. 举个例子, 
如果你在自己的代码的注释里面用`*`或者`-`开始的话, 
在输出的文档(HTML)里面你就可以看到很好看的(无序)列表格式. 
```ruby
# Get the time of this date as an Array of:
# * hours
# * minutes
# * seconds
# * fraction_of_a_second
def time() self.class.day_fraction_to_time(day_fraction) end
```

还有其他的一些简单的例子: 假如想要有序号的列表的话, 就可以用
`1.`开头(也就是用数字加上一个点号开头). 强调的文字被`_`下划线包围,
加粗的文字被`*`星号包围, (行)代码被`+`包围, 
样例就是简单的一块带缩进的文本
(或者说是加上缩进就会用像`markdown`里面的代码块的形式显示). 

下面是`_why`的项目`RedCloth`里面的一段代码: 

(钩沉: 可以在`github`上看到`_why`的镜像. 哇, 不愧是`_why`神, 
有口皆碑, 他的镜像的名字叫做
**A mirror of _why's executable poetry**, 
诗一样的代码. )

```ruby
#
# Returns a new RedCloth object, based on +String+ and
# enforcing all the included +restrictions+.
#
#   r = RedCloth.new( "h1. A <b>bold</b> man", [:filter_html] )
#   r.to_html
#     #=>"<h1>A &amp;lt;b&amp;gt;bold&amp;lt;/b&amp;gt; man</h1>"
#
def initialize( string, restrictions = [] )
    @lite = false
    restrictions.each { |r| method( "#{ r }=" ).call( true ) }
    super( string )
end
```

(这段代码是`initialize`方法的一部分, 注意这里的缩进的部分, 
实际上就是样例部分, 可以说是很简单了. )

欲知后事, 速来读**RDoc**的**Markup**章节. 
(这里给出`_why`的
[链接](http://rdoc.sourceforge.net/doc/files/README.html)
, 目前还是可以用的. )

### Pushing Out Your Own Ri
发布你自己的`ri`文档(`RDoc`). 

虽然`ri`不会自动地生成, 但是你可以通过告诉它在你需要在哪里启动, 
然后它就会很贴心地运行了, 只要两步: 
```
cd ~/cvs/your-code
```
首先把你的目录切换到你的(项目)代码所在的文件夹
(一个项目一个文件夹有一个好处就是可以防止文件混乱). 
```
rdoc --ri-site
```
接下来利用`rdoc`工具就可以生成`rdoc`信息, 
接下来你就可以`ri YourClass`来看到你的`rdoc`文档了. 

或者你可以直接通过
```
rdoc
```
来直接生成`HTML`的文件, 
你可以通过查看`index.html`来看看自己的好东西. 

当你看到这行字的时候, 恭喜你, 你已经踏进`ruby`的世界了. 

![the tiger finds a new home and learns to eventually move on](https://poignant.guide/images/tigers.vest-4.gif)

### 本章后记
不知道`_why`前面的漫画有没有什么用意, 
我觉得可能是为了说: 
看我们的`irb`, 就和激冻枪一样强力; 
但是假如没有好的文档的话, 我们可能就像是瞎来的老虎一样, 
所以 **let us move on into the ruby world** :p. 