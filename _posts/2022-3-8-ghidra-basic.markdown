---
layout: post
title:  "Ghidra Basic"
date:   2022-03-08 20:21:05 +0800
categories: ctf
---
# Ghidra Basic and Tiny Reverse
![Welcome Page]({{ site.github.url }}/_img/ghidra-basic/welcome.png "加载中...")

> A software reverse engineering (SRE) suite of tools developed by 
> NSA's Research Directorate in support of the Cybersecurity mission.    
> [ghidra site](https://ghidra-sre.org)

## 逆向工具 - 啥是 Ghidra 以及为啥用它
Ghidra 是一个 NSA(National Security Agency) 开发的一个逆向工具, 
可以将打包的程序(比如`.exe`, `.out`之类的东西)给拆开来看看里面的内容, 
这样就可以了解程序里面的运行逻辑. (然后就可以嘿嘿嘿了...)

<details>
<summary> 
点我看嘿嘿嘿:p 
</summary>

  <h3>逆向的个人理解</h3>

  不知道有没有人用过外挂, 破解软件还有注册机之类的东西, 
  嗯, 十分抱歉, 我用过的. QAQ, 不过现在基本上成为历史啦, 
  因为(钱包富裕了, bushi), 不, 是使用开源软件了. 

  啊, 扯远了. 就拿注册机来说明吧, 为什么逆向工程这么吸引我, 
  一个简单的例子: 

  {% highlight ruby %}
    print "Hello, what is your password: "
    get = gets.strip
    if get == "Lucky"
      puts "Yes! You're in. "
    else
      puts "Ah oh, something's wrong. "
    end
  {% endhighlight %}

  呀, 假如我们能够看到代码的话, 想要破解密码就是易如反掌的事. 
  这个就是逆向的一个想法吧: 得到一个程序, 然后将程序的逻辑进行分析, 
  最后将这个程序给 ban 了. 

  然而一般的什么要注册机的软件哪会直接吧代码给我呢... 稍微复杂一点的例子, 
  假如有一个被 gcc 编译的程序也就是一个二进制的程序, 然后就"看不到"代码了, 
  但是实际上并不是这样, 放到 ghidra 里面就可以看到反编译的汇编指令, 
  以及反汇编的伪代码, 等等. (甚至, 假如只是简单的编译的话, 还能看到代码呢, 
  比如说原来的程序里面的函数名称等等. )

  不过嘛, 知道会被这样干的开发商怎么可能会坐以待毙呢? 肯定会将这些函数名字删掉, 
  (stripped 的程序), 然后在程序里面加入混淆的代码, (就是恶心你这种逆向的人, 
  但是不怕, 我有耐心, TAT), 或者是用别的什么方式, 比如说虚拟指令, 又比如说, 
  给程序加壳(类似于压缩)等等. (虽然我不是很懂了, 毕竟才刚接触, 咳)

  大概的介绍就是这样了. (滑稽)

</details>

![色图]({{ site.github.url }}/_img/ghidra-basic/color-pic.jpg "点开上面, 看好东西. ")

嗯, 因为我现在不使用 IDA Pro 了, ~~真是日渐单薄的钱包啊~~, 主要是为了减少盗版的使用, 
真的是难啊. 虽然可惜的是, 两个都不是很会. 我只是记录一下我在使用 Ghidra 里面学到的, 
或者是用到的一些小技巧, 或者是说, 学到的一点点小玩具啦... 

非科班出身, 不过是乱搞的经验之谈. 

## Initilizing
去官网下载就好, 按照 [github](https://github.com/NationalSecurityAgency/ghidra) 
上面的说明, 准备好[JDK](https://adoptium.net/releases.html?variant=openjdk11&jvmVariant=hotspot),
然后执行`ghidraRun`的脚本. 

(注: 假如你不知道什么是`JDK`, `JRE`, `JVM`的话, 可以试试去 Google 一下, 
我提供一个一点点的解释: `JDK` 是 Java Develop Kit 的缩写, 
`JRE` 是 Java Runtime Environment 的缩写, `JVM` 则是 Java Visual Machine
的缩写. 不过说了这么些应该是很难懂的... 但是假如玩过我的世界的 Java 版的话, 
嗯, 应该是清楚的. )

(Tips: 可以把这个脚本扔到macOS的shortcut里面, 然后以后就可以快速打开了. 呃, 
其他系统随意? )

打开软件, 会看到一个确认文件, 确认就好啦. 

对于 ghidra, 它的思路是基于项目的, 一个项目里面可以有很多的文件, 
可能是为了处理那种大型的程序用的吧? (咳, 还真是高看我了. 我就只能将乱七八糟的, 
什么一堆程序丢到里面去, 用完了之后再删掉... )

一开始打开的时候就只有一个空界面, 选择`File-New project`就可以创建项目了, 
创建完项目后就可以将要分析的程序拖进去分析啦. 

![window]({{ site.github.url }}/_img/ghidra-basic/window.png "随便扔了一个最近在做的, 不会啊...")

双击文件可以打开`CodeBroswer`, 来看看里面程序里面到底卖的是什么药. 
对于第一次打开的程序, 会问问你是不是要分析这个程序. (当然啦, 干他! 
虽然目前还不是很会, 但是我直接就选择了`Analysis all`, 嘛, 一劳永逸嘛. 
基本就是试试. 以后估计会学一点更深入的东西了. )

![CodeBroswer]({{ site.github.url }}/_img/ghidra-basic/codebrowser.png "这个是一个stripped程序, 所以里面看不见什么函数名字, 只有FUN_00100b6a这样的奇怪地址, 然后程序代码也挺无所谓的. ")
观察窗口的主要组成部分: 左边的是一些东西, 中间是一些东西, 然后右边又是一些东西, 
嗯, 解释完毕, 拜拜. (bushi

(左边的是一些程序里面的符号表, 类型表等等, 中间是一个反汇编的代码, 
可以理解为将二进制内容转换为了汇编语言以及数据字段等等. 右边的是对反汇编的反编译, 
毕竟汇编语言不是什么让人喜欢的东西. 下面是命令的输出窗口, 呃, 没怎么用过. )

其实一般对于那种没有`stripped`函数, 往往是可以通过查看左边的`Symbol Tree`来看看突破口的, 
但是嘛, 这次的程序是`stripped`的, (也就是从程序里面删除了对应的函数符号表), 
所以 ghidra 只能分析出一些函数的结构, 然后将程序的函数用地址来命名. 
虽然可以通过将一个个的函数都看过去, 但是(好麻烦)这样非常的不切实际(虽然以前我就是这样的)
有时候遇到超级多的一坨函数, 而且还是`stripped`了的话, 就不能了. 

后来我想到了一个妙招: 程序运行的时候会有输出, 那么看看输出的字符串在哪里, 然后反过来找函数, 
就行了. (没想到这个方法是一个很基础的方法, 咳, 亏我没有虚拟机的时候还真的是一个个分析的, 
真是傻啊)

在 ghidra 里面找程序中的字符串只需要这样: 在菜单栏的`Search-Program Text`就可以找字符了. 
因为我比较菜, 所以用的是`All Fields`, 或者可以精确一些, 选择`Selected Fields`, 
然后在里面选择对应的类别. 

![search]({{ site.github.url }}/_img/ghidra-basic/search.png "查找字符串, 别的功能还没用过")

找到了字符串, 就可以利用`XREF`, 也就是在中间`Listing`窗口(汇编和数据)的窗口里面, 
在对应的数据边上会有像是注释一样的东西: 

![xref]({{ site.github.url }}/_img/ghidra-basic/xref.png "看看右边的XREF")

所谓的`XREF`就是`Cross reference address`的标志, 稍微翻译一下就是引用了这段数据, 
或者是调用这段函数的其他函数等等. 在上面的图片里面, 我们可以看见绿色的`XREF`, 
后面跟着调用这个数据的函数`FUN_00100b6a`, 这个时候就可以双击这个`FUN_00100b6a`, 
然后 ghidra 就会跳转到对应的地址. (其实 ghidra 里面的大部分东西都可以双击, 
然后跳转到对应的地址里去)

这样就可以得到(可能的)函数地址啦! 然后就可以开始试试通过看看这个函数里面有什么, 
然后就可以将这个程序进行一个逆向了. 

## 丑陋的代码
嗯, 假如我们得到了主要的函数(上面的`FUN_00100b6a`的函数就是了), 
不过我还没有那么牛, 也没有那么有耐心, 不太能够有心情去搞汇编的逆向. 
但是 ghidra 有一个很好的功能就是可以将汇编的东西反编译成类似于 C 语言的伪代码, 
这样就很人性化了. 

![decompile]({{ site.github.url }}/_img/ghidra-basic/decompile.png "就是右边的东西啦")

虽然现在的代码变得像是 C 代码一样的东西了, 但是里面的东西实在是, 反人类, 
就跟我朋友写的随意换行, 随意命名变量, 能不用空格就死不用空格的令人心脏骤停的 C 代码, 
咳. 这样的分析可真的是令人窒息. 

虽然简单的代码可能会比较能够理解, 但是还是可以解决的. 很容易分析得到里面的结果. 
但是假如遇到那个很庞大的项目, 很坑爹的长长的函数的时候, 死磕的方式就不太适合了. 

但是 ghidra 可以对反编译的结果进行一个修改, 还可以进行一些批注等等, 
进行一个对内容的美化吧: 

* 将光标停在符号上面(比如变量名, 函数名), 按下快捷键`L`就能修改符号的名字, 
  (或者是右键`Rename Local Variable`, 这就可以修改名字了)
* 将光标停在符号上面, 右键`Retype Variable`就能将符号的类型修改, 
  (或者是用快捷键, 在 mac 上是`CMD-L`)
* 按快捷键`;`可以给代码做注释, 方便理解程序里面的内容之类的东西. 

不过, 这样一般还是很难理解 C 的代码, 除非它是真的简单. 不过, 我还是比较喜欢把这个伪代码
进行一个抄写, 转换成我比较喜欢的 ruby 的代码, 这样会看起来舒服一点. (好吧, 
就是我个人的 xp 系统啦. )

这样一般就能够解决我目前遇到的问题了. 

<details><summary> An example </summary>

来一个简单的例子: 先写一个程序. 

{% highlight C %}
#include <stdio.h>
#include <string.h>

int main()
{
  int cmp;
  char passwd[6];
  printf("I'm Lucky. \nWhat is your password: ");
  scanf("%5s", passwd);
  cmp = strcmp(passwd, "Magic");
  if (cmp == 0)
  {
    printf("Yes. \n");
  } else {
    printf("No. \n");
  }
}
{% endhighlight %}

虽然怎么说呢, 这个代码非常简单, 并且非常容易被破解, 用 ltrace 或者干脆 strings 
命令就可以将密码得到了. 呃, 不管, 我的编程水平可差了. 毕竟是业余的. 
<br>
用 gcc 编译完了以后再放到 ghidra 里面打开, (我没有删除符号表, 所以会简单一点)

{% highlight C %}

undefined4 entry(void)

{
  char *in_x1;
  char acStack30 [6];
  int local_18;
  undefined4 local_14;
  
  local_14 = 0;
  __stubs::_printf("I\'m Lucky. \nWhat is your password: ");
  __stubs::_scanf("%5s",in_x1);
  local_18 = __stubs::_strcmp(acStack30,"Magic");
  if (local_18 == 0) {
    __stubs::_printf("Yes. \n");
  }
  else {
    __stubs::_printf("No. \n");
  }
  return local_14;
}
{% endhighlight %}

啊, 坏了, 因为我为了偷懒, 所以直接用 mac 的 gcc 进行了一个编译, 
可能和 linux 的结果不一样, 也可能和 Windows 的结果不一样. 
并且还是 m1, 所以和 Intel 的 mac 的结果也是不一样的. 不过逆向的平台多样, 
很多时候我没得选. (其实就是懒), 直接美化就好了: 

{% highlight C %}
int main(void)

{
  char *in_x1;
  char passwd [6];
  int res;
  int ret;
  
  ret = 0;
  __stubs::_printf("I\'m Lucky. \nWhat is your password: ");
  __stubs::_scanf("%5s",in_x1);
  res = __stubs::_strcmp(passwd,"Magic");
  if (res == 0) {
    __stubs::_printf("Yes. \n");
  }
  else {
    __stubs::_printf("No. \n");
  }
  return ret;
}

{% endhighlight %}

现在至少看起来好看一点了, 对程序的结构也比较好懂了. 
(虽然实际的问题会难亿点点, 不过大体上差不多了. )

</details>

## 鸽ing~
上面的大概就是一个简单的教程了, 先写到这里. 感觉可能是不如 IDA 热门吧, 
又或者是还不够强大, 网上的教程数量感觉比较少. 待我出去看看有没有什么好资料, 
学会一点点之后再补充. 

## 拓展
找到了一个看起来比较有用的文章(实际上应该是相当有用了, 因为很多都是我不会的, 
嗯, 学, 都可以学. )

下面就类似与一个笔记(或者摘要的东西了), 原本的网址如下: 
[原版网页第一篇](https://www.shogunlab.com/blog/2019/04/12/here-be-dragons-ghidra-0.html), 
[原版网页第二篇](https://www.shogunlab.com/blog/2019/12/22/here-be-dragons-ghidra-1.html), 
[翻译](https://bbs.pediy.com/thread-257445.htm). 

### 文件管理
* 在项目窗口`File`选择`Batch Import...`可以批量导入文件
  (虽然我都是鼠标拖放导入的... )

### 主要窗口
* `Program Tree`里面可以查看程序的各个 sections, 嗯, 据说这对于PWN十分有帮助
* `Symbol Trees` 里面是程序导入, 导出, 还有函数, 标签, 类, 命名空间. 
* `Data Type`窗口里面是程序里面用到的数据的类型(可以查看 Show reference)
* `Listing`窗口里面可以查看反汇编代码
  (可以通过右上角的 Edit the listing fields 来修改 Listing 窗口的排版内容)
* `Window -> Function Graph` 可以显示函数的图形模式(类似于流程图一样的东西)
* `Window -> Function Call Trees` 可以显示函数调用了什么其他的函数. 
* `Window -> Fuction Call Graph` 可以显示函数调用其他函数的图像(类似于流程图)
* `Decomplie`反编译窗口
* `Window -> Script Manager` 可以查看, 使用脚本(这些脚本可以用来帮助逆向)
  (虽然我还没有用过... 不过看起来非常的香. 可以运行 python 的脚本, 
  但是需要 Jython. )

### 一些操作
* 在符号上面右键可以`Show References to`(快捷键`<C-S-F>`), 
  可以看到函数的被调用关系. 
* 在符号上面双击可以跳转到符号所指向(大概)的地址, 比如查看储存在内部的字符, 
  或者是 call 的函数等等. 
* 在`Listing`(或者`Decomplie`窗口)右键点击数值可以选择`Convert`来转换数值格式, 
  比如选择十进制`Unsigned Decimal`, 十六进制`Unsigned Hex`或者别的什么的. 
  (感觉有点像是改变显示方式)
* 在`Listing`里面看到放在内存里面的一堆数据, 比如说一堆字节数组, 
  可以选中, 右键, `Data -> string` 就能够变成字符串的类型. 
  (或者也可以在这段数据的头部也可以右键修改)
* `Window -> Memory Map` 内存映射, 可以设置反汇编的二进制文件的装载的基址
  (没用过, 不是很懂. )

### 帮助
* Gihdra 有一个叫做 Tip of the Day 的东西, 在启动的时候会给一些提示, 
  还是挺有用的. (尤其是在学习的时候)
* `F1`帮助: 在大多数的界面里面(呃, 假如你不确定的话, 用鼠标点一点, 
  就相当于聚焦在这个界面上了)按下`F1`, 就会打开一个对于这个界面功能说明的
  帮助文档. 

## 更多的资源
嗯, ~~全球最大同性交友网站诚不欺我.~~ 果然, 以后想要学什么东西, 
先去找找那些`awesome-xxx`的项目, 里面的资源实在是十分丰富. 

先留下[链接](https://github.com/AllsafeCyberSecurity/awesome-ghidra), 
然后以后再慢慢看... 
