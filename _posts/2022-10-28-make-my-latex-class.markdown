---
layout: post
title:  "Boook: 制作自己的 LaTeX 文档风格"
date: 2022-11-03 22:56:21 +0800
categories: misc
---
# Boook: 制作自己的 LaTeX 文档风格
关于我为什么制作 LaTeX 文档 Class? 好吧, 其实就是闲的蛋疼. 
虽然网上已经有很多很好看的现成的 LaTeX 类了. 
(比如 [Elgantbook](https://github.com/ElegantLaTeX/ElegantBook),
后面的文章很多是建立在这个的基础上的. 另一个值得参考的是
[这个](http://tutex.tug.org/pracjourn/2005-4/hefferon/hefferon.pdf)).
以及, LaTeX 的一些[文档](http://latexref.xyz/index.html) 等等, 
都是很好的参考. 

但是作为一个技术, 学了总是不会吃亏. 
(虽然学的这个行为就已经是吃亏本身了. )

不过好在网上这类的教程感觉都不是很详细, 大多数的教程都是教你如何下载包, 
使用现成的包和代码. 虽然这样很方便, 也很新手友好 (大概). 但是, 
想要更进一步就有一些为难了. 

(Tips: 一个很好的方法就是看别人的代码, 然后改. 
虽然我看了代码之后发现了一个很痛苦的事情: 那就是, 
可能是因为我的境界不够, 所以会觉得别人的代码很混乱吧... 
我tmd根本看不懂啊!!! )

## 基本
虽说是自己写, 但是也不全然是自己写, 更像是写一个整合器, 
把别人的已经写好的代码重新调用, 然后按照自己喜欢的风格来, 
最后得到一个新的结果. 

(Note: 虽说是基本, 但是默认已经学会使用简单的 LaTeX 操作了, 
毕竟这些在网上到处都有. )

### Class 的基本结构
我的目标是要做到能够在写文档的时候, 只需要: 

```latex
\documentclass[lang=cn]{boook}
\title{the Boook}
\begin{document}
I am Lucky. 
\end{document}
```

就能够有一个很好的主题. 这个时候, `document` 制定了文档的类型, 
也就是我要写的 Class. 一般它会被存放在 `.cls` 文件里面. 

而一个 `.cls` 文件, 其开头类似于: 

* Identification: 声明自己是什么

```latex
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{boook}[2022/10 ver.book boook]

% codes...
```

  大概就是在说: 我的文件需要的 `TeX` 版本是 `LaTeX2e`, 
  我的类名称叫做 `boook`. 目前的版本是 `2022/10` 的 `ver.book`, 
  以及对应的一些说明. 

  > Note: 有一个非常奇怪且离谱的错误, 就是在后面加了 `[]` 
  > 之后, 编译可能会通不过, 这个时候你可以试试看删掉它. 
  > 虽然一开始我确实出现过了这个问题.  
  > 你可以参考这个 [问题(stackexchange)](https://tex.stackexchange.com/questions/274650/missing-inserted-for-ifnum)
  
* Preliminary declarations: 加载 Package 等等的东西, 
  定义命令和其他的一堆可选项. 比如 `LoadClass`, `RequirePackage`. 
* Options: 定义一些类的选项, 比如 `DeclareOption` 之类的. 
* 更多的定义, 比如一些使用的操作, `newcommand`, `renewcommand`. 

然后接下去就要对这个类里面有什么进行一个说明. (当然, 是用代码). 

### 简单的代码
一般来说, 代码好像并不会特别多和特别复杂. 唯一麻烦的地方就是, 
LaTeX 的代码特别没有美感. 写起来非常的让人心灰意冷. 
代码上没有标识符和注记符, 格式和形式上也有特别麻烦的要求... 

所以就只需要知道一些最基本的代码, 然后剩下的就在实际使用中看着办吧: 

* 加载已经写好的 Package, 类似于 Ruby 的 `require 'gem_name'`.  
  对于调用的 Package, 可以在 [CTAN](https://ctan.org) 上找文档. 

```latex
\RequirePackage{PackageName}
```

* 定义新的命令, 以及重新改变原本的命令. 

```latex
\newcommand{\commandName}[numberOfArgs]{code}
\renewcommand{\commandName}{code}
```

* 条件判断, 名字里面一般带一个 `if`... 
* 啊, 好麻烦啊, 还是直接开始吧. 

## 设计
嘛, 设计才是老大. 虽然我不是很懂设计, 但是我想要一个什么垃圾东西, 
多少心里还是有点数的. 

![boook]({{ site.github.url }}/_img/pieces/boook.png "好吧, 这个设计一点也不出彩, 甚至很平庸, 不如直接用现成的. ")

至于之后的一些细节就留给之后吧. 

## 开始
因为我也没有见过很好的代码结构(阅历太少), 所以只能够按自己的规范来了, 
自己胡乱建立一个规范结构, 让之后的我再修改吧. (加油吧, 之后的我. )

于是我的结构是这样的: 

```latex
% ----- header -----
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{boook}[2022/10 version.book boook]

% ----- load class -----
\LoadClass{book} % use book as default

% ----- requirements -----
\RequirePackage{
  kvoptions, % for key value option
  etoolbox,  % for more function ablilities
  % more packages
}

% ----- options -----

% ----- commands -----

```

这里载入了两个 Package, `kvoptions` 和 `etoolbox`, 
用于简化配置的书写, 比较方便. (分别在 CTAN 上的链接: 
[kvoptions](https://ctan.org/pkg/kvoptions), 
[etoolbox](https://ctan.org/pkg/etoolbox))

并且使用 `book` 类 (latex 自带的类型) 来提供默认的缺省, 
可以不必所有都自己设置 (不然就太累了. )

> Note: 关于 `book` 类, 
> 可以去查看在 CTAN 上的[文档](https://mirror-hk.koddos.net/CTAN/macros/latex/base/classes.pdf). 
> 看了那个文档之后, 我只能说下面的都可以不用看了, 
> 我写的都是渣渣, 不如直接看官方文档, 会比较规范上手. 

> Note: 将 `LoadClass` 的类提前到 RequirePackages 的前面, 
> 是因为像我这样写 RequirePackage 的话, 没有给这些包默认值,
> 如果在 LoadClass 前面调用包就会出现闹人的报错提醒: 
> `! Package geometry Error: \paperheight (0.0pt) too short.`.
> 所以让它们闭嘴的最好办法就是提前给一个预设, 
> 之后的 code 部分就是在预设上进行编辑和拓展了. 

接下来就是 `option` 的设置, 因为经常会看到的: 

```latex
\documentclass[lang=cn, math]{boook}
```

通过方括号传递的参数可以一定程度上修改文档的属性. 
虽然一开始不应该添加太多的选项, (因为不方便编写), 
但是实际中就是有这么多的选项和需求... 

不过前面导入的两个 Package 中的 `kvoptions` 可以帮忙简化设置, 
(虽然也没有简化到哪里去就是了). 

比如说要对多语种进行一个支持, `lang=en`, 或者是
`lang=cn` 都可以. 那么就可以设置:

```latex
% ----- options -----
\SetupKeyvalOptions{                        % set up kvoptions
  family=BOOOK, prefix=BOOOK@, setkeys=\kvsetkeys}
\newcommand{\ekv}[1]{\kvsetkeys{BOOOK}{#1}} % quick binding

\DeclareStringOption[en]{lang}              % {default}{name}
\DeclareVoidOption{en}{\ekv{lang=en}}       % valid k-v pairs
\DeclareVoidOption{cn}{\ekv{lang=cn}}

\DeclareDefaultOption{
  \PassOptionsToClass{\CurrentOption}{book}}
\ProcessKeyvalOptions*\relax
```

第一个命令初始化了 `kvoptions` 的操作, 
而第二个命令为 `kvoptions` 里面的 `kvsetkeys` 命令做了一个快捷链接. 
(就是为了方便输入而已, 因为名字太长了. )

接下来是对一组 key - value 来进行一个设置, 这里通过 `DeclareVoidOption`
来设置有效的输入对, 用 `DeclareStringOption` 来声明默认值. 

最后的两个代码语句是用来告诉程序, Option 的设置已经完成了, 
接下去就要把已经设置好的东西载入到 `book` 类中. 
(因为是在基本的类进行的拓展开发)

这样在后面的代码部分就能够通过逻辑判断来根据不同的输入来改变代码逻辑: 

```latex
% ----- code -----
\ifdefstring{\BOOOK@lang}{cn}{
  % codes
}{\relax}
```

因为使用了 `kvoptions` 包, 所以访问变量的时候通过 `\BOOOK@lang`
就能够找到那个变量. 差不多这样. 那么继续. 

> Note: 在进一步之前的一个小提示.  
> 为了方便你确认自己真的是修改了, 并且修改对了代码, 一个简单的做法就是: 
> 写一个 `.tex` 文件然后用它来调用你的 `.cls` 文件. 然后看看效果.  
> 不过高手估计不会这么做, 毕竟太蠢了, 没有技术含量. 

### A Little Tricky Chinese
乐, 对中文这样的字符输入, 实际上并不太友好, 需要稍微设置一下才能够好用. 
那么对于中文, 需要做的就是引入 `ctex`
([CTAN](https://ctan.org/pkg/ctex)) 来支持中文的输入: 

```latex
\ifdefstring{\BOOOK@lang}{cn}{
  \RequirePackage[UTF8, scheme=plain]{ctex}
  \xeCJKsetup{AutoFakeBold=true}
}{\relax}
```

这里通过 `schem=plain` 的方式来把 `ctex` 中自带的信息给去掉了,
(为了下一步能够更好地自定义. )

接下来设置相应的字体 (我对字体没什么感觉, 所以用的就是 ctex
中的设置), 或者可以用 `fontset=none` 来删除 ctex 的预设值, 
然后自己写字体对应关系. 下面是 Elegantbook 中的一段设置:

```latex
\RequirePackage[UTF8, scheme=plain, fontset=none]{ctex}
\setCJKmainfont[BoldFont={FZHei-B01},ItalicFont={FZKai-Z03}]{FZShuSong-Z01}
\setCJKsansfont[BoldFont={FZHei-B01}]{FZKai-Z03}
\setCJKmonofont[BoldFont={FZHei-B01}]{FZFangSong-Z02}
\setCJKfamilyfont{zhsong}{FZShuSong-Z01}
\setCJKfamilyfont{zhhei}{FZHei-B01}
\setCJKfamilyfont{zhkai}[BoldFont={FZHei-B01}]{FZKai-Z03}
\setCJKfamilyfont{zhfs}[BoldFont={FZHei-B01}]{FZFangSong-Z02}
\newcommand*{\songti}{\CJKfamily{zhsong}}
\newcommand*{\heiti}{\CJKfamily{zhhei}}
\newcommand*{\kaishu}{\CJKfamily{zhkai}}
\newcommand*{\fangsong}{\CJKfamily{zhfs}}
```

虽然可以替换, 那么还是懒得搞那么麻烦了, 目前作为一个自学的尝试, 
先简单一些. 

<details>
<summary> 更多的中文的设置 </summary>
<p>
虽然一般来说, 上面那么多的设置应该就够用了, 
但是如果你观察生成的文档的话, 就会发现一些 "关键词", 
比如 Contents 之类的并没有被处理. 
所以一个简单的想法就是把他们翻译了: 
</p>

{% highlight latex %}
%% commands for the particular label name specific
%% to different languages
\ifdefstring{\BOOOK@lang}{cn}{
\renewcommand{\contentsname}{目录}
\renewcommand{\listtablename}{表格目录}
\renewcommand{\listfigurename}{插图目录}
\renewcommand{\thepart}{第\zhnumber{\arabic{part}}部分}
\renewcommand{\figurename}{图}
\renewcommand{\tablename}{表}
\renewcommand{\bibname}{参考文献}
\renewcommand{\appendixname}{附录}
\renewcommand{\indexname}{索引}
\renewcommand{\proofname}{证明}
}{\relax}
{% endhighlight %}

<p>
虽然但是, 如果你有一些特殊的爱好的话, 想要保留不翻译这些, 
可以写一些代码来跳过, 比如设置语言是 mix 之类的. 
</p>
</details>

### 文档尺寸
嗯, 不能忘了, 我是要一个 b6paper 尺寸的小书呢. 
在声明文档尺寸的细节前, 可能还需要了解一下有哪些是需要声明的. 
使用的是一个叫做 `geometry` 的包 ([文档链接](https://mirror.aarnet.edu.au/pub/CTAN/macros/latex/contrib/geometry/geometry.pdf)). 

![geometry]({{ site.github.url }}/_img/pieces/geometry.png "这个还挺直观好理解的")

所以需要声明一下文档的尺寸: 

用 `RequirePackage` 增加一个 `geometry` 的 Package, 
然后在 `code` 部分增加下面的代码. 

```latex
\geometry{
  paper=b6paper,
  inner=25mm,
  outer=15mm,
  top=15mm,
  bottom=10mm,
  headheight=8mm,
  headsep=5mm,
  footskip=7mm,
}
```

(Note: 关于文档的尺寸, 可以参考这个
[链接(papersizes.io)](https://papersizes.io/b/))

### 封面
里面可能可以不是很好看, 但是至少封面要酷吧. 
那么放一张图片在封面上, 然后加上一些文字, 
装模作样认为自己还算是设计了个能看的封面了吧. 
と、そんな感じ. 

这个时候就需要重新设计一个命令 `maketitle`: 

```latex
\renewcommand*{\maketitle}{ %
\begin{titlepage}
  \newgeometry{margin=0in}
  \parindent=0pt
  \includegraphics[width=\linewidth]{cover.png}
  
  \begin{huge}
	\@title
  \end{huge}
\end{titlepage}
}
```

解释: 
* 用 `\renewcommand` 来重新定义在 book 中就已经定义过的命令;
* 用 `titlepage` 环境来定义一个封面环境. 
* 在封面环境中, 取消普通的对齐 (因为封面需要不一样的边距, 
毕竟大家应该都想要一个没有边框的封面). 
* 去掉段落前面的缩进 (否则的话就会看到图片有一个位移)
* 使用 `graphicx` 包 (需要在前面的 RequirePackage 部分载入,
CTAN [链接](https://ctan.org/pkg/graphicx)) 中的
`includegraphics` 命令载入封面图片, 这里的图片宽度和文档同宽. 
* 然后再加上一个大号的标题. 

于是这个时候, 就做好了一个最基本的, 超级丑的一个. 
因为太丑了, 所以还是不放上效果图了. 

不过从这里开始就可以使用 git 进行版本控制了, 
虽然 git 可能不是很好学, 但是可以有效防止你在狗屎一样的代码里面折腾. 

#### 封面的初步美化
* **让图片大小确定**:  

  ```latex
  \includegraphics[
    width=\linewidth,
    height=0.85\textheight,
  % keepaspectratio
  ]{cover.png}
  ```

虽然这种把图片大小写死, 一旦用的图片大小不合适的话, 
就会被强制缩放的操作是很傻逼的, 但是我目前还不知道能无伤缩放的方法.

不过对于完美主义者, 可以将代码里面的 `keepaspectratio` 打开,
或者改变使用的图片的大小. ( 其实可以用 options 的方法来做, 
之后可以增加. )
* **让标题和在下方处于一个合理的位置**:  
因为一开始只是用了一个 huge 的 title, 看起来就很丑, 
* 所以一个简单的方法就是让它在下方的上方居中:  
使用 `\vfill` 命令可以添加一个自动的高度上的空格来在竖直方向上居中,
而同理, 使用 `\hfill` 命令可以添加一个自动的宽度来在水平方向上居中.
( vertical, horizonal )

	```latex
	% center vertiially
	\vfill
	\@title
	\vfill
	```
	
	或者
	
	```latex
	% center horizonally
	\hfill \@title \hfill
	```
	
	> Note: 注意因为 LaTeX 的特性, 实际上这里的书写是否换行都是无所谓的,
	> 但是为了方便看代码, 所以我写成了这种鸟样子. 
	
	* 但是, 这样一般不会很高级. 所以可能还需要一些更加细致的操作, 
	这个时候, 就能够使用 `\hspace{}`, `\vspace{}` 等的命令来控制位置. 
	
	> Note: 实际上, `\hfill` 命令其实等价于 `\hspace{\fill}`.
	> 并且除了占位置, 这些 space 命令还能够接受负的长度单位, 
	> 来做到向负方向移动的目的. 
	
	不过如果是为了对齐的话, 还能够使用无边框的表格来达到目的: 
	
	```latex
	\begin{tabular}{l} % `l` for left align
	  subtitle\\
	  author name\\
	  % ...
	\end{tabular}
	```
	
	或者是使用 `minipage` 环境来构建一个独立的区域. 
	( 这个的操作给我的感觉就像是 HTML 里面的 `div` 了)
	* 大概这些操作就能够用来排版了
	* **其他的一些小小细节**:  
	* 页码: 一般来说, 对书本的封面应该是不会记录在正式的页码里面的, 
    但是目前的版本却会将封面记作第一页. 
	一个简单的操作就是给封面一个额外的记录页码的方式: 
	
    ```latex
	\pagenumbering{Alph}
	```
	* 链接: 如果你使用了像 `hyperref` 这样的包用来提供链接, 
    一个可能的操作就是在前面加入 `\hypersetup{pageanchor=false}` 
	来取消对封面的链接记录. 
	
	> Note: 关于 `hyperref` 的设置, 我参考的是 ElegantBook 
	> 里面的设置. 做了一点点小修改. 更多的设置可以看下面的
	> [目录](#目录) 部分的设置. 
	* 更多用户可以自定义的内容. 比如常常可以在 LaTeX 文档里面看到: 
    
	```latex
	\title{boook}
	\subtitle{the booooooooook}
	\author{凉凉}
	```
	
	这样的东西, 来自定义文档的信息. 
	
	一个简单的操作就是定义一个赋值命令, 然后在通过变量的访问来修改: 
	
	```latex
	% \global\let\@varName\@empty => 清空变量
	% \gdef{\@varName}{varVal} => 设置变量
	\newcommand{\subtitle}[1]{\gdef\@subtitle{#1}}
	% ...
	```
	* **搞点颜色**:  
	使用 `xcolor` Package ([CTAN](https://ctan.org/pkg/xcolor) 
	就能够帮助文档设置颜色. 
	* 添加颜色: 在文档中使用 `\definecolor{name}{RGB}{r,g,b}`
	就能够添加对应 `name` 名字的颜色.
	* 设置文字颜色: `\color{name}` 可以使用对应名字的颜色. 
	
	> Note: 有一些包, 比如 `xcolor-material` 之类的, 
	> 就会把一些颜色提前定义好, 用的时候只需要查手册, 
	> 然后输入名字即可, 很方便. (虽然我不用就是了)
	* **字体**:  
	这个目前先不管了. 因为我没有想法... 
	
	
	> Note: 为了防止你在 titlepage 中做得任何修改和设定影响到之后的页面,
	> 一个合适的做法就是在 titlepage 结束后加入几个命令: 
	> `\restoregeometry`, `\thispagestyle{empty}`, 
	> `\pagenumbering{abric}`. 

于是我们就能够得到一个最简单的封面: 

![titlepage]({{ site.github.url }}/_img/pieces/titlepage.jpg "虽然还有各种各样的小缺陷, 但是目前还是可以过得去的一个封面. ")

<details>
<summary>Some Codes (你可以在我的 Github 仓库里面找到这个代码)</summary>
{% highlight latex %}
\renewcommand*{\maketitle}{ %
\begin{titlepage} % specify the title page
\newgeometry{margin=0in}
\pagenumbering{Alph} % not counting title page into normal page
\hypersetup{pageanchor=false}
\parindent=0pt % no paragraph indent
\ifcsname @cover\endcsname % set cover image
\includegraphics[ % the best of the image size should be 1476*1870 300dpi
width=\linewidth,
height=0.9\textheight,
% keepaspectratio % force resize the image for better alignment
]{\@cover} 
\else % if user not provide cover image, will try using default cover image
\ClassWarning{boook}{No Cover Image provided, Trying on default cover.png}
\includegraphics[
width=\linewidth,
height=0.9\textheight,
% keepaspectratio
]{cover.png}
\fi
% below is the main title part
\vfill
\hspace{5mm}
\begin{minipage}[c]{0.65\linewidth} % title subtitle author
\begin{huge}\textbf{\ifcsname @title\endcsname \@title\else Title \fi}\end{huge}
\vfill
\hspace{2mm} \ifcsname @subtitle\endcsname \color{dimgray}\textbf{\@subtitle} \fi
\hspace{0.5mm}\color{lightgray} \textbf{|} 
\begin{small}\color{darkgray}\@author\end{small}
\end{minipage}
\hspace{7mm}
\begin{minipage}[c]{0.2\textwidth} % logo page
\vfill
\ifcsname @logo\endcsname
\includegraphics[
width=\linewidth,
height=0.2\textheight,
keepaspectratio
]{\@logo} 
\else
\ClassWarning{boook}{No Cover Image provided, Trying on default logo.png}
\includegraphics[
width=\linewidth,
height=0.2\textheight,
keepaspectratio
]{logo.png}
\fi
\vfill
\end{minipage}
\vfill
\vfill
\end{titlepage}
\restoregeometry
\thispagestyle{empty}
\pagenumbering{arabic}
}
{% endhighlight %}
</details>

### 目录
> Note: 你可以发现, 我设计的顺序就是按照一本书打开阅读的顺序来的,
> 虽然可能并不是非常的专业和严谨, 并且设计的顺序和代码的组织方式
> 我会尽可能地去做到. 不过万一遗漏了的话, 也没办法. 

对于目录, 没什么想法, 就希望能够有一个好看点的跳转链接: 
默认的跳转是一个红色的方框 -- 挺丑的. 于是就可以使用 `hyperref`
包 ([CTAN](https://ctan.org/pkg/hyperref)) 来自定义链接颜色. 

```latex
\hypersetup{linktoc=all, linkcolor=\color{black}}
```

设置了链接所有, 以及使用黑色来表示链接颜色. 

<details>
<summary>settups for hyperref</summary>
{% highlight latex %}
%% belowing code mainly from ElgantBook
\hypersetup{
breaklinks,
unicode,
linktoc=all,
bookmarksnumbered=true,
bookmarksopen=true,
pdfkeywords={BOOOK},
colorlinks,
linkcolor=darkgray,
citecolor=darkgray,
urlcolor=darkgray,
plainpages=false,
pdfstartview=FitH,
pdfborder={0 0 0},
linktocpage
}
{% endhighlight %}
</details>

### 接下来是各种各样的东西了... 我好累
到了目前这个阶段, 本来应该可以说基本能够用了, 
但是仔细一看 (指我把这个用到了我之前的那个一个项目
([magic the book](https://github.com/li-yiyang/magic_the_book),
不过这个学期应该是没有时间继续做这个项目了) 
上面的时候, 疯狂地报错, 因为缺少了很多的环境, 
比如练习环境之类的; 以及疯狂的溢出, 因为我的废话和代码都写得太长了,
b6paper 的小尺寸看来不太适合这个书籍的排版. )

#### Chapter Section 等标题的特性
参考的是 [stackexchange](https://tex.stackexchange.com/questions/94729/how-to-customize-chapter-heading-style)
上面的相应的问题, 使用的是 `titleformat` 命令. 
(来自 `titlesec` 包([CTAN](https://ctan.org/pkg/titlesec)))

```latex
\titleformat{⟨command⟩}[⟨shape⟩]{⟨format⟩}{⟨label⟩}{⟨sep⟩}{⟨before⟩}[⟨after⟩]
```

虽然但是, 我懒得做得很精致了, 所以我就直接抄了 `titlesec` [文档](http://tug.ctan.org/tex-archive/macros/latex/contrib/titlesec/titlesec.pdf)
的默认示例代码. 稍微修改增加了一些颜色的设置... 

#### 页码, 页眉
虽然但是, 这个时候我又开始懒了. 我的做法就是删除: 
一口气把能删掉的东西都删掉. 因为太麻烦了. 

使用 `fancyhdr` 包来删除掉原本带有的各种设置. 
主要是参考了其[文档](https://mirror-hk.koddos.net/CTAN/macros/latex/contrib/fancyhdr/fancyhdr.pdf)
中的一些代码: 

```latex
\fancyhf{} % remove former style
\fancyhead{} % clear all header fields
\fancyfoot{} % clear all footer fields
\ifdefstring{\BOOOK@geomode}{digital}{
    \fancyfoot[C]{\thepage} % for digital geomode, center the page number
  }{ % default
    \fancyfoot[LE,RO]{\thepage} % for default geomode, even page left, odd page right
  }
\renewcommand{\headrulewidth}{0pt} % no rule
\renewcommand{\footrulewidth}{0pt}
```

#### 图片的标题和对齐
我希望能通过尽可能简单的方法就能够往文档中插入图片, 
并且这样的图片满足比较好的性质. 比如能够通过: 

```latex
\image{image-path} % plain centered image
\image[caption]{image-path} % with caption
```

于是我的做法就是定义一个新的命令来实现, 
其中检测可选变量是否为空的方法来源于网络 (见代码注)

```latex
\newcommand{\image}[2][]{
\begin{figure}[!htbp]
  \centering
  \includegraphics[
    width=0.618\textwidth,
    height=0.147\textheight,
    keepaspectratio
  ]{#2}
  % test var if empty
  %% from https://tex.stackexchange.com/questions/53068/how-to-check-if-a-macro-value-is-empty-or-will-not-create-text-with-plain-tex-co
  \def\temp{#1}\ifx\temp\empty
  \else
    \caption{#1}
  \fi
\end{figure}
}
```

尽管还有一些不太美观... 但是目前就这样吧. 
假如用户需要自己添加图片的话, 可以通过: 

```latex
\begin{figure}
  \includegraphics[size options]{path-to-img}
  \caption{...}
\end{figrue}
```

#### 表格
一个简单的操作就是: 直接使用 `tabularx` ([CTAN](https://ctan.org/pkg/tabularx))
来添加表格. 为了方便我使用, 
我已经直接将其添加到了我的 RequirePackage 中了. 

#### 代码
作为一个普通的计算机爱好者, 来点代码肯定是一个必要项了. 
我希望能够通过简单的方法就能够给文档带来很便捷插入代码的方法. 

```latex
\begin{code}{ruby}
  until enough?
    # do something
  end
\end{code}
```

为了实现这个操作, 我使用的是一个叫做 `minted` 代码包. 
定义了一个新的环境: 

```latex
\newenvironment{code}[1]
 {\VerbatimEnvironment % use this to avoid other formatts distrubting
  \begin{minted}[tabsize=2,breaklines]{#1}}
 {\end{minted}}
```

注意到其中有一个操作: `\VerbatimEnvironment`, 
是用来防止其他的环境, 比如说 CJK 环境中的字体等等, 
让输出错误. 

不过实际上 `minted` 的表现感觉不是特别的好. 
比如没有办法让用户方便地自定义临时文件的输出文件夹. 

如果你使用 `-output-directory` 参数编译文档的话, 
会出现错误, 因为 `minted` 的默认输出文件夹不会跟着走. 

(好吧, 如果不是很在意的话, 可能也没关系了. )

并且在编译的时候, 需要带上 `-shell-escape` 的参数来编译. 

对于一般的用户, 可能这样的操作并不是一个刚需, 
所以我觉得还是为文档添加一个 `code` 的 option
可能会显得更加人性化. 

#### Tikz
因为最近还在学习 `tikz` 包的使用, 所以我将其也导入到了
RequirePackage 中了. 

## 后记
### 项目
可以在 [boook](https://github.com/li-yiyang/boook) 
这里找到 boook 的项目. 我应该尽可能地把这个项目写得比较规范了. 
如果觉得上面的写得跟狗屎一样, 还能够看看我狗屎一样的代码 :).

### 我不是大佬, 我是菜狗
在大学里有一个好处就是可以清晰且正确地认识到自己是个菜狗. 
那句话是怎么说的来着? 差生文具多是吧. 大概我也就是这样了. 

相比那些受过严谨的培训, 可以快速上手, 
并且愿意在某个项目上死磕的大佬. 我感觉我就像是一个耍耍小聪明,
走着野路子的三流的业余爱好者 (甚至可能都称不上).
(这点你可以从越来越短, 越来越肤浅的这篇文章看出... )

### 自动化: 一个不切实际的想法
作为一个懒惰的鸟人, 看到这个坑爹配置书写就很不爽, 
所以在写这些坑爹玩意的时候, 我脑子里真的是想着怎么样才能改变这个, 
比如像 Lisp 一样, 重新为它 wrap 一个形式. 

不过太菜了, 干不来. 只能写一个帮助生成的文档了... 

### 之后? 
应该会在有时间的时候更进的... 不过如果我没有什么别的特别的需求,
或者是没有什么别人的要求的话, 我觉得应该是不会更新了. 
就这样吧. 大学生精神状态可好了... 可好了...
