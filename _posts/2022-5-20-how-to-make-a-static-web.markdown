---
layout: post
title:  "How to Make a Website"
date: 2022-05-20 18:44:18 +0800
categories: jekyll
---
# 如何设计一个网页 ~~最好是不用动脑子的那种~~
(注: 本篇文章, 和老师上课讲的逻辑及方式完全不同, 所以, 可以说是我的一本歪经. )

## Design first and Code at the End. 
说实话, 其实说白了, 设计一个网页其实也就是一种和他人对话的方式. 我记得在三国演义里面, 就有一个经典的桥段叫做锦囊妙计: 出外作战的将士们打开军师的锦囊, 就像是军师站在他们面前, 然后挥舞羽毛扇, 从容不迫地对他们说着这般那般的计策. 

而我们其实要做的也差不多, 情况不过是把锦囊变成最后的网页, 而设计的步骤仍然是相同的. 我们需要考虑一个界面中应该有什么, 图片的显示应该是如何的, 界面与用户的交流应该是如何的... 当一切都规划完毕后, 我们就会得到一个对于全局的完整的把握. 这个时候, 制作网页不过是坐在电脑前的苦力活罢了. (但也不是说完全不重要. )

![original design]({{ site.github.url }}/_img/how-to-make-web/original-design-of-cards.jpeg "一开始的CARDs的设计稿, 可以发现里面有很多东西我后来都没有去实现, 因为懒, 并且也没有这么多时间去打磨了, 毕竟都花了一周了, 不能再继续投入了. ")

关于设计, 我应该是没什么资格说这些的. 毕竟, 你看, 我的博客用的就是jekyll的默认格式, 没有什么漂亮的设计什么的. 所以我会给一些推荐, 这些有一些是我在设计[CARDs](https://li-yiyang.github.io/CARDs)的过程中参考的资料. 

(注: 应该我不会再修改CARDs了. )

* [Apple Human Interface Guidelines](https://developer.apple.com/design/human-interface-guidelines) 其中我认为Themes里面有很多很有帮助的一些观点. 不得不说苹果很厉害. 
  * Clarity - 清晰: 在交互系统的设计中的元素要满足连续性的原理 -- 字体和字号的统一, 信息的传递要清晰. 
  * Immersive - 沉浸感: (这个感觉我做的东西没有达到...)
  * Target - 专一: 不要让很多的信息和操作让用户的操作分散. (我的CARDs好像也没有做到)
  * Accessibility - 无障碍: 一开始我在看苹果的SwiftUI教程的时候(后来因为太麻烦了所以弃坑了), 苹果官方的教程里面从第一讲就开始讲无障碍设计(VoiceOver朗读之类的东西...). 确实在某种程度上发现了新世界. 巧的是刚好那个时候我在新闻里面也看到了关于残障人士的专门读屏软件. (天哪, 新闻里面的软件界面真的是好丑, 宛若Windows 98的设计感... 然后看看苹果的无障碍... )
* 日常的设计, 这本书里面介绍了一些关于设计的想法, 可惜还没读完, 感觉里面的通过认知的过程来反向思考设计的方法的观点很有意思. 
* ...

最后贴一句我觉得说得很酷的话: 

> It's the users' experience that matters, and the user doesn't care how you created this.    
> Dylan Beattie

## 如果我要制作一个网页, 那么我需要知道什么
> 肉体, 精神和灵魂.    
> 《钢之炼金术师》

不必知道太多, 你只需构筑一个网页的肉体(HTML), 精神(CSS)与灵魂(JAVASCRIPT). 即可. 

![The simplest way]({{ site.github.url }}/_img/how-to-make-web/abc.png "接下来, 不是魔法, 而是科学. ")

### Overview
如果我们要将自己的设计转化为一个网页, 那么很显然, 我们第一步就需要将我们设计中的主要的内容实现. (抓住主要矛盾... ) 比如说, 我们想要一个显示蒙娜丽莎的网页, 那么我们的可能就想要一个框能够把图片放进去, 然后将我们的蒙娜丽莎给放上去. 好的, 我们的卢浮宫做完了. (对不起, 卢浮宫, 这只是一个例子. )

这个时候, 我们就需要使用HTML标记语言来帮助我们来描述我们的网页里面有什么东西. 而[HTML(Hypertext Markup Language)](https://developer.mozilla.org/zh-CN/docs/Web/HTML), 正如其名: 超文本**标记**语言, 通过一种叫做标签(tags)的方式来标记网页中出现的元素(elements). 

HTML的语言常常会有这样的形式: 

```html
<一种什么乱七八糟的东西>对, 没错, 这个乱七八糟的东西里面的内容就是我. </一种什么乱七八糟的东西>
```

还记得数学里面的括号么? 这东西就像是数学的括号, 不过高级一些, 它是个有buff的括号, 被括在里面的东西就会具有标签所指出的性质, 或者可以用来描述文档的性质等等. 

而一个标准的html的文档可能会有这样的形式: 

```html
<!DOCTYPE 魔法少女>
<魔法少女>
  <head>
    <title>巴麻美</title>
  </head>
  <body>
    <Muskettenschütze>Getrud</Muskettenschütze>
    <likes>Black Tea</likes>
  </body>
</魔法少女>
```

首先我们要告诉QB, 我们接下来要描述的是魔法少女(HTML). ~~她的头~~ (对不起) 标记了标题(title)是什么, 然后在主体中的内容又是什么... 

(对不起... 我不该这样的. )

好吧, 我不乱说了, (虽然对于熟知XML的读者来说, 这些东西应该还算过得去; 但是对于学姐的拥护者来说, 我可能要掉头了. ) 那么正经的HTML是什么样的呢: 

```html
<!DOCTYPE html>
<html>
  <head>
    <title>标题</title>
  </head>
  <body>
    <!-- 一些东西 -->
  </body>
</html>
```

好吧, 大差不差. 那么既然描述完了"肉体"的骨架, 我们手头就会有足够的元件来合成我们的整体了. 但是这些元件看起来并不是很好看, 总觉得平平淡淡的缺了点"精神", 用人话来说就是不够好看. 所以我们需要加上一些点缀, 来让我们的作品变得更加得好看. 这个时候, 我们就需要CSS(Cascading Style Sheets)了. 

[CSS](https://developer.mozilla.org/zh-CN/docs/Web/CSS), 层叠**样式**表, 正如其名字所说的一样, 就是一种用来描述我们的"骨架"究竟应该长什么样的东西. 比如说, (在我高中的时候, 常常会有同学写小说, 他们写的小说里面设定非常的完整, 技能啊什么的都非常完善. 可惜那个时候我还是个无聊的人, 缺乏了他们那样的高雅情操, 总之是有一些不够的. ) 我们设定了一个非常炫酷的角色, 她拥有炫酷的光学迷彩, 喜欢跳楼, 并且力大无穷, 可以徒手拆战车... 于是我们就会说, 她很有型(style). 

于是我们可以在HTML的标记的基础上加上新的描述的标签: 

```html
<motoko style="strength: 100%; opacity: 0%;">母猩猩</motoko>
```

嗯, 所以我们就得到了一只人畜无害的~~母猩猩~~少佐. 没错, 正如你所想的那样, 只要我们在标签里面填入信息就能够增加用来描述这个标签的信息了. 

(啊, 什么? 你说我又在乱说? 啊, ~~才没有这回事呢~~还真~~不~~是. )

那么正经的例子应该是什么呢? (你好无聊哦... )

```html
<div style="color: green; background: red;">
  我曾经的技术课上老师特别喜欢整的专用配色
</div>
```

<details>
<summary>因为太辣眼睛了, 所以折叠起来了</summary>
大概的效果是这样的: 

<div style="color: green; background: red;">
  我曾经的技术课上老师特别喜欢整的专用配色
</div>
</details>


嗯, 不难发现, 只不过是`属性: 属性值; `这样的东西而已嘛. 我的战斗力可是有九万万亿呢, 悟空. 

但是有时候你会发现, 这样一个一个地调整会太麻烦了, 没错, 程序员也发现了这个问题, 所以就有了一种能够选择一大类东西的方案, 叫做[CSS选择器](https://developer.mozilla.org/zh-CN/docs/Glossary/CSS_Selector). 只要: 

```css
/* 这是一条注释, 为了告诉你这段代码写的是CSS文件 */
.saiyajin {
  character: fighting-to-death;
  strength: 999999;
  tail: true;
}

.saiyajin:moon {
  mad: true !important;
}
```

于是就能够描述一类的对象. 即所有的标签中有`class`属性并且属性值为`saiyajin`的对象. 比如: `<div class="saiyajin"></div>`. 并且我们还用了一个`:moon`来描述了他们被月亮照到的状态(强制狂暴, `!important`是用来说明这个属性是非常强制的. ). 但是有时候我们可能又想要单独描述特定的一个个体, 比如, 一个叫做kakarotto的稍微特殊一些的对象. 

```css
/* CSS */
#kakarotto {
  strength: 99999999999;
  hair: black; /* 留给赛亚人的颜色不多了 */
}
```

于是我们的标签属性`id`为`kakarotto`的元素, 比如`<div id="kakarotto">`就会拥有这样的性质. 和前面的用`.`开头的标记不一样的是, 我们这里是用`#`来标记的. 没错, 这就是CSS选择器的语法咯. 但是如果你想直接对所有的`div`元素来修改属性的话, 那么我们可以利用这样的形式来修改: 

```css
/* CSS */
div {
  color: pink;
  font-size: 30px;
}
```

然后我们可以像这样将它们导入到我们的网页中: 

```html
<!DOCTYPE html>
<html>
  <head>
    <style>
      div {
        color: blue;
        font-size: 30px;
      }
    </style>
  </head>
</html>
```

![a demo]({{ site.github.url }}/_img/how-to-make-web/demo.png "大概就是这样子的一个对应关系, 如果你有兴趣的话, 可以吧这个做得更加完善一些, 不过我是没什么兴趣了. 不过这个歌倒是比较好听, 比较推荐, 在Apple Music上面可以找到. 我听的版本是: 命に嫌われている。 feat.相沢")

于是我们的网页就有了漂亮的皮囊了, 但是人不能只有好看的皮囊, 一个有意思的灵魂也是必要的. 如果想要设计一个射击游戏, 总不能只有亮闪闪的枪支模型吧, 还要让它们能够瞄准, 射击等等. 也就是说我们还需要为网页加上许多动作和逻辑. 

于是这个时候, [JAVASCRIPT](https://developer.mozilla.org/zh-CN/docs/Learn/JavaScript)就上场了. 我们可以用它来描述网页是如何运作的. 比如说我们想要触发板机: 

```html
<板机 onclick="fire();">巴雷特</扳机>
```

于是每当我们点击巴雷特的板机的时候, 就会执行`fire()`脚本. 

<details>
<summary>大概可以是这样的感觉</summary>
<div id="gun" onclick="document.getElementById('gun').hidden = true; document.getElementById('gun-fire').hidden = false; alert('fire!')">
  <img src="{{ site.github.url }}/_img/how-to-make-web/gun.png">
</div>

<div id="gun-fire" onclick="document.getElementById('gun-fire').hidden = true; alert('But what would the war bring us? Nothing...'); alert('And though you could refresh the window to see the gun again, but how could the world? ')" hidden>
  <img src="{{ site.github.url }}/_img/how-to-make-web/gun-fire.png">
</div>
</details>

这个大概就是所谓的JS脚本了. 它可以给我们加入充满灵魂的动作. 

但是同时, 问题也来了, 如果我们想要给多个元素加入类似的功能, 并且这样的功能非常的复杂(用人话来说就是有一坨代码), 那么和CSS类似的, 我们也能够使用相应的方法在一个地方定义一些函数, 或者别的什么的, 然后来让这些元素动起来. 

比如说: 

```html
<!DOCTYPE html>
<html>
  <head>
    <script>
      function turn_pink(min_length = 10) {
        var the_p_s = document.getElementsByTagName("p");
        for (var i = 0; i < the_p_s.length; i++) {
          if (the_p_s[i].innerText.length > min_length) {
            the_p_s[i].setAttribute('style', 'color: pink');
          }
        }
      }
    </script>
  </head>
</html>
```

可能就是一个不错的小玩具. 当然, 具体如何编程的细节这里我们不会展开介绍, 我们只需要知道每个部分都在干什么, 他们各自的语法大概如何即可. 见个面熟先吧. (又是个老梗了, 没吃过猪肉, 总见过猪跑吧. 虽然我觉得对于现代人来说, 应该反过来. )当我们见了面熟之后, 剩下的就是用我们的代码来极力拟合我们的设计, 然后学会如何查文档了. 

某种程度上来说, 我们可能永远也没法完全了解所有的HTML标签, CSS样式, JAVASCRIPT脚本, 但是我觉得也没有必要要完全了解. ~~毕竟有一种开发方式叫做面向谷歌开发~~, 因为我们有完整的文档可以查找. 比如: 

* [MDN](https://developer.mozilla.org/) 只能说是非常的齐全, 基本我觉得能想到的都在里面了. (虽然部分中文的翻译比较尴尬, 但是基本读英文的或者看看代码就够了. )
* [W3School](https://www.w3schools.com) 里面有很多的例子, 非常完善. 并且不只是HTML, CSS和JAVASCRIPT三件套, 还有很多的网页编程的东西. (不过我只能是用到的时候再去找. )
* 谷歌... (这个就不用多介绍了吧... )

虽然下面会介绍一些标签和样式之类的东西, 但是我认为这些只是杯水车薪罢了, 建议是水过一遍然后去查文档. 

## 如何看到网页的一个简单模型
![a demo]({{ site.github.url }}/_img/how-to-make-web/data-trans.png "客户端发送请求信息, 而服务器端针对请求返回数据, 而最终在客户端将数据渲染成漂亮的图形界面. ")

想象一下, 当你访问网页的时候, 你向服务器发送了一个请求信息, 这条请求信息通过网络从客户端(client)发出, 就像是潺潺流水一般流到服务器端, 然后服务器就啪地一下啊, 很是激灵(就像是摸鱼的时候, 突然听到有人叫自己的名字, 大脑响应并接管了这个中断, 然后转换了工作状态, 开始处理... ). 于是对你的请求进行了处理之后, 向你发送了一段返回信息. 而我们的计算机接收到了这段返回信息之后, 则会将其在本地进行处理并渲染成我们所见到的网页. 

在我们前面介绍的部分里面, 我们其实略去了如何发送请求信息, 如何响应请求, 如何返回请求, 如何渲染信息 -- 我们只需要思考, 如何构造一段**用来渲染的信息**. 而这样的信息, 就可以由我们上面所说的三种方式来"编码"描述. 

### HTML
在进一步介绍HTML之前, 我们不妨先稍微介绍一下请求和返回的形式. 它们的形式有点像是学生交作业一样的东西, 在开头写上自己的一些信息, 然后接着跟着一堆内容. 比如: 

<details>
<summary>因为太长了, 所以我决定折叠起来</summary>

{% highlight text %}
HTTP/1.1 200 OK
Content-Type: text/html; charset=utf-8
Content-Length: 26791
Last-Modified: Sat, 21 May 2022 03:47:22 GMT
Cache-Control: private, max-age=0, proxy-revalidate, no-store, no-cache, must-revalidate
Server: WEBrick/1.7.0 (Ruby/2.7.5/2021-11-24)
Date: Sat, 21 May 2022 05:20:07 GMT
Connection: close

<!DOCTYPE html>
<html lang="en"><head>
  <meta charset="utf-8">
  <meta http-equiv="X-UA-Compatible" content="IE=edge">
  <meta name="viewport" content="width=device-width, initial-scale=1"><!-- Begin Jekyll SEO tag v2.7.1 -->
<title>How to Make a Website | My Blog</title>
{% endhighlight %}

</details>

里面的内容我们可以(暂时)不用理解, 它们的形式就像是这样: 

```text
您的作业已提交 hahaha OK
接收时间: 五月二十一号

(我们的信息就在这里)
```

但是讲这些有什么用呢? 故事是这样的, 我们发送和接受信息的时候, 可能并不一定总是一帆风顺的, 假如你受到了一点点可怕的~~校园~~网络问题, 你可能会遇到一些严重的丢包问题, 你得到的信息可能会有一些"破损". 于是问题来了, 既然我们的HTML是用标签(tags)来标记的, 那么假如遇到了破损的标签, 没有配对的标签等等, 那么我们该怎么办? 

答案是: 忽略它们. 并尽可能地去渲染出一个结果. 所以哪怕你的HTML写得不是那么严谨, 但是至少, 它能够运行... (但是这并不是说我们就应该在自己的代码里面留下一堆恶意不闭合的tag然后考验游览器的耐心和能力). 

并且作为一种标记语言, HTML为了减少源代码对结果的影响, 采用了许多的方式, 比如在源代码中的换行符并不会在网页显示中展示出一种"换行"的效果(大概就是那种空格的效果, 建议试试看)

<details>
<summary>现在压力来到了tags</summary>
其实我挺讨厌HTML自带的那些tags的, 因为默认的排版虽然方便, 但是不好看. 就像是Markdown一样, 容易写, 但是不容易变得很帅. 比如说, 如果我想要在屏幕上放一个非常大号的字体, 通常的h1标签就不适合了. 这个时候, 你就需要用CSS去hack in这个tag, 然后写入各种各样的style. 可是这样是不是有点... 麻烦了呢? 

<br>

所以我就先介绍最普通的两个元素: div和span: 

{% highlight html %}
<div style="background: yellow;">
  This is div box. Yes, it is a box-like thing.
</div>
<span style="background: yellow;">
  This is something more like a line of words. 
</span>
{% endhighlight %}

大概是这个样子的: 

<div style="background: yellow;">
  This is div box. Yes, it is a box-like thing.
</div>
<span style="background: yellow;">
  This is something more like a line of words. 
</span>

<br>

这两个元素的区别就是显示和排版的方式: 是否是以一种块状的形式来排版. 就像是Office Words里面的的图片环绕方式一样. 

<br>

大概就是一种朴素的感觉吧. 

<br>

HTML里面有很多自带的特殊标签, 为我们实现了很多的效果, (至少比上面的朴素的div和span稍微有点不同, 虽然我觉得都挺丑...), 比如: 

{% highlight html %}

<h1>Large Title</h1>
<h2>Smaller</h2>
<p>This is a paragraph. </p>
<li>This is a list</li>

{% endhighlight %}

<div style="background: yellow;">
  <h1>Large Title</h1>
  <h2>Smaller</h2>
  <p>This is a paragraph. </p>
  <li>This is a list</li>
</div>

<br>

好的, 上面大概就是我用过的大部分的标签了, 如果你还想要了解更多的话, 请查文档. 
</details>

### CSS
正如前面所说的, CSS能够给HTML中的东西变得很漂亮. 比如我们可以像使用Photoshop或者Affinity Photo或者别的什么的画图软件里面一样, 我们可以指定一个元素的位置, 指定它的大小, 指定它的字号... 这个时候, 我们就需要知道一点点的排版艺术 -- 或者说, 排版的行话. 比如边距(margin), 行高(line-height), 字号(font-size)等等. 知道如何符合逻辑地设计排版其实也是一种学问. 

```css
* {
  font-size: 30vh;
  color: pink;
}

#box {
  position: absolute;
  top: 20in;
  left: 30px;
  margin-left: 10%;
}
```

(很遗憾, 目前我还没有非常细致地去了解过有关排版的东西, 所以我没法很好地介绍这些东西. 但是在这里我可以给你介绍一个好东西: 那就是(应该)每个游览器都会携带的网页检查工具. 在Safari中, 你可以通过打开开发者模式, 然后就可以在网页上右键Inspect Element来进入; 而在Chrome中, 使用快捷键F12会更加简单. 在其中的Element界面, 你可以通过特定的工具来选定网页中的元素, 并且进入其Style属性窗口, 然后在其中尝试修改, 增加, 删除其CSS描述属性, 于是你就能够实时观察到自己的选择会有什么样的效果. )

![inspect]({{ site.github.url }}/_img/how-to-make-web/inspect.png "大概的效果就是这样的, 在Style里面修改, 然后确认后写入自己的代码中. 简简单单. ")

并且你可以发现, 里面的数量都像是物理量一样带有单位. 比如`px`是像素(piex), `vh`是游览器可视区域的高度(visual hight), 甚至还有百分比(百分比是相对于父元素的对应量的大小). 就像是数学的公式一样, 括号里面有括号, 括号外面也有括号, 括号边上也有括号. 在HTML里面, 根据相对tags的位置, 我们称其他的tag叫做父元素(包括tag的元素), 子元素(被tag包括的元素). 

而CSS选择器也提供了这样的相对选择的方式, 比如可以选择所有在`#background`中的`img`元素: 

```css
#background img {
  width: 50px;
  height: 50px;
  filter: blur(3px);
}
```

没错, 只要让子元素跟在父元素后面就好了. 语法就是这么简单. (试试看修改一下这个界面上的一些元素. 然后看看会发生什么. )

### JAVASCRIPT
终于到了这个不太友好的家伙了. 所以我只会介绍一点点. (不过如果你有兴趣的话, 建议你试试看[Opal](https://opalrb.com), 一个能够把Ruby编译成Javascript的好东西, 我之前的[りlang]({{ site.github.url }}/ruby/ri-lang/)就是这样发布的. )

首先, Javascript上手很快: 

<details>
<summary>如何运行这些代码? </summary>

如果你的电脑里面恰好安装了nodejs, 那么你可以在命令行里面输入node, 然后就能够进入一个REPL(Read-Eval-Print-Loop)界面, 你输入的代码将会被执行. <br>

但是何必这么麻烦? 我们每个人的电脑里面(应该)都有游览器, 打开游览器, 进入检查视图, 然后在Console中, 你就能够试试看直接运行代码, 然后查看代码的运行效果. 

<img src="{{ site.github.url }}/_img/how-to-make-web/console.png"><img>
<br>
</details>

```javascript
// 这是一条注释
// 下面介绍的是几种赋值的方式: 
var varname = 2;
let alsoavar = "2333";
const pi = 3.14159265354;
```

假如你不关心的话, `var`和`let`**好像**完全没有什么区别, 具体的区别在[这](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Statements/let). 

<details>
<summary>关于let的东西</summary>

呃, 虽然在数学证明里面, 我们喜欢这样说, let x equals to 4 and the x times 4 is 16... 嗯, 听起来好像是那么回事. 但是我想说的是, 如果你接触过一些很酷的语言的话, 比如Lisp. 那么你应该会接触过这样的语法形式: 

{% highlight scheme %}
(let ((x 4))
     (* x 4))
{% endhighlight %}

在Lisp中的let让我们能够尽可能地在接近使用的地方建立局部变量约束. 说人话就是, 我们用let来构造那种随用随弃的工具变量来表述符号. 而var, 则是全局性的变量约束. 知道了这么些历史知识, 那么Javascript中的let的作用就比较容易理解了: 我们就是为了声明一个仅仅在当前块中有用的变量. <br>

不过说实话, 在撰写本文前, 我还真没想过这个, 我甚至认为这玩意和Swift语言里面的let一样, 是一种常量声明的格式. 尬, 不过我不会去改之前的代码的, 毕竟能跑.(bushi) 
</details>

```javascript
// 条件判断
if (x == 9) {
  // console.log() 会在终端输入信息, 可以在检查视图看到. 
  console.log('yes, x is 9');
} else if (x == 8) {
  console.log('yes, x is 8');
} else {
  console.log('err, x is neither 9 nor 8. ');
}

// 或者, 行if
if (x == 9) console.log('yes, x is 9');

// 循环
for (var i = 0; i < 10; i++) {
  // 或者其他乱七八糟的代码
  console.log(i);
}
while (condition) {}
```

嗯... 上面的东西好无聊哦. 是吧, 那么我们来一些有趣的东西: 

在Javascript中, 函数(过程)就像是Lisp一样, 可以被作为数据来储存和传递: 

```javascript
var square = function (item) { return item ** 2 };
console.log(square) // => function (item) { return item * 2 }
console.log(square(2)); // => 4

function ten_times(f) {
  for (var i = 0; i < 10; i++) {
    console.log(f(i));
  }
}

// 将会输出0, 1, 4, 9, ..., 81, 也就是一百以内的平方数
ten_times(square); 
```

你会发现, 我们把函数放在了一个变量里面储存, 并且还能够将它当作数据来传递给一个新的函数, 但是同时, 也能够将它当作是一种普普通通的函数 -- 通过括号来调用. (这样的东西可以有超级漂亮的用处, 虽然我目前没有这个水平, 但是没准你可以去查一查一种叫做lambda演算的东西, 它和图灵机是等价的. 超酷的好不好! )

于是我们可以写出这样的代码: 

```javascript
// 这将会延时一秒来执行我们的函数
setTimeout(function () {ten_times(square)}, 1000);
```

或者用这样的方法来对数组(Array)中的所有元素都进行一遍处理: 

```javascript
[1, 3, 7].map(square); // => [1, 9, 49]
```

于是你就可以开始编程了. 

<details>
<summary>还有一件事</summary>
那么我们要怎么来和网页中的元素互动呢? 操作就是先选择, 再修改. <br>

我们可以这样来选择<a href="https://developer.mozilla.org/zh-CN/docs/Web/API/Document">document</a>中的元素: 

{% highlight javascript%}
// 这会从HTML中找出有特定id的那个元素
var object = document.getElementById('id_of_the_element');
// 这会从HTML中找出class的所有的元素, 得到的结果类似于一个数组
var objects = document.getElementsByClassName('class_name'); 
// ...
{% endhighlight %}

对于那些选中的元素, 我们可以重新修改他们的属性或者修改他们的内容: 

{% highlight javascript %}
object.setAttribute('style', 'opacity: 0%;'); // 修改style的属性值
object.hidden = true; // 改变属性, 让元素隐藏
object.innerHTML = 'Hello, I am Lucky. '; // 修改内容
{% endhighlight %}
</details>

嗯, 还有什么呢?

## 附录: 关于使用的工具和开发网页的一些帮助

* **文件管理**: 比如给文件命名, 合理建立文件夹以及做好版本管理. (这个时候我就要吐槽一下之前帮别人改代码的时候遇到的奇葩命名了, 虽然作为代码混淆来说这样的工作可以增加逆向的难度之类的, 但是, 为什么是我? )    
  一般的习惯是把主页放在`index.html`里面, 将样式放在`style`文件夹里面, 将脚本放在`script`文件夹里面. 然后在HTML文件里面用`<link rel="stylesheet" type="text/css" href="./styles/main.css">`或者`<script src="./scripts/main.js"></script>`这样的形式来导入你的文件. 这样有助于文件整理和管理. (不过对于我的计算机科学导论作业来说, 所有东西都要整一个`index.html`文件里面, 就有点麻烦. )    
  (又, 以防你不知道, 文件的后缀名并不会改变文件的内容, 但是良好的后缀名有助于让计算机和你识别文件的种类, 以便能够采用合理的处理措施. 所以给HTML文件加上`.html`后缀名, 给CSS文件加上`.css`后缀名, 给JAVASCRIPT脚本加上`.js`后缀, 虽然没有什么, 但是还是挺重要的. )
* **自动补全和代码格式**: 呃, 其实就是文本编辑器的事情了. 前面说了, 制作网页的时候需要查很多的东西, 而在输入的时候假如有个贴心的小工具能够帮助你检查输入, 自动补全的话就会很不错了. 在NeoVim上面, 有coc-nvim, 里面安装了很多的插件可以用来代码补全; 在VSCode里面, 应该也有很多的插件的(比较推荐的一个就是Live Server, 可以帮助你在本地预览网页的效果).    
  至于代码格式, 怎么说吧, 就跟排版艺术一样的... 