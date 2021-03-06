---
layout: post
title:  "Why's (poignant) Guide to Ruby(3)"
date:   2022-01-05 16:03:15 +0800
categories: ruby why's_(poignant)_guide_to_ruby
---
# Them What Make the Rules and Them What Live the Dream
![under the title](https://poignant.guide/images/chapter.poignant.guide-5.jpg)

![through the space and time... in his bell jar... on a mission to find himself](https://poignant.guide/images/dr.cham-1.gif "一个坐在玻璃瓶中穿越时空去寻找自我和真实的男人")

## 正文前的叨叨
摊牌了, 我就是在乱翻译. 并且我之前其实也没有认真地读完, 
就是水了一遍代码, 故事都没看. 所以我现在挑着我之前不怎么会的地方, 
(因为我发现我现在的代码写出来和别的语言没什么大区别, 
不够优美, 所以, 我要改变! )
然后重新学习, 然后(尽可能地)认真翻译. 

假如你想看原文的话, 看[这](https://poignant.guide/book/chapter-5.html)

## 回到正文
说实话, 那些说Dr. Cham(Cham博士)是个疯子的鬼话我已经听腻了. 
没错, 他的行为无异于将自己活生生地埋入那个玻璃棺材; 
他电击他侄女的行为也是有目共睹; 
他也的确将那个快要报废的房子用炸弹轰上了天. 
但是这些可都是在他的深思熟虑下的仔细操作
(并且我相信, 在每一次这样的事情中, 他都采取了正确的措施. )

随大流吧, 孩子, 但是我可以肯定, 在你的内心深处, 
总还是有一丝丝地对他的难以抑制地敬仰 -- 
毕竟曾经他花了很多时间教会了你如何定义一个`ruby`的类, 
甚至还教会了你如何`mixin`模块. 所以有可能的话, 
我希望我们可以在这章结束的时候回顾一下博士的灰蓝色的一生, 
然后不要再叫他"疯子"了. 

但是假如你执意要叫他"疯子", 那我就只好冲到铁轨上, 
看着火车呼啸而过, 碾碎一串又一串的闪亮的灯带, 碎片撒落一地, 
场面一片混乱. 然后你就只好骂骂咧咧地去清理这些碎渣了. 

好的, 我们继续. 

## 1. This One’s For the Disenfranchised
*这家伙出局*

![some people still can't get past what he did](https://poignant.guide/images/elderly-1.gif)

如果你问我Dr. Cham一生中的任何一个时刻, 
我可以为你大致地回忆起那段时间里Dr. Cham的事情. 
并且我还要用`ruby`里面的`method`的方法来做到这件事, 
所以`method`会将这段代码变成一段独立的代码, 
虽然是一块和其他代码孤立起来的代码, 但是当程序喊他的时候, 
它又会被调用并执行. 

(这一段的翻译不是很好, 给出原文: )
> so it’s an independent piece, 
> an isolated chunk of code 
> which can be hooked up 
> to the voice of a robotic volcano, 
> when such a thing becomes the apex of 
> authoritative voice talents.

(大概的意思是: 当控制着程序世界的至高之声喊到它的名字的时候, 
哪怕这不过是一块独立的代码, 它也会被机器火山的熔岩勾连起来, 
最后程序的一部分运行. )

好的, 我希望你可以注意一下关键词`def`, `case`和`when`. 
你应该见过`Ranges`了, 
(对不起, 没翻译第三章:p, 简单科普一下, 
`Range`顾名思义就是区间的意思, 大概理解一下就好. )
在这里的是一个闭区间`1895..1913`
(就是包含开头的`1895`和结尾的`1913`的东西), 假如没印象了的话, 
请返回[第三章](https://poignant.guide/book/chapter-3.html). 

然后在某些行的末尾的斜杠`\`的作用是告诉字符串
把我们的输入中的回车键忽略掉, 
这样就可以告诉`ruby`这是一个很长很长的字符串, 
长到甚至有好几行. 

好的, 注意看着`def`, `case`还有`when`哦. 

```ruby
def dr_chams_timeline( year )
  case year
  when 1894
    "Born."
    # 出生
  when 1895..1913
    "Childhood in Lousville, Winston Co., Mississippi."
    # 在Lousville, Winston Co., Mississippi的童年
  when 1914..1919
    "Worked at a pecan nursery; punched a Quaker."
    # 在坚果托儿所工作; 殴打了一位贵格会教徒
  when 1920..1928
    "Sailed in the Brotherhood of River Wisdomming, which journeyed \
     the Mississippi River and engaged in thoughtful self-improvement, \
     where he finished 140 credit hours from their Oarniversity."
    # 在Wisdomming河中航行, 
    # 在沿着Mississippi河的航程中, 
    # 他开始思考一些自我完善的问题. 
    # 在航行的过程中, 他修完了Oarniversity大学的140学分. 
  when 1929
    "Returned to Louisville to pen a novel about time-travelling pheasant hunters."
    # 回到了家乡Louisville, 
    # 开始着笔写作关于能够穿越时间的野鸡猎人
  when 1930..1933
    "Took up a respectable career insuring pecan nurseries.  Financially stable, he \
     spent time in Brazil and New Mexico, buying up rare paper-shell pecan trees.  Just \
     as his notoriety came to a crescendo: gosh, he tried to buried himself alive."
    # 找到了一个推销坚果托儿所(? pecan nurseries)保险的
    # 还算受尊重的工作. 在经济稳定后, 他跑到巴西和新墨西哥
    # 度过了一段时间. 在这段时间里, 他买了一些薄壳坚果树. 
    # 不幸的是, 这段时间也是他的"骂名"到达极点的时候: 
    # 因为他试着把自己活埋了. 
  when 1934
    "Went back to writing his novel.  Changed the hunters to insurance tycoons and the \
     pheasants to Quakers."
    # 重新回去继续着笔写作他的小说. 
    # 将猎人写成了保险业的龙头, 将野鸡写成了贵格会教徒. 
  when 1935..1940
    "Took Arthur Cone, the Headmaster of the Brotherhood of River Wisdomming, as a \
     houseguest.  Together for five years, engineering and inventing."
    # 和Brotherhood of River Wisdomming的校长
    # Arthur Cone一起. 
    # 这一个五年他们在一起做了一些工程和发明的事情. 
  when 1941
    "And this is where things got interesting."
    # 现在, 故事开始变得更加有趣了. 
  end
end
```

注意这里的`def`关键词, 这可是我们的定义的第一个 **method**. 
这个简单的函数实际上是一个定义在 **kernel** 中的函数, 
也就是说这个函数可以在`ruby`中的任何一个地方运行. 
让我们来看看如何运行这个函数: 
```ruby
puts dr_chams_timeline( 1941 )
```

于是程序就会告诉我们
`"And this is where things got interesting."`
这个时候就又要提到那个老掉牙的故事了: "好好用你的答案". 
在这里我用了`case`陈述的方式来返回一个字符串, 
(这个是因为`case`方法就是
我们定义的`dr_chams_timeline`方法中最后的一个运行的东西. 
所以最后处理的东西就会被`method`抛出作为返回值. )
这个`case`的陈述就像是一股涓涓细流从一个判断语句流向另一个判断. 

> Trickling water spilling down from ledge to ledge.

让我来帮你把这个`case`语句搞清楚一点: (哦, 确切来说, 
我应该叫它`case..when`语句, 这是因为他们没法分开单独地使用. )
首先`case`的关键词后面会跟着一个值, 
然后这个值就会依次和`when`后面的值进行比较. 
假如遇到一个匹配的值的时候, 这个时候就会返回接下来的语句的值, 
然后将剩下的比较都忽略掉. 
虽然你可以用`if..elsif`来写`case`代码, 但是这样很麻烦. 

```ruby
case year
when 1894
  "Born."
when 1895..1913
  "Childhood in Lousville, Winston Co., Mississippi."
else
  "No information about this year."
end
```

上面的代码就和下面的代码类似: 

```ruby
if 1894 === year
  "Born."
elsif (1895..1913) === year
  "Childhood in Lousville, Winston Co., Mississippi."
else
  "No information about this year."
end
```

上面的`===`三个等号的比较符有点像是一个松松垮垮的牛仔套索, 
虽然它也会像`==`一样来检查两个值是否相等, 但是这三等号的比较符, 
相比之下, 没那么严格, 所以它允许一定限度的可变性. 

拿上面的`Ranges`的作为一个例子吧, 
`(1895..1913)`实际上和`1905`是不一样的, 
(他们甚至都不是一个类型! )
假如我们想要有两个真正一样的`Ranges`的话, 咳, 
那就只能是另外一个一模一样的`(1895..1913)`. 
但是在上面的例子里面, 
三个等号的比较符会为你在`Ranges`里面为你开出一条道路 -- 
然后让`1905`这个数字在比较的时候, 就放在一个区间里面进行比较. 
这样的小操作在很多的时候会很便利, 比如就像是我上面的人生大纲一样.

上面的真的就像是一个人生的大纲一样对吧. 呃, 我是说, 
虽然`dr_chams_timeline`不过是段代码, 
但是它真的就像是一个人生大纲一样, 简洁又美丽. 

![what research revealed](https://poignant.guide/images/elderly-2.gif "调查显示了...")

### But Was He Sick??
你要知道, 他一生多舛. 虽然在大众眼中, 他只是一个小说家, 
但是他在炼金术上的造诣颇深: 他曾经用山羊奶和海盐
治好了自己的脚伤; 让一个家伙长回了自己失去的一个拇指; 
发明了一种虽然闻着像是臭脚丫, 但是确实纯天然的夜视药水. 
他还曾经着手制造过一种叫"Liquid Ladder"(液梯)的东西, 
但是因为我才疏学浅, 所以不知道这是什么, 显然这可不只是什么梯子, 
谁知道呢? 

曾经的确有一家报纸采访过Dr. Cham. 然后也确实写了些关于他的文字,
并且在读者反馈中, Dr. Cham得到了4星的好评. 

要知道Dr. N. Harold Cham实际上对他的侄女充满了歉意. 
虽然本来她就身患小儿麻痹症, 即使他没有冒这个险去执行电击疗法, 
她也可能会因为小儿麻痹症而去世. 

所以在1941年的9月9日, 
在自己的秘密手术室中用一罐镇静剂麻倒了他的侄女后, 
他将电极接到了Hannah的鼻子, 舌头, 脚趾头, 还有手肘上. 
在他的助手 -- Marvin Holyoake, 一个劣迹斑斑的本科生, 
(a bespeckled undergraduate) 的帮助下, 
他们在她的身上铺上了一层被医生们叫做*opus magnum*的薄片状东西. 
然后撒上了一些可以导电的白色金粉 -- 这种物质有助于增强导电性, 
然后刺激他的侄女的血液循环. 
(forcing her blood to bloom and fight and vanquish. 
让她的血液爆发性的迸流. )

但是这是为什么失败了呢? 当电闸被按下的时候, 电压缓缓地升高, 
突然间她弹了起来, 她的脚踢到了电缆线 -- 咔咔咔砰!! 
强光冲了出来, 她的头发散落, 死亡的警铃大作, 一股青烟缓缓地升起, 
实验失败了. (接下来的几个星期, 街坊们见面聊的都是: 
"她差一点就没机会了...")

对于Hannah, 我写下了如下的代码: 
```ruby
opus_magnum = true
def save_hannah
  success = opus_magnum
end
```

每个`method`都有自己的一块三分地, 并且进入其中的东西
(变量之类的)是不会受到它周围的东西影响的. 
所以Dr. Cham没法除去他侄女的毛病. 
因为外面的`opus_magnum`变量没法突破`method`的铁壁铜墙, 
也就没法进入`method`的代码块中了. 

假如我们运行这个`save_hanhah`方法的话, 
`ruby`就会大声地抱怨我们(squawk at us), 
然后指出它找不到一个叫`opus_magnum`的东西. 

> NameError (undefined local variable or method `opus_magnum' for main:Object)

我所说的就是 **scope**. 
显微镜 **micoscopes** 会把你的视野限制并放大你所见的; 
望远镜 **telescope** 会极大地扩展你可视的距离. 
在`ruby`中, **scope** 指的也就是在`method`和`block`中一种
"视野". 

> 我觉得这段比喻很形象, 所以看看原文吧. 
> I’m talking about scope. 
> Microscopes narrow and magnify your vision. 
> Telescopes extend the range of your vision. 
> In Ruby, scope refers to a field of vision 
> inside methods and blocks.

想象一下, 当你睁开了眼睛(用`def`开始定义了一个函数, 
或者是打开了一个代码块之类的...), 你所见到的(变量之类的)
就都在你的头脑中投映出来(这些变量就都是有意义的), 
然后当你闭上眼睛之后(用`end`关上了这个 **scpoe**), 
这些变量就都消失了. 你可以把数据通过参数(argument)传到里面, 
你也可以通过`return`的方法把数据从方法中送出, 
但是一个在`method`中的变量只能够在它自己的**scope**中使用. 

有一些变量可以有一些更加广泛的 **scope**. 比如, 
像`$LOAD_PATH`一样的用`$`开头命名的全局变量, 
可以在任何一个 **scope** 中被使用, 访问; 
像`@names`一样的用`@`开头的实例变量, 
可以在 **class scope** 中的任何一个地方访问和使用, 
(就是在类的实例中的任何一个地方都能用); 
像`@@tickets`一样的用`@@`开头的类变量, 
可以在(同一个)类(的实例之间)的任何一个地方使用. 
关于类变量和实例变量的话, 我们可以等一下再详细地介绍一下. 

代码块(`block`)也有 **scope**, 但是这却有一点点的模糊, 
也可以有一些可以"通融"的地方. 

```ruby
verb = 'rescued'
['sedated', 'sprinkled', 'electrocuted'].
each do |verb|
  puts "Dr. Cham " + verb + " his niece Hannah."
end
puts "Finally, Dr. Cham " + verb + " his niece Hannah."
```

上面的代码块对博士的每个行动(这个`array`中的每个元素)进行循环, 
(iterates, spins, cycles), 在每一次的循环中, 
`verb`变量的值都会发生变化, 分别是: "sedated", "sprinkled",
"electrocuted". (安抚, 撒上金属薄片, 通电)

所以现在问题来了, 当这个代码块运行完毕之后, 他是否解救了他的侄女?

```
Dr. Cham sedated his niece Hannah.
Dr. Cham sprinkled his niece Hannah.
Dr. Cham electrocuted his niece Hannah.
Finally, Dr. Cham rescued his niece Hannah.
```

代码块可以访问在它周围的变量. 但是在上面的代码中, 
代码块的每一次循环中都有一个自身的`verb`变量, 
并且在结束代码块的循环后, 在外面的`verb`变量仍然保持不变. 

("代码块可以访问在它周围的变量.": 
```ruby
sum = 0
5.times{|i| sum += i}
# sum => 10
```
"每一次循环中都有一个自身的变量"
```ruby
i = 1
5.times{|i| print(i+=1)} # 12345
# i => 1
```
)

下面的代码显示的就是局部变量的特性: 当scope关闭的时候, 
这个变量以及它的值就随之消去了. 
比如说假如`verb`变量在这个代码块前面并没有定义过的话. 

```ruby
['sedated', 'powdered', 'electrocuted'].
each do |verb|
  puts "Dr. Cham " + verb + " his niece Hannah."
end
puts "Yes, Dr. Cham " + verb + " his niece Hannah."
```

那么如果尝试运行的话, 就会打印出来一条错误信息: 
```undefined local variable or method `verb'```. 
啊, 看来里面的变量绝对不会跑到外头去. 

既是是对于像Dr. Cham一样的大科学家, 
失去他的侄女的心情一定不会是轻松的 -- 她的裙子才刚刚上过浆, 
漂亮的刺绣还在她的裙子上开花, 但是她只是静静地倒在他的怀里, 
绛紫色的嘴唇失去了生命, 暗红色的血沫在她的嘴角结成了血块... 
在Dr. Cham的日记中, 他这样地写道: "每天晚上, 
我都会被她的闪耀着金色光芒的焦糊的幽魂折磨者我的良心. 
地狱的猎犬和天使们审判之手密密麻麻地向我扑来... "
毫无疑问他的幻觉愈发得严重了. 

数周之后, 他离开了 -- 在那巨大的内疚的驱动下, 
在一阵轰鸣的爆炸声中, 他将自己从这颗星球上放逐到了无尽的太空中. 

哦, 当你读到这些文字的时候, (大概吧), 我们孤独的Dr. Cham
可能正坐在他的玻璃罐中, 迎来了漫长六年的宇宙漂流的第一次着陆. 
随着航行器不断地靠近这个新世界 -- 翻滚着划过天际, 
这里是太阳风激荡的"热油锅", 又在绚丽的极光中划开了一道口子, 
在剧烈的摇晃中, Dr. Cham朦朦胧胧地睁开了沉睡的眼睛: 
之间圆球状的大地在他面前展开了自己无边无际的画卷... 

![safe landing. amazement.](https://poignant.guide/images/dr.cham-2.gif "多么惊人的着陆啊...")

你所见的就是Dr. Cham在Endertromb(有"沉重散步的尽头"之意)
星球上的着陆的经历. 据我所知, 他着陆的时候恰好是当地的
"Desolate Season"(无人季节? )将要结束的时候. 
在所谓的Desolate Season里, 这片大陆上的生物将失去大脑的活性, 
他们的身体就会一点点地解体, 
变成只会哼哼叫的四分之一智能和四分之三水蒸气的没有生气的鬼魂. 

这个时候我就应该谦虚地指出, 我对Endertromb星球上的历史, 
气候的了解是来自于我女儿的管风琴老师的 -- 他出生在那个星球上. 

![dead husband could destory doctors](https://poignant.guide/images/elderly-3.gif "她死去的丈夫没准可以毁灭那医生")

我曾经常常因为我女儿的管风琴老师没法合理地履行我们的合约而指责他,
希望他至少能够在零星的时间里为我接听一下紧急的家庭电话. 
后来他终于自曝了: 跑过来跟我说自己是个外星人. 
他们在白天有五百四十个小时是清醒的. 于是我就高兴地接受了, 
和他签订了一个持续到2060年的合约. 

大概持续了三天左右 -- Dr. Cham是根据自己口袋里的手表来判断的, 
他穿过了一片贫瘠的平原, 空气里混杂着令人窒息的沙砾, 
黑压压的天空像是要将人闷在一个阴森森的矿井中. 但是在第三天, 
Desolate Season结束了, 所以当他终于醒来的时候, 
他发现自己的身边却是一片鲜艳的饱含露水的红色的花海, 
远方是层层叠叠的堡垒山峰. 

> # Caring For You. And Your Wellness.
> *为了你的精神健康...*  
> 
> 我希望在你读了大半本书之后还能有个健全的心智. 
> 现在我觉得你应该要来些心智训练. 
> 
> 首先, 先做一些深呼吸吧. 深吸一口气, 然后在心中默念
> 1, 2, 3, 4... 然后呼气. 你可以感觉到你自己的眼睛. 
> (? You can feel your eyes. 不妨翻译成: 
> 闭上你的眼睛去感受它) 没错, 就是这样. 
> 
> 现在让我们再深吸一口气, 然后在你的脑海中想象一只河马. 
> (想得快一点, 免得你被憋死... ) 想想它的四条腿, 
> 它皮肤上的褶皱, 它棉花糖一样的牙齿. Okay, 完成啦. 
> 现在呼气吧. 
> 
> 再一次深深地吸一口气, 然后紧紧的屏住气. 感受着胸腔中的压力, 
> 然后想象自己正在被这股压力压成了一只甲虫. 
> 其他的甲虫们被你"七十二变"的特效惊呆了, 手舞足蹈地, 
> 他们围在你的周围, 疯狂地摇晃着自己的触须. 哦, 不. 
> 但你之前还在头上顶了个苹果, 这个苹果现在正作着自由落体运动...
> 哐! 它砸在了这群虫子上! 你也被砸死了. 现在呼气! 
> 
> 再一次, 深深地吸进一口气. 然后这一次, 
> 想想你居住在一个所有东西都是电话磁带(telephone cords)
> (这个应该是历史了吧? 合理猜测. )做成的. 
> 房子们是用磁带做成的; 河边上的鹅卵石, 河上的木筏
> 也是磁带做成的; 门廊是用磁带做成的 -- 厚厚的磁带黑压压的一片, 
> 你只能艰难地从这磁带门中挤过去; 当夜晚来临, 你想要入眠时, 
> 你的床也是用磁带做成的. 没错, 就像我说的这样, 
> 所有东西都是磁带做成的. 甚至电话本身也是电话磁带做成的 -- 
> 但是电话用的磁带却不是电话做成的, 它们是用面包片和小树枝做成的.
> 
> 现在呼气. 
> 
> 然后吸气. 1, 2, 3, 4. 然后呼气. 
> 
> 吸一口气. 1, 2. 然后再短短地吸一口气. 3, 4. 
> 想象一下自己的双手啪地一下从你的手腕上跳了出来, 
> 冲进了你的电脑屏幕, 然后从里面开始了编程. 
> 
> 呼气. 
> 
> 深深地吸气. 你好像是搭乘着一艘潜艇深深地潜入你的体内... 
> 这艘潜艇还有个舌头. 好了, 呼气. 
> 
> 用你的鼻孔深深地吸一口气. 你的鼻子好像在放光, 
> 空气经过你的鼻子的过滤, 
> 好像是上帝将那清新自然的的空气吹入了你的胸腔. 
> 想象一个被孤儿们堵塞的软盘口, 它止不住的咳出孤儿们... 
> 与此同时, 你的肺中美妙的充满生命的上帝的吐息
> 却正在悄然地变成威力无比的毒药... 快一点吧, 
> 把这口气快呼出去吧, 然后吸入一些新的空气吧. 
> 
> 现在你可以醒来了. 把你游览器中的褶皱熨平吧. 
> 现在你可以获得一个对你自身的重整, 
> 并且也不要把你自己一生中的那些许许多多的冒险经历忘掉了. 
> 你可能不会再记得这个短短的经历. 
> 可能你只会记得自己曾经远远地教一只小兔子如何使用剪刀. 
> 
> 然后你睁开了你的眼睛, 你醒来了. 看着这段联系的开头, 
> 你没准像再来一次. 但是这一次, 没准你可以试试 -- 
> 甚至连你自己的影子都是由电话磁带做成的. 

## 2. A Castle Has Its Computers
![the panoramic vales of Sedna on Endertromb.](https://poignant.guide/images/dr.cham-3.jpg "Sedna山谷的绝妙全景")

我们无畏的博士穿过了花海, 走向了那座外星堡垒... 
随着脚步在地上哒哒地响起, 那座堡垒在地平线上一点又一点地升了起来.
本来他指望着周围会有些种马(? stallion), 但是放眼望去, 
完全没有任何的种马的迹象. 这个时候他就明白了, 
这个星球绝不会让他轻松地事事如意. (不会读他的心并且回应他的心愿)

但是我女儿的管风琴老师却说, 这个星球**可以读心**, 并且
还可以**满足你的愿望** -- 只是没法同时做到这件事情. 

有一天我问他(就是那个管风琴老师)的时候, 
他在一张纸上写下了下面的代码 -- 这张纸有着奶酪样的颜色, 
但不知怎么的, 我觉得甚至还有一种奶酪样的气味. 

```ruby
require 'endertromb'
class WishMaker
  def initialize
    @energy = rand( 6 )
  end
  def grant( wish )
    if wish.length > 10 or wish.include? ' '
      raise ArgumentError, "Bad wish."
    end
    if @energy.zero?
      raise Exception, "No energy left."
    end
    @energy -= 1
    Endertromb::make( wish )
  end
end
```

上面的就是wish maker(愿望实现者). 

呃, 实际上来说, 这是**wish maker的定义**. 
对于`ruby`来说, 这就是一个**类的定义**. 
这样的代码就会向计算机说明特定类型的**对象**是如何工作的. 

每个早上, wish maker就会开始实现最多五个许下的愿望. 
也就是说, 随着太阳升起, 一个新的`WishMaker`对象就会被创建. 

```ruby
todays_wishes = WishMaker.new
```

其中类方法`new`会创建一个新的空白的对象(**object**). 
然后它会自动地去调用对象的`initialize`方法, 而在这里, 
正如你所见到的, `WishMaker`类的`initialize`
方法里面只有一句简单的代码: `@energy = rand( 6 )`. 

这里的这个具有随机值的变量`@energy`就是传说中的实例变量, 
(**instance variable**). 这个实例变量在这个类的任何地方, 
任何时候都是有效的. 但是一旦到了外头(outside the scope 
of the class), 就没办法用了. 

在第三章中, 我们简要的讲到了实例变量, 并且将他们视为属性, 
(respect them as **attributes**, 就是假如你不深究的话, 
可以把`@`符号看作是"属性"的意思. ). 虽然, 
实例变量可以用来存放任何的数据信息, 但是通常情况下, 
它们都是用来存放一些代表一个类的对象的信息. 

在上面的例子里, 每一天的wish maker都有自己的能量槽(energy 
level). 你可以把它想象成一台机器, 上面有一个仪表盘, 
仪表盘的指针就正好指向着它自己的能量槽值. 我们的`@energy`
变量的作用就像是这个仪表盘的指针一样. 

```ruby
todays_wishes = WishMaker.new
todays_wished.grant( "antlers" )
# antlers : 鹿角
```

好的, 现在我们回过头来再看看, 确保你自己把上面的例子搞懂了. 
我们的`WishMaker`的类就是我们的魔法如何实现的大纲了. 
当然了, 这个并不是完完全全的真正的代码, 有很多的东西都被隐藏在背后. 
(因为你真的直接扔到ruby里面运行的话, 显然是不可以运行的, 
ruby会用像是看中二病一样的眼神看着你, 然后说, 啊, 
我不知道什么是`endertromb`, 对不起, 噗. )
但是我要说, 虽然我们不能告诉电脑如何像精灵一样实现我们的愿望, 
但是我们可以让电脑去告诉小精灵如何实现我们的愿望 -- 
只要你准备好了那些书面文件, 因为精灵们就好那一口. 

首先, `todays_wishes`就是创建了一个小精灵的实例, 
然后我们向它许了一个愿望, 给我们一个鹿角. 
(我绝对不想听到什么你通过这个例子得到了一个鹿角什么的, 
假如你真的得到了的话, 那么就快到草地上欢呼雀跃吧. 
你的美梦成真了. )

上面的代码里面由两个部分组成: 

1. 定义了一些东西
2. 对这些东西施加操作(Putting those things into action.)
   
什么是ruby中的**action**(操作)? 就是我们的**Methods**(方法). 
也许你现在对ruby语言中的这个定义还有一点点懵. 那么简单地介绍一下吧: 
我们可以用`def`来定义方法, 用`class`来定义类. 

在这一点上, 没准是时候告诉你了: 
**在ruby里, 所有东西都是对象**. 

```ruby
number = 5
print number.next                   # prints '6'

phrase = 'wishing for antlers'
print phrase.length                 # prints '19'

todays_wishes = WishMaker.new
todays_wishes.grant( "antlers" )
```

所以, 必然的, 每个对象的背后都有一个对应的对象的类. 

```ruby
print 5.class                       # prints 'Integer'
print 'wishing for antlers'.class   # prints 'String'
print WishMaker.new.class           # prints 'WishMaker'
```

Dr. Cham没能够在那大平原上看到什么wish maker的踪迹, 
这是因为wish makers都待在距离他现在所在的这个平原很远的Sedna峡谷里. 
在那个陡峭的山谷里, 沿着峭壁向下, 就是一片灌木林. 
当你到了那个地方了之后, 你需要把自己的愿望写在一张小小的纸上
(written on a small 1" x 6" slip), 然后卷成一小卷, 
然后从山谷顶上扔下去 -- 希望运气好的话, 它能够落到一只蜥蜴的背上, 
正好落到它小小的, 细长的角上. 

比方说你的愿望真的完成了这般的壮举. 好吧, 那么可惜的是, 
在那扭曲的灌木丛中的, 那些火蜥蜴, 它们尖叫着在灌木丛中赛跑, 
踏过那些腐烂的木头 -- 这些宝贵的木头在很久以前还是一座教堂, 
屹立在峡谷的边缘上, 但是在一次火蜥蜴的赛跑中, 被不小心**推倒了**, 
然后对于那些在教堂里头的那*饱经沧桑*的神父, 自然不用说, 
对那些该死的恼人的两栖动物深恶痛绝到了极点, 
一定会用他那被祝福了的神圣的金链子活活勒死那些怪物们, 
然后将它们留作自己的一年一度的*Getting To Know You*早餐来吃. 
然后... (哦, 不! ) 神父踩到了你珍贵的愿望纸条! 
你的纸条恰好被卡在了鞋子的花纹里面. 
然后更惨的是, **盗贼们来了**! 这些盗贼们最爱对他人**施加酷刑**, 
他们拿着利刃威逼, 一寸一寸地从头到脚地剥夺神父身上的财宝 -- 
谁能够顶得住这样的酷刑呢? 
然后当(可怜的)神父终于只剩下那一双薄薄的皮鞋时, 
贪婪的盗贼们仍然不肯放过他, 
他们将这珍贵的**牛皮鞋**捧在手中, 欢呼祝福自己的好运气. 
然后在欢呼庆祝完毕后, 这些盗贼轻轻地划着自己的笨重的**独木舟** -- 
这些独木舟上装满了财宝, 沉得几乎都要随时沉没了. 
他们兴奋地有些飘飘然了, 关掉了船上的发动机, 
在平静的水流中慢慢地划着自己的小船. 
但是你的愿望纸条的奇幻之旅并没有到此结束, 它现在卡在鞋底上, 
而这鞋子却是被随意地松松垮垮地挂在一个盗贼的腰带上的. 
突然之间, 一条**毛茸茸的鲤鱼**从水中一跃而出, 
好巧不巧地撞到了那双鞋上 -- 一口吞了下去. 
哼, 就让那些盗贼们慌张吧, 慌张有什么用呢? 
他们又不能够追着那只鞋子冲进水中. 假如他们能够做到的话, 
没准他们可以看到海底的数千万根光纤交织形成的海底电缆. 
等等, 那条鱼在哪里? 哦, 
那条鱼现在正**卡**在这Endertromb星球的核心的**外围** -- 
然后这个核心所做的就是一口将那条鱼**吞入**自己的内部, 
然后你的愿望纸条**才最终到了它的最终归宿之中**. 

> And let’s say your wish makes it that far. 
> Well, then, down the twisted wood goes the skinny salamander, 
> scurrying through the decaying churches 
> which had been pushed over that steep canyon ledge once 
> and for all. And the expired priest inside, 
> who weathered the fall as well, 
> will kill the little amphibian — strangle it to death 
> with a blessed gold chain — and save it for the annual 
> Getting To Know You breakfast. 
> He’ll step on your precious little wish and, 
> when the thieves come, that slip will still be there, 
> stuck on his sole. Of course, the thieves’ preferred method of 
> torture is to cut a priest in thin deli-shaved slices from top 
> to bottom. Who can cull evidence from that? 
> And when they chop that last thin slice of shoe sole, 
> they’ll have that rubber scalp in hand for good luck 
> and good times. But they canoe much too hard, these thieves. 
> They slap their paddles swiftly in the current to get that 
> great outboard motor mist going. But the shoe sole is on 
> a weak chain, tied to one man’s belt. And a hairy old carp leaps, 
> latches on to that minute fraction of footwear. 
> And the thieves can try, but they don’t see underwater. 
> If they could, they’d see that mighty cable, 
> packed with millions of needly fiber optics. 
> Indeed, that fish is a peripheral plugged right 
> into the core workings of the planet Endertromb. 
> All it takes is one swallow from that fish 
> and your wish is home free!

嗯, 这就是孩子们的愿望是如何成真的(艰辛之路). 

在我女儿的管风琴老师讲完了有关wish maker的故事后, 
他有讲起了有关这个星球的mind reader的事情. 

```ruby
require 'endertromb'
class MindReader
  def initialize
    @minds = Endertromb::scan_for_sentience
  end
  def read
    @minds.collect do |mind|
       mind.read
    end
  end
end
```

就像你之前看到的那些东西一样, `initialize` 方法将会在一个新的
`MindReader` 类的对象被创建的时候被调用. 
这个`initialize`方法将会收集扫描这个星球上的思念体的意识. 
看起来这些思念体好像是被储存在一个数组中的, 
因为接下来他们将会通过一个`collect`方法经过递归来处理. 

在上面的故事里, 
wish maker和mind reader都用到了一个叫做`Endertromb`的类. 
这个类是通过`require 'endertromb'`这条命令, 
来从`endertromb.rb`文件里面调用的.
实际上, 这个文件里面就恰好定义了`Endertromb`的类的东西. 
通常的, 你需要调用其他的类来完成你的程序. 
接下来这本书的部分将要探索很多有趣的ruby中可以导入的类. 

(注: 大概是因为why没有写完就消失了的原因, 
这本书没有完全写完一开始想要讲的东西. )

### Dr. Cham Ventures Inside
Dr. Cham慢慢地走近了那个城堡, 因为我们之前说了, 
这个星球是一个可以读心的星球, 所以他的所有内心活动, 
他迈出的每一步, 都正被这颗星球所感知 -- 心如死灰的感情. 
他沉重地踏上了大门前的台阶, 
然后穿过了入口的那曾经漂亮的拱门 -- 
现在却破旧得好像是在大声哭诉自己被遗弃的命运. 

Dr. Cham站在门口, 沉默的风儿拍打在他的面颊上, 
终于, 他敲开了门. 

![blocky whale greeting](https://poignant.guide/images/castle-1.jpg "像素鲸的问候")

看着那只鲸鱼慢慢地鼓得像只气球一样地浮起来, 
Dr. Cham顿时感到了一丝庆幸, 还好不是一只张牙舞爪, 
迫不及待地想要和自己玩耍的巨鹰来开门; 也幸好, 
从门口中探出的不是一只巨大的老鼠的脑袋; 
或者是什么一人大小的龙卷风. 只是一只笨拙的鲸鱼罢了. 

"这个城堡可不是个令人省心的地方. " Dr. Cham说到. 

一开始, 他还以为自己进入了一个十分昏暗的走廊之中. 
当时当他的眼睛逐渐地适应了周遭的环境之后, 
他惊奇地发现自己所在的这个入口竟然连接着一条隧道. 
这个城堡的大门直直的对着这条用长长的扁石板造就的隧道. 
这条隧道的一部分形成了走廊一样的结构, 另外一些部分变得狭窄, 
突出, 最后形成了高高的尖顶. 

许多小得几乎只能够装下一抱(armful)的卷心菜的冰箱散落在地上, 
不知道为什么, 他们的门都不见了, 里面的溢出的灯光点亮了走廊. 
Dr. Cham好奇的瞥向一个冰箱之中, 里面的光均匀地洒在其中, 
落在冰块上面. 

他伸手抓了一把冰块, 感受着冰块黏在他手掌上的感觉, 
然后双手合拢不停地搓揉着, 融化的冰水淌下了他的手中. 
终于, 他用这冰水享受到了一次小小的沐浴 -- 
距离上一次的沐浴是隔了多久了? 十年? 还是三十年? 

沿着道路, 四下七零八落地躺着一卷卷的布料, 穿过了这些, 
不一会儿, 一个由明亮的像素风的瓷器制成的洞穴出现在了他的眼前.

他发现了一个在洞穴中的隐秘的房间. 房间里排满了巨大的空龟壳, 
甚至墙上也点缀着不少龟壳. 他迷惑地看着这个房间, 
这里曾经是什么? 他有点想要做到龟壳上, 就像是做到凳子上一样, 
休息一下; 但是突然又转念一想, 万一会有什么蜘蛛从龟壳中出来, 
于是放弃了休息的想法, 继续前进了. 

### Meal in a Castle’s Pocket
他一路沿着隧道前行, 脚下的主路分成了几条支路, 
又最终在一个巨大的空空荡荡的山洞中汇合了. 
他发现了这个地方的环境有一些共同的主题: 
有几个房间被一个像是泵一样的机器聚在一起, 
而在另外的一个地区, 眼前全是遍地的布料和一缸一缸的胶水. 
听着声音, 他摸索着来到了一个充满毛绒织品和枕头的洞穴 -- 但是,
这是一条死胡同: 弯曲的墙壁上在于视线齐平的高度上
却刻了一个小房间. 

他走进了那堵墙, 然后在那个正方体形状的洞里, 
发现了两只坐在桌子上饱餐的食蚁兽. 

它们默默地看了一眼Dr. Cham, 却仍然没有停下自己进食的小爪子, 
对着它们面前足足有两倍于自身身材的蚁穴展开了猛烈的进攻: 
敲开蚁穴的大门, 努力地探出自己的身子去舔食...

当它们终于停下了自己嘴里的工作后, 盯着自己手上的叉子. 
Dr. Cham觉得是时候说些什么了: "你们好啊, 小家伙们. "
(这里原文是Hello, little puppets, 虽然和后面的故事有关, 
但是我觉得翻译成小木偶的话有点怪. )

"我真希望我的侄女Hannah能够在这里看到你们这些小家伙, "
他看着其中较小的一只食蚁兽(因为它看起来好像有点好说话的样子),
说到, "她一定会觉得你们是一个绝妙的木偶表演. " 他瞥向了餐厅, 
橱柜上有着一排一排的盘子, 手巾. 一台机器里面蹦出来一只只
小小的兔子, 小兔子手中又捧着一盘盘浇着奶油的鲜艳的面条,  
从机器里探出上半身来... 在房间的后面的半开着一扇门, 透过门缝, 
Dr. Cham只能或隐或现地看见几张椅子和一闪而过的摩托. 

"大概所有的孩子都想要一个这样的梦幻小屋吧", 他说到. "我之前说了, 
我侄女Hannah也有一个小小的针织玩偶, 假如放在一台纺织机边上的话, 
就好像是在纺织一般 -- 哦, 不是, 我只是在做比喻罢了, 洋娃娃不会纺织. "

其中一只食蚁兽打开了地上的活板门, 然后点亮里面的按钮, 
于是一个小小的投影机从地上升了起来. 另一只食蚁兽静静地坐着, 看着Dr. Cham. 

"但是Hannah老是跑到他的洋娃娃屋里玩耍, 把自己的纺织作品都小心翼翼地包起来. 
她喜欢把这些展示给他妈妈看. 因为我妹妹很会用缝纫来给娃娃做衣服, 
所以Hannah总会很高兴地为她的娃娃换上妈妈做的衣服. 

"然后她喜欢和娃娃说: '娃娃, 你看, 你可爱的外表真适合这样漂亮衣服. 
你绝对会被警官长邀请到他的州长大厦的晚会去的. ' 哦, 
她还有一个穿着警官衣服扮演警官长角色的娃娃. 只是他长得实在是太瘦小了, 
估计要再多来点结结实实的塑料才能够看起来像是个真正的警官长. "

那个负责放映机的食蚁兽装上了电影胶卷, 然后将投影机对准了后墙. 
电影机的胶卷慢慢地转了起来, 那只食蚁兽选了一张凳子坐了下来. 
渐渐地, 墙上出现了一个绿色的正方形投影, 一只食蚁兽仍然盯着Dr. Cham. 

"你们的电影竟然是彩色的, " Dr. Cham 说到, "多么可爱的小生命啊. "

影片继续播放着: 一个蓝色的正方形. 接着是红色的圆圈. 然后又是一个橙色的方形. 
那只食蚁兽移开了自己的视线, 然后看向了慢慢变成一个粉色三角形的屏幕, 
继续品尝着自己的盘中食. 

一个紫色的星形. 一个红色的方形. 无声的影片静静地播放着, Dr. Cham
可以听见投影仪转动的静静的噪音. 好像是一个缓缓转动的八音盒 -- 
它的齿轮在音轨上拨动, 轻轻地弹奏...

"好啦, 希望你们能享受你们的晚餐, " Dr. Cham如此说道, 然后他礼貌的点了点头, 
沿着来时的路走了出去. 

### Another Dead End Where Things Began
于是Dr. Cham发现自己有一次地在城堡的隧道中迷失了方向. 
周围的东西都看起来十分的陌生. 但是他并不慌 -- 毕竟是在另外一个星球, 
不管怎样都是一种迷路嘛. 

他打算穿过隧道, 试试看能不能重新回忆起来时的路, 但是在好奇心的驱使下, 
他打算干脆直接开始探索脚下的这块新土地. 于是沿着一条隧道, 渐渐地深入, 
慢慢地走向隧道的深处... 脚下的地面逐渐变得陡峭起来, 
以至于他只好小心翼翼地依靠在隧道的边上, 每一步都迈得十分小心. 
因为这个星球的重力感觉和地球没有什么大区别, 所以他便得以轻松地滑了下去. 

尽管不知道自己身在何方, 他却有一种迷之自信, 大概他已经离开了这个城堡的边界了. 
自从他进入城堡的大门后, 他已经在这个深邃漫长的隧道中漫步了将近一个多小时了, 
渐渐地, 隧道转了个弯. 看着这个拐弯, 他抱着"柳暗花明又一村"的想法, 
认为没准在过了这个弯后, 就会遇到一个新的房间, 没准是个什么可以探出的窗口, 
好让自己可以透过这个窗口可以看到窗外的风景. 突然他的脑中闪过一个念头: 
没准自己不应该沿着这条路走着么深的. 于是最后只好抱着
"啊, 希望没有什么冬眠的怪兽在前面等着我吧"的想法继续前进了. 

然后就看到了一个死胡同. 一个黑漆漆的死胡同. 

![at the end of the tunnel: a computer and a book](https://poignant.guide/images/dr.cham-4.gif "在隧道的尽头, 是一台神奇机器和一本书")

因为他有很多的时间, 所以他好好地读了读那本书. 
他读到了小狐狸和他们追击偷走自己的小卡车的小豪猪的故事. 
他也读到了小精灵和火腿的故事. 他看到了自己的漫画, 然后看到了有关自己的故事. 
他也甚至学会了ruby语言. 最后他看到了故事的结尾. 

如果我是他的话, 我估计没法忍受这件事. 但是他却可以. 
然后他在心中默默地记下了接下去的故事. 

在神奇机器的显示器上, Dr. Cham看到一个闪烁着的`irb`提示符. 就像是Dr. Cham一样, 
你没准可以认出`irb`的提示符就是我们在
[The Tiger’s Vest](https://poignant.guide/book/expansion-pak-1.html)
(注: 我翻译了这个, 竟然还是我第一个翻译的文章诶. )
(这篇文章是这本书的第一个拓展部分, 讲的主要是关于Interactive Ruby的简单介绍)

就像是刚刚他用自己的双脚来探索城堡的隧道一样, 
他现在开始尝试着用`irb`的终端(prompt 提示符)来探索这台神奇机器. 
他把之前的书返回了原来的地方. 因为他再也不需要这本书了. 
接下去的故事都不过是他是否要使用ruby的故事了. 

于是他在终端中输入了: 

```
irb> Object::constants
  => ["Marshal", "String", "Dir", "LoadError", "Float", ... and so on ]
```

这行代码将会列出所有的top-level(顶层)的常量. 并且由于类常常会以常数的形式被列出, 
所以在这个数组中就会被输出, 所以你就可以看到上面的结果. 
并且我们就可以利用这样的方式来随时随地地看到在ruby中载入了什么东西. 

他扫了一眼上面的数组: 
`Marshal`, `String`, `Dir`, `LoadError`, `Float`... 这些都是ruby自带的类. 

但是慢慢地往下看, 发现了一些陌生的东西: 

```
... "Struct", "Values", "Time", "Elevator", "Range" ...
```

等等, `Elevator`? 这个好像可不是什么在正经ruby里面的东西. 
看起来没准这个就是一个突破口. 他打算试试: 

```
irb> Elevator::methods
  => ["method", "freeze", "allocate", ... another long list ... ]
irb> Elevator::class_variables
  => ['@@diagnostic_report', '@@power_circuit_active', '@@maintenance_password']
irb> Elevator::constants
  => []
```

看起来`Elevator`类有一堆的方法(methods). 
大多数方法看起来就像是和ruby中的其他正常的对象(object)一样. 举个例子, 
`freeze` 和 `allocate` 方法是所有Ruby中的对象都会有的普通的方法. 
(`Elevator::freeze`方法会组织`Elevator`类被改变. 而`Elevator::allocate`方法, 
则会以不调用`initialize`方法的方式新建一个`Elevator`类的对象, 也就是说, 
是新建一个没有初始化过的对象. )

但是`class_variables`的输出确让Dr. Cham感到有点意思, 看起来这个电梯有些智能. 
但是因为`constants`是一个空数组, 所以在`Elevator`中没有什么嵌套的类. 
(虽然我觉得一般的想法是这个类里面没有常量, 而类也同时是常量的一种吧? )

于是他试了试新建一个`Elevator`的对象: 

```
irb> e = Elevator::new
ArgumentError: wrong number of arguments (0 for 1), requires a password
        from (irb):2:in `initialize'
        from (irb):2:in `new'
        from (irb):2
        from :0
```

(嗯, 要一个密码啊... )于是他试了试几个密码: 

```
irb> e = Elevator::new( "going up" )
AccessDeniedError: bad password
irb> e = Elevator::new( "going_up" )
AccessDeniedError: bad password
irb> e = Elevator::new( "stairs_are_bad" )
AccessDeniedError: bad password
irb> e = Elevator::new( "StairsAreBad" )
AccessDeniedError: bad password
```

就是瞎猜. 呃, 等等! `maintenance_password`, 在前面的`class_varibles`里面列出过: 

```
irb> Elevator::maintenance_password
NoMethodError: undefined method `maintenance_password' for Elevator:Class
        from (irb):1
        from :0
```

哈? 出错了. 这是因为实例变量只能够在对象里面被调用, 而类变量也只在类里面能调用. 
那么我们该怎么办呢? (虽然我们也不是没有... )

```
irb> class Elevator
irb>   def Elevator.maintenance_password
irb>     @@maintenance_password
irb>   end
irb> end
  => nil
irb> Elevator::maintenance_password
  => "stairs_are_history!"
```

好的, 现在(正如你所见的)他得到了密码. 


(上面那个看起来很神奇的操作就是一个ruby的特性, 因为ruby可以让我们在任何时候, 
都打开一个类进行修改等等. ) Dr. Cham向`Elevator`类中加入了一个新的方法. 
和你以为的重新定义`Elevator`类的想法不同, Ruby会直接把你的改动加入到已有的类中. 

类变量往往会用两个艾特号`@@`来放在前面. 但是一个点号`.`实际上也是可以的, 
因为`Elevator`本身就是一个类, 所以在程序执行`Elevator.mainentenance_password`时, 
Ruby会去确认你是否在调用一个类的方法. 
所以`@@`的前缀可以让我们的类名字变得更加直观. 

(感觉这一段说得有些不太清楚, 但是我也get不到那个点... )

> Class methods are usually called with the double colon. 
> But, a period is fine as well. Since Elevator is a class itself, 
> Ruby will figure that if you call Elevator.maintenance_password, 
> you’re calling a class method. The double colon simply helps 
> make class methods obvious to the reader.

所以就像这样. 类的方法会有一点点的特殊. 通常你不会想要在一个类里面直接存放信息. 
但是