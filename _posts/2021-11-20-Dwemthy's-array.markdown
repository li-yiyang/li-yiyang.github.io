---
layout: post
title:  "Why's (poignant) Guide to Ruby(2)"
date:   2021-11-20 19:03:15 +0800
categories: ruby why's_(poignant)_guide_to_ruby
---
#  \$\$\$ DWEMTHY_S ARRAY ^!^ A RUBY MINI_DUNGEON ^!^ ONLY 60 LINES OF CODE ^!^ DWEMTY IS WATCHING ^!^ A STALE BREATH IS ON YOUR NAPE, IS IT NOT ?? ^!^ DWEMTHY COMETH \$\$\$ :: Why's (Poignant) Guide to Ruby 
![Dwenthy's Array](https://poignant.guide/images/dwemthy-header-dissolve.png)

![a ruby mini_adventure](https://poignant.guide/images/dwemthy-logo.gif)

(没错, 又是一篇`_why`的文章, 虽然都是番外性质的文章, 
但我还是在很努力地翻译啦. 这篇文章短小精悍, 
是一个关于类的有趣小故事. )

(这篇文章的网页设计很好, 虽然我的网页是显示不出来了的, 
建议到[原网站](https://poignant.guide/dwemthy/)查看. )

![it was asweome](https://poignant.guide/images/dwemthy-asweome.gif)

## You Are A Rabbit, UNTIL!
在这个游戏中, 你是一个将要被恶龙杀死的小白兔. 
快进入Dwemthy's Array之中吧! 
```ruby
class Dragon < Creature
  life 1340     # tough scales: 强壮的生命力
  strength 451  # bristling veins: 肌肉强壮, 青筋暴露
  charisma 1020 # toothy smile: 特点是邪恶的笑容
  weapon 939    # fire breath: 武器是火焰吐息
end
```
等一下就会有更多的代码. 

等等, 你是不是想问问谁是 **Dwemthy**. 
傻孩子, 他可是至高主宰(mastermind), 没有人知道他在哪里, 
没有人能说出他的真名. 一个伟大王朝在他手下诞生. 
巨人族被他一把火毁灭. 他的邪恶气息散布在空气中让马儿们十分惶恐. 
最可怕的是, 他将 (哔 -- ) 玩弄于掌心... 
(carnal pleasures, 好孩子请不要查这个词. )
想一想这个东西... (啊...)

这就是他的Array. 

![wixi](https://poignant.guide/images/wixl.dwemthy-2.gif)

(这个时候不妨我们来一段美文欣赏: )

> A scalding SEETHING LAVA infiltrates 
> the cacauphonous ENGORGED MINESHAFTS 
> deep within the ageless canopy of 
> the DWEMTHY FOREST... chalky and 
> nocturnal screams from the belly of 
> the RAVENOUS WILD STORKUPINE... 
> who eats wet goslings RIGHT AFTER 
> they've had a few graham crackers and 
> a midday nap... amidst starved hippos 
> TECHNICALLY ORPHANED but truthfully 
> sheltered by umbrellas owned jointly 
> by car dealership conglomerates... 
> beneath uncapped vials of mildly 
> pulpy BLUE ELIXIR... which shall remain... 
> heretofore... UNDISTURBED... DWEMTHY!!!

(能力有限, 只能瞎翻译. 这回是两种语言都能力不够用了. )

这是无边无际的Dwemthy森林. 
犹如鲜血一般的滚热的熔岩嘶吼着涌向矿井中... 

从爱用小饼干喂小鹅, 爱陪着小鹅睡午觉, 
更爱生吃小鹅的可怖的Storkupine的饥饿的胃中传来了阴森可怖的叫声...

在汽车商人的伞下面的阴影里, 
一匹一匹又一匹的瘦骨嶙峋的河马饿得瘫倒在地上...

地上散乱地倒着一瓶又一瓶的大敞着瓶口的酒瓶, 
从里面汩汩地淌出粘稠的蓝色长命水... 
可那应该是Dwemthy的!!! 

和开头的那些可能会又一点误导性的介绍漫画不一样, 
DWEMTHY'S ARRAY可不是什么思维谜题. 
这不过是一个短短的60行的角色扮演小游戏. 
简简单单的一个关于`ruby`元编程的小章节而已. 
这个游戏来自于`_why`的书的
[第六章](https://poignant.guide/book/chapter-6.html). 

假如你不是很能理解Dwemthy's Array, 那完全就是Dwemthy的错. 
因为他把这个游戏设置得太复杂了, 但是假如这个游戏可以简单一点的话, 
那我们又怎么能够在接下来的一段美妙的时间
里享受到一个完美的游戏体验呢? 

## Enough Elaborate Metaphor And Sly Juxtaposition, GIVE US THE CORRESPONDING CODE!
够了, 忘掉那些精妙的比喻, 优雅的铺陈吧, 让我们直接来点代码! 

(不然我就要翻译爆炸了)

首先, 你如果想要玩这个游戏的话, 至少把`ruby`安装在你的电脑里面. 

(以防你不知道, `ruby`是一个"beloved programming language", 
虽然不像`python`之类的那么出名, 但是很好用就是了. )

假如你没有安装的话, 那么你最好赶快从Dwemthy's Array中离开, 
(因为这就像是没有希尔之石的林克却要在海拉鲁大陆生活一样可怕, )
然后为了你的个人安全, 最好对此默不作声, 
(当心Dwemthy, 他的眼线无处不在, 
但是对于你可以交付性命的法律顾问, 
你大可以向他哭诉你在Dwemthy's Array中的骇人见闻). 
假如(勇者先生)你现在想要一个`ruby`, 那可真是太棒了, 
快来看看
[The Tiger's Vest](https://poignant.guide/book/expansion-pak-1.html)
(或者可以去翻我之前的翻译. )

把脚步放轻一点 -- 这就是DWEMTHY'S ARRAY里面的代码!!
```ruby
# The guts of life force within Dwemthy's Array
# 让我们来看看Dwemthy's Array中的生物的结构
class Creature

  # Get a metaclass for this class
  # 得到一个元类(翻译的时候遇到了知识盲区)
  # metaclass: 因为在ruby里面所有东西都是对象, 
  # 所以自然的, 连类class也是对象
  # metaclass就是一个由class创造的class
  def self.metaclass; class << self; self; end; end

  # Advanced metaprogramming code for nice, clean traits
  # 为了优雅和整洁的特性, 我们不妨写一点高级的元编程语言

  def self.traits( *arr )
    return @traits if arr.empty?

    # 1. Set up accessors for each variable
    #    为每一个变量都提供访问的通道
    attr_accessor( *arr )

    # 2. Add a new class method to for each trait.
    #    为每个特性都加上一个对应的类方法
    arr.each do |a|
      metaclass.instance_eval do
        define_method( a ) do |val|
          @traits ||= {}
          @traits[a] = val
        end
      end
    end

    # 3. For each monster, the `initialize' method
    #    should use the default number for each trait.
    #    对于每一个怪兽, 初始化的`initialize'方法
    #    应该对每一个特性都输入默认的值. 
    class_eval do
      define_method( :initialize ) do
        self.class.traits.each do |k,v|
          instance_variable_set("@#{k}", v)
        end
      end
    end

  end

  # Creature attributes are read-only
  # 创造一些只读的变量
  traits :life, :strength, :charisma, :weapon

  # This method applies a hit taken during a fight.
  # 这个方法会在战斗过程中进行攻击
  def hit( damage )
    p_up = rand( charisma )
    if p_up % 9 == 7
      @life += p_up / 4
      puts "[#{ self.class } magick powers up #{ p_up }!]"
    end
    @life -= damage
    puts "[#{ self.class } has died.]" if @life <= 0
  end

  # This method takes one turn in a fight.
  # 这个方法会让用户加入战斗
  def fight( enemy, weapon )
    if life <= 0
      puts "[#{ self.class } is too dead to fight!]"
      return
    end

    # Attack the opponent
    # 攻击对手
    your_hit = rand( strength + weapon )
    puts "[You hit with #{ your_hit } points of damage!]"
    enemy.hit( your_hit )

    # Retaliation
    # 反击
    p enemy
    if enemy.life > 0
      enemy_hit = rand( enemy.strength + enemy.weapon )
      puts "[Your enemy hit with #{ enemy_hit } points of damage!]"
      self.hit( enemy_hit )
    end
  end

end

class DwemthysArray < Array
  alias _inspect inspect
  def inspect; "#<#{ self.class }#{ _inspect }>"; end
  def method_missing( meth, *args )
    answer = first.send( meth, *args )
    if first.life <= 0
      shift
      if empty?
        puts "[Whoa.  You decimated Dwemthy's Array!]"
      else
        puts "[Get ready. #{ first.class } has emerged.]"
      end
    end
    answer || 0
  end
end
```

想要开始玩的话, 打开`irb`. 把上面的代码复制粘贴到上面. 
(活着你也可以通过新建一个叫做`dwemthy.rb`的文档, 
然后在`irb`里面通过`require 'dwemthy'`的方式来调用. )

## Introducing: You
有请你来自我介绍一下: 

你应当对自己 -- 也就是这只小白兔做一点定义. 
因为你是`Rabbit`类的一个对象, 
而`Rabbit`又是`Creature`的一个派生(子类). 
(在面向对象的编程中, 有一些叫做类的东西, 有点像是生物的分类学, 
同一类的东西有相同的方法, 数据结构之类的东西. 
是一个相对来说可以简化编程的一个好东西. )
你现在只需要利用类的继承关系, 就可以声明`Rabbit`类了. 
```ruby
class Rabbit < Creature
  traits :bombs

  life 10
  strength 2
  charisma 44
  weapon 4
  bombs 3

  # little boomerang
  # 小小的回旋镖
  def ^( enemy )
    fight( enemy, 13 )
  end
  # the hero's sword is unlimited!!
  # 英雄之剑是无限的
  def /( enemy )
    fight( enemy, rand( 4 + ( ( enemy.life % 10 ) ** 2 ) ) )
  end
  # lettuce will build your strength and extra ruffage
  # will fly in the face of your opponent!!
  # 蔬菜可以增强你的身体并且额外的ruffage(? 碎末)会飞到敌人的脸上
  def %( enemy )
    lettuce = rand( charisma )
    puts "[Healthy lettuce gives you #{ lettuce } life points!!]"
    @life += lettuce
    fight( enemy, 0 )
  end
  # bombs, but you only have three!!
  # 炸弹, 但是你只有三个
  def *( enemy )
    if @bombs.zero?
      puts "[UHN!! You're out of bombs!!]"
      return
    end
    @bombs -= 1
    fight( enemy, 86 )
  end
end
```

你只有四件武器, 我认为我已经将它们介绍的很清楚了. 

## Rabbit Fights ScubaArgentine!
请不要急匆匆地还没有准备好就直接冲进Dwemthy's Array里面!! 
你可得先经过恶魔们的试炼关卡, 或者北上通过煤炭的魔窟. 

所以我们首先不妨先人工演练一番, 就当作是新手教学吧. 
第一个敌人: ScubaArgentine!!!
```ruby
class ScubaArgentine < Creature
  life 46
  strength 35
  charisma 91
  weapon 2
end
```

想要开始战斗, 首先确保你已经建立了一个你和ScubaArgentine的实例: 
```ruby
r = Rabbit.new
s = ScubaArgentine.new
```

现在让我们开始战斗(首先试一试小小的回旋镖如何? )
```
>> r ^ s
[You hit with 6 points of damage!]
#<ScubaArgentine:0x0000aaab0d8bc9b0 @life=40, @strength=35, @charisma=91, @weapon=2>
[Your enemy hit with 16 points of damage!]
[Rabbit magick powers up 7!]
[Rabbit has died.]
 => nil 
```

啊嗷!! 我们的小兔子被打爆了!!!

啊, 不要铁青着脸嘛. 我不会让你回到兔子王国中从头再来的. 
你只要假装自己没死然后重新生成一个新的兔子实例就好了. 

## The Harsh Realities of Dwemthy's Array AWAIT YOU TO MASH YOU!!
*更多残酷的挑战还在前面呢...*

当你终于将那家伙击倒在地, 奄奄一息时, 那么是时候进入The Array了. 
我觉得你可能很难通关. 因为你竟然把你的小斧头落在了家里面! 
(哦, 温馨提示, 不要把你宝贵的炸弹一下子全浪费在那些小怪上了. )

六个对手, 现在上场! 

```ruby
# 爱做恶作剧的赛博猴
class IndustrialRaverMonkey < Creature
  life 46
  strength 35
  charisma 91
  weapon 2
end

# 堕天使
class DwarvenAngel < Creature
  life 540
  strength 6
  charisma 144
  weapon 50
end

# (显然是米国的)政府官员和他们的邪恶触手
class AssistantViceTentacleAndOmbudsman < Creature
  life 320
  strength 6
  charisma 144
  weapon 50
end

# 利齿鹿
class TeethDeer < Creature
  life 655
  strength 192
  charisma 19
  weapon 109
end

# 无畏的丧尸骑兵
class IntrepidDecomposedCyclist < Creature
  life 901
  strength 560
  charisma 422
  weapon 105
end

# 恶龙
class Dragon < Creature
  life 1340     # tough scales
  strength 451  # bristling veins
  charisma 1020 # toothy smile
  weapon 939    # fire breath
end
```

这些就是在Dwemthy's Array中的邪恶的怪兽们, 
大张着他们凶残暴虐的嘴, 从鼻子里吐出令人作呕的死亡吐息... 
我也不知道他们是怎么到那的 -- 没人知道这些东西. 说真的, 
要我猜的话, 我觉得IntrepidDecomposedCyclist
(无畏的丧尸骑兵)大概是驾着他的十速(ten-speed)快车来的. 
不过其他的嘛: **谁知道**...

如果你实在要较真的话, 那我们不妨就说他们就是在那出生的, 
好吗? 哦, 请别再追问了, 我们能不能就这样跳过这个问题?? 

好的, 当我们不断的深入Dwemthy's Array, 
我们会遇到更多的可怕的怪物. 
```ruby
dwarr = DwemthysArray[IndustrialRaverMonkey.new,
                      DwarvenAngel.new,
                      AssistantViceTentacleAndOmbudsman.new,
                      TeethDeer.new,
                      IntrepidDecomposedCyclist.new,
                      Dragon.new]
```

在这个`Array`中一路战斗吧, 无畏的勇士兔! 
愿上帝保佑你能平安归来, 幸运天使将在你的肩上落脚. 
相信你一定可以带着惊险刺激的冒险故事凯旋而归! 

哦, 别再说什么"我还年轻, 我不敢死"的鬼话了, 我可受够这些了. 
(题外话: 老林克已经从塔上面摔死了好多次了. QAQ)
这样的鬼话难道不是在侮辱我们这些还没死的年轻人吗? 
他们可是我们的未来! 当我们的未来终于结束了之后, 
那就终将是他们来替我们续写故事. 

![the end](https://poignant.guide/images/wixl.dwemthy-3.gif)

## 译后记
吼吼吼吼吼!!!