---
layout: post
title:  "无聊的小游戏制作[进行中...]"
date:   2022-02-04 19:42:11 +0800
categories: jekyll update
---
# Ruby & Gosu Make A Game
之前的博客基本上是学习的笔记的记录, 感觉自己输出的有点少了, 
所以来点新鲜的, 写一个无聊的小游戏的制作, 
(又: 这个小游戏还是我在国航航班上玩到的一个小游戏, 有些魔性, 也有些好玩. )
来作为ruby的一个简单的推广. 

我会尽量只用最简单的知识来写的. 
(因为我只会简单的编程啊, 不会什么高级操作, 不知道能不能成功, 
这里就当做是一个记录吧. )

(假如之后代码写得太烂了, 请不要喷. ~~狗头~~)

## 前期准备
### 游戏设计和规划
虽然这个是一个很重要的事情, 但是因为我是一个复刻的操作(~~老腾讯了~~), 
所以就省了. 简单介绍一下这个游戏的大概内容吧: 

* 就是飞机打障碍物
* 飞机撞到障碍物就会减血
* 飞机的子弹有三种模式: 单发, 双射, 四射
* 障碍物有数字来标记它剩下的血量
* 障碍物落到地上会弹回去

### Ruby Basic
说实话, 我也没有什么系统地学过编程, 只有一点点基础, 
虽然我觉得应该是够用了. (虽然网上的Ruby教程很少, 并且质量也, 
挺一般的, 更多是像手册一样的参考资料. )

这里列举一些我觉得很好用的资源吧: 
* [Ruby in 20 mins](https://www.ruby-lang.org/en/documentation/quickstart/)    
  一开始我真的是看这个学的, ~~因为我比较怕麻烦~~
* [why's (poignant) guide to ruby](https://poignant.guide)    
  但是说起来, 这个教程才是我真正想学ruby的原因, ~~因为很帅~~

(嘛, 实在懒得学的话, 我觉得可以直接上手看代码, 这样简单一点. 
我下面的文章将假设你什么也不会, 直接开始也不是不行. 
如果你会一些编程基础的话, 还请直接跳过我的一些废话, 看看代码估计就很不错了. )

安装方式看[官网](https://www.ruby-lang.org/en/downloads/), 
windows下的话可以看看[windowsInstaller](https://rubyinstaller.org), 
(温馨提示: 建议选择with devkit的版本的)
linux/mac下可以用[rvm](http://rvm.io). 

(~~假如遇到了网络的问题, 还请自行找可靠的方法, 不是~~, 
windowsInstaller有一个[国内的镜像](https://rubyinstaller.cn), 
rvm没找到比较正式的方法)

### Gems
ruby的软件包管理器. 可以方便地安装已有的软件包, 并且解决软件包的安装问题, 
并且还可以发布自己的代码. 不过这个不是重点, 
我应该用到的就只有安装和删除等很少的功能. 

* 安装软件包`gem install <package_name>`
* 删除软件包`gem remove <package_name>`

([国内镜像网站](https://gems.ruby-china.com), 并且还有配置的教程. )

(用一个很中二的想法来看就是: gem就像是钢铁侠的武器库, 
可以随时随地地调用各种乱七八糟的神仙武器. )

### Gosu
(遣词造句尝试解释失败, 直接复制粘贴[官网](https://www.libgosu.org)的介绍算了)

> Gosu is a 2D game development library for Ruby and C++.
> 
> It’s available for macOS, Windows, Linux, and iOS.
> 
> Gosu is focused, lightweight and has few dependencies (mostly SDL 2).

(关于GUI: 其实有一个叫做[shoes](http://shoesrb.com)的框架, 也是非常好用的, 
以前用过, 各种控件比较完善, 感觉适合简单的小程序的制作. 
可惜很久都没有更新了. ~~让人怀疑是不是跑路~~; 或者就是那种普普通通的gtk之类的, 
没有用过所以不太好说. )

安装方式: `gem install gosu`

(不知道windows的版本要不要什么devkit的安装)

**简单的原理的介绍**: 

对于一个gosu程序的代码里面, 一般会有类似于下面的结构: 

(现在暂时不需要理解这些代码)

```ruby
require 'gosu'

class GameWindow < Gosu::Window
  def initialize
  end

  def update
  end

  def draw
  end 
end
```

阅读[文档](https://github.com/gosu/gosu/wiki/Window-Main-Loop)
(虽然我只读了一点点), 可以知道在一个gosu的逻辑过程里面, 
有这样的一个循环过程: 接受输入, 更新`update`, 然后是`draw`画出界面. 
(可能需要科学上网才能看到, 因为图片的原地址是github上的, 而github国内却时常抽风... )

![官方的图片](https://github.com/gosu/gosu/wiki/main_loop.png "这个图画的很形象了")

这个的大概意思用**离谱**代码来写就是: 

```ruby
loop do
  update
  draw
  sleep(update_interval)
end
```

大概的意思是这样的, 
于是只要按照这样的逻辑顺序在合适的位置上准备我们的代码就可以了. 

### Others
* 素材资源:     
  * [OpenGameArt](https://opengameart.org)    
    是一个"开源"游戏素材库, 里面有很多的素材
  * 自己画

## Basic
### Basic I: 出现一个窗口
选择一个自己顺手的路径并新建一个文件夹吧. 
然后新建一个ruby的代码文件, 不妨叫做`main.rb`吧. 

在`main.rb`里面填写上上面的代码: 

```ruby
require 'gosu'

class GameWindow < Gosu::Window
  # 初始化
  def initialize
    # 设置窗口的大小, 宽度为480, 长度为680
    super 480, 680
    # 设置窗口的标题
    self.caption = "Little Game"
  end

  # 更新
  def update
  end

  # 绘制界面
  def draw
  end 
end

# 打开窗口
GameWindow.new.show
```

从头到脚开始看, 首先是一个`require`语句, 
这个语句的作用是告诉ruby我们要使用一个叫做`gosu`的库. 
(当然, 前提是你已经安装了这个库, 假如没有, 请看上面的gosu介绍)

接下来是一个`class ... end`的结构, 我们只需要这就像是一个括号, 
里面包装的内容是什么不是特别重要, (因为这就是一种抽象的东西) 
重要的是我们用这样的括号定义了一个叫做`GameWindow`的东西, 
然后`GameWindow < Gosu::Window`说明的是`GameWindow`是一个`Gosu::Window`的子类. 

类是一种面向对象编程思想里面的概念: 
面向对象程序设计可以看作一种在程序中包含各种独立而又互相调用的对象的思想, 
(来自[wikipedia](https://zh.wikipedia.org/wiki/面向对象程序设计))
类似于这样: 有不同类型的玩具(对象), 不同的玩具有不同的功能和属性, 
同一类的玩具有相同的功能. 然后我们编程的时候就可以通过操作对象来完成各种操作. 
(就好像是`toy.wear(:dress)`就是让`toy`对象穿上`:dress`裙子, 
具体的话可以看看[why的教程](https://poignant.guide/dwemthy/), 
我的翻译在[这里](../../..//2021/11/20/Dwemthy's-array.html). 
假如不想深入了解的话, 展示先放放也不是不行. )

那么我们先不管`GameWindow`里面发生了什么, 跳过这里看到下面的代码: 
`GameWindow.new`告诉计算机我们要新建一个新的`GameWindow`类的对象实例. 
对于ruby来说, 没新建一个对象实例的时候, 
就会自动的调用`initialize`方法来对这个对象进行初始化, 在这个初始化的过程中, 
我们可以设置很多关于这个对象的东西. 比如设置了窗口的大小, 窗口的标题等等. 

接下来又对这个对象调用了一个`show`的方法(method), 让这个窗口能够显示出来. 
这个方法我们可以不必太关心它的实现, 因为这个是gosu库实现的. 

于是运行这段代码, 我们可以得到一个黑色的窗口: 

![黑色窗口]({{ site.github.url }}/_img/i-am-boring/black-window.png "亘古之初, 啥也没有, 就是一坨黑")

(虽然我用的是macOS, 但是windows子类的应该是类似的. )

嗯, 是不是觉得这个黑色的窗口有一些单调, 那么我们可以为这个窗口加上一些背景, 
因为是太空设计小游戏, 所以我画了一个太空背景, 放在`rec`文件夹中, 命名为`background.png`. 

为了让这个图片背景能够在我们的窗口中, 我们就需要把这个图片载入到程序里面, 
然后在画图的时候画出来在屏幕上, 为此, 我们需要修改一下代码: 

首先是载入图片: 

```ruby
  def initialize
    # 设置窗口的大小
    super 480, 680
    # 设置窗口的标题
    self.caption = "Little Game"

    # 载入图片
    @background = Gosu::Image.new("rec/background.png")
  end
```

其中`@background`是一个实例变量, 相较于普通的变量来说, 
实例变量是在一个实例内都可以随便访问的, 而普通的变量可以访问的范围是有限的. 
这是ruby里一个叫做scope(范围, 领域)的东西. 有点像是一个维恩图, 
变量的作用范围是被限定在有限范围的. 

这里之所以要使用实例变量, 是因为我们想要在初始化之后, 
可以在每一次绘制界面的时都直接调用这个背景图片, 然后让图片在窗口显示. 
假如只是普通变量的话, 那可就没法在不同的方法scope中访问了. 

于是我们就可以在绘制界面里面绘制图片了. 查阅文档: 

> #draw(x, y, z = 0, scale_x = 1, scale_y = 1, color = 0xff_ffffff, mode = :default) => void
> 
> Draws the image with its top left corner at (x, y).

这里暂时不必计较其他的参数, 只需要关心前两个参数, 也就是画图的位置. 

```ruby
  def draw
    @background.draw(0, 0)
  end
```

这样就可以画出背景了. 这里解释一下, 在gosu中的坐标是以窗口的左上角为原点, 
向右为x轴正方向, 向下为y轴正方向. 

### Basic II: 画一个飞机
射击游戏没有什么飞机开炮那不是太没意思了, 那么我们就画一个飞机吧. 
(嗯, 手绘板还在路上, 那么我就只好用笔记本的触摸板来展示一波灵魂画技吧. )

为了方便操控, 我们不妨新建一个飞机的类: 

```ruby
# 飞机
class Plane      
  # 初始化的时候在窗口的底部中间  
  def initialize  
    # @x, @y分别是横竖坐标  
    @x, @y = 240, 600                                                                
    # 飞机的图像  
    @image = Gosu::Image.new("rec/plane.png")     
  end                                                                                
  
  # 画出飞机  
  def draw  
    @image.draw(@x, @y, 1)
  end
end
```

然后在`initialize`方法里面加入一些代码`@player = Plane.new`, 
在`draw`方法里面加入`@player.draw`, 这样就可以让飞机出现在屏幕上了. 

这里解释一下, 我们的飞机肯定是要在背景的上面的, 不然被背景遮住不是很尴尬? 
(这波我在你的上一层... )所以我们将`Image#draw`方法默认的`z`参数写成了`1`, 
也就是向上移动了一层, 显示在了背景上面. 

但是飞机肯定是要会飞的. 不会动还怎么玩? 这个时候我们就需要接受键盘的输入, 
然后控制飞机运动了. 为了达到这个目的, gosu里面提供了一个方法: 

> `.button_down?(id)` => true, false
> 
> Returns whether the button `id` is currently pressed. 
> Button states are updated once per tick, 
> so repeated calls during the same tick will always yield the same result.

注意这一的`id`也就是系统对应的键盘信号, 虽然每个系统的对应信号不一定一样, 
并且我们也没有必要真的知道是什么, 因为gosu库中已经为这些做好了封装, 
只要调用`Gosu::KB_LEFT`等常量即可. 

这样我们就可以下手了, 在`GameWindow.update`里面写一些代码: 

```ruby
  def update                                                                         
    @player.mv_left if button_down?(Gosu::KB_LEFT)                                   
    @player.mv_right if button_down?(Gosu::KB_RIGHT)                                 
  end
```

(这里可能你会感到奇怪, `.button_down`是怎么调用的? 
~~这就不得不讲一个关于命名空间的笑话了~~. 
这是因为我们是在类中写代码的, 所以我们可以直接调用这个方法, 
或者是用`self.button_down?`的方式在自己内部调用自己的方法. )

然后还要让我们的飞机能够拥有对应的左右移动的能力: 

```ruby
class Plane
  # 前面的略去

  # 向左移动      
  def mv_left      
    @x -= 10      
  end                
      
  # 向右移动      
  def mv_right      
    @x += 10      
  end                
end
```

(这样一来就可以享受左右横跳的快乐了. )

### Basic III: 飞机的一点点改进
(郑重声明: 我没有强迫症)只是我们会发现, 
我们的飞机显示的还是有那么点不尽人意的, 
虽然我们一开始设置的默认位置是在屏幕中间, 
但是实际上飞机显示出来的却是有那么些位移. 
这是因为飞机的图像在绘制的时候是以我们设置的`@x`, `@y`为原点, 
向右向下绘制的, 而不是以`@x`, `@y`为原点绘制的, 这样多少还是有点差别. 

更加离谱的是, 我们的飞机还可以肆意妄为地飞出屏幕, 这样不是太离谱了吗? 

所以我们要做的就是将这个误差减少并且让这个飞机更加的合理: 

为此, 我们需要一个对飞机中心坐标还有左上角的绘制坐标之间的转换代码, 
以及需要一个判断飞机是否飞出有效区域的代码, 用这个代码来限制飞机能否继续飞. 
最后还要保证原来的代码不必有太多的修改(毕竟这样简单一些). 

首先查阅[文档](https://www.rubydoc.info/gems/gosu/Gosu/Image), 
可以知道载入的图片的大小是可以通过简单的常量调用就可以得知的: 

> Instance Attribute Summary
> 
> `#height => Integer` **readonly**    
> The image's height, in pixels.
> 
> `#width => Integer` **readonly**    
> The image's width, in pixels.

于是我们就可以在`Plane#initialize`的时候进行一些新的补充: 

```ruby
class Plane  
  # 初始化的时候在窗口的底部中间  
  def initialize  
    # 飞机的图像  
    @image = Gosu::Image.new("rec/plane.png")      
    # 图像的一半长宽  
    @hf_width = @image.width / 2  
    @hf_height = @image.height / 2                                                   
    
    # @x, @y分别是中心坐标坐标  
    # 在绘制的时候就需要计算一下坐标的位置  
    # 也就是@x - @hf_width, @y - @hf_height  
    @x, @y = 240, 600                                
  end                                                                                
                                                                                     
  # 画出飞机                                                                         
  def draw                                                                           
    @image.draw(@x - @hf_width, @y - @hf_height, 1)                                  
  end                                                                                
                                                                                     
  # 向左移动                                                                         
  def mv_left
    # 防止飞出左边                                                                        
    @x -= 10 if @x > 0                                                               
  end                                                                                
                                                                                     
  # 向右移动                                                                         
  def mv_right
    # 防止飞出右边                                                                       
    @x += 10 if @x < 480                                                             
  end                                                                                
end
```

### Basic IV: 如法炮制的子弹和炮弹
类似于`Plane`的做法, 我们可以简单的写出子弹`Bullet`类, 障碍物`Ball`类的代码: 

首先是子弹, 飞机一开始只能射击单发的子弹, 接下去确可以射击并排的双发, 
最后在双发的基础上射击向两边散开的子弹. 

所以在程序设计的时候, 我们要考虑有不同档位的子弹. 不过一开始, 
我们还是先实现最简单的子弹设计吧: (单发)

```ruby
class Bullet
  # 输入子弹的起点坐标x, y
  def initialize(x, y)
    # 子弹的图像
    @image = Gosu::Image.new("rec/bullet.png")
    @hf_height = @image.width / 2
    @hf_width = @image.height / 2

    # 还是一样的, 中心坐标@x, @y             
    @x, @y = x, y                            
  end                                        
                                             
  # 目前是向上移动                           
  def move                                   
    @y -= 15                                 
  end                                        
                                                       
  # 子弹在飞机的上一层                                   
  def draw                                               
    @image.draw(@x - @hf_width, @y - @hf_height, 2)      
  end                                                             
end

class Plane
  def shoot(bullets)
    bullets << Bullet.new(@x, @y - @hf_height)
  end
end

class GameWindow < Gosu::Window
  def initialize
    # ...
    # 子弹的数组                             
    @bullets = []
  end

  def update
    # ...
    @player.shoot(@bullets) if button_down?(Gosu::KB_UP)

    # 子弹的移动
    @bullets.each { |bullet| bullet.move }
  end

  def draw
    # ... 
    @bullets.each { |bullet| bullet.draw }
  end
end
```

这样以来运行程序的时候我们就可以按住上方向键来射击了. 唯一的问题就是, 
当我们发射了很多子弹的时候, 我们的程序就会变得很慢, 
这是因为我们的`@bullets`数组里面储存了太多的无用子弹了. 
(这些子弹都飞到了屏幕外面看不到了, 却还会继续在代码里面碍事, 
所以我们要像一个办法把这些子弹给删掉. )

所以第一个优化的地方出现了: 如何删掉无用的子弹? 答案是我们可以像飞机的移动一样做一个判断. 

```ruby
class Bullet
  # 无用子弹的判断                        
  def within?                                     
    @y > 0 && @x > 0 && @x < 480 
  end  
end
```

这里我们用了一个ruby的特性: 函数的返回值在没有`return`显式声明的时候, 
就是最后一个表达式的返回值, 也就是我们这里写的`@y > 0 && @x > 0 && @x < 480`的返回值. 
这个表达式是一个逻辑表达式, 它的意思是: 只有当`@y > 0`和`@x > 0`以及`@x < 480`都成立的时候, 
我们才会返回真`true`. 

(没什么用的小知识: 在ruby里面, 大家一般都会用类似的方式来命名自己的函数之类的, 
比如说返回值是`true`或者`false`之类的判断方法, 就会用一个`?`来结尾, 
象征着查询的意思; 然后那些会修改自身数据的方法, 因为比较危险, 需要注意, 
所以就会用一个`!`来强调. 并且一般也相应对应着一个不那么危险的没有感叹号的方法. )

```ruby
class GameWindow < Gosu::Window
  def update
    # ...
    # 删除无用的子弹                                                                 
    i = 0                                                                            
    while i < @bullets.length                                                        
      unless @bullets[i].within?                                                     
        @bullets.delete_at(i)                                                        
      else                                                                           
        i += 1                                                                       
      end                                                                            
    end                                                                              
  end
end
```

等等, 还是有那么点点的小问题的: 我们子弹射得太快了. 一口气就会射出太多的子弹, 
所以我们要为这个射击加一个间隔时间. 为此只需要在`Plane`类里面加入一个`@last_shoot_time`, 
每次射击的时候就会判断时间间隔是否满足条件. 

```ruby
class Plane
  def initialize
    # ...
    # 前一次的射击时间
    @last_shoot_time = Time.new
  end

  def shoot(bullets)
    if Time.new - @last_shoot_time > 0.2
      bullets << Bullet.new(@x, @y - @hf_height)
      @last_shoot_time = Time.new
    end
  end
end
```

那么继续, 趁热打铁来加入`Ball`类吧. 这里我们暂时不管受到攻击之类的, 
但是打算用一些有意思一些的东西来让这个球画出来: 

还是之前就看过的`draw`方法, 这个时候我们来用下之前没有被用过的参数: 

> `#draw(x, y, z = 0, scale_x = 1, scale_y = 1, color = 0xff_ffffff, mode = :default)` => void
> 
> Draws the image with its top left corner at (x, y).

(补充一个没什么用的小知识: 对于RGB色彩模式来说, 每一个颜色都是由三种基本颜色合成的. 
这三种颜色分别是R(ed)红色, G(reen)绿色, 还有B(lue)蓝色. 
不同颜色混合的多少就决定了是什么颜色. 这就是我们的颜色的`0xff_ffffff`后面几位的意义, 
那么前面的`ff`是什么? 那就是透明度. 假如有兴趣的可以修改一下颜色的透明度之类的. )

```ruby
class Ball
  def initialize
    @image = Gosu::Image.new("rec/ball.png")
    @scale = rand(4..10)
    @hf_height = (@image.height / 2) * @scale
    @hf_width = (@image.width / 2) * @scale

    @x, @y = rand(480), 0 
    @v = 0
  end

  def move
    @y += @v
    @v *= -1 if @y > 600 && @v > 0                         
    @v += 0.1 unless @y > 600                                
  end                                                      
                                                           
  def draw                                                 
    # 球在子弹的上一层                                     
    @image.draw(@x - @hf_width, @y - @hf_height, 3, @scale, @scale)  
  end                                                                         
end

class GameWindow < Gosu::Window
  # 初始化
  def initialize
    # ...
    # 球的数组
    @balls = []
  end

  def update
    # ...
    # 球的移动
    @balls.each do |ball|
      ball.move
    end
    # 删除没用的球, 删除没用的子弹等
  end

  # 绘制界面
  def draw
    # ...
    @balls.each { |ball| ball.draw }
  end
end
```

这里解释一下代码: 
* 我们定义了一个`@scale`来缩放球的大小
* 然后让这个球的出现是在x方向上随机分布的
* 球的运动是模拟了一个自由落体运动

这样一对操作之后, 我们运行代码就可以得到一个比较好的结果了: 

![一个比较好的结果]({{ site.github.url }}/_img/i-am-boring/a-good-result.png "有点感觉了吧?")

## 让游戏活起来
毕竟上面的那些简单的代码并不能让我们的游戏变得合理, 相反的, 这让我们的游戏有点麻烦, 
因为我们的球的行为很奇怪: 撞到子弹不会消失, 撞到飞机不会爆炸等等. 
所以我们需要让游戏变得更加的合理. 

所以我们要给我们的球加上一些生命, 然后让我们的子弹能够在击中后消失, 
让我们的飞机在碰到球后会受伤... 

### 给球以生命
让我们给`Ball`一个生命`@life`, 在每次受到子弹撞击的时候就把自己的生命`-1`, 
然后在`@life <= 0`时出局. 这样就可以解决杀不死球的问题了. 

(先这样, 第一部分结束了, 大概用上面的知识就可以继续做下去了, 
所以我将在之后发布第二部分的内容. ~~绝对不是我想跑路的原因.~~)

(开始啦... )

```ruby
class Ball
  def initialize
    # ...
    @life = rand(3..5)
  end

  def hit_bullets?(bullets)
    bullets.each do |bullet|
      if hit?(bullet.pos)
        @life -= 1
        bullet.hit!
      end
    end
  end

  def hit?(pos)
    x, y = pos
    distance = ((x - @x) ** 2 + (y - @y) ** 2) ** 0.5
    distance <= @hf_height
  end

  def hit_bullets?(bullets)    
    bullets.each do |bullet|    
      if hit?(bullet.pos)    
        @life -= 1    
        bullet.hit!    
        return true    
      end    
    end    
    return false    
  end    
    
  def hit_plane?(plane)    
    if hit?(plane.pos)
      @life = 0
      plane.hit!
      return true
    end
    return false
  end

  def within?
    @life > 0
  end
end

class Plane
  # 初始化的时候在窗口的底部中间
  def initialize
    # 生命值
    @life = 3
  end

  def pos
    return @x, @y
  end

  def hit!
    @life -= 1
  end

  def life
    @life
  end
end

class Bullet
  def initialize(x, y)
    # ...
    @life = 1
  end

  def pos
    return @x, @y
  end

  def hit!
    @life -= 1
  end
end

class GameWindow < Gosu::Window
  # 初始化
  def initialize
    # ...
    # 球的数组
    @balls = []
  end
  
  # 更新
  def update
    # ...
    # 新增加一个球
    if rand(200) == 0
      @balls << Ball.new
    end

    # 球的移动
    @balls.each do |ball|
      ball.move
      ball.hit_bullets?(@bullets)
      ball.hit_plane?(@player)
    end

    # ...
    delete_useless_balls
  end

  # 绘制界面
  def draw
    # ...
    @balls.each { |ball| ball.draw }
  end
  
  # 删除死掉的Ball
  def delete_useless_balls
    i = 0
    while i < @balls.length
      unless @balls[i].within?  
        @balls.delete_at(i)
        # puts "delete"
      else
        i += 1
      end
    end
  end
end
```

上面的代码也是很好理解的吧? ~~所以我就不解释了.~~ 
简单地解释一下: 
* 首先是两个命中判定的函数`hit_bullets?`和`hit_plane?`, 
  这两个的逻辑是类似的, 都是利用的是`hit?`方法来判断有没有相撞. 
  那么相撞的判断是什么呢? 很简单, 就是我们熟悉的勾股定理. 
* 接下来是我们的删除代码: 因为被打爆的球要从屏幕上消失, 
  所以我们可以利用和删除无用子弹一样的代码来处理, 重复利用了
  (甚至我觉得可以直接写成一个代码, 但是暂时我们不必做得那么绝. )
* 为了达到类似的效果, 我们还需要做到对`Plane`类和`Bullet`类进行一些修改, 
  也就是要达到让他们能够被击中的效果

### 简单的交互
最后我们运行程序就可以达到了射击和击中的效果了, 但是是不是还差一点? 
我们的交互有点差: 我们要知道自己还剩下多少血量, 要知道自己的分数, 
等等... 这样才好玩嘛. 所以这个时候我们就要知道如何让gosu显示文字. 

[文档在此](https://www.rubydoc.info/gems/gosu/Gosu/Font), 
那么我们需要的是一个`draw_text`的函数: 

> `#initialize(height, options = {})` => Font
> 
> Load a font from the system fonts or a file.
> 
> `#draw_text(text, x, y, z, scale_x = 1, scale_y = 1, color = 0xff_ffffff, mode = :default)` => void
> 
> This method returns an undefined value.
> 
> Draws a single line of text with its top left corner at (x, y).

于是我们可以修改代码让我们的窗口的左上角能够显示一个我们剩余的生命值: 

```ruby
class GameWindow < Gosu::Window
  # 初始化
  def initialize
    # ...
    # 文字
    @text = Gosu::Font.new(20)
  end

  def draw
    # ...
    @text.draw_text("Life: #{@player.life}", 10, 10, 4)
  end
end
```

这里提供一个简单的解释: 我们的`@text`是一个`Gosu::Font`类, 也就是载入一个字体, 
然后设置了字体的高度, 也就是常见的字号大小, 这里的单位是px也就是像素, 
用这个字体, 我们可以在(10, 10)坐标的地方画出我们的文字`"Life: #{@player.life}"`, 
这个文字利用了ruby的一个字符串的特性: 就是会将双引号中`#{}`里面的内容`.to_s`后插入到字符串中. 

类似的, 我们还可以加入分数: 

```ruby
class GameWindow < Gosu::Window
  # 初始化
  def initialize
    # ...
    # 分数  
    @score = 0  
  end

  # 更新
  def update
    # ...
    # 球的移动                                        
    @balls.each do |ball|                             
      ball.move                                       
      @score += 1 if ball.hit_bullets?(@bullets)  
      ball.hit_plane?(@player)                                    
    end                                                           
  end

  def draw
    # ...
    @text.draw_text("Score: #{@score}\nLife: #{@player.life}", 10, 10, 4)
  end
end
```

这里我们用了我们之前写的`ball.hit_bullets?`方法的返回值, 
这样的话我们的程序就看起来像是那么回事了: 

![煞有其事]({{ site.github.url }}/_img/i-am-boring/not-bad.png "不错哦")

### 这个数字是什么意思? - 简单的优化
虽然我的程序运行起来人模狗样的, 但是我的代码并不是那么友好: 

里面有太多的不知道有什么意义的数字: 比如是设置`draw`的`z`值的`1`, `2`等, 
很容易让人看晕了, 并且假如我们想要进一步修改的话, 就会遇到让人混乱的修改问题 -- 
我们可能改了这又要修改那, 这也太麻烦了. 

(这样的代码很容易让人今天写完代码, 明天就忘记了自己写的是啥, 看代码就会看不懂. )

所以这一部分, 我们不必关注如何增加我们程序的功能, 而是简单地关注一下如何让代码变得好看, 
毕竟这是一个看脸的时代. 

首先, 我们需要把代码里面的那些无意义的数字给换掉, 
这个时候我们可以利用ruby中的`module`来将这些常数给命名: 

```ruby
module ZOrder
  BACKGROUND, PLANE, BULLET, BALL, UI = (0..4).to_a
end
```

然后我们在相应的地方就可以调用这样的代码, 就可以保证调用的时候很友好了, 
比如说这里以飞机为例: 

```ruby
class Plane
  # 画出飞机            
  def draw                                   
    @image.draw(@x - @hf_width, @y - @hf_height, ZOrder::PLANE)
  end
end
```

这样就很容易区分了. 

(并且除此之外, 使用`module`还可以让我们把方法method打包在一个命名空间里面, 
将相同的方法进行分发和包装, 达到将代码整洁的作用, 但是目前我没怎么用过, 
所以以后再试试. )

## 加入不同的场景
虽然我们的游戏可以玩了, 但是我们现在的游戏不仅没有结束, 
(你甚至可以看到生命值为负的情况, 这样不就是无敌作弊版嘛? )
还没有开始, 那么玩家们该如何知道要如何使用这个游戏呢? 
所以我们需要加入一些不同的场景. 

(因为我好像没有看到过类似的东西, 所以我们需要自己从头开始写类似的场景切换代码, 
可能是有类似的, 但是我也不是一个高手, 只是一个凭兴趣学学的门外汉而已. 
所以我们就用土味`case`语句来做到场景切换吧. )

### 将我们手头的场景打包成`playing`场景
首先我们为`GameWindow`加入一个`@scene`变量, 里面储存着我们目前的场景, 
然后把原来的所有的`update`和`draw`代码重新命名为`playing_update`和`playing_draw`, 
在新的场景里面进行调用. 

(在写到这段的时候, 我突然想到了一个很重要的事情: 这个游戏没有音效!!! 
嘛, 也难怪, 毕竟我是个除了看番听歌, 电脑手机常年静音的鸟人. 忘记很正常. 
在听到了隔壁玩某农药的尴尬音效后, 我决定将音效再往后推一推, 
在我们结束这个场景的部分. )

```ruby
class GameWindow < Gosu::Window
  # 初始化
  def initialize
    # ...
    # 场景
    @scene = :playing
  end

  # update
  def update
    case @scene
    when :playing
      playing_update
    when :ending
      ending_update
    when :starting
      starting_update
    end
  end

  # draw                                                              
  def draw                                                            
    case @scene                                                       
    when :playing
      playing_draw
    when :ending
      ending_draw
    when :starting
      starting_draw
    end
  end

  # playing更新
  def playing_update
    # ...
    @scene = :ending unless @player.life > 0
  end

  # ...
end
```

(没什么用的小知识: 你可以在ruby里面写一些空方法, 
虽然此时你可能还不知道该往这个空方法里面扔什么, 但是先写一个, 
然后去写大的框架, 总是可以完成的. 也就是说, 
在我们的`GameWindow`里面还有很多类似于`def starting_draw; ; end`这样的空方法. )

那么我们现在运行程序, 在生命值降为0的时候, 我们就会进入一个黑色的界面, 
像极了我们一开始什么也没有干的界面. 没错这就是我们的结束界面. 
这个时候我们可以修改一下`ending_draw`来让我们的结束界面看起来合理一些: 

```ruby
class GameWindow < Gosu::Window
  def ending_draw  
    @background.draw(0, 0, ZOrder::BACKGROUND)  
    @text.draw_text("Game Over!\nYour Score: #{@score}",                            
                    200, 340, ZOrder::UI)
  end
end
```

于是我们这次故意死掉(死也是一种技术), 就可以看到死亡界面了: 

![死亡界面]({{ site.github.url }}/_img/i-am-boring/dead-window.png "耶, 挂掉了欸!?")

如法炮制, 我们可以写出各种各样的场景和界面了. 

Have a try? 

接下来是开始界面, 我们需要对游戏界面进行一个修改: 让屏幕中间可以显示一个标题, 
然后可以出现一个文字提示, 最后还要一个游戏开始的方式, 但这些都做完了以后, 
我们就可以说我们的游戏差不多可以算是一个比较完整的(无声)游戏了. 

```ruby
  def starting_update               
    @player.mv_left if button_down?(Gosu::KB_LEFT)
    @player.mv_right if button_down?(Gosu::KB_RIGHT)                                 
    @scene = :playing if button_down?(Gosu::KB_UP)                                   
  end

  def starting_draw
    @background.draw(0, 0, ZOrder::BACKGROUND)
    @player.draw
    @text.draw_text("Little Game\nPress Left & Right to Move\nPress Up to Shoot (and start)", 200, 340, ZOrder::BACKGROUND)
  end
```

![开始界面]({{ site.github.url }}/_img/i-am-boring/starting-window.png "天哪, 美工死了... 好丑. ")

笑, 界面实在是太丑了, 所以以后的任务就是让这个游戏看起来更加的好看, 更加好玩. 

## 一点点的美化
### 上帝说, 要有声音
~~不久前他不小心放了个屁...~~

(因为我现在没法上网, 所以我只能用电脑里的iMovie的声音素材库来制作这个声音. 
所以效果肯定一般啦, 就是那种廉价小游戏个水平了... 真是难堪啊. 毕竟我暂时还不会编曲, 
嗯, 加入到学习清单里面... )

首先是背景音乐, 我捡了一段迪斯科蹦迪一般的音乐, 很是魔性. 
查阅一下[文档](https://www.rubydoc.info/gems/gosu/Gosu/Sample): 

> Overview
> ========
> A sample is a short sound that is completely loaded in memory, 
> can be played multiple times at once and offers very flexible 
> playback parameters. Use samples for everything that's not music.

很好, 看看它的方法: 

> `#play(volume = 1, speed = 1, looping = false)` => Channel
> 
> Plays the sample without panning.
> * `looping` (`true`, `false`) (defaults to: `false`)
>  — **whether the sample should play in a loop**. 
> If you pass `true`, be sure to store the return value of this method 
> so that you can later stop the looping sound.

嗯, 不错, 我们就新建一个`@bgm`来控制这个背景音乐的播放. 

(一个没什么用的小知识: 由iMovie导出的m4a文件有1.2MB左右, 实际上是有点大的, 
会让程序变得很大, 所以我们要想一些来压缩这个东西, 我采用的方法是`ffmpeg`, 
`ffmpeg -i bgm.m4a -f ogg bgm.ogg`这样我们就可以得到一种叫做`ogg`的音乐格式, 
这种格式的文件大小很小. )

很好, 现在的游戏有点样子了. 那么就要让它稍微好玩一点点. 

### 恐惧来源于活力不足
让我们来试试双发子弹的魔法魅力. 嗯, 让我们定义一个`double_shoot`的`Plane`方法. 

```ruby
class Plane
  def double_shoot(bullets)
    if Time.new - @last_shoot_time > 0.1
      bullets << Bullet.new(@x + 7, @y - @hf_height)
      bullets << Bullet.new(@x - 7, @y - @hf_height)
      @last_shoot_time = Time.new
    end
  end
end
```

为了测试, 这个时候我们将`GameWindow`里面的`@player.shoot`换成`@player.double_shoot`, 
(老作弊了属于是... ), 这个时候就有一种双持射击的快乐了. 

但是我记得我玩的还有向两侧发射斜着的子弹的最强版本, 所以我们还要能够斜着射子弹. 
嗯, 这个时候我们不妨修改一下`Bullet`类的内容: 

```ruby

```

![恐惧来源于火力不足]({{ site.github.url }}/_img/i-am-boring/fire-on.png "在我的火力面前恐惧吧...")

嗯哼... 

接下来我们应该写一个射击模式切换的代码, 毕竟~~我们可以作弊,~~ 
用户不能作弊嘛. 

对于这个简单的游戏, 我们不妨通过一个简单的`if`判断和切换吧: 
当我们的分数达到一定的程度的时候就转换到新的模式, (这样子也太无聊了, 
之后我们将要换一种更加有意思的方式来转换. )

哼哼... 

```ruby
  # playing更新
  def playing_update
    @player.mv_left if button_down?(Gosu::KB_LEFT)
    @player.mv_right if button_down?(Gosu::KB_RIGHT)
    if button_down?(Gosu::KB_UP)
      case @score  
      when 0..10  
        @player.single_shoot @bullets  
      when 10..40                                                                   
        @player.double_shoot @bullets                                               
      else                                                                          
        @player.triple_shoot @bullets                                               
      end                                                                           
    end 
```

现在试试玩玩看? 感觉还算是个不错的游戏了. 就是有点简单, 我们的游戏没有压迫感. 

### 颤抖吧... 
所以我们要让玩家感到恐惧, 用人话说就是要提高游戏的难度, 并且这个难度最好还要不断地增加, 
让这个游戏不是一个简单的游戏. 

于是我更新了一下代码, 来让`Ball`类变得有些意思: 

```ruby
class Ball
  def initialize(seed)
    @image = Gosu::Image.new("./rec/ball.png")

    @x, @y = rand(480), 0 
    @v = 0

    @life = (rand(seed) / 5 + 10) / 2
    @scale = @life > 10 ? 10 + rand(3) : @life

    @hf_height = (@image.height / 2) * @scale
    @hf_width = (@image.width / 2) * @scale
  end
end

class GameWindow < Gosu::Window
    # ...
    # 新增加一个球
    if rand(100) == 0
      @balls << Ball.new(@score)
    end
end
```

现在有那么点点太困难了... 我难以达到超过3000的分数. QAQ. 

嗯, 就是不知道球还剩下的血量, 所以我们应该再加入一些的内容. 
来让这个游戏更加有意思. 

首先我想搞颜色. 

> `#draw(x, y, z = 0, scale_x = 1, scale_y = 1, color = 0xff_ffffff, mode = :default)` => void
> 
> Draws the image with its top left corner at (x, y).

之前的`draw`方法的`color`我们不是还没有用过嘛, 所以我们现在利用`Gosu::Color`来上色. 
[原文档](https://www.rubydoc.info/gems/gosu/Gosu/Color)里面提供了一点点的颜色, 
所以我们现在就用简单的颜色来画这些球: 

```ruby
class Ball  
  def initialize(seed)  
    # ...
    case @life  
    when 0..30  
      @color = Gosu::Color::GREEN                                                    
    when 30..60  
      @color = Gosu::Color::YELLOW                                                   
    else  
      @color = Gosu::Color::RED  
    end                                                                              
  end   

  def draw                                                                           
    # 球在子弹的上一层                                                               
    @image.draw(@x - @hf_width, @y - @hf_height,                                     
                ZOrder::BALL, @scale, @scale, @color)                                
  end
end
```

然后我们也想让球上显示球剩下的血量. 

```ruby
class Ball  
  def initialize(seed)  
    # ...
    @text = Gosu::Font.new(@hf_height)  
  end

  def draw
    # ...
    @text.draw_text("#{@life}",                                                      
                    @x, @y - @hf_height / 2,                                         
                    ZOrder::UI)                                                      
  end 
end
```

所以现在我们的游戏有点像样了. 但是是不是完美, 我的答案是绝对不是, 
我们还有很长的路要走... 

(先到这里告一段落, 我要去搞些~~薯条~~新的玩具了. )