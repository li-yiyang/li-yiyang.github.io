---
layout: post
title:  "First Touch of Python"
date:   2021-11-16 16:02:19 +0800 
categories: jekyll update
---
# First Touch of Python
`Python`千千万, 到处都在吹, 我也知道它很好, 只是我不爱. 

算了, 为了更好的学习和与他人交流, 我现在要学一点`python`. 
但是, 因为我学过一点点(真的只有一点点)半吊子的`ruby`, 
所以我会用`ruby`来学习`python`, 并且尽量找一种可以把`python`
代码用`ruby`来写的做法. 

## Python Basis
### 运算符, 赋值, 类型
目测和`ruby`是大同小异的
```python
string = "Hello, this is python. "
x = 3
y = 4
((1 + 2 / 4) ** x - ((6 % y) << 7)) // 4
# // 是整除的运算
# 在ruby里面默认是整除
# 比如3/2 => 1; 3.0/2 => 1.5
# 这是因为整数和浮点数的区别
# python自动做了这样的事
```

高级操作: (和`ruby`一样)
```python
x, y = 4, 5 # x = 4, y = 5
x, y = y, x # 交换变量

c = d = 6   # c = 6, d = 6
```

### 简单IO
```python
print("yes? or no? ")
# 发现print("a", "b", "c")的话, 
# 会自动在输出中间插入空格
# => "a b c"
# 嗯, 又是一个不一样的地方
input("Okay? ")
```
这样就可以得到输入和输出了

和`ruby`不一样的是, `ruby`不强制要求函数输入括号; 
但是`ruby`给了很多的输出函数, 这些函数还都有一点点的...
tricky...(QAQ, 这种时候感觉自己输了. )

```ruby
puts("hello")  # 和puts "hello"一样, 输出的末尾自带"\n"换行符
print("yes? ") # 输出的末尾不会换行, python 的会换行
p("ooo")       # 一种输出, 不过很少见到, 在打印对象的时候会遇到

gets()         # 得到输入, 唯一坑爹的是最后会加上一个"\n"
```

`python`好像也有类似的打印命令, 
但是要引入一个库. 

```python
import pprint

pprint.pprint(obj)   # => 会打印出来好的形式
pprint.pformat(obf)  # => 会输出一个字符串
```

### String
```python
s = "this is a string"
# 字符串的长度
len(s)         # => 16, ruby: s.length

# 类型转换
# int(string, base=10)
int("23")      # => 23, ruby: s.to_i
# float() 同理
float("-0.13") # => -0.13, ruby: s.to_f
# str(obj)
str(23)        # => "23", ruby: a.to_s
```

### 控制流
#### 条件 if
在`python`中判断条件, 并进行分支. 
```python
if a:
  print("a is true\n")
  print("good. \n")
else:
  print("a is false\n")
  print("also good, too. \n")

if a:
  # ...
elif b:
  # ...
elif c:
  # ...
else:
  # ...
```
在`ruby`中也是这样的: 
```ruby
if condition == true then # 可以把then给扔掉
  # hai~
elsif conditon == nil
  # ruby 里面有true, nil(空), false
  # 虽然nil可以用在逻辑判断中和false等价
  # 但是实际上nil是一种类似于0一样的占位符
  # 好像可以这样理解: 
  # 我不知道这个变量的值是什么, 所以就叫它nil算了
  # python 里面也有, 叫做None
else
  # ...
end

# ruby有行if和unless
# unless等价于写了if但是把条件加了一个not
puts a unless a.nil?
```
#### 循环
`python`里面有`while`和`for`, 
虽然和`ruby`一样, 这个`for`和`C`的`for`很不一样. 

`while`: 
```python
while condition == true:
  # ...

while True:
  if condition != True:
    break               # 跳出while循环
  if restartCondition == True:
    continue            # 回到循环的开头
```
`for`: 
```python
for i in range(5):
  print("the", i, "times")

# range(start, end, step)
# range(start, end), step = 1
# range(end), step = 1, start = 0
```

这个时候`ruby`就有很多的骚操作了, 
你可以写的超级花里胡哨, 总之是怎么花怎么来. 
```ruby
5.times do |i|
  puts "the #{i} time"
end

5.times{print "ruby"}

(0..100).step(4).each{|i| puts i}

loop do
  # 等价于while true
  # break if ...
end

while condition == true
  # ...
end
```

#### 吐槽
这里可以看到`python`该死的块结构, 我觉得就很迷幻, 
为什么要用缩进来表示块结构, 这个冒号`:`也太容易打漏了, 
我看代码的时候也很累. 

(好吧, 只是一己之偏见, 实际上ruby也是这样的啦, 
也有缩进, 虽然不强制. `end`在多重嵌套的时候又会漏. )

不过不得不说, 感觉`ruby`就是一个超级缝合怪, 
什么语言很高级, 很优美, 就吸收什么语言的特性, 
佩服一下Matz, (刚好近代史纲提了一嘴日本). 

### 函数
```python
def funcName(arg, defaultArg="default value"):
  # 如果一开始没有设定默认值的参数
  # 在调用的时候就一定要给参数, 否则就会尬
  # do things
  return None # 返回值 不一定强制写返回值, 可以自动给出最后一个运算的值
```
在这个时候就会有一个叫作环境的概念, 
不同的代码块可以看作是不同的环境, 
可以想象一种`global`的环境, 就是总的代码的运行环境, 
里面放了很多的东西, 但是当进入了一个新的环境的时候, 
比如说在调用函数的时候, 就会局部开一个新的环境. 
在这样的环境中, 如果调用变量的话就要有一些考虑了. 

(一个猜想, 不一定对, 没准这就是`stack`的机理. )

```python
x = 1
def hello():
  print(x) # 这里的x是外环境的变量
def helloTwo():
  x = 3    # 这里的x是局部变量
  print(3)
def helloThree():
  global x # 这里用global强制地声明x是外环境的变量
  x = 3    # 改变了外环境的x
  print(x)

hello()    # => 1

hello()    # => 3
print(x)   # => 1, 没有改变外环境的x

helloThree()
print(x)   # => 3
# 这样的函数比较危险, 还是少用为妙
```

简单的`ruby`代码: 
```ruby
def func(x, y=0)
  print(x+y)
end
```

### 异常处理
```python
try:
  # codes
  # for example:
  x = 4 / 0
except ZeroDivisionError:
  print("Error")
  # 处理代码, 救命用
```
在`ruby`中的形式: 
```ruby
begin
  # ...
rescue => exception
  # ...
else
  # ...
end
```

### 数据结构
#### 列表 List
在`ruby`里面叫`Array`
* 定义列表: `list = ["a", 1, ["a", "asd"]]`
* 访问元素: 
  * index: `list[0] # => "a"`顺着拿, 从0开始
  * negative index: `list[-1] # => ["a", "asd"]`倒着拿, 从1开始
* 修改元素: 
  * `list[index] = value`
* 列表操作:
  * `len(list) # => 3` 列表长度 `ruby`: `a.length`
  * `del list[index]` 删除值 `ruby`: `a.delete_at index`
  * `list.append(value)` 向末尾添加值 `ruby`: `a << value`
  * `list.insert(index, value)` 往`index`处插入元素
    `ruby`: `a.insert index, value`
  * 
  * `value in list` 是否在列表里 `ruby`: `a.include? value`   
    `value not in list` 不再列表
  * `list.index(value)` 找到值的位置 `ruby`: `a.index value`   
    `list.remove(value)` 删除值 `ruby`: `a.delete value`
    不一样的地方是`python`找不到就会报`ValueError`, 
    `ruby`找不到的话就会返回`nil`a   
    一样的地方都是只会给出第一次出现的匹配的值的位置
  * `for i in list:` 类似于 `ruby` 的`list.each do ... end`
  * `list1 + list2` 相加
  * `list.sort` 排序 `list.sort(reverse=True)`反向排序, 
    `list.sort(key=str.lower)` 
    利用关键词参数`key`可以设置排序方式为字典排序
    `ruby`: `a.sort!`    
    这里注意有一个`ruby`方法命名的约定, 就是方法命名的时候, 
    一般把会改变原来的对象的方法名称后面都有一个`!`表示要注意, 
    假如没有`!`, 只会产生一个排了序的副本. 
* 列表切片: `list[startIndex:endIndex]` `ruby`: `a[start..end]`

为什么要列表切片? 这是因为列表又一点像`C`里面的数组指针, 
当`a = list`, 假如修改了`a`里面的元素, 
`list`里面的元素也会随着发生变化. 

#### 列表和字符串
可以把字符串看作是一个列表

(在`C`中, 也确实是这样的. )

所以可以类似的这样访问字符串中的第几个字符: `s[2]`
但是不可以用来修改, 比如`s[2] = "3"`是不可以的. 
因为字符串是不可变的对象? 
(虽然在`ruby`里面是可以的)

#### 元组
元组可以看成是不可以变的列表.  

`tuple = ('hello', 42, (1, 2, 3))`

但是好像在`ruby`里面好像没怎么有类似的东西. 
好像有冻结? 忘了. 

#### 类型转换
`tuple(list)`, `list(tuple)`看就懂了. 

#### 字典
字典没有排序, 因为是靠`key`来访问的. 
好像在`ruby`里面是靠`hash`值计算来访问的, 
所以叫`hash`, 名字有点不一样. 

```python
dictionary = {
  'key1': value1, 
  'key2': value2
}
dictionary['key1'] # => value1

for v in dictionary.values():
  print(v)         # => 输出value
for k in dictionary.keys():
  # key
for i in dictionary.items():
  # i = (key, value)
  # 是一个键值对
```

`ruby`里面的`hash`是这样的: 
```ruby
hash = {
  "key1" => value1,
  "key2" => value2
}
hash['key1']

# ruby 还可以用Symbol来当key
hash = {
  :key1 => value
}
```

* `value in dictionary`, 等价于`value in dictionary.values()`
* `key in dictionary`
* `dictionary.get(key, default)`得到`key`对应的值, 
  假如没有就返回`default`
* `dictionary.setdefault(key, value)`检查`key`对应的值, 
  没有的话就把`key`的值设成`value`, 返回值就是`key`对应的值

### 字符串
和`ruby`一样, 单引号在`python`里面和双引号还是有那么大点区别, 
坑爹的地方就是双引号里面支持字符转义, 单引号里不支持, 要注意...

因为字符串和列表很像, 所以有很多的类似的方法: 
```python
s = "abcdefghijklmn"
s[0] # => "a"
s[:4] # => "abcd"
s[7:] # => "hijklmn"
s[4:7] # => "efg"

"z" in s # => False
"c" not in s

s.upper() # 不会改变s
s.lower()

s.islower()
s.isupper()

s.startwith(string)
s.endwith(string)
```
虽然看起来很好用, 实际上鸡肋的很. 
实际上还是直接正则表达式的感觉很好用的样子. 

#### 字符串和列表
```python
", ".join(['a', 'b', 'c'])
# => a, b, c
"a, b, c".split(', ')   # => 假如没有输入的话, 默认是空格
# => ['a', 'b', 'c']
```

#### 漂亮格式
```python
"string".rjust(length)   # 按长度length扩张字符串, 右对齐
"string".ljust(length)   # 按长度length扩张字符串, 左对齐
"string".center(length)  # 按长度length扩张字符串, 居中
# 上面的函数还可以添加第二个参数, 描述用来填充的字符
"string".center(10, '#') # => '##string##'

# 删除空白字符
" s a\n".strip()         # 开头和末尾都没有空白字符"\n", " "等
" s a\n".lstrip()        # left
" s a\n".rstrip()        # right
# 还可以添加参数, 就会把参数里面的所有字符都当作是空白字符
```

#### ClipBoard
```python
import pyperclip         # pip install pyperclip

pyperclip.copy("string") # copy
pyperclip.paste()        # paste
```

## Advanced
### Regexp
在`ruby`里面, 正则表达式是默认就开启的, 
是一种内置的对象. 可惜`python`的没有. 

正则表达式需要另外学, 感觉`python`的就有点麻烦. 

```python
import re

regexp = re.compile(r'regexp')
match = regexp.search('string') # 只找第一次
match = findall('string')     # 全部找, 返回字符串的列表或者是元组的列表
match = regexp.sub('代替用字符串', '匹配用字符串') # 查找替换字符串

match.group(index)
# index = 0或者不输入:  返回匹配的全部文本
# index >= 1:         返回匹配组
```
### FileIO
#### Basic
有一个很坑爹的地方: 
在`windows`和`linux`或`mac`的文件路径名字不一样. 
就是`/`和`\`的坑. 

```python
import os
os.path.join('usr', 'bin')  # => 'usr/bin' linux
```

(实际上`ruby`里面的`File`类就直接支持这个, 
`File.join('usr', 'bin')`. )

```python
os.getcwd()             # => 当前工作环境
os.chdir("path")        # 打开文件夹, 打不开就FileNotFoundError

os.makedirs("path")     # 新建文件夹

os.getlistdir("path")   # 列出path里面的文件
os.path.getsize("path") # 文件大小
os.path.exists("path")  # 是否存在
os.path.isdir("path")   # 是否是文件夹
os.path.isfile("path")  # 是否是文件
```

读取文件
```python
f = open("path")        # 打开文件 
string = f.read()       # 读取文件
l = f.readlines()       # 读取行, 返回的是列表
f.write("string")       # 写文件
f.close()               # 关闭文件
```

保存数据的另外方法
```python
import shelve

shelfFile = shelve.open('data') # 打开
shelfFile['valueName'] = value  # 写入
shelfFile.close()               # 关闭

# 或者用pprint.formant来输出.py
# 然后再import
```
#### A little bit handy
```python
import os
import shutil     # 用来复制文件和文件夹
import send2trash # 放到回收站 pip install send2trash

shutil.copy(source, destination) # 复制文件
shutil.move(source, destination) # 移动文件, 也可以用来重命名

os.unlink(path)             # 删除文件
os.rmdir(path)              # 删除空文件夹
shutil.rmtree(path)         # 删除文件夹(包括内容)
send2trash.send2trash(path) # 放到回收站

for folderName, subfolders, filename in os.walk(path):
  # 历遍文件树, 建议不要在里面删除文件, 还是新建一个列表然后再循环
```
### Documents
#### ZIP
```python
import zipfile    # zip 文件访问

zipf = zipfile.ZipFile(path)     # 读取文件
zipf.namelist()                  # 返回文件列表
zipf.getinfo(path).file_size     # 文件大小
zipf.getinfo(path).compress_size # 压缩大小

zipf.extractall()                # 把文件全部解压到当前文件夹
zipf.extract(path)               # 解压文件到当前文件夹
zipf.extract(path, toPath)       # 解压到toPath

zipf = zipfile.ZipFile(path, 'w')# 新建文件夹
zipf.write(path, compress_type=zipfile.ZIP_DEFLATED)
# 把path地方的文件压缩到zip文件里

zipf.close
```
#### Excel
#### PDF
#### Picture
### Debug
```python
raise Exception('error message')
# 报错, 报错类型是Exception, 错误信息是error message

try:
  # ...
except Exception as err:
  print(str(err))

# str(err)就是文件的报错信息
```
#### 断言 assert

```
##############2021.11.17 先停一下 下次继续################
```
### Web
#### HTTP
#### E-Mail
### Data
#### CSV
#### JSON
### Thread
### GUI
