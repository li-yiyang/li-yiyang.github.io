---                                                                     
layout: post
title:  "Golang PickUP"
date:   2022-02-26 11:52:20 +0800 
categories: notes golang
---
# Golang PickUP
咳, 计科导要用, 迫于生活, 不得不学. 为了防止以后学go让我太累, 
现在先学一点点. 

## Golang ENV setUP
怎么说吧, 我觉得搭建环境的时候真的很让人不爽. 因为搭建环境的选择太多了, 
然而实际上的东西又是差不多的名字, 就像是题目里面的抠字眼的问题. 
太容易让人上套了. 然而讲的人却默认了省略的语境, 导致了提供帮助的人, 
却成为了阻挠的人, 或者说, 成为了题目里面的干扰条件. 

> 五色令人目盲; 五音令人耳聾; 五味令人口爽; 馳騁田獵, 令人心發狂; 
> 難得之貨, 令人行妨. 是以聖人為腹不為目, 故去彼取此. 

所以我只知道一件事: 在我的电脑上, `macbook air (m1, 2020) macOS 12.2.1`
上面, 安装 golang 的环境还算是挺简单的. 去[官网](go.dev)下载对应的安装包, 
就是arm版本的, 然后安装就可以使用了. 

唯一的配置就是配置了一下 vim-go: (我用的是 
[Vim-Plug](https://github.com/junegunn/vim-plug)) 

```
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }   
```

然后利用 [coc-nvim](https://github.com/neoclide/coc.nvim), 
安装了对应的 [lsp](https://github.com/josa42/coc-go) 插件: 
`:CocInstall coc-go`

然后就完事了. 虽然如此, 我觉得是挺简单的, 但是想了想我之前做了怎样的铺垫, 
有了多少的准备, 什么 vim 的插件, 什么 coc-nvim 还有 neovim, 一堆乱七八糟的, 
甚至还有终端的配置, 别说还有什么字体啊, 乱七八糟的一堆又一堆的东西. 
总之我忘掉了. 然后看别人的痛苦安装过程中, 我一开始是不解的, 然后, 
啊, 原来如此, 我原来忘记了啊. 这种痛苦. 啊. 

## Golang Basic
本来我就是个低水平业余计算机爱好者, 对计算机也不是很懂. 所以主要参考了
[Go by Example](https://gobyexample-cn.github.io) 的教程. 

然后把最简单的一些东西写在下面: 

<details><summary> 点击展开一坨代码 </summary>

{% highlight go %}
/*
	使用类似于 C 的注释风格
	教程来源: https://gobyexample-cn.github.io
*/

// 将下面的程序都定义为 main 的包中的内容
// 感觉有点像是 ruby 里面的模块
package main

// formatted I/O
import (
	"fmt"
	"time"
)

// 同时导入多个包的做法
// import (
//	 "fmt"
//   "time"
// )

// main.main() 函数的名字就叫这个
// 麻了, 我绝对不想要在逆向里见到这个家伙
func main() {
	// 使用 fmt 包进行一个格式化 I/O 输出
	fmt.Println("Lucky Me. ")

	// 定义变量
	// 会自动判断变量类型, 假如有一个初始值的话,
	// 比如下面的还可以写成 var a = 1
	var a int = 1
	var b, c float32 = 3.14, 8.314
	var (
		d bool
		e string
	)
	// 初始化变量的简写
	f := "short"

	d, e = true, "the string of f is: "
	// 发现会在两个参数之间加入分割用的 " "
	fmt.Println("a = ", a)
	fmt.Println("b * c = ", b*c)
	fmt.Println(e, f, d)

	// 定义常数
	// 常数表达式可以执行任意精度的运算, 并且可以根据上下文自动确定类型
	const pi = 3.1415926

	// 数组
	var arr_1 [5]int
	arr_2 := [5]int{1, 2, 3, 4, 5}
	var arr_3 [5][3]string

	// 访问数组的内容
	arr_1[0] = len(arr_1)
	arr_3[3][2] = "lalala"

	fmt.Println("arr_1 is", arr_1)
	fmt.Println("arr_2 is", arr_2[4])
	fmt.Println("arr_3 is", arr_3)

	// 切片
	slice_1 := make([]string, 3)
	slice_2 := []string{"L", "u", "c", "k", "y", "Me", "."}

	slice_1[1] = "WoW"
	l := slice_2[0:6]
	slice_3 := make([]string, len(l))

	// 复制的逻辑和切片的逻辑还是不一样的, 没有那种藕断丝连的感觉
	copy(slice_3, l)
	l[2] = "C"
	// append 将新的元素写入到切片里面
	// 这里会发现 "G" 把 "." 给覆盖了, 该不会可以利用类似的手法来进行溢出?
	l = append(l, "G")

	// 切片的切的逻辑和 python 是差不多的感觉, 也是不包含最后一个
	// 但是感觉像更是 C 里面的数组, 数据还是连着的, 不是 python 的复制
	fmt.Println(slice_1[0:])
	fmt.Println("l", l)
	fmt.Println("slice 2", slice_2)
	fmt.Println("slice 3", slice_3)

	/*
		数组的逻辑稍微和之间接触的编程语言有点不一样,
		有点像是将数组看作是一种类型, 不同长度的类型竟然还不一样
		感觉还是切片更像是数组一点...

		并且多维数组和多维切片的不同之处在于切片的数组可以像这样:
		(我的写法和 ruby 有点像)
		[[1, 2, 3], [4, 5], [2], [6, 6, 6, 6]]
		而数组只能是方方正正的东西.
	*/

	// map (有点像是 ruby 里面的 hash)
	// make(map[key-type]val-type)
	m := make(map[string]string)

	m["Me"] = "Lucky"
	m["Her"] = "Happy"
	m["He"] = "Good"

	fmt.Println("I'm", m["Me"])

	delete(m, "Her")
	delete(m, "He")

	fmt.Println("the map is", m)

	// 循环结构, 只有 for 循环
	i := 1
	// 单条件, 实际上感觉就是个 while
	for i <= 3 {
		fmt.Println(i)
		i = i + 1
	}
	// 经典版本
	for j := 7; j <= 10; j++ {
		fmt.Println(j)
	}
	// loop
	for {
		fmt.Println("comment the following line and you will loop forever. ")
		// break 跳出
		break
		// continue 进入下一个循环
	}

	// Range 遍历
	for i, num := range arr_2 {
		fmt.Println("The", i, "th is", num)
	}
	// 对于变量的赋值, 实际上还可以利用类似于 ruby 的一个操作:
	// for _, v := range map {}
	// for k := range map {}
	for k, v := range m {
		fmt.Println("the", k, "key has", v, "value")
	}
	for i, c := range "Lucky Me" {
		// 输出是 unicode point
		fmt.Println("the", i, "th is", c)
	}

	// 条件判断语句, 没有三目运算符
	if 7%2 == 0 {
		fmt.Println("7 is even")
	} else if num := 9; num < 0 {
		fmt.Println(num, "is less than zero")
	} else {
		fmt.Println("the if and else")
	}

	// 读取时间 time.Now().Weekday()

	// switch 选择语句
	switch time.Now().Weekday() {
	case time.Saturday, time.Sunday:
		fmt.Println("Weekends")
	default:
		fmt.Println("Workdays")
	}

	// 调用函数
	fmt.Println(max_f(6, 9))
	fmt.Println(reverse(8, 6))

	sum(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
	// 将切片作为多个参数传入函数,
	// 不是将切片当作一个参数
	slice_4 := []int{1, 3, 5, 7, 9}
	sum(slice_4...)

	// 总感觉这种闭包很玄妙
	nextInt := intSeq()
	fmt.Println(nextInt())
	fmt.Println(nextInt())
	fmt.Println(nextInt())
	// 输出会是 1, 2, 3 地递增, 因为每次都更新了函数的本体吗

	// 闭包的递归
	// 需要提前显式声明, 否则无法调用
	var fib func(n int) int
	fib = func(n int) int {
		if n < 2 {
			return n
		}
		return fib(n-1) + fib(n-2)
	}
	fmt.Println(fib(7))

	i = 2
	point_func(&i)
	fmt.Println(i)
}

// 定义函数的方法
// 和 C 一样, 如果不写 return 的话不会自动返回最后一个表达式的值
// 也可以写类似于 void 类型的没有返回值的函数
func max_f(a int, b int) int {
	if a > b {
		return a
	} else {
		return b
	}
}

// 多返回值, 然后对于多返回值, 可以利用多赋值来解决问题
func reverse(a int, b int) (int, int) {
	return b, a
}

// 多参数
func sum(num ...int) {
	fmt.Println(num, "&")
	total := 0
	for _, n := range num {
		total += n
	}
	fmt.Println(total)
}

// 函数的闭包
func intSeq() func() int {
	i := 0
	// 这里定义了一个没有名字的函数
	return func() int {
		i++
		return i
	}
}

// 递归
func fact(n int) int {
	if n == 0 {
		return 1
	}
	return n * fact(n-1)
}

// 指针
func point_func(input *int) {
	// 用 & 取地址, 指针的语法和 C 类似
	fmt.Println("the address is:", &input)
	// 会修改指针指向的内容
	*input = 0
}
{% endhighlight %}

</details>

(说实话, 上面的东西基本就是照抄, 加上一点点的自己的注释, 
烂得很, 所以还是要写一点自己的东西的, 以免沦为大自然的搬运工. )

参考 [wiki](https://zh.wikipedia.org/wiki/指令式編程), 
大部分的编程语言有这样的特性: 
* 运算语句, 然后还有储存(赋值)语句, 将运算结果储存起来
* 循环语句, 反复执行. 
* 条件判断语句
* 无条件判断跳转语句

基本上掌握了这些就可以进行一个程序的编写了, 然后个人理解, 
计算机就是在进行一个运算的过程. 然后不同的数据在不同的声明下可能有不同的意义, 
比如字符`"Lucky Me"`, 可能在另一种观点下就是一串排列得比较好的数而已: 
`76, 117, 99, 107, 121, 32, 77, 101`, 仅此而已. 

然后在计算机中, 这样的数据被放在不同的地方, 叫做寄存器里面. 
寄存器里面数据根据不同的读法和使用方式从而拥有了不同的意义. 
字符串, 小数(浮点数)等等数据类型更像是根据不同的数据进行一个解读. 
比如可以将字符和数进行一个一一映射, 可以将浮点数分成整数部分和小数部分
等等操作, 这样的操作根据不同的规则有了不同的实现. 这些数据储存在寄存器里, 
每个寄存器都有特定的编号, 然后计算机根据这些编号(地址)去访问数据, 
修改数据等等. 然后对于数组, 切片之类的东西更像是一种将原来的单个的数据组合起来, 
这样的组合方法有点像是讲对应的寄存器的编号列在一起, 然后在要用的时候, 
根据编号去访问数据所对应的寄存器, 然后取得数据. 

然后, 我认为就对最基本的编程了解了大概了. 接下去再来一些之前不是很了解的, 
(就是以前学的时候从来没有用过的)

## 结构
感觉结构有点类似于 ruby 里面的对象, 之前在做逆向的题目的时候遇到的结构, 
给我的感觉都很像是一堆数据, 函数什么的打包在一个东西里面. 
所以姑且先这样理解. 

<details><summary> 继续工作, 我是大自然的搬运工(bushi </summary>

{% highlight go %}
package main

import "fmt"

/*
  接口, 虽然我觉得有点像是通用函数的一个东西,
  是有点像是 python 的 len() 函数之类的定义的手法?
  或者像是 ruby 的 duck type 思想?
  有点像是为了将结构的类型的特点消除.
*/
type interface_func interface {
	func_1() int
	func_2() string
}

type object struct {
	name  string
	index int
}

type another_obj struct {
	name string
	size int
}

// 给 object 类定义了一个方法
func (obj object) func_1() int {
	return obj.index + 2
}

func (obj object) func_2() string {
	return "Hello" + obj.name
}

/*
	个人感觉上面两种方法超级像, 就是类型有点不一样.
	又, 在调用方法的时候, 会出现值和指针的转换,
	假如想要避免在调用方法的时候产生拷贝, 可以利用指针来调用方法.
*/

func (a_obj another_obj) func_1() int {
	return a_obj.size + 6
}

func (a_obj another_obj) func_2() string {
	return "Oh? I don't know you, " + a_obj.name
}

func abstract_func(obj interface_func) {
	fmt.Println(obj)
	fmt.Println(obj.func_1())
	fmt.Println(obj.func_2())
}

func main() {
	fmt.Println("Stuct")

	// 利用类似的手法进行一个初始化,
	// 然后没有直接声明的东西就是默认为0
	obj := object{name: "Name"}
	obj.index = 0
	an_obj := another_obj{name: "Name_2", size: 99}

	fmt.Println("index of obj", obj.func_1())
	fmt.Println("index of obj", obj.func_2())

	abstract_func(obj)
	abstract_func(an_obj)
}

{% endhighlight %}
</details>

虽然不知道这样的理解是否正确, 但是我觉得结构简直就是 ruby 里面的对象, 
对象的 instance varible, 比如`@name`, `@life`等等. 所以目前还是, 
就这样简单地理解一下. (虽然感觉自己是在用一个复杂的东西来理解简单的东西, 
或者是, 用更高层的结构去理解基础的结构... 毕竟这两个东西还是有点不一样. )

## Error!
怎么说呢, 有点感觉 go 的错误处理有点, 感觉像是一开始写代码一堆报错中断, 
然后苦不堪言, 希望有一种能不管所有的报错, 先给我运行了之后, 然后我想看结果, 
中间有点报错我可以接受... 于是我就会在 ruby 里面这样乱写: 

```ruby
def edit_file(files)
  files.each do |f|
    File.rename(f, f + ".yes")
  end
end

begin
  edit_file("test.txt")
rescue
  puts "Although there might be a error, but i don't care. "
end
```

虽然最后我发现了一个坑爹的情况, 那就是很有可能因为我这样的智障操作, 
导致了我接下来的代码爆炸了, 结果... 我还以为程序可以运行, 就只能, 
在我之前的狗屎代码里面疯狂 debug. 

嗯, 接下来回到 go 的部分, 嗯, go 没有类似于 ruby 和 C 之类的 interrupt
类似的东西. 它, 就是, 嗯, 很朴素的, 不报错, 然后将自己的报错信息, 
用多返回值的形式返回: 

```go
func f(arg int) (int, error) {
	if arg == 0 {
		return -1, errors.New("the arg is 0")
	}
	return 1 / arg
}
```


