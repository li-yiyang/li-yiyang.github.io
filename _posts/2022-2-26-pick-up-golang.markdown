---                                                                     
layout: post
title:  "Golang PickUP"
data: 2022-02-27 21:15:01 +0800
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

/*
  然后对于结构体, 还可以进行嵌套地使用:
  struct net_struct struct {
    object
    str string
  }
  然后可以像这样调用:
  net_struct.object.index
  等等. 并且嵌套在里面的结构的方法也能够在外面的结构体使用:
  net_struct.func_1()
  那么问题来了, 为什么要叫这个概念为 composition ?
  看不出来为什么要加上一个新的概念的理由.
	*/
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

后来发现还有一个叫做 panic 的东西, 和正经的中断很像: 

先看一个 ruby 的报错: `raise "I was a bad guy. "`, 
然后是 go 的 panic: `panic("I was a good guy. ")`. 
嗯, 就是这样. 

然后有类似 C 里面的处理(Unix)系统信号的函数: 

```go
import (
  "os/signal"
  "syscall"
)

func main() {
  // 信号通过 channel 出来
  sigs := make(chan os.Signal, 1)
  // 用一个函数捕捉信号, 然后送到 channel 里面
  signal.Notify(sigs, syscall.SIGINT, syscall.SIGTERM)

  // 然后就编辑函数进程来处理浙西乱七八糟的信号...
}
```

## GoRoutine
先看代码: 

<details><summary> 点点点点点点 </summary>

{% highlight go %}
package main

import (
	"fmt"
	"time"
)

func f(from string) {
	// 这里我选了一个比较离谱的 i 值,
	// 原因是这样才能够更好地看到多线程的交替输出的效果
	for i := 0; i < 3; i++ {
		fmt.Println(from, ":", i)
	}
}

func main() {
	fmt.Println("GoRoutine")

	f("no routine")

	go f("in a routine")
	go f("another routine")

	time.Sleep(time.Second)
	fmt.Println("done")

	/*
		  然而, 上面的两个函数实际上并没有什么交流,
		  比如说, 假如我想要干一些鸟事, 比如写爬虫,
		  就需要在不同的爬虫线程里面交换信息, 防止重复爬取
			然后我们就可以利用 channels 来联系两个进程.
	*/
	// make(chan val-type)
	// make(chan val-type, buff_size)
	// 传入 buff_size 来设置缓冲大小, 虽然目前看不出来怎么用
	message := make(chan string)
	// 用 channel <- value 来发送数据到通道里面去
	go func() {
		message <- "value"
		message <- "yes"
	}()

	// 用 <-channel 的方式从通道里面得到数据
	msg := <-message
	fmt.Println(msg)
	msg = <-message
	fmt.Println(msg)

	/*
		  并且 <-channel 会让程序为进程在子进程没有运行完前会停下来等等
		  好像这个叫做阻塞通道
		  可以利用这个特性来保证所有的通道运行完了之后才让程序结束
			方法是这样的:
			func main() {
				done := make(chan bool)
				// 其他的所有的代码
				<-done
				// 用上面的 <-done 的方式来保证所有的东西才结束
			}
	*/
	c1 := make(chan string)
	c2 := make(chan string)

	go func() {
		time.Sleep(1 * time.Second)
		c1 <- "start one"
		time.Sleep(2 * time.Second)
		c1 <- "done one"
	}()

	go func() {
		time.Sleep(2 * time.Second)
		c2 <- "start two"
		time.Sleep(2 * time.Second)
		c2 <- "done two"
	}()

	for i := 0; i < 4; i++ {
		select {
		case msg1 := <-c1:
			fmt.Println(msg1)
		case msg2 := <-c2:
			fmt.Println(msg2)
		}
	}

	/*
		可以创作一些只读只写的进程:
		func write_only(ping chan<- string, msg string) {
			pings <- msg
		}
		func read_only(ping chan<- string) {
			msg := <-pings
			return msg
		}
	*/

	/*
		非阻塞通道的写法: 前面说的阻塞通道, 就是假如没有得到输入的话, 就要一直等下去,
		但是可以换一种思路, 除非我收到了输入, 否则我就不管了, 我就直接运行下去.
		(欸, 为什么我觉得有一种中断的感觉...)
		select {
		case msg := <-channel:
			// ...
		default:
			// 这里就是假如程序运行到这里没有得到 channel 的信号的话,
			// 就直接运行这里的代码
		}
	*/

	/*
		通道遍历: 假如通道里面有一堆的数据, (哪怕通道关闭了), 也可以用这样的方式来历遍:
		for elem := range channel {
			// ...
		}
	*/

  /*
    超时处理: 
      先了解一些关于时间的操作: 
      * 就是超级简单的等待: time.Sleep(sec * time.Second)
      * 定时器: 
        timer := time.NewTime(sec * time.Second)
        // 等待直到定时器结束
        <-timer.C
        // 感觉这样的操作, 其实也不是不能自己实现... 定义一个结构, 
        // 然后在这个结构里面开一个通道, 然后利用通道的阻塞效果...
        // 管它呢
      * 打点器: (笑死, 这个翻译, 太形象了, 打点计时器... )
        ticker := time.NewTicker(sec * time.Second)
        for {
          select {
          case t := <-ticker.C:
            // 这个时候的 t 就是时间, Time.now
          default: 
            // ...
          }
        }
    然后就是关于超时的处理: 
    select {
    case res := <-channel:
      // ...
    case <-time.After(sec * time.Second):
      // 超过时间没有收到的话, 就会触发这个分支
    }
  */

	// 关闭通道
	// 关了之后就不能往里面写数据了
	close(c1)
	close(c2)
}

{% endhighlight %}

</details>

我觉得这个看起来真香, 虽然我之前 ruby 里面的 Thread 一直没能理解, 
然后最后就只好在一堆的 bug 之中, 灰溜溜的跑路, 最后不得不去写单线程. 
(据说现在引入了 Fiber, 但是我没有试过, 以后有时间了之后试试. )

但是上面的感觉更像是每一个线程对应一个通道, 但是假如我想搞点~~薯条~~, 
哦, 不是, ~~我想搞很多的薯条, 非常非常多的那种~~, 就是我要处理超级多的线程, 
然后来一个大统领来管理这些线程. 不错. 

<details><summary> 最简单的版本 </summary>

{% highlight go %}
package main

func worker(id int, jobs <-chan int, results chan<- int) {
	for j := range jobs {
		// ...
	}
}

func main() {
	// 新建通道

	// 新建分支
	for w := 1; w <= 3; w++ {
		go worker(w, job_channel, results_channel)
	}

	// 发放任务
	for job_id := 0; job_id <= job_number; job_id++ {
		// job_channel <- mission
	}

	close(job_channel)

	// 收集结果
	for job_id := 0; job_id <= job_number; job_id++ {
		// result := <- result_channel
	}
}
{% endhighlight %}
</details>

上面的感觉有点没能体现高级的地方, 所以折叠了. 
下面的是利用了一个叫做 WaitGroup 的东西来搞的, 感觉有点意思

```go
package main

import (
	"fmt"
	"sync"
	"time"
)

func main() {
	var waitgroup sync.WaitGroup

	for i := 1; i <= 5; i++ {
		// 告诉 waitgroup 增加了一个要等待的线程
		waitgroup.Add(1)
		go func() {
			// 利用了闭包的东西
			defer waitgroup.Done()
			// do something
		}()
	}

	// 等待 waitgroup 里面所有的东西都执行完毕
	waitgroup.Wait()
}
```

嗯, 相比前面折叠的东西, 这个更加简单一点. 虽然感觉... 差不多? 

但是假如是同时进行一个读写变量的操作? 
嗯... 怎么觉得单看上去的话和 ruby 里面的 Mutex 一样...

<details><summary> 锁住变量的水分, 让它保持鲜嫩 </summary>

{% highlight go %}
package main

import (
	"fmt"
	"sync"
	"sync/atomic"
)

type mutex_var struct {
	// 重点是下面的 sync.Mutex 的变量
	// 可以有锁状态和解锁的状态
	mu sync.Mutex
	// 其实其他的变量是什么都挺无所谓的
	variable string
}

// 因为互斥锁不能复制, 所以要用指针来传递函数, 
// 反之出现尴尬
func (v *mutex_var) edit_string(new_str string) {
	// 对于 v, 因为要使用, 所以要锁定
	v.mu.Lock()
	// defer 关键词告诉程序, 在函数运行结束后将变量解锁
	defer v.mu.Unlock()
	// 其实个人觉得, 这个互斥锁防君子不防小人, 
	// 就是所有程序在想要修改 v 的时候, 先看看锁是否打开, 
	// 然后再决定是否修改, 嗯...
	v.variable = new_str
}

func main() {
	// 利用的是一个原子计数器 
	// 原子计数器的作用就是为了防止同时写的情况的出现
	var adder uint64
	
	mutex_v := mutex_var{
		variable: "string",
	}

	var wg sync.WaitGroup
	for i := 0; i < 50; i++ {
		wg.Add(1)

		go func() {
			for c := 0; c < 1000; c++ {
				// 将计数器进行一个加
				atomic.AddUint64(&adder, 1)
				// 其实应该是填写一些新的东西的
				mutex_v.edit_string("string")
			}
			wg.Done()
		}()

		wg.Wait()
		fmt.Println("adder: ", adder)
	}
}
{% endhighlight %}

</details>

差不多够用了, 虽然我知道, 多线程对我目前是没什么用的, 
还是文件处理之类的朴素的东西好...

## 排序
算法总是要会的吧? 好像不用, 对于水平低下, 从未学过算法的我, 
只知道 ruby 里面有一个排序算法叫 `[3, 1, 2].sort`, 再高级一点就只有, 
`[3, 1, 2].sort { |a, b| a > b }`之类的. 或者就是`sort_by`. 就, 没了...

所以, 就... 折叠了吧... 

<details><summary> 排序... 在搞算法的那些人眼里... 就是个调包而已 </summary>

{% highlight go %}
import "sort"

// 简简单单的排序
iarr := []int{3, 1, 2}
sort.Ints(iarr)
sarr := []string{"a", "c", "b"}
sort.Strings{sarr}

// 自定义排序, 感觉就是利用了前面的通用函数的感觉
// 主要要对排序的东西定义 Len, Swap, Less 三个函数
// 然后 sort 包就会利用这三个函数来排序
type the_type []string
func (arr the_type) Less(i int, j int) bool {
  // 其实感觉很像是 ruby 的 sort 的带 block 的模式
  return len(s[j]) < len(s[j])
}
{% endhighlight %}
</details>

## 文件读写
需要调用一个 `os` 包. 然后在代码里面就: 

```go
// 这里我就想到了一个问题, 假如有想我一样的菜鸟, 
// 忘了写代码的时候加上 err 检查, 然后错误地读了文件
// 那不是会很糟糕? 
r_data, r_err := os.ReadFile("file/path")
check(r_err)
// 用 string 函数将这些读取的东西转换为字符串
fmt.Print(string(r_data))

// 麻了, 写文件的模式还是这种... 看来还要记住? 
// 应该是有的查的... 没错, 就是unix的 32 位模式数...
// 就是这样: -rwxrwxrwx, 
// 分别表示是否是目录, 所有者权限, 用户组权限, 其他人权限
const WRITE_MODE = 0644
w_data := []byte("a string, which would be translated into byte and write to file")
w_err := os.WriteFile("file/path", w_data, 0644)
```

不得不说, 上面的操作实在是比较简单, 想要高级一点的, 更底层一点的, 
还是用 `os.Open(file_path)` 和 `os.Create(file_path)`. 因为没用过, 
所以就这样云过去吧... 

## 假装我已经把该学的学了
嗯, 所以到时候学习的时候遇到什么东西再临时抱佛脚算了, 摆烂... 
