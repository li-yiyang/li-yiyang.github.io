---
layout: post
title:  "GTK in macOS"
date: 2022-12-01 09:58:29 +0800
categories: misc
---
# GTK in macOS
本来应该是要吐槽 macOS 上的环境的搭建很麻烦的.
然而最终发现只有我才是小丑. 

## 超级简要的介绍
GTK 是一个图形库, 可以提供一些 GUI 控件的支持. 

不过因为刚开始学, 所以不太会. 

在 macOS 上安装 GTK 的库, 可以用 homebrew 来很方便地做到:

```shell
brew install gtk+3
```

在安装完毕后, 通过: 

```shell
pkg-config --libs --cflags gtk+-3.0
```

来检验安装的结果. 这两步应该是比较方便的. 

因为 GUI 的这个操作的逻辑有点类似于后端运行, 然后前端渲染的感觉 
([XServer](https://en.wikipedia.org/wiki/X_Window_System)). 
所以在 macOS 上, 需要再安装一个 [XQuartz](https://www.xquartz.org),
来渲染 GUI.

在安装完毕后, 可以使用 `xeyes` 来查看效果. 
如果不出意外的话, 那么你就能够看到一双傻乎乎的眼睛了. 

### 脱裤子放屁的做法
不过一开始我以为我搞错了, 于是舍近求远, 干了些蠢事:

在 UTM 虚拟机中安装 Linux (可以参考 
[之前的记录]({{ site.github.url }}/ctf/untitled(2)/)). 

一个简单的操作可以是这样的:

1. 安装 Ubuntu Server, 选择 minium 安装
2. 安装桌面环境: `sudo apt install -no-install-recommends ubuntu-desktop`
3. 安装 GTK 环境: `sudo apt install libgtk-3-dev`

然后一个简单的想法就是可以直接在虚拟机里面配置编辑环境了,
但是我不是很喜欢 Ubuntu 的桌面和操作, 并且也不是很会美化配置,
于是舍近求远, 选择在 macOS 上通过 XQuartz `ssh -Y` 来连接虚拟机,
通过 SSHF 来访问文件. 方法如下:

* 连接虚拟机:
  1. 首先安装 XQuartz
  2. 然后连接到虚拟机: `ssh -Y -p <port-number> <usr>@<host>`
  3. 在虚拟机中执行 `xeyes` 我们就能够看到一个小眼睛了
* SSHF 来挂载远程的硬盘
  1. 首先安装 [macfuse](https://osxfuse.github.io)  
	 (不过安装的时候要打开 System Kernel Extension)
  2. 然后安装 [sshfs](https://osxfuse.github.io)
  3. 在终端中使用: 
	 `sshfs -p <port-number> <usr>@<host>:<remote-path> <local-path>`
	 来将 `<remote-path>` 挂载到本地, 并且映射到 `<local-path>` 中,
	 其中, `<local-path>` 需要是一个空文件夹. 

于是就能够在本地编辑文档并轻松同步了. 

(注: 这个方法在最后被放弃了, 因为实在是多此一举. )

### Emacs Flycheck 配置
在 Emacs 的配置中加入 `flycheck-pkg-config` 来提供 header 帮助,
否则就会在编辑的时候出现恼人的报错提醒. 

```elisp
(use-package flycheck-pkg-config)
```

然后在每个项目中, 使用 `M-x flycheck-pkg-config`,
然后输入对应的 lib, 比如 `gtk+-3.0` 来往 
`flycheck-clang-include-path` 等变量中加入项目的库的地址. 

## 编写并编译
一个简单的程序如下: 

```c
#include <gtk/gtk.h>

#define WIDTH 300
#define HEIGHT 200

int main (int argc, char *argv[]) {
  gtk_init(&argc, &argv);

  GtkWidget* window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_widget_set_size_request(window, WIDTH, HEIGHT);
  gtk_window_set_title(GTK_WINDOW(window), "Basic Example");
  g_signal_connect(window, "destroy", G_CALLBACK(gtk_main_quit), NULL);
  
  gtk_widget_show_all(window);
  gtk_main();
  return 0;
}
```

编译:

```shell
gcc main.c `pkg-config --libs --cflags gtk+-3.0` -o main
```

然后运行: 

```shell
./main
```

这样我们就能够看到一个最最朴实无华的界面了. 

## 接下来? 
这个只是一个记录怎么做的一个小记录. 之后再更新吧.

### Glade
在我以前用 [Shoes](http://shoesrb.com) 写一些简单的小玩具的时候,
我就幻想过能否直接有一个程序能够所见即所得地编辑 GUI. 
因为那样真的超级方便. 

而 Glade 的操作基本上就是干的就是这样的事情. 在 macOS 上:

```shell
brew install glade
```

安装完后, 使用:

```shell
glade
```

来运行. 可以看到一个比较简约的界面: 

![glade.png]({{ site.github.url }}/_img/pieces/glade.png "不过我讨厌这个设计出现在我的电脑上, 不是因为其不好看, 我觉得还是很好看的, 就是 UI 和我的电脑 macOS 不匹配. ")

新建文件后, 可以按照: 搭建窗口 - 选择窗口布局 - 添加元件的一个逻辑来编辑. 

1. 搭建窗口: 选择 Toplevel - GtkWindow 可以创建一个窗口. 
2. 选择窗口布局: 在创建的窗口下, 选择 Containers - GtkFixed 然后添加到刚创建的窗口中
3. 添加元件: 通过 Control 或者 Display 的选项下拉菜单来添加元件

于是就能够得到嵌套关系: 

```text
+--- GtkWindow
 +--- GtkFixed
  +--- GtkButton
  +--- GtkLabel
  +--- ...
```

对于每个关系, 都能够通过右侧的属性菜单来编辑属性, 设置 `id`. 
其中需要注意的两点: 

* `id` 是用来在程序中访问对应元素的名字
* `signal` 是让该元件聆听特定信号的事件, 
  类似于 javascript 中的 `addEventListener`.
  
在编辑好后保存为 `.glade` 文件. 编写代码: 

```c
 GtkBuilder *builder = gtk_builder_new();

  if (gtk_builder_add_from_file(builder, "gui.glade", NULL) == 0) {
    printf("Error. \n");
    return 0;
  }

  GtkWidget *welcome_window = GTK_WIDGET(gtk_builder_get_object(builder, "welcome_window"));
  GtkWidget *quit_button = GTK_WIDGET(gtk_builder_get_object(builder, "quit_button"));

  g_signal_connect(welcome_window, "destroy", G_CALLBACK(gtk_main_quit), NULL);
  g_signal_connect(quit_button, "clicked", G_CALLBACK(on_quit_button_clicked), NULL);
```

即可用来控制程序了. 

(不过对于我要做的东西来说, 可能有点没用, 不如自己直接去写. 
并且在我的 mac 上, 不知道是不是因为使用不当, 闪退并且操作不太舒服.
最坑爹的事情是, 文档太少了, 所以学得不太明白. )

所以换回之前脱裤子放屁的那个做法. 

主要参考的连接:
* [【Ruby】10分でGladeを使って作るRuby/GTK3 GUIアプリ](https://qiita.com/kojix2/items/d9da8e08757dcc6b1a7d)
* [Glade 3 でGUI開発](https://www.nslabs.jp/glade.rhtml)
* [GTK Glade C Programming Tutorial](https://web.archive.org/web/20210507031457/https://prognotes.net/gtk-glade-c-programming/)

### Handmake GTK Code
这一部分主要是参考了 [入門 GTK+](http://iim.cs.tut.ac.jp/member/sugaya/GTK+/files/gtkbook-20210127.pdf) 以及 [ZetCode](https://zetcode.com/gui/gtk2/)
(为什么是日语的呢? 也不是我想学, 而是那个教程写得实在是傻瓜, 
几乎把所有的操作都截图了. 反正只看代码, 看看截图, 大概就好了. 
而后面的是一个英文的教程. 不过有一个中文的 [翻译](https://www.kancloud.cn/apachecn/zetcode-zh/1950310))

## 参考
* [How do you run Ubuntu Server with a GUI?](https://askubuntu.com/questions/53822/how-do-you-run-ubuntu-server-with-a-gui)
* [How to install gtk on OSX for use with g++/gcc compiler](https://stackoverflow.com/questions/20098862/how-to-install-gtk-on-osx-for-use-with-g-gcc-compiler)
* [flycheck-pkg-config Github](https://github.com/Wilfred/flycheck-pkg-config)
* [GTK+3入門(C言語)](https://jitaku.work/it/category/gui/gtk-plus/)
* [入門 GTK+](http://iim.cs.tut.ac.jp/member/sugaya/GTK+/files/gtkbook-20210127.pdf)
