---
layout: post
title:  "Little Sound Dj Manual"
date:   2025-1-25
categories: jekyll
math: true
---
<style type="text/css">
img {
  width: 300px;
}
</style>

# Table of Contents

1.  [About](#org2175a95)
2.  [LSDJ Manual (9\_2\_6)](#orge557a05)
    1.  [Getting Started](#orgdb40db9)
        1.  [Game Boy Sound](#org75f998a)
        2.  [Key Presses](#orgd0143af)
        3.  [Navigating the Program](#org94d8da1)
        4.  [Online Help](#org554bad9)
        5.  [Making Your First Sounds](#org0ba08f1)
        6.  [Hexadecimal Number System](#org29e1cec)
        7.  [Initial Troubleshooting](#orgb6a2360)
    2.  [The Screens](#org0f59458)
        1.  [Screen Map](#orgb38643a)
        2.  [Starting and Stopping](#org0f39187)
        3.  [Song Screen](#org4a8cc79)
        4.  [Chain Screen](#orgf717492)
        5.  [Phrase Screen](#org3d589b9)
        6.  [Instrument Screen](#org260e8ef)
        7.  [Table Screen](#org710bfac)
        8.  [Groove Screen](#orgb8ef217)
        9.  [Synth Screen](#org12aa9c3)
        10. [Wave Screen](#org898f0e7)
        11. [Project Screen](#orgd9c3f54)
        12. [File Screen](#org154bb4a)
        13. [Border Information](#org8afd5fc)
    3.  [Advanced Techniques](#orgd42444c)
        1.  [Copy and Paste](#org0d912e0)
        2.  [Cloning](#org66586be)
        3.  [The Importance of Backups](#org8fd8794)
        4.  [Muting, Soloing and Panning](#org15726bd)
        5.  [Live Mode](#orgefa579f)
        6.  [Synthetic Drum Instruments](#org55f0191)
    4.  [Commands](#org7207dc1)
        1.  [A: Table Start/Stop](#org9164593)
        2.  [B: Maybe](#orge82b18f)
        3.  [C: Chord](#org6067cdd)
        4.  [D: Delay](#orga017e9a)
        5.  [E: Amplitude Envelope](#org2a63d32)
        6.  [F: Wave Frame/Finetune](#org58fd7a7)
        7.  [G: Groove Select](#org372b258)
        8.  [H: Hop](#orgcfa20a8)
        9.  [K: Kill Note](#orgfb4210b)
        10. [L: Slide](#org9ee3827)
        11. [M: Master Volume](#org4553129)
        12. [O: Set Output](#orgd029d21)
        13. [P: Pitch Bend](#orgda21ac6)
        14. [R: Retrig/Resync](#org264271a)
        15. [S: Sweep/Shape](#org290de49)
        16. [T: Tempo](#org4542f7d)
        17. [V: Vibrato](#org388289f)
        18. [W: Wave](#org614bc62)
        19. [Z: RandomiZe](#orgc6cb9bc)
    5.  [Synchronization](#org04ddea8)
    6.  [Appendix A: Sample Kits](#org49ff945)
    7.  [Appendix B: Allophones](#org1281a8a)
    8.  [Appendix C: SRAM Memory Map](#orgff09472)
3.  [Dictionary](#orge0dcbe0)

<a id="org2175a95"></a>

# About

不是 [DAW](#orgcd6a4d5) 买不起, 而是 Game Boy 更有意思.
小小翻译一下 [LSDJ](#org734d1a9) 的手册, 之后再去学点简单的编曲估计就可以了.
感觉 bit tone 音乐真有意思.

阅读指南:

-   可以先阅读 [2.1.5](#org0ba08f1) 以大概了解如何使用 [LSDJ](#org734d1a9)
-   然后快速翻阅一遍 [2.2](#org0f59458) (大概了解有哪些 Screen),
    然后快速看一遍 [2.4](#org7207dc1) (大概了解有那些命令 `A-Z`)
-   去读点编曲教材或者自动编曲的文章

<a id="orge557a05"></a>

# LSDJ Manual (9\_2\_6)


<a id="orgdb40db9"></a>

## Getting Started


<a id="org75f998a"></a>

### Game Boy Sound

[Game Boy](#orgca69ab8) 的 APU 包含 4 个通道, 每个通道的分辨率为 4-bit.

-   **Pluse Channel 1**: 带有 [包络](#orgff49735) 以及 [扫频](#org4468189) 的方波输出
-   **Pluse Channel 2**: 带有 [包络](#orgff49735) 的方波输出
-   **Wave Channel**: [软件合成器](#orgd2428f2), [采样播放](#orga1e5098) 以及 [语音合成器](#org5e8b5a7)
-   **Noise Channel**: 带有 [包络](#orgff49735) 和 [形状](#orga721860) 的噪声通道


<a id="orgd0143af"></a>

### Key Presses

在本文档中, 按键有如下的记号:

-   `A`, `B`, `START`, `SELECT` 按键
-   `CURSOR` 十字键:
    -   `LEFT`, `RIGHT`, `UP`, `DOWN`
    -   `LEFT/RIGHT`, `UP/DOWN`
-   `SELECT+A` 在按下 `SELECT` 时同时按 `A`
-   `SELECT+(B,B)` 在按下 `SELECT` 时同时按两次 `B`


<a id="org94d8da1"></a>

### Navigating the Program

![img](/_img/lsdj/song-screen.png "Song Screen")

[LSDJ](#org734d1a9) 在启动时进入 **S**​ong Screen. 其中的四列 (*PU1*, *PU2*, *WAV*, *NOI*)
代表 Game Boy 的四个声道: 两个脉冲, 一个任意波形以及一个噪声通道.
使用十字键可以将光标在不同的通道间切换.

[LSDJ](#org734d1a9) 有九个 screen, 按照右下角的 map (小地图) 进行排列.

在 map 的中间栏为: **S**​ong, **C**​hain, **P**​hrase, **I**​nstrucment 以及 **T**​able.
其按照细节程度进行排列: Song 为 Chain 的排列, 而 Chain 是 Phrase
的组合, Phrase 由音符 Note 组成.

使用 `SELECT+CURSOR` 在不同的 screen 间切换.


<a id="org554bad9"></a>

### Online Help

<a id="org408b834"></a>

![img](/_img/lsdj/project-screen-help.png "Project Screen Help")

内置的帮助可以通过在 Project screen 中, 将光标移动到 HELP 上按 `A` 打开.
Help screen 中会列出不同 screen 的按键以及命令表.


<a id="org0ba08f1"></a>

### Making Your First Sounds

-   在 Song screen, 将光标移动到 *PU1* 列上,
    按下 `A` 键将会新建一个名为 `00` 的 Chain;
-   使用 `SELECT+RIGHT` 移动到 Chain screen.

    在 Chain Screen, 按下 `A` 在 Chain 中插入一个新的 Phrase;
-   使用 `SELECT+RIGHT` 移动到 Phrase Screen.

    在 Phrase Screen, 按下 `A` 可以插入音符:

    ![img](/_img/lsdj/phrase-screen.png "Phrase Screen")

    其中 `C` 表示 [音符](#org8c7e5ac), `2` 表示 [音阶](#org2191143).
    按下 `START` ([音符](#org8c7e5ac) 将会按照从上到下的顺序) 将会播放这段 phrase.

    -   当按住 `A` 键时, 可以使用十字键更改 [音符](#org8c7e5ac):
        -   `A+LEFT/RIGHT` 更改 [音符](#org8c7e5ac)
        -   `A+UP/DOWN` 更改 [音阶](#org2191143)
    -   使用十字键可以移动并在其他位置插入 [音符](#org8c7e5ac);
    -   在按住 `B` 的同时, 按下 `A` 键可以删除当前的 [音符](#org8c7e5ac);
-   使用 `SELECT+RIGHT` 移动到 Instrument Screen.

    ![img](/_img/lsdj/instrument-screen.png "Instrument Screen")

    在 Instrument Screen 中:

    -   使用 `A+LEFT/RIGHT` 可以修改 `ENV` ([包络](#orgff49735))

        **例**: 将包络设为 `83` 将会让声音听起来更短
    -   使用 `A+LEFT/RIGHT` 可以修改 `TYPE` (乐器类型)

        乐器类型是和通道类型强关联的:
        `PULSE` 类型的乐器只能用于脉冲通道 (Pluse);
        `WAVE` 和 `KIT` 类型的乐器只能用于波形 (Wave) 通道;
        `NOISE` 类型的乐器只能用于噪声 (Noise) 通道.

    **例**: Drum Kits: 在 Song Screen 中选择 *WAVE* 通道,
    并设置 `INSTR` 的类型为 `KIT` <sup><a id="fnr.drum-kits" class="footref" href="#fn.drum-kits" role="doc-backlink">1</a></sup>


<a id="org29e1cec"></a>

### Hexadecimal Number System

[LSDJ](#org734d1a9) 使用十六进制数的表示 <sup><a id="fnr.hex-presentation" class="footref" href="#fn.hex-presentation" role="doc-backlink">2</a></sup>.
十六进制的作用是减少数的表示所占用的空间大小.
需要注意的是负数的表示是反码 (但是没有符号位),
所以会稍微有些难懂.


<a id="orgb6a2360"></a>

### Initial Troubleshooting

假如卡带没有正常启动, 崩溃了, 或者运行得比较奇怪, 可以尝试:

-   换上新的电池以防止丢失数据
-   用棉签和酒精清洁卡带触电
-   多次重新插拔卡带来清除触电上的氧化物
-   确保卡带插紧了 (也许可以使用胶带来帮助卡带固定)
-   初始化卡带的储存: 在 Project Screen 上的 `LOAD/SAVE FILE`
    上按下 `SELECT+A+B`
-   部分 Game Boy Advance/Nintendo DS 卡带不支持 [LSDJ](#org734d1a9),
    可以尝试 "[Goomba](https://www.littlesounddj.com/lsd/latest/rom_images/goomba/)" 版本的 [LSDJ](#org734d1a9).
-   LSDJ Wiki


<a id="org0f59458"></a>

## The Screens

[LSDJ](#org734d1a9) 有 9 个 Screen, 在小地图中以 $5 \times 3$ 的结构进行排布.


<a id="orgb38643a"></a>

### Screen Map

<!-- This HTML table template is generated by emacs 30.0.93 -->
<table border="1">
  <tr>
    <td colspan="2" align="left" valign="top">
      &nbsp;Project&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
    </td>
    <td colspan="2" align="left" valign="top">
      &nbsp;Synth&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
    </td>
    <td align="left" valign="top">
      &nbsp;Wave&nbsp;&nbsp;
    </td>
  </tr>
  <tr>
    <td align="left" valign="top">
      &nbsp;Song&nbsp;
    </td>
    <td align="left" valign="top">
      &nbsp;Chain&nbsp;
    </td>
    <td align="left" valign="top">
      &nbsp;Phrase&nbsp;
    </td>
    <td align="left" valign="top">
      &nbsp;Instr.&nbsp;
    </td>
    <td align="left" valign="top">
      &nbsp;Table&nbsp;
    </td>
  </tr>
  <tr>
    <td colspan="5" align="left" valign="top">
      &nbsp;Groove&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
    </td>
  </tr>
</table>

其中 <sup><a id="fnr.hidden-screens" class="footref" href="#fn.hidden-screens" role="doc-backlink">3</a></sup>

-   Song, Chain, 和 Phrase Screen 用于编曲;
-   Wave, Synth, Instrument 和 Table Screen 用于修改声色;
-   Project Screen 包含项目的设定
-   Groove Screen 控制音序器的时间

在编曲时, 主要使用的是中间的一行.

使用 `SELECT+CURSOR` 在不同的 Screen 之间切换.


<a id="org0f39187"></a>

### Starting and Stopping

-   在 Song Screen 按下 `START` 时, [LSDJ](#org734d1a9) 将会播放所有四个通道;
-   在其他 Screen 按下 `START` 时, [LSDJ](#org734d1a9) 只会播放当前编辑的通道;
-   若要在其他 Screen 播放所有的通道, 按下 `SELECT+START`


<a id="org4a8cc79"></a>

### Song Screen

![img](/_img/lsdj/song-screen.png "Song Screen")

Song Screen 为音序器的最高层级. (其他说明前面写过了, 略)

**Tip**:

-   在空 step 上按下 `B+A` 可以将其下方的 Chains 向上移动;
-   按 `B+UP/DOWN` 快速跳到下一页的 row


<a id="orgf717492"></a>

### Chain Screen

![img](/_img/lsdj/chain-screen-example.png "当前的 Chain 包含 Phrase 3 (转调 5 个半音), 后面跟着 Phrase 4, 5, 6")

Chain 包含一列可播放的 Phrases. 可以用来表示旋律,
或是 Bass Line. 每个 Phrases 都有一个可选的变调 (transpose).

Chains 可以被不同的通道重复使用.
比如可以在两个脉冲通道里面播放一个 Chain.


<a id="org3d589b9"></a>

### Phrase Screen

![img](/_img/lsdj/phrase-screen.png "Phrase Screen")

在 Phrase Screen 中输入 [音符](#org8c7e5ac). 包含四列: Note (音符),
Instrument (乐器), Command (命令) 以及 Command Value (命令参数).

Phrase 在不同的通道间可以共享. 但是在不同的通道里一个 Phrase
可能听起来有些不同. 比如在脉冲通道里面播放的旋律,
可能在其他通道里其他听起来有些不同.

-   Note 列:
    根据所选取的乐器不同, Note 列的显示可能有些不同.
    -   通常会显示 [音符](#org8c7e5ac) 和 [音阶](#org2191143)
    -   对于 KIT 或 SPEECH 类型的 [采样](#orga1e5098) 将会显示采样的名称
    -   对于 NOISE 类型将会显示其值或 [音符](#org8c7e5ac) 和 [音阶](#org2191143)
-   INSTR 列:
    显示的是音符所用的乐器. 在其上按下 `SELECT+RIGHT`
    可以编辑在 Instrument Screen 中对应的乐器.
-   CMD 列:
    用于添加命令. 不同的命令有不同的特殊功能.
    比如 `H` (HOP) 跳至下一个 Phrase. 在任意的命令上按下 `A,A`
    可以弹出 Quick-Help 显示.

**Tip**:

-   使用 `B+A` 来剪切当前选择的音符
-   对于 DRUM KIT 可以使用 `A+RIGHT` 直到显示 `OFF` 来将 KIT 静音
    (只在 [采样](#orga1e5098) 少于 15 个的情况下有用)


<a id="org260e8ef"></a>

### Instrument Screen

![img](/_img/lsdj/instrument-screen.png "Instrument Screen")

有五种乐器类型:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Type</th>
<th scope="col" class="org-left">Function</th>
<th scope="col" class="org-left">Channel</th>
</tr>
</thead>
<tbody>
<tr>
<td class="org-left"><b>PULSE</b></td>
<td class="org-left">Pulse Wave</td>
<td class="org-left"><i>PU1</i>, <i>PU2</i></td>
</tr>

<tr>
<td class="org-left"><b>WAVE</b></td>
<td class="org-left">waves from Synth, Wave Screen</td>
<td class="org-left"><i>WAV</i></td>
</tr>

<tr>
<td class="org-left"><b>KIT</b></td>
<td class="org-left">samples from <a href="#orgd3adc68">ROM</a></td>
<td class="org-left"><i>WAV</i></td>
</tr>

<tr>
<td class="org-left"><b>NOISE</b></td>
<td class="org-left">pitched 7-bit or 15-bit noise</td>
<td class="org-left"><i>NOI</i></td>
</tr>

<tr>
<td class="org-left"><b>SPEECH</b></td>
<td class="org-left">Intrument 40 for speech</td>
<td class="org-left"><i>WAV</i></td>
</tr>
</tbody>
</table>

在 `TYPE` 栏按下 `A+CURSOR` 可以更改乐器的类型.

乐器并不会自动匹配对应的通道.
所以在选择乐器的时候需要确保其应与通道相匹配.

1.  Instrument Parameters

    下面是乐器的通用参数:

    <table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


    <colgroup>
    <col  class="org-left" />

    <col  class="org-left" />
    </colgroup>
    <thead>
    <tr>
    <th scope="col" class="org-left">NAME</th>
    <th scope="col" class="org-left">按 <code>A</code> 改名字.</th>
    </tr>

    <tr>
    <th scope="col" class="org-left">&#xa0;</th>
    <th scope="col" class="org-left">乐器的名称会在 Phrase Screen 选中时在边栏显示</th>
    </tr>
    </thead>
    <tbody>
    <tr>
    <td class="org-left">TYPE</td>
    <td class="org-left">乐器类型</td>
    </tr>
    </tbody>
    <tbody>
    <tr>
    <td class="org-left">LENGTH</td>
    <td class="org-left">声音长度</td>
    </tr>
    </tbody>
    <tbody>
    <tr>
    <td class="org-left">OUTPUT</td>
    <td class="org-left">控制声道输出 (左/双/右)</td>
    </tr>
    </tbody>
    <tbody>
    <tr>
    <td class="org-left">PITCH</td>
    <td class="org-left">控制 <code>P</code>, <code>L</code>, <code>V</code> 命令的行为.</td>
    </tr>

    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left"><code>A+U/D</code> 改变 pitch 更新的速度: FAST 将 pitch 更新为 360Hz; TICK</td>
    </tr>

    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left">在每次 tick 更新 pitch; STEP 类似于 FAST, 但是 P does pitch</td>
    </tr>

    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left">change instead of pitch bend; DRUM 类似于 FAST 但是有对数衰减,</td>
    </tr>

    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left">常用于 P kick. <code>A+L/R</code> 在下三角波, 锯齿和方波, 上三角波,</td>
    </tr>

    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left">以及锯齿和方波之间改变振动的形状.</td>
    </tr>
    </tbody>
    <tbody>
    <tr>
    <td class="org-left">TRANSP</td>
    <td class="org-left">当为 <code>ON</code> 的时候, pitch 将会受 Project 和 Chain 而改变</td>
    </tr>
    </tbody>
    <tbody>
    <tr>
    <td class="org-left">CMD/RATE</td>
    <td class="org-left">减缓 <code>C</code> 和 <code>R</code> 命令的速度.</td>
    </tr>

    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left">当 PITCH 设为 TICK 时, 也影响 <code>P</code> 和 <code>V</code> 命令 (<code>0</code> 最快, <code>F</code> 最慢)</td>
    </tr>
    </tbody>
    <tbody>
    <tr>
    <td class="org-left">TABLE</td>
    <td class="org-left">按 <code>SELECT+RIGHT</code> 编辑播放 <a href="#org8c7e5ac">音符</a> 时所用的 Table;</td>
    </tr>

    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left">按 <code>A,A</code> 创建一个新的 Table; 按 <code>SELECT+(B,A)</code> 克隆一个 Table.</td>
    </tr>

    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left">修改 TICK 为 STEP 可以使得 <a href="#org734d1a9">LSDJ</a> 历遍 Table (每次增加一步)</td>
    </tr>
    </tbody>
    </table>

    **Tip**

    -   在 Instrument Screen, 按下 `A,A` 可以显示 Quick-help

2.  Pulse Instrument Parameters

    ![img](/_img/lsdj/instrument-screen.png "Pulse Instrument Screen (其中的包络为 32/AD/10)")

    <table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


    <colgroup>
    <col  class="org-left" />

    <col  class="org-left" />
    </colgroup>
    <thead>
    <tr>
    <th scope="col" class="org-left">ENV.</th>
    <th scope="col" class="org-left">有三段包络控制值: 每一位都第一位控制幅值,</th>
    </tr>

    <tr>
    <th scope="col" class="org-left">&#xa0;</th>
    <th scope="col" class="org-left">第二个值控制上升/下降的速度.</th>
    </tr>
    </thead>
    <tbody>
    <tr>
    <td class="org-left">WAVE</td>
    <td class="org-left">波形类型</td>
    </tr>
    </tbody>
    <tbody>
    <tr>
    <td class="org-left">SWEEP</td>
    <td class="org-left">扫频的设置: 第一位设置时间,</td>
    </tr>

    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left">第二位设置音调与下降. 只在 <i>PU1</i> 起效.</td>
    </tr>
    </tbody>
    <tbody>
    <tr>
    <td class="org-left">PU2 TSP.</td>
    <td class="org-left">Transpose <i>PU2</i></td>
    </tr>
    </tbody>
    <tbody>
    <tr>
    <td class="org-left">FINETUNE</td>
    <td class="org-left">detune <i>PU1</i> downwards, <i>PU2</i> upwards</td>
    </tr>
    </tbody>
    </table>

    **Note**: 包络被分成三段, 每段都是由 Amplitude 和 Speed 两部分组成.

3.  Wave Instrument Parameters

    ![img](/_img/lsdj/wave-instrument-parameter.png "Wave Instrument Parameters")

    WAVE 类型的乐器将会播放在 SYNTH Screen 编辑的合成器声音.

    <table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


    <colgroup>
    <col  class="org-left" />

    <col  class="org-left" />
    </colgroup>
    <thead>
    <tr>
    <th scope="col" class="org-left">VOLUME</th>
    <th scope="col" class="org-left">设置音量 (增幅, <code>0</code> 0%, <code>1</code> 25%, <code>2</code> 50%, <code>3</code> 100%) 以及左右声道的输出</th>
    </tr>
    </thead>
    <tbody>
    <tr>
    <td class="org-left">FINETUNE</td>
    <td class="org-left">detune</td>
    </tr>
    </tbody>
    <tbody>
    <tr>
    <td class="org-left">WAVE</td>
    <td class="org-left">选择 <a href="#orga1e5098">采样的波形</a>. (可以在 Wave Screen 设置波形)</td>
    </tr>

    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left">当 <code>PLAY</code> 设置为 <code>MANUAL</code> 以外的值时, 其参数会被替换为 <code>SYNTH</code></td>
    </tr>
    </tbody>
    <tbody>
    <tr>
    <td class="org-left">SYNTH</td>
    <td class="org-left">选择合成器声音. (可以在 Synth Screen 设置. <code>A,A</code> 新建一个合成器,</td>
    </tr>

    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left"><code>SELECT+(B,A)</code> 可以克隆波形, <code>SELECT+(B,A)</code> 可以克隆合成器)</td>
    </tr>
    </tbody>
    <tbody>
    <tr>
    <td class="org-left">PLAY</td>
    <td class="org-left">如何播放采样: <code>MANUAL</code>, <code>ONCE</code>, <code>LOOP</code> 或 <code>PINGPONG</code>.</td>
    </tr>

    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left"><code>MANUAL</code> 只会播放一个波形. 若要在 <code>MANUAL</code> 模式下播放合成器声音,</td>
    </tr>

    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left">则需要使用 <a href="#org58fd7a7"><code>F</code></a> 命令在波形间步进.</td>
    </tr>
    </tbody>
    <tbody>
    <tr>
    <td class="org-left">SPEED</td>
    <td class="org-left">设置合成器声音播放的速度</td>
    </tr>
    </tbody>
    <tbody>
    <tr>
    <td class="org-left">LENGTH</td>
    <td class="org-left">合成器声音的长度</td>
    </tr>
    </tbody>
    <tbody>
    <tr>
    <td class="org-left">LOOP POS</td>
    <td class="org-left">合成器声音的循环位置</td>
    </tr>
    </tbody>
    </table>

4.  Kit Instrument Parameters

    ![img](/_img/lsdj/kit-instrument-parameter.png "Kit Instrument Parameters")

    <table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


    <colgroup>
    <col  class="org-left" />

    <col  class="org-left" />
    </colgroup>
    <thead>
    <tr>
    <th scope="col" class="org-left">KIT</th>
    <th scope="col" class="org-left">选择所用的 <a href="#org5180cd5">鼓机</a>.</th>
    </tr>

    <tr>
    <th scope="col" class="org-left">&#xa0;</th>
    <th scope="col" class="org-left">第一个鼓机用于 Phrase 左侧音符,</th>
    </tr>

    <tr>
    <th scope="col" class="org-left">&#xa0;</th>
    <th scope="col" class="org-left">第二个鼓机用于 Phrase 右侧音符</th>
    </tr>
    </thead>
    <tbody>
    <tr>
    <td class="org-left">VOLUME</td>
    <td class="org-left">设置音量 (幅值 <code>0</code> 0%, <code>1</code> 25%, <code>2</code> 50%, <code>3</code> 100%),</td>
    </tr>

    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left">以及输出 (左右声道, 双声道或关闭)</td>
    </tr>
    </tbody>
    <tbody>
    <tr>
    <td class="org-left">FINETUNE</td>
    <td class="org-left">音调修改</td>
    </tr>
    </tbody>
    <tbody>
    <tr>
    <td class="org-left">OFFSET</td>
    <td class="org-left">设置循环开始的位置.</td>
    </tr>

    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left">若 <code>LOOP</code> 为 <code>OFF</code>, <code>OFFSET</code> 可以用于跳过循环开头.</td>
    </tr>
    </tbody>
    <tbody>
    <tr>
    <td class="org-left">SPEED</td>
    <td class="org-left"><code>1X</code> (全速) 或 <code>0.5X</code> (半速)</td>
    </tr>
    </tbody>
    <tbody>
    <tr>
    <td class="org-left">CLIP</td>
    <td class="org-left">选择两个鼓机的声音信号如何混合.</td>
    </tr>

    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left"><code>HARD</code> 为默认值;</td>
    </tr>

    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left"><code>SOFT</code> 将衰减信号以减少失真, 产生类似磁带的效果;</td>
    </tr>

    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left"><code>FOLD</code> 和 <code>WRAP</code> 将信号 <a href="#org8e943f3">折叠</a> (fold) 或 <a href="#orge164fe3">包裹</a> (wrap), 其阈值为 <code>0</code> 到 <code>F</code> 之间设置.</td>
    </tr>

    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left">在 <code>HARD</code> 模式下, 按下 <code>A+(LEFT,LEFT)</code> 可以使用 raw memory 内容进行混合.</td>
    </tr>
    </tbody>
    </table>

    **Tip**:

    -   想要替换默认的鼓机采样, 使用 `ladpatcher` 程序. ([Github](https://github.com/jkotlinski/lsdpatch))

5.  Noise Instrument Parameters

    ![img](/_img/lsdj/noise-instrument-parameter.png "Noise Instrument Parameters")

    <table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


    <colgroup>
    <col  class="org-left" />

    <col  class="org-left" />
    </colgroup>
    <thead>
    <tr>
    <th scope="col" class="org-left">ENV.</th>
    <th scope="col" class="org-left">同 <a href="#org57b4b03">2.2.6.2</a></th>
    </tr>
    </thead>
    <tbody>
    <tr>
    <td class="org-left">PITCH</td>
    <td class="org-left">当设为 <code>FREE</code> 时, <a href="#orgdb2f8b5">音调</a> 的改变可能会随机静音;</td>
    </tr>

    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left"><code>SAFE</code> 通过在 <a href="#orgdb2f8b5">音调</a> 改变的时候重新开始声音来避免随机的静音.</td>
    </tr>
    </tbody>
    </table>

6.  Speech Instrument

    ![img](/_img/lsdj/speech-instrument-parameter.png "Speech Instrument Parameters")

    [LSDJ](#org734d1a9) 有 59 个叫作 [allophones](#org1281a8a) 的 speech sound.
    通过组合这些 [allophones](#org1281a8a) 理论上可以说出任意的英语或短语.

    Speech 类型的乐器被固定为 `40` 的编号, 并且只能用于 *WAV* 通道.
    其包含 `14` 个 word 槽, 默认从 `W-0` 到 `W-D` 进行命名.

    若需要编辑 word, 在其上按下 `SELECT+RIGHT` 可以进入 Word Screen.

    ![img](/_img/lsdj/word-example.png "Example Word: (应该会说 Little Sound Dj)")

    在 Speech Instrument Screen 上按 `A` 可以给 Word 进行命名,
    以使得 Word 更加任意记住.

    **Tip**:

    -   在编辑 [allophones](#org1281a8a) 的时候, 请考虑它们听起来如何而不是拼起来如何;
    -   在英文里, 相同的字符在不同的词里面听起来可能会不同


<a id="org710bfac"></a>

### Table Screen

![img](/_img/lsdj/table-slide.png "Table Screen")

Table 为一系列 [变调](#orgc2bd5e1), [命令](#org7207dc1) 以及 [音量](#org98ff6cf) 变化的组合.
其可以以任意速度运行, 也可以应用到任意的通道上.
通过在 [Instrument Screen](#org260e8ef) 中设置 Table, Table
将会在每次该乐器播放的时候都生效.
这可以实现一些单独乐器无法实现的有趣效果.

Table 包含 6 列:

-   第一列为 [包络](#orgff49735), 用于控制自定义的增幅包络;
-   第二列为 [变调](#orgc2bd5e1), 用于控制当前 [音符](#org8c7e5ac) 的 [变调](#orgc2bd5e1) (以 [半音](#org67fac3c) 为单位)
-   其他列则类似 Phrase 为 [命令](#org7207dc1)

默认的 Table 速度为 1 tick/step.
可以通过 [`G`](#org372b258) 来改变 Table 速度.
使用 `B+CURSOR` 在不同的 Table 之间切换.

**Tip**

-   在 Phrase Screen 上的 [`A`](#org9164593) 命令按下 `SELECT+RIGHT`
    可以编辑对应的 Table. 按下 `SELECT+LEFT` 来返回.

1.  Envelope Example

    ![img](/_img/lsdj/table-envelope-example.png "Table Envelope with Tremolo Effect")

    [包络](#orgff49735) 参数的第一位设置了 [振幅](#org98ff6cf), 第二位设置了包络持续的时间
    (持续多少个 tick). 在设置循环时, 第一位参数设置想要跳转到的 step,
    第二位参数会被传给 [`H`](#orgcfa20a8) 命令.

    在图 [120](#org60a8ec4) 中构建了一个 [Tremolo](#orgb08c327) 效果.

2.  Arpeggio Example

    ![img](/_img/lsdj/table-transpose-example.png "Table Transpose with Major Arpeggio")

    应用 Table 的另一个常见的例子是 [琶音](#org5872bf7),
    即通过快速播放音符来实现类似和弦的效果.

    图 [123](#orga304af0) 中实现了一个 [大三和弦](#org5a3547c) 的效果.
    当然, 也可以用 [`C`](#org6067cdd) 实现一些较短的 [琶音](#org5872bf7).


<a id="orgb8ef217"></a>

### Groove Screen

![img](/_img/lsdj/groove-screen.png "Groove Screen")

Groove 控制了播放 Phrases 和 Table 的速度. 假如合理使用,
可以使得你的音乐更加有活力.

[音序器](#orgfd705a1) 按照 tick 为最小单位进行更新, 其持续时间由歌曲的 [节奏](#org963c929) 控制.
对于 125 BPM, 约为 50 tick/second. 更高的 [节奏](#org963c929) 意味着更快的 tick,
反之则更慢. 在 Groove Screen 中, 可以控制每个 Phrase 或 Table
step 将持续多少个 tick. 图 [126](#org0178721) 设置了每个 step 包含 6 个 tick.

![img](/_img/lsdj/swing-example.png "Swing Example")

也可以使用 Groove 来创建自定义的 [旋律](#orgd84b25d).
图 [129](#org60c9baf) 将设置偶数 [音符](#org8c7e5ac) step 持续 8 个 ticks,
奇数 [音符](#org8c7e5ac) step 持续 5 个 ticks.
Groove 也可以用于创作 [三连音符](#orgf48c317) 或其他更加复杂的 [旋律](#orgd84b25d).

所有的 Phrase 的默认 Groove 是 Groove `0`.
但是也可以通过 [`G`](#org372b258) 命令选择不同的 Groove,
该命令也可以用在 Table 中.

在 Groove Screen 中可以用 `B+CURSOR`.

**Tip**

-   `A+UP/DOWN` 会在不改变 ticks 总数 (即歌曲的速度)
    的情况下改变 swing 的比例.

    **例**: 比如说原本的值为 `6/6` (50%),
    按下 `A+UP` 则会将其值变为 `7/5` (58%).
-   在 [`G`](#org372b258) 命令上按下 `SELECT+DOWN` 可以编辑对应的 Groove


<a id="org12aa9c3"></a>

### Synth Screen

![img](/_img/lsdj/synth-screen.png "Synth Screen")

Synth Screen 提供了一个 [软件合成器](#orgd2428f2) 以通过 *WAV* 通道播放声音.
每隔合成器的声音可以使用 10 个波形. 合成器声音 `0` 使用了
`00-0F` 的波形, 合成器 `1` 使用了 `10-1F` 的波形, 以此类推.
生成的合成器波形可以在 [Wave Screen](#org898f0e7) 中预览.

总共有 16 个合成器声音. 可以通过 `B+CURSOR` 切换不同的合成器声音.

1.  Fixed Synth Settings

    <table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


    <colgroup>
    <col  class="org-left" />

    <col  class="org-left" />
    </colgroup>
    <thead>
    <tr>
    <th scope="col" class="org-left">SIGNAL</th>
    <th scope="col" class="org-left"><a href="#org199ce8b">方波</a>, <a href="#orgfe01183">锯齿波</a>, <a href="#orgf2b489c">三角波</a> 或自定义的波形.</th>
    </tr>

    <tr>
    <th scope="col" class="org-left">&#xa0;</th>
    <th scope="col" class="org-left"><code>W.FX</code> 使用 <code>F0-FF</code> 的波形作为信号</th>
    </tr>
    </thead>
    <tbody>
    <tr>
    <td class="org-left">FILTER</td>
    <td class="org-left"><a href="#org1c34574">低通</a>, <a href="#org889a962">高通</a>, <a href="#org3902f02">带通</a> 或者 <a href="#orgfffe43c">全通</a></td>
    </tr>
    </tbody>
    <tbody>
    <tr>
    <td class="org-left">DIST</td>
    <td class="org-left"><a href="#org81a5b7a">失真</a> (Distortion) 模式.</td>
    </tr>

    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left"><code>CLIP</code> 将根据 <code>LIMIT</code> 参数限制波形;</td>
    </tr>

    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left"><code>FOLD</code> 将反转超出 <code>LIMIT</code> 参数设定的波形;</td>
    </tr>

    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left"><code>WRAP</code> 将偏移超出 <code>LIMIT</code> 参数设定的区域.</td>
    </tr>
    </tbody>
    <tbody>
    <tr>
    <td class="org-left">PHASE</td>
    <td class="org-left">水平地压缩波形.</td>
    </tr>
    </tbody>
    </table>

2.  Variable Synth Settings

    通过设置第一个和最后一个声音的波形, 来产生一个平滑的 [淡出](#org6e908d5) 效果.

    <table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


    <colgroup>
    <col  class="org-left" />

    <col  class="org-left" />
    </colgroup>
    <thead>
    <tr>
    <th scope="col" class="org-left">VOLUME</th>
    <th scope="col" class="org-left">信号音量</th>
    </tr>
    </thead>
    <tbody>
    <tr>
    <td class="org-left">CUTOFF</td>
    <td class="org-left"><a href="#org1af1e38">滤波器</a> 的截断频率</td>
    </tr>
    </tbody>
    <tbody>
    <tr>
    <td class="org-left">Q</td>
    <td class="org-left">控制 <a href="#orge513ac8">回响</a>.</td>
    </tr>
    </tbody>
    <tbody>
    <tr>
    <td class="org-left">VSHIFT</td>
    <td class="org-left">竖直地偏移信号.</td>
    </tr>
    </tbody>
    <tbody>
    <tr>
    <td class="org-left">LIMIT</td>
    <td class="org-left">限制在 <code>DIST</code> 模式的的阈值.</td>
    </tr>

    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left"><code>0-F</code> 减少音量,</td>
    </tr>

    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left"><code>10-FF</code> 允许在偏移音量,</td>
    </tr>

    <tr>
    <td class="org-left">&#xa0;</td>
    <td class="org-left">对较大音量的信号可以添加一些有趣的音调</td>
    </tr>
    </tbody>
    <tbody>
    <tr>
    <td class="org-left">PHASE</td>
    <td class="org-left">水平压缩信号.</td>
    </tr>
    </tbody>
    </table>


<a id="org898f0e7"></a>

### Wave Screen

在 Wave Screen 中可以预览和编辑合成器的单个波形.
16 个合成器声音分别有 16 个波形. 这意味着第一个合成器 `0`
可以使用 `0-F` 的波形, 第二个合成器 `1` 可以使用 `10-1F` 的波形,
以此类推.

Wave Screen 的按键可以在 Help Screen 中查看.

**例**: Phase Examples

-   默认的波形

    ![img](/_img/lsdj/original-wave.png "Phrase example. Orginial Wave")
-   PINCH Pharsing

    ![img](/_img/lsdj/pinch-phasing.png "Pinch phasing")
-   WARP Phasing

    ![img](/_img/lsdj/warp-phasing.png "WARP phasing")
-   RESYNC Phasing

    ![img](/_img/lsdj/resync-phasing.png "RESYNC phasing")

**例**: Vshift Examples

-   默认的波形

    ![img](/_img/lsdj/original-wave.png "Vshift example. Orginal Wave")
-   Vshift signal (`VSHIFT` = `40`, `CLIP` = `WRAP`)

    ![img](/_img/lsdj/vshift-40.png "Vshifted signal (Vshift = 40, Clip = WRAP)")
-   Vshift signal (`VSHIFT` = `80`, `CLIP` = `WRAP`)

    ![img](/_img/lsdj/vshift-80.png "Vshifted signal (Vshift = 80, Clip = WRAP)")


<a id="orgd9c3f54"></a>

### Project Screen

![img](/_img/lsdj/project-screen.png "Project Screen")

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">TEMPO</th>
<th scope="col" class="org-left">以 BPM 为单位设置歌曲的节奏</th>
</tr>
</thead>
<tbody>
<tr>
<td class="org-left">TRANSPOSE</td>
<td class="org-left">以给定的 <a href="#org67fac3c">半音</a> 数量调整 <code>PULSE</code> 和 <code>WAVE</code> 类型的乐器</td>
</tr>
</tbody>
<tbody>
<tr>
<td class="org-left">SYNC</td>
<td class="org-left">和其他的设备连接</td>
</tr>
</tbody>
<tbody>
<tr>
<td class="org-left">CLONE</td>
<td class="org-left">设置 <code>DEEP</code> 或 <code>SLIM</code> 的克隆方式:</td>
</tr>

<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left"><code>DEEP</code> 将克隆 Chain 的所有 Phrases,</td>
</tr>

<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left"><code>SLIM</code> 将重复使用旧的 Phrases.</td>
</tr>

<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">详见 <a href="#org66586be">2.3.2</a>.</td>
</tr>
</tbody>
<tbody>
<tr>
<td class="org-left">LOOK</td>
<td class="org-left">改变字体和颜色设置</td>
</tr>
</tbody>
<tbody>
<tr>
<td class="org-left">KEY DELAY/REPEAT</td>
<td class="org-left">设置 <a href="#orgca69ab8">Game Boy</a> 按键的延迟</td>
</tr>
</tbody>
<tbody>
<tr>
<td class="org-left">PRELISTEN</td>
<td class="org-left">设置是否在进入 <a href="#org8c7e5ac">音符</a> 或乐器的时候播放声音</td>
</tr>
</tbody>
<tbody>
<tr>
<td class="org-left">HELP</td>
<td class="org-left">进入 <a href="#org408b834">Help Screen</a>.</td>
</tr>

<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left"><a href="#org408b834">Help Screen</a> 中包含按键和命令的使用参考</td>
</tr>
</tbody>
<tbody>
<tr>
<td class="org-left">CLEAN SONG DATA</td>
<td class="org-left">去掉重复的 Chain 和 Phrases 并清除未使用的数据</td>
</tr>
</tbody>
<tbody>
<tr>
<td class="org-left">CLEAN INSTR DATA</td>
<td class="org-left">去掉重复的 Table 和未使用的乐器, Tables, 合成器以及波形</td>
</tr>
</tbody>
<tbody>
<tr>
<td class="org-left">LOAD/SAVE SONG</td>
<td class="org-left">进入 <a href="#org154bb4a">File Screen</a></td>
</tr>
</tbody>
</table>

Project Screen 也包含两个时钟:

-   `WORKED` 显示制作当前歌曲所使用的时间;

    当播放的时候会被替换为 `PLAY` 时钟, 显示歌曲被播放的时间长度.
-   `TOTAL` 显示当前卡带被使用的时间

**Tip**:

-   可以使用 lsdpatcher 替换字体和配色.

1.  Total Memory Reset

    在 `LOAD/SAVE FILE` 上按下 `SELECT+A+B` 将清空所有的数据.
    通常在卡带出现问题的时候有用.


<a id="org154bb4a"></a>

### File Screen

![img](/_img/lsdj/file-screen.png "File Screen")

在 Project Screen 中的 `LOAD/SAVE SONG` 可以进入 File Screen.
在 File Screen 中可以将歌曲保存在 [内存](#org2958711) <sup><a id="fnr.memory-or-disk" class="footref" href="#fn.memory-or-disk" role="doc-backlink">4</a></sup> 中.
也可以将 [内存](#org2958711) 中的歌曲读取到当前内存 (运行内存) 中.
可以保存最多 32 个歌曲到卡带中.

File Screen 只能用于有 64kb [SRAM](#org2958711) 的卡带中.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">FILE</th>
<th scope="col" class="org-left">显示当前编辑的文件名称.</th>
</tr>

<tr>
<th scope="col" class="org-left">&#xa0;</th>
<th scope="col" class="org-left">当显示 <code>!</code> 时表示未保存.</th>
</tr>
</thead>
<tbody>
<tr>
<td class="org-left">LOAD</td>
<td class="org-left">载入歌曲.</td>
</tr>

<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">按下 <code>A</code> 后选择文件并再按下 <code>A</code> 可以载入歌曲.</td>
</tr>
</tbody>
<tbody>
<tr>
<td class="org-left">SAVE</td>
<td class="org-left">保存歌曲.</td>
</tr>

<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">按下 <code>A</code> 后选择槽位并再次按下 <code>A</code> 可以写入歌曲.</td>
</tr>
</tbody>
<tbody>
<tr>
<td class="org-left">ERASE</td>
<td class="org-left">清除歌曲.</td>
</tr>

<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">按下 <code>A</code> 后选择文件并再次按下 <code>A</code> 可以清除歌曲.</td>
</tr>
</tbody>
<tbody>
<tr>
<td class="org-left">BLOCK</td>
<td class="org-left">显示使用了多少储存空间.</td>
</tr>

<tr>
<td class="org-left">USED</td>
<td class="org-left">一个块 (block) 为 512 比特.</td>
</tr>

<tr>
<td class="org-left">&#xa0;</td>
<td class="org-left">在底部的数值为十六进制表示, 即 <code>BF * 512 = 97792</code> 有效比特</td>
</tr>
</tbody>
</table>

按下 `B` 可以回到 [Project Screen](#orgd9c3f54).

**Tip**:

-   可以使用 lsdpatcher 来管理歌曲.

1.  Song List

    歌曲列表显示歌曲的名称, 版本编号以及文件大小. 当保存的时候,
    歌曲文件将会进行压缩, 所以不同的歌曲可能会有不同的大小.
    若想要开始创建新的歌曲, 可以载入 `EMPTY` 槽位.

    **Tip**

    -   在歌曲列表中按下 `SELECT+A` 中可以载入歌曲而不需要切换到 [Song Screen](#org4a8cc79),
        然后就可以按下 `START` 来播放或停止歌曲.
        这样就可以快速预览歌曲而不需要反复切换到不同的 Screen 中.
        这对于 Live 的表演比较有用.


<a id="org8afd5fc"></a>

### Border Information

边栏上会显示一些有用的信息:

1.  Screen 的标题: 显示当前正在编辑的内容.
2.  显示当前正在编辑的通道, 及选中的 Song Screen 的列
3.  当前正在编辑的 Chain 的位置
4.  当前歌曲的 [节奏](#org963c929) (BPM)
5.  显示当前正在播放的通道.
    当按下 `B+SELECT` 或者 `B+START` 是会显示 `MUTE` (静音)
6.  在 *WAV* 通道正在播放的波形
7.  在 [Phrase Screen](#org3d589b9) 正选中的乐器
8.  Sync 状态
9.  Screen Map


<a id="orgd42444c"></a>

## Advanced Techniques


<a id="org0d912e0"></a>

### Copy and Paste

[LSDJ](#org734d1a9) 中有一部分的内存空间作为剪贴板.
按下 `B+A` 可以将光标下的值进行剪切, 并将其储存在剪贴板中.
按下 `SELECT+A` 可以粘贴剪贴板中的内容.

在大多数的 Screen 中, 可以用 `SELECT+B` 来选中,
然后移动光标来选中块. 当选中块时, 可以按下 `B` 键将其复制到剪贴板中,
或使用 `SELECT+A` 来剪切到剪贴板中. 同样, 可以使用 `SELECT+A` 粘贴.

一些快速的选择操作:

-   `SELECT+(B,B)` 快速标记行或列
-   `SELECT+(B,B,B)` 快速标记整个屏幕

当标记块后, 可以按下 `A+CURSOR` 来修改整个块中的数据.
这样可以快速的对一系列 [音符](#org8c7e5ac) 进行 [转调](#orgc2bd5e1).


<a id="org66586be"></a>

### Cloning

克隆可以用来减少一些不必要的复制粘贴工作.
可以直接从 [Song](#org4a8cc79), [Chain](#orgf717492), [Phrase](#org3d589b9) 或者 [Instrument Screen](#org260e8ef)
中克隆 Chain, Phrases 或 Tables.

**例**: 想要克隆 Chain `0` 的旋律.

-   在 [Song Screen](#org4a8cc79) 中在 `00` 上按下 `A` 来选中 Chain
-   移动到下一个空的 step 中再次按下 `A` 来应用对应的 Chain
-   按下 `SELECT+(B,A)` 来克隆 Chain `0`

1.  Deep vs. Slim-Cloning

    克隆模式分为 `DEEP` 和 `SLIM`, 可以在 [Project Screen](#orgd9c3f54) 中选择.

    -   当使用 `SLIM` 克隆 Chain 时, 会同时克隆和原本 Phrase 一样的内容.
    -   当使用 `DEEP` 克隆 Chain 时, 新的 Chain 包含原本的 Phrase.

    `DEEP` 克隆的好处是可以减少不小心修改旧的 Phrase.
    坏处是这样会更快地消耗可用 Phrase 内存, 并导致在保存的时候占用更多内存.

    如果你用完了可用的 Phrase, 可以使用 [Project Screen](#orgd9c3f54) 中的 `CLEAN SONG DATA`.


<a id="org8fd8794"></a>

### The Importance of Backups

一些血泪教训: 在使用 Game Boy 的卡带时, 常保存!
大多数 Game Boy 的卡带依赖其内部的电池储存数据,
但其电量可能会耗尽以导致你丢失数据或精度.
请时不时做保存和备份或者至少将你的歌曲录制下来以免丢失.


<a id="org15726bd"></a>

### Muting, Soloing and Panning

-   在任意 Screen 下按下 `B+SELECT` 可以静音当前通道

    若 `B` 在 `SELECT` 键松开前被松开, 该通道在按下 `B` 键前将一直保持静音.
-   在任意 Screen 下按下 `B+START` 可以将该通道声音独立出来
    (除了该通道其他通道都静音)

    若 `B` 在 `START` 键松开前被松开, 其他通道在按下 `B` 键前将一直保持静音.
    若 `START` 键先松开, 则其他所有通道都会被取消静音.
-   在 [Song Screen](#org4a8cc79) 按下 `B+LEFT/RIGHT` 可以改变左右声道的 pan


<a id="orgefa579f"></a>

### Live Mode

Live 模式可以用于在 Live 演奏时自由地混合和匹配 Chains:

此时的 [Song Screen](#org4a8cc79) 包含:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left"><code>SELECT+LEFT</code></th>
<th scope="col" class="org-left">在 Song 和 Live 模式之间切换</th>
</tr>
</thead>
<tbody>
<tr>
<td class="org-left"><code>START</code></td>
<td class="org-left">开始选中的 Chain</td>
</tr>
</tbody>
<tbody>
<tr>
<td class="org-left"><code>SELECT+START</code></td>
<td class="org-left">停止选中的 Chain</td>
</tr>
</tbody>
<tbody>
<tr>
<td class="org-left"><code>LEFT+START</code></td>
<td class="org-left">开始在该行中的所有的 Chain</td>
</tr>
</tbody>
</table>

若 Chain 已经开始播放, 则开始和停止将会等到其完全播放完后生效.
按下 `START` 两次将会加速该过程.


<a id="org55f0191"></a>

### Synthetic Drum Instruments

不适用鼓机采样来创建鼓类乐器的好处是可以让通道的分配更加灵活.
以下是一些简单的例子:

(注: 这里不翻译了, 因为不会)

1.  Pulse Bass Drum

2.  Snare Drum

3.  Hi-Hats and Cymbals

4.  Wave Bass Drum


<a id="org7207dc1"></a>

## Commands

在 Phrase 和 Table 中使用命令可以做任何事情.
一个建议是先快速略过本节以大致了解命令可以做些什么.

**Tip**:

-   在命令上按 `A,A` 可以在屏幕上方显示一个滚动的帮助文本.
    `A+L/R` 可以用于在这些命令之间游览. 按下 `SELECT` 可以停止文本的滚动.


<a id="org9164593"></a>

### A: Table Start/Stop

开始或停止当前的通道的 Table. 其参数为对应的 Table Number,
或是 `20` 表示停止 Table.

**例**:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left"><code>A03</code></td>
<td class="org-left">开始 Table <code>03</code></td>
</tr>

<tr>
<td class="org-left"><code>A20</code></td>
<td class="org-left">停止 Table</td>
</tr>
</tbody>
</table>


<a id="orge82b18f"></a>

### B: Maybe

1.  In Phrases (MayBe Play Note)

    控制其左边与右边的 [音符](#org8c7e5ac) 或者 [采样](#orga1e5098) 被触发的概率.
    第一位数设置​**左边** KIT 的概率, 第二位数设置​**右边** KIT 的概率.

    **例**:

    <table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


    <colgroup>
    <col  class="org-left" />

    <col  class="org-left" />
    </colgroup>
    <tbody>
    <tr>
    <td class="org-left"><code>B00</code></td>
    <td class="org-left">永不触发 <a href="#org8c7e5ac">音符</a></td>
    </tr>

    <tr>
    <td class="org-left"><code>B0F</code></td>
    <td class="org-left">总是触发右侧的 <a href="#org8c7e5ac">音符</a></td>
    </tr>

    <tr>
    <td class="org-left"><code>BF0</code></td>
    <td class="org-left">总是触发左侧的 <a href="#org8c7e5ac">音符</a></td>
    </tr>

    <tr>
    <td class="org-left"><code>B08</code></td>
    <td class="org-left">50% 概率触发右侧的 <a href="#org8c7e5ac">音符</a></td>
    </tr>
    </tbody>
    </table>

2.  In Tables (MayBe Hop)

    设置一个有概率的跳转 (Hop). 第一位数设置触发概率,
    第二位数设置跳转的目标行.

    **例**:

    <table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


    <colgroup>
    <col  class="org-left" />

    <col  class="org-left" />
    </colgroup>
    <tbody>
    <tr>
    <td class="org-left"><code>BF5</code></td>
    <td class="org-left">跳转到第 <code>5</code> 行, 15/16 的概率</td>
    </tr>

    <tr>
    <td class="org-left"><code>B84</code></td>
    <td class="org-left">跳转到第 <code>4</code> 行, 1/2 的概率</td>
    </tr>

    <tr>
    <td class="org-left"><code>B03</code></td>
    <td class="org-left">永远不会跳转到第 <code>3</code> 行</td>
    </tr>
    </tbody>
    </table>


<a id="org6067cdd"></a>

### C: Chord

通过 [琶音](#org5872bf7) 将输入的参数 [半音](#org67fac3c) 在基础的 [音符](#org8c7e5ac) 拓展为和弦.
[琶音](#org5872bf7) 的速度可以通过在 Instrument Screen 的 CMD/RATE 调节.

**例**:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left"><code>C37</code></td>
<td class="org-left"><a href="#org408e8eb">小三和弦</a>: 0, 3, 7, 0, 3, 7, 0, 3, 7, &#x2026;</td>
</tr>

<tr>
<td class="org-left"><code>C47</code></td>
<td class="org-left"><a href="#org5a3547c">大三和弦</a>: 0, 4, 7, 0, 4, 7, 0, 4, 7, &#x2026;</td>
</tr>

<tr>
<td class="org-left"><code>C0C</code></td>
<td class="org-left">0, 0, C, 0, 0, C, 0, 0, C, &#x2026;</td>
</tr>

<tr>
<td class="org-left"><code>CC0</code></td>
<td class="org-left">0, C, 0, C, &#x2026;</td>
</tr>

<tr>
<td class="org-left"><code>CCC</code></td>
<td class="org-left">0, C, C, 0, C, C, 0, C, C, &#x2026;</td>
</tr>

<tr>
<td class="org-left"><code>C00</code></td>
<td class="org-left">清除和弦设置</td>
</tr>
</tbody>
</table>


<a id="orga017e9a"></a>

### D: Delay

延迟根据给定的参数延迟触发.


<a id="org2a63d32"></a>

### E: Amplitude Envelope

1.  For Pulse and Noise Instruments

    第一位数字设置初始的增幅 (`0` 表示最小, `F` 表示最大);
    第二位数字设置 [释放时间](#org148325b) (`0,8`: 表示没有, `1-7`: 减少, `9-F`: 增加).

2.  For Wave Instruments

    **例**:

    <table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


    <colgroup>
    <col  class="org-left" />

    <col  class="org-left" />
    </colgroup>
    <tbody>
    <tr>
    <td class="org-left"><code>E00</code></td>
    <td class="org-left">音量 0%</td>
    </tr>

    <tr>
    <td class="org-left"><code>E01</code></td>
    <td class="org-left">音量 25%</td>
    </tr>

    <tr>
    <td class="org-left"><code>E02</code></td>
    <td class="org-left">音量 50%</td>
    </tr>

    <tr>
    <td class="org-left"><code>E03</code></td>
    <td class="org-left">音量 100%</td>
    </tr>
    </tbody>
    </table>


<a id="org58fd7a7"></a>

### F: Wave Frame/Finetune

1.  For Pulse Instruments

    第一位数设置 *PU2* TSP, 第二位数按 [1/32 音](#org67fac3c) 每 step `FINETUNE`.
    详见 [2.2.6.2](#org57b4b03).

2.  For Kit Instruments

    修改 [采样](#orga1e5098) 的播放位置. `00-7F` step forward, `80-FF` steps back.

3.  For Wave Instruments

    改变 *WAV* 通道所播放的波形帧 (wave frame),
    其参数为相对当前波形帧 (frame number) 的位移.
    可以用于手动播放合成器声音 (synth sound).

    **Tip**:

    -   因为一个合成器声音 (synth sound) 包含 16 (10) 个波形,
        所以 `F10` 会跳转到下一个合成器声音里

    **例**:

    <table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


    <colgroup>
    <col  class="org-left" />

    <col  class="org-left" />
    </colgroup>
    <tbody>
    <tr>
    <td class="org-left"><code>F01</code></td>
    <td class="org-left">假如当前已经播放了 frame 3, 那么前进一帧, 播放 frame 4</td>
    </tr>
    </tbody>
    </table>


<a id="org372b258"></a>

### G: Groove Select

使用要播放的 Phrases 或 Tables 的 Groove

**例**:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left"><code>G04</code></td>
<td class="org-left">选择 groove 4</td>
</tr>
</tbody>
</table>


<a id="orgcfa20a8"></a>

### H: Hop

跳转到新的播放位置. 也同时可以被用于停止播放.

1.  H in Phrases

    **例**:

    <table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


    <colgroup>
    <col  class="org-left" />

    <col  class="org-left" />
    </colgroup>
    <tbody>
    <tr>
    <td class="org-left"><code>H00-F0F</code></td>
    <td class="org-left">跳到下一个 Phrase. 其参数设置了目标的 Phrase 对应的 step</td>
    </tr>

    <tr>
    <td class="org-left"><code>H10-HFE</code></td>
    <td class="org-left">在当前 Phrase 跳转, 第一位设置次数, 第二位设置目标 step</td>
    </tr>

    <tr>
    <td class="org-left"><code>HFF</code></td>
    <td class="org-left">停止播放</td>
    </tr>
    </tbody>
    </table>

    **Tip**

    -   To compose in waltz time (3/4), put `H00` commands on step C in
        every phrase

2.  H in Tables

    在 Table Screen 中, `H` 用于实现循环. 第一位设置循环的次数或
    `0` 表示无数次. 第二位数设置跳转的 step. 循环跳转可以是嵌套的,
    可以在大循环里面设置小循环.

    **例**:

    <table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


    <colgroup>
    <col  class="org-left" />

    <col  class="org-left" />
    </colgroup>
    <tbody>
    <tr>
    <td class="org-left"><code>H21</code></td>
    <td class="org-left">跳到 Table 的第 1 step 2 次</td>
    </tr>

    <tr>
    <td class="org-left"><code>H04</code></td>
    <td class="org-left">无限次掉到 Table 的第 4 step</td>
    </tr>
    </tbody>
    </table>


<a id="orgfb4210b"></a>

### K: Kill Note

`K` 停止当前的声音 (会产生一个 click 声). 假如不希望有 click 声,
可以使用 `E00` (*WAV*) 或者 `E11` (*PU\**, *NOI*).

**例**:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left"><code>K00</code></td>
<td class="org-left">立刻清除当前的 <a href="#org8c7e5ac">音符</a></td>
</tr>

<tr>
<td class="org-left"><code>K03</code></td>
<td class="org-left">在 3 tick 后清除 <a href="#org8c7e5ac">音符</a></td>
</tr>
</tbody>
</table>


<a id="org9ee3827"></a>

### L: Slide

在给定时间里移至对应的 [音符](#org8c7e5ac) 实现 [滑音](#org886994a). 如果乐器的 PITCH 为 TICK,
则给定的时间的单位为 tick, 否则则为 n/360 秒.

**例**: 从 `C-4` 到 `F-4` 并快速回到 `C-4`.

    C-4 ---
    F-4 L40
    --- ---
    C-4 L10

1.  L in Tables

    ![img](/_img/lsdj/table-slide.png "Table Slide")

    `L` 命令可以在左边的 CMD 列中使用.
    TSP (Transpose) 列将会被设为相对 base note 的 target note.

    (&#x2026;)


<a id="org4553129"></a>

### M: Master Volume

改变主音量 (master output volume). 第一位为左声道输出,
第二位为右声道输出. 可以以相对值或绝对值的方式进行设置.

对于 `0-7` 的值为绝对值, `8-F` 为相对值 (`8` 不改变, `9-B` 增加, `D-F` 减少).

**例**:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left"><code>M77</code></td>
<td class="org-left">最大音量</td>
</tr>

<tr>
<td class="org-left"><code>M08</code></td>
<td class="org-left">左声道音量最小, 右声道音量不变</td>
</tr>

<tr>
<td class="org-left"><code>M99</code></td>
<td class="org-left">增加 1 音量</td>
</tr>

<tr>
<td class="org-left"><code>MFE</code></td>
<td class="org-left">减少左声道 1 音量, 增加右声道 2 音量</td>
</tr>
</tbody>
</table>


<a id="orgd029d21"></a>

### O: Set Output

设置通道的输出为左声道, 右声道, 无或者双声道 (pan).


<a id="orgda21ac6"></a>

### P: Pitch Bend

1.  For Pulse, Wave and Kit Instruments

    以给定速度进行频移. 其行为受乐器的 `PITCH` 影响:

    <table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


    <colgroup>
    <col  class="org-left" />

    <col  class="org-left" />
    </colgroup>
    <tbody>
    <tr>
    <td class="org-left">DRUM</td>
    <td class="org-left">对数频移 (更新率 360Hz)</td>
    </tr>

    <tr>
    <td class="org-left">FAST</td>
    <td class="org-left">线性频移 (更新率 360Hz)</td>
    </tr>

    <tr>
    <td class="org-left">TICK</td>
    <td class="org-left">每 tick 更新</td>
    </tr>

    <tr>
    <td class="org-left">STEP</td>
    <td class="org-left">立刻更新频率</td>
    </tr>
    </tbody>
    </table>

    **例**:

    <table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


    <colgroup>
    <col  class="org-left" />

    <col  class="org-left" />
    </colgroup>
    <tbody>
    <tr>
    <td class="org-left"><code>P02</code></td>
    <td class="org-left">以 <code>2</code> 的速率增加音调</td>
    </tr>

    <tr>
    <td class="org-left"><code>PFE</code></td>
    <td class="org-left">以 <code>2</code> (<code>FE</code> 为 <code>-2</code>) 的速率减少音调</td>
    </tr>
    </tbody>
    </table>

2.  For Noise Instruments

    在每个 tick 处应用 [`S`](#org290de49) 命令.


<a id="org264271a"></a>

### R: Retrig/Resync

重新触发之前播放过的 [音符](#org8c7e5ac). 第一位数修改脉冲或噪声的音量
(`0` 不变, `1-7` 增加, `9-F` 减少), 第二位设置重新触发的频率 (rate),
(`1` 为最快, `F` 为最慢, `0` 表示只重新触发一次).


<a id="org290de49"></a>

### S: Sweep/Shape

`S` 的效果根据乐器类型的不同会有不同的效果.

1.  Pulse Instruments

    效果为扫频. 在 bass drum 和打击乐比较有用.
    第一位设置了时间, 第二位设置了 [音调](#orgdb2f8b5) 的增加和减少.
    这只在 *PU1* 有效.

2.  Kit Instruments

    `S` 改变循环点. 第一位修改循环偏移值, 第二位修改循环长度.
    (`1-7` 增加, `9-F` 减少) 如果使用得当, 可以实现有效的打击乐的效果.

3.  Noise Instruments

    改变噪声的形状 (见 [2.2.6.5](#org5ae1b8e)).
    其参数值会增加当前生效的 *NOI* 形状的值. (相对偏移)


<a id="org4542f7d"></a>

### T: Tempo

改变 tick 的速率以匹配给定的 [BPM](#orga5817b1). 不过需要注意的是,
只有当 Groove 为 6 tick/step 时 [BPM](#orga5817b1) 是直接设置的.
否则 [2](#orga5817b1) 将需要通过以下的公式进行计算:

$$\mathrm{BPM}_{\mathrm{LSDJ}} = \mathrm{BPM}_{\mathrm{desired}} \times \frac{\mathrm{tick}}{\mathrm{step}} / 6$$

`T28-TFF` 设置 40-255 BPM, `T00-T27` 设置了 256-295 BPM.

**例**:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left"><code>T80</code></td>
<td class="org-left">128 BPM</td>
</tr>

<tr>
<td class="org-left"><code>TFF</code></td>
<td class="org-left">255 BPM</td>
</tr>

<tr>
<td class="org-left"><code>T27</code></td>
<td class="org-left">295 BPM</td>
</tr>
</tbody>
</table>


<a id="org388289f"></a>

### V: Vibrato

添加 [颤音](#org9c2edc4). 第一位设置速度, 第二位设置深度.

[颤音](#org9c2edc4) 的速度和形状受乐器的 PITCH 影响. 当 PITCH 设为 TICK 时,
[颤音](#org9c2edc4) 会被设置为与音乐同步 (假设有一个 6ticks/step 的节奏).

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Depth</th>
<th scope="col" class="org-right">0</th>
<th scope="col" class="org-right">1</th>
<th scope="col" class="org-right">2</th>
<th scope="col" class="org-right">3</th>
<th scope="col" class="org-right">4</th>
<th scope="col" class="org-right">5</th>
<th scope="col" class="org-right">6</th>
<th scope="col" class="org-right">7</th>
</tr>
</thead>
<tbody>
<tr>
<td class="org-left">Semitones</td>
<td class="org-right">0.125</td>
<td class="org-right">0.25</td>
<td class="org-right">0.375</td>
<td class="org-right">0.5</td>
<td class="org-right">0.75</td>
<td class="org-right">1</td>
<td class="org-right">1.5</td>
<td class="org-right">2</td>
</tr>
</tbody>
</table>

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />

<col  class="org-right" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Depth</th>
<th scope="col" class="org-right">8</th>
<th scope="col" class="org-right">9</th>
<th scope="col" class="org-right">A</th>
<th scope="col" class="org-right">B</th>
<th scope="col" class="org-right">C</th>
<th scope="col" class="org-right">D</th>
<th scope="col" class="org-right">E</th>
<th scope="col" class="org-right">F</th>
</tr>
</thead>
<tbody>
<tr>
<td class="org-left">Semitones</td>
<td class="org-right">2.5</td>
<td class="org-right">3</td>
<td class="org-right">3.5</td>
<td class="org-right">4</td>
<td class="org-right">5</td>
<td class="org-right">6</td>
<td class="org-right">7</td>
<td class="org-right">8</td>
</tr>
</tbody>
</table>

**例**:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left"><code>V42</code></td>
<td class="org-left">速度为 4, depth 为 0.375 的半音</td>
</tr>

<tr>
<td class="org-left"><code>V00</code></td>
<td class="org-left">重设颤音</td>
</tr>
</tbody>
</table>


<a id="org614bc62"></a>

### W: Wave

1.  For Pulse Instruments

    改变波形. 受限于硬件, 乐器 LENGTH 的计时器将会清零,
    可能会增加声音的持续时间.

2.  For Wave Instruments

    第一位设置了声音的速度, 第二位设置了合成器的声音长度.
    `0` 表示不改变. 若 LENGTH 被修改时, 将会重新开始 Synth.


<a id="orgc6cb9bc"></a>

### Z: RandomiZe

重复前一个非 `Z` 或 `H` 的命令, 在原本的参数上加上一个随机值.
其中 `Z` 的参数控制了增加的随机值的最大大小.

**例**:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<tbody>
<tr>
<td class="org-left"><code>Z02</code></td>
<td class="org-left">0, 1, 2</td>
</tr>

<tr>
<td class="org-left"><code>Z20</code></td>
<td class="org-left">0, 10, 20</td>
</tr>

<tr>
<td class="org-left"><code>Z22</code></td>
<td class="org-left">0, 1, 2, 10, 11, 12, 20, 21, 22</td>
</tr>
</tbody>
</table>


<a id="org04ddea8"></a>

## Synchronization

(用的是模拟器, 所以不管这部分)


<a id="org49ff945"></a>

## Appendix A: Sample Kits

(不知道, 略)


<a id="org1281a8a"></a>

## Appendix B: Allophones

建议去读原文档.


<a id="orgff09472"></a>

## Appendix C: SRAM Memory Map

在 SRAM 中如何存放歌曲数据.

(略)


<a id="orge0dcbe0"></a>

# Dictionary

注: 这里的解释完全是以瞎解释为主. 我就喜欢瞎解释, 嘻嘻.

-   <a id="orgcd6a4d5"></a>              DAW          (**D**​igital **A**​udio **W**​orkstation)

    数字音频工作站 (直译是这样的).
-   <a id="orga5817b1"></a>              BPM          (**B**​eats **P**​er **M**​inute)
-   <a id="orgca69ab8"></a>          Game Boy APU (**A**​udio **P**​rocessing **U**​nits)

    [Game Boy](#orgca69ab8) 的音频处理单元 ([Audio Overview (Pan Doc)](https://gbdev.io/pandocs/Audio.html)).
-   <a id="org734d1a9"></a>             LSDJ       (**L**​ittle **S**​ound **Dj**)

    [Game Boy](#orgca69ab8) 上的 [音序器](#orgfd705a1) ([LSDj Site](https://www.littlesounddj.com/lsd/index.php)).
-   <a id="orgd3adc68"></a>              ROM        (**R**​ead **O**​nly **M**​emory)

    [Game Boy](#orgca69ab8) 的卡带, 或是下载的游戏文件.
    是储存 [GB](#orgca69ab8) 游戏的程序的部分.
-   <a id="org2958711"></a>             SRAM       (**S**​tatic **R**​random **A**​ccess **M**​emory)

    在 [Game Boy](#orgca69ab8) 的一些卡带上一般会有的另外的芯片, 实现了存档之类的功能.
    受限于当时的技术, 这个芯片的一个特点就是掉电了就会丢数据,
    所以通常会在边上带一个小电池来保持 (也是常见的丢数据的元凶了).
-   <a id="org42b54cd"></a>    声音

    声音是一种振动. 描述一个最简单的振动即描述其振动的 [频率](#orgdb2f8b5) (音调),
    以及 [振幅](#org98ff6cf) (音量). 当然, 有许多不同的振动模式就是了:
-   <a id="org98ff6cf"></a>        振幅       (amplitude)

    虽然说振幅为 "音量" 可能有些不太准确, 但是差不多有些关联:
    振幅更大, 音量更大.
-   <a id="org199ce8b"></a>           方波       (square wave)

    方方的波.
-   <a id="orgf2b489c"></a>         三角波     (triangle)

    上升, 掉到低电平, 然后再慢慢上升, 掉到低电平.
-   <a id="orgfe01183"></a>              锯齿波     (saw tooth)

    有点类似 [三角波](#orgfe01183), 但是不是立刻掉到低电平,
    而是上升, 下降再上升, 再下降.
-   <a id="org8c7e5ac"></a>             音符       (note)

    给一段声音命名当作一个单位声音片段来使用.
-   <a id="orgdb2f8b5"></a>            音调       (pitch)

    主要描述了声音的频率. 但是和任意频率声音不同,
-   <a id="orgc2bd5e1"></a>        变调       (transpose)

    改​**变** [**音**​调](#orgdb2f8b5).
-   <a id="org1af1e38"></a>           滤波器     (filter)

    按照波的特定类型和特征对其进行一定的筛选.
    假如我们把波看作是不同频率的波的叠加 (傅里叶变换),
    那么常见的 [高通](#org889a962) (high-pass), [低通](#org1c34574) (low-pass), [带通](#org3902f02) (band-pass)
    滤波器的功能就是根据频率的高低控制对应的波是否通过.
    高通就是只让频率高的成分通过, 低通同理,
    带通则是在一定频率区域内的成分通过. <sup><a id="fnr.real-filter" class="footref" href="#fn.real-filter" role="doc-backlink">5</a></sup>
-   <a id="org1c34574"></a>         低通       (low pass)
-   <a id="org889a962"></a>        高通       (high pass)
-   <a id="org3902f02"></a>        带通       (band pass)
-   <a id="orgfffe43c"></a>         全通       (all pass)
-   <a id="org6e908d5"></a>             淡出       (fade)

    PPT 经典特效淡出. 你可以想象 [音量](#org98ff6cf) 逐渐减小.
-   <a id="orgd84b25d"></a>           旋律       (rhythm)

    嗯, 一段听起来好听的 [声音](#org42b54cd) 的排列.
-   <a id="orgf48c317"></a>         三连音符   (triplets)
-   <a id="org963c929"></a>            节奏       (tempo)

    按照一定时间间隔变换 [声音](#org42b54cd). 比如
-   <a id="org2191143"></a>           八度       (octave)

    把 [声音](#org42b54cd) 按照 [频率](#orgdb2f8b5) 进行一个分割.
    一般来说, 把声音按照频率分成几个大块,
    然后在大块中按照 `C`, `D`, `E`, `F`, `G`, `A`, `B`
    (对应简谱的 `1`, `2`, `3`, `4`, `5`, `6`, `7`) 的方式进行分割与命名.

    即:

        C2 D2 E2 F2 G2 A2 B2 C3 D3 E3 F3 G3 A3 B3

    (频率按照从左到右增加)
-   <a id="orgfd705a1"></a>        音序器     (sequencer)

    按照一定频率 (时间间隔) 产生音符信号以控制其他乐器 (合成器)
    发出声音的东西. 在这里 [Song](#org4a8cc79), [Chain](#orgf717492), [Phrase Screen](#org3d589b9) 控制了
    [LSDJ](#org734d1a9) 会发出什么信号以及什么时候发出信号.
-   <a id="org5180cd5"></a>         鼓机       (drum kit)

    会发出鼓的声音的一种 [采样器](#orga1e5098).
-   <a id="orgff49735"></a>         包络       (envelope)

    一种控制声音振幅的东西.

    **例**: 假如有包络形如 $A(t)$,
    那么将其施加到一个正弦波上即 $A(t) \sin \omega t$.
-   <a id="org81a5b7a"></a>       失真       (distortion)

    指会将信号原本的波形进行一个不真实的变换.

    虽然听起来好像很不好, 但是有时候会有些有趣的效果.
-   <a id="org8e943f3"></a>        折叠       (fold)

    一种非线性的音效: 对于超过限制的振幅, 会 "反射" 回去.

    类似于:

            #       #                ____________________ LIMIT
           # #     #       fold         # #     # #
          #   #   #     ---------->    # @ #   # @ #
         #     # #                    #     # #     #
        #       #                    #       #       #

    有点像栅栏加密算法.
-   <a id="orge513ac8"></a>        回音, 回响 (resonance)

    比如说大家在浴室里面唱歌, 墙壁就会有 "回音" (混响).
    这样就会听起来更好听 (并不是你唱得好&#x2026; 伤心捏).

    但是坏处就是如果调多了就会导致声音很糊.

    当然, 如果回音的时间延迟比较久, 那么也会有其他的效果.
    (类似双声部? )
-   <a id="orge164fe3"></a>        包裹       (wrap)

    一种非线性的音效: 对于超过限制的振幅, 会 "平移" 回去.

    类似于:

            #       #                ____________________ LIMIT
           # #     #       fold         # #     # #
          #   #   #     ---------->    #   #   #   #
         #     # #                    #     # #     #
        #       #                    #   @   #   @   #

    有点类似于 MD 模拟的平移不变边界条件.
-   <a id="org4468189"></a>            扫频       (sweep)

    随时间变换频率.

    假如你见过钢琴, 并且保证屁股够硬,
    你可以拿起扫帚在钢琴键上从一头扫 (sweep) 到另一头,
    你就可以听到 [扫频](#org4468189) 的声音了. (当然, 我想后面估计还能听到屁股被打的声音)
-   <a id="orgd2428f2"></a> 软件合成器 (soft synthesizer)

    通过软件产生声音信号并对这些声音信号进行运算和混合的东西.

    对应的是 **硬件** 合成器. 是模电领域了. 有点想玩.
-   <a id="orga1e5098"></a>  采样播放   (sample playback)

    用人话说就是用录音机录下来然后再播放.

    可以通过改变录下来再播放的音调和速度等方式来调整听起来的效果,
    这样就会比较好玩.
-   <a id="org5e8b5a7"></a> 语音合成器 (speech synthesis)

    顾名思义, 是用来合成 "人声" 的合成器.
-   <a id="orga721860"></a>            形状       (shape)
-   <a id="org5872bf7"></a>         琶音       (arpeggio)

    通过快速弹一连串的音来实现听起来有 [和弦](#org737e9e1) 的效果.
-   <a id="org9c2edc4"></a>          颤音       (vibrato)

    著名音乐下午茶动画 <del>滑滑蛋</del> 轻音少女中亦有记载:
    在吉他拨动后扭动琴弦以实现声音的 "摇晃".

    用严谨一些的说法就是在声音后半部分加入一些音频参数上的改变.
    比如说 [音调](#orgdb2f8b5) 上的略微变化.
-   <a id="org67fac3c"></a>         半音       (semitone)
-   <a id="org737e9e1"></a>            和弦       (chord)

    同时按下几个音, 听起来就比较好听.
-   <a id="org408e8eb"></a>      小三和弦   (minor chord)

    按下 `0, 3, 7` 常用于渲染恐怖的氛围.
-   <a id="org5a3547c"></a>      大三和弦   (major chord)

    按下 `0, 4, 7`.
-   <a id="org886994a"></a>            滑音       (slide)

-   <a id="orgb08c327"></a>          Tremolo    (有翻译为: 颤音)

    用人话来说就是随着时间改变 [振幅](#org98ff6cf).
-   <a id="orgcadc64b"></a>             ASDR       (**A**​ttack **D**​ecay **S**​ustain **R**​elease)

    一种用来描述包络的参数的方式.
-   <a id="org03a4ee8"></a>      Attack     (**A**​DSR Attack)

    上升 (跑到最高点)
-   <a id="org278e0c0"></a>       Decay      (A​**D**​SR Decay)

    衰减 (掉下去一点点)
-   <a id="orgfa18c83"></a>     Sustain    (AD​**S**​R Sustain)

    保持 (会影响声音可以持续多久)
-   <a id="org148325b"></a>     Release    (ADS​**R** Release)

    释放 (掉到零)


# Footnotes

<sup><a id="fn.1" href="#fnr.1">1</a></sup> 这里省略了部分的例子. 主要是一些重复的 Chain,
Phrase 的选择说明.

<sup><a id="fn.2" href="#fnr.2">2</a></sup> 这里省略了部分的十六进制的说明.
假如你不了解什么是十六进制的话, 请参考 [Hexadecimal (Wikipedia)](https://en.wikipedia.org/wiki/Hexadecimal).

<sup><a id="fn.3" href="#fnr.3">3</a></sup> 另外还有 3 个不在 map 中显示的隐藏 Screens:
File, Word 以及 Help Screens.

<sup><a id="fn.4" href="#fnr.4">4</a></sup> 内存 (memory)? 还是硬盘 (disk)?
请不要追究这里的用词是否严谨.

<sup><a id="fn.5" href="#fnr.5">5</a></sup> 实际上并没有这种好事.
理论上听起来我们只需做一个简单的傅里叶变换: $s(t) \rightarrow A(\omega)$,
然后筛选函数 $f(\omega) = 0\ \mathrm{if}\ \omega < \omega_0\ \mathrm{else}\ 1$ (以高通为例),
最后做一个逆变换: $s(t) \xrightarrow{\mathcal{F}} A(\omega) \rightarrow f(\omega) A(\omega) = A'(\omega) \xrightarrow{\mathcal{F}^{-1}} s'(t)$ 即可.
实际上, 比如用 RC 滤波, 会有一堆的问题,
也很难实现尖锐的频率筛选的效果. 不过不管怎么说, 差不多理解就好,
这里的字典不追求严谨性, 图一乐吧&#x2026;
