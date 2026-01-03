# .emacs.d 我的一个简易的基底式的 Emacs 配置
***使用请注意裁剪配置。***   
***目前仅限 MSYS2 Windows 平台。***   
## 1. 前置准备
### 1.1 对于 Windows，需要额外配置 MSYS2 环境
不推荐使用 scoop 包管理安装，推荐前往 MSYS2 官网下载 installer 手动安装。  
常见安装位置为“C:/msys64”，提供了多个 Terminal 终端客户端提供 Linux 环境模拟，我们使用里面的 UCRT 即可。  
**需要安装位置和你选择的 MSYS2 shell 写入 Windows 系统变量即 PATH 列表的第一位。**    
***或***在配置文件前部加入类似以下 elisp 代码的片段告知 Emacs 它的位置：  
``` elisp
(setenv "PATH" (concat "c:/msys64/ucrt64/bin;" (getenv "PATH")))
(setenv "PATH" (concat "c:/msys64/mingw64/bin;" (getenv "PATH")))
```
提示：按住 Windows 徽章按键，输入“环境变量”即可看到对应设置。  
     之后点击“环境变量”按钮，在上部列表的“PATH”中键入上方的仅路径信息，这里结尾不用写分号。  
     需要注意 Windows 的路径使用反斜杠“\”，elisp 可以写为“\\\”或“/”。  
``` text
c:\msys64\ucrt64\bin
c:\msys64\mingw64\bin
```
### 1.2 使用 MSYS2 的包管理器安装 Windows Emacs
``` bash
pacman -Syu
pacman -Ss emacs | grep ucrt
pacman -S mingw-w64-ucrt-x86_64-emacs
pacman -S git mingw-w64-ucrt-x86_64-ripgrep
pacman -S mingw-w64-ucrt-x86_64-aspell-en
```
## 2. 安装本仓库的配置
### 2.1 前置准备  
需要学习以下 Emacs 操作：  
``` text
C-x C-f => find-file => 寻找文件并打开。  
M-x eval-buffer => 重新评估（eval）当前缓冲区（buffer）。  
                   通常对 init.el 执行该操作。  
                   Emacs 将检查包的安装，可以理解在没有正确安装的情况下再次尝试，  
                   该命令也适用于修改 init.el 后重新让 Emacs 读取配置，但有局限性有时候需要重启。   
```
### 2.2 安装配置
``` text 
a. 将本仓库的 early-init.el 与 init.el 放入“你的家目录”下的 .emacs.d 文件夹即可。    
b. 然后打开 Emacs，会自动安装，如果拉取包过程卡住，可以尝试多次重启。
```
如果不知道通常也就是默认的 .emacs.d 位置，  
可以先可以先打开 Emacs，之后通过 everything 这样的搜索工具查看文件夹位置，  
之后删除自动生成的文件重新放入即可。  
### 2.3 提示
因为使用 straight.el 进行包管理，    
即字面意思直接从远程软件镜像源和仓库进行拉取。  
在直接从 git 仓库拉取的时候，可能会比较缓慢。  
Emacs 在比较新的版本会自动编译（native-compile）插件的 .el 文件到 .elc 字节码或 .eln 机器码，    
目的是加快运行速度。  
在一个典型的插件安装过程里，会出现编译过程的保护性报错，  
只要插件可以正确运行并且不会重复告知显式错误，忽略即可。  
如果编译错误，尝试使用 M-x straight-rebuild-package 手动进行再编译。  
### 2.4 （可选）安装你心仪的字体
因为等宽字体每个字符的宽度都是一致的，因此代码对齐效果很好。  
为了中英文显示一致中文也采用等宽字体。  
字体英文名里的 Mono 即为等宽含义。  
有一些中文字体同时含有英文字符集，最终视觉效果更统一。  
需要注意的是，需要选择字体提供的系列里的 Mono ，也就是等宽的样式。  
常见的有：  
``` text
基于 Noto Sans 出品的 Sarasa Gothic 更纱黑体。
基于 Klee One 出品的 LXGW WenKai 霞鹜文楷。
```
我选择的是 BlexMono Nerd Font Mono，详细可搜索 Nerd Font 官网查看。    
其中 Nerd Font 表明这款字体补充了该字体集，即加入编程领域常见的 icon 标识作为可显示的字体。  
以及 Noto Sans Mono CJK SC。推荐安装 nerd-icons 字体。  
