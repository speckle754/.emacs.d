# .emacs.d 
一个核心化的 Emacs 配置。  
仅限 Windows 平台。  
## 1. 怎么用（中文版施工中，缓更）  
### 1.1 前置准备
#### 1.1.1 安装字体
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
我选择的是 BlexMono Nerd Font Mono，  
其中 Nerd Font 表明加入编程领域常见的 icon 即标识作为可显示的字体。  
以及 Noto Sans Mono CJK SC。如何配置字体参见配置57行。  
推荐安装 nerd-icons 字体。  
#### 1.1.2 对于 Windows，需要额外配置 MinGW64 环境
不推荐使用 scoop 包管理安装，推荐前往 MSYS2 官网下载 installer 手动安装。  
常见安装位置为“C:/msys64”，提供了多个 Terminal 客户端模拟 Linux 环境，我们使用里面的 MinGW64 即可。  
需要将下方的位置写入 Windows 系统变量即 PATH 列表的第一位。  
或在配置文件开头加入该 elisp 代码告知 Emacs 它的位置：  
``` elisp
(setenv "PATH" (concat "C:\\msys64\\mingw64\\bin;" (getenv "PATH")))
```
#### 1.1.3 对于 Windows，需要额外配置 Emacs-kl 版本
参见 https://github.com/kiennq/emacs-build  
可以使用 scoop 或在该项目的 release 页面下载。推荐使用前者。  
（待补充步骤）    
### 1.2 安装
#### 1.2.1 前置准备  
需要学习以下 Emacs 命令：  
``` text
C-x C-f => find-file => 寻找文件并打开。  
M-x eval-buffer => 重新评估（eval）当前缓冲区（buffer）。通常对 init.el 执行该操作。Emacs 将检查包的安装，可以理解在没有正确安装的情况下再次尝试，该命令也适用于修改 init.el 后重新读取配置。  
```  仓库的 early-init.el 与 init.el 放入  
你的家目录 下的 .emacs.d 文件夹即可，  
然后打开 Emacs，会自动安装，可能需要多次重启。  
如果不知道通常也就是默认的 .emacs.d 位置，  
可以先可以先打开 Emacs，之后通过 everything 这样的搜索工具查看文件夹位置，自之后删除自动生成的文件重新放入即可。  使用 straight.el 进行包管理，    
即字面意思直接从远程软件仓库，通常是 github 进行拉取。  
可能会比较缓慢。  
Emacs 在比较新的版本会自动编译（native-compile）插件的 .el 文件到 .elc 字节码或 .eln 机器码，    
目的是加快运行速度。  
在一个典型的插件安装过程里，通常会出现编译过程的保护性报错，  
只要插只要插件可以正确运行并且不会重复告知显式错误，忽略即可。
