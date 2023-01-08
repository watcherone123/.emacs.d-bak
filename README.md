# 仓库使用
1. clone代码
```
git clone git@github.com:watcherone123/.emacs.d.git ~/.emacs.d
```

2. 更新submodule
```
cd ~/.emacs.d
git submodule update --init --recursive
```
# linux安装
1. 安装字体
```
wqy-microhei
```
2. 安装eaf
```
https://github.com/emacs-eaf/emacs-application-framework
```

# 更新扩展
```
git submodule foreach git pull --rebase
```