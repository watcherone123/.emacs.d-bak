# 仓库使用
1. clone代码
```
git clone git@github.com:watcherone123/.emacs.d.git
```

2. 更新submodule
```
git submodule update --init --recursive

git submodule foreach git reset --hard

git submodule foreach git checkout master
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