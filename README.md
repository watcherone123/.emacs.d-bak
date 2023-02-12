# 仓库使用
1. clone 代码
```
git clone git@github.com:watcherone123/.emacs.d.git ~/.emacs.d
```

2. 更新 submodule
```
cd ~/.emacs.d
git submodule update --init --recursive
git submodule foreach git reset --hard
git submodule foreach git checkout master
```
# linux 安装
1. 安装字体
```
wqy-microhei
```
2. 安装 eaf
```
https://github.com/emacs-eaf/emacs-application-framework
```

# 更新扩展
```
git submodule foreach git pull --rebase
```
# 删除 submodule
```
# Remove the submodule entry from .git/config
git submodule deinit -f path/to/submodule

# Remove the submodule directory from the superproject's .git/modules directory
rm -rf .git/modules/path/to/submodule

# Remove the entry in .gitmodules and remove the submodule directory located at path/to/submodule
git rm -f path/to/submodule
```
https://gist.github.com/myusuf3/7f645819ded92bda6677
