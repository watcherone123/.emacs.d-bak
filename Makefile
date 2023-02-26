# 初始化下载，更新到 .gitmodules 中指定的 commit
init:
	@git submodule update --init --recursive
	@git submodule foreach git reset --hard
	@git submodule foreach 'git checkout master || git checkout main'

# 修改 .gitmodules 后
sync:
	git submodule sync

update:
	git submodule foreach git pull --rebase