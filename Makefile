# 当前目录
SHELL := /bin/bash
SHELL_DIR := $(shell /bin/pwd)

################################################################################
# simple_make 修改区域
# simple_make脚本路径
MMAKE_FILE := \"./script/easy_make.erl\"
# 编译脚本文件的选项
MAKE_OPTS := {inline_size, 30},report,debug_info,warnings_as_errors,verbose,{outdir,\"ebin\"}
EASY_MAKE := easy_make
################################################################################

# erlang 命令
ERL := erl
EBIN_DIRS := ebin
h: help
help:
	@echo "用法:"
	@echo "make all		        - 编译项目"
	@echo "make config          - 编译配置"
	@echo "make erl		        - 编译所有erlang代码"

# 命令
all: init_make make_lib make_proto make_config make_src deploy
config: make_config
erl: make_erl
proto: make_proto

# 执行的命令
init_make:
	@(mkdir -p ebin)
	@$(ERL) -pa $(EBIN_DIRS) -noinput -eval "case make:files([$(MMAKE_FILE)], [$(MAKE_OPTS)]) of error -> halt(1); _ -> halt(0) end."

# 生成协议
make_proto:
	@echo "make proto"
	@(escript ./script/gen_proto.es)

make_lib:
	@echo "make library"
	@erl -pa $(EBIN_DIRS) -noshell -eval 'case $(EASY_MAKE):make_lib() of error -> halt(1); _ -> halt(0) end.'

# 动态生成配置文件
make_config:
	@echo "make config files"
	@(escript ./script/gen_config.es)

# 使用erlang的make模块读取emakefile进行编译
make_src:
	@echo "make erl"
	@erl -pa $(EBIN_DIRS) -noshell -eval 'case $(EASY_MAKE):make_src() of error -> halt(1); _ -> halt(0) end.'

deploy:
	@echo "deploy"
	@echo "move apps"
	@cp ./lib/apps/*.app ./ebin/
	@echo "deploy done"