# 当前目录
SHELL := /bin/bash
SHELL_DIR := $(shell /bin/pwd)

################################################################################
# simple_make 修改区域
# simple_make脚本路径
MMAKE_FILE := \"./script/easy_make.erl\"
# 编译脚本文件的选项
MAKE_OPTS := {inline_size, 30},report,debug_info,warnings_as_errors,verbose,{outdir,\"ebin\"}
################################################################################

# erlang 命令
ERL := erl
EBIN_DIRS := ebin
h: help
help:
	@echo "用法:"
	@echo "make all		        - 编译项目"
	@echo "make erl		        - 编译所有erlang代码"

all: init_make make_erl

init_make:
	@(mkdir -p ebin)

# 使用erlang的make模块读取emakefile进行编译
make_erl:
	@$(ERL) -pa $(EBIN_DIRS) -noinput -eval "case make:files([$(MMAKE_FILE)], [$(MAKE_OPTS)]) of error -> halt(1); _ -> halt(0) end."
	@erl -pa $(EBIN_DIRS) -noshell -eval 'case mmake:all() of error -> halt(1); _ -> halt(0) end.'