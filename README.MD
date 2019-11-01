### 根据emakefile把目录下的文件都编译到一个ebin文件夹的脚本

#### TODO
- [x] 使用指南
- [x] 规定文档结构
- [x] 针对差异文件编译
- [x] 多核编译
- [x] 根据配置文件在编译期动态生成配置代码

#### 使用与配置
使用： 
1. 下载对应的依赖，demo中用了mysql-otp poolboy mysql-otp-poolboy
2. `make all`

配置：
1. meta_dir: meta文件存放的位置
2. emakefile: emakefile的位置
3. meta_ets_file: meta文件的名字: 默认为ete_meta_config
4. app_dirs: app配置文件的位置，app文件会被一起移动到ebin目录下
5. worker_num: 参与编译的工作进程数, 默认20个

#### 文档结构
```
.
├── ebin
│   └── %% 编译后的beam文件和app resource文件
├── emakefile %% 编译规则
├── config
│   ├── erl %% 生成的配置文件 .erl
|   └── src %% 配置文件 .config
├── include
│   └── %% 项目头文件
├── lib
|   ├── %% 依赖的开源工程
│   ├── apps
│   │   └── %% 手写的app resource文件
│   ├── mysql-otp
│   │   ├── include
│   │   ├── src
│   │   │   └── %% 依赖的源文件..
│   │   └── ...
│   ├── mysql-otp-poolboy
│   │   └── src
│   │   │   └── %% 源文件
│   │   └── ...
│   └── poolboy
│       ├── src
│       │    └── %% ...
│       └── ...
├── Makefile %% 命令所在
├── README.MD
├── script
│   ├── easy_make.erl %% 脚本
|   └── easy_make.config %% 编译脚本的配置
└── src
    ├── test_app.erl
    ├── test.erl
    └── test_sup.erl
```
#### 编译差异文件
使用ets保存文件的修改时间，下一次编译先比对，后编译。easy_make.config中有对应的配置：meta_ets_file & meta_dir

#### 多核编译
思路：把所有文件存到一个ets，然后新建多个worker编译文件，每一次编译都从ets中取出一个文件
ets的update_counter保证了每一次进1，使得不同进程读取的文件都是不一样的。只要有一个进程失败，直接向父进程发送消息停止编译。

#### 根据配置文件在编译期动态生成配置代码
`config/erl`下是配置的.erl文件，只导出一个函数`find`。利用erlang的代码分析树获取对应`find`函数下的所有`key-value`。然后根据自己写的规则生成对应新的配置文件,
规则在`script/gen_config.es`下实现