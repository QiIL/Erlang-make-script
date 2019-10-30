### 根据emakefile把目录下的文件都编译到一个ebin文件夹的脚本

#### TODO
[X]规定文档结构
[X]针对差异文件编译
[]多核编译

#### 文档结构
```
.
├── ebin
│   └── %% 编译后的beam文件和app resource文件
├── emakefile %% 编译规则
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
│   ├── ets_meta_config %% 记录文件编译信息的ets文件
│   └── mmake.erl %% 脚本
└── src
    ├── test_app.erl
    ├── test.erl
    └── test_sup.erl
```