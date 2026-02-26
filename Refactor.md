# fprettify 项目重构建议

## 一、项目分析

### 1.1 项目概述
fprettify 是一个用于现代 Fortran 代码的自动格式化工具，主要功能包括：
- 自动缩进
- 行续行符对齐
- 一致的运算符和分隔符间距
- 内置函数大小写转换
- 支持预处理器指令（cpp/fypp）

### 1.2 当前代码结构问题
1. **单文件过大**：[`fprettify/__init__.py`](fprettify/__init__.py) 文件有 3349 行，包含所有核心逻辑
2. **职责混乱**：解析器、格式化器、CLI 等混杂在一起
3. **测试组织松散**：测试文件分散，缺少清晰的测试分类
4. **版本管理不规范**：[`fprettify/version.py`](fprettify/version.py) 使用 importlib-metadata，但缺少统一的版本管理

## 二、工程化目标项目结构

```
fprettify/
├── __init__.py              # 包初始化，导出主要 API
├── __main__.py              # CLI 入口点
├── _version.py              # 版本信息（由 setuptools_scm 生成）
│
├── cli/                     # 命令行接口模块
│   ├── __init__.py
│   ├── parser.py            # 参数解析器
│   ├── runner.py            # 执行器
│   └── config.py            # 配置文件处理
│
├── core/                    # 核心格式化引擎
│   ├── __init__.py
│   ├── formatter.py         # 主要格式化逻辑
│   ├── indenter.py          # 缩进处理
│   ├── aligner.py           # 对齐处理
│   └── whitespace.py        # 空格格式化
│
├── parser/                  # 解析器模块
│   ├── __init__.py
│   ├── scanner.py           # 词法扫描器
│   ├── stream.py            # 输入流处理
│   ├── preprocessor.py      # 预处理器支持
│   └── ast.py               # 抽象语法树（可选）
│
├── utils/                   # 工具函数
│   ├── __init__.py
│   ├── logger.py            # 日志记录
│   ├── exceptions.py        # 自定义异常
│   └── helpers.py           # 辅助函数
│
├── formats/                 # 格式化规则配置
│   ├── __init__.py
│   ├── presets.py           # 预设格式
│   └── defaults.py          # 默认配置
│
├── tests/                   # 测试目录
│   ├── __init__.py
│   ├── conftest.py          # pytest 配置
│   ├── unit/                # 单元测试
│   │   ├── __init__.py
│   │   ├── test_formatter.py
│   │   ├── test_indenter.py
│   │   ├── test_aligner.py
│   │   └── test_parser.py
│   ├── integration/         # 集成测试
│   │   ├── __init__.py
│   │   └── test_examples.py
│   └── fixtures/            # 测试数据
│       ├── __init__.py
│       └── samples/         # 示例 Fortran 文件
│
├── examples/                # 示例目录（独立于源码）
│   └── in/
│       └── example.f90
│
└── docs/                    # 文档目录
    ├── index.md
    ├── usage.md
    ├── configuration.md
    └── api.md
```

## 三、重构建议

### 3.1 模块化拆分
| 当前位置 | 新位置 | 说明 |
|---------|--------|------|
| `fprettify/__init__.py` (部分) | `core/formatter.py` | 主要格式化逻辑 |
| `fprettify/__init__.py` (部分) | `core/indenter.py` | 缩进处理类 |
| `fprettify/__init__.py` (部分) | `core/aligner.py` | 对齐处理类 |
| `fprettify/__init__.py` (部分) | `core/whitespace.py` | 空格格式化 |
| `fprettify/fparse_utils.py` | `parser/scanner.py` | 词法扫描器 |
| `fprettify/fparse_utils.py` | `parser/stream.py` | 输入流处理 |
| `fprettify/__init__.py` (CLI) | `cli/` | 命令行接口 |

### 3.2 依赖管理优化
- 将 `configargparse` 改为可选依赖
- 添加 `typing-extensions` 用于类型提示
- 使用 `pyproject.toml` 统一管理依赖

### 3.3 测试改进
- 使用 pytest 替代 unittest
- 添加 fixture 系统管理测试数据
- 增加类型检查测试（mypy）
- 添加代码覆盖率报告

### 3.4 文档改进
- 添加 API 文档（使用 Sphinx 或 MkDocs）
- 添加配置说明文档
- 添加贡献指南

## 四、实施步骤建议

1. **第一阶段**：创建新目录结构，迁移 `fparse_utils.py` 到 `parser/`
2. **第二阶段**：拆分 `__init__.py` 到 `core/` 模块
3. **第三阶段**：创建 `cli/` 模块，分离 CLI 逻辑
4. **第四阶段**：重构测试目录，迁移到 pytest
5. **第五阶段**：添加类型提示和文档

## 五、兼容性考虑
- 保持 CLI 接口不变
- 保持配置文件格式不变
- 提供迁移指南