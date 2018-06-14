# tman
仿ranch基于erlang的tcp连接管理的小demo
## 介绍 ##
1.跟ranch很像，但是因为本人能力问题，ssl的部分就没有加进去，所以去掉了两层的controlling_process，代码也基本上由gen_server行为模式来写
2.acceptor在断开之后，主动通知连接管理进程移除自己，让连接管理进程能够主动唤醒处于睡眠的acceptor
## 用法 ##
暂时只支持linux系统
### 编译 ###
主目录下``` make ``` 或者 ``` make all ```
### 启动demo ###
``` ctl start ```
## 调用测试代码 ##
``` tman_test:test() ```
至此，我们启动了一个tcp监听进程，端口为8089
## 测试连接 ##
``` gen_tcp:connect(localhost, 8089, []) ```
因为测试代码没有发送消息，所以连接会五秒后自动断开
