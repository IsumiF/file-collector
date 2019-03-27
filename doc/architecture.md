# Architecture

系统分为web前端、REST服务器。

部署静态资源服务器，用于存放前端页面（考虑使用GitHub project pages）。web前端通过REST api调用后台服务。

用户上传的文件保存在云计算提供商的对象存储服务，例如七牛云的对象存储，Google Cloud Platform的object storage。
