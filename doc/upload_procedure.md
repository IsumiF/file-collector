# 文件上传流程

1. client调用server的api，获得上传目的地以及上传证书
2. client利用上传证书向云存储上传文件
3. 云存储调用server的api，告知其上传完毕
