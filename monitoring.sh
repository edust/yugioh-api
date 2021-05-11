#!/bin/sh
 
#切换到目录
cd /root/server
date=`date`
 
#查询端口占用
lsof -i:9800
 
# $? -ne 0 不存在 $? -eq 0存在 
if [ $? -ne 0 ]
then
    nohup ./ygosvr >/dev/null 2>&1 &
    echo $date  ":=============== restart ===============" >> monitoring.log
else
    echo "normal"
fi