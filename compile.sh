temp_path=$(dirname "$0")
cd $temp_path
real_path=$(pwd)
echo  "本脚本文件所在目录路径是: $real_path "
cd $real_path


mkdir -p priv
gcc -fPIC -O3 -c c_src/equeue.c -o c_src/equeue.o -I /usr/local/lib/erlang/usr/include
g++ -shared -fPIC -O3 -o /usr/local/lib/libgtpushsdk_mid.so c_src/gtpushsdk_mid.cpp  -lgtpushsdk_gcc4.1.2_64 -L lib/ -I c_src/
ldconfig
#cp priv/libgtpushsdk_mid.so /usr/local/lib
gcc --shared -fPIC -O3 c_src/equeue.o -o priv/getui_nif_async.so c_src/getui_nif_async.c  -I . -I /usr/local/lib/erlang/usr/include -I c_src/  -L lib/ -lpthread -ldl -lrt -lgtpushsdk_mid
#gcc --shared -fPIC c_src/equeue.o -o priv/getui_nif_async.so c_src/getui_nif_async.c  -I . -I /usr/local/lib/erlang/usr/include -I c_src/  -L priv/ -lpthread -ldl -lrt -lgtpushsdk_mid  -D DEBUG
