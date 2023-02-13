ARG=$1
FILE=${ARG%.*}
make
./compile ${FILE}.wacc
arm-linux-gnueabi-gcc -o ${FILE} -mcpu=arm1176jzf-s -mtune=arm1176jzf-s "${FILE}.s"
qemu-arm -L /usr/arm-linux-gnueabi/ ${FILE}