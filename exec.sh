ARG=$1
BASEPATH=${ARG%.*}
BASENAME=${BASEPATH##*/}
arm-linux-gnueabi-gcc -o ${BASENAME} -mcpu=arm1176jzf-s -mtune=arm1176jzf-s "${BASENAME}.s"
qemu-arm -L /usr/arm-linux-gnueabi/ ${BASENAME}
echo "\nExit code: $?"
rm ${BASENAME}