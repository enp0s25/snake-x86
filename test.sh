nasm $1 -o boot.bin && qemu-system-x86_64 boot.bin -accel kvm
