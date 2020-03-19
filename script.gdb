# catch syscall read
# catch syscall lseek
# disable 1 2

# commands 2 (if read syscall then do this)
#  disable 1 2
#  continue
# end

# commands 3 (if lseek syscall then do this)
#  if $rdi == 31
#   enable 1 2
#   continue
#  else
#   continue
#  end
# end

# set {ulong} 0x13371337 = 0xdeadbeef

# idabridge 0.0.0.0:2305

start
continue

kill
