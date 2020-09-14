# catch syscall read
# catch syscall lseek
# disable 1 2

# commands 1 (if read syscall then do this)
#  disable 1 2
#  continue
# end

# commands 2 (if lseek syscall then do this)
#  if $rdi == 31
#   enable 1 2
#   continue
#  else
#   continue
#  end
# end

# set {ulong} 0x13371337 = 0xdeadbeef

# idabridge 0.0.0.0:2305

# call (void*) memset(start_addr, char, size)
# call (void*) mprotect(start_addr, size, X|W|R)

# set {unsigned long} memory_address = new_value

start
continue

kill
