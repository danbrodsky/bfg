# NOTE: Only works after modifying .gdbinit-gef.py to include `gef_out` to
# store gef command-specific returns
class GrepCmd(gdb.Command):
    """Execute command, but only show lines matching the pattern
    Usage: grep_cmd <cmd> <pattern> """

    def __init__(_):
        super().__init__("grep_cmd", gdb.COMMAND_STATUS)

    def invoke(_, args_raw, __):
        args = gdb.string_to_argv(args_raw)
        if len(args) != 2:
            print("Wrong parameters number. Usage: grep_cmd <cmd> <pattern>")
            return

        global gef_out
        gef_out = ""
        gdb_out = gdb.execute(args[0], to_string=True)
        if gef_out:
            out = gef_out
        else:
            out = gdb_out
        for line in out.splitlines():
            if args[1] in str(line):
                print(line)


GrepCmd()  # required to get it registered
