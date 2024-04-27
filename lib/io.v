use stdio from C_STD
use stdarg from C_STD
use stdlib from C_STD
use string from C_STD
use '../lib/range' from C

func print(fmt: const str, *args) do
  fmt_len: int = strlen(fmt)
  arg_type: str = malloc(128 * sizeof(char)) as str
  defer free(arg_type)
  arg_type_len: int = 0
  
  i: int = 0

  while i < fmt_len do
    if fmt[i] == !{'$'} and fmt[i + 1] == !{'{'} do
      i += 2

      while fmt[i] != !{'}'} do
        arg_type[arg_type_len] = fmt[i]

        arg_type_len++
        i++
      end
      i++

      arg_type[arg_type_len] = !{'\0'}
      arg_type_len = 0

      if strcmp(arg_type, "int") == 0 do
        printf("%d", !{ next_arg(args, int) } )
        continue
      end
      if strcmp(arg_type, "float") == 0 do
        printf("%f", !{ next_arg(args, double) } )
        continue
      end
      if strcmp(arg_type, "hex") == 0 do
        printf("%x", !{ next_arg(args, int) } )
        continue
      end
      if strcmp(arg_type, "str") == 0 do
        printf("%s", !{ next_arg(args, char*) } )
        continue
      end
      if strcmp(arg_type, "char") == 0 do
        printf("%c", !{ next_arg(args, int) } )
        continue
      end

      return
    end

    putchar(fmt[i])
    i++
  end
end

!{
#define next_arg(args, type) va_arg(args, type)

void println(const char* fmt, ...) {
  print(fmt);
  putchar('\n');
}

char* read(int size) {
  char* input = (char*) malloc(sizeof(char) * size);
  fgets(input, size, stdin);
  return input;
}
}

