#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include "../lib/range"

void print(const char* fmt, ...);

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

void print(const char* fmt, ...) {
va_list args;va_start(args, fmt);	int fmt_len = strlen(fmt);
	char* arg_type = ((char*) malloc(128 * sizeof(char)));
	int arg_type_len = 0;
	int i = 0;
	while ( i < fmt_len ) {
		if(fmt[i] == '$' && fmt[i + 1] == '{') {
			i += 2;
			while ( fmt[i] != '}' ) {
				arg_type[arg_type_len] = fmt[i];
				arg_type_len++;
				i++;
			}
			i++;
			arg_type[arg_type_len] = '\0';
			arg_type_len = 0;
			if(strcmp(arg_type, "int") == 0) {
				printf("%d",  next_arg(args, int) );
				continue;
			}
			if(strcmp(arg_type, "float") == 0) {
				printf("%f",  next_arg(args, double) );
				continue;
			}
			if(strcmp(arg_type, "hex") == 0) {
				printf("%x",  next_arg(args, int) );
				continue;
			}
			if(strcmp(arg_type, "str") == 0) {
				printf("%s",  next_arg(args, char*) );
				continue;
			}
			if(strcmp(arg_type, "char") == 0) {
				printf("%c",  next_arg(args, int) );
				continue;
			}
			va_end(args);			free(arg_type);
			return;
		}
		putchar(fmt[i]);
		i++;
	}
	free(arg_type);
}


void other(int n);

int main() {
	int n = atoi(read(2));
	other(n);

	return 0;
}

void other(int n) {
	if(n == 3) {
		println("c3");
		println("${int}", 2);
	}
	if(n == 6) {
		println("c6");
		println("${int}", 1);
		println("${int}", 3);
		return;
	}
	println("${int}", 1);
	return;
}

