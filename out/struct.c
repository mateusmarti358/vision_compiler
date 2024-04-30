#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

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
va_list args;va_start(args, fmt);	long long fmt_len = strlen(fmt);
	char* arg_type = ((char*) malloc(128 * sizeof(char)));
	long long arg_type_len = 0;
	long long i = 0;
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


typedef struct {
  float x;
  float y;
  float z;
} vec3;

typedef struct {
  vec3* pos;
  long long health;
  char* name;
} entity;

void vec3_move_forward(vec3* self);
void vec3_move_left(vec3* self);
void vec3_print(vec3* self);
void entity_print(entity* self);

int main() {
	entity* player = &(entity){.pos = &(vec3){.x = 5,.y = 5,.z = 5,},.health = 100,.name = "Player",};
	entity_print(player);
	vec3_move_forward(player->pos);
	vec3_move_left(player->pos);
	entity_print(player);

	return 0;
}

void vec3_move_forward(vec3* self) {
	self->z = self->z - 1;
}

void vec3_move_left(vec3* self) {
	self->x = self->x - 1;
}

void vec3_print(vec3* self) {
	println("pos: ${float}, ${float}, ${float}", self->x, self->y, self->z);
}

void entity_print(entity* self) {
	println("name: ${str}\nhealth: ${int}\n", self->name, self->health);
	vec3_print(self->pos);
}

