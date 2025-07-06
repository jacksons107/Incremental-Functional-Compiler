#include <stdio.h>
#include <stdint.h>

extern int64_t entry(void);

void print_int(int64_t n) { printf("%lld\n", n); }

int main(int argc, char **argv)
{
    int64_t result = entry();
    print_int(result);
    return 0;
}
