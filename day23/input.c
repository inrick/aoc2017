#include <stdio.h>
#include <stdbool.h>

bool isprime(int n) {
  if (n < 2) {
    return false;
  }
  for (int i = 2; i*i <= n; ++i) {
    if (n%i == 0) {
      return false;
    }
  }
  return true;
}

int main() {
  int b = 0;
  int c = 0;
  int h = 0;
  b = 79*100 + 100000;
  c = b + 17000;
  for (;;) {
    if (!isprime(b)) {
      ++h;
    }
    if (b == c) {
      break;
    }
    b += 17;
  }
  printf("h=%d\n", h);
  return 0;
}

/* Original translation of input which was reduced to the above: */
/*int main() {
  int a = 1;
  int b = 0;
  int c = 0;
  int d = 0;
  int e = 0;
  int f = 0;
  int g = 0;
  int h = 0;
  b = 79;
  c = b;
  if (a != 0) {
    b = b*100 + 100000;
    c = b + 17000;
  }
  for (;;) {
    f = 1;
    d = 2;
    do {
      e = 2;
      do {
        g = d*e - b;
        if (g == 0) {
          f = 0;
        }
        ++e;
        g = e - b;
      } while (g != 0);
      ++d;
      g = d - b;
    } while (g != 0);
    if (f == 0) {
      ++h;
    }
    g = b - c;
    if (g == 0) {
      break;
    }
    b += 17;
  }
  printf("h=%d\n", h);
  return 0;
}*/
