#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

enum dir { L, R };
enum state { A, B, C, D, E, F };

typedef struct node { struct node* left; struct node* right; int value; } node;

node* new(node* left, node* right) {
  node* n = malloc(sizeof(*n));
  n->left = left;
  n->right = right;
  n->value = 0;
  if (left != NULL) {
    left->right = n;
  }
  if (right != NULL) {
    right->left = n;
  }
  return n;
}

node* move(node* cur, enum dir dir) {
  switch (dir) {
  case L: return cur->left == NULL ? new(NULL, cur) : cur->left;
  case R: return cur->right == NULL ? new(cur, NULL) : cur->right;
  default: assert(0);
  }
}

void next(
  node** cur, enum state* state,
  int value0, enum dir dir0, enum state state0,
  int value1, enum dir dir1, enum state state1) {
  switch ((*cur)->value) {
  case 0:
    (*cur)->value = value0;
    *cur = move(*cur, dir0);
    *state = state0;
    break;
  case 1:
    (*cur)->value = value1;
    *cur = move(*cur, dir1);
    *state = state1;
    break;
  default: assert(0);
  }
}

int main() {
  enum state state = A;
  node* cur = new(NULL, NULL);
  for (int i = 0; i < 12459852; ++i) {
    switch (state) {
    case A: next(&cur, &state, 1, R, B, 1, L, E); break;
    case B: next(&cur, &state, 1, R, C, 1, R, F); break;
    case C: next(&cur, &state, 1, L, D, 0, R, B); break;
    case D: next(&cur, &state, 1, R, E, 0, L, C); break;
    case E: next(&cur, &state, 1, L, A, 0, R, D); break;
    case F: next(&cur, &state, 1, R, A, 1, R, C); break;
    default: assert(0);
    }
  }
  while (cur->left != NULL) cur = cur->left;
  int sum = 0;
  while (cur != NULL) {
    sum += cur->value;
    cur = cur->right;
  }
  printf("a) %d\n", sum);
  return 0;
}
