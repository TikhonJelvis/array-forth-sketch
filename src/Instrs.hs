{-# LANGUAGE QuasiQuotes #-}

module Instrs where

import SketchQQ

generateInstrs :: IO ()
generateInstrs = writeFile "instrs.sk" $ instrs 18 10

instrs :: Int -> Int -> String
instrs bits memory = [sketch|

int MEM_SIZE = $memorySize;
int BIT_SIZE = $bitSize;

int to_int(bit[BIT_SIZE] n) {
  int res = 0;
  int i_n = 1;
  for (int i = 0; i < BIT_SIZE; i++) {
    if (n[i]) {
      res += i_n;
    }
    i_n *= 2;
  }

  return res;
}

struct Stack {
  int ptr;
  bit[BIT_SIZE][8] body;
}

Stack empty() { return new Stack(ptr = 0, body = {0,0,0,0,0,0,0,0}); }

void push_s(Stack stack, bit[BIT_SIZE] n) {
  stack.ptr = (stack.ptr + 1) % 8;
  stack.body[stack.ptr] = n;
}

bit[BIT_SIZE] pop_s(Stack stack) {
  bit[BIT_SIZE] top = stack.body[stack.ptr];
  stack.ptr = (stack.ptr - 1) % 8;
  return top;
}

/* Maybe not terribly efficient. Meh. */
struct State {
  bit[BIT_SIZE] a;
  bit[BIT_SIZE] b;
  bit[BIT_SIZE] p;
  bit[BIT_SIZE] r;
  bit[BIT_SIZE] s;
  bit[BIT_SIZE] t;
  Stack data;
  Stack ret;
  bit[BIT_SIZE][MEM_SIZE] memory;
}

State start() {
  return new State(a      = 0,
                   b      = 0,
                   p      = 0,
                   r      = 0,
                   s      = 0,
                   t      = 0,
                   data   = empty(),
                   ret    = empty(),
                   memory = {});
}

State s = new State(a      = 0,
                    b      = 0,
                    p      = 0,
                    r      = 0,
                    s      = 0,
                    t      = 0,
                    data   = new Stack(ptr = 0, body = {0,0,0,0,0,0,0,0}),
                    ret    = new Stack(ptr = 0, body = {0,0,0,0,0,0,0,0}),
                    memory = {});

void reset() {
  s = start();
}

bit[BIT_SIZE] pop_d(State s) {
  bit[BIT_SIZE] temp = s.t;
  bit[BIT_SIZE] res = pop_s(s.data);
  s.t = s.s;
  s.s = res;
  return temp;
}

void push_d(State s, bit[BIT_SIZE] value) {
  push_s(s.data, s.s);
  s.s = s.t;
  s.t = value;
}

bit[BIT_SIZE] pop_r(State s) {
  bit[BIT_SIZE] temp = s.r;
  s.r = pop_s(s.ret);
  return temp;
}

void push_r(State s, bit[BIT_SIZE] value) {
  push_s(s.ret, s.r);
  s.r = value;
}

bit[BIT_SIZE] ret() {
  s.p = s.r;
  return pop_r(s);
}

bit[BIT_SIZE] exec() {
  bit[BIT_SIZE] temp = s.r;
  s.r = s.p;
  s.p = temp;
  return 0;
}

bit[BIT_SIZE] unext() {
  if (s.r == 0) {
    pop_r(s);
  } else {
    s.r--;
    s.p--;
  }
  return 0;
}

bit[BIT_SIZE] fetchP() {
  push_d(s, s.memory[to_int(s.p)]);
  s.p++;
  return 0;
}

bit[BIT_SIZE] fetchPlus() {
  push_d(s, s.memory[to_int(s.a)]);
  s.a++;
  return 0;
}

bit[BIT_SIZE] fetchB() {
  push_d(s, s.memory[to_int(s.b)]);
  return 0;
}

bit[BIT_SIZE] fetch() {
  push_d(s, s.memory[to_int(s.a)]);
  return 0;
}

bit[BIT_SIZE] storeP() {
  bit[BIT_SIZE] temp = pop_d(s);
  s.memory[to_int(s.p)] = temp;
  s.p++;
  return 0;
}

bit[BIT_SIZE] storePlus() {
  bit[BIT_SIZE] temp = pop_d(s);
  s.memory[to_int(s.a)] = temp;
  s.a++;
  return 0;
}

bit[BIT_SIZE] storeB() {
  bit[BIT_SIZE] temp = pop_d(s);
  s.memory[to_int(s.b)] = temp;
  return 0;
}

bit[BIT_SIZE] store() {
  bit[BIT_SIZE] temp = pop_d(s);
  s.memory[to_int(s.a)] = temp;
  return 0;
}

bit[BIT_SIZE] multiplyStep() {
  return 0; // I'm too lazy to implement multiply step at the moment.
}

bit[BIT_SIZE] times2() {
  s.t = s.t >> 1;
  return 0;
}

bit[BIT_SIZE] div2() {
  s.t = s.t << 1;
  return 0;
}

bit[BIT_SIZE] not() {
  s.t = ~s.t;
  return 0;
}

bit[BIT_SIZE] plus() {
  bit[BIT_SIZE] res = s.t + s.s;
  pop_d(s);
  s.t = res;
  return 0;
}

bit[BIT_SIZE] and() {
  bit[BIT_SIZE] temp = pop_d(s);
  s.t = s.t & temp;
  return 0;
}

bit[BIT_SIZE] or() {
  bit[BIT_SIZE] temp = pop_d(s);
  s.t = s.t ^ temp;
  return 0;
}

bit[BIT_SIZE] drop() {
  pop_d(s);
  return 0;
}

bit[BIT_SIZE] dup() {
  push_d(s, s.t);
  return 0;
}

bit[BIT_SIZE] pop() {
  push_d(s, pop_r(s));
  return 0;
}

bit[BIT_SIZE] over() {
  push_d(s, s.s);
  return 0;
}

bit[BIT_SIZE] readA() {
  push_d(s, s.a);
  return 0;
}

bit[BIT_SIZE] nop() {
  return 0;
}

bit[BIT_SIZE] push() {
  push_r(s, pop_d(s));
  return 0;
}

bit[BIT_SIZE] setB() {
  s.b = pop_d(s);
  return 0;
}

bit[BIT_SIZE] setA() {
  s.a = pop_d(s);
  return 0;
}

bit[BIT_SIZE] loadLiteral(bit[BIT_SIZE] literal) {
  push_d(s, literal);
  return 0;
}

|]
  where bitSize    = show bits
        memorySize = show memory
