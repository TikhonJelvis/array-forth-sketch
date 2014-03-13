{-# LANGUAGE QuasiQuotes #-}

module Instrs where

import qualified Data.List as List

import           SketchQQ

generateInstrs :: IO ()
generateInstrs = writeFile "instrs.sk" $ program 18 10

program :: Int -> Int -> String
program bits memory = [sketch|

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
  assert stack.ptr < 8 && stack.ptr >= 0;
  stack.body[stack.ptr] = n;
}

bit[BIT_SIZE] pop_s(Stack stack) {
  assert stack.ptr < 8 && stack.ptr >= 0;
  bit[BIT_SIZE] top = stack.body[stack.ptr];
  stack.ptr = stack.ptr - 1 >= 0 ? stack.ptr - 1 : 7;
  assert stack.ptr < 8 && stack.ptr >= 0;
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

$body
|]
  where bitSize    = show bits
        memorySize = show memory
        body       = List.intercalate "\n\n" instrs


instr name body = [sketch|
int $name(int step) {
  if (step % 4 == 0) {
    s.p++;
  }
  step++;
$body
  return step % 4;
}
|]

instrs = [ 
-- ret used to return pop_r(s) but I'm honestly not sure why.
  instr "ret" [sketch| 
  s.p = s.r;
  step = 0;
  pop_r(s);
  |],

  instr "exec" [sketch|
  bit[BIT_SIZE] temp = s.r;
  s.r = s.p;
  s.p = temp;
  |],

  -- XXX: I think unext exposes a weird bug in Sketch
  -- Error: The parents of a boolean operator must be boolean !!!  mth=name_259__ARR_R  fth=C1

  -- instr "unext" [sketch|
  -- if (s.r == 0) {
  --   pop_r(s);
  -- } else {
  --   s.r--;
  --   s.p--;
  -- }
  -- |],

  instr "fetchP" [sketch|
  push_d(s, s.memory[to_int(s.p)]);
  s.p++;
  |],

  instr "fetchPlus" [sketch|
  push_d(s, s.memory[to_int(s.a)]);
  s.a++;
  |],

  instr "fetch" [sketch|
  push_d(s, s.memory[to_int(s.a)]);
  |],

  instr "fetchB" [sketch|
  push_d(s, s.memory[to_int(s.b)]);
  |],

  instr "storeP" [sketch|
  bit[BIT_SIZE] temp = pop_d(s);
  s.memory[to_int(s.p)] = temp;
  s.p++;
  |],

  instr "storePlus" [sketch|
  bit[BIT_SIZE] temp = pop_d(s);
  s.memory[to_int(s.a)] = temp;
  s.a++;
  |],

  instr "storeB" [sketch|
  bit[BIT_SIZE] temp = pop_d(s);
  s.memory[to_int(s.b)] = temp;
  |],

  instr "store" [sketch|
  bit[BIT_SIZE] temp = pop_d(s);
  s.memory[to_int(s.a)] = temp;
  |],

  instr "multiplyStep" [sketch|
  return 0; // I'm too lazy to implement multiply step at the moment.
  |],

  instr "times2" [sketch|
  s.t = s.t << 1;
  |],

  instr "div2" [sketch|
  s.t = s.t >> 1;
  |],

  instr "not" [sketch|
  s.t = ~s.t;
  |],

  instr "plus" [sketch|
  bit[BIT_SIZE] res = s.t + s.s;
  pop_d(s);
  s.t = res;
  |],

  instr "and" [sketch|
  bit[BIT_SIZE] temp = pop_d(s);
  s.t = s.t & temp;
  |],

  instr "or" [sketch|
  bit[BIT_SIZE] temp = pop_d(s);
  s.t = s.t ^ temp;
  |],

  instr "drop" [sketch|
  pop_d(s);
  |],

  instr "dup" [sketch|
  push_d(s, s.t);
  |],

  instr "pop" [sketch|
  push_d(s, pop_r(s));
  |],

  instr "over" [sketch|
  push_d(s, s.s);
  |],

  instr "readA" [sketch|
  push_d(s, s.a);
  |],

  instr "nop" [sketch|
  |],

  instr "push" [sketch|
  push_r(s, pop_d(s));
  |],

  instr "setB" [sketch|
  s.b = pop_d(s);
  |],

  instr "setA" [sketch|
  s.a = pop_d(s);
  |],

  [sketch|
int loadLiteral(bit[BIT_SIZE] literal, int step) {
  if (step % 4 == 0) {
    s.p++;
  }
  step++;

  push_d(s, literal);
  s.p++;
  return step % 4;
}
  |]
  ]
