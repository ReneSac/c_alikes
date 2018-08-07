#? strongSpaces


#converter toUint8*(x: int{lit}):uint8 = x.uint8
#converter toUint16*(x: int{lit}):uint16 = x.uint16
#converter toUint32*(x: int{lit}):uint32 = x.uint32
#converter toUint64*(x: int{lit}):uint64 = x.uint64

#proc `shl` *[T,U: SomeInteger](x:T, y:U): T {.inline, noSideEffect.} = x shl T(y)
#proc `shr` *[T,U: SomeInteger](x:T, y:U): T {.inline, noSideEffect.} = x shr T(y)
#proc `or`  *[T,U: SomeInteger](x:T, y:U): T {.inline, noSideEffect.} = x or T(y)
#proc `and` *[T,U: SomeInteger](x:T, y:U): T {.inline, noSideEffect.} = x and T(y)
#proc `xor` *[T,U: SomeInteger](x:T, y:U): T {.inline, noSideEffect.} = x xor T(y)
#proc `div` *[T,U: SomeInteger](x:T, y:U): T {.inline, noSideEffect.} = x div T(y)

proc `*<`*[T,U: SomeInteger](x:T, y:U): T {.inline, noSideEffect.} =
  ## Shift left, equivalent to C's `<<`. We can't use the same operator in nim
  ## so this *< is the best compromise, as the precedence is also correct.
  x shl y

proc `*>`*[T,U: SomeInteger](x:T, y:U): T {.inline, noSideEffect.} =
  ## Shift right, equivalent to C's `>>` We can't use the same operator in nim
  ## so this *< is the best compromise, as the precedence is also correct.
  x shr y

proc `|`*[T,U: SomeInteger](x:T, y:U): T {.inline, noSideEffect.} = 
  ## Bitwise OR. Due to Nim's precedence rules, it has higher precedence than
  ## `&`, the same precedence as `+` and `-`, unlike C.  
  x or y

proc `&`*[T,U: SomeInteger](x:T, y:U): T {.inline, noSideEffect.} = 
  ## Bitwise AND. Due to Nim's precedence rules, it has lower precedence than
  ## `|` and `|^`. At least it has higher precedence than logical `and`, `or`,
  ## `..` and all comparison operators, unlike C.
  x and y

proc `|^`*[T,U: SomeInteger](x:T, y:U): T {.inline, noSideEffect.} = 
  ## Bitwise XOR. `^` was already taken in nim. The `|` in the front ensures equal
  ## precedence to the simple `|`.
  x xor y

proc `~`*[T:SomeInteger](x: T): T {.inline, noSideEffect.} = 
  ## Bitwise NOT.
  not x

proc `//`*[T,U: SomeInteger](x:T, y:U): T {.inline, noSideEffect.} = 
  ## Integer division.
  x div y

proc `%`*[T,U: SomeInteger](x:T, y:U): T {.inline, noSideEffect.} =
  ## Integer modulo.
  x mod y
  
# Increment and decrement operators from C
proc `++`*(x: var SomeInteger): SomeInteger {.inline, discardable.} = inc(x); return x
proc `--`*(x: var SomeInteger): SomeInteger {.inline, discardable.} = dec(x); return x

# Equivalent to the C postfix operators, but are prefix operators too, as
# Nim don't allows postfix operators. If you want to discard the result, use
# the above operators.
proc `+++`*(x: var SomeInteger): SomeInteger {.inline.} = result = x; inc(x)
proc `---`*(x: var SomeInteger): SomeInteger {.inline.} = result = x; dec(x)

# Assignment Operators:

proc `*>=`*[T:SomeInteger](x: var T, y: SomeInteger) {.inline, noSideEffect.} =
  x = x *> y

proc `*<=`*[T:SomeInteger](x: var T, y: SomeInteger) {.inline, noSideEffect.} =
  x = x *< y

proc `|=` *[T:SomeInteger](x: var T, y: SomeInteger) {.inline, noSideEffect.} =
  x = x | y

proc `&=` *[T:SomeInteger](x: var T, y: SomeInteger) {.inline, noSideEffect.} =
  x = x & y

proc `|^=` *[T:SomeInteger](x: var T, y: SomeInteger) {.inline, noSideEffect.} =
  x = x |^ y

proc `//=`*[T:SomeInteger](x: var T, y: SomeInteger) {.inline, noSideEffect.} =
  x = x // y

proc `%=` *[T:SomeInteger](x: var T, y: SomeInteger) {.inline, noSideEffect.} =
  x = x % y


# Non-operator operations

proc rotl*[T](x, shift: T): T {.inline, noSideEffect.} =
  const x_bits: T = sizeof(x) * 8
  return (x *< shift) || (x *> (x_bits - shift))

proc rotr*[T](x, shift: T): T {.inline, noSideEffect.} =
  const x_bits: T = sizeof(x) * 8
  return (x *> shift) || (x *< (x_bits - shift))

# Bit manipulation

proc setBit*[T:SomeInteger](x: var T, pos: Natural) {.inline, noSideEffect.} =
  x = x or cast[T](1 *< pos)

proc clearBit*[T:SomeInteger](x: var T, pos: Natural) {.inline, noSideEffect.} =
  x &= cast[T](~(1 *< pos))

proc toggleBit*[T:SomeInteger](x: var T, pos: Natural) {.inline, noSideEffect.} =
  x |^= cast[T](1 *< pos)

proc isBitSet*[T:SomeInteger](x: var T, pos: Natural): bool {.inline, noSideEffect.} =
  ## Test the bit at the position `pos` of `x`
  x & (1 *< pos) > 0



proc bitSizeof*(t:typedesc[SomeInteger]): int8 =
  ## Returns the size in bits of a given type ``t``.
  ## Supports only types whose ``sizeof()`` is less than 32 due to it's
  ## output type.
  ## .. code-block:: Nim
  ##   assert int32.bitLenght == 32
  ##   assert .bitLenght == 1
  assert(t.sizeof < 32)
  t.sizeof * 8

proc bitLenght*(x: SomeInteger): int8 =
  ## Returns the minimum number of bits needed to store a given positive value.
  ## Will return 1 if x == 0.
  ## .. code-block:: Nim
  ##   assert 1.bitLenght == 1
  ##   assert 0.bitLenght == 1
  ##   assert 3.bitLenght == 2
  ##   assert 4.bitLenght == 3
  x.bitSizeof - x.countLZ + ord(x == 0)

# Adapted from Jehan: http://forum.nim-lang.org/t/1188

# Addition and subtraction on pointers (pointer arithmetic)
template `+`*[T](p: ptr T, off: SomeInteger): ptr T =
  cast[ptr type(p[])](cast[ByteAddress](p) +% off.int * sizeof(p[]))

template `+=`*[T](p: ptr T, off: SomeInteger) =
  p = p + off

template `-`*[T](p: ptr T, off: SomeInteger): ptr T =
  cast[ptr type(p[])](cast[ByteAddress](p) -% off.int * sizeof(p[]))

template `-=`*[T](p: ptr T, off: SomeInteger) =
  p = p - off

# Subtraction to get the difference between pointers
proc `-`*[T](a, b: ptr T): int {.inline.} =
  # Distance between pointed elements, in multiples of that element size.
  #
  # If the size of the pointed elements is 1 byte and they are more than 2GB
  # apart in a 32 bit machine, the result will be bogus (as it probably will be
  # in C too).
  #
  # There is no similar gotcha for 64 bits as the addressing space for the
  # forseable future is limited to well bellow 63 bits.
  cast[int](cast[uint](a) div sizeof(T).uint) -
    cast[int](cast[uint](b) div sizeof(T).uint)

# Indexing directly on pointers
template `[]`*[T](p: ptr T, off: int): T =
  (p + off)[]

template `[]=`*[T](p: ptr T, off: int, val: T) =
  (p + off)[] = val

# Nim style increment and decrement of pointers
template inc*[T](p: ptr T, off = 1) =
  p = p + off

template dec*[T](p: ptr T, off = 1) =
  p = p - off

# Pre and post increments and decrements on pointers
proc `++`*[T](p: var ptr T): ptr T {.inline, discardable.} =
  p = p + 1
  result = p

proc `--`*[T](p: var ptr T): ptr T {.inline, discardable.} =
  p = p - 1
  result = p

proc `+++`*[T](p: var ptr T): ptr T {.inline.} =
  result = p
  p = p + 1

proc `---`*[T](p: var ptr T): ptr T {.inline.} =
  result = p
  p = p - 1

# The same as above, but deferencing the resulting pointer
proc `*++`*[T](p: var ptr T): var T {.inline.} =
  # Same as *++p in C
  result = (++p)[]

proc `*--`*[T](p: var ptr T): var T {.inline.} =
  ## Same as *--p in C
  result = (--p)[]

proc `*+++`*[T](p: var ptr T): var T {.inline.} =
  ## Same as *p++ in C
  result = (+++p)[]

proc `*---`*[T](p: var ptr T): var T {.inline.} =
  ## Same as *p-- in C
  result = (---p)[]

proc testPtrMath() =
  var p = [2,5,1,0]
  var x = addr p[0]
  assert x[] == 2
  let v = ++x
  assert x[0] == 5
  assert v[1] == 1
  dec x[1]
  assert v[1] == 0
  assert *--x == 2
  assert x[] == 2

template cfor*(preLoop, condition, postLoop, body: untyped) =
  ## C style for loop. It is useful over the normal for when you need to
  ## change the loop counter inside the loop, for example.
  ## To see what exactly it does, look at the source. It is beautifully simple.
  ## Use with #! strongSpaces so you don't need parenthesis around all the arguments.
  ## Use a () around the first argument to transform it in an expression.
  ## For example: (var i = 0; c = 1) or (discard).
  block:
    preLoop
    while condition:
      body
      postLoop


# Not really c-alike, but useful templates from Jehan
# https://forum.nim-lang.org/t/1543#9665
template `<-`*[T](target, source: var T) =
  ## Shallow copies from source to target. Similar to C++ move semantics. See
  ## the nim documentation about `shallowCopy()`.
  shallowCopy target, source

template alias*[T](varname: untyped, value: var T) =
  ## Shallow copies value into a new var with the given name, making an alias.
  ## Similar to C++ move semantics.
  var varname {.inject.}: type(value)
  shallowCopy varname, value

# toHex with pretty printing
proc toHex*[T](x: T, len: int = -1, pretty: bool = false, prefix: bool = false,
               sep: char = '\0', sepInterval: Positive = 4): string {.noSideEffect.} =
  const HexChars = "0123456789ABCDEF"
  var shift: T = 0
  var iprefix = prefix
  var isep = sep

  if pretty:
    iprefix = true
    if isep == '\0':
      isep = '_'

  var resLen = if len < 0: T.sizeof * 2 else: len
  let sepPadding = if isep != '\0': (resLen//sepInterval - 1) else: 0
  let prefixPadding = if iprefix: 2 else: 0
  resLen += sepPadding + prefixPadding
  result = newString(resLen)

  if iprefix:
    result[0..1] = "0x"

  for j in countdown(resLen-1, prefixPadding):
    if shift % 16 == 0 and shift > 0 and result[j+1] != isep:
      result[j] = isep
      continue
    result[j] = HexChars[toU32(x *> shift) & 0xF'i32]
    shift += 4


when isMainModule:
  import math
  proc testAssigment() =
    var a = 1
    a *<= 0x02 & 0x07
    assert a == 4
    a *>= 0x03 |^ 0x01
    assert a == 1
    a |= 0x0A
    assert a == 11
    a &= 0x02
    assert a == 2
    a |^= 0x09
    assert a == 11
    a //= 3
    assert a == 3
    a %= 2
    assert a == 1

  proc testAliases() =
    assert 1 *< 2 == 4
    assert 16*>1 == 8
    assert 0x0F | 0xF0 == 0xFF
    assert 0x0F & 0xF2 == 0x02
    assert 0x0F |^ 0xF2 == 0xFD
    assert 5 // 3 == 1
    assert 5 % 3 == 2
    assert (~0xFF'u16) == 0xFF00
    #TODO: Test precedence

  template foo(x:int) =
    echo x
    return

  for x in 55 .. 60:
    echo x
    if x > 0:
      break
  type Int = int


  proc testPrecedence() =
    foo(394)
    echo "afvsdfasd"
    assert 1 + 16 shl 1 == 1 + (16 shl 1)# == 1 + 16 * 2)
    assert 1 + 16 shl 1 == 1 + (16 shl 1)
    discard

  var x = newSeq[int](10)
  echo x.len
  x.setlen 15
  echo x.len
  testPtrMath()
  testAssigment()
  testAliases()
  testprecedence()
  echo "aaaAll done"
