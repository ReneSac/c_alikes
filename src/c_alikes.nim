#? strongSpaces

proc `*<`*[T,U: SomeInteger](x:T, y:U): T {.inline, noSideEffect.} =
  ## Shift left, equivalent to C's ``<<``. We can't use the same operator in nim
  ## so this ``*<`` is the best compromise, as the precedence is also correct.
  x shl y

proc `*>`*[T,U: SomeInteger](x:T, y:U): T {.inline, noSideEffect.} =
  ## Shift right, equivalent to C's ``>>`` We can't use the same operator in nim
  ## so this ``*<`` is the best compromise, as the precedence is also correct.
  x shr y

proc `|`*[T,U: SomeInteger](x:T, y:U): T {.inline, noSideEffect.} = 
  ## Bitwise OR. Due to Nim's precedence rules, it has higher precedence than
  ## ``&``, the same precedence as ``+`` and ``-``, unlike C.  
  x or y

proc `&`*[T,U: SomeInteger](x:T, y:U): T {.inline, noSideEffect.} = 
  ## Bitwise AND. Due to Nim's precedence rules, it has lower precedence than
  ## ``|`` and ``|^``. At least it has higher precedence than logical ``and``, ``or``,
  ## ``..`` and all comparison operators, unlike C.
  x and y

proc `|^`*[T,U: SomeInteger](x:T, y:U): T {.inline, noSideEffect.} = 
  ## Bitwise XOR. ``^`` was already taken in nim. The ``|`` in the front ensures equal
  ## precedence to the simple ``|``.
  x xor y

proc `~`*[T:SomeInteger](x: T): T {.inline, noSideEffect.} = 
  ## Bitwise NOT. Alias for nim's ``not`` operator.
  not x

proc `//`*[T,U: SomeInteger](x:T, y:U): T {.inline, noSideEffect.} = 
  ## Integer division. Alias for nim's ``div`` operator.
  x div y

proc `%`*[T,U: SomeInteger](x:T, y:U): T {.inline, noSideEffect.} =
  ## Integer modulo. Alias for nim's ``mod`` operator.
  x mod y


# Increment and decrement operators from C

proc `++`*(x: var SomeInteger): SomeInteger {.inline, discardable.} =
  ## Increment `x` and return the incremented value. Discardable.
  ## Same as ``++x`` pre-increment in C.
  inc(x)
  result = x

proc `--`*(x: var SomeInteger): SomeInteger {.inline, discardable.} =
  ## Decrement `x` and return the decremented value. Discardable.
  ## Same as ``--x`` pre-decrement in C.
  dec(x)
  result = x

proc `+++`*(x: var SomeInteger): SomeInteger {.inline.} =
  ## Increment `x` but return the value before the increment.
  ## Same as ``x++`` post-increment in C, but a prefix operator, as nim doesn't
  ## support postfix operators.
  ## If you want to discard the returned value, use the pre-increment version.
  result = x
  inc(x)

proc `---`*(x: var SomeInteger): SomeInteger {.inline.} = 
  ## Decrement `x` but return the value before the decrement.
  ## Same as ``x--`` post-decrement in C, but a prefix operator, as nim doesn't
  ## support postfix operators.
  ## If you want to discard the returned value, use the pre-decrement version.
  result = x
  dec(x)

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
  ## Test the bit at the position `pos` of `x`.
  x & (1 *< pos) > 0



proc bitSizeof*(t: typedesc): int =
  ## Returns the size in bits of a given type `t`.
  ##
  ## .. code-block:: Nim
  ##   assert int32.bitLenght == 32
  t.sizeof * 8

proc bitLenght*(x: SomeInteger): int =
  ## Returns the minimum number of bits needed to store a given positive value.
  ## Will return 1 if ``x == 0``. 
  ## For that the use of the `noUndefinedBitOps` flag is required.
  ##
  ## .. code-block:: Nim
  ##   assert 1.bitLenght == 1
  ##   assert 0.bitLenght == 1
  ##   assert 3.bitLenght == 2
  ##   assert 4.bitLenght == 3
  x.bitSizeof - x.countLeadingZeroBits + ord(x == 0)

# Adapted from Jehan: http://forum.nim-lang.org/t/1188

# Addition and subtraction on pointers (pointer arithmetic)
template `+`*[T](p: ptr T, off: SomeInteger): ptr T =
  ## Addition on pointer. Same as in C.
  cast[ptr type(p[])](cast[ByteAddress](p) +% off.int * sizeof(p[]))

template `+=`*[T](p: ptr T, off: SomeInteger) =
  ## Addition on pointer. Same as in C.
  p = p + off

template `-`*[T](p: ptr T, off: SomeInteger): ptr T =
  ## Subtraction on pointer. Same as in C.
  cast[ptr type(p[])](cast[ByteAddress](p) -% off.int * sizeof(p[]))

template `-=`*[T](p: ptr T, off: SomeInteger) =
  ## Subtraction on pointer. Same as in C.
  p = p - off

# Subtraction to get the difference between pointers
proc `-`*[T](a, b: ptr T): int {.inline.} =
  ## Distance between pointed elements, in multiples of that element size.
  ##
  ## If the size of the pointed elements is 1 byte and they are more than 2GB
  ## apart in a 32 bit machine, the result will be bogus (as it probably will be
  ## in C too).
  ##
  ## There is no similar gotcha for 64 bits as the addressing space for the
  ## forseable future is limited to well bellow 63 bits.
  cast[int](cast[uint](a) div sizeof(T).uint) -
    cast[int](cast[uint](b) div sizeof(T).uint)

# Indexing directly on pointers
template `[]`*[T](p: ptr T, off: int): T =
  ## Indexing like arrays directly on pointers
  (p + off)[]

template `[]=`*[T](p: ptr T, off: int, val: T) =
  ## Indexing assigment like arrays directly on pointers
  (p + off)[] = val

# Nim style increment and decrement of pointers
template inc*[T](p: ptr T, off = 1) =
  ## Nim style increment for pointers.
  p = p + off

template dec*[T](p: ptr T, off = 1) =
  ## Nim style decrement for pointers.
  p = p - off

# Pre and post increments and decrements on pointers
proc `++`*[T](p: var ptr T): ptr T {.inline, discardable.} =
  ## Increment pointer `p` in-place and return the incremented pointer.
  ## Discardable.
  ## Same as ``++p`` pointer pre-increment in C.
  p = p + 1
  result = p

proc `--`*[T](p: var ptr T): ptr T {.inline, discardable.} =
  ## Decrement pointer `p` in-place and return the decremented pointer.
  ## Discardable.
  ## Same as ``++p`` pointer pre-decrement in C.
  p = p - 1
  result = p

proc `+++`*[T](p: var ptr T): ptr T {.inline.} =
  ## Increment pointer `p` in-place but return the pointer
  ## before the increment.
  ## Discardable.
  ## Same as ``p++`` pointer post-increment in C.
  result = p
  p = p + 1

proc `---`*[T](p: var ptr T): ptr T {.inline.} =
  ## Decrement pointer `p` in-place but return the pointer
  ## before the decrement.
  ## Discardable.
  ## Same as ``p++`` pointer post-decrement in C.
  result = p
  p = p - 1

# The same as above, but deferencing the resulting pointer
proc `*++`*[T](p: var ptr T): var T {.inline.} =
  ## First increment pointer `p` in place and then deference it, 
  ## returning a mutable reference to the pointed value.
  ## Same as ``*++p`` in C.
  result = (++p)[]

proc `*--`*[T](p: var ptr T): var T {.inline.} =
  ## First decrement pointer `p` in place and then deference it, 
  ## returning a mutable reference to the pointed value.
  ## Same as ``*--p`` in C.
  result = (--p)[]

proc `*+++`*[T](p: var ptr T): var T {.inline.} =
  ## First deference pointer `p`, returning a mutable reference to
  ## the pointed value, and then increment the pointer in place.
  ## Same as ``*p++`` in C.
  result = (+++p)[]

proc `*---`*[T](p: var ptr T): var T {.inline.} =
  ## First deference pointer `p`, returning a mutable reference to
  ## the pointed value, and then decrement the pointer in place.
  ## Same as ``*p--`` in C.
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
  ##
  ## Use with `#? strongSpaces` so you don't need parenthesis around all the arguments.
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
  ## Shallow copies from `source` to `target`. Similar to C++ move semantics. See
  ## the nim documentation about `shallowCopy()`.
  shallowCopy target, source

template alias*[T](varname: untyped, value: var T) =
  ## Shallow copies value into a new var with the given name, making an alias.
  ## Similar to C++ move semantics.
  var varname {.inject.}: type(value)
  shallowCopy varname, value

proc toHex*[T](x: T, len: int = -1, pretty: bool = false, prefix: bool = false,
               sep: char = '\0'): string {.noSideEffect.} =
  ## Same as the toHex from strutils, but with optional pretty printing.
  ## By default, when pretty printing, it uses ``_`` as the separator
  ## each 4 hex digits.
  ##
  ## .. code-block:: Nim
  ##   let h = 0xDEAD_BEEF'u32
  ##   doAssert h.toHex(pretty=true) == "0xDEAD_BEEF"
  const HexChars = "0123456789ABCDEF"
  let casted = cast[int64](x)
  var shift = 0'i64
  var iprefix = prefix
  var isep = sep

  if pretty:
    iprefix = true
    if isep == '\0':
      isep = '_'

  var resLen = if len < 0: T.sizeof * 2 else: len
  let sepPadding = if isep != '\0': (resLen//4 - 1) else: 0
  let prefixPadding = if iprefix: 2 else: 0
  resLen += sepPadding + prefixPadding
  result = newString(resLen)

  if iprefix:
    result[0..1] = "0x"

  for j in countdown(resLen-1, prefixPadding):
    if shift % 16 == 0 and shift > 0 and result[j+1] != isep:
      result[j] = isep
      continue
    result[j] = HexChars[int(casted *> shift & 0xF)]
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
