# some values
ZERO  = IntegerNumber.new(0)
ONE   = IntegerNumber.new(1)
TWO   = IntegerNumber.new(2)
THREE = IntegerNumber.new(3)

### IntegerNumber
class IntegerNumber
  def initialize(value)
    @value = value
  end
  
  def +(b)
    if b.is_a? IntegerNumber
      IntegerNumber.new(value + b.value)
    elsif b.is_a? RationalNumber
      RationalNumber.new(value, 1) + b
    else
      raise ArgumentError.new("IntegerNumber cannot add #{b.class}")
    end
  end

  def -(b)
    if b.is_a? IntegerNumber
      IntegerNumber.new(value - b.value)
    elsif b.is_a? RationalNumber
      RationalNumber.new(value, 1) - b
    else
      raise ArgumentError.new("IntegerNumber cannot sub #{b.class}")
    end
  end
  
  def -@; IntegerNumber.new(- @value); end

  def *(b)
    if b.is_a? IntegerNumber
      IntegerNumber.new(value * b.value)
    elsif b.is_a? RationalNumber
      RationalNumber.new(value, 1) * b
    else
      raise ArgumentError.new("IntegerNumber cannot multiply #{b.class}")
    end
  end
  
  def /(b)
    if b.is_a? IntegerNumber
      raise ZeroDivisionError.new("You can only divide a IntegerNumber by a NONE ZERO number... ") if b.value == 0
      RationalNumber.new(value, b.value)
    elsif b.is_a? RationalNumber
      RationalNumber.new(value, 1) / b
    else
      raise ArgumentError.new("IntegerNumber cannot divide #{b.class}")
    end
  end
  
  def %(b)
    raise ArgumentError.new("") unless b.is_a? IntegerNumber
    IntegerNumber.new(value % b.value)
  end

  def ==(b)
    b.is_a?(IntegerNumber) && value == b.value
  end

  def !=(b)
    !(self == b)
  end

  def >(b)
    if b.is_a? IntegerNumber
      value > b.value
    elsif b.is_a? RationalNumber
      RationalNumber.new(value, 1) > b
    else
      raise ArgumentError.new("IntegerNumber cannot be compared with #{b.class}")
    end
  end

  def >=(b)
    if b.is_a? IntegerNumber
      value >= b.value
    elsif b.is_a? RationalNumber
      RationalNumber.new(value, 1) >= b
    else
      raise ArgumentError.new("IntegerNumber cannot be compared with #{b.class}")
    end
  end

  def <(b)
    if b.is_a? IntegerNumber
      value < b.value
    elsif b.is_a? RationalNumber
      RationalNumber.new(value, 1) < b
    else
      raise ArgumentError.new("IntegerNumber cannot be compared with #{b.class}")
    end
  end

  def <=(b)
    if b.is_a? IntegerNumber
      value <= b.value
    elsif b.is_a? RationalNumber
      RationalNumber.new(value, 1) <= b
    else
      raise ArgumentError.new("IntegerNumber cannot be compared with #{b.class}")
    end
  end

  def value; @value; end

  def iquot(b)
    raise ArgumentError.new("IntegerNumber could only quotient IntegerNumber...") unless b.is_a? IntegerNumber

    IntegerNumber.new(value / b.value)
  end

  def irem(b)
    raise ArgumentError.new("IntegerNumber could only get remainder of IntegerNumber...") unless b.is_a? IntegerNumber

    IntegerNumber.new(value % b.value)
  end

  def inspect
    @value.to_s
  end
end

# MPL iquot
def iquot(a, b)
  raise ArgumentError.new("I want IntegerNumber... ") unless a.is_a? IntegerNumber and b.is_a? IntegerNumber

  a.iquot(b)
end

# MPL irem
def irem(a, b)
  raise ArgumentError.new("I want IntegerNumber... ") unless a.is_a? IntegerNumber and b.is_a? IntegerNumber

  a.irem(b)
end

# Euclid GCD
def gcd(a, b)
  raise ArgumentError.new("I want Integer... ") unless a.is_a? IntegerNumber and b.is_a? IntegerNumber

  if b == ZERO
    a > ZERO ? a : -a
  else
    gcd(b, irem(a, b))
  end
end

# Extended Euclid GCD
def ext_gcd(a, b)
  raise ArgumentError.new("I want Integer... ") unless a.is_a? IntegerNumber and b.is_a? IntegerNumber

  def iter(a, b, mm, nn, m, n)
    if b == ZERO
      a > ZERO ? [a, mm, nn] : [-a, -mm, -nn]
    else
      q = iquot(a, b)
      iter(b, irem(a, b), m, n, mm - q * m, nn - q * n)
    end
  end

  iter(a, b, ONE, ZERO, ZERO, ONE)
end

# Square
def square(n)
  n * n
end

# Test if prime?
def prime?(n)
  raise ArgumentError.new("I want Integer... ") unless n.is_a? IntegerNumber

  if n == ZERO || n == ONE
    return false
  elsif n == TWO
    return true
  elsif n.even?
    return false
  else
    d = THREE
    while square(d) < n
      return false if irem(n, d) == ZERO
      d += THREE
    end
    return true
  end
end

# MPL integer factor
def ifactor(n)
  raise ArgumentError.new("I want Integer... ") unless n.is_a? IntegerNumber

  def times(n, p)
    k = ZERO
    while n % p == ZERO
      k += ONE
      n /= p
    end
    return k == ZERO ? [n, nil] : [n, p, k]
  end

  factors = []
  n, *factor = times(n, TWO)
  factors << factor if factor[0]
  p = THREE
  while n != ONE
    if prime?(p)
      n, *factor = times(n, p)
      factors << factor if factor[0]
    end
    p += TWO
  end
  return factors
end

# Chinese Remainder
def chinese_remainder(*eqs)
  if eqs.length == ZERO
    raise ArgumentError.new("Equations not Match")
  elsif eqs.length == ONE
    x, m = eqs[0]
    return irem(x, m), m
  else
    x1, m1 = eqs[0]
    x2, m2 = eqs[1]
    gcd, c, d = ext_gcd(m1, m2)
    raise ArgumentError.new("Base not relatively prime! ") if gcd != ONE
    m = m1 * m2
    x = c * m1 * x2 + d * m2 * x1
    chinese_remainder(*([[x, m]] + eqs[2..]))
  end
end

# floor and ceiling in Ruby
## n.floor
## n.ceil

# integer divisors
def integer_divisors(n)
  res = []
  (1..Math.sqrt(n).floor).each do |i|
    if irem(n, i) == ZERO
      j = iquot(n, i)
      res.append(i, -i, j, -j)
    end
  end
  return res.sort
end

# base replace
## change base to b
def base_rep(n, b)
  n == ZERO ? [] : [irem(n, b)] + base_rep(iquot(n, b), b)
end

# lcm
def lcm(a, b)
  a * b / gcd(a, b)
end

# extended lcm
def ext_lcm(a, b)
  d, u, v = ext_gcd(a, b)
  k, t = iquot(a, d), iquot(b, d)
  return a * b / d, t * k * u, t * k * v
end

## Rational Number
class RationalNumber
  def initialize(numberator, denoinator)
    print(numberator, denoinator)
    raise ArgumentError.new("Numberator and Denoinator needs to be Integer. ") unless numberator.is_a? Integer and denoinator.is_a? Integer

    if numberator % denoinator
      return IntegerNumber.new(numberator / denoinator)
    else
      sgn = denoinator > 0 ? 1 : -1
      d = gcd(numberator, denoinator)
      @numberator, @denoinator = sgn * numberator / d, sgn * denoinator / d
    end
  end

  def inspect
    "#{@numberator} / #{@denoinator}"
  end
end