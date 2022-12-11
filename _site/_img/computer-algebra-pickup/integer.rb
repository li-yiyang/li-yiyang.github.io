### Integer

# MPL iquot
def iquot(a, b)
  raise ArgumentError.new("I want Integer... ") unless a.is_a? Integer and b.is_a? Integer

  a / b
end

# MPL irem
def irem(a, b)
  raise ArgumentError.new("I want Integer... ") unless a.is_a? Integer and b.is_a? Integer

  a % b
end

# Euclid GCD
def gcd(a, b)
  raise ArgumentError.new("I want Integer... ") unless a.is_a? Integer and b.is_a? Integer

  if b == 0
    a > 0 ? a : -a
  else
    gcd(b, irem(a, b))
  end
end

# Extended Euclid GCD
def ext_gcd(a, b)
  raise ArgumentError.new("I want Integer... ") unless a.is_a? Integer and b.is_a? Integer

  def iter(a, b, mm, nn, m, n)
    if b == 0
      a > 0 ? [a, mm, nn] : [-a, -mm, -nn]
    else
      q = iquot(a, b)
      iter(b, irem(a, b), m, n, mm - q * m, nn - q * n)
    end
  end

  iter(a, b, 1, 0, 0, 1)
end

# Square
def square(n)
  n * n
end

# Test if prime?
def prime?(n)
  raise ArgumentError.new("I want Integer... ") unless n.is_a? Integer

  if n == 0 || n == 1
    return false
  elsif n == 2
    return true
  elsif n.even?
    return false
  else
    d = 3
    while square(d) < n
      return false if irem(n, d) == 0
      d += 3
    end
    return true
  end
end

# MPL integer factor
def ifactor(n)
  raise ArgumentError.new("I want Integer... ") unless n.is_a? Integer

  def times(n, p)
    k = 0
    while n % p == 0
      k += 1
      n /= p
    end
    return k == 0 ? [n, nil] : [n, p, k]
  end

  factors = []
  n, *factor = times(n, 2)
  factors << factor if factor[0]
  p = 3
  while n != 1
    if prime?(p)
      n, *factor = times(n, p)
      factors << factor if factor[0]
    end
    p += 2
  end
  return factors
end

# Chinese Remainder
def chinese_remainder(*eqs)
  if eqs.length == 0
    raise ArgumentError.new("Equations not Match")
  elsif eqs.length == 1
    x, m = eqs[0]
    return irem(x, m), m
  else
    x1, m1 = eqs[0]
    x2, m2 = eqs[1]
    gcd, c, d = ext_gcd(m1, m2)
    raise ArgumentError.new("Base not relatively prime! ") if gcd != 1
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
    if irem(n, i) == 0
      j = iquot(n, i)
      res.append(i, -i, j, -j)
    end
  end
  return res.sort
end

# base replace
## change base to b
def base_rep(n, b)
  n == 0 ? [] : [irem(n, b)] + base_rep(iquot(n, b), b)
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

def frac(a, b)
  d = gcd(a, b)
  sgn = d > 0 ? 1 : -1
  return [sgn * a / d, sgn * b / d]
end

class RationalNumber
  def initialize(numberator, demoinator)
    @numberator, @denoinator = *frac(numberator, demoinator)
  end
end