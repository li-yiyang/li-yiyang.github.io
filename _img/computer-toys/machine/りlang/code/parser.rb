# 编译器
# 首先设计一个语法规范: 
# 为了方便我, 所以我打算参考Lisp的括号语法来制作我的玩具, 
# 但是因为我不是很想要做一个Lisp的方言, 
# 所以语言的功能实现我不会采用Lisp的做法
#
# 现在就只是简单的四则运算罢了
# (+ a b c d ...) # a + b + c + d + ...
# (- a b c d ...) # a - b - c - d - ...
# 其他同理
#
# 重新设计语法: 
#
# [x] 四则运算: 
# (add 1 2 3 (sub 9 8 -3))
#
# 赋值
# (let x 1)
# (let x (add 2 3))
#
# 定义函数
# (def (f x y)
#   (add x y))
# (def (f x y)
#   (proc (def z (add x y))
#         (def t (sub x y))
#         (mul z t)))
#
# 过程, 或者叫做代码块
# (proc (f a b)
#       (f b c))
#
# 条件
# (if (condition)
#     (true)
#     (false))
#
# 函数调用
# (f arg1 arg2)
#
# 列表 # 第一版还是不实现了, 有点麻烦, 
#      # 不知道能不能实现动态数组, 
#      # 因为万一到时候做出了什么错误
#      # 把自己给pwn了就不好了
# (list a b c d e)
#
# 列表操作
# (read (list a b c) 0)

def literal(item)
  case item
  when ""
    return ""
  when /^-{0,1}\d+$/
    return item.to_i
  when /^-{0,1}0x[0-9a-fA-F]+$/
    return item.to_i(16)
  when /^-{0,1}0b[01]+$/
    return item.to_i(2)
  when /^-{0,1}0o[0-7]+$/
    return item.to_i(8)
  else
    return item.to_sym
  end
end

def parser(code, head = 0)
  main = head
  struct = []
  item = ""
  ignore = :none
  while head < code.length
    chr = code[head]
    case ignore
    when :none
      case chr
      when "\\"
        ignore = :next
      when "\""
        ignore = :string
      when "("
        item = literal(item)
        struct << item unless item == ""
        temp_struct, head = parser(code, head + 1)
        struct << temp_struct
      when ")"
        item = literal(item)
        struct << item unless item == ""
        return struct, head
      when " "
        item = literal(item)
        struct << item unless item == ""
        item = ""
      else
        item += chr
      end
    when :next
      ignore = :none
      item += chr
    # 将字符串看作是字符的数值的列表
    when :string
      if chr == "\""
        a = []
        item.each_char do |c|
          a << c.ord
        end
        item = [:list] + a
        struct << item
        item = ""
        ignore = :none
      # 应该要增加对转义字符的支持
      else
        item += chr
      end
    end
    head += 1
  end
  if main == 0
    return [:proc] + struct
  else
    return struct, head
  end
end

# 需要一点点修改, 但是至少可以使用了
# 见test.rb
def evaluate(atom, environment)
  # local_enviroment = {}
  raise "no func" unless func = environment[atom[0]]
  arg = []
  atom[1..].each do |item|
    if item.is_a? Symbol
      raise "no name" unless value = environment[item]
      if value.is_a? Array
        arg << evaluate(value)
      else
        arg << value
      end
    elsif item.is_a? Array
      arg << evaluate(item, environment)
    else
      arg << item
    end
  end
  return func[*arg]
end

