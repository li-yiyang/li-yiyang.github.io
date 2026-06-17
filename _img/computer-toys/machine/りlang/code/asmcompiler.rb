# env = {
#   arg: [:arg_name],
#   var: [:var_name],
#   fun: {
#     func_name: [:func_arg]
#   },
#   asm: ["MOV 0, AX"]
#   subenv: {
#     func_name: {
#       # 同 env
#     }
#   }
# }

def make_env(arg = [])
  return env = {
    arg: arg,
    var: [],
    fun: {},
    asm: [], 
    subenv: {}
  }
end

def output_env(env)
  puts env[:asm]
  puts "HALT"
  env[:subenv].each do |name, subenv|
    output_env(subenv)
  end
end

def list_process(env, list)
  head, body = list[0], list[1..]
  process(env, head, body)
end

# 每一个process完了之后, 表达式的值都储存在AX里面
def process(env, head, body)
  case head
  # 四则运算
  when /^(add|sub|mul|div)$/
    compile_calculate(env, head, body)
  # 差点忘了逻辑运算
  when /^(less|greater|equal)$/
    compile_compare(env, head, body[0], body[1])
  # 代码块, 算了以后还是改成叫做block吧
  # 啊, 还是不改了, 以后假如会重构的话再改吧, 
  # 毕竟我觉得这个很麻烦
  when :proc
    compile_proc(env, body)
  # 条件判断
  when :if
    compile_if(env, head, body[0], body[1], body[2])
  # 定义函数, 放弃和赋值共用了
  when :def
    pat, process = body[0], body[1]
    compile_def(env, pat, process)
  # 赋值
  when :let
    name, value, check = body
    # 单赋值啦
    raise "Compile Warning" if check
    compile_let(env, name, value)
  # 处理函数调用
  else
    if env[:fun].include? head
      compile_func_call(env, head, body)
    elsif offset = env[:arg].index(head)
      env[:asm] << "MOV M[BP + #{env[:arg].length - offset + 2}], AX"
    elsif offset = env[:var].index(head)
      env[:asm] << "MOV M[SP + CX + #{offset}], AX"
    else
      raise "Compile Error"
    end
  end
end

# 处理四则运算
# 这个未来的想法是将那些可以提前计算的东西全部计算掉, 
# 比如在代码里面出现的字面量的计算就应该给去掉
# 这样会让编译出来的代码短一点点
# 不过我觉得挺无所谓的
# 不过效率还是比较重要的啦
# 坏了, 因为引入了栈的概念, 导致了使用变量的时候, 
# 就会让变量调用错误, 害, 自己把自己给坑了, 
# 现在的想法是用 CX 来配合栈来储存
#
# 又又又坏了, 因为 CX 的引入, 导致了在函数调用的时候, 
# 由于栈的使用, 于是导致了悲剧. 就是原本的 CX 会因为函数栈, 
# 导致偏移量出现问题, 并且这个还有可能会导致被覆盖写错的问题
# 所以现在考虑是直接PUSH和POP, 然后在利用CX作为当前的栈的偏移. 
# 然后在函数调用的时候, 要将CX保存起来
def compile_calculate(env, head, body)
  # body.sort_by! { |atom| compile_calculate_order(atom) }
  i = 0
  body.each do |atom|
    if atom.is_a? Array
      # env[:asm] << "INC CX" << "MOV DX, M[SP - CX]" unless i == 0
      env[:asm] << "PUSH DX" << "INC CX" unless i == 0
      list_process(env, atom)
      # env[:asm] << "MOV M[SP - CX], DX" << "DEC CX" unless i == 0
      env[:asm] << "POP DX" << "DEC CX" unless i == 0
    elsif atom.is_a? Symbol
      if (offset = env[:arg].index atom)
        env[:asm] << "MOV M[BP + #{env[:arg].length - offset + 2}], AX"
      elsif (offset = env[:var].index atom)
        env[:asm] << "MOV M[SP + CX + #{offset}], AX"
      else
        raise "Compile Error"
      end
    elsif atom.is_a? IntegerNumber
      env[:asm] << "MOV #{atom}, AX"
    else
      raise "Compile Error"
    end
    if i == 0
      env[:asm] << "MOV AX, DX"
    else
      env[:asm] << "#{head.to_s.upcase} AX, DX"
    end
    i += 1
  end
  env[:asm] << "MOV DX, AX"
end

# 用于四则运算的一个顺序表
# 由于引入了栈的概念, 所以不需要使用这样的操作了
# def compile_calculate_order(list)
#   if list.is_a? Array
#     return 3
#   elsif list.is_a? Symbol
#     return 2
#   elsif list.is_a? Integer
#     return 1
#   else
#     return 0
#   end
# end

def compile_compare(env, head, a, b)
  case head
  when :less
    list_process(env, [:sub, a, b])
  when :greater
    list_process(env, [:sub, b, a])
  when :equal
  end
end

# 处理代码块
def compile_proc(env, procs)
  procs.each do |p|
    list_process(env, p)
  end
  # 现在的想法是让函数编译的部分直接来找这个函数
  # 然后让这些代码都由函数编译的部分来处理
  # proc_asm = []
  # # 处理函数的参数调用
  # if env[:arg].length > 0
  #   proc_asm << "MOV SP, BP"
  # end
  # 处理局部变量
  proc_asm = []
  proc_asm << "SUB #{env[:var].length}, SP" if env[:var].length > 0
  env[:asm] = proc_asm + env[:asm]
end

# 跳转的标签
$ifTAG = "ZZZ"

# 处理条件判断
def compile_if(env, head, condition, true_bench, false_bench)
  list_process(env, condition)
  env[:asm] << "CMP AX, 0"
  # 跳转到true bench
  env[:asm] << "JL #{$ifTAG = $ifTAG.succ}"
  # false bench
  list_process(env, false_bench)
  # 跳转到条件判断结束
  env[:asm] << "JMP #{$ifTAG.succ}"
  # true bench
  env[:asm] << "// #{$ifTAG}"
  list_process(env, true_bench)
  env[:asm] << "// #{$ifTAG = $ifTAG.succ}"
end

# 处理函数定义
def compile_def(env, pat, process)
  func_name, func_arg = pat[0], pat[1..]
  env[:fun][func_name] = func_arg
  env[:subenv][func_name] = make_env(func_arg)
  env[:subenv][func_name][:fun][func_name] = env[:fun][func_name].dup
  env[:subenv][func_name][:asm] << "// #{func_name.to_s.upcase}"
  env[:subenv][func_name][:asm] << "MOV SP, BP"
  compile_proc(env[:subenv][func_name], [process])
  env[:subenv][func_name][:asm] << "MOV BP, SP"
  env[:subenv][func_name][:asm] << "RET"
end

def compile_func_call(env, func_name, func_arg)
  raise "Wrong Arg!" if env[:fun][func_name].length != func_arg.length
  func_arg.each do |arg|
    if arg.is_a? Array
      list_process(env, arg)
    elsif arg.is_a? Symbol
      # 取值的时候都是优先函数参数, 
      # 其次是外围变量
      if offset = env[:arg].index(arg)
        env[:asm] << "MOV M[BP + #{env[:arg].length - offset + 2}], AX"
      elsif offset = env[:var].index(arg)
        env[:asm] << "MOV M[SP + CX + #{offset}], AX"
      end
    elsif arg.is_a? IntegerNumber
      env[:asm] << "MOV #{arg}, AX"
    else
      raise "Compile Error"
    end
    env[:asm] << "PUSH AX"
  end
  # 调用前先把 CX 给保存了
  env[:asm] << "PUSH CX"
  env[:asm] << "MOV 0 CX"
  # 嗯, 还要把 BP 给保存了, 防止多重栈的时候跳不回去
  env[:asm] << "PUSH BP"
  env[:asm] << "CALL #{func_name.to_s.upcase}"
  # 把 BP 给保存了
  env[:asm] << "POP BP"
  # 调用后再把 CX 给复位了
  env[:asm] << "POP CX"
  # 然后还要把栈 SP 的还原
  env[:asm] << "ADD #{func_arg.length}, SP"
end

# 赋值
# 是否考虑多赋值? 不, 我不打算坑自己, 所以单赋值, 
# 因为我觉得如果多赋值了的话, 估计就可以实现函数调用了
# 而函数调用实在是太难写了
def compile_let(env, name, value)
  env[:var] << name unless (off = env[:var].index(name))
  if value.is_a? Array
    list_process(env, value)
  elsif value.is_a? Symbol
    if offset = env[:arg].index(value)
      env[:asm] << "MOV M[BP + #{env[:arg].length - offset + 2}], AX"
    elsif offset = env[:var].index(value)
      env[:asm] << "MOV M[SP + CX + #{offset}], AX"
    else
      raise "Compile Error"
    end
  elsif value.is_a? IntegerNumber
    env[:asm] << "MOV #{value}, AX"
  end
  env[:asm] << "MOV AX, M[SP + #{off || env[:var].length - 1}]"
end
