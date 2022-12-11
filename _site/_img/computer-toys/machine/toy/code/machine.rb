# 重新实现了虚拟机, 
# 将虚拟机当作是理想机器, 又如下的特性: 
#   * 无限位宽, 不存在溢出问题
#   * 代码和数据是两种类型的东西, 
#     也就是不存在将数据当作代码来执行的问题
#     (其实是不太对的一件事, 之后会想办法解决)

MAX_MEMORY = 1024

# 虚拟机类, 初始化的时候载入代码, 
# 也同时支持利用 +load_code+ 方法来载入代码
# 执行代码分为两种方式, 一种是直接+step+来按步执行内存里面的代码, 
# 一种是通过调用+execute+来直接执行代码. +run+命令会在之后重新加入. 
# 本次重构修改了读写以及对代码的美观程度有了比较好的提升, 
# 对于指令的实现也做了比较友好的优化
class Machine
  def initialize(code, config = {})
    load_config(config)
    load_code(code)
    load_instructions
  end

  # 为了更好地输出结果到网页或者终端上, 
  # 提供一个输出接口
  def output(mode = :control)
    res = []

    if mode == :control
      pc = @register[:PC]
      flags = @register[:FLAGS]
      flags = flags > 0 ? ">" : (flags < 0 ? "<" : "=")

      res << "ASM Code: "
      res << "    #{@memory[pc + 1]}" if pc < @max_memory - 1
      res << " -> #{@memory[pc]}"
      res << "    #{@memory[pc - 1]}"
      res << "Registers: "
      res << "FLAGS: #{flags}  PC: #{pc}"
      res << "AX: #{@register[:AX]} SP: #{@register[:SP]} BP: #{@register[:BP]}"
      res << "R0: #{@register[:R0]} R1: #{@register[:R1]} R2: #{@register[:R2]}"
    elsif mode == :memory
      i = 0
      @memory.each do |m|
        res << "#{i}: #{m}" if m
        i += 1
      end
    end
    
    return res
  end

  # 单步执行
  def step
    execute(@memory[@register[:PC]])
    @register[:PC] -= 1 if @step
    @step = true
  end

  # 运行代码
  def execute(code)
    raise "Invaild Operation! " unless code.is_a? String
    i, s, d = code.gsub(/\[.+\]/){|m| m.gsub(/\s/, "")}.gsub(",", "").split(" ")
    raise "No Instruction! " unless @instructions.include? i.to_sym
    @instructions[i.to_sym].call(s, d)
  end

  # 读数据
  def read(a)
    case a
    # 输入的是一个数字, 
    # 对应的是类似于MOV 2, R0之类的命令
    when /^\d+$/
      return a.to_i 
    # 访问地址的时候
    when /^M\[(.+)\]$/
      exp = Regexp.last_match[1].to_s.gsub(/R0|R1|R2|PC|FLAGS|AX|BP|SP/) do |m|
        @register[m.to_sym]
      end
      # 这个地方又可能会有危险, 因为用到了eval函数
      return @memory[eval(exp)]
    # 读到的是寄存器的时候
    else
      return @register[a.to_sym]
    end
  end

  # 写数据
  # 将能写的寄存器写死了, 防止写入不存在的寄存器
  def write(a, v)
    case a
    when /^M\[(.+)\]$/
      exp = Regexp.last_match[1].to_s.gsub(/R0|R1|R2|PC|FLAGS|AX|BP|SP/) do |m|  
        @register[m.to_sym]  
      end
      @memory[eval(exp)] = v
    else
      @register[a.to_sym] = v
    end
  end

  # 处理一些配置
  def load_config(config)
    # 默认的内存是 1024, 不过对于理想机器来说, 
    # 这个就只是说能够放下 1024 个数量的东西罢了
    @max_memory = config[:max_memory] || MAX_MEMORY
    @step = true
  end

  # 往内存里面载入代码
  def load_code(code)
    # 内存清空, 并设置最大内存
    @memory = Array.new(@max_memory)

    # 为了支持直接用手写汇编的特性, 
    # 提供 +tag+ 跳转的功能

    # 标签
    tag = {}
    # 当前载入虚拟机内存的地址
    load_num = @max_memory - 1
    code.each_line do |l|
      if (i = l.index("//"))
        @memory[load_num] = l[0...i]
        label = l[(i + 2)..].strip.to_sym
        tag[label] = load_num
      else
        @memory[load_num] = strip_tag(l, tag)
      end
      load_num -= 1
    end

    # 寄存器归位
    # 寄存器设计的时候借鉴了 x86, 
    # 将寄存器简化了一点
    # 以后更新寄存器设计的时候, 要记得更新write和read方法
    @register = {
      AX: 0, SP: load_num, BP: load_num, 
      FLAGS: 0, PC: @max_memory - 1, R0: 0, R1: 0, R2: 0
    }
  end

  # 载入指令集
  # 以后考虑去模拟不同类型的指令
  def load_instructions
    @instructions = {
      # 赋值操作
      MOV: -> (a, b) { write(b, read(a)) }, 

      # 四则运算
      # 对于除法进行一个特别说明: 假如除数为零, 被除数也变成零 
      ADD: -> (a, b) { write(b, read(b) + read(a)) }, 
      SUB: -> (a, b) { write(b, read(b) - read(a)) }, 
      MUL: -> (a, b) { write(b, read(b) * read(a)) }, 
      DIV: -> (a, b) { (v_a = read(a)) != 0 ? write(b, read(b) / v_a) : write(b, 0) }, 
      INC: -> (a, b) { @instructions[:ADD]["1", a] }, 
      DEC: -> (a, b) { @instructions[:SUB]["1", a] }, 

      # 控制流
      JMP: -> (a, b) { @register[:PC] = read(a); @step = false },
      CMP: -> (a, b) { @register[:FLAGS] = read(b) - read(a) },
      JL: -> (a, b) { @instructions[:JMP][a, b] if @register[:FLAGS] < 0 },
      JE: -> (a, b) { @instructions[:JMP][a, b] if @register[:FLAGS] = 0 },

      # 栈操作
      PUSH: -> (a, b) { @register[:SP] -= 1; @memory[@register[:SP]] = read(a) },

      # 什么也不做
      HALT: -> (a, b) { @step = false }
    }
  end

  # 处理跳转的标签
  # 这里用的方法就是粗暴地替换, 
  # 所以需要跳转的tag不是保留关键词, 否则会出现bug
  # 这个之后会想办法修正
  def strip_tag(code, tag)
    tag.each do |tag, addr|
      code = code.gsub(tag.to_s, addr.to_s)
    end
    code.strip
  end
end
