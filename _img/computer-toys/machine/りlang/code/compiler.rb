# require_relative "parser"
# require_relative "asmcompiler"

class Compiler
  def initialize(code = ""); @code = code; end
  # 追加载入
  def update(code); @code += code; end
  # 重新载入
  def load(code); @code = code; end

  # 返回一个编译的数组
  def compile_a
    def output(env)
      asm = env[:asm]
      asm << "HALT"
      env[:subenv].each do |name, subenv|
        asm += output(subenv)
      end
      asm
    end
    
    env = make_env
    list_process(env, parser(@code.gsub("\n", " ")))
    output(env)
  end

  # 返回编译的字符串
  def compile; compile_a.join("\n"); end

  # 将编译的结果写入到文件里面
  def compilef(path); File.write(path, compile); end

  # 直接打印输出编译的结果
  def compilet; puts compile; end
end
