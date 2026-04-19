require "opal"
require "opal-parser"
require "opal/jquery"

m = Machine.new("MOV 0, R0")
c = Compiler.new("")

def m.pc; @register[:PC]; end

def output(loaded_code, m, mode = :memory)
  loaded_code.text = ""
  m.output(mode).each do |line|
    loaded_code << (Element.new("div") << line)
  end
end

Document.ready? do
  load_button = Element.find('#load')
  step_button = Element.find('#step')
  memory_button = Element.find('#memory')
  compile_button = Element.find('#compile')
  run_button = Element.find('#run')

  memory_buff = Element.find('#memories')
  # run_button = Element.find('#run')

  code = Element.find('#code')
  loaded_code = Element.find('#loaded_code')
  rilang = Element.find('#rilang')

  run_button.on(:click) do
    loop do
      pc = m.pc
      m.step
      # wait(0.1)

      # sleep(0.1)

      begin
        m.step
      rescue Exception => e
        alert "Invaild Operation! #{e}"
      end
      output(loaded_code, m, :control)
      output(memory_buff, m, :memory)

      break if pc == m.pc
    end
  end

  compile_button.on(:click) do
    if rilang.value == ""
      alert "No Code Could Ne Compiled"
    else
      begin
        c.load(rilang.value)
        code.value = c.compile
      rescue Exception => e
        alert "Compile Error #{e}"
      end
      alert "Success! "
    end
  end

  load_button.on(:click) do
    if code.value == ""
      alert "No Code Could Be Loaded. "
    else
      begin
        m.load_code(code.value)
        output(loaded_code, m, :control)
        output(memory_buff, m, :memory)
        alert "Loaded Successfully. "
      rescue
        alert "LoadError!"
      end
    end
  end

  step_button.on(:click) do
    begin
      m.step
    rescue Exception => e
      alert "Invaild Operation! #{e}"
    end
    output(loaded_code, m, :control)
    output(memory_buff, m, :memory)
  end

  memory_button.on(:click) do
    output(memory_buff, m, :memory)
  end
end
