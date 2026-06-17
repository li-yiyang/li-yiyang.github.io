require "opal"
require "opal-parser"
require "opal/jquery"

m = Machine.new("MOV 0, R0")

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

  memory_buff = Element.find('#memories')
  # run_button = Element.find('#run')

  code = Element.find('#code')
  loaded_code = Element.find('#loaded_code')

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
    rescue
      alert "Invaild Operation! "
    end
    output(loaded_code, m, :control)
    output(memory_buff, m, :memory)
  end

  memory_button.on(:click) do
    output(memory_buff, m, :memory)
  end
end
