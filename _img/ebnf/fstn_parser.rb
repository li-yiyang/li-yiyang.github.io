class FSTN
  attr_reader :ast
  
  SYMBOL = /[^\s,.:]+/
  COMMA = /\s*,\s*/
  ARRAY = /(#{SYMBOL}#{COMMA})+#{SYMBOL}/
  
  NODE = /\s*(Initial|Final)\s+(#{SYMBOL}|#{ARRAY})\s*/
  ARC = /\s*From\s+(#{SYMBOL})\s+to\s+(#{SYMBOL})\s+by\s+(#{SYMBOL})\s*/
  NETWORK_LHS = /Name\s+(#{SYMBOL})\s*:\s*/
  NETWORK_DES = /#{NODE}|#{ARC}/
  
  NETWORK = /\s*#{NETWORK_LHS}(#{NETWORK_DES})+\./
  ABBREVIATE = /\s*(#{SYMBOL})\s+abbreviates\s*:\s*(#{ARRAY})\s*\./
  
  FSTN_RULES = /(#{NETWORK}|#{ABBREVIATE})+/
  
  def initialize(input, options = {})
    @ast = to_ast(input)
    @options = options

    @options[:shape]   ||= :circle
    @options[:start]   ||= :point
    @options[:layout]  ||= :dot
    @options[:rankdir] ||= :LR
    @options[:org]     ||= true
    @options[:fstn]   ||= nil
    @options[:join]    ||= " "
  end

  def read(input)
    @ast = to_ast(input)
  end

  def generate(options = {})
    generate_ast(@ast, **options)
  end

  def generate_by(input, options = {})
    generate_ast(to_ast(input), **options)
  end
  
  def draw(options = {})
    draw_ast(@ast, **options)
  end

  def draw_by(input, options = {})
    draw_ast(to_ast(input), **options)
  end

  def inspect
    @ast.to_s
  end

  private

  def generate_ast(ast, options = {})
    fstn = options[:fstn] || @options[:fstn]
    join  = options[:join]  || @options[:join]
    
    network = fstn.nil? ? ast[:network].values.first : ast[:network][fstn]
    state = "qi"
    res = []

    while state != "qa"
      choose = network[state][rand(network[state].length)]
      char = ast[:abbreviate][choose[:arc]] || choose[:arc]
      res << char if choose[:arc] != "#"
      state = choose[:to]
    end

    res.join(join)
  end

  def draw_ast(ast, options = {})
    options = @options.merge(options)
    res = "layout = #{options[:layout]};\n" +
          "rankdir = #{options[:rankdir]};\n" +
          "node [shape = #{options[:start]}]; qi, qa;\n" +
          "node [shape = #{options[:shape]}];\n"
    
    # TODO: for multi-fstn
    ast[:network].each do |name, arcs|
      arcs.each do |from, arc|
        arc.each do |tos|
          by, to = tos[:arc], tos[:to]
          res << ("#{from} -> #{to}" +
                  (by == "#" ? ";\n" : " [label = \"#{by}\"];\n"))
        end
      end
    end

    options[:org_mode] ? res : "digraph {\n #{res} }"
  end
  
  def to_ast(input)
    raise "No matched FSTN Rules." unless input.match(FSTN_RULES)
    
    network = read_network(input)
    abbreviate = read_abbreviate(input)

    { network: network, abbreviate: abbreviate }
  end
  
  def read_network(input)
    input.gsub(NETWORK).map do |network|
      name = network.match(NETWORK_LHS)[1]
      arcs = {}
      network.gsub(NETWORK_DES).map do |des|
        if m = des.match(NODE)
          type, nodes = m[1], m[2].split(COMMA)
          nodes.map do |node|
            type == "Initial" ? { arc: "#", from: "qi", to:  node } :
              { arc: "#", from: node, to: "qa" }
          end
        elsif m = des.match(ARC)
          from, to, by = m[1], m[2], m[3]
          { arc: by, from: from, to: to }
        end
      end.flatten.each do |arc|
        arcs[arc[:from]] ||= []
        arcs[arc[:from]] << { arc: arc[:arc], to: arc[:to] }
      end
      { name => arcs }
    end.inject(:merge)
  end

  def read_abbreviate(input)
    input.gsub(ABBREVIATE).map do |abbreviate|
      m = abbreviate.match(ABBREVIATE)
      name, array = m[1], m[2].split(COMMA)
      { name => array }
    end.inject(:merge)
  end
end
