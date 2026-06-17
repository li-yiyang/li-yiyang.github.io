require "gosu"

G = 0.05
E = 0.000_000_000_1

INI_POS = 300
INI_PHI = 0
INI_D = 50
INI_H = 90
MASS = 1000
BOARDER = 450

K = 0.01

class Cylinder
  def initialize(x = 300, phi = INI_PHI, d = INI_D, h = INI_H, m = MASS)
    @x = x
    @pos, @phi = INI_POS, phi
    @d, @h, @m, @i = d, h, m, m * (3 * d ** 2 + 4 * h ** 2) / 24
    @r, @alpha = Math.sqrt(@h ** 2 + @d ** 2) / 2, Math.atan(@h / @d)
    @v, @w = 0, 0
  end

  def energy(pos, phi)
    dy1, dy2 = 
      @r * Math.cos(@alpha + phi),
      @r * Math.cos(@alpha - phi)

    e = 0
    [pos + dy1, pos - dy1, pos + dy2, pos - dy2].each do |y|
      e += y > BOARDER ? (Math.exp(y - BOARDER) - E * y * G * @m / 4) : - y * G * @m / 4
    end

    return e
  end

  def update
    @pos += @v
    @phi += @w

    d = 0.000_000_000_000_1
    @v += -(energy(@pos + d, @phi) - energy(@pos, @phi)) / (d * @m) - K * @v
    @w += -(energy(@pos, @phi + d) - energy(@pos, @phi)) / (d * @i) - K * @w
  end

  def color(id)
    [Gosu::Color::RED, Gosu::Color::YELLOW][id]
  end

  def draw
    dx1, dx2, dy1, dy2 = 
      @r * Math.sin(@alpha + @phi),
      @r * Math.sin(@alpha - @phi),
      @r * Math.cos(@alpha + @phi),
      @r * Math.cos(@alpha - @phi)

    return @x + dx1, @pos - dy1, color(1),
           @x + dx2, @pos + dy2, color(1),
           @x - dx1, @pos + dy1, color(0),
           @x - dx2, @pos - dy2, color(0)
  end
end

class Simulator < Gosu::Window
  def initialize
    super 600, 800

    @cylinders = [
      Cylinder.new(300, rand * (Math::PI / 2))
    ]
  end

  def button_down(id)
    @cylinders << Cylinder.new(self.mouse_x, rand * (Math::PI / 2)) if id == Gosu::MS_LEFT
  end

  def update
    @cylinders.map { |cylinder| cylinder.update }
  end

  def draw
    @cylinders.map { |cylinder| draw_quad(*(cylinder.draw)) }
  end
end

Simulator.new.show