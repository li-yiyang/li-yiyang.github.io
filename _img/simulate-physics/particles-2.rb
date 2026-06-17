require "gosu"

DELTA       = 0.000_000_000_000_1
G           = 1
HEIGHT      = 400
WIDTH       = 800
AREA_RIGHT  = 390
AREA_LEFT   = 10
AREA_TOP    = 390
AREA_BOTTOM = 10
$dt = 0.1

# in this basic 2-dimensional particle class, 
# particles has two degree of freedom
class Particle
  attr_accessor :x, :y, :vx, :vy

  # position: x, y; velocity: vx, vy
  # energy: to other object
  def initialize(x, y, vx, vy, 
      energy = -> (x, y) {
        if (x - @x < 2 * DELTA || x - @x > 2 * DELTA) && (y - @y < 2 * DELTA || y - @y > 2 * DELTA)
          0
        else
          r = 1000 * @size / Math.sqrt((x - @x)**2 + (y - @y)**2);
          r ** 12 - r ** 6 # Lennard-Jones potential
        end
      },
      style = {})
    # position, velocity, energy
    # I should consider general p, q later. 
    @x, @y, @vx, @vy, @energy = x, y, vx, vy, energy

    @color = style[:color] || Gosu::Color::YELLOW
    @size  = style[:size]  || 5

    # evaluate paramters
    @t     = style[:dt]    || $dt
    @@dx   = style[:dx]    || 2
    @mass  = style[:mass]  || 1
  end

  def energy(*freedom)
    @energy[*freedom]
  end

  # update the infomation
  def update(env, d = DELTA)
    if @vx * $dt > @@dx || @vy * $dt > @@dx
      dt1, dt2 = @@dx / @vx, @@dx / @vy
      return false, (dt1 < dt2 ? dt1 : dt2) 
    end
    @x, @y = @x + @vx * $dt, @y + @vy * $dt
    env.each do |obj|
      @vx += -((obj.energy(@x + d, @y) - obj.energy(@x, @y)) / (d * @mass)) * $dt
      @vy += -((obj.energy(@x, @y + d) - obj.energy(@x, @y)) / (d * @mass)) * $dt
    end
    return true, @t
  end

  def draw(window)
    window.draw_rect(@x, @y, @size, @size, @color)
  end
end

class Field
  def initialize(energy = -> (x, y) {
    - G * y + 
    (x > AREA_RIGHT ? (x - AREA_RIGHT) ** 4 : 
      (x < AREA_LEFT ? (AREA_LEFT - x) ** 4 : 0)) + 
    (y > AREA_TOP ? (y - AREA_TOP) ** 4 : 
      (y < AREA_BOTTOM ? (AREA_BOTTOM - y) ** 4 : 0))
  })
    @energy = energy
  end

  def update(env); return true, $dt; end

  def vx; 0; end
  
  def vy; 0; end
  
  def draw(window); end

  def energy(*freedom)
    @energy[*freedom]
  end
end

class Simulator < Gosu::Window
  def initialize
    super WIDTH, HEIGHT
    self.caption = "经典粒子的重力场分布"
    @font = Gosu::Font.new(20)
    @env = [Field.new]
    100.times do
      theta = 2 * Math::PI * rand
      @env << Particle.new(rand(AREA_LEFT..AREA_RIGHT), rand(AREA_BOTTOM..AREA_TOP), 
                           10 * Math.cos(theta), 10 * Math.sin(theta))
    end
  end
  
  def update
    switch= true
    @env.each do |obj|
      s, t = obj.update(@env)
      t = t < 0 ? -t : t
      switch &= s
      $dt = $dt > t ? t : $dt
    end
    @env.each { |obj| obj.update(@env) } unless switch
  end

  def draw
    @env.each { |obj| obj.draw(self) }
    v = 0
    rem = 0
    @env.each do |obj|
      v2 = obj.vx**2 + obj.vy**2
      rem += v2
      v += Math.sqrt(v2)
    end
    @font.draw_text("dt: #{$dt}\naverage v: #{v / @env.length}\nv: #{Math.sqrt(rem / @env.length)}", WIDTH / 2, 10, 1)
  end
end

Simulator.new.show