require 'tk'
root = TkRoot.new { title 'Citadel Bombardment'}

# contants
CANVAS_WIDTH = 600
CANVAS_HEIGHT = 400
TARGET_X = 510
TARGET_Y = 0
TARGET_WIDTH = 60
TARGET_HEIGHT = 5
CANNON_X = 20
CANNON_Y = 20
CANNON_LENGTH = 40
CANNON_HEIGHT = 15
DELTA_TIME = 0.15
G = 10
MESSAGES = {greeting: 'Shoot your goo, my dude', in_flight: '*screams and terror*',
  fail: 'Come on, man', success: 'I-it\'s in ❤︎'}

# misc vars
$target = nil
$cannon = nil
$ball = nil
$has_ball_fallen = false
$t = 0

# creating widgets for the game
content = TkFrame.new(root)
velocity_label = TkLabel.new(content) {text 'Init velocity'}
$v0 = TkVariable.new(0.0)
velocity_entry = TkEntry.new(content) {textvariable $v0}
angle_label = TkLabel.new(content) {text 'Angle'}
$alpha = TkVariable.new(0.0)
angle_entry = TkEntry.new(content) {textvariable $alpha}
fire_button = TkButton.new(content) {text 'Fire'}
fire_button.bind("1") {fire}
$canvas = TkCanvas.new(content) {width CANVAS_WIDTH; height CANVAS_HEIGHT; background 'gray'}
$message_label = TkLabel.new(content) {text ''}

# placing widgets on a grid
content.grid :column => 0, :row => 0
$canvas.grid :column => 0, :row => 0, :columnspan => 3
velocity_label.grid :column => 0, :row => 1, :sticky => 'w'
velocity_entry.grid :column => 0, :row => 2, :sticky => 'w'
angle_label.grid :column => 1, :row => 1
angle_entry.grid :column => 1, :row => 2
fire_button.grid :column => 2, :row => 1, :rowspan => 2
$message_label.grid :column => 0, :row => 3, :columnspan => 3

def fire
  erase_cannon
  draw_cannon
  draw_ball
  $t = 0
  $has_ball_fallen = false
  $message_label['text'] = MESSAGES[:in_flight]
  while not $has_ball_fallen
    $t += DELTA_TIME
    pos = ball_pos
    if pos[1] < 0
      $has_ball_fallen = true
      if pos[0] >= TARGET_X and pos[0] <= TARGET_X + TARGET_WIDTH
        $message_label['text'] = MESSAGES[:success]
      else
        $message_label['text'] = MESSAGES[:fail]
      end
    end
    pos = [pos[0], [pos[1], 0].max]
    pos = fix_coords(pos)
    $canvas.move_to($ball, pos[0], pos[1])
    Tk.update
    sleep(DELTA_TIME/4)
  end
end

def draw_target
  $target = TkcRectangle.new($canvas, TARGET_X,
   CANVAS_HEIGHT - TARGET_Y, TARGET_X+TARGET_WIDTH,
   CANVAS_HEIGHT - (TARGET_Y + TARGET_HEIGHT), :fill => 'red')
end

def draw_wheels
  $wheels = TkcOval.new($canvas, CANNON_X,
  	CANVAS_HEIGHT - CANNON_Y, 2*CANNON_X,
  	CANVAS_HEIGHT, :fill => 'black')
  $canvas.move($wheels, -10, 0)
end

def rotate(x, y, angle)
  radians = degree_to_radians(angle)
  new_x = x*Math.cos(angle_in_radians) - y*Math.sin(radians)
  new_y = x*Math.sin(angle_in_radians) + y*Math.cos(radians)
  return [new_x, new_y]
end

def degree_to_radians(angle)
  return angle * Math::PI / 180
end

def fix_coords(xy)
  x = xy[0]
  y = CANVAS_HEIGHT - xy[1]
  return [x,y]
end

def draw_cannon
  br_x = CANNON_LENGTH * Math.cos(degree_to_radians($alpha)) + CANNON_X
  br_y = CANNON_LENGTH * Math.sin(degree_to_radians($alpha)) + CANNON_Y
  b = CANNON_HEIGHT * Math.sin(degree_to_radians(90 - $alpha))
  a = CANNON_HEIGHT * Math.cos(degree_to_radians(90 - $alpha))
  tr_x = br_x - a
  tr_y = br_y + b
  dx = tr_x - br_x
  dy = tr_y - br_y
  tl_x = CANNON_X + dx
  tl_y = CANNON_Y + dy
  coords = []
  [[CANNON_X, CANNON_Y], [br_x, br_y], [tr_x, tr_y], [tl_x, tl_y]].each do |xy|
    coords << fix_coords(xy)
  end
  $cannon = TkcPolygon.new($canvas, coords.flatten, :fill => 'black')
end

def erase_cannon
  $canvas.delete($cannon)
end

def draw_ball
  $ball = TkcOval.new($canvas, CANNON_X,
    CANVAS_HEIGHT - CANNON_Y, CANNON_X + CANNON_X / 2,
    CANVAS_HEIGHT - CANNON_X / 2, :fill => 'black')
  $canvas.move($ball, 10, -13)
end

def ball_pos
  # formula: x(t) = v0 * t * cos(alpha)
  #          y(t) = v0 * t * sin(alpha) - g*t^2 / 2
  x = $v0.to_f * $t * Math.cos(degree_to_radians($alpha.to_f))
  y = $v0.to_f * $t * Math.sin(degree_to_radians($alpha.to_f)) - 10*($t**2)/2
  return [x, y]
end


draw_target
draw_wheels
draw_cannon

Tk.mainloop