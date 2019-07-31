require 'tk'

root = TkRoot.new { title 'Tic Tac Toe'}
$player_symbol = nil
$ai_symbol = nil
$available_cells = 9
$is_game_over = false

# constants
SYMBOLS = {blank: ' ', x: 'X', o: 'O'}
MESSAGES = {blank: ' ', win: 'You won!', lose: 'You lost!', draw: 'Draw!'}
FRAME_WIDTH = 15
BUTTON_WIDTH = FRAME_WIDTH / 3

# creating widgets for the game
content = TkFrame.new(root)
select_label = TkLabel.new(content) {text 'Select Symbol'}
o_button = TkButton.new(content) {text SYMBOLS[:o]; width BUTTON_WIDTH}
o_button.bind("1") {reset; $player_symbol = SYMBOLS[:o]; $ai_symbol = SYMBOLS[:x]}
x_button = TkButton.new(content) {text SYMBOLS[:x]; width BUTTON_WIDTH}
x_button.bind("1") {reset; $player_symbol = SYMBOLS[:x]; $ai_symbol = SYMBOLS[:o]; simulate_ai}
$end_message = TkLabel.new(content) {text MESSAGES[:blank]; height 2}
# t - top, c - center, b - bot, l - left, r - right
tl = TkButton.new(content) {width BUTTON_WIDTH}
tc = TkButton.new(content) {width BUTTON_WIDTH}
tr = TkButton.new(content) {width BUTTON_WIDTH}
cl = TkButton.new(content) {width BUTTON_WIDTH}
cc = TkButton.new(content) {width BUTTON_WIDTH}
cr = TkButton.new(content) {width BUTTON_WIDTH}
bl = TkButton.new(content) {width BUTTON_WIDTH}
bc = TkButton.new(content) {width BUTTON_WIDTH}
br = TkButton.new(content) {width BUTTON_WIDTH}

# placing widgets on a grid
content.grid :column => 0, :row => 0
o_button.grid :column => 0, :row => 0
select_label.grid :column => 1, :row => 0
x_button.grid :column => 2, :row => 0
$end_message.grid :column => 1, :row => 1
tl.grid :column => 0, :row => 2
tc.grid :column => 1, :row => 2
tr.grid :column => 2, :row => 2
cl.grid :column => 0, :row => 3
cc.grid :column => 1, :row => 3
cr.grid :column => 2, :row => 3
bl.grid :column => 0, :row => 4
bc.grid :column => 1, :row => 4
br.grid :column => 2, :row => 4

# placing tic tac toe buttons in an array
$cells = [[tl, tc, tr], [cl, cc, cr], [bl, bc, br]]

def reset
  # clearing cell symbols
  $cells.each do |row|
  	row.each do |cell|
  	  cell['text'] = SYMBOLS[:blank]
  	end
  end
  $available_cells = 9
  $end_message['text'] = MESSAGES[:blank]
  $is_game_over = false
end

def simulate_ai
  flat = $cells.flatten
  has_filled = false
  while !$is_game_over and !has_filled and $available_cells > 0
  	cell = flat.sample
  	if cell['text'] == SYMBOLS[:blank]
  	  cell['text'] = $ai_symbol
  	  has_filled = true
  	  $available_cells -= 1
  	  if has_won?
  	  	$end_message['text'] = MESSAGES[:lose]
  	  	$is_game_over = true
  	  end
  	  if $available_cells == 0
  	    $end_message['text'] = MESSAGES[:draw]
  	    $is_game_over = true
  	  end
  	end
  end
end

def onclick(cell)
  if !$is_game_over and cell['text'] == SYMBOLS[:blank]
  	cell['text'] = $player_symbol
  	$available_cells -= 1
  	if has_won? 
  	  $end_message['text'] = MESSAGES[:win]
  	  $is_game_over = true
  	end
  	if $available_cells == 0
  	  $end_message['text'] = MESSAGES[:draw]
  	  $is_game_over = true
  	end
  	simulate_ai
  end
end

def has_won?
  # diagonals
  d1 = [$cells[0][0]['text'], $cells[1][1]['text'], $cells[2][2]['text']].uniq
  return true if d1.length == 1 and d1[0] != SYMBOLS[:blank] 
  d2 = [$cells[0][2]['text'], $cells[1][1]['text'], $cells[2][0]['text']].uniq
  return true if d2.length == 1 and d2[0] != SYMBOLS[:blank] 
  # verticals
  v1 = ($cells.map {|row| row[0]['text']}).uniq
  return true if v1.length == 1 and v1[0] != SYMBOLS[:blank]
  v2 = ($cells.map {|row| row[1]['text']}).uniq
  return true if v2.length == 1 and v2[0] != SYMBOLS[:blank]
  v3 = ($cells.map {|row| row[2]['text']}).uniq
  return true if v3.length == 1 and v3[0] != SYMBOLS[:blank]
  # horizontals
  h1 = ($cells[0].map {|cell| cell['text']}).uniq
  return true if h1.length == 1 and h1[0] != SYMBOLS[:blank]
  h2 = ($cells[1].map {|cell| cell['text']}).uniq
  return true if h2.length == 1 and h2[0] != SYMBOLS[:blank]
  h3 = ($cells[2].map {|cell| cell['text']}).uniq
  return true if h3.length == 1 and h3[0] != SYMBOLS[:blank]
  return false
end


# binding cells
$cells.each do |row|
  row.each do |cell|
    # "1" means 'onclick'
    cell.bind("1") {onclick(cell)}
  end
end

Tk.mainloop
