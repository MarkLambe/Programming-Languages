# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = Piece::All_Pieces.concat([
    rotations([[0,0],[0,-1],[1,0],[1,-1],[1,1]]),
    [[[0, 0], [-1, 0],[-2, 0], [1, 0], [2, 0]], # long (only needs two)
    [[0, 0], [0, -1], [0, -2], [0, 1], [0, 2]]],
    rotations([[0,0],[0,1],[1,0]])
    ])

  Cheat_Piece = [[[0,0]]]
  # your enhancements here


  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheat_piece (board)
    MyPiece.new(Cheat_Piece, board)
  end

end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    super(game)
    @current_block = MyPiece.next_piece(self)
    @cheating_next = false
    @cost_of_cheat = 100
  end

  def cheating_next
    @cheating_next
  end

  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def next_piece
    if @cheating_next
        @current_block = MyPiece.cheat_piece(self)
        @cheating_next = false
    else
        @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  def cheat
    if game_over? or @score < 100 or @cheating_next
        return
    end
    @cheating_next = true
    @score -= @cost_of_cheat
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0...locations.size).each do |index|
      current = locations[index]
      @grid[current[1] + displacement[1]][current[0] + displacement[0]] = 
        @current_pos[index]
    end
    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

class MyTetris < Tetris
  # your enhancements here
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings 
    super
    @root.bind('u', proc {@board.rotate_180}) 
    @root.bind('c', proc {@board.cheat}) 
  end

end


