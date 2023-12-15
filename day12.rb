class Memoised
    def initialize (&b)
        @func = b
        @memo = {}
    end
    def of (*args)
        if not (@memo.key? args) then
            @memo[args] = @func.call *args
        end
        return @memo[args]
    end
end

Make_regexp = Memoised.new do |i|
    Regexp.new ("^" + ("[#\\?]" * i) + "[\\?\\.]")
end

def maybe_consume (s, amount)
    if s.length < amount then
        s
    elsif s.length == amount and s.chars.all? { |c| "#?".include? c } then
        ""
    else
        res = s.sub (Make_regexp.of amount), "."
        res
    end
end

solver = Memoised.new do | s, vals |
    if s == nil then
        vals.empty? ? 1 : 0
    elsif vals.empty? then
        (s.chars.all? { |c| ".?".include? c }) ? 1 : 0
    else
        s = s.sub /\.*/, ""
        if s.start_with? "?" then
            s = s.delete_prefix "?"
            (solver.of "." + s, vals) + (solver.of "#" + s, vals)
        elsif s.start_with? "#" then
            new_s = maybe_consume s, vals.first
            if new_s == s then
                0
            else
                solver.of new_s, (vals.drop 1)
            end
        else
            # s consists only of ...s, but vals != []
            # (or, invalid character)
            0
        end
    end
end

input_file = open "input12.txt", "r"
lines = input_file.readlines.map &:chomp
input_file.close

# Part 1
puts (lines.map do | line |
    s, vals = line.split " "
    solver.of s, ((vals.split ",").map &:to_i)
end).sum

# Part 2
# Takes ~40s
puts (lines.map do | line |
    s, vals = line.split " "
    big_s = (Array.new 5, s).join "?"
    big_vals = (Array.new 5, vals).join ","
    solver.of big_s, ((big_vals.split ",").map &:to_i)
end).sum
