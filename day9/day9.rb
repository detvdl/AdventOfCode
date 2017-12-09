line = File.read("input")

cancelled = line.gsub(/\!.{1}/, '')
garb_count = 0
garbage = cancelled.gsub(/\<([^\>]*)\>/) { garb_count += $+.length; '' }
groups = Array.new
val = 0
garbage.chars.each do |char|
  if char == '{'
    groups.push("{")
  elsif char == '}'
    val += groups.length
    groups.pop()
  end
end
puts "The total value of the groups is #{val}."
puts "The total garbage count is #{garb_count}."
