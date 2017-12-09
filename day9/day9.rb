line = File.read("input")

garbage = line.gsub!(/\!.{1}/, '').gsub!(/\<([^\>]*)\>/).inject(0) { |v,_| v += $+.length }
groups = Array.new
val = 0
line.chars.each do |char|
  if char == '{'
    groups.push("{")
  elsif char == '}'
    val += groups.length
    groups.pop()
  end
end
puts "The total value of the groups is #{val}."
puts "The total garbage count is #{garbage}."
