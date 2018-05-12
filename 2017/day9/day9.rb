line = File.read("input")
count = 0
val = 0

garbage = line.gsub!(/\!.{1}/, '')
            .gsub!(/\<([^\>]*)\>/)
            .inject(0){ |acc| acc += $+.length }

line.chars.each do |char|
  if char == '{'
    count += 1
  elsif char == '}'
    val += count
    count -= 1
  end
end
puts "The total value of the groups is #{val}."
puts "The total garbage count is #{garbage}."
