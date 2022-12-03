s = open("input02.txt") do file
    readlines(file)
end

outcomes1 = Dict("A X" => 4,"A Y" => 8,"A Z" => 3,
"B X" => 1,"B Y" => 5,"B Z" => 9,
"C X" => 7,"C Y" => 2,"C Z" => 6)
sum([outcomes1[k] for k in s])
# 9651

outcomes2 = Dict("A X" => 3,"A Y" => 4,"A Z" => 8,
"B X" => 1,"B Y" => 5,"B Z" => 9,
"C X" => 2,"C Y" => 6,"C Z" => 7)
sum([outcomes2[k] for k in s])
# 10560
