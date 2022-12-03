s = open("input03.txt") do file
    readlines(file)
end

rucksacks = collect.(s)

n=0
for i in 1:lastindex(rucksacks)
    z = [k for k in rucksacks[i] if k in rucksacks[i][1:end÷2] && k in rucksacks[i][end÷2+1:end]][1]
    if islowercase(z)
        n += z-'a'+1
    else
        n += z-'A'+27
    end
end

n
#8039

n=0
for i in 1:3:lastindex(rucksacks)
    z = [k for k in rucksacks[i] if k in rucksacks[i+1] && k in rucksacks[i+2]][1]
    if islowercase(z)
        n += z-'a'+1
    else
        n += z-'A'+27
    end
end

n
#2510