open("input01.txt","r") do f
    global n = 0
    global calories = []
    while !eof(f)
        s = readline(f)
        if length(s)==0
            append!(calories,n)
            n = 0
        else
            n += parse(Int64,s)
        end
    end
end

append!(calories,n)

maximum(calories)
#69528

sum(sort(calories)[(end-2):end])
#206152