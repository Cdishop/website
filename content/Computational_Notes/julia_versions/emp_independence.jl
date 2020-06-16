using DataFrames,
      DataFramesMeta,
      Query,
      Statistics,
      Random,
      Distributions,
      PrettyTables

main_course = [
    "chicken",
    "salmon",
    "pork",
    "chicken",
    "pancakes",
    "french toast",
]
side = ["salad", "salad", "green beans", "corn", "carrots", "bacon"]

# what is the empirical estimate of p(dinner | chicken)?
# p(chicken & dinner) / p(chicken)
# should be close to 0.83333


sims = 10000
df = DataFrame(chicken_and_dinner = [], chicken = [])


for i = 1:sims
    eat_main = sample(main_course, 1, replace = false)[1]
    eat_side = sample(side, 1, replace = false)[1]

    chicken_and_dinner = false

    if eat_main == "chicken" && (eat_side == "salad" ||
        eat_side == "green beans" ||
        eat_side == "corn" || eat_side == "carrots")
        chicken_and_dinner = true
    end

    chicken = false
    if eat_main == "chicken"
        chicken = true
    end

    single_run_result = [chicken_and_dinner, chicken]
    push!(df, single_run_result)

end

tally_chicken_and_dinner = count(j == 1 for j in df.chicken_and_dinner)
tally_chicken = count(k == 1 for k in df.chicken)

prob_chicken_and_dinner = tally_chicken_and_dinner / sims
prob_chicken = tally_chicken / sims

result = prob_chicken_and_dinner / prob_chicken
println(result) # remember that it should be close to 0.83
