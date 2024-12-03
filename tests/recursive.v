use io from lib

func sum_even(i: i32): i32 do
  if i <= 1 do
    return = 0
  end

  if i % 2 == 0 do
    return = i + sum_even(i - 1)
  end

  return = sum_even(i - 1)
end

func main do
  j: i32 = sum_even(10)
  print("${int}", j)
end
