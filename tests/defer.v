use io from lib

func other(n: i32) do
  defer println('${int}', 1)

  if n == 3 do
    defer println('${int}', 2)
    println('c3')
  end

  if n == 6 do
    defer println('${int}', 3)
    println('c6')
    return
  end

  return
end

func main do
  n: i32 = atoi(read(2))
  other(n)
end
