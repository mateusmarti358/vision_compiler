use io from lib

func fib(n1: i32, n2: i32, max: i32) do
  n3: i32 = n1 + n2
  print('${int} + ${int} = ${int}\n', n1, n2, n3)
  if n3 < max do
    fib(n2, n3, max)
  end
end

func main do
  max: i32 = atoi(read(8))
  fib(1, 1, max)
end
