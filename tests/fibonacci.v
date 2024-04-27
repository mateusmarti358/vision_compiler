use io from lib

func fib(n1: int, n2: int, max: int) do
  n3: int = n1 + n2
  print('${int} + ${int} = ${int}\n', n1, n2, n3)
  if n3 < max do
    fib(n2, n3, max)
  end
end

func main do
  max: int = atoi(read(8))
  fib(1, 1, max)
end