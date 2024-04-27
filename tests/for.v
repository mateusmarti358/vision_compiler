use io from lib

func main do
  sum: float = 0
  
  for i in 1.2..10.2 do
    sum = sum + i
  end

  println('%f', sum)
end