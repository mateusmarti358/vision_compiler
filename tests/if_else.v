use io from lib

func main do
  x: i32 = atoi(read(2))
  if x < 10 do
    println("x < 10")
  else
    println("x >= 10")
  end
end
