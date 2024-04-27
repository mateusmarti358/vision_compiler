use io from lib

enum test
  foo
  bar
  baz
end

func main do
  t: test = test::foo

  match t
    test::foo do
      println("foo")
    end

    test::bar do
      println("bar")
    end

    default do
      println("baz")
    end
  end
end