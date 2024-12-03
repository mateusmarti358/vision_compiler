use io from lib

# enum named test
enum test
    foo
    bar
    baz
end

func main do
    t: test = test::foo

    # match statement
    match t
        test::foo do
            println("foo")
        end

        test::bar do
            println("bar")
        end

        default
            println("baz")
        end
    end
end
