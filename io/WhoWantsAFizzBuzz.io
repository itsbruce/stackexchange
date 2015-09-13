// http://codereview.stackexchange.com/questions/104526/who-wants-a-fizzbuzz

toBuzzWord := method(n,
    if (n % 15 == 0, return "FizzBuzz")
    if (n % 5 == 0, return "Fizz")
    if (n % 3 == 0, return "Buzz")
    return n
)
Range // addon reference required in newer versions
1 to(100) foreach(i, toBuzzWord(i) println)
