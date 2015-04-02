

func main() {
    var c chan int = make(chan int);
    var n chan int = make(chan int); 
    c <- 5;
    c <- 24;
    n <- 4;
    var l int = 10;
    Show(l);
    l += <-c;
    Show(l);
    l += <-c;
    Show(l);
    l += <-n;
    Show(l);
    
};
func test(n int) int {
    return n;
};
func testb(n int) int {
    return n+1;
};
