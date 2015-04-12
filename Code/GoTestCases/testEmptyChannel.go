
func main() {
    var c chan int = make(chan int);
    var j int = <-c;
    Show(j);
};