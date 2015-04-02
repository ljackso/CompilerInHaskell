
func main() {
    var c chan int = make(chan int);
    go pushNum(c);
    go showNum(c);
};

func pushNum(c) int{
    for(var i int = 0; i < 5; i++){
        c <- i;
    };
};

func showNum(c) int {
    var int l = 0;
    for(var i int = 0; i < 5; i++){
        l = <- c;
        Show(l);
    };    
};