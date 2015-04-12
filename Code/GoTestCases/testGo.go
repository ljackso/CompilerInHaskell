

func main() {
	var c chan = Make(chan int);
    for (var i int = 0; i < 20; i++){
        go count(20);
    };
    Wait();
    var j int = getCount();
	Show(j);
};

func count(t int) {
    for (t > 0){
        c <- 1;
        t--;
        Show(t);
    };
    
};

func getCount() int {
	var l int = 1;
    var j int = 0;
	for (l == 1) {
        l = <-c;
		j += l;
	};
    return j;
};
