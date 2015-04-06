
func main() {
	var c chan int = make(chan int);
    for (var i int = 0; i < 20; i++){
        go count();
    };
    go showCount();
    Wait();
    var j int = getCount();
	Show(j);
};

func count() int {
	var t int = 20;
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

func showCount() int {

    var l int = 1;
    var j int = 0;
	for (l == 1) {
        l = <-c;
		j += l;
        Show(j);
	};

};