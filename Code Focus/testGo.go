
func main() {
	var c chan int = make(chan int);
	go count(20);
	go count(20);
	Wait();
    Kill();
    var j int = getCount();
	Show(j);
}

func count(t int) {
	for (t > 0) {
		c <- 1;
		t--;
	};
}

func getCount() int {
	var l int = 1;
    var j int = 0;
	for (l != 0) {
        l = <-c;
		j += l;
	};
    return j;
}