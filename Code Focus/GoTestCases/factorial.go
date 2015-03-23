

func main() int {
    var luke int = fac( add(addOne(2), 5));
    return luke;
}; 

func fac(n int) int {
	if (n == 0) {
		return 1;
	} else {
		return n * fac(n-1);
	};
};

func addOne(n int) int {
    return n + 1;
};

func add(n int, m int) int {
    return n + m;
};  

