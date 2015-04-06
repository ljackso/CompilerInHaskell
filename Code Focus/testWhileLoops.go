
func main() {
    var l int = testLessThan(20);
    Show(l);
};

func testLessThan(n int) int {
    var l int = 0; 
    for (n > 0) {
        l++;
        n--;
    };
    return l;
};