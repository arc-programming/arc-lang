// Test Arc source file
fn main() {
    var message = "Hello, Arc!";
    var count = 42;
    var pi = 3.14159;
    
    if count > 0 {
        print(message);
        return true;
    } else {
        return false;
    }
}

// Test some Arc-specific syntax
type Point struct {
    x f32;
    y f32;
}

fn distance(p1 Point, p2 Point) f32 {
    var dx = p1.x - p2.x;
    var dy = p1.y - p2.y;
    return sqrt(dx * dx + dy * dy);
}

// Test operators and keywords
var result = data |> filter(fn(x) x > 0) |> map(fn(x) x * 2);
