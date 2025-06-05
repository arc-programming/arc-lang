mod test;

use std::io;

type Point struct {
    x: f32;
    y: f32;
}

func distance(p1: Point, p2: Point) f32 {
    let dx = p1.x - p2.x;
    let dy = p1.y - p2.y;
    return math.sqrt(dx * dx + dy * dy);
}

func main() Result(void, Error) {
    let p1 = Point{.x = 1.0, .y = 2.0};
    let p2 = Point{.x = 4.0, .y = 6.0};
    let dist = distance(p1, p2);
    io.println("Distance: {dist}");
    return .Ok;
}