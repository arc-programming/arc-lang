mod test;

use std::io;

type Point struct {
    x: f32;
    y: f32;
}

fn distance(p1: Point, p2: Point) f32 {
    var dx = p1.x - p2.x;
    var dy = p1.y - p2.y;
    return math.sqrt(dx * dx + dy * dy);
}

fn main() Result(void, Error) {
    var p1 = Point{.x = 1.0, .y = 2.0};
    var p2 = Point{.x = 4.0, .y = 6.0};
    var dist = distance(p1, p2);
    io.println("Distance: {dist}");
    return .Ok;
}