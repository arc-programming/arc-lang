fn main() {
    var x: i32 = "hello";
    var y: i32 = 42;
    var z: i32 = y;
    var unknown: i32 = undeclared_var;
    var a: i32 = 10;
    var a: f64 = 3.14;
    return;
}

fn test_function(param: i32) -> i32 {
    return param * 2;
}
