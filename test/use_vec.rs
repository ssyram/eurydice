fn use_vec(v: Vec<i32>) {
    let l = v.len();
    for x in v {
        assert!(x > 0);
    }
    assert!(l > 0);
}

fn main() {}
