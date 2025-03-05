// Tuple structs
pub struct Color(pub u8, pub u8, pub u8);
pub struct Point(pub f32, f32);
pub struct Wrapper<T>(pub T);
pub struct Complex(pub u32, String, bool);

// Testing if our tool can parse tuple struct fields with mixed visibility