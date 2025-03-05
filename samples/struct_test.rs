// Simple struct with public fields
pub struct Point { 
    pub x: i32, 
    pub y: i32 
}

// More complex struct with different field types and visibility
pub struct User {
    pub id: u64,
    pub name: String,
    pub email: String,
    active: bool,
    created_at: chrono::DateTime<chrono::Utc>,
    pub tags: Vec<String>
}

// Tuple struct
pub struct Color(pub u8, pub u8, pub u8);

// Empty struct
pub struct Empty;

// Generic struct with lifetime
pub struct Container<'a, T> {
    pub data: &'a T,
    pub modified: bool
}
