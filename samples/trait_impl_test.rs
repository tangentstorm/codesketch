// Trait with method signatures
pub trait Animal {
    fn name(&self) -> String;
    fn sound(&self) -> String;
    fn make_sound(&self) {
        println!("The {} says {}", self.name(), self.sound());
    }
}

// Implementation with method signatures
pub struct Dog {
    name: String,
    breed: String,
}

impl Animal for Dog {
    fn name(&self) -> String {
        self.name.clone()
    }

    fn sound(&self) -> String {
        "Woof!".to_string()
    }
}

impl Dog {
    pub fn new(name: String, breed: String) -> Self {
        Self { name, breed }
    }

    pub fn breed(&self) -> &str {
        &self.breed
    }
}