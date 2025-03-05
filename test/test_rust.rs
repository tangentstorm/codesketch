// Test Rust file for codesketch

pub mod test_module {
    pub struct TestStruct {
        pub field: i32,
        private_field: String,
    }

    pub trait TestTrait {
        fn trait_method(&self) -> bool;
    }

    impl TestStruct {
        pub fn public_method(&self) -> i32 {
            self.field
        }

        fn private_method(&self) -> String {
            self.private_field.clone()
        }
    }

    impl TestTrait for TestStruct {
        fn trait_method(&self) -> bool {
            true
        }
    }

    pub fn public_function(arg: i32) -> i32 {
        arg * 2
    }

    fn private_function() -> bool {
        true
    }
}

pub enum TestEnum {
    VariantA,
    VariantB(i32),
    VariantC { field: String }
}

pub const CONSTANT: i32 = 42;