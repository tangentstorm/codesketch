use serde::{Serialize, Deserialize};

/// Definition type enumeration
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum DefType {
    Module,
    Struct,
    Enum,
    Function,
    Trait,
    Impl,
    #[serde(untagged)]
    Other(String),
}

impl DefType {
    pub fn as_str(&self) -> &str {
        match self {
            DefType::Module => "module",
            DefType::Struct => "struct",
            DefType::Enum => "enum",
            DefType::Function => "fn",
            DefType::Trait => "trait",
            DefType::Impl => "impl",
            DefType::Other(s) => s,
        }
    }
}

/// Visibility enumeration
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Visibility {
    #[serde(rename = "*")]
    Public,
    #[serde(rename = "+")]
    Protected,
    #[serde(rename = "-")]
    Private,
}

impl Visibility {
    pub fn as_str(&self) -> &str {
        match self {
            Visibility::Public => "pub",
            Visibility::Protected => "protected",
            Visibility::Private => "",
        }
    }
    
    pub fn as_symbol(&self) -> &str {
        match self {
            Visibility::Public => "*",
            Visibility::Protected => "+",
            Visibility::Private => "-",
        }
    }
}

/// Definition information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DefInfo {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub signature: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parent: Option<String>,
    #[serde(default)]
    pub children: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub line_num: Option<u32>,
}

impl Default for DefInfo {
    fn default() -> Self {
        DefInfo {
            signature: None,
            parent: None,
            children: Vec::new(),
            line_num: None,
        }
    }
}

/// Definition structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Definition {
    pub iden: String,
    #[serde(rename = "type")]
    pub def_type: DefType,
    pub vis: Visibility,
    #[serde(flatten)]
    pub info: DefInfo,
}

/// Path information structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PathInfo {
    pub path: String,
    pub defs: Vec<Definition>,
}

/// Root structure
pub type Root = Vec<PathInfo>;