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
    Macro,
    Constant,
    TypeAlias,
    #[serde(untagged)]
    Other(String),
}

// Implementation for DefType is left for potential future use
// Currently unused but may be needed for extensibility

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

// Implementation for Visibility is left for potential future use
// Currently unused but may be needed for extensibility

/// Definition information
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct DefInfo {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub signature: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parent: Option<String>,
    #[serde(default)]
    pub children: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub line_num: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub line_end: Option<u32>,
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