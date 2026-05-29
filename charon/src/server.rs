//! Wire protocol types for Charon's server (on-demand translation) mode.
//!
//! The protocol is newline-delimited JSON over stdin/stdout: each request is a JSON object
//! terminated by `\n`, and each response is a JSON object terminated by `\n`.

use crate::ast::{File, FileId, ItemId};
use crate::options::CliOpts;
use serde::{Deserialize, Serialize};

/// A request from the client to the server.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Request {
    /// Client-assigned request id, echoed back in the response.
    pub id: u32,
    #[serde(flatten)]
    pub body: RequestBody,
}

/// The body of a client request.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "method", content = "params")]
pub enum RequestBody {
    /// Return session-level metadata about the crate.
    #[serde(rename = "crate_info")]
    CrateInfo,
    /// Resolve a name-pattern to a list of [`ItemId`]s. Does not trigger translation.
    #[serde(rename = "resolve")]
    Resolve { pattern: String },
    /// Return the translated AST for the given item, translating it on first request.
    /// The response payload has shape `{ "kind": "Fun"|"Type"|..., "decl": <ast node> }`.
    #[serde(rename = "get")]
    Get { id: ItemId },
    /// Return the `Name` of an item without translating it.
    #[serde(rename = "name")]
    Name { id: ItemId },
    /// Return the source-file record for the given [`FileId`].
    #[serde(rename = "file")]
    File { id: FileId },
    /// Return server statistics (items registered / translated).
    #[serde(rename = "stats")]
    Stats,
    /// Close the session.
    #[serde(rename = "shutdown")]
    Shutdown,
}

/// A response from the server.
#[derive(Debug, Serialize)]
pub struct Response {
    /// Echoed from the corresponding [`Request::id`].
    pub id: u32,
    #[serde(flatten)]
    pub result: ResponseResult,
}

/// Whether the request succeeded.
#[derive(Debug, Serialize)]
#[serde(tag = "status")]
pub enum ResponseResult {
    #[serde(rename = "ok")]
    Ok { data: ResponseData },
    #[serde(rename = "error")]
    Err { message: String },
}

impl ResponseResult {
    pub fn ok(data: ResponseData) -> Self {
        Self::Ok { data }
    }
    pub fn err(message: impl std::fmt::Display) -> Self {
        Self::Err {
            message: message.to_string(),
        }
    }
}

/// The payload of a successful response.
#[derive(Debug, Serialize)]
#[serde(tag = "kind", content = "value")]
pub enum ResponseData {
    #[serde(rename = "crate_info")]
    CrateInfo(CrateInfoResult),
    #[serde(rename = "resolve")]
    Resolve(Vec<ItemId>),
    /// The translated item as `{ "kind": "Fun"|..., "decl": <ast node> }`,
    /// or `null` if the id is unknown or translation failed.
    #[serde(rename = "get")]
    Get(Option<serde_json::Value>),
    /// The serialized `Name`, or `null` if the id is unknown.
    #[serde(rename = "name")]
    Name(Option<serde_json::Value>),
    /// The source-file record, or `null` if the id is unknown.
    #[serde(rename = "file")]
    File(Option<File>),
    #[serde(rename = "stats")]
    Stats(StatsResult),
    #[serde(rename = "shutdown")]
    Shutdown,
}

/// Session-level metadata returned by `crate_info`.
#[derive(Debug, Serialize)]
pub struct CrateInfoResult {
    pub crate_name: String,
    pub charon_version: String,
    pub options: CliOpts,
}

/// Counters returned by `stats`.
#[derive(Debug, Serialize)]
pub struct StatsResult {
    /// Items that have been fully translated (or whose translation was attempted).
    pub items_translated: usize,
    /// Items that have been registered (assigned an [`ItemId`]) in total.
    pub items_registered: usize,
}
