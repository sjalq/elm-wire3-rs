// Auto-generated from Elm module: Messages
// Do not edit manually — regenerate from the Elm source.

#![allow(dead_code, unused_imports)]

use std::collections::{BTreeMap, BTreeSet};
use elm_wire3_rs::wire3::{Wire3Encoder, Wire3Decoder, types::*};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Task {
    pub completed: bool,
    pub id: ElmInt,
    pub title: String,
}

impl Task {
    pub fn wire3_encode(&self, enc: &mut Wire3Encoder) {
        enc.encode_bool(self.completed);
        enc.encode_int(&self.id);
        enc.encode_string(&self.title);
    }
}

impl Task {
    pub fn wire3_decode(dec: &mut Wire3Decoder) -> Result<Self, Wire3DecodeError> {
        let completed = dec.decode_bool()?;
        let id = dec.decode_int()?;
        let title = dec.decode_string()?;
        Ok(Task {
            completed,
            id,
            title,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ToBackend {
    AddTask(String),
    DeleteTask(ElmInt),
    RequestTasks,
    ToggleTask(ElmInt),
}

impl ToBackend {
    pub fn wire3_encode(&self, enc: &mut Wire3Encoder) {
        match self {
            ToBackend::AddTask(v0) => {
                enc.encode_tag8(0);
                enc.encode_string(&v0);
            }
            ToBackend::DeleteTask(v0) => {
                enc.encode_tag8(1);
                enc.encode_int(&v0);
            }
            ToBackend::RequestTasks => enc.encode_tag8(2),
            ToBackend::ToggleTask(v0) => {
                enc.encode_tag8(3);
                enc.encode_int(&v0);
            }
        }
    }
}

impl ToBackend {
    pub fn wire3_decode(dec: &mut Wire3Decoder) -> Result<Self, Wire3DecodeError> {
        let tag = dec.decode_tag8()?;
        match tag {
            0 => {
                let v0 = dec.decode_string()?;
                Ok(ToBackend::AddTask(v0))
            }
            1 => {
                let v0 = dec.decode_int()?;
                Ok(ToBackend::DeleteTask(v0))
            }
            2 => Ok(ToBackend::RequestTasks),
            3 => {
                let v0 = dec.decode_int()?;
                Ok(ToBackend::ToggleTask(v0))
            }
            _ => Err(Wire3DecodeError::InvalidTag { tag: tag as u8, type_name: "ToBackend" }),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ToFrontend {
    TaskAdded(Task),
    TaskDeleted(ElmInt),
    TaskList(Vec<Task>),
    TaskToggled(ElmInt, bool),
}

impl ToFrontend {
    pub fn wire3_encode(&self, enc: &mut Wire3Encoder) {
        match self {
            ToFrontend::TaskAdded(v0) => {
                enc.encode_tag8(0);
                v0.wire3_encode(enc);
            }
            ToFrontend::TaskDeleted(v0) => {
                enc.encode_tag8(1);
                enc.encode_int(&v0);
            }
            ToFrontend::TaskList(v0) => {
                enc.encode_tag8(2);
                enc.encode_list(&v0, |enc, v| v.wire3_encode(enc));
            }
            ToFrontend::TaskToggled(v0, v1) => {
                enc.encode_tag8(3);
                enc.encode_int(&v0);
                enc.encode_bool(*v1);
            }
        }
    }
}

impl ToFrontend {
    pub fn wire3_decode(dec: &mut Wire3Decoder) -> Result<Self, Wire3DecodeError> {
        let tag = dec.decode_tag8()?;
        match tag {
            0 => {
                let v0 = Task::wire3_decode(dec)?;
                Ok(ToFrontend::TaskAdded(v0))
            }
            1 => {
                let v0 = dec.decode_int()?;
                Ok(ToFrontend::TaskDeleted(v0))
            }
            2 => {
                let v0 = dec.decode_list(|dec| Task::wire3_decode(dec))?;
                Ok(ToFrontend::TaskList(v0))
            }
            3 => {
                let v0 = dec.decode_int()?;
                let v1 = dec.decode_bool()?;
                Ok(ToFrontend::TaskToggled(v0, v1))
            }
            _ => Err(Wire3DecodeError::InvalidTag { tag: tag as u8, type_name: "ToFrontend" }),
        }
    }
}

