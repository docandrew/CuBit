//! Private metadata ABI for the same single-owner Database as value operations.
//! This persists declarations, never authorizes namespaces or installs types.
use super::{Database, Request};
use crate::{
    StorageState, name,
    schemas::{Creation, EncodedSchema, MAX_ENCODED, Management},
};

#[repr(u32)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Action {
    Create = 1,
    Recover = 2,
}

#[repr(u32)]
#[derive(Clone, Copy)]
pub enum Status {
    Created = 1,
    AlreadyExists = 2,
    DefinitionConflict = 3,
    Loaded = 4,
    Absent = 5,
    Rejected = 6,
    Uncertain = 7,
    LoadFailed = 8,
    LoadedManaged = 9,
    ManagementConflict = 10,
}

#[repr(C)]
pub struct Reply {
    pub code: u32,
    pub length: u32,
    pub data: [u8; MAX_ENCODED],
}
impl Reply {
    fn new(code: Status) -> Self {
        Self {
            code: code as u32,
            length: 0,
            data: [0; MAX_ENCODED],
        }
    }
    fn invalid() -> Self {
        Self {
            code: 0,
            length: 0,
            data: [0; MAX_ENCODED],
        }
    }
}

fn field(bytes: &[u8; 128], length: u32) -> Option<&str> {
    let n = usize::try_from(length).ok()?;
    if !(1..=128).contains(&n) || bytes[n..].iter().any(|b| *b != 0) {
        return None;
    }
    let s = std::str::from_utf8(&bytes[..n]).ok()?;
    name(s).then_some(s)
}

impl Database {
    fn schema_failure(&mut self, action: Action) -> Reply {
        self.retired = true;
        Reply::new(if action == Action::Recover {
            Status::LoadFailed
        } else {
            Status::Uncertain
        })
    }

    fn schema_execute(&mut self, request: &Request, input: &[u8]) -> Reply {
        let action = match request.action {
            n if n == Action::Create as u32 => Action::Create,
            n if n == Action::Recover as u32 => Action::Recover,
            _ => return Reply::invalid(),
        };
        if self.retired || self.store.state() != StorageState::Ready {
            return self.schema_failure(action);
        }
        if request.reserved != 0 || request.expected_revision != 0 {
            return Reply::invalid();
        }
        let (Some(namespace), Some(context)) = (
            field(&request.name, request.name_length),
            field(&request.context, request.context_length),
        ) else {
            return Reply::invalid();
        };
        if action == Action::Recover {
            if !input.is_empty() || request.schema != [0; 4] {
                return Reply::invalid();
            }
            return match self.store.read_definition(namespace, context) {
                Ok(Some(schema)) => {
                    let status = match self.store.management(namespace, context) {
                        Ok(Some(Management::ApplicationState)) => Status::Loaded,
                        Ok(Some(Management::DeclarationManaged)) => Status::LoadedManaged,
                        _ => return self.schema_failure(action),
                    };
                    let mut reply = Reply::new(status);
                    reply.length = schema.bytes().len() as u32;
                    reply.data[..schema.bytes().len()].copy_from_slice(schema.bytes());
                    reply
                }
                Ok(None) => Reply::new(Status::Absent),
                Err(_) => self.schema_failure(action),
            };
        }
        let mut key = [0; 32];
        for (part, word) in key.chunks_exact_mut(8).zip(request.schema) {
            part.copy_from_slice(&word.to_be_bytes());
        }
        let schema = match EncodedSchema::parse(input) {
            Ok(schema) if schema.key() == &key => schema,
            _ => return Reply::new(Status::Rejected),
        };
        match self.store.create_object(namespace, context, &schema) {
            Ok(Creation::Created) => Reply::new(Status::Created),
            Ok(Creation::AlreadyExists) => Reply::new(Status::AlreadyExists),
            Ok(Creation::DefinitionConflict) => Reply::new(Status::DefinitionConflict),
            Ok(Creation::ManagementConflict) => Reply::new(Status::ManagementConflict),
            Err(_) => self.schema_failure(action),
        }
    }
}

/// # Safety
/// Same exclusive lifetime/non-overlap requirements as the value ABI:
/// live Database; readable stable Request and input; aligned writable Reply.
/// Buffers are owned worker memory, NOT client/grant pointers. Nothing retained.
/// Native panic aborts; no unwinding across this boundary.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn cubit_config_schema_execute(
    database: *mut Database,
    request: *const Request,
    output: *mut Reply,
) {
    if output.is_null() {
        return;
    }
    unsafe {
        output.write(Reply::invalid());
    }
    if database.is_null() || request.is_null() {
        return;
    }
    let request = unsafe { &*request };
    if request.input_length > MAX_ENCODED as u64
        || (request.input_length != 0 && request.input.is_null())
    {
        return;
    }
    let input = if request.input_length == 0 {
        &[]
    } else {
        unsafe { std::slice::from_raw_parts(request.input, request.input_length as usize) }
    };
    let reply = unsafe { &mut *database }.schema_execute(request, input);
    unsafe {
        output.write(reply);
    }
}

#[cfg(test)]
mod tests {
    use super::super::tests::{database, request};
    use super::*;

    fn schema() -> Vec<u8> {
        let mut bytes = vec![0x84, 1, 0x58, 32];
        bytes.extend_from_slice(&[1; 32]);
        bytes.extend_from_slice(&[1, 0x80]);
        bytes
    }
    fn call(db: &mut Database, action: Action, input: &[u8]) -> Reply {
        let mut r = request(action as u32);
        if action == Action::Recover {
            r.schema = [0; 4];
        }
        r.input = input.as_ptr();
        r.input_length = input.len() as u64;
        let mut reply = Reply::invalid();
        // SAFETY: local disjoint initialized buffers and exclusive database.
        unsafe {
            cubit_config_schema_execute(db, &r, &mut reply);
        }
        reply
    }
    #[test]
    fn managed_metadata_and_value_abi_reject_ordinary_writes() {
        let mut db = database();
        let bytes = schema();
        let definition = EncodedSchema::parse(&bytes).unwrap();
        assert_eq!(
            db.store
                .register_object(
                    "org.cubit",
                    "test",
                    &definition,
                    Management::DeclarationManaged
                )
                .unwrap(),
            Creation::Created
        );
        let loaded = call(&mut db, Action::Recover, &[]);
        assert_eq!(loaded.code, Status::LoadedManaged as u32);
        assert_eq!(&loaded.data[..loaded.length as usize], bytes);
        assert_eq!(
            call(&mut db, Action::Create, &bytes).code,
            Status::ManagementConflict as u32
        );
        let mut object = vec![0x84, 1, 0x58, 32];
        object.extend_from_slice(&[1; 32]);
        object.extend_from_slice(&[0x81, 0x82, 0x18, 42, 0, 0x40]);
        let result = db.execute(&request(2), &object);
        assert_eq!(result.code, super::super::Status::Rejected as u32);
        assert!(!db.retired());
        assert_eq!(db.store.head("org.cubit", "test").unwrap(), 0);
        assert_eq!(
            call(&mut db, Action::Recover, &[]).code,
            Status::LoadedManaged as u32
        );
        db.close().unwrap();
    }

    #[test]
    fn declaration_abi_create_conflict_recover_unset() {
        let mut db = database();
        let bytes = schema();
        assert_eq!(
            call(&mut db, Action::Recover, &[]).code,
            Status::Absent as u32
        );
        assert_eq!(
            call(&mut db, Action::Create, &bytes).code,
            Status::Created as u32
        );
        assert_eq!(
            call(&mut db, Action::Create, &bytes).code,
            Status::AlreadyExists as u32
        );
        let mut different = bytes.clone();
        different[36] = 2;
        assert_eq!(
            call(&mut db, Action::Create, &different).code,
            Status::DefinitionConflict as u32
        );
        let reply = call(&mut db, Action::Recover, &[]);
        assert_eq!(reply.code, Status::Loaded as u32);
        assert_eq!(&reply.data[..reply.length as usize], bytes);
        assert!(reply.data[reply.length as usize..].iter().all(|n| *n == 0));
        assert_eq!(db.store.head("org.cubit", "test").unwrap(), 0);
        db.close().unwrap();
    }

    #[test]
    fn lost_creation_receipt_recovers_definition_without_initializing_value() {
        let io = std::sync::Arc::new(turso_core::MemoryIO::new());
        let path = "worker-lost-creation-receipt.db";
        let mut db = Database::new(crate::Store::open_with_io(io.clone(), path).unwrap());
        let bytes = schema();
        // Persist the definition, then lose the reply before the caller can
        // receive a handle. Retirement must not mean "nothing was created".
        assert_eq!(
            call(&mut db, Action::Create, &bytes).code,
            Status::Created as u32
        );
        assert_eq!(
            db.schema_failure(Action::Create).code,
            Status::Uncertain as u32
        );
        assert!(db.close().unwrap_err().is::<crate::RecoveryRequired>());

        let mut recovered = Database::new(crate::Store::open_with_io(io, path).unwrap());
        let loaded = call(&mut recovered, Action::Recover, &[]);
        assert_eq!(loaded.code, Status::Loaded as u32);
        assert_eq!(&loaded.data[..loaded.length as usize], bytes);
        assert_eq!(recovered.store.head("org.cubit", "test").unwrap(), 0);
        // Explicit create-or-open with the identical declaration is idempotent.
        // Even a reused claimed digest cannot replace its actual definition.
        assert_eq!(
            call(&mut recovered, Action::Create, &bytes).code,
            Status::AlreadyExists as u32
        );
        let mut different = bytes.clone();
        different[36] = 2;
        assert_eq!(
            call(&mut recovered, Action::Create, &different).code,
            Status::DefinitionConflict as u32
        );
        let loaded = call(&mut recovered, Action::Recover, &[]);
        assert_eq!(&loaded.data[..loaded.length as usize], bytes);
        assert_eq!(recovered.store.head("org.cubit", "test").unwrap(), 0);
        assert_eq!(
            recovered.execute(&request(1), &[]).code,
            super::super::Status::Absent as u32
        );
        recovered.close().unwrap();
    }
    #[test]
    fn malformed_requests_never_write_and_retirement_is_shared() {
        let mut db = database();
        assert_eq!(
            call(&mut db, Action::Create, &[0]).code,
            Status::Rejected as u32
        );
        assert_eq!(call(&mut db, Action::Recover, &[0]).code, 0);
        let mut r = request(Action::Create as u32);
        r.reserved = 1;
        assert_eq!(db.schema_execute(&r, &schema()).code, 0);
        r.reserved = 0;
        r.name_length = 129;
        assert_eq!(db.schema_execute(&r, &schema()).code, 0);
        assert!(!db.retired());
        assert!(
            db.store
                .read_definition("org.cubit", "test")
                .unwrap()
                .is_none()
        );
        db.retired = true;
        assert_eq!(
            call(&mut db, Action::Recover, &[]).code,
            Status::LoadFailed as u32
        );
        assert_eq!(
            call(&mut db, Action::Create, &schema()).code,
            Status::Uncertain as u32
        );
    }
    #[test]
    fn abi_layout_and_oversized_input_before_dereference() {
        assert_eq!(std::mem::size_of::<Reply>(), 8 + MAX_ENCODED);
        assert_eq!(std::mem::align_of::<Reply>(), 4);
        assert_eq!(std::mem::offset_of!(Reply, data), 8);
        let mut db = database();
        let mut r = request(Action::Create as u32);
        r.input_length = MAX_ENCODED as u64 + 1;
        let mut reply = Reply::invalid();
        // SAFETY: the oversized/null input is rejected before dereference.
        unsafe {
            cubit_config_schema_execute(&mut db, &r, &mut reply);
        }
        assert_eq!(reply.code, 0);
        r.input_length = 1;
        unsafe {
            cubit_config_schema_execute(&mut db, &r, &mut reply);
        }
        assert_eq!(reply.code, 0);
        assert!(!db.retired());
    }

    #[test]
    fn damaged_metadata_retires_value_and_schema_operations() {
        let mut db = database();
        assert_eq!(
            call(&mut db, Action::Create, &schema()).code,
            Status::Created as u32
        );
        db.store
            .connection
            .execute("UPDATE object_types SET declaration=x'00'")
            .unwrap();
        assert_eq!(
            call(&mut db, Action::Recover, &[]).code,
            Status::LoadFailed as u32
        );
        assert!(db.retired());
        assert_eq!(
            call(&mut db, Action::Create, &schema()).code,
            Status::Uncertain as u32
        );
        assert_eq!(
            db.execute(&request(1), &[]).code,
            super::super::Status::LoadFailed as u32
        );
        assert!(db.close().unwrap_err().is::<crate::RecoveryRequired>());
    }
}
